//! Stackful fiber runtime backing the sys.thread API.
//!
//! HashLink's hl target assumes HL_THREADS: Haxe code spawns worker threads
//! and blocks on locks/deques they release. Each OS thread that enters the VM
//! owns a scheduler and its worker-affine krio fibers. Compiled AIR V2 thread
//! bodies are distributed over an M:N worker pool; interpreter/hybrid bodies
//! remain on the main scheduler because their frames belong to one
//! `HLInterpreter`. Blocking primitives park the current fiber and let
//! runnable siblings advance.
//!
//! GC: each fiber stack is registered with the conservative scanner
//! (krio's switch spills all callee-saved regs — including d8-d15 — onto
//! the fiber stack, so scanning [saved_sp, stack_top) covers registers).
//! Exception state (trap chain head + exception value) is per-fiber,
//! swapped into the GC singleton's cells around every resume.
//!
//! The host is notified around every fiber switch so it can swap logical VM
//! state (notably interpreted AIR v2 frames). The interpreter publishes both
//! its active and suspended register files as GC roots.

use crate::error::TrapContext;
use crate::hl::{vclosure, vdynamic};
use krio_fiber::{Fiber, FiberState};
use std::cell::{Cell, RefCell};
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, LazyLock, Mutex, OnceLock, Weak};
use std::time::Instant;

pub type ClosureRunner =
    unsafe extern "C" fn(*mut vclosure, *mut *mut vdynamic, i32) -> *mut vdynamic;
pub type FiberSwitchHook = unsafe extern "C" fn(u32, u32);
pub type StubResolver = unsafe extern "C" fn(i32) -> *mut c_void;

static CLOSURE_RUNNER: AtomicUsize = AtomicUsize::new(0);
static FIBER_SWITCH_HOOK: AtomicUsize = AtomicUsize::new(0);
static STUB_RESOLVER: AtomicUsize = AtomicUsize::new(0);
static NEXT_FIBER_ID: AtomicU32 = AtomicU32::new(1);
static NEXT_SCHEDULER_ID: AtomicU64 = AtomicU64::new(1);
static NEXT_WAIT_TOKEN: AtomicU64 = AtomicU64::new(1);
static LOGICAL_THREADS: AtomicUsize = AtomicUsize::new(0);
static COMPILED_WORKERS_ENABLED: AtomicBool = AtomicBool::new(false);
const STUB_SENTINEL_LIMIT: usize = 0x100000;

#[derive(Clone, Copy)]
enum SchedulerCommand {
    Wake(Waiter),
    Spawn { id: u32, closure: usize },
}

struct SchedulerEndpoint {
    commands: Mutex<VecDeque<SchedulerCommand>>,
    changed: Condvar,
}

impl SchedulerEndpoint {
    fn new() -> Self {
        Self {
            commands: Mutex::new(VecDeque::new()),
            changed: Condvar::new(),
        }
    }

    fn push(&self, command: SchedulerCommand) {
        self.commands.lock().unwrap().push_back(command);
        self.changed.notify_one();
    }
}

static SCHEDULER_ENDPOINTS: LazyLock<Mutex<HashMap<u64, Weak<SchedulerEndpoint>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WaitStatus {
    Waiting,
    Notified,
    TimedOut,
}

#[derive(Clone, Copy)]
struct WaitRegistration {
    scheduler_id: u64,
    fiber_id: u32,
    status: WaitStatus,
}

static WAIT_REGISTRY: LazyLock<Mutex<HashMap<u64, WaitRegistration>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

fn worker_trace(label: &str, id: u64, detail: u64) {
    static START: OnceLock<Instant> = OnceLock::new();
    static ENABLED: OnceLock<bool> = OnceLock::new();
    if !*ENABLED.get_or_init(|| std::env::var_os("ASH_FIBER_TRACE").is_some()) {
        return;
    }
    let elapsed = START.get_or_init(Instant::now).elapsed().as_secs_f64() * 1e3;
    eprintln!("[fiber] {elapsed:8.2}ms {label} id={id} detail={detail}");
}

/// Registered by the host (interpreter) so native code can run a Haxe
/// closure whose `fun` is an interpreter stub pointer (findex+1).
#[no_mangle]
pub unsafe extern "C" fn hlp_set_closure_runner(f: ClosureRunner) {
    CLOSURE_RUNNER.store(f as usize, Ordering::Release);
}

/// Registered by the host so logical VM state follows krio's stack switch.
/// Fiber id 0 denotes the scheduler/main context.
#[no_mangle]
pub unsafe extern "C" fn hlp_set_fiber_switch_hook(hook: FiberSwitchHook) {
    FIBER_SWITCH_HOOK.store(hook as usize, Ordering::Release);
}

/// The host enables worker dispatch only for compiled-only AIR V2 execution.
/// Hybrid/interpreter closures need their owning `HLInterpreter` and remain
/// on its main scheduler.
#[no_mangle]
pub unsafe extern "C" fn hlp_set_compiled_worker_mode(enabled: bool) {
    COMPILED_WORKERS_ENABLED.store(enabled, Ordering::Release);
}

/// Register the compiled-only sentinel resolver with native std helpers.
/// Runtime object slots such as `castFun` and `compareFun` are bare function
/// pointers and therefore need the same lazy compilation boundary as AIR V2
/// call sites.
#[no_mangle]
pub unsafe extern "C" fn hlp_set_stub_resolver(resolver: StubResolver) {
    STUB_RESOLVER.store(resolver as usize, Ordering::Release);
}

pub(crate) unsafe fn resolve_stub_sentinel(address: usize) -> *mut c_void {
    if address == 0 || address >= 0x100000 {
        return address as *mut c_void;
    }
    let resolver = STUB_RESOLVER.load(Ordering::Acquire);
    if resolver == 0 {
        return ptr::null_mut();
    }
    let resolver = std::mem::transmute::<usize, StubResolver>(resolver);
    resolver(address.wrapping_sub(1) as i32)
}

/// True only while an OS worker is executing a worker-affine VM fiber.
/// The host uses this to prevent a cold JIT sentinel from re-entering the
/// single main-thread interpreter through the legacy stub bridge.
#[no_mangle]
pub unsafe extern "C" fn hlp_fiber_is_worker_lane() -> bool {
    WORKER_LANE.with(Cell::get)
}

/// The registered interpreter re-entry runner, if any. Used by native code
/// (e.g. virtual method dispatch fallback) that encounters a stub-sentinel
/// function pointer it must not call directly.
pub(crate) unsafe fn closure_runner() -> Option<ClosureRunner> {
    let runner = CLOSURE_RUNNER.load(Ordering::Acquire);
    (runner != 0).then(|| std::mem::transmute::<usize, ClosureRunner>(runner))
}

struct VmFiber {
    fiber: Box<Fiber>,
    id: u32,
    closure: *mut vclosure,
    run_state: FiberRunState,
    resume_cause: ResumeCause,
    gc_blocking_depth: u32,
    /// Per-fiber exception state, swapped into the GC cells while running.
    trap_head: *mut TrapContext,
    exc_value: *mut vdynamic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FiberRunState {
    Runnable,
    Running,
    Waiting(u64),
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum ResumeCause {
    #[default]
    Scheduled,
    Notified,
    TimedOut,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Waiter {
    scheduler_id: u64,
    pub(crate) fiber_id: u32,
    pub(crate) token: u64,
}

#[derive(Clone, Copy)]
struct ParkRequest {
    waiter: Waiter,
    deadline: Option<Instant>,
}

/// State visible while a fiber stack is executing. The `VmFiber` itself is
/// temporarily removed from its scheduler before `resume`, so callbacks from
/// the running stack can borrow the scheduler without aliasing its storage.
#[derive(Clone, Copy)]
struct ActiveFiber {
    id: u32,
    closure: *mut vclosure,
    resume_cause: ResumeCause,
    pending_park: Option<ParkRequest>,
    gc_blocking_depth: u32,
}

struct Scheduler {
    id: u64,
    endpoint: Arc<SchedulerEndpoint>,
    fibers: Vec<VmFiber>,
    ready: VecDeque<u32>,
    timers: BinaryHeap<Reverse<(Instant, u64, u32)>>,
    main_gc_blocking_depth: u32,
    main_trap: *mut TrapContext,
    main_exc: *mut vdynamic,
}

impl Scheduler {
    fn new() -> Self {
        let id = NEXT_SCHEDULER_ID.fetch_add(1, Ordering::Relaxed);
        let endpoint = Arc::new(SchedulerEndpoint::new());
        SCHEDULER_ENDPOINTS
            .lock()
            .unwrap()
            .insert(id, Arc::downgrade(&endpoint));
        Self {
            id,
            endpoint,
            fibers: Vec::new(),
            ready: VecDeque::new(),
            timers: BinaryHeap::new(),
            main_gc_blocking_depth: 0,
            main_trap: ptr::null_mut(),
            main_exc: ptr::null_mut(),
        }
    }

    fn fiber_index(&self, id: u32) -> Option<usize> {
        self.fibers.iter().position(|fiber| fiber.id == id)
    }

    fn enqueue_ready(&mut self, id: u32) {
        if !self.ready.contains(&id) {
            self.ready.push_back(id);
        }
    }

    fn wake_claimed(&mut self, waiter: Waiter) -> bool {
        if waiter.scheduler_id != self.id {
            return false;
        }
        if waiter.fiber_id == 0 {
            return true;
        }
        let Some(index) = self.fiber_index(waiter.fiber_id) else {
            return false;
        };
        if self.fibers[index].run_state != FiberRunState::Waiting(waiter.token) {
            return false;
        }
        self.fibers[index].run_state = FiberRunState::Runnable;
        self.fibers[index].resume_cause = ResumeCause::Notified;
        self.enqueue_ready(waiter.fiber_id);
        true
    }

    fn wake_due_timers(&mut self) {
        let now = Instant::now();
        loop {
            let Some(Reverse((deadline, token, id))) = self.timers.peek().copied() else {
                return;
            };
            if deadline > now {
                return;
            }
            self.timers.pop();
            if !claim_timeout(token) {
                continue;
            }
            let Some(index) = self.fiber_index(id) else {
                continue;
            };
            if self.fibers[index].run_state != FiberRunState::Waiting(token) {
                continue;
            }
            self.fibers[index].run_state = FiberRunState::Runnable;
            self.fibers[index].resume_cause = ResumeCause::TimedOut;
            self.enqueue_ready(id);
        }
    }
}

thread_local! {
    static SCHEDULER: RefCell<Scheduler> = RefCell::new(Scheduler::new());
    static ACTIVE_FIBER: Cell<Option<ActiveFiber>> = const { Cell::new(None) };
    static WORKER_LANE: Cell<bool> = const { Cell::new(false) };
}

struct WorkerPool {
    workers: Vec<Arc<SchedulerEndpoint>>,
    next: AtomicUsize,
}

static WORKER_POOL: OnceLock<Option<WorkerPool>> = OnceLock::new();

fn configured_worker_count() -> usize {
    static COUNT: OnceLock<usize> = OnceLock::new();
    *COUNT.get_or_init(|| {
        if let Ok(value) = std::env::var("ASH_WORKERS") {
            return value.trim().parse().unwrap_or(0);
        }
        std::thread::available_parallelism()
            .map(|count| count.get().saturating_sub(1))
            .unwrap_or(0)
    })
}

fn worker_pool() -> Option<&'static WorkerPool> {
    WORKER_POOL
        .get_or_init(|| {
            let count = configured_worker_count();
            if count == 0 {
                return None;
            }
            let (sender, receiver) = std::sync::mpsc::channel();
            let mut started = 0usize;
            for index in 0..count {
                let sender = sender.clone();
                let spawn = std::thread::Builder::new()
                    .name(format!("ash-vm-{index}"))
                    .spawn(move || worker_main(sender));
                if spawn.is_ok() {
                    started += 1;
                }
            }
            drop(sender);
            // A worker that panics before publishing its endpoint must not
            // hang VM startup forever. Missing workers merely reduce the
            // pool; dispatch remains correct with any non-empty subset.
            let mut workers = Vec::with_capacity(started);
            for _ in 0..started {
                match receiver.recv_timeout(std::time::Duration::from_secs(2)) {
                    Ok(endpoint) => workers.push(endpoint),
                    Err(_) => break,
                }
            }
            (!workers.is_empty()).then(|| WorkerPool {
                workers,
                next: AtomicUsize::new(0),
            })
        })
        .as_ref()
}

fn worker_main(sender: std::sync::mpsc::Sender<Arc<SchedulerEndpoint>>) {
    WORKER_LANE.with(|worker| worker.set(true));
    crate::gc::gc_register_current_os_thread();
    let endpoint = SCHEDULER.with(|scheduler| Arc::clone(&scheduler.borrow().endpoint));
    let scheduler_id = SCHEDULER.with(|scheduler| scheduler.borrow().id);
    worker_trace("worker-ready", scheduler_id, 0);
    if sender.send(Arc::clone(&endpoint)).is_err() {
        crate::gc::gc_unregister_current_os_thread();
        return;
    }
    loop {
        unsafe {
            drain_scheduler_commands();
            if schedule_step() {
                continue;
            }
        }
        let deadline = SCHEDULER.with(|scheduler| {
            scheduler
                .borrow()
                .timers
                .peek()
                .map(|Reverse((deadline, _, _))| *deadline)
        });
        let queue = endpoint.commands.lock().unwrap();
        if !queue.is_empty() {
            continue;
        }
        crate::gc::gc_set_blocking(true);
        if let Some(deadline) = deadline {
            let wait = deadline.saturating_duration_since(Instant::now());
            let _ = endpoint.changed.wait_timeout(queue, wait).unwrap();
        } else {
            drop(endpoint.changed.wait(queue).unwrap());
        }
        crate::gc::gc_set_blocking(false);
    }
}

fn can_dispatch_to_worker() -> bool {
    COMPILED_WORKERS_ENABLED.load(Ordering::Acquire)
        && configured_worker_count() != 0
}

fn dispatch_to_worker(id: u32, closure: *mut vclosure) -> bool {
    let Some(pool) = worker_pool() else {
        return false;
    };
    let index = pool.next.fetch_add(1, Ordering::Relaxed) % pool.workers.len();
    worker_trace("dispatch", id as u64, index as u64);
    pool.workers[index].push(SchedulerCommand::Spawn {
        id,
        closure: closure as usize,
    });
    true
}

/// wren_lift-proven default; 64 KB tripped on real workloads there.
const FIBER_STACK_SIZE: usize = 256 * 1024;

pub(crate) unsafe fn fibers_active() -> bool {
    ACTIVE_FIBER.with(|active| active.get().is_some())
        || SCHEDULER.with(|scheduler| !scheduler.borrow().fibers.is_empty())
        || LOGICAL_THREADS.load(Ordering::Acquire) != 0
}

/// The opaque handle used by `thread_create` for the currently running Haxe
/// fiber. The main context continues to use its native OS-thread identity.
pub(crate) unsafe fn current_handle() -> Option<*mut c_void> {
    ACTIVE_FIBER.with(|active| {
        active
            .get()
            .map(|fiber| ((fiber.id as usize) << 4 | 1) as *mut c_void)
    })
}

/// Logical Haxe-thread id. Zero is the scheduler/main context; every stackful
/// worker has its own non-zero id independent of the OS worker carrying it.
pub(crate) unsafe fn current_id() -> u32 {
    ACTIVE_FIBER.with(|active| active.get().map_or(0, |fiber| fiber.id))
}

pub(crate) unsafe fn new_waiter() -> Waiter {
    let token = NEXT_WAIT_TOKEN.fetch_add(1, Ordering::Relaxed).max(1);
    let waiter = Waiter {
        scheduler_id: SCHEDULER.with(|scheduler| scheduler.borrow().id),
        fiber_id: current_id(),
        token,
    };
    WAIT_REGISTRY.lock().unwrap().insert(
        token,
        WaitRegistration {
            scheduler_id: waiter.scheduler_id,
            fiber_id: waiter.fiber_id,
            status: WaitStatus::Waiting,
        },
    );
    waiter
}

/// Wake a waiter if it still names the wait operation on which the fiber is
/// parked. The token check makes stale semaphore/condition timer entries
/// harmless after a timeout or a later wait by the same fiber.
pub(crate) unsafe fn wake(waiter: Waiter) -> bool {
    if !claim_notification(waiter) {
        return false;
    }
    let endpoint = {
        let mut endpoints = SCHEDULER_ENDPOINTS.lock().unwrap();
        let endpoint = endpoints.get(&waiter.scheduler_id).and_then(Weak::upgrade);
        if endpoint.is_none() {
            endpoints.remove(&waiter.scheduler_id);
        }
        endpoint
    };
    if let Some(endpoint) = endpoint {
        worker_trace("wake", waiter.fiber_id as u64, waiter.token);
        endpoint.push(SchedulerCommand::Wake(waiter));
        true
    } else {
        WAIT_REGISTRY.lock().unwrap().remove(&waiter.token);
        false
    }
}

fn claim_notification(waiter: Waiter) -> bool {
    let mut waiters = WAIT_REGISTRY.lock().unwrap();
    let Some(registration) = waiters.get_mut(&waiter.token) else {
        return false;
    };
    if registration.scheduler_id != waiter.scheduler_id
        || registration.fiber_id != waiter.fiber_id
        || registration.status != WaitStatus::Waiting
    {
        return false;
    }
    registration.status = WaitStatus::Notified;
    true
}

fn claim_timeout(token: u64) -> bool {
    let mut waiters = WAIT_REGISTRY.lock().unwrap();
    let Some(registration) = waiters.get_mut(&token) else {
        return false;
    };
    if registration.status != WaitStatus::Waiting {
        return false;
    }
    registration.status = WaitStatus::TimedOut;
    true
}

fn wait_status(token: u64) -> Option<WaitStatus> {
    WAIT_REGISTRY
        .lock()
        .unwrap()
        .get(&token)
        .map(|registration| registration.status)
}

fn finish_wait(token: u64) -> Option<WaitStatus> {
    WAIT_REGISTRY
        .lock()
        .unwrap()
        .remove(&token)
        .map(|registration| registration.status)
}

unsafe fn wake_due_timers() {
    SCHEDULER.with(|scheduler| scheduler.borrow_mut().wake_due_timers());
}

unsafe fn scheduler_idle(deadline: Option<Instant>) {
    // The main scheduler can spend an arbitrary amount of time waiting for a
    // logical thread. It is still a registered GC mutator, so rendezvous with
    // a collection requested by another OS worker before sleeping.
    crate::gc::gc_safepoint();
    let now = Instant::now();
    let own_wait = deadline.map_or(std::time::Duration::from_millis(1), |d| {
        d.saturating_duration_since(now)
            .min(std::time::Duration::from_millis(1))
    });
    if !own_wait.is_zero() {
        std::thread::sleep(own_wait);
    }
}

/// Park the current logical Haxe thread until its waiter is notified or its
/// deadline expires. Worker fibers leave a precise wait token for the
/// scheduler; the main context drives the scheduler while it waits.
pub(crate) unsafe fn park(waiter: Waiter, deadline: Option<Instant>) -> bool {
    debug_assert_eq!(waiter.fiber_id, current_id());
    if waiter.fiber_id == 0 {
        worker_trace("park-main", waiter.token, deadline.is_some() as u64);
        loop {
            crate::gc::gc_safepoint();
            match wait_status(waiter.token) {
                Some(WaitStatus::Notified) => {
                    finish_wait(waiter.token);
                    worker_trace("resume-main", waiter.token, 1);
                    return true;
                }
                Some(WaitStatus::TimedOut) | None => {
                    finish_wait(waiter.token);
                    worker_trace("resume-main", waiter.token, 0);
                    return false;
                }
                Some(WaitStatus::Waiting) => {}
            }
            if deadline.is_some_and(|limit| Instant::now() >= limit) {
                let _ = claim_timeout(waiter.token);
                return finish_wait(waiter.token) == Some(WaitStatus::Notified);
            }
            if !schedule_step() {
                scheduler_idle(deadline);
            }
        }
    }

    if wait_status(waiter.token) == Some(WaitStatus::Notified) {
        finish_wait(waiter.token);
        return true;
    }

    ACTIVE_FIBER.with(|active| {
        let mut fiber = active.get().expect("park called outside a fiber");
        debug_assert!(fiber.pending_park.is_none());
        fiber.pending_park = Some(ParkRequest { waiter, deadline });
        active.set(Some(fiber));
    });
    krio_fiber::yield_now();
    let notified = ACTIVE_FIBER.with(|active| {
        active
            .get()
            .is_some_and(|fiber| fiber.resume_cause == ResumeCause::Notified)
    }) || wait_status(waiter.token) == Some(WaitStatus::Notified);
    finish_wait(waiter.token);
    notified
}

/// Sleep without keeping a worker fiber runnable. The main context continues
/// driving ready fibers while it waits for its own deadline.
pub(crate) unsafe fn sleep_until(deadline: Instant) {
    let waiter = new_waiter();
    let _ = park(waiter, Some(deadline));
}

/// Per-logical-thread bookkeeping for `Gc.blocking`. It is intentionally kept
/// with the fiber rather than in Rust OS TLS, which every Haxe fiber shares.
pub(crate) unsafe fn update_gc_blocking_depth(blocking: bool) -> bool {
    if ACTIVE_FIBER.with(|active| active.get().is_some()) {
        return ACTIVE_FIBER.with(|active| {
            let mut fiber = active.get().expect("active fiber disappeared");
            let changed = update_blocking_depth(&mut fiber.gc_blocking_depth, blocking);
            active.set(Some(fiber));
            changed
        });
    }
    SCHEDULER.with(|scheduler| {
        update_blocking_depth(
            &mut scheduler.borrow_mut().main_gc_blocking_depth,
            blocking,
        )
    })
}

pub(crate) unsafe fn is_gc_blocking() -> bool {
    ACTIVE_FIBER.with(|active| {
        active
            .get()
            .map(|fiber| fiber.gc_blocking_depth != 0)
    })
    .unwrap_or_else(|| {
        SCHEDULER.with(|scheduler| scheduler.borrow().main_gc_blocking_depth != 0)
    })
}

fn update_blocking_depth(depth: &mut u32, blocking: bool) -> bool {
    if blocking {
        *depth = depth.saturating_add(1);
        true
    } else if *depth == 0 {
        false
    } else {
        *depth -= 1;
        true
    }
}

/// Whether `c` is the closure at the root of the currently running fiber.
///
/// Native helpers also use the closure runner to re-enter interpreted AIR V2
/// methods. Those nested calls must propagate exceptions through their active
/// native trap; only a real thread body's outermost call owns an uncaught
/// exception and may terminate the fiber.
#[no_mangle]
pub unsafe extern "C" fn hlp_fiber_is_root_closure(c: *mut vclosure) -> bool {
    ACTIVE_FIBER.with(|active| {
        active
            .get()
            .is_some_and(|fiber| fiber.closure == c)
    })
}

unsafe fn notify_switch(from: u32, to: u32) {
    if WORKER_LANE.with(Cell::get) {
        return;
    }
    let hook = FIBER_SWITCH_HOOK.load(Ordering::Acquire);
    if hook != 0 {
        let hook = std::mem::transmute::<usize, FiberSwitchHook>(hook);
        hook(from, to);
    }
}

unsafe fn run_closure(c: *mut vclosure) {
    let fun = (*c).fun as usize;
    if WORKER_LANE.with(Cell::get) {
        if fun >= STUB_SENTINEL_LIMIT {
            hlp_jit_closure_runner(c, std::ptr::null_mut(), 0);
        } else {
            eprintln!(
                "[ash] worker fiber received uncompiled closure sentinel {fun:#x}; refusing interpreter re-entry"
            );
        }
        return;
    }
    if let Some(runner) = closure_runner() {
        runner(c, std::ptr::null_mut(), 0);
    } else if fun >= STUB_SENTINEL_LIMIT {
        // Invoke a compiled thread body through the same typed ABI bridge used
        // by dynamic native calls. We are already on the thread fiber's stack;
        // creating another fiber here would change Thread.current() identity.
        hlp_jit_closure_runner(c, std::ptr::null_mut(), 0);
    } else {
        eprintln!(
            "[ash] fiber: cannot run closure (fun={:#x}, hasValue={}) — no closure runner set",
            fun,
            (*c).hasValue
        );
    }
}

/// Run a compiled closure through the same dynamic argument marshaller used
/// by `hlp_call_method`. Native event-loop code supplies `vdynamic**` args to
/// the registered closure runner, so standalone JIT fibers must register this
/// bridge instead of relying on the interpreter's runner.
#[no_mangle]
pub unsafe extern "C" fn hlp_jit_closure_runner(
    c: *mut vclosure,
    args: *mut *mut vdynamic,
    nargs: i32,
) -> *mut vdynamic {
    if c.is_null() || nargs < 0 {
        return ptr::null_mut();
    }

    let mut closure = c;
    if (*closure).hasValue == 2 {
        let wrapper = closure as *mut crate::hl::vclosure_wrapper;
        closure = (*wrapper).wrappedFun;
        if closure.is_null() {
            eprintln!("[ash] fiber: closure wrapper has no wrapped function");
            return ptr::null_mut();
        }
    }

    let closure_type = (*closure).t;
    if closure_type.is_null() {
        eprintln!("[ash] fiber: compiled closure has no function type");
        return ptr::null_mut();
    }

    // Bound closures store the stripped closure type; its parent is the full
    // method type whose first argument is the bound receiver.
    let call_type = if (*closure).hasValue != 0 {
        let parent = (*closure_type)
            .__bindgen_anon_1
            .fun
            .as_ref()
            .map_or(std::ptr::null_mut(), |fun| fun.parent);
        if parent.is_null() {
            closure_type
        } else {
            parent
        }
    } else {
        closure_type
    };

    let total = nargs as usize + usize::from((*closure).hasValue != 0);
    let array = crate::obj::hlp_alloc_dyn_array(total as i32);
    if array.is_null() {
        return ptr::null_mut();
    }
    let values = crate::types::hl_aptr::<*mut vdynamic>(array);
    if (*closure).hasValue != 0 {
        *values = (*closure).value as *mut vdynamic;
    }
    for i in 0..nargs as usize {
        *values.add(i + usize::from((*closure).hasValue != 0)) = if args.is_null() {
            ptr::null_mut()
        } else {
            *args.add(i)
        };
    }

    // hlp_call_method expects a closure without an already-bound value and
    // receives the receiver as the first dynamic argument instead.
    let call_closure = vclosure {
        t: call_type,
        fun: (*closure).fun,
        hasValue: 0,
        stackCount: 0,
        value: ptr::null_mut(),
    };
    crate::fun::hlp_call_method(&call_closure as *const vclosure as *mut vdynamic, array)
}

unsafe fn install_fiber(id: u32, c: *mut vclosure) {
    worker_trace("install", id as u64, SCHEDULER.with(|scheduler| scheduler.borrow().id));
    let c_usize = c as usize;
    let fiber = Box::new(Fiber::with_stack_size(FIBER_STACK_SIZE, move || {
        run_closure(c_usize as *mut vclosure);
    }));
    let (base, len) = fiber.stack_range();
    crate::gc::gc_register_fiber_stack(id, base as usize, len);
    // Charge the off-heap stack as GC allocation pressure so dead fibers'
    // stacks translate into collections (wren_lift core/fiber.rs:189-199).
    crate::gc::hlp_gc_track_external(len as u64);

    SCHEDULER.with(|scheduler| {
        let mut scheduler = scheduler.borrow_mut();
        scheduler.fibers.push(VmFiber {
            fiber,
            id,
            closure: c,
            run_state: FiberRunState::Runnable,
            resume_cause: ResumeCause::Scheduled,
            gc_blocking_depth: 0,
            trap_head: std::ptr::null_mut(),
            exc_value: std::ptr::null_mut(),
        });
        scheduler.enqueue_ready(id);
    });
}

unsafe fn drain_scheduler_commands() {
    let endpoint = SCHEDULER.with(|scheduler| Arc::clone(&scheduler.borrow().endpoint));
    let commands: Vec<SchedulerCommand> = {
        let mut queue = endpoint.commands.lock().unwrap();
        queue.drain(..).collect()
    };
    for command in commands {
        match command {
            SchedulerCommand::Wake(waiter) => {
                SCHEDULER.with(|scheduler| {
                    scheduler.borrow_mut().wake_claimed(waiter);
                });
            }
            SchedulerCommand::Spawn { id, closure } => {
                install_fiber(id, closure as *mut vclosure);
            }
        }
    }
}

/// Spawn a Haxe thread as a fiber. Returns an opaque non-null handle.
pub(crate) unsafe fn thread_create(c: *mut vclosure) -> *mut c_void {
    if c.is_null() {
        return std::ptr::null_mut();
    }
    let id = NEXT_FIBER_ID.fetch_add(1, Ordering::Relaxed);

    // Root the closure while the fiber exists — it is otherwise reachable
    // only from Rust heap memory the GC cannot see.
    crate::gc::gc_add_persistent(c as *mut vdynamic);
    LOGICAL_THREADS.fetch_add(1, Ordering::Release);
    worker_trace("create", id as u64, (*c).fun as usize as u64);
    if can_dispatch_to_worker() {
        // A freshly-created closure commonly still carries findex+1. Resolve
        // that sentinel before choosing a lane so the first Haxe thread gets
        // the same M:N treatment as later closures whose call sites happened
        // to compile them already. Failure is non-fatal: the main scheduler
        // can still execute it through the interpreter bridge.
        let fun = (*c).fun as usize;
        if fun != 0 && fun < STUB_SENTINEL_LIMIT {
            let resolved = resolve_stub_sentinel(fun);
            if !resolved.is_null() {
                (*c).fun = resolved;
            } else {
                worker_trace("resolve-failed", id as u64, fun as u64);
            }
        }
        if (*c).fun as usize >= STUB_SENTINEL_LIMIT && dispatch_to_worker(id, c) {
            return ((id as usize) << 4 | 1) as *mut c_void;
        }
    }
    install_fiber(id, c);

    // Give the new thread a chance to run to its first blocking point,
    // matching the "starts immediately" expectation of real threads.
    schedule_step();

    // Handle = fiber id, offset so it is never null and never a valid ptr.
    ((id as usize) << 4 | 1) as *mut c_void
}

unsafe fn remove_fiber(fiber: VmFiber, state: FiberState) {
    worker_trace(
        "remove",
        fiber.id as u64,
        u64::from(matches!(state, FiberState::Errored)),
    );
    crate::gc::gc_unregister_fiber_stack(fiber.id);
    crate::gc::gc_remove_persistent(fiber.closure as *mut vdynamic);
    LOGICAL_THREADS.fetch_sub(1, Ordering::Release);
    if let FiberState::Errored = state {
        eprintln!("[ash] fiber {} terminated with a panic", fiber.id);
    }
}

/// Resume each fiber that was ready at the beginning of this scheduler turn.
/// Fibers parked on a resource or timer do not consume context switches.
/// Runs on the main context only. Returns true if any fiber was resumed.
pub(crate) unsafe fn schedule_step() -> bool {
    if ACTIVE_FIBER.with(|active| active.get().is_some()) {
        // A fiber calling this should yield instead — never nest resumes.
        return false;
    }
    drain_scheduler_commands();
    wake_due_timers();
    let mut resumed = false;
    let turns = SCHEDULER.with(|scheduler| scheduler.borrow().ready.len());
    for _ in 0..turns {
        let Some(mut vm_fiber) = SCHEDULER.with(|scheduler| {
            let mut scheduler = scheduler.borrow_mut();
            loop {
                let id = scheduler.ready.pop_front()?;
                let Some(index) = scheduler.fiber_index(id) else {
                    continue;
                };
                break Some(scheduler.fibers.swap_remove(index));
            }
        }) else {
            break;
        };
        let state = vm_fiber.fiber.state();
        if matches!(state, FiberState::Done | FiberState::Errored) {
            remove_fiber(vm_fiber, state);
            continue;
        }
        if vm_fiber.run_state != FiberRunState::Runnable {
            SCHEDULER.with(|scheduler| scheduler.borrow_mut().fibers.push(vm_fiber));
            continue;
        }
        resumed = true;
        let id = vm_fiber.id;

        // Record where the main stack is suspended for the GC, then swap
        // this fiber's exception state into the live cells.
        let probe: usize = 0;
        crate::gc::gc_update_fiber_sp(0, &probe as *const usize as usize);
        let (mut trap, mut exc) = (vm_fiber.trap_head, vm_fiber.exc_value);
        crate::gc::gc_swap_exc_state(&mut trap, &mut exc);
        SCHEDULER.with(|scheduler| {
            let mut scheduler = scheduler.borrow_mut();
            scheduler.main_trap = trap;
            scheduler.main_exc = exc;
        });
        vm_fiber.run_state = FiberRunState::Running;
        let active = ActiveFiber {
            id,
            closure: vm_fiber.closure,
            resume_cause: vm_fiber.resume_cause,
            pending_park: None,
            gc_blocking_depth: vm_fiber.gc_blocking_depth,
        };
        vm_fiber.resume_cause = ResumeCause::Scheduled;
        ACTIVE_FIBER.with(|slot| {
            debug_assert!(slot.get().is_none());
            slot.set(Some(active));
        });
        notify_switch(0, id);

        vm_fiber.fiber.resume();

        // Publish the suspended fiber stack before the host switch hook can
        // publish interpreter roots. `hlp_gc_scan_roots_done` is allowed to
        // honor a pending collection, so doing this after `notify_switch`
        // left the just-yielded compiled frames invisible during precisely
        // that collection. Under GC stress this reclaimed values held by a
        // worker loop (for example its linked-list head) before the fiber was
        // resumed.
        crate::gc::gc_update_fiber_sp(id, vm_fiber.fiber.saved_sp() as usize);
        notify_switch(id, 0);
        let active = ACTIVE_FIBER
            .with(|slot| slot.replace(None))
            .expect("resumed fiber lost its active state");
        vm_fiber.gc_blocking_depth = active.gc_blocking_depth;
        let (mut trap, mut exc) = SCHEDULER.with(|scheduler| {
            let scheduler = scheduler.borrow();
            (scheduler.main_trap, scheduler.main_exc)
        });
        crate::gc::gc_swap_exc_state(&mut trap, &mut exc);
        vm_fiber.trap_head = trap;
        vm_fiber.exc_value = exc;

        let state = vm_fiber.fiber.state();
        if matches!(state, FiberState::Done | FiberState::Errored) {
            remove_fiber(vm_fiber, state);
        } else if let Some(request) = active.pending_park {
            debug_assert_eq!(request.waiter.fiber_id, id);
            vm_fiber.run_state = FiberRunState::Waiting(request.waiter.token);
            SCHEDULER.with(|scheduler| {
                let mut scheduler = scheduler.borrow_mut();
                if let Some(deadline) = request.deadline {
                    scheduler
                        .timers
                        .push(Reverse((deadline, request.waiter.token, id)));
                }
                scheduler.fibers.push(vm_fiber);
            });
        } else {
            vm_fiber.run_state = FiberRunState::Runnable;
            SCHEDULER.with(|scheduler| {
                let mut scheduler = scheduler.borrow_mut();
                scheduler.fibers.push(vm_fiber);
                scheduler.enqueue_ready(id);
            });
        }
        wake_due_timers();
    }
    resumed
}

/// Cooperative scheduling safe point for a running VM.
///
/// Blocking primitives already call [`block_yield`], but a main-thread event
/// loop can run forever without touching one of them. In that case every
/// worker fiber remains suspended at its first blocking point even after work
/// is queued for it. The AIR V2 interpreter calls this helper at bounded
/// execution safe points; compiled tiers can use the same scheduling hook at
/// their own safe points.
///
/// A safe point reached on the main stack advances each worker once. A safe
/// point reached by a worker yields that worker back to the scheduler. There
/// is deliberately no sleep or SDL pump here: the VM that owns the safe point
/// is still runnable and remains responsible for its own frame pacing.
#[no_mangle]
pub unsafe extern "C" fn hlp_fiber_poll() {
    crate::gc::gc_safepoint();
    if !fibers_active() {
        return;
    }
    if ACTIVE_FIBER.with(|active| active.get().is_some()) {
        krio_fiber::yield_now();
    } else {
        schedule_step();
    }
}

/// Universal "I am blocked" primitive: on a fiber, yield to the scheduler;
/// on the main context, run other fibers and take a short pacing nap.
pub(crate) unsafe fn block_yield() {
    if ACTIVE_FIBER.with(|active| active.get().is_some()) {
        krio_fiber::yield_now();
    } else {
        crate::gc::gc_safepoint();
        schedule_step();
        // Always pace after a pass: a resumed-but-still-blocked fiber yields
        // instantly, and treating that as progress spins main and every
        // blocked fiber at 100% CPU ping-pong. 1ms keeps the old cadence
        // without burning a core.
        // std's sleep rather than nanosleep: same syscall on unix, plus the
        // EINTR retry a pacing nap wants anyway, and no Win32 fork. Windows
        // may round the wait up to the OS timer resolution — acceptable,
        // because this is pacing and not a deadline.
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}
