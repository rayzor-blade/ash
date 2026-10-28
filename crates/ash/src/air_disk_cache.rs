//! Persistent AIR reuse for AOT builds. Entries contain no backend objects or addresses.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Write as _};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Instant, SystemTime};

use air::v2::Function;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::bytecode::DecodedBytecode;
use crate::types::HLFunction;

const SCHEMA: u32 = 1;
const MAGIC: &[u8; 8] = b"ASHAIR01";
const MAX_ENTRY: u64 = 16 * 1024 * 1024;
type Hash = [u8; 32];

struct HashWriter(Sha256);
impl std::fmt::Write for HashWriter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.update(s.as_bytes());
        Ok(())
    }
}
pub(crate) fn hash_debug(value: &impl Debug) -> Hash {
    let mut writer = HashWriter(Sha256::new());
    write!(writer, "{value:?}").unwrap();
    writer.0.finalize().into()
}
fn hex(hash: &Hash) -> String {
    hash.iter().map(|b| format!("{b:02x}")).collect()
}

#[derive(Clone, Debug)]
pub struct Options {
    pub directory: Option<PathBuf>,
    pub budget_bytes: u64,
    pub enabled: bool,
    pub stats: bool,
}
impl Default for Options {
    fn default() -> Self {
        let directory = std::env::var_os("ASH_AIR_DISK_CACHE_DIR")
            .map(PathBuf::from)
            .or_else(|| {
                let base = if cfg!(target_os = "windows") {
                    std::env::var_os("LOCALAPPDATA").map(PathBuf::from)
                } else if let Some(base) = std::env::var_os("XDG_CACHE_HOME") {
                    Some(PathBuf::from(base))
                } else {
                    std::env::var_os("HOME").map(|home| {
                        PathBuf::from(home).join(if cfg!(target_os = "macos") {
                            "Library/Caches"
                        } else {
                            ".cache"
                        })
                    })
                };
                base.map(|base| base.join("ash/air"))
            });
        Self {
            directory,
            budget_bytes: std::env::var("ASH_AIR_DISK_CACHE_MB")
                .ok()
                .and_then(|s| s.parse::<u64>().ok())
                .unwrap_or(512)
                .saturating_mul(1024 * 1024),
            enabled: std::env::var("ASH_AIR_DISK_CACHE").as_deref() != Ok("0"),
            stats: std::env::var("ASH_AIR_DISK_CACHE_STATS")
                .is_ok_and(|s| s != "0" && !s.is_empty()),
        }
    }
}
static OPTIONS: OnceLock<Mutex<Option<Options>>> = OnceLock::new();
/// Configure before starting a build. Library callers may instead use the environment.
pub fn configure(options: Options) {
    *OPTIONS.get_or_init(|| Mutex::new(None)).lock().unwrap() = Some(options);
}
pub fn options() -> Options {
    OPTIONS
        .get_or_init(|| Mutex::new(None))
        .lock()
        .unwrap()
        .clone()
        .unwrap_or_default()
}

#[derive(Debug)]
pub struct Inspection {
    pub directory: Option<PathBuf>,
    pub entries: usize,
    pub bytes: u64,
    pub budget_bytes: u64,
    pub compiler_revision: &'static str,
    pub enabled: bool,
}
pub fn inspect() -> io::Result<Inspection> {
    let opts = options();
    let mut entries = 0;
    let mut bytes = 0;
    if let Some(root) = &opts.directory {
        for (_, size, _) in files(&root.join("entries"))? {
            entries += 1;
            bytes += size;
        }
    }
    Ok(Inspection {
        directory: opts.directory,
        entries,
        bytes,
        budget_bytes: opts.budget_bytes,
        compiler_revision: env!("ASH_AIR_REVISION"),
        enabled: opts.enabled && opts.budget_bytes != 0,
    })
}
/// Keep the lock inode in place: a writer waiting on it must still serialize with the next one.
pub fn clear() -> io::Result<()> {
    if let Some(root) = options().directory {
        let store = Store { root, budget: 0 };
        let mut lock = store.lock()?;
        lock.dirty()?;
        match fs::remove_dir_all(store.root.join("entries")) {
            Ok(()) => {}
            Err(e) if e.kind() == io::ErrorKind::NotFound => {}
            Err(e) => return Err(e),
        }
        lock.finish(0)?;
    }
    Ok(())
}

fn files(dir: &Path) -> io::Result<Vec<(PathBuf, u64, SystemTime)>> {
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(e),
    };
    let mut out = Vec::new();
    for entry in entries {
        let entry = entry?;
        let path = entry.path();
        if path.extension().is_some_and(|s| s == "air") && entry.file_type()?.is_file() {
            let meta = entry.metadata()?;
            out.push((
                path,
                meta.len(),
                meta.modified().unwrap_or(SystemTime::UNIX_EPOCH),
            ));
        }
    }
    Ok(out)
}

struct Store {
    root: PathBuf,
    budget: u64,
}
struct Lock {
    file: File,
    used: Option<u64>,
}
impl Lock {
    fn dirty(&mut self) -> io::Result<()> {
        self.file.set_len(0)
    }
    fn finish(&mut self, used: u64) -> io::Result<()> {
        self.file.seek(SeekFrom::Start(0))?;
        self.file.write_all(&used.to_le_bytes())?;
        self.file.write_all(&(!used).to_le_bytes())?;
        self.file.set_len(16)
    }
}
impl Store {
    fn lock(&self) -> io::Result<Lock> {
        fs::create_dir_all(&self.root)?;
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(self.root.join(".lock"))?;
        file.lock()?;
        let mut bytes = Vec::new();
        Read::by_ref(&mut file).take(17).read_to_end(&mut bytes)?;
        let used = (bytes.len() == 16)
            .then(|| {
                let n = u64::from_le_bytes(bytes[..8].try_into().unwrap());
                (u64::from_le_bytes(bytes[8..].try_into().unwrap()) == !n).then_some(n)
            })
            .flatten();
        Ok(Lock { file, used })
    }
    fn path(&self, key: &Hash) -> PathBuf {
        self.root.join("entries").join(format!("{}.air", hex(key)))
    }
    fn load(&self, key: &Hash) -> io::Result<Option<Entry>> {
        let file = match File::open(self.path(key)) {
            Ok(file) => file,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(e),
        };
        if file.metadata()?.len() > MAX_ENTRY {
            return Err(io::Error::other("oversized AIR entry"));
        }
        let mut bytes = Vec::new();
        (&file).take(MAX_ENTRY + 1).read_to_end(&mut bytes)?;
        if bytes.len() < 40
            || bytes.len() as u64 > MAX_ENTRY
            || &bytes[..8] != MAGIC
            || Sha256::digest(&bytes[40..]).as_slice() != &bytes[8..40]
        {
            return Err(io::Error::other("invalid AIR entry checksum"));
        }
        let (entry, rest): (Entry, _) =
            postcard::take_from_bytes(&bytes[40..]).map_err(io::Error::other)?;
        if !rest.is_empty() || entry.schema != SCHEMA || &entry.key != key {
            return Err(io::Error::other("incompatible AIR entry"));
        }
        let _ = file.set_modified(SystemTime::now());
        Ok(Some(entry))
    }
    fn store(&self, entry: &impl Serialize, key: &Hash) -> io::Result<(bool, usize)> {
        let payload = postcard::to_stdvec(entry).map_err(io::Error::other)?;
        let size = payload.len() as u64 + 40;
        if size > MAX_ENTRY || size > self.budget {
            return Ok((false, 0));
        }
        let mut lock = self.lock()?;
        let dir = self.root.join("entries");
        fs::create_dir_all(&dir)?;
        let mut used = match lock.used {
            Some(n) => n,
            None => {
                // Only writers create staging files, always under this lock.
                for entry in fs::read_dir(&dir)? {
                    let entry = entry?;
                    if entry.file_name().to_string_lossy().starts_with(".tmp") {
                        let _ = fs::remove_file(entry.path());
                    }
                }
                files(&dir)?.iter().map(|(_, n, _)| n).sum()
            }
        };
        lock.dirty()?;
        let path = self.path(key);
        let old = fs::metadata(&path).map_or(0, |m| m.len());
        let mut evictions = 0;
        if used.saturating_sub(old).saturating_add(size) > self.budget {
            let mut candidates = files(&dir)?;
            used = candidates.iter().map(|(_, n, _)| n).sum();
            candidates.sort_by_key(|(_, _, age)| *age);
            let target = self.budget.saturating_mul(9) / 10;
            for (candidate, bytes, _) in candidates {
                if used.saturating_sub(old).saturating_add(size) <= target {
                    break;
                }
                if candidate != path && fs::remove_file(candidate).is_ok() {
                    used = used.saturating_sub(bytes);
                    evictions += 1;
                }
            }
        }
        let mut temp = tempfile::NamedTempFile::new_in(&dir)?;
        temp.write_all(MAGIC)?;
        temp.write_all(&Sha256::digest(&payload))?;
        temp.write_all(&payload)?;
        temp.flush()?;
        temp.persist(&path).map_err(|e| e.error)?;
        lock.finish(used.saturating_sub(old).saturating_add(size))?;
        Ok((true, evictions))
    }
}

#[derive(Serialize, Deserialize)]
struct Entry {
    schema: u32,
    key: Hash,
    dependencies: Vec<(usize, Hash)>,
    ir: Function,
}

#[derive(Serialize)]
struct EntryRef<'a> {
    schema: u32,
    key: Hash,
    dependencies: Vec<(usize, Hash)>,
    ir: &'a Function,
}

#[derive(Default)]
struct Stats {
    lower_hits: usize,
    optimized_hits: usize,
    misses: usize,
    invalid: usize,
    written: usize,
    evictions: usize,
    errors: usize,
    identity_ns: u128,
    read_ns: u128,
    write_ns: u128,
    lower_ns: u128,
    passes_ns: u128,
    verify_ns: u128,
}
struct Session {
    store: Arc<Store>,
    enabled: bool,
    identity: Hash,
    stats: Arc<Mutex<Stats>>,
    report: bool,
    modules: Mutex<HashMap<(usize, u64), Arc<ModuleCache>>>,
}
thread_local! { static SESSION: RefCell<Option<Arc<Session>>> = const { RefCell::new(None) }; }
pub(crate) struct BuildGuard(Option<Arc<Session>>);
impl Drop for BuildGuard {
    fn drop(&mut self) {
        let session = SESSION.with(|s| s.replace(self.0.take()));
        if let Some(session) = session.filter(|s| s.report) {
            let s = session.stats.lock().unwrap();
            eprintln!(
                "[air-disk-cache] enabled={} lower_hits={} optimized_hits={} misses={} invalid={} written={} evictions={} errors={} identity_ms={:.3} read_ms={:.3} write_ms={:.3} lower_ms={:.3} passes_ms={:.3} verify_ms={:.3}",
                session.enabled,
                s.lower_hits,
                s.optimized_hits,
                s.misses,
                s.invalid,
                s.written,
                s.evictions,
                s.errors,
                s.identity_ns as f64 / 1e6,
                s.read_ns as f64 / 1e6,
                s.write_ns as f64 / 1e6,
                s.lower_ns as f64 / 1e6,
                s.passes_ns as f64 / 1e6,
                s.verify_ns as f64 / 1e6
            );
            #[cfg(unix)]
            unsafe {
                let mut usage = std::mem::zeroed::<libc::rusage>();
                if libc::getrusage(libc::RUSAGE_SELF, &mut usage) == 0 {
                    let scale = if cfg!(target_os = "macos") { 1 } else { 1024 };
                    eprintln!(
                        "[aot-time] peak_rss_bytes={}",
                        (usage.ru_maxrss as u64).saturating_mul(scale)
                    );
                }
            }
        }
    }
}
/// An AOT build's target/ABI/profile/host contract, established before bytecode preparation.
pub(crate) fn enter(contract: &impl Debug, allowed: bool) -> BuildGuard {
    let opts = options();
    let enabled = opts.enabled && opts.budget_bytes != 0 && opts.directory.is_some();
    let session = if allowed {
        Some({
            Arc::new(Session {
                store: Arc::new(Store {
                    root: opts.directory.unwrap_or_default(),
                    budget: opts.budget_bytes,
                }),
                enabled,
                identity: hash_debug(&(SCHEMA, env!("ASH_AIR_REVISION"), contract)),
                stats: Arc::new(Mutex::new(Stats::default())),
                report: opts.stats,
                modules: Mutex::new(HashMap::new()),
            })
        })
    } else {
        None
    };
    BuildGuard(SESSION.with(|s| s.replace(session)))
}

pub(crate) struct ModuleCache {
    store: Arc<Store>,
    enabled: bool,
    stats: Arc<Mutex<Stats>>,
    identity: Hash,
    functions: BTreeMap<usize, Hash>,
    pub(crate) native_at: Arc<HashMap<usize, usize>>,
    pub(crate) function_at: Arc<HashMap<usize, usize>>,
    pub(crate) lowered: Arc<Mutex<crate::air_cache::BodyCache<usize, Function>>>,
    pub(crate) frameless: Arc<Mutex<HashMap<usize, bool>>>,
}
impl ModuleCache {
    fn new(
        session: &Session,
        bc: &DecodedBytecode,
        identity: Hash,
        functions: BTreeMap<usize, Hash>,
    ) -> Self {
        Self {
            store: Arc::clone(&session.store),
            enabled: session.enabled,
            stats: Arc::clone(&session.stats),
            identity,
            functions,
            native_at: Arc::new(
                bc.natives
                    .iter()
                    .enumerate()
                    .map(|(i, n)| (n.findex as usize, i))
                    .collect(),
            ),
            function_at: Arc::new(
                bc.functions
                    .iter()
                    .enumerate()
                    .map(|(i, f)| (f.findex as usize, i))
                    .collect(),
            ),
            lowered: Arc::new(Mutex::new(crate::air_cache::BodyCache::new(
                2 * 1024 * 1024,
            ))),
            frameless: Arc::new(Mutex::new(HashMap::new())),
        }
    }
    pub(crate) fn enabled(&self) -> bool {
        self.enabled
    }
    pub(crate) fn for_module(bc: &DecodedBytecode) -> Option<Arc<Self>> {
        SESSION.with(|current| {
            let session = current.borrow().clone()?;
            if !bc.host_natives.is_empty()
                || !bc.host_classes.is_empty()
                || !bc.host_modules.is_empty()
                || bc.reload_init.is_some()
            {
                return None;
            }
            let key = (bc as *const _ as usize, crate::generated::generation(bc));
            let mut modules = session.modules.lock().unwrap();
            if let Some(module) = modules.get(&key) {
                return Some(Arc::clone(module));
            }
            if !session.enabled {
                let module = Arc::new(Self::new(&session, bc, [0; 32], BTreeMap::new()));
                modules.insert(key, Arc::clone(&module));
                return Some(module);
            }
            let began = Instant::now();
            let mut writer = HashWriter(Sha256::new());
            write!(
                writer,
                "{:?}",
                (
                    &session.identity,
                    &bc.ints,
                    &bc.strings,
                    &bc.bytes_pos,
                    &bc.bytes_data,
                    (
                        &bc.types,
                        &bc.globals,
                        &bc.natives,
                        &bc.constants,
                        &bc.debug_files,
                        bc.has_debug,
                        bc.entrypoint,
                        bc.tree_fusion_runtime
                    )
                )
            )
            .unwrap();
            writer.0.update((bc.floats.len() as u64).to_le_bytes());
            for f in &bc.floats {
                writer.0.update(f.to_bits().to_le_bytes());
            }
            let mut stack_readers: Vec<_> = bc.body_facts().stack_readers.iter().copied().collect();
            stack_readers.sort_unstable();
            write!(writer, "{stack_readers:?}").unwrap();
            for f in &bc.functions {
                write!(writer, "{:?}", (f.findex, &f.type_, &f.regs)).unwrap();
            }
            let functions = bc
                .functions
                .iter()
                .map(|f| {
                    (
                        f.findex as usize,
                        hash_debug(&(
                            f.findex,
                            &f.type_,
                            &f.regs,
                            f.body().cache_digest(),
                            &f.obj,
                            &f.field_name,
                            f.ref_,
                        )),
                    )
                })
                .collect();
            let module = Arc::new(Self::new(
                &session,
                bc,
                writer.0.finalize().into(),
                functions,
            ));
            session.stats.lock().unwrap().identity_ns += began.elapsed().as_nanos();
            modules.insert(key, Arc::clone(&module));
            Some(module)
        })
    }
    pub(crate) fn key(&self, f: &HLFunction, pipeline: &impl Debug) -> Hash {
        let environment: Vec<_> = [
            "ASH_AIR_SPLIT_CALLS",
            "ASH_AIR_NO_INLINE",
            "ASH_NO_UNBOX",
            "ASH_AIR_NO_CELLDSE",
            "ASH_STRIP_MINE",
            "ASH_AIR_NO_WIDEN",
            "ASH_TRACE_LINES",
        ]
        .iter()
        .map(|key| (*key, std::env::var_os(key)))
        .collect();
        hash_debug(&(
            self.identity,
            self.functions.get(&(f.findex as usize)),
            pipeline,
            environment,
            crate::air_pipeline::shadow_frames(),
        ))
    }
    pub(crate) fn load(
        &self,
        key: &Hash,
        findex: Option<usize>,
        optimized: bool,
    ) -> Option<Function> {
        if !self.enabled {
            return None;
        }
        let began = Instant::now();
        let result = self.store.load(key);
        self.stats.lock().unwrap().read_ns += began.elapsed().as_nanos();
        let entry = match result {
            Ok(Some(entry)) => entry,
            Ok(None) => {
                self.stats.lock().unwrap().misses += 1;
                return None;
            }
            Err(_) => {
                self.stats.lock().unwrap().invalid += 1;
                return None;
            }
        };
        if entry
            .dependencies
            .iter()
            .any(|(id, hash)| self.functions.get(id) != Some(hash))
            || entry.ir.findex != findex
        {
            self.stats.lock().unwrap().misses += 1;
            return None;
        }
        let began = Instant::now();
        let valid = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            air::v2::verify(&entry.ir).is_ok()
        }))
        .unwrap_or(false);
        let mut stats = self.stats.lock().unwrap();
        stats.verify_ns += began.elapsed().as_nanos();
        if !valid {
            stats.invalid += 1;
            return None;
        }
        if optimized {
            stats.optimized_hits += 1;
        } else {
            stats.lower_hits += 1;
        }
        Some(entry.ir)
    }
    pub(crate) fn save(
        &self,
        key: Hash,
        ir: &Function,
        dependencies: impl IntoIterator<Item = usize>,
    ) {
        if !self.enabled {
            return;
        }
        let dependencies = dependencies
            .into_iter()
            .filter_map(|id| self.functions.get(&id).map(|hash| (id, *hash)))
            .collect();
        let began = Instant::now();
        let result = self.store.store(
            &EntryRef {
                schema: SCHEMA,
                key,
                dependencies,
                ir,
            },
            &key,
        );
        let mut stats = self.stats.lock().unwrap();
        stats.write_ns += began.elapsed().as_nanos();
        match result {
            Ok((written, evictions)) => {
                stats.written += usize::from(written);
                stats.evictions += evictions;
            }
            Err(_) => stats.errors += 1,
        }
    }
    pub(crate) fn timing(&self, lower_ns: u128, passes_ns: u128, verify_ns: u128) {
        let mut stats = self.stats.lock().unwrap();
        stats.lower_ns += lower_ns;
        stats.passes_ns += passes_ns;
        stats.verify_ns += verify_ns;
    }
}

/// Stage wall time, separate from the cumulative per-function AIR timings.
pub(crate) fn stage(name: &str, elapsed: std::time::Duration) {
    if options().stats {
        eprintln!("[aot-time] {name}_ms={:.3}", elapsed.as_secs_f64() * 1000.0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::air_pipeline::{self, AirConfigKey, AshModule};
    use crate::opcodes::{Opcode, RefFun, RefInt, Reg};
    use crate::types::{HLType, HLTypeFun, TypeRef};

    fn body() -> Function {
        air::v2::lower(&[Opcode::Ret { ret: Reg(0) }], &[air::v2::TypeRef(0)]).unwrap()
    }
    fn entry(id: u8) -> Entry {
        Entry {
            schema: SCHEMA,
            key: [id; 32],
            dependencies: Vec::new(),
            ir: body(),
        }
    }
    fn activate(root: &Path, contract: &str) -> BuildGuard {
        let session = Arc::new(Session {
            store: Arc::new(Store {
                root: root.into(),
                budget: 1024 * 1024,
            }),
            enabled: true,
            identity: hash_debug(&(SCHEMA, env!("ASH_AIR_REVISION"), contract)),
            stats: Arc::new(Mutex::new(Stats::default())),
            report: false,
            modules: Mutex::new(HashMap::new()),
        });
        BuildGuard(SESSION.with(|s| s.replace(Some(session))))
    }
    fn stats() -> (usize, usize) {
        SESSION.with(|s| {
            let s = s.borrow();
            let stats = s.as_ref().unwrap().stats.lock().unwrap();
            (stats.optimized_hits, stats.misses)
        })
    }
    fn program() -> DecodedBytecode {
        let mut bc = DecodedBytecode::default();
        bc.ints = vec![7];
        bc.types = vec![
            HLType {
                kind: crate::hl::hl_type_kind_HVOID,
                ..Default::default()
            },
            HLType {
                kind: crate::hl::hl_type_kind_HI32,
                ..Default::default()
            },
            HLType {
                kind: crate::hl::hl_type_kind_HFUN,
                fun: Some(HLTypeFun {
                    args: vec![],
                    ret: TypeRef(1),
                    ..Default::default()
                }),
                ..Default::default()
            },
        ];
        let mut root = HLFunction::with_body(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Call1 {
                    dst: Reg(1),
                    fun: RefFun(1),
                    arg0: Reg(0),
                },
                Opcode::Ret { ret: Reg(1) },
            ],
            vec![],
        );
        root.type_ = TypeRef(2);
        root.regs = vec![TypeRef(1), TypeRef(1)];
        root.findex = 0;
        let mut callee = HLFunction::with_body(
            vec![
                Opcode::Add {
                    dst: Reg(1),
                    a: Reg(0),
                    b: Reg(0),
                },
                Opcode::Ret { ret: Reg(1) },
            ],
            vec![],
        );
        callee.type_ = TypeRef(2);
        callee.regs = root.regs.clone();
        callee.findex = 1;
        let mut other = HLFunction::with_body(
            vec![
                Opcode::Int {
                    dst: Reg(0),
                    ptr: RefInt(0),
                },
                Opcode::Ret { ret: Reg(0) },
            ],
            vec![],
        );
        other.type_ = TypeRef(2);
        other.regs = vec![TypeRef(1)];
        other.findex = 2;
        bc.functions = vec![root, callee, other];
        bc
    }

    #[test]
    fn lossless_roundtrip_and_corruption_are_misses() {
        let dir = tempfile::tempdir().unwrap();
        let store = Store {
            root: dir.path().into(),
            budget: 4096,
        };
        let original = entry(1);
        store.store(&original, &original.key).unwrap();
        let cached = store.load(&original.key).unwrap().unwrap();
        air::v2::verify(&cached.ir).unwrap();
        assert_eq!(format!("{:?}", original.ir), format!("{:?}", cached.ir));
        let path = store.path(&original.key);
        let mut bytes = fs::read(&path).unwrap();
        bytes[45] ^= 1;
        fs::write(&path, bytes).unwrap();
        assert!(store.load(&original.key).is_err());
        fs::write(&path, b"partial").unwrap();
        assert!(store.load(&original.key).is_err());
        let mut old = entry(1);
        old.schema += 1;
        store.store(&old, &old.key).unwrap();
        assert!(store.load(&old.key).is_err());
        assert!(store.load(&[2; 32]).unwrap().is_none());
    }

    #[test]
    fn concurrent_writers_obey_budget_and_recover_interrupted_ledger() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        let workers: Vec<_> = (0..4)
            .map(|worker| {
                let root = root.clone();
                std::thread::spawn(move || {
                    let store = Store { root, budget: 2048 };
                    for i in 0..24 {
                        let entry = entry((i + worker * 24) as u8);
                        store.store(&entry, &entry.key).unwrap();
                    }
                })
            })
            .collect();
        for worker in workers {
            worker.join().unwrap();
        }
        let entries = files(&root.join("entries")).unwrap();
        assert!(!entries.is_empty());
        assert!(entries.iter().map(|(_, n, _)| n).sum::<u64>() <= 2048);
        for (path, _, _) in &entries {
            let key: Vec<u8> = path
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .as_bytes()
                .chunks(2)
                .map(|s| u8::from_str_radix(std::str::from_utf8(s).unwrap(), 16).unwrap())
                .collect();
            assert!(
                Store {
                    root: root.clone(),
                    budget: 2048
                }
                .load(&key.try_into().unwrap())
                .unwrap()
                .is_some()
            );
        }
        fs::write(root.join(".lock"), b"interrupted").unwrap();
        fs::write(root.join("entries/.tmp-abandoned"), b"partial").unwrap();
        Store {
            root: root.clone(),
            budget: 512,
        }
        .store(&entry(250), &[250; 32])
        .unwrap();
        assert!(!root.join("entries/.tmp-abandoned").exists());
        assert!(
            files(&root.join("entries"))
                .unwrap()
                .iter()
                .map(|(_, n, _)| n)
                .sum::<u64>()
                <= 512
        );
    }

    #[test]
    fn callee_edit_invalidates_its_callers_but_preserves_unrelated_functions() {
        let dir = tempfile::tempdir().unwrap();
        let bc = program();
        let cfg = AirConfigKey::standard();
        let first = {
            let _guard = activate(dir.path(), "native");
            let module = AshModule::new(&bc);
            air_pipeline::invalidate_optimized();
            let root = air_pipeline::optimized_with_config(&module, &bc.functions[0], cfg).unwrap();
            air_pipeline::optimized_with_config(&module, &bc.functions[2], cfg).unwrap();
            assert!(
                !root
                    .ir
                    .blocks
                    .iter()
                    .flat_map(|b| &b.instrs)
                    .any(|i| matches!(i, air::v2::Instr::Call { fun: 1, .. }))
            );
            format!("{:?}", root.ir)
        };
        {
            let _guard = activate(dir.path(), "native");
            air_pipeline::invalidate_optimized();
            let module = AshModule::new(&bc);
            let root = air_pipeline::optimized_with_config(&module, &bc.functions[0], cfg).unwrap();
            assert_eq!(first, format!("{:?}", root.ir));
            assert_eq!(stats().0, 1);
        }
        let mut edited = bc.clone();
        edited.functions[1].ops_mut()[0] = Opcode::Mul {
            dst: Reg(1),
            a: Reg(0),
            b: Reg(0),
        };
        edited.body_facts.take();
        {
            let _guard = activate(dir.path(), "native");
            air_pipeline::invalidate_optimized();
            let module = AshModule::new(&edited);
            let root =
                air_pipeline::optimized_with_config(&module, &edited.functions[0], cfg).unwrap();
            assert_ne!(first, format!("{:?}", root.ir));
            assert_eq!(stats().0, 0);
            air_pipeline::optimized_with_config(&module, &edited.functions[2], cfg).unwrap();
            assert_eq!(stats().0, 1);
        }
    }

    #[test]
    fn build_views_share_bounded_state_and_release_it_at_exit() {
        let dir = tempfile::tempdir().unwrap();
        let bc = program();
        let weak = {
            let _guard = activate(dir.path(), "native");
            let first = AshModule::new(&bc);
            let second = AshModule::new(&bc);
            let a = first.lowered_body(1).unwrap();
            let b = second.lowered_body(1).unwrap();
            assert!(Arc::ptr_eq(&a, &b));
            let module = ModuleCache::for_module(&bc).unwrap();
            let (_, _, resident) = module.lowered.lock().unwrap().storage();
            assert!(resident <= 2 * 1024 * 1024);
            Arc::downgrade(&module)
        };
        assert!(weak.upgrade().is_none());
    }

    #[test]
    fn cached_frameless_refusal_records_transitive_dependencies() {
        let dir = tempfile::tempdir().unwrap();
        let mut bc = program();
        bc.types.push(HLType {
            kind: crate::hl::hl_type_kind_HDYN,
            ..Default::default()
        });
        bc.functions[1].set_ops(vec![
            Opcode::Call1 {
                dst: Reg(1),
                fun: RefFun(2),
                arg0: Reg(0),
            },
            Opcode::Ret { ret: Reg(1) },
        ]);
        bc.functions[2].regs = vec![TypeRef(1), TypeRef(1), TypeRef(3)];
        bc.functions[2].set_ops(vec![
            Opcode::Null { dst: Reg(2) },
            Opcode::Throw { exc: Reg(2) },
        ]);
        let cfg = AirConfigKey {
            callees: air_pipeline::CalleeView::Frameless,
            ..AirConfigKey::standard()
        };
        let first = {
            let _guard = activate(dir.path(), "wasm");
            let module = AshModule::new(&bc);
            assert!(!module.frameless(1));
            air_pipeline::invalidate_optimized();
            let body = air_pipeline::optimized_with_config(&module, &bc.functions[0], cfg).unwrap();
            format!("{:?}", body.ir)
        };
        let mut edited = bc.clone();
        edited.functions[2].set_ops(vec![
            Opcode::Int {
                dst: Reg(0),
                ptr: RefInt(0),
            },
            Opcode::Ret { ret: Reg(0) },
        ]);
        edited.body_facts.take();
        let _guard = activate(dir.path(), "wasm");
        let module = AshModule::new(&edited);
        assert!(module.frameless(1));
        air_pipeline::invalidate_optimized();
        let body = air_pipeline::optimized_with_config(&module, &edited.functions[0], cfg).unwrap();
        assert_ne!(first, format!("{:?}", body.ir));
        assert_eq!(stats().0, 0);
    }

    #[test]
    fn keys_cover_metadata_target_profile_and_pipeline() {
        let dir = tempfile::tempdir().unwrap();
        let bc = program();
        let key = |bc: &DecodedBytecode, contract, pipeline| {
            let _guard = activate(dir.path(), contract);
            ModuleCache::for_module(bc)
                .unwrap()
                .key(&bc.functions[0], &pipeline)
        };
        let base = key(&bc, "native/abi1/profile1", "O3");
        assert_ne!(base, key(&bc, "wasm/abi1/profile1", "O3"));
        assert_ne!(base, key(&bc, "native/abi2/profile1", "O3"));
        assert_ne!(base, key(&bc, "native/abi1/profile2", "O3"));
        assert_ne!(base, key(&bc, "native/abi1/profile1", "O2"));
        let mut edited = bc.clone();
        edited.ints[0] += 1;
        assert_ne!(base, key(&edited, "native/abi1/profile1", "O3"));
        let mut edited = bc.clone();
        edited.types[1].kind = crate::hl::hl_type_kind_HF32;
        assert_ne!(base, key(&edited, "native/abi1/profile1", "O3"));
        let mut edited = bc.clone();
        edited.globals.push(TypeRef(1));
        assert_ne!(base, key(&edited, "native/abi1/profile1", "O3"));
        let mut edited = bc.clone();
        edited.natives.push(crate::types::HLNative {
            lib: "std".into(),
            name: "time".into(),
            ..Default::default()
        });
        assert_ne!(base, key(&edited, "native/abi1/profile1", "O3"));
    }

    #[test]
    fn structurally_invalid_entry_is_rejected_and_body_mutation_changes_digest() {
        let dir = tempfile::tempdir().unwrap();
        let bc = program();
        let _guard = activate(dir.path(), "native");
        let module = ModuleCache::for_module(&bc).unwrap();
        let key = module.key(&bc.functions[0], &"bad");
        let mut ir = body();
        ir.blocks.clear();
        module.save(key, &ir, []);
        assert!(module.load(&key, None, false).is_none());
        let mut function = bc.functions[1].clone();
        let original = function.body().cache_digest();
        function.ops_mut()[0] = Opcode::Mul {
            dst: Reg(1),
            a: Reg(0),
            b: Reg(0),
        };
        assert_ne!(original, function.body().cache_digest());
    }
}
