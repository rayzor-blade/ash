//! Thread-specific data for a wasm module whose libraries ask for pthread keys.
//!
//! A side module built with a current Rust nightly registers its thread-local
//! destructors with pthread keys, so it imports `pthread_key_create` and its
//! companions from the program that hosts it. wasi-libc defines them, but its
//! `pthread_key_delete` walks a circular list of threads that this runtime does
//! not maintain, and spins on the first call. They are answered here instead.
//!
//! Which keys exist is process-wide; the value under a key belongs to the
//! thread that set it. A key carries a generation, bumped when it is created,
//! so a value stored under a deleted key is not seen under the key that reuses
//! its number. Destructors are recorded and never run: nothing on this target
//! ends a thread through pthreads, so a value lives as long as its thread.

use std::cell::RefCell;
use std::ffi::c_void;
use std::sync::Mutex;

const KEYS_MAX: usize = 128;
#[cfg_attr(not(any(test, target_family = "wasm")), allow(dead_code))]
const EAGAIN: i32 = 11;
#[cfg_attr(not(any(test, target_family = "wasm")), allow(dead_code))]
const EINVAL: i32 = 22;

type Destructor = Option<unsafe extern "C" fn(*mut c_void)>;

#[derive(Clone, Copy)]
struct Key {
    generation: u32,
    in_use: bool,
    // Kept so the table holds everything the C API stores; see the module doc.
    #[allow(dead_code)]
    destructor: Destructor,
}

struct Table([Key; KEYS_MAX]);

// The destructors are code addresses, never called here.
unsafe impl Send for Table {}

impl Table {
    const EMPTY: Table = Table(
        [Key {
            generation: 0,
            in_use: false,
            destructor: None,
        }; KEYS_MAX],
    );

    fn create(&mut self, destructor: Destructor) -> Result<u32, i32> {
        let index = self.0.iter().position(|k| !k.in_use).ok_or(EAGAIN)?;
        let key = &mut self.0[index];
        key.in_use = true;
        key.generation = key.generation.wrapping_add(1);
        key.destructor = destructor;
        Ok(index as u32)
    }
}

static TABLE: Mutex<Table> = Mutex::new(Table::EMPTY);

#[derive(Clone, Copy)]
struct Value {
    generation: u32,
    value: *mut c_void,
}

thread_local! {
    // No drop glue, so this registers no destructor of its own -- which a
    // current std would do with a pthread key, and these ARE the pthread keys.
    static VALUES: RefCell<[Value; KEYS_MAX]> = const {
        RefCell::new([Value { generation: 0, value: std::ptr::null_mut() }; KEYS_MAX])
    };
}

fn table() -> std::sync::MutexGuard<'static, Table> {
    TABLE.lock().unwrap_or_else(|e| e.into_inner())
}

fn create(destructor: Destructor) -> Result<u32, i32> {
    table().create(destructor)
}

fn delete(key: u32) -> i32 {
    match table().0.get_mut(key as usize).filter(|k| k.in_use) {
        Some(k) => {
            k.in_use = false;
            0
        }
        None => EINVAL,
    }
}

/// The generation of `key` if it exists.
fn generation(key: u32) -> Option<u32> {
    table()
        .0
        .get(key as usize)
        .filter(|k| k.in_use)
        .map(|k| k.generation)
}

fn set(key: u32, value: *mut c_void) -> i32 {
    let Some(generation) = generation(key) else {
        return EINVAL;
    };
    VALUES.with(|values| values.borrow_mut()[key as usize] = Value { generation, value });
    0
}

fn get(key: u32) -> *mut c_void {
    let Some(generation) = generation(key) else {
        return std::ptr::null_mut();
    };
    VALUES.with(|values| {
        let stored = values.borrow()[key as usize];
        if stored.generation == generation {
            stored.value
        } else {
            std::ptr::null_mut()
        }
    })
}

#[cfg(target_family = "wasm")]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn pthread_key_create(key: *mut u32, destructor: Destructor) -> i32 {
    match create(destructor) {
        Ok(index) => {
            unsafe { *key = index };
            0
        }
        Err(error) => error,
    }
}

#[cfg(target_family = "wasm")]
#[unsafe(no_mangle)]
pub extern "C" fn pthread_key_delete(key: u32) -> i32 {
    delete(key)
}

#[cfg(target_family = "wasm")]
#[unsafe(no_mangle)]
pub extern "C" fn pthread_setspecific(key: u32, value: *const c_void) -> i32 {
    set(key, value as *mut c_void)
}

#[cfg(target_family = "wasm")]
#[unsafe(no_mangle)]
pub extern "C" fn pthread_getspecific(key: u32) -> *mut c_void {
    get(key)
}

#[cfg(test)]
mod tests {
    use super::*;

    // The table is process-wide and the tests share it, so each one makes its
    // own keys and deletes them.

    #[test]
    fn a_value_is_returned_under_its_key_and_gone_after_the_key_is_deleted() {
        let key = create(None).unwrap();
        assert!(get(key).is_null(), "a new key holds null");
        assert_eq!(set(key, std::ptr::without_provenance_mut::<c_void>(42)), 0);
        assert_eq!(get(key) as usize, 42);
        assert_eq!(delete(key), 0);
        assert!(get(key).is_null());
        assert_eq!(
            set(key, std::ptr::without_provenance_mut::<c_void>(1)),
            EINVAL
        );
        assert_eq!(delete(key), EINVAL, "a key is deleted once");
    }

    #[test]
    fn a_reused_key_number_does_not_show_the_old_value() {
        let first = create(None).unwrap();
        set(first, std::ptr::without_provenance_mut::<c_void>(7));
        delete(first);
        // Usually the same number, unless another test took it meanwhile.
        let second = create(None).unwrap();
        assert!(get(second).is_null(), "a reused number starts empty");
        delete(second);
    }

    #[test]
    fn a_value_belongs_to_the_thread_that_set_it() {
        let key = create(None).unwrap();
        set(key, std::ptr::without_provenance_mut::<c_void>(5));
        let seen = std::thread::spawn(move || {
            let before = get(key) as usize;
            set(key, std::ptr::without_provenance_mut::<c_void>(9));
            (before, get(key) as usize)
        })
        .join()
        .unwrap();
        assert_eq!(seen, (0, 9));
        assert_eq!(get(key) as usize, 5);
        delete(key);
    }

    #[test]
    fn running_out_of_keys_is_eagain() {
        let mut full = Table::EMPTY;
        for expected in 0..KEYS_MAX as u32 {
            assert_eq!(full.create(None), Ok(expected));
        }
        assert_eq!(full.create(None), Err(EAGAIN));
    }

    #[test]
    fn a_key_that_does_not_exist_is_einval_and_reads_null() {
        assert_eq!(set(KEYS_MAX as u32, std::ptr::null_mut()), EINVAL);
        assert_eq!(delete(KEYS_MAX as u32), EINVAL);
        assert!(get(u32::MAX).is_null());
    }
}
