//! The `sqlite` HDLL, as a wasm side module.
//!
//! Its own crate and its own `.wasm`, loaded beside a program that asks for
//! it -- see `docs/wasm-hdlls.md`. Compiled into the runtime it cost 1.67 MB
//! of every module, hello world included, because a library present in the
//! runtime object is also reachable from it.
//!
//! Nothing of the runtime is linked in: it is reached through [`abi`], the C
//! ABI an HDLL has always used. `sys/db/Sqlite.hx` is not modified and cannot
//! tell the difference, which is the point.
//!
//! Nothing about SQLite needs a shared library -- it builds for
//! `wasm32-wasip1` unchanged -- so what was missing was never the code but a
//! way for a primitive to arrive. Each function below is exported as
//! `hlp_<name>`, the resolver `DEFINE_PRIM` would have written, and the
//! program finds it by name among this module's exports.
//!
//! # What the Haxe side expects
//!
//! Every string crossing this boundary is UTF-16: `getResult`,
//! `getFieldsNames` and the text branch of `doNext` all go through
//! `String.fromUCS2`. A value in a row is a `Dynamic` whose type decides how
//! it is read -- bytes are a string, an array is the `[data, length]` pair of
//! a BLOB, and anything else is taken as-is.
//!
//! # Where this differs from upstream
//!
//! `hl_sqlite_request` streams; this runs the statement and keeps the rows.
//! The reason is Rust's, not SQLite's: a `Statement` borrows its
//! `Connection`, so a handle holding both is self-referential. Materialising
//! costs memory on a large result and buys a `result_get_length` that does
//! not have to drain the cursor to answer.


mod abi;

/// This library allocates through the program. See [`abi::ProgramAllocator`].
#[global_allocator]
static ALLOCATOR: abi::ProgramAllocator = abi::ProgramAllocator;

/// A NUL-terminated UTF-16 string, as Rust.
///
/// The runtime has this too, but reaching it would mean linking the runtime
/// in, and it is six lines.
unsafe fn ucs2_to_string(p: *const u16) -> String {
    let mut units = Vec::new();
    let mut at = p;
    while *at != 0 {
        units.push(*at);
        at = at.add(1);
    }
    String::from_utf16_lossy(&units)
}

use std::ffi::c_void;
use std::os::raw::{c_char, c_int};

use crate::abi::{
    hl_aptr, hlp_alloc_array, hlp_alloc_bytes, hlp_alloc_dynamic, hlp_type_array, hlp_type_bytes,
    hlp_type_dyn, hlp_type_f64, hlp_type_i32, varray, vbyte, vdynamic,
};

/// Stamped into both handles. `hl.Abstract` is an untyped pointer on the VM
/// side, so a slot that was never opened, was closed, or holds something else
/// entirely arrives here indistinguishable from a live one.
const DB_MAGIC: u64 = 0x4153_485f_5351_4c44;
const RES_MAGIC: u64 = 0x4153_485f_5351_4c52;

struct Db {
    magic: u64,
    conn: rusqlite::Connection,
    /// Set by `request`, read by `last_id`. Kept here because the result of
    /// an INSERT is a different handle from the connection it came from.
    last_insert_id: i64,
}

/// One column of one row, owned so the statement can be dropped.
enum Cell {
    Null,
    Int(i64),
    Real(f64),
    Text(String),
    Blob(Vec<u8>),
}

struct Res {
    magic: u64,
    names: Vec<String>,
    rows: Vec<Vec<Cell>>,
    /// Index of the row `result_next` will return.
    next: usize,
    /// Index of the row it last returned, which is what `result_get` reads.
    current: Option<usize>,
    /// Rows changed, for a statement that returns none.
    changes: i32,
}

unsafe fn db<'a>(p: *mut c_void) -> Option<&'a mut Db> {
    let d = p as *mut Db;
    if d.is_null() || (*d).magic != DB_MAGIC {
        return None;
    }
    Some(&mut *d)
}

unsafe fn res<'a>(p: *mut c_void) -> Option<&'a mut Res> {
    let r = p as *mut Res;
    if r.is_null() || (*r).magic != RES_MAGIC {
        return None;
    }
    Some(&mut *r)
}

/// A string the VM handed over.
///
/// `hl.Bytes` taken from a `String` is UTF-16 -- `Sqlite.hx` passes
/// `s.bytes` straight through -- so this is not a C string, and reading it as
/// one stops at the first NUL, which for ASCII SQL is the second byte.
unsafe fn borrow_ucs2(p: *const vbyte) -> Option<String> {
    if p.is_null() {
        return None;
    }
    Some(ucs2_to_string(p as *const u16))
}

/// GC-allocated NUL-terminated UTF-16, which is what every string returned
/// from here is read as.
unsafe fn ucs2(s: &str) -> *mut vbyte {
    let units: Vec<u16> = s.encode_utf16().collect();
    let out = hlp_alloc_bytes(((units.len() + 1) * 2) as c_int);
    if out.is_null() {
        return std::ptr::null_mut();
    }
    let u = out as *mut u16;
    std::ptr::copy_nonoverlapping(units.as_ptr(), u, units.len());
    *u.add(units.len()) = 0;
    out
}

unsafe fn raw_bytes(data: &[u8]) -> *mut vbyte {
    let out = hlp_alloc_bytes(data.len() as c_int + 1);
    if out.is_null() {
        return std::ptr::null_mut();
    }
    std::ptr::copy_nonoverlapping(data.as_ptr(), out, data.len());
    *out.add(data.len()) = 0;
    out
}

unsafe fn box_int(v: i32) -> *mut vdynamic {
    let d = hlp_alloc_dynamic(hlp_type_i32());
    if !d.is_null() {
        (*d).v.i = v;
    }
    d
}

unsafe fn box_float(v: f64) -> *mut vdynamic {
    let d = hlp_alloc_dynamic(hlp_type_f64());
    if !d.is_null() {
        (*d).v.d = v;
    }
    d
}

/// A BLOB, as the `[data, length]` pair `doNext` unpacks for `haxe.io.Bytes`.
unsafe fn box_blob(data: &[u8]) -> *mut vdynamic {
    let pair = hlp_alloc_array(hlp_type_dyn(), 2);
    if pair.is_null() {
        return std::ptr::null_mut();
    }
    let slots = hl_aptr::<*mut vdynamic>(pair);
    let b = hlp_alloc_dynamic(hlp_type_bytes());
    if b.is_null() {
        return std::ptr::null_mut();
    }
    (*b).v.bytes = raw_bytes(data);
    *slots = b;
    *slots.add(1) = box_int(data.len() as i32);
    let d = hlp_alloc_dynamic(hlp_type_array());
    if !d.is_null() {
        (*d).v.ptr = pair as *mut c_void;
    }
    d
}

unsafe fn box_text(s: &str) -> *mut vdynamic {
    let d = hlp_alloc_dynamic(hlp_type_bytes());
    if !d.is_null() {
        (*d).v.bytes = ucs2(s);
    }
    d
}

unsafe fn box_cell(c: &Cell) -> *mut vdynamic {
    match c {
        Cell::Null => std::ptr::null_mut(),
        Cell::Int(v) => box_int(*v as i32),
        Cell::Real(v) => box_float(*v),
        Cell::Text(s) => box_text(s),
        Cell::Blob(b) => box_blob(b),
    }
}

/// The text a column reads as, which is what `getResult` asks for whatever
/// the column's storage class is.
fn cell_text(c: &Cell) -> Option<String> {
    match c {
        Cell::Null => None,
        Cell::Int(v) => Some(v.to_string()),
        Cell::Real(v) => Some(v.to_string()),
        Cell::Text(s) => Some(s.clone()),
        Cell::Blob(b) => Some(String::from_utf8_lossy(b).into_owned()),
    }
}

// ---------------------------------------------------------------- primitives

unsafe extern "C" fn sqlite_connect(path: *const vbyte) -> *mut c_void {
    let Some(path) = borrow_ucs2(path) else {
        return std::ptr::null_mut();
    };
    let Ok(conn) = rusqlite::Connection::open(&path) else {
        return std::ptr::null_mut();
    };
    Box::into_raw(Box::new(Db {
        magic: DB_MAGIC,
        conn,
        last_insert_id: 0,
    })) as *mut c_void
}

unsafe extern "C" fn sqlite_close(c: *mut c_void) {
    if db(c).is_some() {
        drop(Box::from_raw(c as *mut Db));
    }
}

unsafe extern "C" fn sqlite_request(c: *mut c_void, sql: *const vbyte) -> *mut c_void {
    let (Some(d), Some(sql)) = (db(c), borrow_ucs2(sql)) else {
        return std::ptr::null_mut();
    };
    let Ok(mut stmt) = d.conn.prepare(&sql) else {
        return std::ptr::null_mut();
    };
    let names: Vec<String> = stmt.column_names().iter().map(|n| n.to_string()).collect();
    let ncols = names.len();

    let mut collected: Vec<Vec<Cell>> = Vec::new();
    if ncols == 0 {
        // Nothing to walk. What the caller wants from a statement like this
        // is how many rows it changed.
        if stmt.execute([]).is_err() {
            return std::ptr::null_mut();
        }
    } else {
        let Ok(mut rows) = stmt.query([]) else {
            return std::ptr::null_mut();
        };
        loop {
            match rows.next() {
                Ok(Some(row)) => {
                    let mut cells = Vec::with_capacity(ncols);
                    for i in 0..ncols {
                        cells.push(match row.get_ref(i) {
                            Ok(rusqlite::types::ValueRef::Integer(v)) => Cell::Int(v),
                            Ok(rusqlite::types::ValueRef::Real(v)) => Cell::Real(v),
                            Ok(rusqlite::types::ValueRef::Text(b)) => {
                                Cell::Text(String::from_utf8_lossy(b).into_owned())
                            }
                            Ok(rusqlite::types::ValueRef::Blob(b)) => Cell::Blob(b.to_vec()),
                            _ => Cell::Null,
                        });
                    }
                    collected.push(cells);
                }
                Ok(None) => break,
                Err(_) => return std::ptr::null_mut(),
            }
        }
    }
    // Both after the statement has run, and both belong to the connection
    // rather than to the result the caller is about to be handed.
    let changes = d.conn.changes() as c_int;
    d.last_insert_id = d.conn.last_insert_rowid();

    Box::into_raw(Box::new(Res {
        magic: RES_MAGIC,
        names,
        rows: collected,
        next: 0,
        current: None,
        changes: if ncols == 0 { changes } else { 0 },
    })) as *mut c_void
}

unsafe extern "C" fn sqlite_last_id(c: *mut c_void) -> c_int {
    db(c).map(|d| d.last_insert_id as c_int).unwrap_or(0)
}

unsafe extern "C" fn sqlite_result_next(r: *mut c_void) -> *mut varray {
    let Some(res) = res(r) else {
        return std::ptr::null_mut();
    };
    if res.next >= res.rows.len() {
        return std::ptr::null_mut();
    }
    let index = res.next;
    res.next += 1;
    res.current = Some(index);

    let row = &res.rows[index];
    let a = hlp_alloc_array(hlp_type_dyn(), row.len() as i32);
    if a.is_null() {
        return std::ptr::null_mut();
    }
    let slots = hl_aptr::<*mut vdynamic>(a);
    for (i, cell) in row.iter().enumerate() {
        *slots.add(i) = box_cell(cell);
    }
    a
}

unsafe fn current_cell<'a>(r: *mut c_void, n: c_int) -> Option<&'a Cell> {
    let res = res(r)?;
    let row = res.rows.get(res.current?)?;
    row.get(usize::try_from(n).ok()?)
}

unsafe extern "C" fn sqlite_result_get(r: *mut c_void, n: c_int) -> *mut vbyte {
    match current_cell(r, n).and_then(cell_text) {
        Some(s) => ucs2(&s),
        None => std::ptr::null_mut(),
    }
}

unsafe extern "C" fn sqlite_result_get_int(r: *mut c_void, n: c_int) -> *mut vdynamic {
    match current_cell(r, n) {
        Some(Cell::Int(v)) => box_int(*v as i32),
        Some(Cell::Real(v)) => box_int(*v as i32),
        Some(Cell::Text(s)) => match s.parse::<i32>() {
            Ok(v) => box_int(v),
            Err(_) => std::ptr::null_mut(),
        },
        _ => std::ptr::null_mut(),
    }
}

unsafe extern "C" fn sqlite_result_get_float(r: *mut c_void, n: c_int) -> *mut vdynamic {
    match current_cell(r, n) {
        Some(Cell::Real(v)) => box_float(*v),
        Some(Cell::Int(v)) => box_float(*v as f64),
        Some(Cell::Text(s)) => match s.parse::<f64>() {
            Ok(v) => box_float(v),
            Err(_) => std::ptr::null_mut(),
        },
        _ => std::ptr::null_mut(),
    }
}

/// Rows for a statement that returned some, and rows CHANGED for one that
/// did not -- which is the only case the Haxe side asks this about.
unsafe extern "C" fn sqlite_result_get_length(r: *mut c_void) -> c_int {
    match res(r) {
        Some(res) if res.names.is_empty() => res.changes,
        Some(res) => res.rows.len() as c_int,
        None => 0,
    }
}

unsafe extern "C" fn sqlite_result_get_nfields(r: *mut c_void) -> c_int {
    res(r).map(|res| res.names.len() as c_int).unwrap_or(0)
}

unsafe extern "C" fn sqlite_result_get_fields(r: *mut c_void) -> *mut varray {
    let Some(res) = res(r) else {
        return std::ptr::null_mut();
    };
    let a = hlp_alloc_array(hlp_type_bytes(), res.names.len() as i32);
    if a.is_null() {
        return std::ptr::null_mut();
    }
    let slots = hl_aptr::<*mut vbyte>(a);
    for (i, name) in res.names.iter().enumerate() {
        *slots.add(i) = ucs2(name);
    }
    a
}

/// The eleven primitives, each exported as the resolver `DEFINE_PRIM` writes.
///
/// The macro expands to exactly what the C one does:
///
/// ```c
/// EXPORT void *hlp_<name>(const char **sign) {
///     *sign = <signature>; return (void*)&<name>;
/// }
/// ```
///
/// a RESOLVER that reports the signature through an out-parameter and returns
/// the real function -- never the primitive itself. Storing the resolver and
/// calling it as the primitive writes a signature string through whatever the
/// first argument happens to be.
///
/// The signature letters are HashLink's: `_` void, `i` i32, `d` f64, `b`
/// bytes, `A` array, `D` dynamic, `?` an abstract pointer. ash reads the
/// string and discards it -- the AOT call site is typed by the Haxe
/// declaration -- but it is what an interpreter would check arity against, so
/// it is written truthfully rather than left blank.
macro_rules! define_prim {
    ($resolver:ident, $function:ident, $signature:literal) => {
        /// # Safety
        /// `sign` must be a writable pointer, which is what the caller of a
        /// `DEFINE_PRIM` resolver passes.
        #[no_mangle]
        pub unsafe extern "C" fn $resolver(sign: *mut *const c_char) -> *mut c_void {
            if !sign.is_null() {
                *sign = concat!($signature, "\0").as_ptr() as *const c_char;
            }
            $function as *mut c_void
        }
    };
}

define_prim!(hlp_connect, sqlite_connect, "b?");
define_prim!(hlp_close, sqlite_close, "?_");
define_prim!(hlp_request, sqlite_request, "?b?");
define_prim!(hlp_last_id, sqlite_last_id, "?i");
define_prim!(hlp_result_next, sqlite_result_next, "?A");
define_prim!(hlp_result_get, sqlite_result_get, "?ib");
define_prim!(hlp_result_get_int, sqlite_result_get_int, "?iD");
define_prim!(hlp_result_get_float, sqlite_result_get_float, "?iD");
define_prim!(hlp_result_get_length, sqlite_result_get_length, "?i");
define_prim!(hlp_result_get_nfields, sqlite_result_get_nfields, "?i");
define_prim!(hlp_result_get_fields, sqlite_result_get_fields, "?A");
