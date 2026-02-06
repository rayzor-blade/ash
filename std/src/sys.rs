use crate::hl::vbyte;

#[no_mangle]
pub extern "C" fn hlp_sys_utf8_path() -> bool {
    true
}

#[no_mangle]
pub unsafe extern "C" fn hlp_sys_print(msg: *const vbyte) {
    if msg.is_null() {
        return;
    }

    // msg points to UTF-16 data (pairs of u16 little-endian) cast as *const u8.
    // Read u16 values until null terminator.
    let p = msg as *const u16;
    let mut len = 0usize;
    while *p.add(len) != 0 {
        len += 1;
    }
    let slice = std::slice::from_raw_parts(p, len);
    let s = String::from_utf16_lossy(slice);
    print!("{}", s);
}


#[no_mangle]
pub extern "C" fn hlp_sys_is64() -> bool {
    #[cfg(target_pointer_width = "64")]
    {
        true
    }
    #[cfg(not(target_pointer_width = "64"))]
    {
        false
    }
}

#[no_mangle]
pub extern "C" fn hlp_sys_check_reload(debug_alt_file: *const vbyte) ->  bool{
    return false
}