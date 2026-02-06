use std::ptr;
use std::alloc::{alloc, Layout};
use crate::hl::uchar;

#[no_mangle]
pub unsafe extern "C" fn ucmp(a: *const uchar, b: *const uchar) -> i32 {
    let mut a_ptr = a;
    let mut b_ptr = b;

    loop {
        let a_val = *a_ptr as u32;
        let b_val = *b_ptr as u32;

        let d = a_val.wrapping_sub(b_val) as i32;
        if d != 0 {
            return d;
        }

        if a_val == 0 {
            return 0;
        }

        a_ptr = a_ptr.add(1);
        b_ptr = b_ptr.add(1);
    }
}

#[no_mangle]
pub unsafe extern "C" fn ustrdup(str: *const uchar) -> *mut uchar {
    if str.is_null() {
        // println!("Input string is null");
        return ptr::null_mut();
    }

    let mut len = 0;
    while *str.add(len) != 0 {
        len += 1;
    }
    // println!("Duplicating string of length {}", len);

    // Ensure we allocate at least one byte (for null terminator)
    let alloc_size = std::cmp::max(len + 1, 1);
    
    let layout = Layout::array::<uchar>(alloc_size).unwrap_or_else(|_| {
        // println!("Failed to create layout for string allocation");
        Layout::new::<uchar>() // Fallback to single char layout
    });

    let new_str = alloc(layout) as *mut uchar;
    if new_str.is_null() {
        // println!("String allocation failed");
        return ptr::null_mut();
    }

    // Use byte-by-byte copy instead of ptr::copy_nonoverlapping
    for i in 0..len {
        *new_str.add(i) = *str.add(i);
    }
    *new_str.add(len) = 0; // Null terminator

    // println!("String duplication successful");
    new_str
}
