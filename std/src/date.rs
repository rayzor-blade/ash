use std::ffi::CString;
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, Utc};

use crate::bytes::hlp_alloc_bytes;
use crate::hl::vbyte;

#[no_mangle]
pub unsafe extern "C" fn hlp_date_to_string(date: i32, len: *mut i32) -> *mut vbyte {
    // Convert the i32 date to a DateTime
    // Assuming date is seconds since 1970-01-01 00:00:00 UTC
    let datetime = DateTime::from_timestamp(date as i64, 0)
        .unwrap_or_else(|| DateTime::from_timestamp(0, 0).unwrap());

    // let datetime = DateTime::<Utc>::from_utc(naive, Utc);

    // Format the date as a string (e.g., "YYYY-MM-DD HH:MM:SS")
    let formatted = datetime.format("%Y-%m-%d %H:%M:%S").to_string();

    // Convert the formatted string to a C-compatible string
    let c_str = CString::new(formatted).unwrap();
    let bytes = c_str.into_bytes_with_nul();

    // Set the length of the resulting string
    *len = bytes.len() as i32;

    // Allocate memory for the string using hl_alloc_bytes
    let result = hlp_alloc_bytes(*len) as *mut vbyte;

    // Copy the bytes to the allocated memory
    std::ptr::copy_nonoverlapping(bytes.as_ptr(), result, *len as usize);

    result
}

#[no_mangle]
pub extern "C" fn hlp_date_new(y: i32, mo: i32, d: i32, h: i32, m: i32, s: i32) -> i32 {
    // Create a NaiveDateTime from the components
    let naive_date = match NaiveDate::from_ymd_opt(y, mo as u32, d as u32) {
        Some(date) => date,
        None => return 0, // Return 0 or some error value if the date is invalid
    };

    let naive_time = match NaiveTime::from_hms_opt(h as u32, m as u32, s as u32) {
        Some(time) => time,
        None => return 0, // Return 0 or some error value if the time is invalid
    };

    let naive_datetime = NaiveDateTime::new(naive_date, naive_time);

    // Convert NaiveDateTime to DateTime<Utc>
    let datetime_utc = DateTime::<Utc>::from_naive_utc_and_offset(naive_datetime, Utc);

    // Convert to timestamp (seconds since the Unix epoch)
    datetime_utc.timestamp() as i32
}