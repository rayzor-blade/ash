use chrono::{Datelike, Local, NaiveDate, NaiveDateTime, NaiveTime, TimeZone, Timelike, Utc};
use std::ptr;

use crate::bytes::hlp_alloc_bytes;
use crate::hl::{uchar, vbyte};

fn local_naive_to_timestamp(naive: NaiveDateTime) -> Option<i32> {
    let dt = Local
        .from_local_datetime(&naive)
        .single()
        .or_else(|| Local.from_local_datetime(&naive).earliest())?;
    i32::try_from(dt.timestamp()).ok()
}

unsafe fn utf16_to_string_with_len(bytes: *const vbyte, byte_len: i32) -> String {
    if bytes.is_null() || byte_len <= 0 {
        return String::new();
    }
    let unit_len = (byte_len as usize) / 2;
    let units = std::slice::from_raw_parts(bytes as *const uchar, unit_len);
    String::from_utf16_lossy(units)
}

unsafe fn alloc_utf16_string(s: &str, out_len: *mut i32) -> *mut vbyte {
    let utf16: Vec<u16> = s.encode_utf16().collect();
    if !out_len.is_null() {
        *out_len = utf16.len() as i32;
    }
    let out = hlp_alloc_bytes(((utf16.len() + 1) * 2) as i32) as *mut u16;
    if out.is_null() {
        return ptr::null_mut();
    }
    ptr::copy_nonoverlapping(utf16.as_ptr(), out, utf16.len());
    *out.add(utf16.len()) = 0;
    out as *mut vbyte
}

#[no_mangle]
pub extern "C" fn hlp_date_new(y: i32, mo: i32, d: i32, h: i32, m: i32, s: i32) -> i32 {
    let month = match mo.checked_add(1) {
        Some(v) if v > 0 => v as u32,
        _ => return 0,
    };
    let naive_date = match NaiveDate::from_ymd_opt(y, month, d as u32) {
        Some(v) => v,
        None => return 0,
    };
    let naive_time = match NaiveTime::from_hms_opt(h as u32, m as u32, s as u32) {
        Some(v) => v,
        None => return 0,
    };
    local_naive_to_timestamp(NaiveDateTime::new(naive_date, naive_time)).unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn hlp_date_now() -> i32 {
    i32::try_from(Local::now().timestamp()).unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn hlp_date_from_time(t: f64) -> i32 {
    let secs = (t / 1000.0).trunc();
    if !secs.is_finite() {
        return 0;
    }
    i32::try_from(secs as i64).unwrap_or(0)
}

#[no_mangle]
pub unsafe extern "C" fn hlp_date_from_string(bytes: *const vbyte, len: i32) -> i32 {
    let s = utf16_to_string_with_len(bytes, len);
    let s = s.trim();

    if let Ok(naive) = NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S") {
        return local_naive_to_timestamp(naive).unwrap_or(0);
    }
    if let Ok(date) = NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        if let Some(naive) = date.and_hms_opt(0, 0, 0) {
            return local_naive_to_timestamp(naive).unwrap_or(0);
        }
    }

    0
}

#[no_mangle]
pub extern "C" fn hlp_date_get_time(t: i32) -> f64 {
    (t as f64) * 1000.0
}

#[no_mangle]
pub unsafe extern "C" fn hlp_date_get_inf(
    t: i32,
    year: *mut i32,
    month: *mut i32,
    day: *mut i32,
    hours: *mut i32,
    minutes: *mut i32,
    seconds: *mut i32,
    wday: *mut i32,
) {
    let dt = Local
        .timestamp_opt(t as i64, 0)
        .single()
        .unwrap_or_else(|| {
            Local
                .timestamp_opt(0, 0)
                .single()
                .expect("epoch should exist")
        });

    if !year.is_null() {
        *year = dt.year();
    }
    if !month.is_null() {
        *month = dt.month0() as i32;
    }
    if !day.is_null() {
        *day = dt.day() as i32;
    }
    if !hours.is_null() {
        *hours = dt.hour() as i32;
    }
    if !minutes.is_null() {
        *minutes = dt.minute() as i32;
    }
    if !seconds.is_null() {
        *seconds = dt.second() as i32;
    }
    if !wday.is_null() {
        *wday = dt.weekday().num_days_from_sunday() as i32;
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_date_get_utc_inf(
    t: i32,
    year: *mut i32,
    month: *mut i32,
    day: *mut i32,
    hours: *mut i32,
    minutes: *mut i32,
    seconds: *mut i32,
    wday: *mut i32,
) {
    let dt = Utc.timestamp_opt(t as i64, 0).single().unwrap_or_else(|| {
        Utc.timestamp_opt(0, 0)
            .single()
            .expect("epoch should exist")
    });

    if !year.is_null() {
        *year = dt.year();
    }
    if !month.is_null() {
        *month = dt.month0() as i32;
    }
    if !day.is_null() {
        *day = dt.day() as i32;
    }
    if !hours.is_null() {
        *hours = dt.hour() as i32;
    }
    if !minutes.is_null() {
        *minutes = dt.minute() as i32;
    }
    if !seconds.is_null() {
        *seconds = dt.second() as i32;
    }
    if !wday.is_null() {
        *wday = dt.weekday().num_days_from_sunday() as i32;
    }
}

#[no_mangle]
pub unsafe extern "C" fn hlp_date_to_string(t: i32, len: *mut i32) -> *mut vbyte {
    let dt = Local
        .timestamp_opt(t as i64, 0)
        .single()
        .unwrap_or_else(|| {
            Local
                .timestamp_opt(0, 0)
                .single()
                .expect("epoch should exist")
        });
    let s = dt.format("%Y-%m-%d %H:%M:%S").to_string();
    alloc_utf16_string(&s, len)
}
