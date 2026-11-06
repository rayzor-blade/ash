use chrono::{
    DateTime, Datelike, Local, NaiveDate, NaiveDateTime, NaiveTime, Offset, TimeZone, Timelike, Utc,
};
use std::ptr;

use crate::bytes::hlp_alloc_bytes;
use crate::hl::{uchar, vbyte};

fn local_naive_to_secs(naive: NaiveDateTime) -> Option<i64> {
    let dt = Local
        .from_local_datetime(&naive)
        .single()
        .or_else(|| Local.from_local_datetime(&naive).earliest())?;
    Some(dt.timestamp())
}

const MICROS: i64 = 1_000_000;

/// Whole seconds of a microsecond count, rounded toward negative infinity.
fn us_to_secs(us: i64) -> i64 {
    us.div_euclid(MICROS)
}

fn local_at(secs: i64) -> DateTime<Local> {
    Local
        .timestamp_opt(secs, 0)
        .single()
        .or_else(|| Local.timestamp_opt(0, 0).single())
        .expect("epoch should exist")
}

fn utc_at(secs: i64) -> DateTime<Utc> {
    Utc.timestamp_opt(secs, 0)
        .single()
        .or_else(|| Utc.timestamp_opt(0, 0).single())
        .expect("epoch should exist")
}

unsafe fn utf16_to_string_with_len(bytes: *const vbyte, byte_len: i32) -> String {
    unsafe {
        if bytes.is_null() || byte_len <= 0 {
            return String::new();
        }
        let unit_len = (byte_len as usize) / 2;
        let units = std::slice::from_raw_parts(bytes as *const uchar, unit_len);
        String::from_utf16_lossy(units)
    }
}

unsafe fn alloc_utf16_string(s: &str, out_len: *mut i32) -> *mut vbyte {
    unsafe {
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
}

fn date_new_secs(y: i32, mo: i32, d: i32, h: i32, m: i32, s: i32) -> Option<i64> {
    let month = match mo.checked_add(1) {
        Some(v) if v > 0 => v as u32,
        _ => return None,
    };
    let naive_date = NaiveDate::from_ymd_opt(y, month, d as u32)?;
    let naive_time = NaiveTime::from_hms_opt(h as u32, m as u32, s as u32)?;
    local_naive_to_secs(NaiveDateTime::new(naive_date, naive_time))
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_new(y: i32, mo: i32, d: i32, h: i32, m: i32, s: i32) -> i32 {
    date_new_secs(y, mo, d, h, m, s)
        .and_then(|v| i32::try_from(v).ok())
        .unwrap_or(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_new2(y: i32, mo: i32, d: i32, h: i32, m: i32, s: i32) -> i64 {
    date_new_secs(y, mo, d, h, m, s)
        .unwrap_or(0)
        .saturating_mul(MICROS)
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_now() -> i32 {
    i32::try_from(Local::now().timestamp()).unwrap_or(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_now2() -> i64 {
    Utc::now().timestamp_micros()
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_from_time2(t: f64) -> i64 {
    let us = (t * 1000.0 + 0.5).floor();
    if us.is_finite() { us as i64 } else { 0 }
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_from_time(t: f64) -> i32 {
    let secs = (t / 1000.0).trunc();
    if !secs.is_finite() {
        return 0;
    }
    i32::try_from(secs as i64).unwrap_or(0)
}

unsafe fn date_from_string_secs(bytes: *const vbyte, len: i32) -> i64 {
    unsafe {
        let s = utf16_to_string_with_len(bytes, len);
        let s = s.trim();

        if let Ok(naive) = NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S") {
            return local_naive_to_secs(naive).unwrap_or(0);
        }
        if let Ok(date) = NaiveDate::parse_from_str(s, "%Y-%m-%d")
            && let Some(naive) = date.and_hms_opt(0, 0, 0)
        {
            return local_naive_to_secs(naive).unwrap_or(0);
        }
        if let Ok(time) = NaiveTime::parse_from_str(s, "%H:%M:%S") {
            // HashLink defines a time-only Date string as that time on the Unix
            // epoch date in UTC.  Treating it as an unsupported string returned
            // timestamp zero, losing the supplied hours/minutes/seconds.
            if let Some(date) = NaiveDate::from_ymd_opt(1970, 1, 1) {
                return Utc
                    .from_utc_datetime(&NaiveDateTime::new(date, time))
                    .timestamp();
            }
        }

        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_date_from_string(bytes: *const vbyte, len: i32) -> i32 {
    unsafe { i32::try_from(date_from_string_secs(bytes, len)).unwrap_or(0) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_date_from_string2(bytes: *const vbyte, len: i32) -> i64 {
    unsafe { date_from_string_secs(bytes, len).saturating_mul(MICROS) }
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_get_time(t: i32) -> f64 {
    (t as f64) * 1000.0
}

#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_get_time2(t: i64) -> f64 {
    t as f64 / 1000.0
}

unsafe fn write_inf<T: TimeZone>(
    dt: DateTime<T>,
    year: *mut i32,
    month: *mut i32,
    day: *mut i32,
    hours: *mut i32,
    minutes: *mut i32,
    seconds: *mut i32,
    wday: *mut i32,
) {
    unsafe {
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
}

#[unsafe(no_mangle)]
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
    unsafe {
        write_inf(
            local_at(t as i64),
            year,
            month,
            day,
            hours,
            minutes,
            seconds,
            wday,
        )
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_date_get_inf2(
    t: i64,
    year: *mut i32,
    month: *mut i32,
    day: *mut i32,
    hours: *mut i32,
    minutes: *mut i32,
    seconds: *mut i32,
    wday: *mut i32,
) {
    unsafe {
        write_inf(
            local_at(us_to_secs(t)),
            year,
            month,
            day,
            hours,
            minutes,
            seconds,
            wday,
        )
    }
}

#[unsafe(no_mangle)]
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
    unsafe {
        write_inf(
            utc_at(t as i64),
            year,
            month,
            day,
            hours,
            minutes,
            seconds,
            wday,
        )
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_date_get_utc_inf2(
    t: i64,
    year: *mut i32,
    month: *mut i32,
    day: *mut i32,
    hours: *mut i32,
    minutes: *mut i32,
    seconds: *mut i32,
    wday: *mut i32,
) {
    unsafe {
        write_inf(
            utc_at(us_to_secs(t)),
            year,
            month,
            day,
            hours,
            minutes,
            seconds,
            wday,
        )
    }
}

/// Local time minus UTC, in seconds.
#[unsafe(no_mangle)]
pub extern "C" fn hlp_date_get_tz_offset2(t: i64) -> i32 {
    local_at(us_to_secs(t)).offset().fix().local_minus_utc()
}

unsafe fn to_string_secs(secs: i64, len: *mut i32) -> *mut vbyte {
    unsafe {
        let s = local_at(secs).format("%Y-%m-%d %H:%M:%S").to_string();
        alloc_utf16_string(&s, len)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_date_to_string(t: i32, len: *mut i32) -> *mut vbyte {
    unsafe { to_string_secs(t as i64, len) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hlp_date_to_string2(t: i64, len: *mut i32) -> *mut vbyte {
    unsafe { to_string_secs(us_to_secs(t), len) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn date_now2_is_microseconds_and_agrees_with_now() {
        let us = hlp_date_now2();
        let secs = hlp_date_now() as i64;
        assert!((us_to_secs(us) - secs).abs() <= 1);
    }

    #[test]
    fn time_round_trips_milliseconds_through_microseconds() {
        let us = hlp_date_from_time2(1_700_000_000_123.0);
        assert_eq!(us, 1_700_000_000_123_000);
        assert_eq!(hlp_date_get_time2(us), 1_700_000_000_123.0);
        assert_eq!(hlp_date_from_time2(f64::NAN), 0);
    }

    #[test]
    fn new2_matches_new_in_seconds() {
        let a = hlp_date_new(2024, 2, 29, 12, 30, 15) as i64;
        assert_eq!(hlp_date_new2(2024, 2, 29, 12, 30, 15), a * 1_000_000);
    }

    #[test]
    fn dates_past_2038_survive() {
        let us = hlp_date_new2(2100, 0, 1, 0, 0, 0);
        assert!(us_to_secs(us) > i32::MAX as i64);
        let (mut y, mut mo, mut d, mut h, mut mi, mut s, mut wd) = (0, 0, 0, 0, 0, 0, 0);
        unsafe {
            hlp_date_get_inf2(
                us, &mut y, &mut mo, &mut d, &mut h, &mut mi, &mut s, &mut wd,
            )
        };
        assert_eq!((y, mo, d, h, mi, s), (2100, 0, 1, 0, 0, 0));
    }

    #[test]
    fn negative_microseconds_floor_to_the_earlier_second() {
        let (mut y, mut mo, mut d, mut h, mut mi, mut s, mut wd) = (0, 0, 0, 0, 0, 0, 0);
        unsafe {
            hlp_date_get_utc_inf2(
                -1, &mut y, &mut mo, &mut d, &mut h, &mut mi, &mut s, &mut wd,
            )
        };
        assert_eq!((y, mo, d, h, mi, s, wd), (1969, 11, 31, 23, 59, 59, 3));
    }

    #[test]
    fn tz_offset2_matches_chrono() {
        let us = hlp_date_new2(2024, 6, 1, 12, 0, 0);
        let want = Local
            .timestamp_opt(us_to_secs(us), 0)
            .unwrap()
            .offset()
            .fix()
            .local_minus_utc();
        assert_eq!(hlp_date_get_tz_offset2(us), want);
    }

    #[test]
    fn to_string2_agrees_with_to_string() {
        let t = hlp_date_new(2024, 2, 29, 12, 30, 15);
        let (mut l1, mut l2) = (0, 0);
        unsafe {
            let a = hlp_date_to_string(t, &mut l1);
            let b = hlp_date_to_string2(t as i64 * 1_000_000, &mut l2);
            assert_eq!(l1, l2);
            let a = std::slice::from_raw_parts(a as *const u16, l1 as usize);
            let b = std::slice::from_raw_parts(b as *const u16, l2 as usize);
            assert_eq!(a, b);
        }
    }
}
