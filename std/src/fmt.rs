//! The `fmt` library's digest and zlib primitives, implemented here.
//!
//! HashLink ships these in `fmt.hdll`, and a native ash program loads that
//! library like any other. A sandbox cannot load anything, so without these
//! every `haxe.crypto.Md5`, `haxe.zip.Compress` and `haxe.zip.Uncompress`
//! raised "Native library 'fmt' not loaded" the moment it was reached -- a
//! dozen classes of the unit suite, and any program that hashes or unzips.
//!
//! Only the part of `fmt` that is pure computation lives here: the four
//! digests and streaming inflate/deflate. Image decoding and audio stay in
//! the HDLL, where they always were.
//!
//! Semantics follow `libs/fmt/fmt.c` exactly, including the details a caller
//! depends on without knowing it: `digest` takes the running CRC or Adler
//! value FROM the output buffer, bit 8 of the format means "the input is a
//! UTF-16 string", and `inflate_buffer`/`deflate_buffer` report through their
//! two out-parameters how much of each buffer they used and return whether
//! the stream ended.

use std::os::raw::{c_int, c_void};

use miniz_oxide::deflate::core::{create_comp_flags_from_zip_params, CompressorOxide};
use miniz_oxide::deflate::stream::deflate;
use miniz_oxide::inflate::stream::{inflate, InflateState};
use miniz_oxide::{MZError, MZFlush, MZStatus};

use crate::error::hlp_error;
use crate::strings::str_to_uchar_ptr;

// ---------------------------------------------------------------------------
// digest
// ---------------------------------------------------------------------------

/// MD5 of `data`, RFC 1321.
pub fn md5(data: &[u8]) -> [u8; 16] {
    const S: [u32; 64] = [
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5,
        9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10,
        15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
    ];
    // K[i] = floor(|sin(i + 1)| * 2^32). Computed rather than transcribed:
    // sixty-four hex constants are sixty-four places for a typo that would
    // hash everything wrong while looking exactly right.
    let k: Vec<u32> = (0..64)
        .map(|i| (((i + 1) as f64).sin().abs() * 4294967296.0) as u32)
        .collect();

    let mut h: [u32; 4] = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476];
    let mut msg = data.to_vec();
    let bit_len = (data.len() as u64).wrapping_mul(8);
    msg.push(0x80);
    while msg.len() % 64 != 56 {
        msg.push(0);
    }
    msg.extend_from_slice(&bit_len.to_le_bytes());

    let (blocks, _) = msg.as_chunks::<64>();
    for chunk in blocks {
        let mut m = [0u32; 16];
        for (i, word) in m.iter_mut().enumerate() {
            *word = u32::from_le_bytes([
                chunk[i * 4],
                chunk[i * 4 + 1],
                chunk[i * 4 + 2],
                chunk[i * 4 + 3],
            ]);
        }
        let [mut a, mut b, mut c, mut d] = h;
        for i in 0..64 {
            let (f, g) = match i / 16 {
                0 => ((b & c) | (!b & d), i),
                1 => ((d & b) | (!d & c), (5 * i + 1) % 16),
                2 => (b ^ c ^ d, (3 * i + 5) % 16),
                _ => (c ^ (b | !d), (7 * i) % 16),
            };
            let rotated = a
                .wrapping_add(f)
                .wrapping_add(k[i])
                .wrapping_add(m[g])
                .rotate_left(S[i]);
            a = d;
            d = c;
            c = b;
            b = b.wrapping_add(rotated);
        }
        h[0] = h[0].wrapping_add(a);
        h[1] = h[1].wrapping_add(b);
        h[2] = h[2].wrapping_add(c);
        h[3] = h[3].wrapping_add(d);
    }
    let mut out = [0u8; 16];
    for (i, word) in h.iter().enumerate() {
        out[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
    }
    out
}

/// SHA-1 of `data`, FIPS 180-1.
pub fn sha1(data: &[u8]) -> [u8; 20] {
    let mut h: [u32; 5] = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0];
    let mut msg = data.to_vec();
    let bit_len = (data.len() as u64).wrapping_mul(8);
    msg.push(0x80);
    while msg.len() % 64 != 56 {
        msg.push(0);
    }
    msg.extend_from_slice(&bit_len.to_be_bytes());

    let (blocks, _) = msg.as_chunks::<64>();
    for chunk in blocks {
        let mut w = [0u32; 80];
        for i in 0..16 {
            w[i] = u32::from_be_bytes([
                chunk[i * 4],
                chunk[i * 4 + 1],
                chunk[i * 4 + 2],
                chunk[i * 4 + 3],
            ]);
        }
        for i in 16..80 {
            w[i] = (w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16]).rotate_left(1);
        }
        let [mut a, mut b, mut c, mut d, mut e] = h;
        for (i, &word) in w.iter().enumerate() {
            let (f, k) = match i / 20 {
                0 => ((b & c) | (!b & d), 0x5A827999u32),
                1 => (b ^ c ^ d, 0x6ED9EBA1),
                2 => ((b & c) | (b & d) | (c & d), 0x8F1BBCDC),
                _ => (b ^ c ^ d, 0xCA62C1D6),
            };
            let t = a
                .rotate_left(5)
                .wrapping_add(f)
                .wrapping_add(e)
                .wrapping_add(k)
                .wrapping_add(word);
            e = d;
            d = c;
            c = b.rotate_left(30);
            b = a;
            a = t;
        }
        h[0] = h[0].wrapping_add(a);
        h[1] = h[1].wrapping_add(b);
        h[2] = h[2].wrapping_add(c);
        h[3] = h[3].wrapping_add(d);
        h[4] = h[4].wrapping_add(e);
    }
    let mut out = [0u8; 20];
    for (i, word) in h.iter().enumerate() {
        out[i * 4..i * 4 + 4].copy_from_slice(&word.to_be_bytes());
    }
    out
}

/// zlib's `crc32(crc, buf, len)`: continue the running CRC `crc` over `data`.
/// Start from 0.
pub fn crc32(crc: u32, data: &[u8]) -> u32 {
    let mut c = !crc;
    for &byte in data {
        c ^= byte as u32;
        for _ in 0..8 {
            c = if c & 1 != 0 {
                0xEDB88320 ^ (c >> 1)
            } else {
                c >> 1
            };
        }
    }
    !c
}

/// zlib's `adler32(adler, buf, len)`: continue the running checksum. Start
/// from 1.
pub fn adler32(adler: u32, data: &[u8]) -> u32 {
    const MOD: u32 = 65521;
    let mut a = adler & 0xffff;
    let mut b = adler >> 16;
    for &byte in data {
        a = (a + byte as u32) % MOD;
        b = (b + a) % MOD;
    }
    (b << 16) | a
}

/// `fmt.digest(out, in, len, format)`.
///
/// Low byte of `format`: 0 MD5, 1 SHA-1, 2 CRC32, 3 Adler-32. The CRC and
/// Adler forms READ their starting value from `out` and write the updated one
/// back, which is how Haxe chains them over several buffers. Bit 8 set means
/// `in` is a NUL-terminated UTF-16 string to be hashed as UTF-8, and `len` is
/// then ignored.
pub unsafe extern "C" fn fmt_digest(out: *mut u8, input: *const u8, len: c_int, format: c_int) {
    let mut utf8: Option<Vec<u8>> = None;
    let data: &[u8] = if format & 256 != 0 {
        let mut size: i32 = 0;
        let p = crate::strings::hlp_utf16_to_utf8(input, 0, &mut size);
        if p.is_null() {
            &[]
        } else {
            utf8 = Some(std::slice::from_raw_parts(p, size.max(0) as usize).to_vec());
            utf8.as_deref().unwrap_or(&[])
        }
    } else if input.is_null() || len <= 0 {
        &[]
    } else {
        std::slice::from_raw_parts(input, len as usize)
    };
    match format & 0xff {
        0 => std::ptr::copy_nonoverlapping(md5(data).as_ptr(), out, 16),
        1 => std::ptr::copy_nonoverlapping(sha1(data).as_ptr(), out, 20),
        2 => {
            let seed = (out as *const u32).read_unaligned();
            (out as *mut u32).write_unaligned(crc32(seed, data));
        }
        3 => {
            let seed = (out as *const u32).read_unaligned();
            (out as *mut u32).write_unaligned(adler32(seed, data));
        }
        other => hlp_error(str_to_uchar_ptr(&format!("Unknown digest format {other}"))),
    }
    drop(utf8);
}

// ---------------------------------------------------------------------------
// zlib streams
// ---------------------------------------------------------------------------

/// The `fmt_zip` abstract: one stream, inflating or deflating.
///
/// `zip_end` drops the state but keeps the box, as upstream frees the
/// `z_stream` and leaves the GC object; a call after `close()` then raises
/// instead of touching freed memory.
pub struct FmtZip {
    inflater: Option<Box<InflateState>>,
    deflater: Option<Box<CompressorOxide>>,
    flush: MZFlush,
}

unsafe fn zip<'a>(z: *mut FmtZip) -> &'a mut FmtZip {
    if z.is_null() {
        hlp_error(str_to_uchar_ptr("Invalid zip stream"));
        unreachable!("hlp_error does not return");
    }
    &mut *z
}

fn zlib_error(err: MZError) -> ! {
    let what = match err {
        MZError::Stream => "stream error",
        MZError::Data => "data error",
        MZError::Mem => "memory error",
        MZError::Buf => "buffer error",
        MZError::Version => "version error",
        MZError::Param => "invalid parameter",
        MZError::ErrNo => "I/O error",
    };
    unsafe { hlp_error(str_to_uchar_ptr(&format!("zlib {what}"))) };
    unreachable!("hlp_error does not return")
}

/// `fmt.inflate_init(windowBits)`. Zero means zlib's default of 15; a
/// negative value means a raw deflate stream, as in zlib.
pub unsafe extern "C" fn fmt_inflate_init(window_bits: c_int) -> *mut FmtZip {
    let bits = if window_bits == 0 { 15 } else { window_bits };
    Box::into_raw(Box::new(FmtZip {
        inflater: Some(InflateState::new_boxed_with_window_bits(bits)),
        deflater: None,
        flush: MZFlush::None,
    }))
}

/// `fmt.deflate_init(level)`, writing a zlib header as `deflateInit` does.
pub unsafe extern "C" fn fmt_deflate_init(level: c_int) -> *mut FmtZip {
    let level = if level < 0 { 6 } else { level.min(10) };
    let flags = create_comp_flags_from_zip_params(level, 15, 0);
    Box::into_raw(Box::new(FmtZip {
        inflater: None,
        deflater: Some(Box::new(CompressorOxide::new(flags))),
        flush: MZFlush::None,
    }))
}

/// `fmt.deflate_bound(zip, length)`: an output size that always suffices.
/// zlib's own formula plus its zlib-wrapper allowance; miniz never exceeds
/// it, since stored blocks are the worst case for both.
pub unsafe extern "C" fn fmt_deflate_bound(_zip: *mut FmtZip, length: c_int) -> c_int {
    let n = length.max(0) as i64;
    (n + (n >> 12) + (n >> 14) + (n >> 25) + 13 + 6 + 32).min(i32::MAX as i64) as c_int
}

/// `fmt.zip_end(zip)`.
pub unsafe extern "C" fn fmt_zip_end(z: *mut FmtZip) {
    let z = zip(z);
    z.inflater = None;
    z.deflater = None;
}

/// `fmt.zip_flush_mode(zip, mode)`, in Haxe's `FlushMode` order: NO, SYNC,
/// FULL, FINISH, BLOCK.
pub unsafe extern "C" fn fmt_zip_flush_mode(z: *mut FmtZip, mode: c_int) {
    let z = zip(z);
    z.flush = match mode {
        0 => MZFlush::None,
        1 => MZFlush::Sync,
        2 => MZFlush::Full,
        3 => MZFlush::Finish,
        4 => MZFlush::Block,
        other => {
            hlp_error(str_to_uchar_ptr(&format!("Invalid flush mode {other}")));
            unreachable!("hlp_error does not return");
        }
    };
}

/// The two buffers a `*_buffer` call works on, checked as upstream checks them.
unsafe fn buffers<'a>(
    src: *const u8,
    srcpos: c_int,
    srclen: c_int,
    dst: *mut u8,
    dstpos: c_int,
    dstlen: c_int,
) -> (&'a [u8], &'a mut [u8]) {
    let slen = srclen - srcpos;
    let dlen = dstlen - dstpos;
    if srcpos < 0 || dstpos < 0 || slen < 0 || dlen < 0 {
        hlp_error(str_to_uchar_ptr("Out of range"));
        unreachable!("hlp_error does not return");
    }
    let input = if src.is_null() || slen == 0 {
        &[][..]
    } else {
        std::slice::from_raw_parts(src.add(srcpos as usize), slen as usize)
    };
    let output = if dst.is_null() || dlen == 0 {
        &mut [][..]
    } else {
        std::slice::from_raw_parts_mut(dst.add(dstpos as usize), dlen as usize)
    };
    (input, output)
}

/// `fmt.inflate_buffer(zip, src, srcPos, srcLen, dst, dstPos, dstLen, read, write)`.
/// Returns whether the stream ended.
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn fmt_inflate_buffer(
    z: *mut FmtZip,
    src: *const u8,
    srcpos: c_int,
    srclen: c_int,
    dst: *mut u8,
    dstpos: c_int,
    dstlen: c_int,
    read: *mut c_int,
    write: *mut c_int,
) -> bool {
    let z = zip(z);
    let Some(state) = z.inflater.as_deref_mut() else {
        hlp_error(str_to_uchar_ptr("Not an inflate stream, or already closed"));
        unreachable!("hlp_error does not return");
    };
    let (input, output) = buffers(src, srcpos, srclen, dst, dstpos, dstlen);
    // A full flush has no meaning for inflate and miniz refuses it; zlib
    // treats it as a sync flush there.
    let flush = if z.flush == MZFlush::Full {
        MZFlush::Sync
    } else {
        z.flush
    };
    let result = inflate(state, input, output, flush);
    let status = match result.status {
        Ok(s) => s,
        // No progress on an empty output buffer is not an error to zlib's
        // callers either: they read 0/0 and come back with room.
        Err(MZError::Buf) => MZStatus::Ok,
        Err(e) => zlib_error(e),
    };
    *read = result.bytes_consumed as c_int;
    *write = result.bytes_written as c_int;
    status == MZStatus::StreamEnd
}

/// `fmt.deflate_buffer(zip, src, srcPos, srcLen, dst, dstPos, dstLen, read, write)`.
/// Returns whether the stream ended, which only a FINISH flush can make true.
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn fmt_deflate_buffer(
    z: *mut FmtZip,
    src: *const u8,
    srcpos: c_int,
    srclen: c_int,
    dst: *mut u8,
    dstpos: c_int,
    dstlen: c_int,
    read: *mut c_int,
    write: *mut c_int,
) -> bool {
    let z = zip(z);
    let Some(state) = z.deflater.as_deref_mut() else {
        hlp_error(str_to_uchar_ptr("Not a deflate stream, or already closed"));
        unreachable!("hlp_error does not return");
    };
    let (input, output) = buffers(src, srcpos, srclen, dst, dstpos, dstlen);
    let result = deflate(state, input, output, z.flush);
    let status = match result.status {
        Ok(s) => s,
        Err(MZError::Buf) => MZStatus::Ok,
        Err(e) => zlib_error(e),
    };
    *read = result.bytes_consumed as c_int;
    *write = result.bytes_written as c_int;
    status == MZStatus::StreamEnd
}

/// The primitives above, by their `fmt` names, for the sandbox's resolver.
pub fn primitive(name: &str) -> *mut c_void {
    match name {
        "digest" => fmt_digest as *mut c_void,
        "inflate_init" => fmt_inflate_init as *mut c_void,
        "deflate_init" => fmt_deflate_init as *mut c_void,
        "deflate_bound" => fmt_deflate_bound as *mut c_void,
        "zip_end" => fmt_zip_end as *mut c_void,
        "zip_flush_mode" => fmt_zip_flush_mode as *mut c_void,
        "inflate_buffer" => fmt_inflate_buffer as *mut c_void,
        "deflate_buffer" => fmt_deflate_buffer as *mut c_void,
        _ => std::ptr::null_mut(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hex(bytes: &[u8]) -> String {
        bytes.iter().map(|b| format!("{b:02x}")).collect()
    }

    #[test]
    fn md5_vectors() {
        assert_eq!(hex(&md5(b"")), "d41d8cd98f00b204e9800998ecf8427e");
        assert_eq!(hex(&md5(b"abc")), "900150983cd24fb0d6963f7d28e17f72");
        assert_eq!(
            hex(&md5(b"The quick brown fox jumps over the lazy dog")),
            "9e107d9d372bb6826bd81d3542a419d6"
        );
        // Crosses a block boundary with the length in the second block.
        assert_eq!(hex(&md5(&[b'a'; 56])), "3b0c8ac703f828b04c6c197006d17218");
    }

    #[test]
    fn sha1_vectors() {
        assert_eq!(hex(&sha1(b"")), "da39a3ee5e6b4b0d3255bfef95601890afd80709");
        assert_eq!(
            hex(&sha1(b"abc")),
            "a9993e364706816aba3e25717850c26c9cd0d89d"
        );
        assert_eq!(
            hex(&sha1(
                b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
            )),
            "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
        );
    }

    #[test]
    fn checksums_chain_like_zlib() {
        assert_eq!(crc32(0, b"123456789"), 0xCBF43926);
        assert_eq!(crc32(crc32(0, b"1234"), b"56789"), 0xCBF43926);
        assert_eq!(adler32(1, b"Wikipedia"), 0x11E60398);
        assert_eq!(adler32(adler32(1, b"Wiki"), b"pedia"), 0x11E60398);
    }

    #[test]
    fn deflate_then_inflate_round_trips() {
        let text: Vec<u8> = b"hello hello hello hello, zlib on the other side"
            .iter()
            .cycle()
            .take(10_000)
            .copied()
            .collect();
        unsafe {
            let d = fmt_deflate_init(6);
            fmt_zip_flush_mode(d, 3);
            let mut out = vec![0u8; fmt_deflate_bound(d, text.len() as c_int) as usize];
            let (mut read, mut write) = (0, 0);
            let done = fmt_deflate_buffer(
                d,
                text.as_ptr(),
                0,
                text.len() as c_int,
                out.as_mut_ptr(),
                0,
                out.len() as c_int,
                &mut read,
                &mut write,
            );
            assert!(done);
            assert_eq!(read as usize, text.len());
            assert!((write as usize) < text.len() / 4);
            fmt_zip_end(d);
            // A zlib header, since deflate_init wraps like deflateInit.
            assert_eq!(out[0] & 0x0f, 8);

            let i = fmt_inflate_init(0);
            fmt_zip_flush_mode(i, 1);
            let mut back = vec![0u8; text.len() + 16];
            let (mut r2, mut w2) = (0, 0);
            let done = fmt_inflate_buffer(
                i,
                out.as_ptr(),
                0,
                write,
                back.as_mut_ptr(),
                0,
                back.len() as c_int,
                &mut r2,
                &mut w2,
            );
            assert!(done);
            assert_eq!(r2, write);
            assert_eq!(&back[..w2 as usize], &text[..]);
            fmt_zip_end(i);
        }
    }

    #[test]
    fn inflate_in_small_pieces() {
        let text: Vec<u8> = (0..5000u32).map(|i| (i % 251) as u8).collect();
        unsafe {
            let d = fmt_deflate_init(9);
            fmt_zip_flush_mode(d, 3);
            let mut out = vec![0u8; fmt_deflate_bound(d, text.len() as c_int) as usize];
            let (mut read, mut write) = (0, 0);
            assert!(fmt_deflate_buffer(
                d,
                text.as_ptr(),
                0,
                text.len() as c_int,
                out.as_mut_ptr(),
                0,
                out.len() as c_int,
                &mut read,
                &mut write
            ));
            let compressed = &out[..write as usize];

            // Uncompress.run's loop: a 64-byte scratch buffer, SYNC flush,
            // feeding from `pos` until done.
            let i = fmt_inflate_init(0);
            fmt_zip_flush_mode(i, 1);
            let mut tmp = [0u8; 64];
            let mut pos = 0;
            let mut got = Vec::new();
            loop {
                let (mut r, mut w) = (0, 0);
                let done = fmt_inflate_buffer(
                    i,
                    compressed.as_ptr(),
                    pos,
                    compressed.len() as c_int,
                    tmp.as_mut_ptr(),
                    0,
                    tmp.len() as c_int,
                    &mut r,
                    &mut w,
                );
                got.extend_from_slice(&tmp[..w as usize]);
                pos += r;
                if done {
                    break;
                }
                assert!(r > 0 || w > 0, "no progress");
            }
            assert_eq!(got, text);
        }
    }
}
