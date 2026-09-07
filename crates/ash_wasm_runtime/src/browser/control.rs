//! Shared termination state, independent of any Worker's blocked event loop.
use super::run::Outcome;
use js_sys::{Atomics, BigInt64Array, Int32Array, SharedArrayBuffer, Uint8Array, WebAssembly};
use wasm_bindgen::prelude::*;

const TEXT_START: u32 = 12;
const TEXT_LEN: u32 = 1024;
const WAIT_START: u32 = (TEXT_START + TEXT_LEN) / 4;
const WAIT_COUNT: u32 = 1024;
const WORDS: u32 = WAIT_START + WAIT_COUNT;

#[derive(Clone, Default)]
pub(super) struct Control(Option<Int32Array>);
impl Control {
    pub fn new() -> Self {
        Self::attach(Some(SharedArrayBuffer::new(WORDS * 4)))
    }
    pub fn attach(buffer: Option<SharedArrayBuffer>) -> Self {
        Self(buffer.map(|b| Int32Array::new(&b)))
    }
    pub fn buffer(&self) -> JsValue {
        self.0
            .as_ref()
            .map_or(JsValue::UNDEFINED, |v| v.buffer().into())
    }

    pub fn outcome(&self) -> Option<Outcome> {
        let words = self.0.as_ref()?;
        let state = Atomics::load(words, 0).ok()?;
        if state != 1 && state != 2 {
            return None;
        }
        let status = Atomics::load(words, 1).ok()?;
        let trapped = if state == 2 {
            let len = (Atomics::load(words, 2).ok()? as u32).min(TEXT_LEN);
            let bytes = Uint8Array::new(&words.buffer())
                .subarray(TEXT_START, TEXT_START + len)
                .to_vec();
            Some(String::from_utf8_lossy(&bytes).into_owned())
        } else {
            None
        };
        Some(Outcome { status, trapped })
    }

    pub fn check(&self) {
        if self.outcome().is_some() {
            wasm_bindgen::throw_str("ash: guest program terminated");
        }
    }

    pub fn finish(&self, outcome: &Outcome, memory: Option<&WebAssembly::Memory>) {
        let Some(words) = &self.0 else {
            return;
        };
        if Atomics::compare_exchange(words, 0, 0, -1).unwrap() != 0 {
            return;
        }
        Atomics::store(words, 1, outcome.status).unwrap();
        if let Some(message) = &outcome.trapped {
            let bytes = message.as_bytes();
            let len = bytes.len().min(TEXT_LEN as usize);
            Uint8Array::new(&words.buffer())
                .subarray(TEXT_START, TEXT_START + len as u32)
                .copy_from(&bytes[..len]);
            Atomics::store(words, 2, len as i32).unwrap();
        }
        Atomics::store(words, 0, if outcome.trapped.is_some() { 2 } else { 1 }).unwrap();
        if let Some(memory) = memory {
            loop {
                let view = Int32Array::new(&memory.buffer());
                let mut waiting = false;
                for slot in WAIT_START..WORDS {
                    let at = Atomics::load(words, slot).unwrap();
                    if at == 0 {
                        continue;
                    }
                    waiting = true;
                    if at != -1 {
                        let _ = Atomics::notify(&view, (at as u32 - 1) / 4);
                    }
                }
                if !waiting {
                    break;
                }
                // Re-notify until registrations drain: a wake may race with
                // the last instructions before the engine enters its wait.
                let _ = Atomics::wait_with_timeout(words, 0, Atomics::load(words, 0).unwrap(), 1.0);
            }
        }
    }

    pub fn wait(
        &self,
        memory: &WebAssembly::Memory,
        address: u32,
        offset: u64,
        expected: i64,
        timeout: i64,
        wide: bool,
    ) -> Result<i32, JsValue> {
        self.check();
        let address = (address as u64)
            .checked_add(offset)
            .filter(|&a| a <= u32::MAX as u64)
            .ok_or_else(|| JsValue::from_str("atomic wait address out of bounds"))?
            as u32;
        let align = if wide { 8 } else { 4 };
        if address % align != 0 {
            return Err(JsValue::from_str("unaligned atomic wait"));
        }
        let slot = if let Some(words) = &self.0 {
            let mut found = None;
            for index in WAIT_START..WORDS {
                if Atomics::compare_exchange(words, index, 0, -1)? == 0 {
                    found = Some(index);
                    break;
                }
            }
            let index =
                found.ok_or_else(|| JsValue::from_str("too many simultaneous atomic waits"))?;
            Atomics::store(words, index, address.wrapping_add(1) as i32)?;
            Some(index)
        } else {
            None
        };
        let result = if self.outcome().is_some() {
            Err(JsValue::from_str("ash: guest program terminated"))
        } else {
            let ms = if timeout < 0 {
                f64::INFINITY
            } else {
                timeout as f64 / 1e6
            };
            if wide {
                Atomics::wait_with_timeout_bigint(
                    &BigInt64Array::new(&memory.buffer()),
                    address / 8,
                    expected,
                    ms,
                )
            } else {
                Atomics::wait_with_timeout(
                    &Int32Array::new(&memory.buffer()),
                    address / 4,
                    expected as i32,
                    ms,
                )
            }
        };
        if let (Some(words), Some(slot)) = (&self.0, slot) {
            Atomics::store(words, slot, 0)?;
        }
        self.check();
        Ok(match result?.as_string().as_deref() {
            Some("ok") => 0,
            Some("not-equal") => 1,
            _ => 2,
        })
    }
}
