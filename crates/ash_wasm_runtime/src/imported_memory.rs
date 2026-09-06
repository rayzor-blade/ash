//! Reading the one thing a page has to know before it instantiates.
//!
//! A threads build does not define its memory: it imports `env.memory`,
//! shared, and the host makes it. Making it needs the minimum and maximum the
//! module declared -- too small and instantiation is refused, too large and
//! the page reserves address space nobody asked for -- and the only place
//! those are written is the module's own import section.
//!
//! `WebAssembly.Module.imports()` would say a memory is imported but not how
//! big, unless the engine has the type-reflection proposal. So the section is
//! read here, which works in every browser and costs one pass over a few
//! hundred bytes: the import section is the second one in the file and the
//! walk stops at the memory.

/// What a shared memory has to be created with, in pages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MemoryLimits {
    pub minimum: u32,
    pub maximum: u32,
}

/// The limits of the memory this module imports, or `None` if it defines its
/// own -- which is every build but the threads one.
pub fn imported_memory(bytes: &[u8]) -> Option<MemoryLimits> {
    let mut r = Reader::new(bytes);
    if r.take(4)? != b"\0asm" {
        return None;
    }
    r.take(4)?;
    while let Some(id) = r.byte() {
        let size = r.leb()? as usize;
        let body = r.take(size)?;
        // 2 is the import section, and it is the only one that can name a
        // memory the module does not own.
        if id != 2 {
            continue;
        }
        return imports_of(body);
    }
    None
}

fn imports_of(body: &[u8]) -> Option<MemoryLimits> {
    let mut r = Reader::new(body);
    let count = r.leb()?;
    for _ in 0..count {
        let module = r.name()?;
        let name = r.name()?;
        match r.byte()? {
            // A function names its type; a global its value type and
            // mutability. Both are skipped rather than understood.
            0x00 => {
                r.leb()?;
            }
            0x01 => {
                r.byte()?;
                r.limits()?;
            }
            0x02 => {
                let limits = r.limits()?;
                if (module, name) == ("env", "memory") {
                    return limits;
                }
            }
            0x03 => {
                r.byte()?;
                r.byte()?;
            }
            0x04 => {
                r.byte()?;
                r.leb()?;
            }
            _ => return None,
        }
    }
    None
}

struct Reader<'a> {
    bytes: &'a [u8],
}

impl<'a> Reader<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes }
    }

    fn byte(&mut self) -> Option<u8> {
        let (first, rest) = self.bytes.split_first()?;
        self.bytes = rest;
        Some(*first)
    }

    fn take(&mut self, n: usize) -> Option<&'a [u8]> {
        if self.bytes.len() < n {
            return None;
        }
        let (head, rest) = self.bytes.split_at(n);
        self.bytes = rest;
        Some(head)
    }

    fn leb(&mut self) -> Option<u32> {
        let (mut value, mut shift) = (0u32, 0u32);
        loop {
            let byte = self.byte()?;
            value |= u32::from(byte & 0x7f).checked_shl(shift)?;
            if byte & 0x80 == 0 {
                return Some(value);
            }
            shift += 7;
            if shift > 31 {
                return None;
            }
        }
    }

    fn name(&mut self) -> Option<&'a str> {
        let len = self.leb()? as usize;
        std::str::from_utf8(self.take(len)?).ok()
    }

    /// The limits a table or memory carries. `None` inside the `Some` means a
    /// memory with no maximum, which cannot be shared.
    fn limits(&mut self) -> Option<Option<MemoryLimits>> {
        let flags = self.byte()?;
        let minimum = self.leb()?;
        let maximum = if flags & 0x1 != 0 {
            Some(self.leb()?)
        } else {
            None
        };
        Some(maximum.map(|maximum| MemoryLimits { minimum, maximum }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A module importing `env.memory` as shared, 21 pages up to 16384 --
    /// what ash's linker emits for the threads target -- with a function
    /// import in front of it so the walk has something to step over.
    #[test]
    fn reads_the_limits_a_threads_build_declares() {
        let mut wasm = b"\0asm\x01\0\0\0".to_vec();
        let mut section = vec![2u8]; // two imports
        for (module, name, tail) in [
            ("env", "f", vec![0x00u8, 0x00]),
            ("env", "memory", vec![0x02u8, 0x03, 21, 0x80, 0x80, 0x01]),
        ] {
            section.push(module.len() as u8);
            section.extend_from_slice(module.as_bytes());
            section.push(name.len() as u8);
            section.extend_from_slice(name.as_bytes());
            section.extend_from_slice(&tail);
        }
        wasm.push(2);
        wasm.push(section.len() as u8);
        wasm.extend_from_slice(&section);

        assert_eq!(
            imported_memory(&wasm),
            Some(MemoryLimits {
                minimum: 21,
                maximum: 16384
            })
        );
    }

    #[test]
    fn a_module_that_owns_its_memory_asks_for_none() {
        let wasm = b"\0asm\x01\0\0\0".to_vec();
        assert_eq!(imported_memory(&wasm), None);
    }
}

#[cfg(test)]
mod against_a_real_module {
    /// The limits ash's linker actually emits, read out of a module it built.
    ///
    /// Gated on the path because a 3MB module is not a fixture to commit.
    /// `ASH_THREADS_MODULE=examples/browser/threads.wasm cargo test -p
    /// ash_wasm_runtime --no-default-features --features browser`
    #[test]
    fn reads_what_the_linker_emitted() {
        let Ok(path) = std::env::var("ASH_THREADS_MODULE") else {
            eprintln!("set ASH_THREADS_MODULE to a wasm32-wasip1-threads module");
            return;
        };
        let bytes = std::fs::read(&path).expect("reading the module");
        let limits = super::imported_memory(&bytes).expect("it imports a memory");
        assert_eq!(limits.maximum, 16384, "the linker's declared maximum");
        assert!(limits.minimum > 0 && limits.minimum < limits.maximum);
        eprintln!("{path}: {limits:?}");
    }
}
