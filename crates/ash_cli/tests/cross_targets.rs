//! What ash can compile *for*, as opposed to what it runs on.
//!
//! Linking an executable needs the target platform's linker and system
//! libraries, so `--build` is host-only for native targets. Emitting the
//! object is not: since the ABI is chosen before anything is decoded, every
//! layout, call and constant comes out for the named target rather than for
//! the machine doing the compiling. That is what this checks, across a
//! deliberately wide set.
//!
//! The width is the point. Thirty-two-bit targets exercise a different
//! `TargetAbi` path than the host does -- pointer size, field offsets, enum
//! layout -- and big-endian s390x exercises another. Emitting for one is how
//! that path gets used at all outside a wasm build.
//!
//! # What this does and does not prove
//!
//! It proves the emitter produced an object of the right architecture and
//! word size. It does not prove the object links, or that the program runs:
//! nothing here has a linker or a machine for those targets. Only x86_64,
//! aarch64 and wasm32 are exercised end to end, by `aot_smoke` and
//! `wasm_target`.
//!
//! # Triples are LLVM's, not Rust's
//!
//! `riscv64gc-unknown-linux-gnu` is a Rust target name and fails here:
//! `riscv64gc` is not an architecture LLVM knows, so the triple resolves to
//! no target at all. The LLVM spelling is `riscv64-unknown-linux-gnu`. The
//! same trap sits behind `wasm32-wasip1`, which is why that one is
//! normalised on the way in.

use std::path::{Path, PathBuf};
use std::process::Command;

/// ELF `e_machine` values, from the gABI.
mod machine {
    pub const X86: u16 = 0x03;
    pub const ARM: u16 = 0x28;
    pub const X86_64: u16 = 0x3E;
    pub const AARCH64: u16 = 0xB7;
    pub const RISCV: u16 = 0xF3;
    pub const S390: u16 = 0x16;
    pub const PPC64: u16 = 0x15;
}

/// Word size, in the ELF header's own terms.
const CLASS_32: u8 = 1;
const CLASS_64: u8 = 2;

struct Target {
    triple: &'static str,
    class: u8,
    machine: u16,
}

/// One per architecture family ash claims to emit for.
const TARGETS: &[Target] = &[
    Target {
        triple: "i686-unknown-linux-gnu",
        class: CLASS_32,
        machine: machine::X86,
    },
    Target {
        triple: "x86_64-unknown-linux-gnu",
        class: CLASS_64,
        machine: machine::X86_64,
    },
    Target {
        triple: "aarch64-unknown-linux-gnu",
        class: CLASS_64,
        machine: machine::AARCH64,
    },
    Target {
        triple: "armv7-unknown-linux-gnueabihf",
        class: CLASS_32,
        machine: machine::ARM,
    },
    Target {
        triple: "riscv32-unknown-linux-gnu",
        class: CLASS_32,
        machine: machine::RISCV,
    },
    Target {
        triple: "riscv64-unknown-linux-gnu",
        class: CLASS_64,
        machine: machine::RISCV,
    },
    Target {
        triple: "powerpc64le-unknown-linux-gnu",
        class: CLASS_64,
        machine: machine::PPC64,
    },
    // Big-endian, which nothing else in the suite is.
    Target {
        triple: "s390x-unknown-linux-gnu",
        class: CLASS_64,
        machine: machine::S390,
    },
];

fn program() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../ash/test/tests/test_basic.hl")
        .canonicalize()
        .expect("test_basic.hl")
}

/// `(class, endianness, e_machine)` from an ELF header.
fn elf_header(bytes: &[u8]) -> Option<(u8, u8, u16)> {
    if bytes.len() < 20 || &bytes[..4] != b"\x7fELF" {
        return None;
    }
    let class = bytes[4];
    let endian = bytes[5];
    // `e_machine` is at 0x12, in the file's own byte order.
    let raw = [bytes[18], bytes[19]];
    let machine = if endian == 2 {
        u16::from_be_bytes(raw)
    } else {
        u16::from_le_bytes(raw)
    };
    Some((class, endian, machine))
}

#[test]
fn emits_an_object_for_every_target_it_claims() {
    let ash = PathBuf::from(env!("CARGO_BIN_EXE_ash"));
    let hl = program();
    let dir = std::env::temp_dir().join("ash-cross-targets");
    std::fs::create_dir_all(&dir).expect("scratch dir");

    let mut failures: Vec<String> = Vec::new();
    for target in TARGETS {
        let out = dir.join(format!("{}.o", target.triple));
        let _ = std::fs::remove_file(&out);
        let result = Command::new(&ash)
            .arg("--emit-aot")
            .arg(&out)
            .arg("--target")
            .arg(target.triple)
            .arg(&hl)
            .output()
            .expect("running ash");

        if !result.status.success() {
            failures.push(format!(
                "{}: ash exited {}: {}",
                target.triple,
                result.status,
                String::from_utf8_lossy(&result.stderr)
                    .lines()
                    .last()
                    .unwrap_or("")
            ));
            continue;
        }
        let bytes = match std::fs::read(&out) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!("{}: no object written: {e}", target.triple));
                continue;
            }
        };
        match elf_header(&bytes) {
            None => failures.push(format!(
                "{}: {} bytes, but not an ELF object",
                target.triple,
                bytes.len()
            )),
            Some((class, _, machine)) => {
                if class != target.class || machine != target.machine {
                    failures.push(format!(
                        "{}: emitted class {class} machine {machine:#x}, \
                         expected class {} machine {:#x}",
                        target.triple, target.class, target.machine
                    ));
                }
            }
        }
    }

    assert!(
        failures.is_empty(),
        "{} of {} targets did not emit correctly:\n  {}",
        failures.len(),
        TARGETS.len(),
        failures.join("\n  ")
    );
}
