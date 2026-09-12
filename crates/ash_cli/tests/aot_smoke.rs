//! Every AOT binary must print exactly what the JIT prints.
//!
//! This is the gate on the emitter. It compiles a corpus with `--build`, the same one command a user runs, and compares the binary's output against
//! the same program under the JIT, byte for byte. A difference here is a
//! lowering bug: the two tiers ran the same bytecode and disagreed.
//!
//! It replaced a shell script, which could only run where that shell did and
//! was invisible to `cargo test`.
//!
//! `cargo test -p ash --test aot_smoke -- --nocapture` to watch it work.
//! `ASH_SMOKE_PROGRAMS="a.hl b.hl"` swaps the corpus.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

/// How long any one `ash` invocation may take before it is treated as hung.
///
/// There is a limit at all because a subprocess that never returns takes the
/// whole job down with it: this test ran 75 minutes on a CI arm64 runner and
/// reported nothing, because it prints only once a program has passed and
/// waits forever for one that does not. A named failure after a few minutes
/// is worth more than a stall.
fn limit() -> Duration {
    Duration::from_secs(
        std::env::var("ASH_SMOKE_TIMEOUT")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(300),
    )
}

/// The corpus, chosen so each entry once caught something.
const PROGRAMS: &[&str] = &[
    "bench_fib.hl",
    "test_basic.hl",
    "test_stdlib.hl",
    "bench_deltablue.hl",
    // A virtual's `lookup` was baked null while `indexes` was baked, so the
    // lazy-init guard never fired and every hash-keyed field access failed --
    // `Reflect.field` and plain `dyn.name` returning null, `hasField` false,
    // `Std.string` aborting -- while the interpreter and the JIT both ran it
    // correctly. Nothing else in this list touches a virtual.
    "test_feature_typedef_anon.hl",
    "test_safe_cast_virtual.hl",
    // A virtual-method closure over a receiver whose class inherits the
    // method: the vtable slot indexed the class's own method list.
    "test_virtual_closure_inherited.hl",
    "test_gettype_null.hl",
    "test_std_reflect_type.hl",
];

fn repo_root() -> PathBuf {
    // CARGO_MANIFEST_DIR is crates/ash_cli.
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("the crate lives two levels below the root")
        .to_path_buf()
}

/// What one invocation did: its output, whether it succeeded, and whether it
/// had to be killed.
struct Ran {
    text: String,
    ok: bool,
    hung: bool,
    /// The raw exit code, kept because on Windows it is the NTSTATUS of
    /// whatever killed a process that printed nothing on the way out.
    code: Option<i32>,
}

impl Ran {
    /// `ok`, or the exit code in hex: an NTSTATUS reads as one, not as the
    /// negative decimal `ExitStatus` prints.
    fn exit(&self) -> String {
        match (self.ok, self.code) {
            (true, _) => "ok".to_string(),
            (false, Some(code)) => format!("code {:#x}", code as u32),
            (false, None) => "killed".to_string(),
        }
    }
}

/// Run one command, giving up on it after [`limit`].
///
/// Output goes to files rather than pipes. Polling a child while it fills a
/// pipe nobody is draining deadlocks on the pipe instead of the deadline,
/// which would reintroduce the hang this exists to catch.
fn run(binary: &Path, args: &[&str], label: &str) -> Ran {
    let scratch = std::env::temp_dir().join("ash-aot-smoke");
    let _ = std::fs::create_dir_all(&scratch);
    let out_path = scratch.join(format!("{label}.stdout"));
    let err_path = scratch.join(format!("{label}.stderr"));
    let stdout = std::fs::File::create(&out_path).expect("capture file");
    let stderr = std::fs::File::create(&err_path).expect("capture file");

    let mut child = Command::new(binary)
        .args(args)
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
        .unwrap_or_else(|e| panic!("running {}: {e}", binary.display()));

    let deadline = Instant::now() + limit();
    let status = loop {
        match child.try_wait().expect("waiting on a child") {
            Some(status) => break Some(status),
            None if Instant::now() >= deadline => {
                let _ = child.kill();
                let _ = child.wait();
                break None;
            }
            None => std::thread::sleep(Duration::from_millis(50)),
        }
    };

    let mut text = std::fs::read_to_string(&out_path).unwrap_or_default();
    text.push_str(&std::fs::read_to_string(&err_path).unwrap_or_default());
    Ran {
        text,
        ok: status.map(|s| s.success()).unwrap_or(false),
        hung: status.is_none(),
        code: status.and_then(|s| s.code()),
    }
}

/// Say what is about to happen, before it happens.
///
/// The only thing that made the arm64 stall unactionable was that nothing had
/// been printed by the time it hung, so the log named no program.
fn announce(what: &str) {
    println!("{what}");
    let _ = std::io::stdout().flush();
}

/// The interpreter announces its own return value; nothing else does.
fn normalize(text: &str) -> String {
    text.lines()
        .filter(|l| !l.contains("returned: Void"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn every_aot_binary_matches_the_jit() {
    let ash = PathBuf::from(env!("CARGO_BIN_EXE_ash"));
    let root = repo_root();
    let tests = root.join("crates/ash/test/tests");
    let scratch = std::env::temp_dir().join("ash-aot-smoke");
    std::fs::create_dir_all(&scratch).expect("scratch directory");

    let programs: Vec<PathBuf> = match std::env::var("ASH_SMOKE_PROGRAMS") {
        Ok(list) => list.split_whitespace().map(PathBuf::from).collect(),
        Err(_) => PROGRAMS.iter().map(|p| tests.join(p)).collect(),
    };

    let mut failures: Vec<String> = Vec::new();
    for program in &programs {
        let name = program
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_default();
        if !program.exists() {
            failures.push(format!("{name}: {} does not exist", program.display()));
            continue;
        }
        let binary = scratch.join(&name);
        announce(&format!("{name}: building"));
        let emit = run(
            &ash,
            &[
                "--build",
                &binary.to_string_lossy(),
                &program.to_string_lossy(),
            ],
            &format!("{name}-build"),
        );
        if emit.hung {
            failures.push(format!("{name}: build hung, killed after {:?}", limit()));
            continue;
        }
        if !emit.ok {
            failures.push(format!(
                "{name}: build failed\n{}",
                emit.text
                    .lines()
                    .rev()
                    .take(4)
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
            continue;
        }

        announce(&format!("{name}: running under the jit"));
        let jit = run(
            &ash,
            &["--mode", "jit", &program.to_string_lossy()],
            &format!("{name}-jit"),
        );
        announce(&format!("{name}: running the binary"));
        let aot = run(&binary, &[], &format!("{name}-aot"));
        if jit.hung || aot.hung {
            failures.push(format!(
                "{name}: {} hung, killed after {:?}",
                if jit.hung { "the jit" } else { "the binary" },
                limit()
            ));
            continue;
        }
        if normalize(&jit.text) != normalize(&aot.text) {
            let jit_out = normalize(&jit.text);
            let aot_out = normalize(&aot.text);
            let first = jit_out
                .lines()
                .zip(aot_out.lines())
                .find(|(a, b)| a != b)
                .map(|(a, b)| format!("  jit: {a}\n  aot: {b}"))
                .unwrap_or_else(|| {
                    // Every line they both printed agreed, so the shorter run
                    // stopped early rather than saying something different.
                    // Exit status is what tells you why -- and it is the only
                    // thing that does when the binary dies before printing
                    // anything at all, which a bare line count cannot express.
                    let (jn, an) = (jit_out.lines().count(), aot_out.lines().count());
                    let short = if an < jn { "the binary" } else { "the jit" };
                    format!(
                        "  {short} stopped early; every line they share agreed\n  \
                         jit: {jn} lines, exited {}\n  \
                         aot: {an} lines, exited {}\n  \
                         last line both printed: {}\n  \
                         next line the jit printed: {}",
                        jit.exit(),
                        aot.exit(),
                        jit_out
                            .lines()
                            .zip(aot_out.lines())
                            .last()
                            .map(|(a, _)| a)
                            .unwrap_or("<none>"),
                        jit_out.lines().nth(an.min(jn)).unwrap_or("<none>"),
                    )
                });
            failures.push(format!("{name}: differs from the JIT\n{first}"));
        } else {
            announce(&format!("{name}: ok"));
        }
    }

    assert!(
        failures.is_empty(),
        "AOT binaries disagreed with the JIT:\n{}",
        failures.join("\n")
    );
}
