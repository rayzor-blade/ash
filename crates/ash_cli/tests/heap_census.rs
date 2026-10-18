//! `hl.Gc.dumpMemory` ends with a typed census of the live heap: objects by
//! type, and the sizes of the pointer-free spans.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

/// The `type <count> <bytes> <name>` row for `name`.
fn type_row(dump: &str, name: &str) -> Option<(u64, u64)> {
    dump.lines().find_map(|line| {
        let mut parts = line.strip_prefix("type ")?.splitn(3, ' ');
        let count = parts.next()?.parse().ok()?;
        let bytes = parts.next()?.parse().ok()?;
        (parts.next()? == name).then_some((count, bytes))
    })
}

/// The `noptr-span <bucket> <count> <bytes>` row for `bucket`.
fn span_row(dump: &str, bucket: &str) -> Option<(u64, u64)> {
    dump.lines().find_map(|line| {
        let mut parts = line.strip_prefix("noptr-span ")?.split(' ');
        if parts.next()? != bucket {
            return None;
        }
        Some((parts.next()?.parse().ok()?, parts.next()?.parse().ok()?))
    })
}

fn stat(dump: &str, key: &str) -> u64 {
    dump.lines()
        .find_map(|line| line.strip_prefix(key)?.strip_prefix(' ')?.parse().ok())
        .unwrap_or_else(|| panic!("no {key} in the dump:\n{dump}"))
}

/// The fixture program, compiled when Haxe is here and otherwise the
/// committed bytecode, inside a directory the caller keeps alive.
fn fixture(dir: &tempfile::TempDir) -> std::path::PathBuf {
    let fixture = dir.path().join("heap_census.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestHeapCensus", "-hl"])
            .arg(&fixture)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_heap_census.hl"), &fixture)
            .expect("committed heap census bytecode fixture");
    }
    fixture
}

#[test]
fn dump_memory_reports_live_objects_by_type() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = fixture(&dir);
    for mode in ["interp", "hybrid"] {
        let dump_path = dir.path().join(format!("{mode}.dump"));
        let mut command = Command::new(ash_cli_bin());
        command.args(["--mode", mode]).arg(&fixture).arg(&dump_path);
        let result = run_with_timeout(command, Duration::from_secs(60));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode}: {}",
            render_output(&result.output)
        );
        let dump = std::fs::read_to_string(&dump_path).unwrap();

        // 5000 live Cells of 32 bytes each; a type pointer stored in a field
        // can add a few phantom rows, so the count has a small allowance.
        let (count, bytes) = type_row(&dump, "Cell")
            .unwrap_or_else(|| panic!("{mode}: no Cell row in the dump:\n{dump}"));
        assert!((5000..=5010).contains(&count), "{mode}: Cell count {count}");
        assert!(
            (160_000..=160_320).contains(&bytes),
            "{mode}: Cell bytes {bytes}"
        );

        // `Array<Cell>` is a NativeArray<Dynamic> of at least 5000 pointers.
        let (_, array_bytes) = type_row(&dump, "array<dynamic>")
            .unwrap_or_else(|| panic!("{mode}: no array<dynamic> row in the dump:\n{dump}"));
        assert!(array_bytes >= 40_000, "{mode}: array bytes {array_bytes}");

        // The 1 MiB buffer and the integer array hold no pointers.
        let (_, blob) = span_row(&dump, "<=1M")
            .unwrap_or_else(|| panic!("{mode}: no <=1M span in the dump:\n{dump}"));
        assert!(blob >= 1 << 20, "{mode}: blob span {blob}");
        assert!(
            span_row(&dump, "<=256K").is_some(),
            "{mode}: no integer array span"
        );

        // Most of the pointer-bearing heap is attributed to a type.
        let live = stat(&dump, "census-pointer-live-bytes");
        let classified = stat(&dump, "census-classified-bytes");
        assert!(
            classified * 10 >= live * 8,
            "{mode}: classified {classified} of {live}"
        );
    }
}

#[test]
fn asking_for_the_census_prints_it_when_the_process_exits() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = fixture(&dir);
    for mode in ["interp", "hybrid"] {
        let mut command = Command::new(ash_cli_bin());
        command
            .args(["--mode", mode])
            .arg(&fixture)
            .arg(dir.path().join("unused.dump"))
            .env("ASH_GC_CENSUS", "1");
        let result = run_with_timeout(command, Duration::from_secs(60));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode}: {}",
            render_output(&result.output)
        );
        let stderr = String::from_utf8_lossy(&result.output.stderr);
        assert!(stderr.contains("[gc-census] begin"), "{mode}: {stderr}");
        assert!(stderr.contains("[gc-census] end"), "{mode}: {stderr}");
        // The statics still hold the cells when the program has returned.
        let cell = stderr
            .lines()
            .filter_map(|l| l.strip_prefix("[gc-census] "))
            .find_map(|l| type_row(l, "Cell"))
            .unwrap_or_else(|| panic!("{mode}: no Cell row on stderr:\n{stderr}"));
        assert!(
            (5000..=5010).contains(&cell.0),
            "{mode}: Cell count {}",
            cell.0
        );
    }
}
