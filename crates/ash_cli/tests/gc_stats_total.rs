//! `hl.Gc.stats().totalAllocated` and `allocationCount` move by what the
//! program allocated, so a window with no allocation reads the same as an
//! empty one and a batch costs what the same allocations cost one at a time.
mod common;

use common::{ash_cli_bin, haxe_available, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

fn numbers(list: &str) -> Vec<f64> {
    list.split(',').map(|n| n.parse().unwrap()).collect()
}

#[test]
fn stats_count_what_the_program_allocated() {
    let dir = tempfile::tempdir().unwrap();
    let hl = dir.path().join("gc_stats_total.hl");
    if haxe_available() {
        let output = Command::new("haxe")
            .arg("-cp")
            .arg(tests_dir())
            .args(["-main", "TestGcStatsTotal", "-hl"])
            .arg(&hl)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "haxe: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    } else {
        std::fs::copy(tests_dir().join("test_gc_stats_total.hl"), &hl)
            .expect("committed bytecode fixture");
    }
    for mode in ["interp", "hybrid", "jit"] {
        let mut command = Command::new(ash_cli_bin());
        command.args(["--mode", mode]).arg(&hl);
        let result = run_with_timeout(command, Duration::from_secs(60));
        assert!(
            !result.timed_out && result.output.status.success(),
            "{mode}: {}",
            render_output(&result.output)
        );
        let stdout = String::from_utf8_lossy(&result.output.stdout).into_owned();
        let line = |name: &str| -> Vec<String> {
            stdout
                .lines()
                .find_map(|l| l.strip_prefix(&format!("{name} ")))
                .unwrap_or_else(|| panic!("{mode}: no {name} line in {stdout}"))
                .split(' ')
                .map(str::to_owned)
                .collect()
        };
        let (empty, single, bulk) = (line("empty"), line("single"), line("bulk"));

        // Nothing allocated between two reads costs only the read's own result,
        // and the same each time.
        let (empty_bytes, empty_objects) = (numbers(&empty[0]), numbers(&empty[1]));
        assert!(empty_bytes[0] > 0.0, "{mode}: {stdout}");
        assert!(empty_bytes.iter().all(|b| *b == empty_bytes[0]), "{mode}: {stdout}");
        assert!(empty_objects.iter().all(|o| *o == empty_objects[0]), "{mode}: {stdout}");

        // One allocation moves it, by the same amount every time.
        let (single_bytes, single_objects) = (numbers(&single[0]), numbers(&single[1]));
        assert!(single_bytes[0] > 0.0, "{mode}: {stdout}");
        assert!(single_objects[0] >= 1.0, "{mode}: {stdout}");
        assert!(single_bytes.iter().all(|b| *b == single_bytes[0]), "{mode}: {stdout}");
        assert!(single_objects.iter().all(|o| *o == single_objects[0]), "{mode}: {stdout}");

        // Five thousand of them cost five thousand times as much.
        assert_eq!(
            (bulk[0].parse::<f64>().unwrap(), bulk[1].parse::<f64>().unwrap()),
            (single_bytes[0], single_objects[0]),
            "{mode}: {stdout}"
        );
        assert!(stdout.lines().any(|l| l == "monotone true"), "{mode}: {stdout}");
    }
}
