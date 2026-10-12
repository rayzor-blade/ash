mod common;

use common::{ash_cli_bin, render_output, run_with_timeout, tests_dir};
use std::process::Command;
use std::time::Duration;

#[test]
fn unchanged_binary_trees_exposes_three_immediate_candidates() {
    let input = tests_dir().join("bench_binary_trees.hl");
    let before = std::fs::read(&input).unwrap();
    let mut cmd = Command::new(ash_cli_bin());
    cmd.arg("--quiet")
        .arg(&input)
        .env("ASH_TREE_FUSION_SURVEY", "only")
        .env_remove("ASH_AIR_NO_INLINE");
    let result = run_with_timeout(cmd, Duration::from_secs(30));
    assert!(
        !result.timed_out && result.output.status.success(),
        "{}",
        render_output(&result.output)
    );
    // A diagnostic invocation must not execute the benchmark.
    assert!(result.output.stdout.is_empty());
    let stderr = String::from_utf8_lossy(&result.output.stderr);
    let sites: Vec<_> = stderr
        .lines()
        .filter(|line| line.contains("caller=25 ") && line.contains("producer=24 "))
        .collect();
    assert_eq!(sites.len(), 4, "{stderr}");
    let candidates: Vec<_> = sites
        .iter()
        .filter(|line| line.contains("structural-candidate"))
        .collect();
    assert_eq!(candidates.len(), 3, "{stderr}");
    for line in candidates {
        assert!(line.contains("consumer=Some(23)"), "{line}");
        assert!(line.contains("depth_arg: 1"), "{line}");
        assert!(
            line.contains("allocation-elision contract unknown"),
            "{line}"
        );
        assert!(line.contains("invalidation contract unknown"), "{line}");
    }
    assert!(
        sites.iter().any(|line| line.contains("delayed use")),
        "{stderr}"
    );
    assert!(
        stderr.contains("truncated_callers=0 module_truncated=false"),
        "{stderr}"
    );
    assert_eq!(before, std::fs::read(input).unwrap());
}
