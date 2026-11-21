//! The loop threshold a preset carries apart from its call threshold.

use ash_interp::interpreter::TierPreset;

#[test]
fn only_game_holds_loops_apart_from_calls() {
    let game = TierPreset::Game.to_config();
    assert_eq!(game.opt_threshold, 200);
    assert_eq!(game.loop_threshold, Some(10_000));

    for name in TierPreset::names().iter().filter(|n| **n != "game") {
        let cfg = TierPreset::parse(name).unwrap().to_config();
        assert_eq!(cfg.loop_threshold, None, "{name} follows its opt threshold");
    }
}
