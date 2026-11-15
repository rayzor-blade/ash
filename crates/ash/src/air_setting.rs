//! What `ASH_AIR` selects, read in one place.

use std::sync::OnceLock;

/// Which consumer of AIR the process runs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AirSetting {
    /// AIR runs and the interpreter walks it. An unset variable selects this.
    Walk,
    /// Optimized AIR is serialized back to opcodes and the opcode interpreter
    /// runs those. A check on the walker; debug builds only.
    Serialize,
    /// No AIR: the opcode interpreter runs the bytecode.
    Off,
}

#[derive(Debug, PartialEq, Eq)]
enum Parsed {
    Setting(AirSetting),
    Unknown,
    /// `v2-serialize` in a build that does not accept it.
    Forbidden,
}

fn parse(value: Option<&str>, debug_build: bool) -> Parsed {
    match value {
        Some("0" | "off" | "none") => Parsed::Setting(AirSetting::Off),
        Some("v2-serialize") if debug_build => Parsed::Setting(AirSetting::Serialize),
        Some("v2-serialize") => Parsed::Forbidden,
        None | Some("" | "v2" | "2") => Parsed::Setting(AirSetting::Walk),
        Some(_) => Parsed::Unknown,
    }
}

/// The setting from `ASH_AIR`, resolved once.
///
/// `v2-serialize` outside a debug build ends the process: a run that quietly
/// fell back to the walker would be measured as if it were the serializer.
pub fn setting() -> AirSetting {
    static CELL: OnceLock<AirSetting> = OnceLock::new();
    *CELL.get_or_init(|| {
        let var = std::env::var("ASH_AIR").ok();
        match parse(var.as_deref(), cfg!(debug_assertions)) {
            Parsed::Setting(s) => s,
            Parsed::Unknown => {
                eprintln!(
                    "[air] ignoring ASH_AIR='{}' (expected v2|off); the interpreter walks AIR",
                    var.unwrap_or_default()
                );
                AirSetting::Walk
            }
            Parsed::Forbidden => {
                eprintln!("[air] ASH_AIR=v2-serialize is accepted only by a debug build");
                std::process::exit(2);
            }
        }
    })
}

/// Whether AIR runs at all: everything but `ASH_AIR=0`, `off` and `none`.
pub fn air_enabled() -> bool {
    setting() != AirSetting::Off
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unset_and_v2_walk() {
        for v in [None, Some(""), Some("v2"), Some("2")] {
            assert_eq!(parse(v, true), Parsed::Setting(AirSetting::Walk), "{v:?}");
            assert_eq!(parse(v, false), Parsed::Setting(AirSetting::Walk), "{v:?}");
        }
    }

    #[test]
    fn off_spellings_turn_air_off() {
        for v in ["0", "off", "none"] {
            assert_eq!(parse(Some(v), false), Parsed::Setting(AirSetting::Off));
        }
    }

    #[test]
    fn serialize_needs_a_debug_build() {
        assert_eq!(
            parse(Some("v2-serialize"), true),
            Parsed::Setting(AirSetting::Serialize)
        );
        assert_eq!(parse(Some("v2-serialize"), false), Parsed::Forbidden);
    }

    #[test]
    fn a_typo_is_reported_not_taken_as_off() {
        for v in ["v1", "on", "true", "air", "V2"] {
            assert_eq!(parse(Some(v), true), Parsed::Unknown, "{v}");
        }
    }
}
