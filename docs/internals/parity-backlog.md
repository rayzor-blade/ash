# Parity backlog

Open output differences between ash and the HashLink oracle. Nothing is
open. Add an entry when a case diverges; delete it when the fix lands —
fixed entries are history, and git holds that.

```
- Case:       <case-name>
- Mode:       interp | hybrid-default | hybrid-heavy
- Category:   vm-semantic | std-native | gc-rooting | tier-boundary |
              unsupported-opcode | infra
- Status:     open | in_progress | blocked
- First seen: YYYY-MM-DD
- Notes:      root cause and fix
```

## Standing exception

`TestMapDebug2`, `TestMapDebug3`, `TestMapIter` and `TestMapSimple` match
HashLink, but their map iteration order differs under the `haxe --interp`
fallback oracle, so their `fallback_expectation` stays `exit_only`. Do not
tighten it against the fallback.
