# Parity backlog

Open parity deltas between `ash_cli` and the HashLink oracle. **Nothing is open.**

Add an entry when a case diverges; delete it when the fix lands. A list of
entries marked `fixed` is history, and git already holds that.

## Entry format

```
- Case:       <case-name>
- Mode:       interp | hybrid-default | hybrid-heavy
- Category:   vm-semantic | std-native | gc-rooting | tier-boundary |
              unsupported-opcode | infra
- Owner:      <name>
- Status:     open | in_progress | blocked
- First seen: YYYY-MM-DD
- Linked PR:  <url-or-sha>
- Notes:      root cause and fix
```

## Standing exception

The four map cases (`TestMapDebug2`, `TestMapDebug3`, `TestMapIter`,
`TestMapSimple`) were fixed on 2026-02-27 by post-call destination-kind
coercion and dynamic array boxing in the interpreter's call and array paths.

Their iteration order still differs under the `haxe --interp` fallback oracle,
so their `fallback_expectation` stays `exit_only` until a HashLink artifact
oracle exists. Do not tighten it against the fallback.
