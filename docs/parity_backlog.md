# Parity Backlog

This file tracks parity deltas between `ash_cli` and HashLink oracle runs.

## Entry Template

- Case: `<case-name>`
- Mode: `interp | hybrid-default | hybrid-heavy`
- Category: `vm-semantic | std-native | gc-rooting | tier-boundary | unsupported-opcode | infra`
- Owner: `<name>`
- Status: `open | in_progress | blocked | fixed`
- First Seen: `YYYY-MM-DD`
- Linked PR: `<url-or-sha>`
- Notes: `<root cause and fix summary>`

## Current

- Case: `TestMapDebug2`
- Mode: `interp | hybrid-default`
- Category: `vm-semantic`
- Owner: `unassigned`
- Status: `fixed`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Fixed by post-call destination-kind coercion and dynamic array boxing/unboxing in interpreter call/array paths.`

- Case: `TestMapDebug3`
- Mode: `interp | hybrid-default`
- Category: `infra`
- Owner: `unassigned`
- Status: `fixed`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Pointer payload issue fixed. Remaining order sensitivity differs under haxe --interp fallback; fallback_expectation set to exit_only until HashLink artifact oracle is available.`

- Case: `TestMapIter`
- Mode: `interp | hybrid-default`
- Category: `infra`
- Owner: `unassigned`
- Status: `fixed`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Pointer payload issue fixed. Remaining key/value iteration order differs under haxe --interp fallback; fallback_expectation set to exit_only until HashLink artifact oracle is available.`

- Case: `TestMapSimple`
- Mode: `interp | hybrid-default`
- Category: `vm-semantic`
- Owner: `unassigned`
- Status: `fixed`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Fixed by post-call destination-kind coercion and dynamic array boxing/unboxing in interpreter call/array paths.`
