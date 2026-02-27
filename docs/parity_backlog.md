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
- Status: `open`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Add incompatible types I32 + Ptr in map iteration/sum path (Fun_255 pc=63).`

- Case: `TestMapDebug3`
- Mode: `interp | hybrid-default`
- Category: `vm-semantic`
- Owner: `unassigned`
- Status: `open`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Map iterator returns pointer-like integer value instead of expected Int payload.`

- Case: `TestMapIter`
- Mode: `interp | hybrid-default`
- Category: `vm-semantic`
- Owner: `unassigned`
- Status: `open`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Map value iteration returns pointer-like integers and key iteration order diverges from haxe --interp baseline.`

- Case: `TestMapSimple`
- Mode: `interp | hybrid-default`
- Category: `vm-semantic`
- Owner: `unassigned`
- Status: `open`
- First Seen: `2026-02-27`
- Linked PR: `n/a`
- Notes: `Map value iteration prints pointer-like integer instead of stored value (42).`
