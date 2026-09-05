#!/usr/bin/env python3
"""Compare two conformance arms.

The harness records a row per (suite, program, engine) with the tallies, and
under `cases` only the cases that did NOT pass. So two arms agree exactly when
their tallies match and their not-OK sets are the same -- which is the question
an instrumentation pass has to answer, and one a summary percentage cannot.
"""
import json, sys

d = json.load(open(sys.argv[1]))
a, b = (sys.argv[2], sys.argv[3]) if len(sys.argv) > 3 else ("ash:wasm", "ash:wasm-fibers")

TALLY = ("cases_total", "cases_empty", "cases_attemptable", "cases_ok",
         "cases_failed", "cases_crashed", "cases_timeout",
         "assertions_of_completed", "assertions_passed")

def rows(engine):
    return {(r["suite"], r["program"]): r
            for r in d["results"] if r.get("engine") == engine}

A, B = rows(a), rows(b)
print(f"{a}: {len(A)} programs   {b}: {len(B)} programs")
differ = 0
for key in sorted(set(A) | set(B)):
    ra, rb = A.get(key), B.get(key)
    if ra is None or rb is None:
        print(f"  {key}: present in only one arm"); differ += 1; continue
    ta = {f: ra.get(f) for f in TALLY}
    tb = {f: rb.get(f) for f in TALLY}
    na = {c.get("case") for c in (ra.get("cases") or [])}
    nb = {c.get("case") for c in (rb.get("cases") or [])}
    same = ta == tb and na == nb
    mark = "same" if same else "DIFFERS"
    print(f"  {key[0]}/{key[1]}: {mark}  "
          f"ok {ta['cases_ok']}/{ta['cases_attemptable']} vs {tb['cases_ok']}/{tb['cases_attemptable']}, "
          f"assertions {ta['assertions_passed']}/{ta['assertions_of_completed']} vs "
          f"{tb['assertions_passed']}/{tb['assertions_of_completed']}")
    if not same:
        differ += 1
        for f in TALLY:
            if ta[f] != tb[f]:
                print(f"      {f}: {ta[f]} -> {tb[f]}")
        for c in sorted(na - nb): print(f"      only {a} fails: {c}")
        for c in sorted(nb - na): print(f"      only {b} fails: {c}")
    else:
        by_status = {}
        for c in ra.get("cases") or []:
            by_status.setdefault(c.get("status"), []).append(c.get("case"))
        for st, names in sorted(by_status.items()):
            print(f"      both {st}: {len(names)}"
                  + (f"  e.g. {names[0]}" if names else ""))
print(f"\n{differ} program rows differ between the arms")
