# The Win64 `IMAGE_REL_AMD64_ADDR32NB` abort

A long Windows x86-64 run dies with:

```
LLVM ERROR: IMAGE_REL_AMD64_ADDR32NB relocation requires an ordered section layout
```

It is fatal and it kills the process. It appears after minutes rather than at
startup, which is the shape of the bug rather than a coincidence.

## What produces it

Three facts combine.

**Every JIT function carries unwind data.** `crates/ash/src/llvm/function.rs:295`
sets `frame-pointer=all` on every function, and nothing under
`crates/ash/src/llvm/` sets `nounwind`. On `x86_64-pc-windows-msvc` that means
LLVM emits `.pdata` and `.xdata` per function, which is three
`IMAGE_REL_AMD64_ADDR32NB` relocations each: BeginAddress, EndAddress and
UnwindData. All three are RVAs, so all three are resolved relative to
`__ImageBase`.

**One engine holds every module.** ash uses MCJIT, created once at
`crates/ash/src/llvm/module.rs:338` for the whole-module path and
`module.rs:768` for the tiered path. Every tier promotion then adds another
object to that same engine: `function.rs:755` for a promoted findex and
`function.rs:1319` for an OSR entry. MCJIT owns a single `RuntimeDyld`, which
creates its COFF x86-64 implementation on the first object and reuses it for
all the rest.

**RuntimeDyld computes `__ImageBase` once and caches it.** There is no real
image, so it fakes one: the lowest load address among the sections it has seen.
The value is memoised in a member on first use, and the relocation aborts when
an address falls outside the 32-bit window above it:

```cpp
uint64_t ImageBase;
uint64_t getImageBase() {
  if (!ImageBase) { ImageBase = UINT64_MAX;
    for (const SectionEntry &Section : Sections)
      if (Section.getLoadAddress() != 0)
        ImageBase = std::min(ImageBase, Section.getLoadAddress()); }
  return ImageBase; }
...
case COFF::IMAGE_REL_AMD64_ADDR32NB: {
  // ADDR32NB requires an offset less than 2GB from 'ImageBase'.
  // The MemoryManager can make sure this is always true by forcing the
  // memory layout to be: CodeSection < ReadOnlySection < ReadWriteSection.
  const uint64_t ImageBase = getImageBase();
  if (Value < ImageBase || ((Value - ImageBase) > UINT32_MAX)) { ... }
```

(`RuntimeDyldCOFFX86_64.h`. The comment names the invariant the memory manager
is supposed to provide. LLVM 21, which ash builds against, calls
`report_fatal_error` here where older releases only warned, which is why this
surfaces as `LLVM ERROR:` and a dead process.)

Nothing enforces that invariant across objects. LLVM's `SectionMemoryManager`
keeps three independent memory groups with independent placement hints and
falls back to letting the OS choose whenever a hint is taken. The first object
sets `ImageBase` and freezes it; the hundredth object's sections come from
unrelated `VirtualAlloc` calls. The first section to land below that frozen
value, or more than 4 GB above it, aborts the process.

That is why it takes minutes. Exposure is a function of how many objects the
engine has loaded, so it grows with promotion count and never resets.

## Options

**Give MCJIT a memory manager that keeps the ordering (recommended).** Reserve
one address-space region at engine creation and satisfy every section from it,
bump-allocated upward. The first section then sits at the region base, so the
cached `ImageBase` is that base, and every later section is above it and within
the region — both halves of the check hold for the life of the engine.

inkwell 0.8's `create_jit_execution_engine` does not expose
`LLVMMCJITCompilerOptions.MCJMM`, so this means calling
`LLVMCreateMCJITCompilerForModule` through `llvm-sys` directly and passing a
manager built with `LLVMCreateSimpleMCJITMemoryManager`. Four callbacks:
allocate-code, allocate-data, finalize, destroy.

Points that decide whether it works:

- Reserve with `MEM_RESERVE` and commit per section. The reservation is address
  space, not memory, so it can be generous — 512 MB costs nothing until used.
- Exhausting the region must be a hard error. Falling back to a plain
  `VirtualAlloc(NULL, ...)` puts the bug back, and puts it back in the case
  that is hardest to reproduce.
- `FinalizeMemory` applies `PAGE_EXECUTE_READ` to code and `PAGE_READONLY` to
  read-only data, then `FlushInstructionCache`.
- Windows only. Elsewhere the default manager is correct and this is
  unnecessary risk, so it goes behind `#[cfg(target_os = "windows")]`.

**Give each promotion its own engine.** A one-line change at `function.rs:755`.
`ImageBase` is then recomputed per object, so only the sections of that one
object matter, and those are allocated together. It costs an engine creation
per promotion — the `tiered engine` phase measures about 21 ms — plus separate
symbol binding and a section allocation per engine. It does not remove the
failure mode, it makes it much less likely, so treat it as a stopgap.

**Move to ORC/JITLink.** Its COFF x86-64 backend does not have the cached
`__ImageBase` design. This is the real answer and it is a much larger change
than either of the above.

**Do not mark JIT functions `nounwind`.** It removes the `.pdata` and with it
the relocation, but Win64 `longjmp` performs a real SEH unwind that wants
`.pdata`/`.xdata` for every frame between the throw and the trap.
`docs/windows-port.md` already lists that as untested; deleting the unwind info
makes it untested and definitely wrong.

## Reproducing and verifying

The crash itself is not reliably reproducible: whether a section lands below
the frozen base depends on what `VirtualAlloc` returns, so a passing run proves
nothing. Test the invariant instead of the crash.

Add a section log — one line per `add_module` giving each section's load
address and size — and on Windows assert that no section ever lands below the
lowest address seen in the first object. That is exactly the condition
`getImageBase` freezes, and it fails deterministically on a build without the
fix and passes on one with it.

`.github/workflows/test.yml` already has a `windows-2022` job that builds ash
and runs a corpus, so the check has somewhere to live. It needs a program that
promotes many times: a low promotion threshold over a corpus with many hot
functions loads many objects into the one engine, which is the condition being
tested.

## What is not established

The mechanism is traced through both ash and LLVM, and the abort message can
come from nowhere else in the code. What has not been shown is that this is the
*only* trigger in the reporter's run, because it has not been reproduced — no
Windows x86-64 machine was available. The section log above would settle it,
and would do so on the reporter's own machine without needing a repro.
