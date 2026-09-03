//===- wasm_exception_model.cpp - the one thing the C API cannot say ------===//
//
// WebAssembly has no setjmp. A trap in compiled ash code is a setjmp, so on
// wasm the backend has to rewrite it into the exceptions proposal:
// `__wasm_setjmp` to register a jump target, `__wasm_setjmp_test` in a catch
// block to ask whether an arriving `__wasm_longjmp` belongs to this frame.
// That rewrite is `-wasm-enable-sjlj`, and it only half happens unless the
// target machine's exception model is also `wasm`.
//
// Half is the dangerous amount. `WebAssemblyLowerEmscriptenEHSjLj` rewrites
// the setjmp either way and expresses the catch side as `invoke`; the
// invokes are then removed again by `LowerInvoke`, which `TargetPassConfig`
// adds when the *asm info* says this target has no exception handling. The
// object that comes out references `__wasm_setjmp` and links and runs, and
// the first throw leaves the module as an engine-level exception that no
// handler in the program can see.
//
// The exception model is set correctly by `-exception-model=wasm`, which is
// an llc flag, and by clang, which assigns `TargetOptions::ExceptionModel`
// directly. Neither is reachable from the LLVM C API: `LLVMCreateTargetMachine`
// builds a default `TargetOptions` and copies only the ABI name into it, and
// no `LLVMTargetMachineOptionsSet*` names the exception model.
//
// The WebAssembly target does try to infer it -- `basicCheckForEHAndSjLj`
// promotes the model to `wasm` when `-wasm-enable-sjlj` is set -- but the
// constructor calls `initAsmInfo()` on the line before, so the asm info is
// already built from the un-promoted model and keeps `ExceptionHandling::None`
// for the rest of its life. That ordering is why enabling the option is not
// enough on its own.
//
// So this sets both, after construction, through public members: the option
// the late passes read and the asm info the pass *pipeline* reads. It is the
// state clang produces, reached the only way a library can reach it.
//
//===----------------------------------------------------------------------===//

#include "llvm-c/TargetMachine.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Target/TargetMachine.h"

extern "C" void ash_force_wasm_exception_model(LLVMTargetMachineRef Ref) {
  // The same conversion LLVM's own C bindings perform; `LLVMTargetMachineRef`
  // is an opaque handle to exactly this pointer.
  auto *TM = reinterpret_cast<llvm::TargetMachine *>(Ref);
  if (!TM)
    return;

  // Read by `addPreEmitPass`, which adds `WebAssemblyLateEHPrepare`.
  TM->Options.ExceptionModel = llvm::ExceptionHandling::Wasm;

  // Read by `TargetPassConfig::addPassesToHandleExceptions`, which chooses
  // between preparing wasm EH and discarding it. This is the half the
  // backend's own inference misses.
  if (const llvm::MCAsmInfo *AsmInfo = TM->getMCAsmInfo())
    const_cast<llvm::MCAsmInfo *>(AsmInfo)->setExceptionsType(
        llvm::ExceptionHandling::Wasm);
}
