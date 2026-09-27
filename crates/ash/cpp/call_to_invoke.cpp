//===- call_to_invoke.cpp - turn a call into an invoke --------------------===//
//
// A trap on wasm is an exception handler, so every call inside a trap region
// has to become an `invoke` whose unwind edge reaches it. The C API can build
// an invoke but cannot split a block after an existing call, which is what
// replacing one in place needs; LLVM's own utility does both.
//
//===----------------------------------------------------------------------===//

#include "llvm-c/Core.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/Local.h"

// Replace `Call` with an invoke unwinding to `Unwind`; the instructions after
// it move to a new block, which is returned.
extern "C" LLVMBasicBlockRef ash_call_to_invoke(LLVMValueRef Call,
                                                LLVMBasicBlockRef Unwind) {
  auto *CI = llvm::cast<llvm::CallInst>(llvm::unwrap(Call));
  return llvm::wrap(
      llvm::changeToInvokeAndSplitBasicBlock(CI, llvm::unwrap(Unwind)));
}
