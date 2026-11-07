//===- lazy_bodies.cpp - read function bodies on demand -------------------===//
//
// A module read with LLVMGetBitcodeModuleInContext2 holds its function bodies
// as unread bitcode until something asks for one. The C API has no call that
// asks for one, and none that drops one without reading it first; both are a
// line of LLVM's own.
//
//===----------------------------------------------------------------------===//

#include "llvm-c/Core.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/Error.h"

#include <cstdlib>
#include <cstring>
#include <string>

// Read `F`'s body. Null on success, else a malloc'd message for the caller to
// free. A function that is not waiting to be read succeeds without doing
// anything.
extern "C" char *ash_materialize_function(LLVMValueRef F) {
  auto *Fn = llvm::cast<llvm::Function>(llvm::unwrap(F));
  if (llvm::Error E = Fn->materialize()) {
    std::string Message = llvm::toString(std::move(E));
    return strdup(Message.c_str());
  }
  return nullptr;
}

// Reduce `F` to a declaration. A body that was never read is never read:
// the function stops being readable.
extern "C" void ash_delete_function_body(LLVMValueRef F) {
  llvm::cast<llvm::Function>(llvm::unwrap(F))->deleteBody();
}
