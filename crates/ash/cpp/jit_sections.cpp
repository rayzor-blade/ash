//===- jit_sections.cpp - where MCJIT put an object's sections ------------===//
//
// A compiled frame reports the line it is stopped on by mapping its pc
// through the line table the backend emitted for it. MCJIT loads that table
// like any other section, relocated against the code it describes, but only
// when asked: by default it loads what execution needs and skips the rest,
// and nothing in the C API asks for more or says where a loaded section
// went. Both are one call away in C++.
//
// The listener reports every loaded section's name, load address and size
// once per object, from inside the load, on the thread that asked for the
// function address.
//
//===----------------------------------------------------------------------===//

#include "llvm-c/ExecutionEngine.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/ExecutionEngine/RuntimeDyld.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/Endian.h"

#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

extern "C" {

struct AshLoadedSection {
  const char *name;
  /// Where the section was loaded; 0 for one the loader keeps without an
  /// address, which on ELF is every section execution does not need.
  uint64_t address;
  uint64_t size;
  /// The section's bytes with relocations applied, valid for the call, or
  /// null when they are only available at `address`.
  const uint8_t *data;
};

typedef void (*ash_sections_callback)(void *context, const AshLoadedSection *sections,
                                      size_t count);

} // extern "C"

namespace {

using namespace llvm;
using namespace llvm::object;

/// A section's bytes with its relocations applied against where the loader
/// put their targets.
///
/// The loader copies a section it was told to process but does not give it
/// an address on ELF, and its debug object is the file with only the loaded
/// sections' addresses updated, so a line table there names offsets into an
/// image that was never at 0. Every relocation such a table carries is
/// absolute -- an 8 or 4 byte symbol-plus-addend -- so they are applied
/// here, against the load address of the symbol's section.
std::vector<uint8_t> relocatedCopy(const ObjectFile &object, const SectionRef &section,
                                   const RuntimeDyld::LoadedObjectInfo &loaded) {
  std::vector<uint8_t> bytes;
  Expected<StringRef> contents = section.getContents();
  if (!contents) {
    consumeError(contents.takeError());
    return bytes;
  }
  bytes.assign(contents->bytes_begin(), contents->bytes_end());
  const auto *elf = dyn_cast<ELFObjectFileBase>(&object);
  if (!elf)
    return bytes;
  for (const SectionRef &other : object.sections()) {
    Expected<section_iterator> relocated = other.getRelocatedSection();
    if (!relocated) {
      consumeError(relocated.takeError());
      continue;
    }
    if (*relocated == object.section_end() || **relocated != section)
      continue;
    for (const RelocationRef &reloc : other.relocations()) {
      symbol_iterator symbol = reloc.getSymbol();
      if (symbol == object.symbol_end())
        continue;
      Expected<uint64_t> value = symbol->getAddress();
      Expected<section_iterator> home = symbol->getSection();
      Expected<int64_t> addend = ELFRelocationRef(reloc).getAddend();
      if (!value || !home || !addend) {
        if (!value)
          consumeError(value.takeError());
        if (!home)
          consumeError(home.takeError());
        if (!addend)
          consumeError(addend.takeError());
        continue;
      }
      uint64_t base = *home == object.section_end() ? 0 : loaded.getSectionLoadAddress(**home);
      uint64_t target = base + *value + static_cast<uint64_t>(*addend);
      uint64_t offset = reloc.getOffset();
      // Width by the field the relocation writes; the type numbers differ
      // per architecture, so it is read off the object.
      SmallVector<char, 16> typeName;
      reloc.getTypeName(typeName);
      StringRef type(typeName.data(), typeName.size());
      unsigned width = type.ends_with("_64") ? 8 : type.ends_with("_32") ? 4 : 0;
      if (width == 0 || offset + width > bytes.size())
        continue;
      if (width == 8)
        support::endian::write64le(bytes.data() + offset, target);
      else
        support::endian::write32le(bytes.data() + offset, static_cast<uint32_t>(target));
    }
  }
  return bytes;
}

class SectionListener final : public JITEventListener {
public:
  SectionListener(ash_sections_callback callback, void *context)
      : callback(callback), context(context) {}

  void notifyObjectLoaded(ObjectKey, const ObjectFile &object,
                          const RuntimeDyld::LoadedObjectInfo &loaded) override {
    std::vector<std::string> names;
    std::vector<std::vector<uint8_t>> copies;
    std::vector<AshLoadedSection> sections;
    for (const SectionRef &section : object.sections()) {
      Expected<StringRef> name = section.getName();
      if (!name) {
        consumeError(name.takeError());
        continue;
      }
      uint64_t address = loaded.getSectionLoadAddress(section);
      const uint8_t *data = nullptr;
      if (address != 0) {
        // Loaded with an address: relocated where it lies.
        data = reinterpret_cast<const uint8_t *>(address);
      } else if (name->ends_with("debug_line")) {
        copies.push_back(relocatedCopy(object, section, loaded));
        data = copies.back().data();
      }
      names.push_back(name->str());
      sections.push_back({nullptr, address, section.getSize(), data});
    }
    for (size_t i = 0; i < sections.size(); ++i)
      sections[i].name = names[i].c_str();
    callback(context, sections.data(), sections.size());
  }

private:
  ash_sections_callback callback;
  void *context;
};

} // namespace

extern "C" void ash_llvm_track_sections(LLVMExecutionEngineRef engine_ref,
                                        ash_sections_callback callback, void *context) {
  ExecutionEngine *engine = unwrap(engine_ref);
  // Load and relocate every section, the line tables included. Before the
  // first object: the loader reads the flag once, when it is created.
  engine->setProcessAllSections(true);
  // Owned by the engine for its life; the engine outlives the process here.
  engine->RegisterJITEventListener(new SectionListener(callback, context));
}
