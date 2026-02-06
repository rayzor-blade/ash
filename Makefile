# Makefile for cross-compilation of Rust project

# Default target is the host system
HOST_TARGET := $(shell rustc -vV | sed -n 's|host: ||p')

# LLVM version - update this as needed
LLVM_VERSION := 180

# Operating system
OS := $(shell uname)

# List all installed targets
INSTALLED_TARGETS := $(shell rustup target list | grep '(installed)' | awk '{print $$1}')

# Default to building all targets if none specified
TARGETS ?= $(INSTALLED_TARGETS)

# Cargo command (use 'cross' if available, fallback to 'cargo')
CARGO := $(shell command -v cross 2> /dev/null || echo cargo)

# Temporary Cargo.toml for release profile
TMP_CARGO_TOML := $(shell mktemp)

# Create temporary Cargo.toml with release profile
define create_tmp_cargo_toml
	cp Cargo.toml $(TMP_CARGO_TOML)
	echo "[profile.release]" >> $(TMP_CARGO_TOML)
	echo "lto = \"fat\"" >> $(TMP_CARGO_TOML)
	echo "codegen-units = 1" >> $(TMP_CARGO_TOML)
	echo "opt-level = 3" >> $(TMP_CARGO_TOML)
	echo "debug = false" >> $(TMP_CARGO_TOML)
endef

# Clean up temporary files
define cleanup
	rm -f $(TMP_CARGO_TOML)
endef

# Set LLVM path based on OS and target
define set_llvm_path
	$(if $(filter Darwin,$(OS)), \
		$(if $(filter aarch64-apple-darwin,$1), \
			export LLVM_SYS_$(LLVM_VERSION)_PREFIX=/opt/homebrew/opt/llvm, \
			export LLVM_SYS_$(LLVM_VERSION)_PREFIX=/usr/local/opt/llvm \
		), \
		$(if $(filter Linux,$(OS)), \
			export LLVM_SYS_$(LLVM_VERSION)_PREFIX=/usr/lib/llvm-$(LLVM_VERSION), \
			echo "Unsupported operating system: $(OS)" && exit 1 \
		) \
	)
endef

# Build for a specific target
define build_target
	@echo "Building for target: $1"
	@$(call set_llvm_path,$1)
	@echo "Building ash_std package for target: $1"
	@CARGO_TOML_PATH=$(TMP_CARGO_TOML) CARGO_INCREMENTAL=0  \
		$(CARGO) build --release --package ash_std --target $1
	@CARGO_TOML_PATH=$(TMP_CARGO_TOML) CARGO_INCREMENTAL=0  \
		$(CARGO) build --release --target $1
	@echo "Build completed for target: $1"
endef

# Default target: build for host
.PHONY: default
default: $(HOST_TARGET)

# Target to build for all installed targets
.PHONY: all
all: $(INSTALLED_TARGETS)

# Generic target rule
.PHONY: $(TARGETS)
$(TARGETS):
	$(call create_tmp_cargo_toml)
	$(call build_target,$@)
	$(call cleanup)

# Clean build artifacts
.PHONY: clean
clean:
	cargo clean

# Show help
.PHONY: help
help:
	@echo "Usage:"
	@echo "  make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  default      Build for the host target ($(HOST_TARGET))"
	@echo "  all          Build for all installed targets"
	@echo "  <target>     Build for a specific target"
	@echo "  clean        Clean build artifacts"
	@echo "  help         Show this help message"
	@echo ""
	@echo "Installed targets:"
	@echo "  $(INSTALLED_TARGETS)" | fmt -w 80
	@echo ""
	@echo "Examples:"
	@echo "  make                     # Build for host target"
	@echo "  make all                 # Build for all installed targets"
	@echo "  make aarch64-apple-darwin # Build for specific target"
	@echo "  make TARGETS=\"target1 target2\" # Build for multiple specific targets"

# Install cross if not already installed
.PHONY: ensure_cross
ensure_cross:
	@command -v cross >/dev/null 2>&1 || { echo "Installing cross..."; cargo install cross; }

# Ensure cross is installed before building
$(TARGETS): | ensure_cross