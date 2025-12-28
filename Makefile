.PHONY: help build-wasm test-ts clean

# Use CARGO_TARGET_DIR environment variable if set, otherwise default to rs-code/target
CARGO_TARGET_DIR ?= rs-code/target
export CARGO_TARGET_DIR

# Default target
help:
	@echo "Lox Interpreter Build and Test System"
	@echo ""
	@echo "Available targets:"
	@echo "  build-wasm    - Build Rust code to WebAssembly"
	@echo "  test-ts       - Run TypeScript tests"
	@echo "  clean         - Clean build artifacts"

# Build WASM from Rust code
build-wasm:
	@echo "Building Rust WASM module..."
	cd rs-code/loxwasm && cargo build --release --target wasm32-unknown-unknown
	@echo "Generating JavaScript bindings..."
	wasm-bindgen $(CARGO_TARGET_DIR)/wasm32-unknown-unknown/release/loxwasm.wasm \
		--out-dir playground/dist \
		--target web
	@echo "WASM build complete!"

# Run TypeScript tests
test-ts: build-wasm
	@echo "Running TypeScript tests..."
	cd playground && npm test

# Clean build artifacts
clean:
	@echo "Cleaning Rust build artifacts..."
	cd rs-code && cargo clean
	@echo "Cleaning Node.js build artifacts..."
	cd playground && rm -rf dist node_modules
	@echo "Clean complete!"
