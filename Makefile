.PHONY: help build-wasm test-ts test-all watch clean install

# Default target
help:
	@echo "Lox Interpreter Build and Test System"
	@echo ""
	@echo "Available targets:"
	@echo "  build-wasm    - Build Rust code to WebAssembly"
	@echo "  test-ts       - Run TypeScript tests"
	@echo "  test-all      - Run Rust tests and TypeScript tests"
	@echo "  watch         - Watch Rust files and run tests on changes"
	@echo "  install       - Install dependencies (Rust & Node.js)"
	@echo "  clean         - Clean build artifacts"

# Install all dependencies
install:
	@echo "Installing Rust dependencies..."
	cd rs-code && cargo fetch
	@echo "Installing Node.js dependencies..."
	cd playground && npm install
	@echo "Installing cargo-watch (for watch mode)..."
	cargo install cargo-watch || echo "cargo-watch already installed"

# Build WASM from Rust code
build-wasm:
	@echo "Building Rust WASM module..."
	cd rs-code/loxwasm && cargo build --release --target wasm32-unknown-unknown
	@echo "Generating JavaScript bindings..."
	wasm-bindgen rs-code/target/wasm32-unknown-unknown/release/loxwasm.wasm \
		--out-dir playground/dist \
		--target web
	@echo "WASM build complete!"

# Run TypeScript tests
test-ts: build-wasm
	@echo "Running TypeScript tests..."
	cd playground && npm test

# Run all tests (Rust + TypeScript)
test-all:
	@echo "Running Rust tests..."
	cd rs-code && cargo test --workspace
	@echo "Building WASM and running TypeScript tests..."
	$(MAKE) test-ts

# Watch Rust files and rebuild + test on changes
watch:
	@echo "Watching Rust files for changes..."
	@echo "Will rebuild WASM and run TypeScript tests on each change"
	@echo "Press Ctrl+C to stop"
	cd rs-code && cargo watch \
		-w loxlang/src \
		-w loxwasm/src \
		-w loxint/src \
		-s 'cd .. && make build-wasm && cd playground && npm test'

# Clean build artifacts
clean:
	@echo "Cleaning Rust build artifacts..."
	cd rs-code && cargo clean
	@echo "Cleaning Node.js build artifacts..."
	cd playground && rm -rf dist node_modules
	@echo "Clean complete!"
