.PHONY: help build-wasm test-ts clean download-tests

MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MKFILE_DIR := $(dir $(MKFILE_PATH))

# Default target
help:
	@echo "Lox Interpreter Build and Test System"
	@echo ""
	@echo "Available targets:"
	@echo "  build-wasm    - Build Rust code to WebAssembly"
	@echo "  test-ts       - Run TypeScript tests"
	@echo "  download-tests- Download test cases from test-urls.txt"
	@echo "  clean         - Clean build artifacts"

# Build WASM from Rust code
build-wasm:
	@echo "Building Rust WASM module and generating JavaScript bindings..."
	# Use absolute path for the output directory
	# to ensure wasm-pack works correctly
	wasm-pack build ${MKFILE_DIR}/rs-code/loxwasm \
		--release \
		--out-dir ${MKFILE_DIR}/playground/loxwasm-pkg \
		--target bundler
	@echo "WASM build complete!"

# Run TypeScript tests
test-ts: build-wasm
	@echo "Running TypeScript tests..."
	cd playground && npm test

preview: build-wasm
	@echo "Starting development server for preview..."
	cd playground && npm run build && npm run preview -- --open

# Download test cases from URLs
download-tests:
	@echo "Downloading test cases..."
	@mkdir -p test-cases
	@if [ ! -f test-urls.txt ]; then \
		echo "Error: test-urls.txt not found"; \
		echo "Create test-urls.txt with one URL per line"; \
		exit 1; \
	fi
	@while IFS= read -r url || [ -n "$$url" ]; do \
		case "$$url" in \
			""|"#"*) ;; \
			*) \
				filename=$$(basename "$$url"); \
				echo "Downloading $$filename from $$url"; \
				curl -sS -L "$$url" -o "test-cases/$$filename" || echo "Failed to download $$url"; \
				;; \
		esac; \
	done < test-urls.txt
	@echo "Download complete! Files saved to test-cases/"

# Clean build artifacts
clean:
	@echo "Cleaning Rust build artifacts..."
	cd rs-code && cargo clean
	@echo "Cleaning Node.js build artifacts..."
	cd playground && rm -rf dist node_modules loxwasm-pkg
	@echo "Clean complete!"
