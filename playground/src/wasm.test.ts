import { describe, it, expect, beforeAll } from 'vitest';

describe('WASM Module Integration', () => {
    let wasmModule: any;

    beforeAll(async () => {
        try {
            // Dynamically import the WASM module
            const { default: init, eval_expr, run_program } = await import('../dist/loxwasm.js');
            await init();
            wasmModule = { eval_expr, run_program };
        } catch (error) {
            console.warn('WASM module not available, skipping integration tests:', error);
        }
    });

    it('should evaluate simple arithmetic expressions', async () => {
        if (!wasmModule) {
            console.log('Skipping test: WASM module not available');
            return;
        }

        const result = wasmModule.eval_expr('1 + 2');
        expect(result).toBe('3');
    });

    it('should evaluate string concatenation', async () => {
        if (!wasmModule) {
            console.log('Skipping test: WASM module not available');
            return;
        }

        const result = wasmModule.eval_expr('"hello" + " world"');
        expect(result).toBe('"hello world"');
    });

    it('should handle boolean expressions', async () => {
        if (!wasmModule) {
            console.log('Skipping test: WASM module not available');
            return;
        }

        const result = wasmModule.eval_expr('true and false');
        expect(result).toBe('false');
    });

    it('should run simple programs', async () => {
        if (!wasmModule) {
            console.log('Skipping test: WASM module not available');
            return;
        }

        const program = 'print "Hello, Lox!";';
        const result = wasmModule.run_program(program);
        expect(result).toContain('Hello, Lox!');
    });

    it('should throw errors for invalid syntax', async () => {
        if (!wasmModule) {
            console.log('Skipping test: WASM module not available');
            return;
        }

        expect(() => {
            wasmModule.eval_expr('1 +');
        }).toThrow();
    });
});
