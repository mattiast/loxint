import { describe, it, expect } from 'vitest';
import { eval_expr, LoxResult, run_program } from '../loxwasm-pkg/loxwasm';

describe('WASM Module Integration', () => {
    it('should evaluate simple arithmetic expressions', async () => {
        const result = eval_expr('1 + 2') as LoxResult<number>;
        expect(result).toStrictEqual({ type: 'Success', value: 3 });
    });

    it('should run simple programs', async () => {
        const program = 'print "Hello, Lox!";';
        const result = run_program(program) as LoxResult<string[]>;
        expect(result.value).toContain('Atomic(String("Hello, Lox!"))');
    });

    it('should throw errors for invalid syntax', async () => {
        const result = eval_expr('1 +');
        expect(result.type).toBe('Error');
    });
});
