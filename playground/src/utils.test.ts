import { describe, it, expect } from 'vitest';
import { formatResult, formatError, joinOutputLines, isErrorResult } from './utils';

describe('formatResult', () => {
    it('should format a numeric result', () => {
        expect(formatResult(42)).toBe('Result: 42');
    });

    it('should format a string result', () => {
        expect(formatResult('"hello"')).toBe('Result: "hello"');
    });

    it('should handle zero', () => {
        expect(formatResult(0)).toBe('Result: 0');
    });
});

describe('formatError', () => {
    it('should format error messages', () => {
        expect(formatError('Syntax error')).toBe('Error: Syntax error');
    });

    it('should handle Error objects', () => {
        const error = new Error('Test error');
        expect(formatError(error)).toContain('Error:');
    });

    it('should handle unknown error types', () => {
        expect(formatError(null)).toBe('Error: null');
    });
});

describe('joinOutputLines', () => {
    it('should join multiple lines with newlines', () => {
        const lines = ['line1', 'line2', 'line3'];
        expect(joinOutputLines(lines)).toBe('line1\nline2\nline3');
    });

    it('should handle empty array', () => {
        expect(joinOutputLines([])).toBe('');
    });

    it('should handle single line', () => {
        expect(joinOutputLines(['single'])).toBe('single');
    });
});

describe('isErrorResult', () => {
    it('should return true for error messages', () => {
        expect(isErrorResult('Error: Something went wrong')).toBe(true);
    });

    it('should return false for success messages', () => {
        expect(isErrorResult('Result: 42')).toBe(false);
    });

    it('should return false for empty string', () => {
        expect(isErrorResult('')).toBe(false);
    });
});
