/**
 * Utility functions for the Lox playground
 */

/**
 * Formats evaluation results for display
 */
export function formatResult(result: string | number): string {
    return "Result: " + result;
}

/**
 * Formats error messages for display
 */
export function formatError(error: unknown): string {
    return "Error: " + error;
}

/**
 * Joins output lines with newlines
 */
export function joinOutputLines(lines: string[]): string {
    return lines.join('\n');
}

/**
 * Determines if an error should trigger error styling
 */
export function isErrorResult(message: string): boolean {
    return message.startsWith("Error:");
}
