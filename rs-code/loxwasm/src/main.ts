// Import WASM module functions
import init, { eval_expr, run_program } from '../dist/loxwasm.js';

// Initialize WASM module
await init();

// Type definitions for our elements
const codeInput = document.getElementById('code') as HTMLInputElement;
const executeButton = document.getElementById('executeButton') as HTMLButtonElement;
const output = document.getElementById('output') as HTMLTextAreaElement;

const multiLineCode = document.getElementById('multiLineCode') as HTMLTextAreaElement;
const executeMultiLineButton = document.getElementById('executeMultiLineButton') as HTMLButtonElement;
const multiLineOutput = document.getElementById('multiLineOutput') as HTMLTextAreaElement;

// Single-line expression execution
function executeCode(): void {
    const code = codeInput.value;
    try {
        const result = eval_expr(code);
        output.value = "Result: " + result;
    } catch (error) {
        output.value = "Error: " + error;
    }
}

// Multi-line program execution
function executeMultiLineCode(): void {
    const code = multiLineCode.value;
    try {
        const result = run_program(code);
        // Result is an array of strings (print output)
        multiLineOutput.value = result.join('\n');
        multiLineOutput.style.backgroundColor = '';
    } catch (error) {
        multiLineOutput.value = "Error: \n" + error;
        multiLineOutput.style.backgroundColor = 'lightcoral';
    }
}

// Event listeners
executeButton.addEventListener('click', executeCode);
codeInput.addEventListener('keypress', (event: KeyboardEvent) => {
    if (event.key === 'Enter') {
        event.preventDefault();
        executeCode();
    }
});

executeMultiLineButton.addEventListener('click', executeMultiLineCode);
