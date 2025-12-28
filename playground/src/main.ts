// Import WASM module functions
import { eval_expr, LoxResult, run_program } from '../loxwasm-pkg/loxwasm';
import { formatResult, formatError, joinOutputLines } from './utils';

// Import CodeMirror
import { EditorView, keymap, Decoration } from '@codemirror/view';
import { EditorState, StateEffect } from '@codemirror/state';
import { defaultKeymap } from '@codemirror/commands';
import { linter, Diagnostic, lintGutter, setDiagnostics } from '@codemirror/lint';

// Define error decoration type
const errorDecoration = Decoration.mark({
  class: 'cm-error-highlight'
});


// State effects for adding/clearing errors
const addErrorEffect = StateEffect.define<Array<{ from: number, to: number }>>();
const clearErrorsEffect = StateEffect.define();

// Helper function to add error highlights
function highlightError(view: EditorView, from: number, to: number) {
  view.dispatch({
    effects: addErrorEffect.of([{ from, to }].map(range =>
      errorDecoration.range(range.from, range.to)
    ))
  });
}

// Helper function to clear error highlights
function clearErrors(view: EditorView) {
  view.dispatch({
    effects: clearErrorsEffect.of(null)
  });
}

// Create single-line expression editor
const singleLineEditor = new EditorView({
  state: EditorState.create({
    doc: '',
    extensions: [
      keymap.of([
        ...defaultKeymap,
        {
          key: 'Enter',
          run: () => {
            executeSingleLine();
            return true;
          }
        }
      ]),
      linter(null),
      EditorView.lineWrapping
    ]
  }),
  parent: document.getElementById('singleLineEditor')!
});

// Create multi-line code editor
const exampleCode = `fun f(x) {
    print x/2;
}
var a = 1;
while (a < 10) {
    f(a-1);
    a = a + 1;
}`;

const multiLineEditor = new EditorView({
  state: EditorState.create({
    doc: exampleCode,
    extensions: [
      keymap.of(defaultKeymap),
      linter(null),
      lintGutter(),
      EditorView.lineWrapping,
      EditorState.tabSize.of(4)
    ]
  }),
  parent: document.getElementById('multiLineEditor')!
});

// Get DOM elements
const executeButton = document.getElementById('executeButton') as HTMLButtonElement;
const output = document.getElementById('output') as HTMLTextAreaElement;
const executeMultiLineButton = document.getElementById('executeMultiLineButton') as HTMLButtonElement;
const multiLineOutput = document.getElementById('multiLineOutput') as HTMLTextAreaElement;

// Single-line expression execution
function executeSingleLine(): void {
  const code = singleLineEditor.state.doc.toString();
  clearErrors(singleLineEditor);
    singleLineEditor.dispatch(setDiagnostics(singleLineEditor.state, []));

    const result = eval_expr(code) as LoxResult<string>;
    if (result.type === "Success") {
        output.value = formatResult(result.value);
        output.style.backgroundColor = '';
    } else {
        const error = result.value;
        output.value = formatError(error.message);
        output.style.backgroundColor = 'lightcoral';

        highlightError(singleLineEditor, error.span.start, error.span.end);
        const diagnostic: Diagnostic = {
          from: error.span.start,
          to: error.span.end,
          severity: 'error',
          message: error.message
        };
        singleLineEditor.dispatch(setDiagnostics(singleLineEditor.state, [diagnostic]));
    }
}

// Multi-line program execution
function executeMultiLine(): void {
  const code = multiLineEditor.state.doc.toString();
  clearErrors(multiLineEditor);
  multiLineEditor.dispatch(setDiagnostics(multiLineEditor.state, []));

  const result = run_program(code) as LoxResult<string[]>;
  if (result.type === "Success") {
    multiLineOutput.value = joinOutputLines(result.value);
    multiLineOutput.style.backgroundColor = '';
  } else {
    const error = result.value;
    multiLineOutput.value = formatError(error.message);
    multiLineOutput.style.backgroundColor = 'lightcoral';

    const diagnostic: Diagnostic = {
      from: error.span.start,
      to: error.span.end,
      severity: 'error',
      message: error.message
    };
    multiLineEditor.dispatch(setDiagnostics(multiLineEditor.state, [diagnostic]));
  }
}

// Event listeners
executeButton.addEventListener('click', executeSingleLine);
executeMultiLineButton.addEventListener('click', executeMultiLine);

// Focus the single-line editor on load
singleLineEditor.focus();
