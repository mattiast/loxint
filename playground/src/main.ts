// Import WASM module functions
import init, { eval_expr, run_program } from '../dist/loxwasm.js';
import { formatResult, formatError, joinOutputLines } from './utils';

// Import CodeMirror
import { EditorView, keymap, Decoration, DecorationSet } from '@codemirror/view';
import { EditorState, StateEffect, StateField } from '@codemirror/state';
import { defaultKeymap } from '@codemirror/commands';

// Initialize WASM module
await init();

// Define error decoration type
const errorDecoration = Decoration.mark({
  class: 'cm-error-highlight'
});

// State field to manage error decorations
const errorField = StateField.define<DecorationSet>({
  create() {
    return Decoration.none;
  },
  update(decorations, transaction) {
    decorations = decorations.map(transaction.changes);
    for (let effect of transaction.effects) {
      if (effect.is(addErrorEffect)) {
        decorations = decorations.update({
          add: effect.value.map(range => errorDecoration.range(range.from, range.to))
        });
      } else if (effect.is(clearErrorsEffect)) {
        decorations = Decoration.none;
      }
    }
    return decorations;
  },
  provide: f => EditorView.decorations.from(f)
});

// State effects for adding/clearing errors
const addErrorEffect = StateEffect.define<Array<{ from: number, to: number }>>();
const clearErrorsEffect = StateEffect.define();

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
      errorField,
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
      errorField,
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

  try {
    const result = eval_expr(code);
    output.value = formatResult(result);
    output.style.backgroundColor = '';
  } catch (error) {
    output.value = formatError(error);
    output.style.backgroundColor = 'lightcoral';

    // TODO: Parse error to extract position and highlight
    // For now, we'll just show the error text
    console.log('Error details:', error);
  }
}

// Multi-line program execution
function executeMultiLine(): void {
  const code = multiLineEditor.state.doc.toString();
  clearErrors(multiLineEditor);

  try {
    const result = run_program(code);
    multiLineOutput.value = joinOutputLines(result);
    multiLineOutput.style.backgroundColor = '';
  } catch (error) {
    multiLineOutput.value = formatError(error);
    multiLineOutput.style.backgroundColor = 'lightcoral';

    // TODO: Parse error to extract position and highlight
    // For now, we'll just show the error text
    console.log('Error details:', error);
  }
}

// Event listeners
executeButton.addEventListener('click', executeSingleLine);
executeMultiLineButton.addEventListener('click', executeMultiLine);

// Focus the single-line editor on load
singleLineEditor.focus();
