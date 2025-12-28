import { defineConfig } from 'vite';
import wasm from 'vite-plugin-wasm';

export default defineConfig({
  base: './',
  plugins: [wasm()],
  build: {
    target: 'es2022', // Support top-level await
    outDir: 'dist',
    emptyOutDir: false, // Don't clear dist/ since WASM files are already there
    rollupOptions: {
      input: {
        main: './index.html'
      }
    }
  },
  server: {
    fs: {
      // Allow serving files from the WASM output directory
      allow: ['..']
    }
  }
});
