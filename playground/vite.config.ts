import { defineConfig } from 'vite';

export default defineConfig({
  base: './',
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
