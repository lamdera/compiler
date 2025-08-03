# Elm Tree Shake

A dead code elimination tool for Elm's JavaScript output when using the `--export-all-functions` flag.

## Overview

When compiling Elm modules with `--export-all-functions` for JavaScript/TypeScript interop, the entire Elm runtime (~28KB) is included even for simple modules. This tool removes unused runtime functions, achieving 50-96% size reduction depending on the module complexity.

## Installation

```bash
cd tools/tree-shake
npm install
```

## Usage

### Using the wrapper script (recommended)

```bash
./elm-compile-tree-shake.sh Module.elm output.js
```

This script:
1. Compiles the Elm module with `--export-all-functions`
2. Pipes the output through the tree shaker
3. Produces minimal JavaScript with only used runtime functions

### Manual pipeline

```bash
# Compile to stdout and pipe through tree shaker
lamdera make Module.elm --export-all-functions --output=- 2>/dev/null | tail -n +2 | node elm-tree-shake.js - output.js

# Or use existing compiled output
node elm-tree-shake.js input.js output.js
```

## How it works

1. Parses the JavaScript AST using Acorn
2. Identifies all function declarations in the Elm runtime
3. Finds entry points (exported functions)
4. Builds a dependency graph of function usage
5. Extracts only transitively used functions
6. Reconstructs minimal output preserving the IIFE structure

## Results

- **Simple modules**: ~96% reduction (27KB → 1KB)
- **Complex modules**: ~58% reduction (44KB → 18KB)

The tool preserves:
- Only runtime functions actually used (F2, F3, List operations, etc.)
- Module exports for CommonJS/ES modules
- TypeScript compatibility