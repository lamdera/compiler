#!/usr/bin/env node

const acorn = require('acorn');
const walk = require('acorn-walk');
const escodegen = require('escodegen');
const fs = require('fs');

class ElmTreeShaker {
  constructor(code) {
    this.code = code;
    this.ast = acorn.parse(code, { ecmaVersion: 2020 });
    this.usedIdentifiers = new Set();
    this.functionDeclarations = new Map();
    this.variableDeclarations = new Map();
    this.dependencies = new Map();
  }

  analyze() {
    // Step 1: Extract all function and variable declarations
    this.extractDeclarations();
    
    // Step 2: Find entry points (exports and _Platform_export)
    this.findEntryPoints();
    
    // Step 3: Build dependency graph
    this.buildDependencyGraph();
    
    // Step 4: Collect all transitively used identifiers
    this.collectTransitiveDependencies();
    
    // Step 5: Generate minimal code
    return this.generateMinimalCode();
  }

  extractDeclarations() {
    walk.simple(this.ast, {
      FunctionDeclaration: (node) => {
        if (node.id && node.id.name) {
          this.functionDeclarations.set(node.id.name, node);
          this.dependencies.set(node.id.name, new Set());
        }
      },
      VariableDeclarator: (node) => {
        if (node.id && node.id.name && node.init) {
          this.variableDeclarations.set(node.id.name, node);
          this.dependencies.set(node.id.name, new Set());
        }
      },
      AssignmentExpression: (node) => {
        if (node.left.type === 'Identifier') {
          this.variableDeclarations.set(node.left.name, node);
          this.dependencies.set(node.left.name, new Set());
        }
      }
    });
  }

  findEntryPoints() {
    walk.simple(this.ast, {
      CallExpression: (node) => {
        // Look for _Platform_export calls
        if (node.callee && node.callee.name === '_Platform_export') {
          console.log('Found _Platform_export call');
          this.analyzeExports(node);
        }
      },
      MemberExpression: (node) => {
        // Look for scope['Elm'] assignments
        if (node.object && node.object.name === 'scope' && 
            node.property && node.property.value === 'Elm') {
          console.log('Found scope.Elm assignment');
          this.usedIdentifiers.add('_Platform_export');
        }
      },
      AssignmentExpression: (node) => {
        // Look for scope['Elm'] = ... patterns
        if (node.left && node.left.type === 'MemberExpression' &&
            node.left.object && node.left.object.name === 'scope') {
          console.log('Found scope assignment');
          // Add all exports
          walk.simple(node.right, {
            Identifier: (idNode) => {
              this.usedIdentifiers.add(idNode.name);
            }
          });
        }
      }
    });
    console.log('Entry points found:', Array.from(this.usedIdentifiers));
  }

  analyzeExports(exportCall) {
    if (exportCall.arguments.length > 0) {
      walk.simple(exportCall.arguments[0], {
        Identifier: (node) => {
          this.usedIdentifiers.add(node.name);
        },
        MemberExpression: (node) => {
          if (node.object.type === 'Identifier') {
            this.usedIdentifiers.add(node.object.name);
          }
        }
      });
    }
  }

  buildDependencyGraph() {
    // For each declaration, find what it depends on
    const allDeclarations = [
      ...this.functionDeclarations.entries(),
      ...this.variableDeclarations.entries()
    ];
    
    for (const [name, node] of allDeclarations) {
      const deps = this.dependencies.get(name) || new Set();
      
      // Walk the node to find all identifier references
      const nodeToWalk = node.init || node.right || node;
      if (nodeToWalk) {
        walk.simple(nodeToWalk, {
          Identifier: (idNode) => {
            // Skip the declaration name itself
            if (idNode.name === name) return;
            
            // This is a dependency
            if (this.functionDeclarations.has(idNode.name) || this.variableDeclarations.has(idNode.name)) {
              deps.add(idNode.name);
            }
          }
        });
      }
      
      this.dependencies.set(name, deps);
    }
  }

  collectTransitiveDependencies() {
    const visited = new Set();
    const toVisit = [...this.usedIdentifiers];
    
    while (toVisit.length > 0) {
      const current = toVisit.pop();
      if (visited.has(current)) continue;
      
      visited.add(current);
      
      const deps = this.dependencies.get(current);
      if (deps) {
        for (const dep of deps) {
          if (!visited.has(dep)) {
            toVisit.push(dep);
          }
        }
      }
    }
    
    this.usedIdentifiers = visited;
  }

  generateMinimalCode() {
    // Find the IIFE wrapper
    let iifeNode = null;
    walk.simple(this.ast, {
      CallExpression: (node) => {
        if (node.callee.type === 'FunctionExpression' && 
            node.arguments.length === 1 && 
            node.arguments[0].type === 'ThisExpression') {
          iifeNode = node;
        }
      }
    });
    
    if (!iifeNode) {
      throw new Error('Could not find IIFE wrapper');
    }
    
    // Filter the body of the IIFE to include only used declarations
    const functionBody = iifeNode.callee.body;
    const filteredStatements = [];
    
    for (const statement of functionBody.body) {
      if (this.shouldIncludeStatement(statement)) {
        filteredStatements.push(statement);
      }
    }
    
    // Create new minimal IIFE
    const minimalIife = {
      type: 'CallExpression',
      callee: {
        type: 'FunctionExpression',
        params: iifeNode.callee.params,
        body: {
          type: 'BlockStatement',
          body: filteredStatements
        }
      },
      arguments: iifeNode.arguments
    };
    
    // Generate code
    return escodegen.generate({
      type: 'Program',
      body: [{
        type: 'ExpressionStatement',
        expression: minimalIife
      }]
    }, {
      format: {
        indent: {
          style: '  '
        }
      }
    });
  }

  shouldIncludeStatement(statement) {
    // Always include 'use strict'
    if (statement.type === 'ExpressionStatement' && 
        statement.expression.type === 'Literal' && 
        statement.expression.value === 'use strict') {
      return true;
    }
    
    // Check function declarations
    if (statement.type === 'FunctionDeclaration' && statement.id) {
      return this.usedIdentifiers.has(statement.id.name);
    }
    
    // Check variable declarations
    if (statement.type === 'VariableDeclaration') {
      return statement.declarations.some(decl => 
        decl.id && decl.id.name && this.usedIdentifiers.has(decl.id.name)
      );
    }
    
    // Check expression statements (assignments, calls)
    if (statement.type === 'ExpressionStatement') {
      // Platform export calls
      if (statement.expression.type === 'CallExpression' && 
          statement.expression.callee.name === '_Platform_export') {
        return true;
      }
      
      // Assignments
      if (statement.expression.type === 'AssignmentExpression' && 
          statement.expression.left.type === 'Identifier') {
        return this.usedIdentifiers.has(statement.expression.left.name);
      }
      
      // scope['Elm'] assignments
      if (statement.expression.type === 'AssignmentExpression' && 
          statement.expression.left.type === 'MemberExpression' &&
          statement.expression.left.object.name === 'scope') {
        return true;
      }
    }
    
    // Check if statements (for module.exports check)
    if (statement.type === 'IfStatement') {
      return true; // Keep all if statements for now (module.exports logic)
    }
    
    return false;
  }
}

// CLI usage
if (require.main === module) {
  const args = process.argv.slice(2);
  if (args.length !== 2) {
    console.error('Usage: elm-tree-shake.js <input.js> <output.js>');
    process.exit(1);
  }
  
  const [inputFile, outputFile] = args;
  
  try {
    let code;
    if (inputFile === '-') {
      // Read from stdin
      code = fs.readFileSync(0, 'utf8');
    } else {
      code = fs.readFileSync(inputFile, 'utf8');
    }
    const shaker = new ElmTreeShaker(code);
    const minimalCode = shaker.analyze();
    
    fs.writeFileSync(outputFile, minimalCode);
    
    const originalSize = Buffer.byteLength(code);
    const minimalSize = Buffer.byteLength(minimalCode);
    const reduction = ((originalSize - minimalSize) / originalSize * 100).toFixed(1);
    
    console.log(`Original: ${(originalSize / 1024).toFixed(1)}KB`);
    console.log(`Minimal: ${(minimalSize / 1024).toFixed(1)}KB`);
    console.log(`Reduction: ${reduction}%`);
    
    // Report what was kept
    console.log(`\nKept ${shaker.usedIdentifiers.size} identifiers:`);
    const kept = Array.from(shaker.usedIdentifiers).sort();
    console.log(kept.filter(id => id.startsWith('F') || id.startsWith('A') || id.startsWith('_')).join(', '));
    
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

module.exports = ElmTreeShaker;