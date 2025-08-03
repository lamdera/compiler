#!/usr/bin/env node

// Simple wrapper to collect all stdin before processing
const fs = require('fs');

let chunks = [];
process.stdin.on('data', chunk => chunks.push(chunk));
process.stdin.on('end', () => {
  let input = Buffer.concat(chunks).toString();
  
  // Skip "Success!" line if present
  if (input.trim().startsWith('Success!')) {
    const lines = input.split('\n');
    lines.shift(); // Remove first line
    input = lines.join('\n');
  }
  
  const ElmTreeShaker = require('./elm-tree-shake.js');
  
  try {
    const shaker = new ElmTreeShaker(input);
    const minimalCode = shaker.analyze();
    
    if (process.argv[2]) {
      fs.writeFileSync(process.argv[2], minimalCode);
      
      const originalSize = Buffer.byteLength(input);
      const minimalSize = Buffer.byteLength(minimalCode);
      const reduction = ((originalSize - minimalSize) / originalSize * 100).toFixed(1);
      
      console.error(`Original: ${(originalSize / 1024).toFixed(1)}KB`);
      console.error(`Minimal: ${(minimalSize / 1024).toFixed(1)}KB`);
      console.error(`Reduction: ${reduction}%`);
    } else {
      process.stdout.write(minimalCode);
    }
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
});