#!/usr/bin/env node

"use strict";
const crux = require('./src/crux.js');
const fs = require('fs');

// TODO: error checking
const sourceFile = process.argv[2];
let sourceContents = fs.readFileSync(sourceFile, 'utf8');
let results = crux.compileCrux(sourceContents);
if (results.result) {
  process.stdout.write(results.result);
} else {
  process.stderr.write(results.error);
  process.exit(1);
}
