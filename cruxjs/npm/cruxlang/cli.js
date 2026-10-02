#!/usr/bin/env node

"use strict";
const crux = require('./src/cruxjs.js');
const fs = require('fs');

// TODO: error checking
const sourceFile = process.argv[2];
let sourceContents = fs.readFileSync(sourceFile, 'utf8');
let results = crux.compileCrux(sourceContents);
if (results.result !== undefined) {
  process.stdout.write(results.result);
  process.stdout.write("\n");
} else {
  process.stderr.write(results.error);
  process.stderr.write("\n");
  process.exitCode = 1;
}
