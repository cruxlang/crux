# NPM Integration

npm is the dominant package ecosystem for JavaScript code.  Crux needs to integrate smoothly with it.

## Publishing npm packages

For parity with npm, crux will support multiple executables, but only one library per package.

Crux programs have an automatic, implicit main() function call added to the bottom of the top-level module.

Crux libraries don't.  If the outermost module defines any `export jsffi` exports, they will be available from the generated npm module.

## Importing npm packages

[TODO]
