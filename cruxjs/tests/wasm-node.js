"use strict";

const assert = require("assert");
const path = require("path");
const vm = require("vm");

const binding = require(path.resolve(process.argv[2]));
const source = `import option(...)
fun main() {
  match Some(1) {
    Some(n) => print(n)
  }
}`;

const success = binding.compileCrux(source);
assert.deepStrictEqual(Object.keys(success), ["result"]);
const output = [];
vm.runInNewContext(success.result, {
  console: { log: value => output.push(String(value)) },
});
assert.deepStrictEqual(output, ["1"]);

const failure = binding.compileCrux("fun main() { missing }");
assert.deepStrictEqual(Object.keys(failure), ["error"]);
assert.match(failure.error, /unbound value missing/);

const direct = binding.compile('fun main() { print("direct") }');
assert.match(direct, /direct/);

process.stdout.write("cruxjs Wasm/Node smoke test passed\n");
