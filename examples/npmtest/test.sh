#!/bin/bash
set -e
cd "$(dirname "$0")"

crux build
node node-test.js
