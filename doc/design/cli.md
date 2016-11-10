# Separate "run script" vs. Build CLIs

Node has webpack, rustc has cargo, Python has pip etc.

python --version
python --help
python foo.py

node --version
node --help
node foo.js

rustc ...

# All-in-one

go version
go help
go build
go install

# Crux

The hot flow by commands run will be "crux test" or "crux build" or "crux test -w" or "crux build -w".

But when people think about running the command it will more often than not be to run a particular script.  We should probably support "crux foo.cx" alongside "crux run <scriptname>".


