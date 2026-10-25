# Crux mode for Emacs

Install the mode into your user configuration with:

```sh
cargo run -- editor install-emacs
```

The command copies `crux-mode.el` into Emacs's `lisp` directory and adds its
load path and `require` form to the init file Emacs uses. Running it again
updates the mode without adding another init entry.

Alternatively, add this directory to your Emacs load path and load the mode
in your init file:

```elisp
(add-to-list 'load-path "/path/to/crux/tools/emacs")
(require 'crux-mode)
```

Files ending in `.cx` then open in `crux-mode`. The mode highlights keywords,
declarations, names, numbers, strings, and nested block and line comments.

The keyword list in `crux-mode.el` follows `keyword()` in
`rust/src/lexer.rs`. Update both when adding a Crux keyword.

Run the mode tests from the repository root with:

```sh
emacs --batch -Q -L tools/emacs -l crux-mode-tests -f ert-run-tests-batch-and-exit
```
