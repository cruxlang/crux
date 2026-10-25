# Crux mode for Emacs

Add this directory to your Emacs load path and load the mode in your init file:

```elisp
(add-to-list 'load-path "/path/to/crux/tools/emacs")
(require 'crux-mode)
```

Files ending in `.cx` then open in `crux-mode`. The mode highlights keywords,
declarations, names, numbers, strings, and nested block and line comments.

The keyword list in `crux-mode.el` follows `keyword()` in
`rust/src/lexer.rs`. Update both when adding a Crux keyword. No generated
table or installation command is needed for this small mode; the Rust compiler
currently has no native CLI, and Emacs configuration paths vary by user.

Run the mode tests from the repository root with:

```sh
emacs --batch -Q -L tools/emacs -l crux-mode-tests -f ert-run-tests-batch-and-exit
```
