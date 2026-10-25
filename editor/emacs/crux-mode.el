;;; crux-mode.el --- Major mode for Crux source files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Crux contributors
;; Author: Crux contributors
;; Keywords: languages

;;; Commentary:

;; Basic editing and syntax highlighting for .cx files.  Keep the keyword
;; lists below in sync with keyword() in rust/src/lexer.rs.

;;; Code:

(defvar crux-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for `crux-mode'.")

(defconst crux-mode--syntax-propertize
  (syntax-propertize-rules
   ("\\(/\\)\\*" (1 ". 124bn"))
   ("\\*\\(/\\)" (1 ". 124bn")))
  "Give block comments nesting syntax without making line comments nest.")

(defconst crux-mode--keywords
  '("as" "pragma" "import" "export" "fun" "let" "data"
    "declare" "exception" "throw" "try" "catch" "jsffi"
    "type" "match" "if" "then" "else" "while" "for"
    "in" "do" "return" "mutable" "trait" "impl")
  "Crux keywords, matching `keyword' in rust/src/lexer.rs.")

(defconst crux-mode--font-lock-keywords
  `((,(regexp-opt crux-mode--keywords 'symbols) . font-lock-keyword-face)
    ("\\_<fun\\_>[[:space:]]+\\([[:word:]_]+\\)"
     1 font-lock-function-name-face)
    ("\\_<\\(?:data\\|type\\|trait\\|exception\\)\\_>[[:space:]]+\\([[:word:]_]+\\)"
     1 font-lock-type-face)
    ("\\_<\\(?:True\\|False\\)\\_>" . font-lock-constant-face)
    ("\\_<[[:upper:]][[:word:]_]*\\_>" . font-lock-type-face)
    ("\\_<[0-9]+\\_>" . font-lock-constant-face))
  "Font Lock rules for `crux-mode'.")

;;;###autoload
(define-derived-mode crux-mode prog-mode "Crux"
  "Major mode for editing Crux source files."
  :syntax-table crux-mode-syntax-table
  (setq-local syntax-propertize-function crux-mode--syntax-propertize)
  (setq-local font-lock-defaults '(crux-mode--font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cx\\'" . crux-mode))

(provide 'crux-mode)

;;; crux-mode.el ends here
