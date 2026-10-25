;;; crux-mode-tests.el --- Tests for crux-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'crux-mode)

(ert-deftest crux-mode-highlights-basic-tokens ()
  (with-temp-buffer
    (crux-mode)
    (insert "fun greet(name: String) { let count = 12; return name };\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "fun")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-keyword-face))
    (search-forward "greet")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-function-name-face))
    (search-forward "12")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-constant-face))))

(ert-deftest crux-mode-recognizes-nested-comments-and-strings ()
  (with-temp-buffer
    (crux-mode)
    (insert "/* outer /* inner */ still outer */ let x = \"// text\"; // end\nlet y = 1;")
    (goto-char (point-min))
    (search-forward "still outer")
    (should (nth 4 (syntax-ppss (match-beginning 0))))
    (search-forward "let x")
    (should-not (nth 4 (syntax-ppss (match-beginning 0))))
    (search-forward "// text")
    (let ((position (match-beginning 0)))
      (should (nth 3 (syntax-ppss position)))
      (should-not (nth 4 (syntax-ppss position))))
    (search-forward "end")
    (should (nth 4 (syntax-ppss (match-beginning 0))))
    (search-forward "let y")
    (should-not (nth 4 (syntax-ppss (match-beginning 0))))))

(provide 'crux-mode-tests)

;;; crux-mode-tests.el ends here
