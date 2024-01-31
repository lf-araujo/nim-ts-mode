# nim-ts-mode - An Emacs major mode for Nim using tree-sitter

## Testing tree-sitter queries

```elisp

(defun test-nim-parser ()
  (let ((parser (treesit-parser-create 'nim)))
    (treesit-query-capture
     parser
     '(((identifier) @font-lock-type-face
        (:match "\\btrue\\b\\|\\bfalse\\b" @font-lock-type-face))))))

```
