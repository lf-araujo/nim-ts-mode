# nim-ts-mode - An Emacs major mode for Nim using tree-sitter

## WIP

This is currently a WIP which depends on `nim-mode` and the nim tree-sitter parser (https://github.com/alaviss/tree-sitter-nim) being installed.
To install the Nim language grammar set the following in your init.el and run `treesit-install-language-grammar`:

```elisp
(require 'nim-ts-mode)
(setq treesit-language-source-alist
      '((nim "https://github.com/alaviss/tree-sitter-nim")))

(with-eval-after-load 'tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(nim-ts-mode . nim)))

;; to get LSP-Mode working you also need to set
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(nim-ts-mode . "nim")))
```

## TODO

- [x] Make syntax highlighting work using tree-sitter
- [ ] Create package that auto-installs dependencies and sets up tree-sitter, etc.
- [ ] Make indentation work using tree-sitter
- [ ] See if we can remove dependencies from `nim-mode` (if it makes sense)


Feedback and pull-request are welcome.
