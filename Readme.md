# nim-ts-mode - An Emacs major mode for Nim using tree-sitter


## Installation

Clone this repository into your load-path, so Emacs can find the package.
To make this package work you need `nim-mode` and the corresponding Nim tree-sitter parser library installed. Also if you happen to use an Emacs version without built-in
tree-sitter support you need to install the `tree-sitter` package.

```elisp
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; if you don't have built-in tree-sitter support
;; ---
(unless (require 'tree-sitter nil 'noerror)
  (package-install 'tree-sitter))

(setq treesit-language-source-alist
      '((nim "https://github.com/alaviss/tree-sitter-nim")))

(with-eval-after-load 'tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(nim-ts-mode . nim)))

(require 'tree-sitter)
;; ---

;; if you want to get LSP-Mode working you also need to set the following
;; in addition to the configuration needed for lsp-mode
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(nim-ts-mode . "nim")))

(require 'nim-ts-mode)
```

To install the `nim-tree-sitter` parser run `treesit-install-language-grammar` and select `nim`. You may need to have some build tools like gcc installed for this to work.

## Configuration

You can set `nim-ts-mode-indent-level` to the number of spaces that should be used for indentation. Also tree-sitter provides the ability to set the `treesit-font-lock-level` to a value from 1 to 3,
to controll how much different elements to highlight, with 3 highlighting the most elements.

## TODO

- [x] Make syntax highlighting work using tree-sitter
- [x] Provide simplified indentation mechanism
- [ ] Highlight HTML and Javscript parts in strings using their respective parsers
- [ ] Create package that auto-installs dependencies and sets up tree-sitter, etc.
- ~~[ ] Make indentation work using tree-sitter~~ question if this really is necessary
- [ ] See if we can remove dependencies from `nim-mode` (if it makes sense)


Feedback and pull-request are welcome.
