# Building nim-ts-mode


## Getting nim tree-sitter grammar to work with tree-sitter.el Emacs MELPA package

Because Emacs with built-in tree-sitter support does unfortunately not include treesit-query-builder,
we need to use an Emacs version without tree-sitter built-in, activate the tree-sitter layer in Spacemacs. Then we need to manually compile the Nim tree-sitter parser library by doing the following steps:

1. make sure that you did run `npm i -g tree-sitter-cli` to install tree-sitter-cli
2. run `tree-sitter-cli generate --abi 13` inside the tree-sitter-nim repository
3. run `cc -O2 -shared -Isrc -fPIC -o nim.so src/parser.c src/scanner.c` to create the nim.so tree-sitter parser library
4. move nim.so to /home/p0p3/.emacs.d/elpa/30.0/develop/tree-sitter-langs-20240107.149/bin/
5. call `tree-sitter-query-builder` inside Emacs after activating nim-ts-mode

