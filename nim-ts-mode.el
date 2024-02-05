(require 'nim-mode)
(require 'treesit)

(defvar nim-ts-font-lock-rules
  '(;; SPDX-FileCopyrightText: 2023 Leorize <leorize+oss@disroot.org>
    ;; SPDX-License-Identifier: MPL-2.0

    ;; Punctuations
    :feature delimiter
    :language nim
    :override t
    (([ "." ";" "," ":" ] @punctuation.delimiter-face)
     ([ "(" ")" "[" "]" "{" "}" "{." ".}" ] @punctuation.bracket-face))

    ;; Special
    :feature special
    :language nim
    :override t
    ((blank_identifier) @variable.builtin-face)

    ;; Calls
    :feature call
    :language nim
    :override t
    ((call
      function: [
                 (identifier) @function.call-face
                 (dot_expression
                  right: (identifier) @function.call-face)
                 ])
     (generalized_string
      function: [
                 (identifier) @function.call-face
                 (dot_expression
                  right: (identifier) @function.call-face)
                 ]))

    ;; Declarations
    :feature declaration
    :language nim
    :override t
    (
     (type_symbol_declaration
      name: [
             (identifier) @type.declaration-face
             (exported_symbol) @type.export-face
             ])
     (exported_symbol "*" @type.qualifier-face)
     (_ "=" @punctuation.delimiter-face [body: (_) value: (_)])
     (proc_declaration name: (_) @function-face)
     (func_declaration name: (_) @function-face)
     (converter_declaration name: (_) @function-face)
     (method_declaration name: (_) @method-face)
     (template_declaration name: (_) @function.macro-face)
     (macro_declaration name: (_) @function.macro-face)
     (parameter_declaration
      (symbol_declaration_list
       (symbol_declaration name: (_) @parameter-face)))
     (symbol_declaration name: (_) @variable-face)
     (_
      [
       type: [
              (type_expression (identifier))
              (type_expression (accent_quoted (identifier)))
              ] @type-face
       ;; TODO investigate if there can really be a return_type: node, because I haven't seen one up tu this point
       return_type: [
                     (type_expression (identifier))
                     (type_expression (accent_quoted (identifier)))
                     ] @type-face
       ])
     ;; highlight generic types
     (type_expression (bracket_expression left: (identifier) @type-face
                                          right: (argument_list (identifier) @type-face)))
     )

    ;; Exceptions
    :feature exception
    :language nim
    :override t
    (([
       "try"
       "except"
       "finally"
       "raise"
       ] @exception-face)

     (except_branch values: (expression_list
                             [
                              (identifier) @type-face
                              (infix_expression
                               left: (identifier) @type-face
                               operator: "as"
                               right: (identifier) @variable-face)
                              ])))

    ;; Expressions
    :feature expression
    :language nim
    :override t
    ((dot_expression
      right: (identifier) @field-face))

    ;; Literal/comments
    :feature literal_comment
    :language nim
    :override t
    (([
       (comment)
       (block_comment)
       ] @comment-face)

     ([
       (documentation_comment)
       (block_documentation_comment)
       ] @comment.documentation-face)

     ((interpreted_string_literal) @string-face)
     ((long_string_literal) @string-face)
     ((raw_string_literal) @string-face)
     ((generalized_string) @string-face)
     ((char_literal) @character-face)
     ((escape_sequence) @string.escape-face)
     ((integer_literal) @number-face)
     ((float_literal) @float-face)
     ((custom_numeric_literal) @number-face)
     ((nil_literal) @constant.builtin-face)

     ;; string interpolation needs to added to the parser
     ;; ((string) @python-face--treesit-fontify-string
     ;;  (interpolation ["{" "}"] @font-face-lock-misc-punctuation-face))
     )

    ;; Keyword
    :feature keyword
    :language nim
    :override t
    (([
      "if"
      "when"
      "case"
      "elif"
      "else"
      ] @conditional-face)

     (of_branch "of" @conditional-face)
     ([
       "import"
       "include"
       "export"
       ] @include-face)

     (import_from_statement "from" @include-face)
     (except_clause "except" @include-face)

     ([
      "for"
      "while"
      "continue"
      "break"
      ] @repeat-face)

     (for "in" @repeat-face)
     ([
       "macro"
       "template"
       "const"
       "let"
       "var"
       "asm"
       "bind"
       "block"
       "concept"
       "defer"
       "discard"
       "distinct"
       "do"
       "enum"
       "mixin"
       "nil"
       "object"
       "out"
       "ptr"
       "ref"
       "static"
       "tuple"
       "type"
       ] @keyword-face)

     ([
       "proc"
       "func"
       "method"
       "converter"
       "iterator"
       ] @keyword.function-face)

     ([
       "and"
       "or"
       "xor"
       "not"
       "div"
       "mod"
       "shl"
       "shr"
       "from"
       "as"
       "of"
       "in"
       "notin"
       "is"
       "isnot"
       "cast"
       ] @keyword.operator-face)

     ;; true and false are missing as builtin constants and must be added in the parser lib
     ((identifier) @constant.builtin-face
      (:match "\\btrue\\b\\|\\bfalse\\b" @constant.builtin-face))

     ([
       "return"
       "yield"
       ] @keyword.return-face)
    )

    ;; Operators
    :feature operator
    :language nim
    :override t
    ((infix_expression operator: _ @operator-face)
     (prefix_expression operator: _ @operator-face)
     [
      "="
      ] @operator-face)
    )
  )


(defvar nim-ts-mode--font-remap-alist
  '((punctuation.delimiter-face ('font-lock-delimiter-face) "A face for delimiters")
    (punctuation.bracket-face ('font-lock-bracket-face) "A face for brackets")
    (variable.builtin-face ('font-lock-builtin-face) "A face for builtin variables")
    (function.call-face ('font-lock-function-call-face) "A face for function calls")
    (type.declaration-face ('font-lock-type-face) "A face for type declarations")
    (type.export-face ('font-lock-type-face) "A face for type export")
    (type.qualifier-face ('font-lock-type-face :weight bold) "A face for type qualification")
    (function-face ('font-lock-function-name-face) "A face for functions")
    (method-face ('font-lock-function-name-face) "A face for methods")
    (function.macro-face ('font-lock-function-name-face) "A face for macros")
    (parameter-face ('font-lock-variable-use-face) "A face for parameters")
    (variable-face ('font-lock-variable-name-face) "A face for variables")
    (type-face ('font-lock-type-face) "A face for types")
    (exception-face ('font-lock-warning-face) "A face for exceptions")
    (field-face ('font-lock-property-use-face) "A face for fields")
    (comment-face ('font-lock-comment-face) "A face for comments")
    (comment.documentation-face ('font-lock-doc-face) "A face for documentation")
    (string-face ('font-lock-string-face) "A face for strings")
    (character-face ('font-lock-string-face) "A face for characters")
    (string.escape-face ('font-lock-escape-face) "A face for escaped strings")
    (number-face ('font-lock-number-face) "A face for numbers")
    (float-face ('font-lock-number-face) "A face for floats")
    (constant.builtin-face ('font-lock-constant-face) "A face for constants")
    (conditional-face ('font-lock-keyword-face) "A face for conditionals")
    (include-face ('font-lock-keyword-face) "A face for include statements")
    (repeat-face ('font-lock-keyword-face) "A face for loops")
    (keyword-face ('font-lock-keyword-face) "A face for keywords")
    (keyword.function-face ('font-lock-keyword-face) "A face for function keywords")
    (keyword.operator-face ('font-lock-keyword-face) "A face for operator keywords")
    (keyword.return-face ('font-lock-keyword-face) "A face for return keywords")
    (operator-face ('font-lock-operator-face) "A face for operators"))

  "A list of new-font to existing-font mappings that are used by the `nim-ts-mode--remap-fonts' macro.
Mappings should be in the format (new-font-face 'old-font-face \"description\") inside a list."
  )


(defvar nim-ts-mode--font-base-theme 'doom-one)


(defmacro nim-ts-mode--remap-fonts ()
  "Creates new font-faces using defface, inheriting from the given old font-faces and using
the provided docstring and the theme specified in `nim-ts-mode--font-base-theme'.

The mapping of new-font to old-font can be adjusted by modifying the
`nim-ts-mode--font-remap-alist' variable.
Font-specs should be in the format '(new-font-face 'old-font-face \"description\").
Mappings should be in the format (new-font-face 'old-font-face \"description\") inside the list."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (let ((new (car spec))
                (old (cadr spec))
                (doc (caddr spec)))
            `(nim-ts-mode--remap-font ,new ,old ,doc ',nim-ts-mode--font-base-theme)
            ))
        nim-ts-mode--font-remap-alist
        )))


(defmacro nim-ts-mode--remap-font (new old doc theme)
  "Creates a new font-face using defface and customizes it to match the given theme.

NEW:   - the name to use as a symbol for the new font-face
OLD:   - a list containing the symbol of the font-face to use as a base and
         additional attributes as key-value pairs e.g. ('font-lock-type-face :weight bold)
DOC:   - a docstring that describes the font-face
THEME: - the symbol of the color theme to use to inherit from"
  `(progn
     (defface ,new '((t (:inherit ,@old)))
       ,doc)
     (custom-theme-set-faces ,theme
                             '(,new ((t (:inherit ,old)))))))


(defvar nim-ts-mode-indent-level 2)


;; WIP
(defvar nim-ts-indent-rules
  (let ((offset nim-ts-mode-indent-level))
    `((nim
       (no-node column-0 0)
       (catch-all prev-line 0)
       )
      )
    )
  )


(defun nim-ts-mode-indent-line-simple ()
  "Indent the current Nim code line as simple as it gets."
  (interactive)
  (if (eq this-command 'indent-for-tab-command)
      (indent-line-to (+ (current-indentation) nim-ts-mode-indent-level))
    (let ((c-indent
           (save-excursion
             (forward-line -1)
             (beginning-of-line)
             (current-indentation))))
        (indent-line-to c-indent))))


;;;###autoload
(define-derived-mode nim-ts-mode nim-mode "Nim[ts]"
  "Major-mode for editing Nim files with tree-sitter"
  :syntax-table nim-mode-syntax-table

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'nim)
    (treesit-parser-create 'nim)
    (nim-ts-setup)))


(defun nim-ts-setup ()
  "Setup tree-sitter for nim-ts-mode."

  ;; This handles font-locking
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     nim-ts-font-lock-rules))

  ;; TODO make Indentation work with tree-sitter
  (setq-local indent-line-function #'nim-ts-mode-indent-line-simple)

  (setq-local treesit-font-lock-feature-list
              '((comment keyword literal_comment)
                (declaration call expression)
                (exception delimiter special operator)
                ;; (delimiter special call declaration
                ;;  exception expression literal_comment keyword operator)
                ))

  ;; remap the font-faces used as tree-sitter node captures to usable font-faces
  (nim-ts-mode--remap-fonts)

  (treesit-major-mode-setup))


(provide 'nim-ts-mode)
