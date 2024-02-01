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


(defvar nim-ts-indent-rules
  '())


(defvar nim-ts-mode--font-remap-alist
  '((punctuation.delimiter-face 'font-lock-delimiter-face "A face for delimiters")
    (punctuation.bracket-face 'font-lock-bracket-face "A face for brackets")
    (variable.builtin-face 'font-lock-builtin-face "A face for builtin variables")
    (function.call-face 'font-lock-function-call-face "A face for function calls")
    (type.declaration-face 'font-lock-type-face "A face for type declarations")
    (type.export-face 'font-lock-type-face "A face for type export")
    (type.qualifier-face 'font-lock-type-face "A face for type qualification")
    (function-face 'font-lock-function-name-face "A face for functions")
    (method-face 'font-lock-function-name-face "A face for methods")
    (function.macro-face 'font-lock-function-name-face "A face for macros")
    (parameter-face 'font-lock-variable-use-face "A face for parameters")
    (variable-face 'font-lock-variable-name-face "A face for variables")
    (type-face 'font-lock-type-face "A face for types")
    (exception-face 'font-lock-warning-face "A face for exceptions")
    (field-face 'font-lock-property-use-face "A face for fields")
    (comment-face 'font-lock-comment-face "A face for comments")
    (comment.documentation-face 'font-lock-doc-face "A face for documentation")
    (string-face 'font-lock-string-face "A face for strings")
    (character-face 'font-lock-string-face "A face for characters")
    (string.escape-face 'font-lock-escape-face "A face for escaped strings")
    (number-face 'font-lock-number-face "A face for numbers")
    (float-face 'font-lock-number-face "A face for floats")
    (constant.builtin-face 'font-lock-constant-face "A face for constants")
    (conditional-face 'font-lock-keyword-face "A face for conditionals")
    (include-face 'font-lock-keyword-face "A face for include statements")
    (repeat-face 'font-lock-keyword-face "A face for loops")
    (keyword-face 'font-lock-keyword-face "A face for keywords")
    (keyword.function-face 'font-lock-keyword-face "A face for function keywords")
    (keyword.operator-face 'font-lock-keyword-face "A face for operator keywords")
    (keyword.return-face 'font-lock-keyword-face "A face for return keywords")
    (operator-face 'font-lock-operator-face "A face for operators"))

  "A list of new font to existing font mappings that are used by the `nim-ts-mode--remap-fonts' macro.
Mappings should be in the format (new-font-face 'old-font-face \"description\") inside a list."
  )


(defvar nim-ts-mode--font-base-theme 'doom-one)


;; TODO make macro able to add additional font specs to the new font
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
OLD:   - the symbol of the font-face to use as a base
DOC:   - a docstring that describes the font-face
THEME: - the symbol of the color theme to use to inherit from"
  `(progn
     (defface ,new '((t (:inherit ,old))) ,doc)
     (custom-theme-set-faces ,theme
                             '(,new ((t (:inherit ,old)))))))



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

  ;; This handles indentation
  (setq-local treesit-simple-indent-rules
              nim-ts-indent-rules)

  (setq-local treesit-font-lock-feature-list
              '((comment delimiter special call declaration
                 exception expression literal_comment keyword operator)))

  ;; remap the font-faces used as tree-sitter node captures to usable font-faces
  (nim-ts-mode--remap-fonts)

  (treesit-major-mode-setup))


(provide 'nim-ts-mode)

(defun bla-fun ()
  (defvar python--treesit-settings
    (treesit-font-lock-rules
     :feature 'comment
     :language 'python
     '((comment) @font-lock-comment-face)

     :feature 'string
     :language 'python
     '((string) @python--treesit-fontify-string
       (interpolation ["{" "}"] @font-lock-misc-punctuation-face))


     :feature 'keyword
     :language 'python
     `([,@python--treesit-keywords] @font-lock-keyword-face
       ((identifier) @font-lock-keyword-face
        (:match "\\`self\\'" @font-lock-keyword-face)))

     :feature 'definition
     :language 'python
     '((function_definition
        name: (identifier) @font-lock-function-name-face)
       (class_definition
        name: (identifier) @font-lock-type-face)
       (parameters (identifier) @font-lock-variable-name-face)
       (parameters (default_parameter name: (identifier) @font-lock-variable-name-face)))

     :feature 'builtin
     :language 'python
     `(((identifier) @font-lock-builtin-face
        (:match ,(rx-to-string
                  `(seq bol
                        (or ,@python--treesit-builtins
                            ,@python--treesit-special-attributes)
                        eol))
                @font-lock-builtin-face)))

     :feature 'decorator
     :language 'python
     '((decorator "@" @font-lock-type-face)
       (decorator (call function: (identifier) @font-lock-type-face))
       (decorator (identifier) @font-lock-type-face)
       (decorator [(attribute) (call (attribute))] @python--treesit-fontify-dotted-decorator))

     :feature 'function
     :language 'python
     '((call function: (identifier) @font-lock-function-call-face)
       (call function: (attribute
                        attribute: (identifier) @font-lock-function-call-face)))

     :feature 'constant
     :language 'python
     '([(true) (false) (none)] @font-lock-constant-face)

     :feature 'assignment
     :language 'python
     `(;; Variable names and LHS.
       (assignment left: (identifier)
                   @font-lock-variable-name-face)
       (assignment left: (attribute
                          attribute: (identifier)
                          @font-lock-variable-name-face))
       (augmented_assignment left: (identifier)
                             @font-lock-variable-name-face)
       (named_expression name: (identifier)
                         @font-lock-variable-name-face)
       (pattern_list [(identifier)
                      (list_splat_pattern (identifier))]
                     @font-lock-variable-name-face)
       (tuple_pattern [(identifier)
                       (list_splat_pattern (identifier))]
                      @font-lock-variable-name-face)
       (list_pattern [(identifier)
                      (list_splat_pattern (identifier))]
                     @font-lock-variable-name-face))


     :feature 'type
     :language 'python
     ;; Override built-in faces when dict/list are used for type hints.
     :override t
     `(((identifier) @font-lock-type-face
        (:match ,(rx-to-string
                  `(seq bol (or ,@python--treesit-exceptions)
                        eol))
                @font-lock-type-face))
       (type [(identifier) (none)] @font-lock-type-face)
       (type (attribute attribute: (identifier) @font-lock-type-face))
       ;; We don't want to highlight a package of the type
       ;; (e.g. pack.ClassName).  So explicitly exclude patterns with
       ;; attribute, since we handle dotted type name in the previous
       ;; rule.  The following rule handle
       ;; generic_type/list/tuple/splat_type nodes.
       (type (_ !attribute [[(identifier) (none)] @font-lock-type-face
                            (attribute attribute: (identifier) @font-lock-type-face) ]))
       ;; collections.abc.Iterator[T] case.
       (type (subscript (attribute attribute: (identifier) @font-lock-type-face)))
       ;; Nested optional type hints, e.g. val: Lvl1 | Lvl2[Lvl3[Lvl4]].
       (type (binary_operator) @python--treesit-fontify-union-types)
       ;;class Type(Base1, Sequence[T]).
       (class_definition
        superclasses:
        (argument_list [(identifier) @font-lock-type-face
                        (attribute attribute: (identifier) @font-lock-type-face)
                        (subscript (identifier) @font-lock-type-face)
                        (subscript (attribute attribute: (identifier) @font-lock-type-face))]))

       ;; Patern matching: case [str(), pack0.Type0()].  Take only the
       ;; last identifier.
       (class_pattern (dotted_name (identifier) @font-lock-type-face :anchor))

       ;; Highlight the second argument as a type in isinstance/issubclass.
       ((call function: (identifier) @func-name
              (argument_list :anchor (_)
                             [(identifier) @font-lock-type-face
                              (attribute attribute: (identifier) @font-lock-type-face)
                              (tuple (identifier) @font-lock-type-face)
                              (tuple (attribute attribute: (identifier) @font-lock-type-face))]
                             (:match ,python--treesit-type-regex @font-lock-type-face)))
        (:match "^is\\(?:instance\\|subclass\\)$" @func-name))

       ;; isinstance(t, int|float).
       ((call function: (identifier) @func-name
              (argument_list :anchor (_)
                             (binary_operator) @python--treesit-fontify-union-types-strict))
        (:match "^is\\(?:instance\\|subclass\\)$" @func-name)))

     :feature 'escape-sequence
     :language 'python
     :override t
     '((escape_sequence) @font-lock-escape-face)

     :feature 'number
     :language 'python
     '([(integer) (float)] @font-lock-number-face)

     :feature 'property
     :language 'python
     '((attribute
        attribute: (identifier) @font-lock-property-use-face)
       (class_definition
        body: (block
                  (expression_statement
                   (assignment left:
                               (identifier) @font-lock-property-use-face)))))

     :feature 'operator
     :language 'python
     `([,@python--treesit-operators] @font-lock-operator-face)

     :feature 'bracket
     :language 'python
     '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

     :feature 'delimiter
     :language 'python
     '(["," "." ":" ";" (ellipsis)] @font-lock-delimiter-face)

     :feature 'variable
     :language 'python
     '((identifier) @python--treesit-fontify-variable))
    "Tree-sitter font-lock settings."))
