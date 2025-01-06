;;; troi-c-style.el --- troi's c style for c-mode -*- lexical-binding: t -*-

;; Keywords: c, tools

;; the framework code is from google-c-style.el, Copyright (C) 2008
;; Google Inc. they released it as free software under the terms of
;; either the GNU GPL (any version from 1 on) or the "Artistic
;; License".
;;
;; i'm changing the capitalization (i don't tend to capitalize in
;; comments) and tweaking some of the wording around, but most credit
;; goes to the good folks at the place that once aspired to do no
;; evil.


;;; Commentary:

;; provides my C/C++ coding style. add 'troi-set-c-style' to
;; 'c-mode-common-hook' after requiring this file. example:
;;
;;    (require 'troi-c-style)
;;    (add-hook 'c-mode-common-hook 'troi-set-c-style)
;;
;; to make the <return> key go to the next line and space over to
;; the right place, also add:
;;
;;    (add-hook 'c-mode-common-hook 'troi-make-newline-indent)
;;
;; the style:
;;
;; my actual style was made by the cc-mode c-guess on a large file i
;; had formatted to my liking with the following astyle configuration:
;;
;; style=attach
;; indent=tab=8
;; attach-namespaces
;; attach-classes
;; attach-extern-c
;; attach-closing-while
;; indent-col1-comments
;; align-pointer=name
;; break-return-type
;; break-return-type-decl
;; pad-header
;; unpad-paren
;; max-code-length=80
;; break-after-logical
;; keep-one-line-blocks
;; keep-one-line-statements
;; remove-braces
;; squeeze-lines=8
;; pad-header
;; pad-include
;; pad-comma
;;
;; this gets me attached braces most everywhere, allows one line
;; unbraced blocks under if/else and such, and otherwise seems to
;; follow the linux kernel conventions.


;;; Code:

;; For some reason 1) c-backward-syntactic-ws is a macro and 2) under
;; Emacs 22 bytecode cannot call (unexpanded) macros at run time:
(eval-when-compile (require 'cc-defs))

;; ;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; ;; elegant solution of composing a list of lineup functions or quantities with
;; ;; operators such as "add")
;; (defun google-c-lineup-expression-plus-4 (langelem)
;;   "Indents to the beginning of the current C expression plus 4 spaces.

;; This implements title \"Function Declarations and Definitions\"
;; of the Google C++ Style Guide for the case where the previous
;; line ends with an open parenthesis.

;; \"Current C expression\", as per the Google Style Guide and as
;; clarified by subsequent discussions, means the whole expression
;; regardless of the number of nested parentheses, but excluding
;; non-expression material such as \"if(\" and \"for(\" control
;; structures.

;; Suitable for inclusion in `c-offsets-alist'."
;;   (save-excursion
;;     (back-to-indentation)
;;     ;; Go to beginning of *previous* line:
;;     (c-backward-syntactic-ws)
;;     (back-to-indentation)
;;     (cond
;;      ;; We are making a reasonable assumption that if there is a control
;;      ;; structure to indent past, it has to be at the beginning of the line.
;;      ((looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
;;       (goto-char (match-end 1)))
;;      ;; For constructor initializer lists, the reference point for line-up is
;;      ;; the token after the initial colon.
;;      ((looking-at ":\\s *")
;;       (goto-char (match-end 0))))
;;     (vector (+ 4 (current-column)))))

(c-add-style
 "troi-c-style"
 '("linux"
   (c-basic-offset . 8)
   (c-offsets-alist
    (block-close . 0)
    (brace-list-close . 0)
    (brace-list-entry . 0)
    (brace-list-intro . +)
    (case-label . 0)
    (class-close . 0)
    (cpp-define-intro . 3)
    (defun-block-intro . +)
    (defun-close . 0)
    (else-clause . 0)
    (inclass . +)
    (statement . 0)
    (statement-block-intro . +)
    (statement-case-intro . +)
    (statement-cont . +)
    (substatement . +)
    (topmost-intro . 0)
    (topmost-intro-cont . 0)
    (access-label . -)
    (annotation-top-cont . 0)
    (annotation-var-cont . +)
    (arglist-close . c-lineup-close-paren)
    (arglist-cont c-lineup-gcc-asm-reg 0)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-intro . +)
    (block-open . 0)
    (brace-entry-open . 0)
    (brace-list-open . 0)
    (c . c-lineup-C-comments)
    (catch-clause . 0)
    (class-open . 0)
    (comment-intro . c-lineup-comment)
    (composition-close . 0)
    (composition-open . 0)
    (constraint-cont . +)
    (cpp-macro . -1000)
    (cpp-macro-cont . +)
    (defun-open . 0)
    (do-while-closure . 0)
    (extern-lang-close . 0)
    (extern-lang-open . 0)
    (friend . 0)
    (func-decl-cont . +)
    (incomposition . +)
    (inexpr-class . +)
    (inexpr-statement . +)
    (inextern-lang . +)
    (inher-cont . c-lineup-multi-inher)
    (inher-intro . +)
    (inlambda . 0)
    (inline-close . 0)
    (inline-open . +)
    (inmodule . +)
    (innamespace . +)
    (knr-argdecl . 0)
    (knr-argdecl-intro . 0)
    (label . 0)
    (lambda-intro-cont . +)
    (member-init-cont . c-lineup-multi-inher)
    (member-init-intro . +)
    (module-close . 0)
    (module-open . 0)
    (namespace-close . 0)
    (namespace-open . 0)
    (objc-method-args-cont . c-lineup-ObjC-method-args)
    (objc-method-call-cont
     c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
    (objc-method-intro . [0])
    (statement-case-open . 0)
    (stream-op . c-lineup-streamop)
    (string . -1000)
    (substatement-label . 0)
    (substatement-open . 0)
    (template-args-cont
     c-lineup-template-args
     c-lineup-template-args-indented-from-margin))))


;;;###autoload
(defun troi-set-c-style ()
  "Set the current buffer's c-style to my preferred style."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-set-style "troi-c-style")
  (message "%s" "troi-set-c-style complete"))

;;;###autoload
(defun troi-make-newline-indent ()
  "Set up preferred newline behavior. Not set by default."
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent)
  (message "%s" "troi-make-newline-indent complete"))


(provide 'troi-c-style)
;;; troi-c-style.el ends here
