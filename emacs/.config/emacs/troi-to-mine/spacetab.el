;;; spacetab.el --- spaces, tabs, whitespace, editorconfig  -*- lexical-binding: t -*-

;;; Commentary:

;; in an ideal world everything would pull from editorconfig. alas, we
;; don't live in an ideal world. in particular, the treesitter and lsp
;; formatters don't seem to check there. here are the more global
;; settings. i keep the programming language formatting elsewhere.

;;; Code:




;; this is the way

(use-package editorconfig
  :demand t
  :diminish
  :config (editorconfig-mode 1))


(setopt indent-tabs-mode nil)
(setopt mode-require-final-newline t)
(setopt sentence-end-double-space nil)


;; don't add this to text-mode. markdown at least finds trailing
;; spaces to be significant. yet another reason not to use markdown...

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))


(provide 'spacetab)
;;; spacetab.el ends here
