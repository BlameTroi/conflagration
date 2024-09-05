;;; setqopts.el --- globalish options that don't seem to fit in specific packages -*- lexical-binding: t -*-

;;; Commentary:

;; i use setopt when i can because it reduces the amount of noise
;; warnings from flymake. flymake and flycheck aren't for init file
;; scripts.

;; in some example init.el configurations i see such things wrapped in
;; a use-package for emacs, which i guess is either a meta or pseduo
;; package. i'm not bothering with that.

;;; Code:


;; a biggie

(setopt disabled-command-function nil)


;; ui visuals and such:

(setopt x-underline-at-descent-line nil)
(setopt blink-cursor-mode nil)
(setopt blink-matching-delay 0.1)
(setopt column-number-mode t)
(setopt mode-line-position-column-line-format '(" (%l,%C)"))
(setopt split-height-threshold nil)
(setopt tab-bar-mode t)
(setopt tab-bar-show 1)
(setopt display-time-mode t)
(setopt confirm-kill-emacs 'yes-or-no-p)


;; scrolling.

(setopt scroll-preserve-screen-position t)
(setopt scroll-margin 0)
(setopt scroll-step 1)
(setopt scroll-conservatively 10000)       ;; this in particular works a bit like vim


;; file memory and positioning

(save-place-mode 1)
(recentf-mode)
(global-auto-revert-mode)                ;; refresh if changed


;; i prefer switching to new windows when i open them, for help this
;; lets us close quickly via 'q'.
;; TODO not all such windows are behaving this way yet.

(setopt help-window-select t)
(setopt switch-to-buffer-obey-display-actions t)


;; TODO would this be better elsewhere?

(setopt compilation-auto-jump-to-first-error 'first-known)


;; minibuffer and some global completions stuff.

(savehist-mode)
(setopt enable-recursive-minibuffers t)
(setopt ibuffer-jump-offer-only-visible-buffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt completion-styles '(basic initials substring))
(setopt completion-auto-help 'always)
(setopt completions-max-height 10)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)
;;(setopt tab-always-indent 'complete)


;;TODO  maybe this should be in keybindings?

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)


;; line numbers for programming modes.

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 4)


;; text modes wrap.

(add-hook 'text-mode-hook 'visual-line-mode)


;; highlight the current line.

(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


(provide 'setqopts)
;;; setqopts.el ends here
