;;; keybinding.el --- key bindings that aren't in specific use-packages  -*- lexical-binding: t -*-

;;; Commentary:

;; where possible i try to keep bindings with specific packages, but
;; those that aren't are here.

;;; Code:

;; i may at some point reclaim this key, but i admit it's wasteful to have a whole key
;; dedicated to terminating emacs.

(global-unset-key (kbd "C-x C-c"))

;; based on a post by enzu.ru at medium, i'm changing a few keys
;; around but i probably won't switch to swiper just yet. i find that
;; i don't naturally use the default emacs cursor movement keys, and
;; just use the arrow keys instead. i'd probably use the vi style keys
;; with a modifier if they weren't already used elsewhere. arrows seem
;; quick enough to me.
;;
;; i am using more vimish c-f and c-b, with c-v as a synonym for c-b,
;; for screen scrolling.
;;
;; see: https://enzuru.medium.com/emacs-in-a-few-dozen-keystrokes-and-why-some-of-you-should-just-use-vim-14b9af30be70

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-f") 'scroll-up-command)
(global-set-key (kbd "C-b") 'scroll-down-command)
(global-set-key (kbd "C-v") 'scroll-down-command) ;; yeah, same as but easier to reach
(global-set-key (kbd "C-x <up>") 'beginning-of-buffer)
(global-set-key (kbd "C-x <down>") 'end-of-buffer)

;; these should be shift-arrow. i'll have to get used
;; to not using shift-arrow for selection, but i use
;; control space in org mode already so the transition
;; shouldn't be too difficult.
;;(when (fboundp 'windmove-default-keybindings)
;;  (windmove-default-keybindings))
;; more trouble than it's worth, i don't tend to have many windows open.

;; some keybinds i picked up i know not where ...

(global-set-key "\M-z" 'zap-up-to-char)      ;; this is the way
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; probably the best package for emacs newbies and those of us with
;; memory issues. turns out it's moved into base for emacs 30.

(use-package which-key
  :diminish
  :init (which-key-mode))


;; for discovery of unbound keys.

(use-package free-keys
  :diminish)


(provide 'keybinding)
;;; keybinding.el ends here
