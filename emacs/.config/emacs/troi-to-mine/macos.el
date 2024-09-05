;;; macos.el --- mac specific initialization -*- lexical-binding: t -*-

;;; Commentary:

;; those things that are mac specific. the key remaps for various keys that
;; the mac desktop wants a work in progress.

;;; Code:


;; shell path isn't always included on mac os x.

(use-package exec-path-from-shell
  :demand t)

(setopt exec-path-from-shell-arguments nil)
(exec-path-from-shell-initialize)


;; remap modifier keys. i change caps lock to control in the macos
;; keyboard settings.

;; a standard keyboard is control fn os alt |spacebar| alt(gr) os menu control
;;
;; using karbiner i've changed the bottom row thusly:
;;
;; fn->control
;; control->fn
;; option->command
;; command->alt
;; spbr
;; command->unchanged
;; option->unchanged
;;
;; and so i don't need these anymore, as near as i can tell:
;;;
;;(setopt ns-alternate-modifier 'alt)
;;(setopt ns-command-modifier 'meta)
;;(setopt ns-function-modifier 'hyper)
;;(setopt ns-right-alternate-modifier 'super)


;; save some screen space.

(setopt ns-auto-hide-menu-bar t)


;; a rather heavy handed (but working) way to stop the mac touchpad
;; from moving things on me. i tried to find way to do this as a doom
;; after! but the double and triple variants kept being active. yes, i
;; searched the source. no, i couldn't find where that was done. the
;; customize interface isn't showing me these options in any way that
;; i understand. the goal here is to prevent my ham handed taps and
;; brushes of the touchpad from moving stuff around. i have mixed
;; feelings about drag-the-scrollbar mouse scrolling, but i don't like
;; the mouse wheel in text editing.

(add-to-list
 'emacs-startup-hook
 (lambda ()
   (global-set-key [wheel-up] 'ignore)
   (global-set-key [double-wheel-up] 'ignore)
   (global-set-key [triple-wheel-up] 'ignore)
   (global-set-key [wheel-down] 'ignore)
   (global-set-key [double-wheel-down] 'ignore)
   (global-set-key [triple-wheel-down] 'ignore)
   (global-set-key [wheel-left] 'ignore)
   (global-set-key [double-wheel-left] 'ignore)
   (global-set-key [triple-wheel-left] 'ignore)
   (global-set-key [wheel-right] 'ignore)
   (global-set-key [double-wheel-right] 'ignore)
   (global-set-key [triple-wheel-right] 'ignore)
   (mouse-wheel-mode -1)
   (message "trackpad stuff set to ignore")))

(mouse-avoidance-mode 'banish)


(provide 'macos)
;; macos.el ends here
