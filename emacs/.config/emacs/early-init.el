;;; early-init.el --- Troy Brumley's early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2024-2025 Troy Brumley (aka Troi)

;; Author: Troy Brumley <blametroi@gmail.com>

;; All rights reserved.

;; This file is NOT part of GNU Emacs. The author considers it to be
;; in the public domain.

;; This file was originally generated from an Org document. As much as
;; I try, I just can't get into `literate' configurations. It was a
;; good exercise to go through a few configurations and build my own
;; attempt at being literate, but I can't live with it on a regular
;; basis.
;;
;; As I moved this back to being pre-literate I made many changes.
;; Most noticeable of them is the removal of `use-package'. It's a
;; cool tool and it truly is the best thing since sliced bread, but it
;; lets me skip learning things. It may come back into use once I have
;; the confidence that I'm not avoiding learning things.
;;
;; There are several good 'learning' configurations to be found on
;; reddit, github, and elsewhere on the web. Two highly readable
;; configurations that influenced me the most are:
;;
;; 1) Protesilaos "Prot" Stavrou's highly instructive literal
;;    configuration found on his website:
;;
;;    https://protesilaos.com/emacs/dotemacs
;;
;; 2) Ashton Wiersdorf's Emacs-Bedrock  at:
;;
;;    https://codeberg.org/ashton314/emacs-bedrock

;; The Emacs early initialization can include most anything that
;; doesn't:
;;
;; 1) Require `use-package' or `require'.
;;
;; 2) Involve or change the visual frames and windows of Emacs.
;;
;; I am running Emacs 30 and its capabilities are assumed throughout. I
;; don't do release checks and fallbacks.

;;; Code:


;;; Speed up initialization:

;; Turn off garbage collection during startup. It is turned back on,
;; restoring the default settings, after initialization is finished.

(defvar troi/gc-cons-threshold gc-cons-threshold)
(defvar troi/gc-cons-percentage gc-cons-percentage)
(setopt gc-cons-threshold most-positive-fixnum)
(setopt gc-cons-percentage 1.0)

;; Force a garbage collection if I leave Emacs temporarily. This
;; function is run when garbage collection is turned back on. I don't
;; know how much it helps on modern hardware, but it is harmless
;; enough.

(defun troi/gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

;; Turn of `special' file name handling during startup. During
;; initialization no special handling is required. As literally
;; hundreds (and likely thousands) of files are accessed during
;; initialization, this is thought to be a significant speed-up.
;;
;; These handlers are restored once initialization completes.

(defvar troi/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Version Control can be queried during initialization. To speed
;; things up, I remove all backend support except for Git.
;; The only vcs I use is Git. I save the original backends list but
;; I don't restore it.
;;
;; I've seen some configurations that remove all backends here but
;; then you can't use them from `init.el'.

(defvar troi/vc-handled-backends vc-handled-backends)
(setopt vc-handled-backends '(Git))


;;; Dealing with the touchpad. It's just too touchy.

;; See the function document string:

(defun troi/bad-mouse-stop-that ()
  "Disable the mouse/touch-pad.
This function aggressively swats mouse/touchpad in an attempt
to prevent the Mac track-pad from causing motion when I
inevitably brush it. I am setting it twice, both in
`emacs-startup-hook' and `after-init-hook' because I know of
at least one package that needs `mouse-wheel-mode' turned off
during init and because something else during init overwrites
my `wheel-down' overrides."

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
  (message "track-pad stuff set to ignore"))

(add-to-list
 'emacs-startup-hook #'troi/bad-mouse-stop-that)
(add-to-list
 'after-init-hook #'troi/bad-mouse-stop-that)


;;; Park the mouse pointer in an inoffensive location.

(mouse-avoidance-mode 'banish)
(setopt mouse-avoidance-banish-position
	'((frame-or-window . frame) (side . right) (side-pos . 1)
	  (top-or-bottom . bottom) (top-or-bottom-pos . 15)))


;;; Prepare for display.

;; These set the frame on my Mac to 'real' full-screen.

(setopt frame-inhibit-implied-resize t)
(setopt frame-resize-pixelwise t)
(setopt window-resize-pixelwise t)
(setopt initial-frame-alist '((fullscreen . fullboth)
			      (ns-appearance . dark)
			      (ns-transparent-titlebar . t)))
(setopt default-frame-alist '((fullscreen . fullboth)
                              (ns-appearance . dark)
                              (ns-transparent-titlebar . t)))

;; Use standard Emacs UI elements and not OS tailored ones.

(setopt use-dialog-box nil)
(setopt use-file-dialog nil)
(setopt use-short-answers t)

;; Quiet down the startup, I want to open up in *scratch*.

(setopt inhibit-splash-screen t)
(setopt inhibit-startup-screen t)
(setq inhibit-x-resources t)       ; x includes windows registry and ns here
(setopt inhibit-startup-echo-area-message user-login-name)
(setopt inhibit-startup-buffer-menu t)

;; I leave the menu bar active but hidden. The scroll and tool
;; bars are hidden.

(menu-bar-mode)        ; it's tucked out of the way in MacOS
(setopt ns-auto-hide-menu-bar t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(context-menu-mode -1)


;;; JIT compilation.

;; Silence warnings that aren't relevant during normal sessions.

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setopt native-comp-async-report-warnings-errors 'silent)

;; Enable native compilation if it is available.

(if (and (fboundp 'native-compile-available-p)
         (native-compile-available-p))
    (setopt package-native-compile t)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t)
  (setq native-comp-jit-compilation t))


;;; Metadata and stragglers.

(setopt user-full-name "Troy Brumley")
(setopt user-mail-address "BlameTroi@gmail.com")
(setopt auth-sources '("~/.authinfo.gpg"))
(setopt auth-source-cache-expiry nil)

;; The default is 4K or 16K. 64K is the max for a Mac.

(setq read-process-output-max (* 64 1024))

;; `no-littering' recommends this to stash the native compiled code
;; under var.

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))


;;; Add hooks to restore changes after initialization.

;; Restore garbage collection and file name handler once startup
;; completes. We also plug in the idle time garbage collection
;; trigger.

(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; These were hard coded as 8 Mb and 20%.
   (setopt gc-cons-threshold troi/gc-cons-threshold)
   (setopt gc-cons-percentage troi/gc-cons-percentage)
   (setq   file-name-handler-alist troi/file-name-handler-alist)
   (message "gc-cons-threshold & file-name-handler-alist restored")
   (when (boundp 'after-focus-change-function)
     (add-function
      :after after-focus-change-function
      #'troi/gc-after-focus-change))))

(provide 'early-init)
;;; early-init.el ends here.
