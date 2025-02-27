;;; early-init.el --- troy brumley's early-init  -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; the following bits speed up file searching and reduce garbage
;; collection during startup, and also creates a garbage collection
;; trigger when emacs loses focus. the garbage collection thresholds
;; are set to more reasonable values and anything we suppressed here
;; (file name handlers for the moment) is restored once init.el
;; completes its load.

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq vc-handled-backends '(Git))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun troi/gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun troi/reset-init-values ()
  "Put GC values back."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function
        :after after-focus-change-function
        #'troi/gc-after-focus-change)))))

(with-eval-after-load 'init
  (troi/reset-init-values))


;; quiet some stuff down.

(setq byte-compile-warnings '(not obsolete))
(setopt warning-suppress-log-types '((comp) (bytecomp)))
(setopt native-comp-async-report-warnings-errors 'silent)


;; resizing the emacs frame can be terribly expensive when changing
;; the font.

(setq frame-inhibit-implied-resize t)


;; disable some gui elements.

(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
;; (scroll-bar-mode -1)
(setq inhibit-startup-echo-area-message (user-login-name))


;; using fullboth instead of maximized has a bit of a jarring startup
;; but then moves the frame to it's own desktop or space or whatever
;; the really fullscreen mode is called on macos.

;;(setopt frame-resize-pixelwise t)
(setopt window-resize-pixelwise t)
(setopt initial-frame-alist '((fullscreen . fullboth)
			      (ns-appearance . dark)
			      (ns-transparent-titlebar . t)))
(setopt default-frame-alist '((fullscreen . fullboth)
                              (ns-appearance . dark)
                              (ns-transparent-titlebar . t)))


;; increase interprocess buffer from 4k to something useful. it is not
;; clear how big it can be on a mac, so i guess we'll find out. eshell
;; is one area this is noticeable.

(setq read-process-output-max (* 4 1024 1024))


;; emacs collective's no littering recommendation

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(provide 'early-init)

;;; early-init.el ends here
