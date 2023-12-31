;;; init.el -- Troi's emacs configuration. -*- lexical-binding: t -*-

;; Author: Troy Brumley <blametroi@gmail.com>

;;; Commentary:

;; This init was inspired by many things I've seen exploring, I don't
;; remember where all the ideas came from and any list I come up with
;; would be incomplete. Web searches, github, reddit, emacswiki are
;; all starting points.

;; My programming is likely to be scheme centric with some forays back
;; to Pascal, Cobol, Fortran, and BAL if I start using Hercules again.
;; I am doing some exercisms and will sometimes diddle with other
;; languages but Scheme and Pascal are the top two.

;; after a lot of starts stops and restarts i'm going to try to use
;; org-babel and literate configuration. patrick thomson's from his
;; blog and github is my starting point.

;; i'll be carving up his readme.org and dropping some of his narrative
;; text, but his originals are available on the web.

;; This file loads use-package, org-mode, and compiles and executes init.org.

;;; Code:

;; first things first, let's get real about garbage collection
;; in the twenty first century

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold))))
(add-function
 :after after-focus-change-function
 (lambda ()
   (unless (frame-focus-state) (garbage-collect))))

(setq max-lisp-eval-depth 3000)

;; package infrastructure

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;;(when (eq system-type 'darwin)
;;  (setq ns-auto-hide-menu-bar t))

(require 'package)
(with-eval-after-load 'package
  (dolist (arc '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                 ("melpa-stable" . "https://stable.melpa.org/packages/")))
    (add-to-list 'package-archives arc t))
  (setq package-archive-priorities
        '(("gnu" . 10) ("nongnu" . 9) ("melpa-stable" . 8))))

(setq package-native-compile t)
(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(defun reload-config ()
  "Reload the literate config from ~/.emacs.d/literate/literate-config.org"
  (interactive)
  (org-babel-load-file "~/.emacs.d/literate/literate-config.org"))

(reload-config)
(provide 'init)
;;; init.el ends here
