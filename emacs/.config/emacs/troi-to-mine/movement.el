;;; movement.el --- jumping and such  -*- lexical-binding: t -*-

;;; Commentary:

;; pretty straightforward. avy seems to be the predominate jetpack
;; these days.

;;; Code:


(use-package avy
  :bind
  (("C-'" . avy-goto-char-timer)
   ("C-:" . avy-goto-char)))


(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;; setq aw-background nil) ;; to turn off grey out of window on prompt


(provide 'movement)
;;; movement.el ends here
