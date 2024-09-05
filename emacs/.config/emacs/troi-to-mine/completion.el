;;; completion.el --- sourcing and display of completions  -*- lexical-binding: t -*-

;;; Commentary:

;; most of this started from the excellent emacs-bedrock configuration. i've
;; made a few changes but the base was one of the better completion setups
;; i've seen.

;;; Code:


(use-package vertico
  :init (vertico-mode))


(use-package marginalia
  :init (marginalia-mode))


(use-package corfu
  :init (global-corfu-mode)
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))


(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))


(use-package consult
  :bind
  (
   ;; replace some commands
   ("C-x b" . consult-buffer)     ; was switch-to-buffer
   ("M-y"   . consult-yank-pop)   ; was yank-pop
   ;; new searches
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
   ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
   ("M-s L" . consult-line-multi) ; isearch to M-s s
   ("M-s o" . consult-outline)
   ;; isearch integration
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)   ; was isearch-edit-string
   ("M-s e" . consult-isearch-history) ; was isearch-edit-string
   ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
   )
  :config
  (setopt consult-narrow-key "<"))   ;; Narrowing lets you restrict results to certain groups of candidates


(use-package embark-consult
  :after consult)


(use-package embark
  :demand t
  :after (avy consult)
  :bind
  (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun troi/avy-action-embark (pt)
    (unwind-protect
	(save-excursion
	  (goto-char pt)
	  (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  ;; After invoking avy-goto-char-timer, hit "." to run embark at the
  ;; next candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'troi/avy-action-embark))


(provide 'completion)
;;; completion.el ends here
