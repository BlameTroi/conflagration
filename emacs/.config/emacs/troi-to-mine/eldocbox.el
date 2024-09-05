;;; eldocbox.el --- eldoc everywhere, and the popup box  -*- lexical-binding: t -*-

;;; Commentary:

;; one document api to show them all. eldoc-box is pretty handy since
;; i don't like the mini-buffer fluttering up and down. however, the
;; use of C-g to close the doc frame doesn't seem to work. the author
;; notes that any point movement will accomplish the clear so C-f gets
;; the job done for now.

;; eglot is reloading eldoc and that was blowing out my diminish. defering
;; until after eglot is loaded fixes that.

;;; Code:


(use-package eldoc
  :pin gnu
  :after eglot
  :diminish
  :init (global-eldoc-mode))


(use-package eldoc-box
  :pin melpa-stable
  :diminish
  :bind (:map prog-mode-map
              ("C-h D" . eldoc-box-help-at-point))
  :init ;; see above comments, the following doesn't work reliably.
  (setopt eldoc-box-clear-with-C-g t)
  :config
  (setopt eldoc-echo-area-prefer-doc-buffer t)
  (setopt eldoc-echo-area-use-multiline-p nil))


(provide 'eldocbox)
;;; eldocbox.el ends here
