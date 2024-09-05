;;; colorful.el --- themes fonts icons -*- lexical-binding: t -*-

;;; Commentary:

;; these are some themes i've tried and found at least usable enough
;; to be worthy of a longer trial: ef-melissa-dark, wheatgrass, my
;; favorites so far. orangey-bits, standard-dark vegetative, and some
;; of the other green themes.

;; for me the big issue is contrast, whether using a light or dark
;; mode theme. of late i've found that the acme theme is pretty
;; usable.

;; trying to switch from all the icons to nerd icons

;;; Code:

;;(when (display-graphic-p)
;;(use-package notink-theme
;;  :config
;;  (load-theme 'notink t))

(when (display-graphic-p)
  (use-package acme-theme
    :config
    (load-theme 'acme t)
    (setopt acme-theme-black-fg t)
    (custom-set-faces
     '(hl-line ((t (
                    :inherit highlight
                    :extend t
                    :background "LightGoldenrod2"
                    :foreground "black"))))
     )))
;;'(compilation-error ((t (:background "gray80" :foreground "Red"))))
;;'(flymake-error ((t (:underline (:color "Red" :style wave :position nil)))))
;;'(font-lock-comment-face ((t (:foreground "#707070" :slant oblique))))
;;'(font-lock-comment-face ((t (:foreground "#005500" :slant oblique))))


;;(use-package nano-theme
;;  :config
;;  (load-theme 'nano-light t))
;;)

;; icons and nerdish fonts. i was using all-the-icons but i find
;; the nerd-xxx sets better.

;; have not added corfu yet
(when (display-graphic-p)
  (global-prettify-symbols-mode t)
  (use-package nerd-icons
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono"))
  (use-package nerd-icons-dired
    :diminish
    :after nerd-icons
    :hook
    (dired-mode . nerd-icons-dired-mode))
  (use-package nerd-icons-ibuffer
    :diminish
    :after nerd-icons
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
  (use-package nerd-icons-completion
    :after marginalia
    :config
    (nerd-icons-completion-mode)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
  )

;; faces -- text size and font selection.

;; TODO currently using the custom-set-face functions, but this needs
;; to be changed.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "IOSEVKA NF" :foundry "nil" :slant normal :weight regular :height 220 :width normal))))
 ;;'(compilation-error ((t (:background "gray80" :foreground "Red"))))
 ;;'(flymake-error ((t (:underline (:color "Red" :style wave :position nil)))))
 ;;'(font-lock-comment-face ((t (:foreground "#707070" :slant oblique))))
 '(font-lock-comment-face ((t (:foreground "#005500" :slant oblique))))
;;  '(hl-line ((t (:inherit highlight :extend t :background "LightGoldenrod2" :foreground "black"))))
 '(variable-pitch ((t (:weight regular :height 250 :width normal :family "IOSEVKA NF")))))

;; TODO: the above is the only way i can get acme-theme to show the highlight
;; line distinctly. changes in the theme file itself aren't taking. i opted to
;; make comments oblique as well.


;; giving rainbow delimiters a try again. there is no global mode for
;; this anymore, so it needs to be hooked to the programming modes.
;; if doing it for all programming modes is causing problems, yank
;; the hook here and use more specific hooks elsewhere.
;; TODO: this is not very distinct with the acme theme.

;; (use-package rainbow-delimiters
;;   :defer t
;;   :hook (prog-mode rainbow-delimiter-mode))


;; hl-todo has a problem with the acme theme, it isn't distinct from
;; a marked region. i've fixed that in customization but that needs to be
;; extracted and done properly. TODO see the custom-set-faces earlier in
;; this file.
;;
;; '(font-lock-comment-face ((t (:foreground "#005500" :slant oblique))))
;;  '(hl-line ((t (:inherit highlight :extend t :background "LightGoldenrod2" :foreground "black"))))
;;
;; define colors for TODO keywords and get the keywords
;; squared away to my preferences.

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("C-c tp" . hl-todo-previous)
              ("C-c tn" . hl-todo-next)
              ("C-c to" . hl-todo-occur)
              ("C-c ti" . hl-todo-insert)))


;; colorful.el ends here
