;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;;
;;; doom package configuration is hopefully logically grouped with the rest of my
;;; changes.
;;;


;;;
;;; Some functionality uses this to identify you, e.g. GPG configuration, email
;;; clients, file templates and snippets. It is optional.
;;;

(setq user-full-name "Troy Brumley"
      user-mail-address "blametroi@gmail.com")


;;;
;;; project mismanagment with projetile
;;;

(after! projectile
  (setq projectile-enable-caching nil)
  (setq projectile-project-search-path '("~/projects/")))


;;;
;;; i prefer that we don't continue comments by defaut, gladly this does work
;;;

(after! evil (setq +evil-want-o/O-to-continue-comments nil) )


;;;
;;; If you use `org' and don't want your org files in the default location below,
;;; change `org-directory'. It must be set before org loads!
;;;

(setq org-directory "~/Dropbox/org/")


;;;
;;; mouse and trackpad
;;;

;; a rather heavy handed (but working) way to stop the mac touchpad from moving things on me.
;; i tried to find way to do this as a doom after! but the double and triple variants kept
;; being active. yes, i searched the source. no, i couldn't find where that was done.

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
   (global-set-key [triple-wheel-right] 'ignore)))


;;;
;;; i like big comment blocks and i can not lie
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full width comment box                                                 ;;
;; from http://irreal.org/blog/?p=374                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun troi-comment-box (b e)
  "Draw a box comment around the region but arrange for the region to extend
to at least the fill column. Place the point after the comment box."

  (interactive "r")

  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

(global-set-key (kbd "C-c b b") 'troi-comment-box)

;; i'm working to get rebox2 to work with fortran

;;(load! "site-lisp/rebox2.el")
;;(require 'rebox2)
;;(setq rebox-style-loop '(24 16))
;;(global-set-key [(meta q)] 'rebox-dwim)
;;(global-set-key [(shift meta q)] 'rebox-cycle)
;;(defun troi-f90-setup ()
;;  (setq comment-start "! "
;;        comment-end " !")
;;  (unless (memq 46 rebox-style-loop)
;;    (make-local-variable 'rebox-style-loop)
;;    (nconc rebox-style-loop '(46))))
;;(add-hook 'f90-mode-hook #'troi-f90-setup)


;;;
;;; language specific
;;;


;; pascal, i've made some tweaks hoping to create my own fpc specific mode.

(load! "site-lisp/pascal.el")
(load! "site-lisp/my-pascal-helper.el")


;; sml, all i add is the formatter

(load! "/Users/troi/projects/sml/smlformat/editors/emacs/smlformat.el")


;; fortran

(after! f90
  (set-formatter! 'fprettify
    '("fprettify" "-w 4" "-") :modes '(f90-mode fortran-mode)))


;;;
;;; not using treemacs much yet, but ...
;;;

(after! treemacs
  (treemacs-follow-mode 1)
  (setq treemacs-width 40))


;;;
;;; window splitting ... focus switches to split
;;;

(setq evil-vsplit-window-right t
      evil-split-window-below t
      split-width-threshold 240)


;;;
;;; my hl todo variations
;;;

(after! hl-todo
  (setq hl-todo-keyword-faces
        '(("TODO"   warning bold)
          ("todo"   warning bold)
          ("txb"    success italic)
          ("TXB"    success italic)
          ("NOTE"   success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("FIXME"  error bold)
          ("BUG"    error bold))))


;; cl-libify

(require 'cl-libify)


;;;
;;; fonts
;;;

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24 :weight 'Normal)
      doom-variable-pitch-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24 :weight 'Normal))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!


;;;
;;; themes
;;;

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'vegetative)

(setq doom-theme 'ef-melissa-dark)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;; end file $DOOMDIR/config.el
