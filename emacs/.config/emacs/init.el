;;; init.el --- troy brumley's init.el -*- lexical-binding: t -*-


;;; Commentary:

;; just an 'init.el' file.
;;
;; i am aware of the formatting preference of no dangling close
;; parens, but i sometimes leave the closing paren in use-package
;; blocks on its own line for the sake of error avoidance when working
;; on the settings and for clarity in diff output. when i'm done
;; tweaking i may undangle them if i see them.
;;
;; after three or four rounds of tearing it down and trying new things
;; i'm finally seriously starting _mastering emacs_ and as the author
;; recommends it, i'm going back to a very minimal starting 'init.el'
;; and adding stuff as he discusses it or if i feel a strong need.
;;
;; i'm keeping most real 'init' configuration in this file and in
;; (ugh) 'custom.el'. i get why he says to use the customization
;; interface, but i don't like it. it's good for discovery, but i
;; don't think its maintainable.


;;; Code:


(global-unset-key (kbd "C-x C-c"))

(when (< emacs-major-version 29)
  (error "This configuration requires Emacs 29 or newer!"))


(setopt user-full-name "Troy Brumley")
(setopt user-mail-address "BlameTroi@gmail.com")
(setopt auth-sources '("~/.authinfo.gpg"))
(setopt auth-source-cache-expiry nil)
(setopt initial-scratch-message "
;;; so let it be written,
;;; so let it be done.

;; useful to remember
;;
;; customize-customized -- options and faces changed but not yet saved
;;
;; customize-saved      -- displays all saved options and faces
;;
;; customize-mode       -- for the active major mode

;; subword-mode         -- deals with CamelCase word movement
;;
;; superword-mode       -- deals with snake_case word movement
;;
;; both of the above can be made global by customize-option global-xxx-mode

;; https://gitlab.com/mp81ss_repo/gendoxy if i want to try to use doxygen

;; currently the customization interface is live and changes are loaded
;; at the end of 'init.el'.

")



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packaging and repositories
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use-package is now part of standard emacs. it can be updated from
;; gnu elpa, as can several other built in packages. while use-package
;; is a built in, you have to require it for some of the macro
;; keywords to process.

(eval-when-compile
  (require 'use-package))
(setopt load-prefer-newer t)
(setopt use-package-always-ensure t)

;; if native compilation is available, go ahead and use it. my current
;; (30.0.90) mac build does not have native compile built in.

(if (and (fboundp 'native-compile-available-p)
         (native-compile-available-p))
    (setopt package-native-compile t))

;; gnu and nongnu elpa repositories are available by default. add
;; melpa-stable and melpa but prioritize them below gnu and nongnu.
;; it is also probably a good idea to pin some packages to specific
;; repositories--suspenders and belt.

(with-eval-after-load 'package
  (add-to-list
   'package-archives
   '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/") t)
  (setopt package-archive-priorities
        '(("gnu" . 10)
          ("nongnu" . 9)
          ("melpa-stable" . 8)
          ("melpa" . 5)))
  (setopt package-install-upgrade-built-in t))

;; this is a good place to make sure these are done.

(use-package diminish
  :demand t)

(use-package bind-key
  :demand t)

(use-package which-key
  :diminish
  :init (which-key-mode));

(use-package free-keys
  :diminish
  :pin nongnu)



;; ;;;;;;;;;;;
;; os specific
;; ;;;;;;;;;;;

;; macos is a horse of an entirely different color! these are those
;; things that are mac specific. the key remaps for various keys that
;; the mac desktop wants are a work in progress.  i don't gate these
;; by operating system, but they could be if needed.

;; shell path isn't always included on mac os x.

(use-package exec-path-from-shell
  :demand t)

(setopt exec-path-from-shell-arguments nil)
(exec-path-from-shell-initialize)

;; remap modifier keys.
;;
;; in addition to changing caps lock to control, a standard (non mac)
;; keyboard has on the bottom row control fn os alt |spacebar| alt(gr)
;; os menu control
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
;;
;; (setopt ns-alternate-modifier 'alt)
;; (setopt ns-command-modifier 'meta)
;; (setopt ns-function-modifier 'hyper)
;; (setopt ns-right-alternate-modifier 'super)

;; hiding the menu bare merely dims it, its space appears to always
;; be allocated with the notch, so there's no pressing need to do this
;; unless you find the menu distracting. i do not.
;;
;; (setopt ns-auto-hide-menu-bar t)


;; ;;;;;;;;;;;;;;;;;;;
;; touchpad touchiness
;; ;;;;;;;;;;;;;;;;;;;

;; i've turned this off as i've learned the right settings to work
;; with in mac system settings, but leaving it for documentation.
;;
;; a rather heavy handed (but working) way to stop the mac touchpad
;; from moving things on me. i tried to find way to do this as a doom
;; after! but the double and triple variants kept being active. yes, i
;; searched the source. no, i couldn't find where that was done. the
;; customize interface isn't showing me these options in any way that
;; i understand. the goal here is to prevent my ham handed taps and
;; brushes of the touchpad from moving stuff around. i have mixed
;; feelings about drag-the-scrollbar mouse scrolling, but i don't like
;; the mouse wheel in text editing.
;;
;; (add-to-list
;;  'emacs-startup-hook
;;  (lambda ()
;;    (global-set-key [wheel-up] 'ignore)
;;    (global-set-key [double-wheel-up] 'ignore)
;;    (global-set-key [triple-wheel-up] 'ignore)
;;    (global-set-key [wheel-down] 'ignore)
;;    (global-set-key [double-wheel-down] 'ignore)
;;    (global-set-key [triple-wheel-down] 'ignore)
;;    (global-set-key [wheel-left] 'ignore)
;;    (global-set-key [double-wheel-left] 'ignore)
;;    (global-set-key [triple-wheel-left] 'ignore)
;;    (global-set-key [wheel-right] 'ignore)
;;    (global-set-key [double-wheel-right] 'ignore)
;;    (global-set-key [triple-wheel-right] 'ignore)
;;    (mouse-wheel-mode -1)
;;    (message "trackpad stuff set to ignore")))

(mouse-avoidance-mode 'exile)



;; ;;;;;;;;;;;;;;;;;;
;; directories, paths
;; ;;;;;;;;;;;;;;;;;;

;; my additional elisp that doesn't need to be right in the init
;; file. this is for work in progress, things that i might autoload,
;; and things that aren't in (m)elpa.

(add-to-list
 'load-path
 (concat user-emacs-directory "troi-lisp"))

;; backups are a pain in the ass. sure, they are needed but let's
;; segregate them by collecting them in one place

(defun troi-backup-file-name (fpath)
  "Return a new file path of FPATH, creating directories if needed."
  (let* ((backupRootDir "~/.tmp/emacs-backup/")
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir fpath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'troi-backup-file-name)

;; one last chance to un-delete a file.

(setopt delete-by-moving-to-trash t)



;; ;;;;;;;;;;;;;;;;;;;;;
;; dired and directories
;; ;;;;;;;;;;;;;;;;;;;;;

;; use gls if it's around. the mac supplied ls doesn't suppport all
;; the options dired wants.

(when (executable-find "gls")
  (setopt insert-directory-program "gls"))

;; the number of times i've wantd a brief directory list instead of
;; dired is zero.

(global-set-key (kbd "C-x C-d") 'dired)



;; ;;;;;;;;;;;;;;;;;;;
;; visuals and helpers
;; ;;;;;;;;;;;;;;;;;;;

;; line numbers for programming modes.

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 4)

;; text modes wrap so use visual-line-mode.

(add-hook 'text-mode-hook 'visual-line-mode)

;; highlight the current line.

(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; i often use C-l for visual breaks

(use-package form-feed-st
  :defer t
  :diminish
  :hook (prog-mode . form-feed-st-mode) (text-mode . form-feed-st-mode))

;; column number mode is a must
(column-number-mode)



;; ;;;;;;;;;;;;;;
;; ibuffer & fido
;; ;;;;;;;;;;;;;;

(global-set-key [remap list-buffers] 'ibuffer)
(fido-mode)



;; ;;;;;;;;;;;;;;;;;;;;;;;
;; these hippies don't lie
;; ;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [remap dabbrev-expand] 'hippie-expand)


;; ;;;;;;;;;;;;;;;;;;
;; windowing and tabs
;; ;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-o") 'other-window)
(global-tab-line-mode)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming mode configuration and helpers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; indenting, formatting, and navigation. i have reverted to cc-mode
;; instead of the various c-ts modes. i can always go back to ts if i
;; feel a need.

;; use astyle to do formatting for c. i have an .astylerc set up with
;; options that match troi-c-style.

(use-package reformatter)

(use-package astyle
  :after reformatter
  :when (executable-find "astyle")
  :diminish (astyle-on-save-mode . "as")
  :hook
  (c-ts-mode . astyle-on-save-mode)
  (c++-ts-mode . astyle-on-save-mode))

;; discoverability via go to definition/references and xref seems to
;; work best with eglot instead of the various tagging options.  less
;; configuration and better dwim.

(use-package eglot
  :pin gnu
  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c r" . eglot-rename))
  :custom
  ;; log size 0 disables logging which improves performance
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider)))

;; configure clangd for eglot to my preferences. i was able to avoid
;; the maze of (apparently) cmake generated files for clangd with these
;; options.

(with-eval-after-load 'eglot
  (setopt completion-category-defaults nil)
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "-j=4"
                    "--log=error"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0"))))

;; flymake isn't too intrusive, so i'm adding it for programming modes
;; that i use enough to warrant it.

(use-package flymake
  :pin gnu
  :hook
  (c-ts-mode . flymake-mode)
  (c++-ts-mode . flymake-mode)
  (emacs-lisp-mode . flymake-mode)
  :custom (flymake-mode-line-lighter "FM")
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics)))

;; this is needed to avoid false 'can not find/load' errors on
;; requires that occure before this point in the source.

(with-eval-after-load 'flymake
  (setopt elisp-flymake-byte-compile-load-path load-path))

;; this is really C specific, 3 is confusing, 1 and 2 are unhelpful.

(setopt treesit-font-lock-level 4)

;; use treesitter for c and c++. make sure the grammars are built.

(setopt major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)))

;; it is unclear how much or how little of the cc-mode variables
;; carry forward into treesitter. keeping the various settings
;; for now, but using c(++)-ts-mode, it seems to play better with
;; flymake and tooltips.

(require 'troi-c-style)
(add-hook 'c-mode-common-hook 'troi-set-c-style)
(add-hook 'c-ts-mode 'troi-set-c-style)

(setopt c-basic-offset 8)
(setopt c-default-style "linux")
(setopt c-ignore-auto-fill nil)
(setopt c-mark-wrong-style-of-comment t)
(setopt c-require-final-newline nil)
(setopt c-ts-mode-indent-offset 8)
(setopt c-ts-mode-indent-style 'linux)


;; i keep thinking i should use the doxygen comment format even if
;; nothing i am doing needs a full doxygen treatment. i found this
;; as a working template generator. i may or may not use it, but
;; leaving it on for now. it isn't in melpa, so i downloaded the
;; source instead of doing use-package against git.

(add-to-list
 'load-path
 (concat user-emacs-directory "troi-lisp/gendoxy-v1.0.13"))
(require 'gendoxy)



;; ;;;;;;;;;;;;;;;;;;
;; assorted utilities
;; ;;;;;;;;;;;;;;;;;;

;; screen shots from emacs

;; (use-package emacsshot
;;   :pin melpa-stable
;;   :bind
;;   ("M-S f" . emacsshot-snap-frame)
;;   ("M-S w" . emacsshot-snap-window-include-modeline))

;; the above is out of date, convert is deprecated.
;; WARNING: The convert command is deprecated in IMv7, use "magick" instead of "convert" or "magick convert"
;; doesn't work without xquartz or similar ... commenting out for now.
;; (use-package emacsshot
;;   :vc (:url "https://github.com/BlameTroi/emacsshot.git"
;; 	    :rev :newest)
;;   :bind
;;   ("M-S f" . emacsshot-snap-frame)
;;  ("M-S w" . emacsshot-snap-window-include-modeline))



(global-set-key "\M-z" 'zap-up-to-char)      ;; this is the way


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; easy customization interface
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; i'm not a fan of splitting things between customization and the
;; declarative files such as 'init.el'. i really hate the default
;; behavior of the customization interface tacking stuff on the tail
;; end of 'init.el'. setting a specific custom file name will have any
;; customizations logged into 'custom.el'.
;;
;; i'm currently using customization as i work through _mastering emacs_
;; but i suspect i'll mine the and put them in 'init.el' and stop loading
;; 'custom.el', but for now do this at the end of 'init.el'.

(setopt custom-file (concat user-emacs-directory "custom.el"))

(load custom-file)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes and colors and related visuals
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fix up some bits of the theme, the background color is washed out
;; if i don't reload it here.

;; (use-package acme-theme
;;   :config
;; ;;  (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme 'acme t)
;;   (setopt acme-theme-black-fg t)
;;   (custom-set-faces
;;    '(hl-line ((t (
;;                   :inherit highlight
;;                   :extend t
;;                   :background "LightGoldenrod2"
;;                   :foreground "black"))))
;;    ))

(use-package tomorrow-night-deepblue-theme
  :pin melpa
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'tomorrow-night-deepblue t))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((emacsshot :url "https://github.com/BlameTroi/emacsshot.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
