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

;; emacs regexp cheatsheet
;; https://www.emacswiki.org/emacs/RegularExpression

;; currently the customization interface is live and changes are loaded
;; at the end of 'init.el'.

")



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packaging and repositories
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; while use-package is a built in, you have to require it for some of
;; the macro keywords to process. (this may no longer be needed, but
;; it doesn't seem to hurt so i'll leave it in.)

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
;;
;; experimenting with using 'package-install-upgrade-built-in'
;; didn't like it.

(with-eval-after-load 'package
  (defvar package-archives)
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
          ("melpa" . 5))))

(use-package diminish)
(use-package bind-key)
(use-package free-keys)
(use-package which-key
  :diminish
  :init (which-key-mode));



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple options and one off things
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key "\M-z" 'zap-up-to-char)

(setopt apropos-sort-by-scores t)
(setopt blink-matching-delay 0.1)
(setopt global-auto-revert-mode t)
(setopt save-place-mode t)
(setopt delete-by-moving-to-trash t)

(setopt switch-to-buffer-obey-display-actions t)
(setopt help-window-select t)
(setopt enable-recursive-minibuffers t)

(savehist-mode)

;; dired droppings
(setopt dired-kill-when-opening-new-dired-buffer t)


;; try using new frames instead of tabs

;; (global-tab-line-mode)

;; an experiment
(when (display-graphic-p)
  (context-menu-mode))

(setopt scroll-bar-mode 'right)
(setopt scroll-conservatively 10000)
(setopt sentence-end-double-space nil)

(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'which-function-mode)
(setopt display-line-numbers-width 4)
(setopt mode-line-position-column-line-format '(" (%l,%C)"))

;; i am not liking visual-line-mode for program text. my text
;; mode work is also usually not wanting visual-line-mode so
;; we'll turn this off in the init and turn it on manually
;; for a while.
;; (add-hook 'text-mode-hook 'visual-line-mode)
;; the system default is to not truncate so
(setopt truncate-lines t)

(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; i often use C-l for visual breaks

(use-package form-feed-st
  :diminish
  :hook (prog-mode . form-feed-st-mode) (text-mode . form-feed-st-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dot-mode brings the vim '.' to emacs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; if i end up liking this i'll want to customize the key binds, C-M-.
;; is bound to (xref-find-apropos PATTERN) and i can see wanting to
;; use that.

;; should this be a toggle instead of global? i'm not sure, but
;; (dot-mode) will toggle.

(use-package dot-mode
  ;; :diminish
  :init
  (declare-function global-dot-mode "dot-mode")
  (global-dot-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mac os specific changes
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mac os is a horse of an entirely different color! these are those
;; things that are mac specific. the key remaps for various keys that
;; the mac desktop wants are a work in progress. i don't gate these by
;; operating system, but they could be if needed.

(use-package exec-path-from-shell
  :init
  (declare-function exec-path-from-shell-initialize "exec-path-from-shell" ())
  (declare-function exec-path-from-shell-copy-envs "exec-path-from-shell")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH")))

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
;; be allocated due to the notch.

(setopt ns-auto-hide-menu-bar t)

;; ;;;;;;;;;;;;;;;;;;;
;; touchpad touchiness
;; ;;;;;;;;;;;;;;;;;;;

;; i've turned this off as i've learned the right settings to work
;; with in mac system settings, but leaving it for documentation.
;;
;; even after that i still brush too often. disabling everything but
;; mwheel-scroll.
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
(setopt mouse-avoidance-banish-position
   '((frame-or-window . frame) (side . right) (side-pos . 1)
     (top-or-bottom . bottom) (top-or-bottom-pos . 10)))


;; use gls if it's around. the mac supplied ls doesn't suppport all
;; the options dired wants.

(when (executable-find "gls")
  (setopt insert-directory-program "gls"))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directories, paths, file system stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my additional elisp that doesn't need to be right in the init
;; file. this is for work in progress, things that i might autoload,
;; and things that aren't in (m)elpa.

(add-to-list
 'load-path
 (concat user-emacs-directory "troi-lisp"))

;; backups are a pain in the ass. sure, they are needed but let's
;; segregate them by collecting them in one place

(defun troi/backup-file-name (fpath)
  "Return a new file path of FPATH, creating directories if needed."
  (let* ((backup-root-dir "~/.tmp/emacs-backup/")
         (backup-file-path (replace-regexp-in-string "//" "/" (concat backup-root-dir fpath "~") )))
    (make-directory (file-name-directory backup-file-path) (file-name-directory backup-file-path))
    backup-file-path))
(setopt make-backup-file-name-function 'troi/backup-file-name)



;; an idea from superuser to deal with fat fingering in dired
;; and opening executables or objects files.
;;
;; a better idea would be to use dired-open package from dired-hacks
;; and then check the file via "file -b --mime-type <filename>" which
;; can be used to filter out application/x-mach-binary and other types
;; and prompt to verify the file should be opened in emacs.
;;
;; https://superuser.com/questions/373942/how-to-stop-emacs-from-opening-binary-files/373943#373943
;;
;;(defvar troi/find-file-check-source-extensions
;;  '(".cpp" ".cc" ".c" ".h" ".f90" ".s" ".S"))
;; (defun troi/ad-find-file-read-args (:after my-find-file-read-args-check-source)
;;   (let* ((filename (car ad-return-value))
;;          (source-filename
;;           (catch 'source-file-exists
;;             (mapc (lambda (ext)
;;                     (let ((source-filename (concat filename ext)))
;;                       (when (file-exists-p source-filename)
;;                         (throw 'source-file-exists source-filename))))
;;                   my-find-file-check-source-extensions)
;;             nil)))
;;     (and source-filename
;;          (not (y-or-n-p (format "Source file %s detected. Are you sure you want to open %s? " source-filename filename)))
;;          (error "find-file aborted by user"))))
;; (ad-activate 'find-file-read-args)



;; ;;;;;;
;; frames
;; ;;;;;;

;; from https://stackoverflow.com/a/57318988 how to move a buffer to a
;; new frame. tear-off-window is usually bound to a mouse button but
;; i'm not a heavy mouse user so this function should do the job.

;; it gets an error if the window is the sole window and doesn't
;; actually close the old window, but that's probably because i'm
;; trying to use tab-line-mode, but i'm starting to think that new
;; frames fit my workflow better than tabs.

;; i'll roll back tabs and see how i like using macos desktops
;; instead.

(defun troi/tear-off-window ()
  "Delete the selected window, and create a new frame displaying its buffer."
  (interactive)
  (let* ((window (selected-window))
     (buf (window-buffer window))
     (frame (make-frame)))
    (select-frame frame)
    (switch-to-buffer buf)
    (delete-window window)))

(bind-key "C-x 5t" #'troi/tear-off-window)



;; ;;;;;;;;
;; movement
;; ;;;;;;;;

(use-package avy
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))



;; ;;;;;;;;;;
;; completion
;; ;;;;;;;;;;

;; (setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
;; (setopt tab-always-indent 'complete)
;; (setopt completion-styles '(basic initials substring))
(setopt completion-auto-help 'always)
(setopt completions-max-height 15)
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completions-group-sort 'alphabetical)

;; tab more like shell
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)    ; was occur regexp
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  (setopt consult-narrow-key "<"))

(use-package vertico
  :ensure t
  :init
  (declare-function vertico-mode "vertico")
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :ensure t
  :config
  (declare-function marginalia-mode "marginalia")
  (marginalia-mode))

(use-package corfu
  :ensure t
  :init
  (declare-function global-corfu-mode "corfu")
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; i don't run in terminal, so the terminal popup support isn't needed

;; completion at point, highly configurable, this is minimal
(use-package cape
  :ensure t
  :init
  (declare-function cape-dabbrev "cape")
  (declare-function cape-file "cape")
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; (use-package orderless
;;   :ensure t
;;   :config
;;   (setq completion-styles '(orderless)))



;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setopt wgrep-auto-save-buffer t))


;; ;; ;;;;;;
;; ;; eshell
;; ;; ;;;;;;
;; (use-package eshell
;;   :init
;;   (defun bedrock/setup-eshell ()
;;     ;; Something funny is going on with how Eshell sets up its keymaps; this is
;;     ;; a work-around to make C-r bound in the keymap
;;     (keymap-set eshell-mode-map "C-r" 'consult-history))
;;   :hook ((eshell-mode . bedrock/setup-eshell)))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming mode configuration and helpers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; trying to get lsp for typescript working and this is helpful. i am
;; explicit about the languages here even though that's most of the
;; languages in the default value for 'treesit-auto-recipe-list' to
;; document where things come from.

;; i can not figure out why the 'declare-function' for
;; 'treesit-auto-add-to-auto-mode-alist' doesn't suppress the
;; "function might not be defined at run time" warning. it works fine
;; for 'global-treesit-auto-mode-alist' from the same file.

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (declare-function treeset-auto-add-to-auto-mode-alist "treesit-auto" t t)
  (treesit-auto-add-to-auto-mode-alist
   '(bash c commonlisp cpp go html java javascript json make markdown org python ruby toml typescript yaml))
  (declare-function global-treesit-auto-mode "treesit-auto")
  (global-treesit-auto-mode))


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
;; requires that occur before this point in the source.

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

;;(require 'troi-c-style)
;;(add-hook 'c-mode-common-hook 'troi-set-c-style)
;;(add-hook 'c-ts-mode 'troi-set-c-style)

(setopt c-ts-mode-indent-offset 8)
(setopt c-ts-mode-indent-style 'linux)

;;(setopt c-basic-offset 8)
;;(setopt c-default-style "linux")

;;(setopt c-ignore-auto-fill nil)
;;(setopt c-mark-wrong-style-of-comment t)
;;(setopt c-require-final-newline nil)


;; imenu-list
(use-package imenu-list
  :diminish
  :config
  (setopt imenu-list-focus-after-activation t)
  (setopt imenu-list-auto-resize t)
  :bind ("C-'" . imenu-list-smart-toggle))


;; side-notes
(use-package side-notes
  :diminish
  :bind ("M-s n" . side-notes-toggle-notes)
  :custom
  (side-notes-file "side-notes.txt")
  (side-notes-secondary-file "~/general-side-notes.txt"))


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
;; '(highlight ((t (:background "#00546e" :inverse-video nil))))
;; '(hl-line ((t (:inherit highlight :extend t :background "LightGoldenrod2" :foreground "black")))))

(use-package tomorrow-night-deepblue-theme
  :pin melpa
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'tomorrow-night-deepblue t))


(use-package nerd-icons)
(use-package nerd-icons-dired
  :after nerd-icons
  :diminish
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :after nerd-icons marginalia
  :diminish
  :config
  (declare-function nerd-icons-completion-mode "nerd-icons-completion")
  (nerd-icons-completion-mode)
  (declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after nerd-icons corfu
  :diminish
  :config
  (declare-function nerd-icons-corfu-formatter "nerd-icons-corfu")
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-ibuffer
  :after nerd-icons
  :diminish
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ;;;;;;;;
;; dired hacks
;; ;;;;;;;;

(use-package dired-gitignore
  :init
  (dired-gitignore-global-mode)
  :bind (:map dired-mode-map
	      ("h" . dired-gitignore-global-mode)))


(provide 'init)
;;; init.el ends here
