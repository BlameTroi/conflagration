;;; init.el --- troy brumley's init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;;
;;; Commentary:
;;;

;; just an 'init.el' file.
;;
;; i'm keeping most real 'init' configuration in this file, but some
;; leaks into (ugh) 'custom.el', but i don't like it. it's good for
;; discovery, but i don't think its maintainable.
;;
;; i expect to periodically go through 'custom.el' and review changes
;; and pull them into this init if they are worth keeping.


;;;
;;; Code:
;;;



;;;
;;; system compatability checks
;;;

;; i don't move my init around to foreign systems that don't have
;; current emacs builds, and i always run with the gui.

(when (< emacs-major-version 29)
  (error "This configuration requires Emacs 29 or newer!"))

(when (not (display-graphic-p))
  (message "This configuration assumes you are running a GUI Emacs, some things may break.")
  (sleep-for 5))



;;;
;;; use-package and (m)elpa
;;;

(eval-when-compile
  (require 'use-package))
(setopt load-prefer-newer t)
(setopt use-package-always-ensure t)


(if (and (fboundp 'native-compile-available-p)
         (native-compile-available-p))
    (setopt package-native-compile t))


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
            ("melpa-stable" . 6)
            ("melpa" . 5))))


;; sometimes a build of emacs doesn't grab the environment correctly,
;; especially when run from a shortcut on macos. on the mac the
;; application bundle has a plist that lists environment variables
;; to copy. the only one in the current 'Info.plist' i care about
;; is 'PATH'.
;;
;; therefore, always get the variables you want copied in using
;; exec-path-from-shell.

(use-package exec-path-from-shell
  :config
  (declare-function
   exec-path-from-shell-initialize "exec-path-from-shell" ())
  (declare-function
   exec-path-from-shell-copy-envs "exec-path-from-shell")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("LIBRARY_PATH"
     "INFOPATH"
     "CPATH"
     "MANPATH"
     "MallocNanoZone"
     "CMAKE_GENERATOR"
     "CDPATH"
     "ODIN_ROOT")))



;;;
;;; sane defaults and establish some paths.
;;;

;; no-littering should come as early as possible. as this is a single user
;; emacs, i'm not feeling a need for lockfiles. it is possible to configure
;; no-littering by changing the user-emacs-directory, but i'm not doing
;; that yet. there is a full migration guide but let's just run like this
;; for now to see what it does.
;;
;; https://github.com/emacscollective/no-littering/blob/main/migrate.org

(use-package no-littering
  :init
  (require 'recentf)
  (require 'no-littering)
  (setopt make-backup-files nil)
  (setopt auto-save-default nil)
  (setopt create-lockfiles nil)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package recentf
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :init
  (recentf-mode))

;; this is usually empty.

(add-to-list
 'load-path
 (concat user-emacs-directory "troi-lisp"))


;; not a dashboard.

(if (file-exists-p
     (concat user-emacs-directory
	     "initial-scratch-message.txt"))
    (with-current-buffer (get-buffer "*scratch*")
      (delete-region (point-min) (point-max))
      (insert-file-contents (concat user-emacs-directory
				    "initial-scratch-message.txt"))
      (set-buffer-modified-p nil))
  (setopt initial-scratch-message ";; nothing to see here, move along"))


;;;
;;; visuals
;;;

;; pulling out of custom.el to control here:

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 190)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height 190)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 230 :weight 'medium)


;; Choose some fonts
;; (set-face-attribute 'default nil :family "Iosevka")
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")



;; ??? probably don't need these two lines.
;; (set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8)

(when (display-graphic-p)
  (context-menu-mode))

(when scroll-bar-mode
  (scroll-bar-mode -1))


(column-number-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 4)
(setopt mode-line-position-column-line-format '(" (%l,%C)")) ; %C based 1, %c based 0

;; display function name in mode line

(add-hook 'prog-mode-hook 'which-function-mode)


;; hilight cursor line

(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


;; visual line mode is ok for text, use (truncate-lines t) for code.

(add-hook 'text-mode-hook 'visual-line-mode)


;; Auto parenthesis matching

(add-hook 'prog-mode-hook 'electric-pair-mode)


;; stragglers

(setopt ns-auto-hide-menu-bar t)           ; this gains no space on displays with the notch
(setopt use-dialog-box nil)
(setopt apropos-sort-by-scores t)
(setopt blink-matching-delay 0.1)
(setopt standard-indent 8)


;;;
;;; metadata
;;;

(setopt user-full-name "Troy Brumley")
(setopt user-mail-address "BlameTroi@gmail.com")
(setopt auth-sources '("~/.authinfo.gpg"))
(setopt auth-source-cache-expiry nil)


;;;
;;; nicer scrolling
;;;

(setopt scroll-margin 0)
(setopt scroll-conservatively 100000)
(setopt scroll-preserve-screen-position 1)
(pixel-scroll-precision-mode)                         ; Smooth scrolling


;;;
;;; miscellany
;;;

(setopt sentence-end-double-space nil)
(setopt require-final-newline t)

(setopt switch-to-buffer-obey-display-actions t)
(setopt help-window-select t)
(setopt help-wndow-keep-selected t)
(setopt enable-recursive-minibuffers t)


(delete-selection-mode +1)
(indent-tabs-mode +1)

(setopt tab-always-indent 'complete)

(setopt comment-empty-lines t)
;; (setopt comment-style 'extra-line)

(setopt confirm-kill-emacs 'y-or-n-p)


;;;
;;; restore useful but hidden commands
;;;

;; 'put' is used because these are properties of the function
;; name symbol.

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)



;;;
;;; global (re)keybinds
;;;

;; grouping keybindings with their associated 'use-package' or
;; 'require' makes a lot of sense, but some global bindings exist.
;; most of the following are remappings to change the behavior of a
;; key. for example, C-s will 'isearch-forward-regexp' instead of the
;; usual 'isearch-forward'


;; this makes TAB in the minibuffer behave more like it does in a
;; shell.

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;; Make ESC quit prompts

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; on the mac s-q is the command-Q equivalent. use it to close
;; emacs when not using M-x 'save-buffers-kill-emacs'.

(global-unset-key (kbd "C-x C-c"))


;; the number of times i want the list instead of the smarter
;; ui mode is zero.

(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; default search to regexp instead of string.

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)


;;;
;;; movementish things
;;;

;; to not through!

(global-set-key "\M-z" 'zap-up-to-char)


;; 'dot-mode' brings the vim '.' to emacs. this has a keybind of
;; C-M-. which conflicts with xref-find-apropos.

(use-package dot-mode
  :diminish
  :init
  (declare-function global-dot-mode "dot-mode")
  (global-dot-mode t))


;;;
;;; jumping
;;;

(use-package avy
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

(use-package ace-window
  :after avy
  :bind (("C-x o" . ace-window)
	 ("M-o" . ace-window)))



;;;
;;; the small and obvious packages that don't need much configuration
;;; beyond being included.
;;;

(use-package diminish)

(use-package free-keys)

(use-package which-key
  :ensure nil
  :diminish
  :config (which-key-mode))

(use-package bind-key)

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

(use-package savehist
  :ensure nil
  :config
  (setopt savehist-additional-variables
          '(compile-command
            kill-ring
            regexp-search-ring))
  :hook (after-init . savehist-mode))

(use-package saveplace
  :config
  (save-place-mode)
  :custom
  (save-place-limit 1000))

(use-package autorevert
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-avoid-polling t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;; i often use C-l for visual breaks.

(use-package form-feed-st
  :diminish
  :hook
  (prog-mode . form-feed-st-mode)
  (text-mode . form-feed-st-mode))


;; no touchpad
;; this causes problems if you want to use the system menu bar
;; (use-package disable-mouse
;;   :config
;;   (global-disable-mouse-mode)
;;   :custom
;;   (disable-mouse-mode-lighter "")
;;   (global-disable-mouse-mode-lighter ""))



;;;
;;; built ins that need more configuration
;;;


;; in case i don't want .git to mark.

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".projectile" ".project.el" "fpm.toml")))


;; for dired, use 'gls' if it's available. the default 'ls' in macos
;; and some other systems doesn't support all the options that 'dired'
;; wants.

(use-package dired
  :after exec-path-from-shell
  :ensure nil
  :commands (dired)
  :config
  (setopt
   dired-recursive-copies  'always
   dired-isearch-filenames 'dwim)
  (when (executable-find "gls")    ; use GNU ls
    (setopt dired-use-ls-dired nil)
    (setopt ls-lisp-use-insert-directory-program t)
    (setopt insert-directory-program "gls")
    (setopt dired-listing-switches "-alh --group-directories-first"))
  (setopt dired-recursive-copies 'always)
  (setopt dired-recursive-deletes 'always)
  (setopt dired-kill-when-opening-new-dired-buffer t)
  (setopt delete-by-moving-to-trash t)
  (setopt dired-dwim-target t)
  (setopt dired-auto-revert-buffer t)
  (setopt dired-do-revert-buffer t))


(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; this is from prot, but i name stuff i used with troi/
(defun troi/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'troi/keyboard-quit-dwim)


;; documentation with 'info' and 'eldoc'. for some reason i'm missing
;; system info from homebrew. i should probably move this into my
;; zshenv.

(use-package info
  :after exec-path-from-shell
  :custom
  (Info-additional-directory-list '("/opt/homebrew/share/info")))

(use-package eldoc
  :ensure nil
  :diminish
  :config (global-eldoc-mode))


;;;
;;; oddities with more complex configuration
;;;


;; 'so-long' handles long lines that are usually found in program
;; source code where unneeded whitespace has been removed. forcing
;; paragraph text direction is reported to also help by removing the
;; checks and scans for right to left languages.

(use-package so-long
  :config
  (global-so-long-mode)
  :custom
  (bidi-paragraph-direction 'left-to-right))


;; 'imenu-list' puts 'imenu' in a sidebar.

(use-package imenu-list
  :diminish
  :bind ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))


;; 'dumb-jump' is a dwim for 'xref'.

(use-package dumb-jump
  :hook
  (xref-backend-functions . dump-jump-xref-activate))



;;;
;;; completion
;;;


;; see bedrock and https://www.masteringemacs.org/article/understanding-minibuffer-completion


;; miscellaneous options

(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-styles '(prescient)) ; basic initials substring)) ; might want flex?
(setopt completion-auto-help 'always)
(setopt completions-max-height 10)
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)
;; check help for completion-auto-select
(setopt completion-auto-select t)


;; vertical interactive completion

(use-package vertico
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))


;; add descriptive notes 'in the margin' of various lists/uis.

(use-package marginalia
  :config
  (marginalia-mode))


;; completion in region functions

(use-package corfu
  :commands
  (corfu-mode global-corfu-mode)

  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  ;; :custom
  ;; hide commands in m-x which do not apply to the current mode.
  ;; (read-extended-command-predicate #'command-completion-default-include-p)
  ;; disable ispell completion function. as an alternative try `cape-dict'.
  ;; (text-mode-ispell-word-completion nil)
  ;; (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))


;; completion at point extensions

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))


;; add kind of item icons to marginalia notes (folder, file, etc).

(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; let's try orderless again

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

;; and now let's try prescient

(use-package prescient
  :config
  (prescient-persist-mode))
(use-package corfu-prescient
  :after corfu
  :after prescient
  :config
  (corfu-prescient-mode))
(use-package vertico-prescient
  :after vertico
  :after prescient
  (vertico-prescient-mode))



;;;
;;; templating via yasnippet
;;;

;; yasnippet -capf -classic-snippets -snippets yatemplate license-snippets gitignore-snippets

(use-package git-modes)

(use-package yasnippet-snippets)

(use-package yasnippet-classic-snippets)

(use-package license-snippets)

(use-package gitignore-snippets)

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
	  conf-mode
	  gitignore-mode
          snippet-mode) . yas-minor-mode-on))
  ;; :init
  ;; (setq yas-snippet-dir "~/.emacs.d/snippets"))


(use-package yasnippet-capf
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


;;;
;;; programming modes and supporting packages that require
;;; no or light configuration.
;;;

(use-package git-modes)

(use-package magit)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package cmake-mode)

(use-package ninja-mode)

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :config
  (add-to-list
   'treesit-language-source-alist
   '(markdown
     "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     "split_parser"
     "tree-sitter-markdown/src"))
  (add-to-list
   'treesit-language-source-alist
   '(markdown-inline
     "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     "split_parser"
     "tree-sitter-markdown-inline/src")))

;;
;; experimenting with odin
;;
(require 'odin-mode)

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode)
  :custom
  (hl-todo-require-punctuation t)
  (hl-todo-highlight-punctuation ":")
  ;; these are recommended commands and bindings, need to check
  ;; for conflicts and use-packagify
  ;; (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  ;; (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
  ;; (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
  ;; (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert)
  )



;;;
;;; programming mode packages that require more complex configuration.
;;;


;; treesitter is superior to pattern based modes, use it when it is
;; available. use treesit-auto to build grammars when needed.

(setopt treesit-font-lock-level 4)           ; levels 1-3 are useless
(setopt c-ts-mode-indent-offset 8)           ; turns out i like tabs, who knew?
(setopt c-ts-mode-indent-style 'linux)
(setopt standard-indent 8)


;; auto-hide hides function bodies in some programming modes.
;; my fork includes c-ts-mode, a one liner in spirit.

(use-package auto-hide
 :vc
 (auto-hide :url "https://github.com/BlameTroi/auto-hide.el"
            :branch "main")
 :hook (c-ts-mode . hs-minor-mode)
 :config
 (global-auto-hide-mode))


;; C-c C-c for comment region is redundant with M-;

(require 'c-ts-mode)
(keymap-unset c-ts-base-mode-map "C-c C-c")


;; some of these might require M-x treesit-install-language-grammar

(setopt major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
	  (c-mode . c-ts-mode)
	  (c++-mode . c++-ts-mode)
	  (c-or-c++-mode . c-or-c++-ts-mode)
	  (ruby-mode . ruby-ts-mode)))


(use-package treesit-auto
  :after exec-path-from-shell
  :custom
  (treesit-auto-install 'prompt)
  :config
  (declare-function treeset-auto-add-to-auto-mode-alist "treesit-auto" t t)
  (treesit-auto-add-to-auto-mode-alist
   '(bash
     c
     commonlisp
     cpp
     go
     html
     java
     javascript
     json
     make
     markdown
     org
     python
     ruby
     toml
     typescript
     yaml))
  (declare-function global-treesit-auto-mode "treesit-auto")
  (global-treesit-auto-mode))


;; use 'astyle' to do formatting for c. see '.astylerc'. my style is
;; based on linux and k&r.

(use-package reformatter
  :after exec-path-from-shell)

(use-package astyle
  :after reformatter
  :when (executable-find "astyle")
  :diminish (astyle-on-save-mode . "as")
  :hook
  (c-ts-mode . astyle-on-save-mode)
  (c++-ts-mode . astyle-on-save-mode))


;; eglot for lsp support is much better than tag based solutions.

(use-package eglot
  :after exec-path-from-shell

  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (odin-mode . eglot-ensure)

  :bind (:map eglot-mode-map
	      ;;("C-c t" . troi/clear)
              ("C-c c a" . eglot-code-actions)
              ("C-c c r" . eglot-rename))

  ;; if debugging 'eglot' issues, comment out the fset
  ;; and events-buffer-config lines.
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (setopt jsonrpc-event-hook nil)
  :custom
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)  ; Prevent minibuffer spam
  (eglot-ignored-server-capabilities '(:documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider))
  )

;; configure the 'clangd' language server to my preferences. 'clangd'
;; will need 'CMakeLists.txt' and 'compile_commands.json' in each
;; project's root directory.

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '((c-ts-mode c++-ts-mode)
     . ("clangd"
        "-j=4"                   ; async index threads
	"--log=info"             ;"error" or "verbose"
	"--pch-storage=memory"   ; i have plenty
	"--enable-config"))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "--log=error"						 ;;
;; "--background-index"						 ;;
;; "--clang-tidy"                ; but i use 'astyle' to format. ;;
;; "--completion-style=detailed"				 ;;
;; "--pch-storage=memory"					 ;;
;; "--header-insertion=never"					 ;;
;; "--header-insertion-decorators=0"))))			 ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '(odin-mode . ("ols"))))

;; (require 'eglot-java)
;; (add-hook 'java-mode-hook 'eglot-java-mode)
;; (with-eval-after-load 'eglot-java
;;   (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
;;   (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
;;   (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
;;   (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
;;   (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
;;   (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh))

;; 'flymake' has been a good linter interface. 'eglot' seems to report
;; issues from 'clang-tidy' through 'flymake'.

(use-package flymake
  :after exec-path-from-shell
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; :hook			      ;;
  ;; (c-ts-mode . flymake-mode)	      ;;
  ;; (c++-ts-mode . flymake-mode)     ;;
  ;; (emacs-lisp-mode . flymake-mode) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;
;;; themes fonts icons
;;;

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
  :after nerd-icons
  :diminish
  :config
  (declare-function nerd-icons-completion-mode "nerd-icons-completion")
  (nerd-icons-completion-mode)
  (declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :diminish
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package hide-mode-line
  :bind
  ("C-c C-h" . hide-mode-line-mode))



;;;
;;; applications
;;;

;;;
;;; resisting the weight of 'org-mode' with 'deft' and 'side-notes'.
;;;


;; i know the intelligentsia all use 'org', but i don't. 'org' invites
;; too much fiddling. 'deft' provides a lightweight solution that
;; provides the indexing and access i want.

(use-package deft
  :config
  (setopt deft-directory
	  (expand-file-name "~/Notepad"))
  (setopt deft-text-mode (seq-find 'fboundp '(markdown-mode text-mode)))
  (setopt deft-extension
          (assoc-default deft-text-mode '((markdown-mode . "md") (rst-mode . "rst"))
			 'eq "txt")))


;; 'side-notes' lets you have notes files in any project or directory.
;; the notes are opened in a side window like 'imenu-list'. searching
;; for the notes files are done backward up the the directory path
;; until one is found.

(use-package side-notes
  :diminish
  :bind ("M-s n" . side-notes-toggle-notes)
  :custom
  (side-notes-file "side-notes.txt")
  (side-notes-secondary-file "~/general-side-notes.txt"))

;; eat terminal support for zsh and eshell
;; zsh is by code in zshenv

(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))


(setopt org-confirm-babel-evaluate nil
	org-src-fontify-natively t
	org-src-tab-acts-natively t)



;;Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; These are the default values, but I keep them here for visibility.
 (use-package spacious-padding)

;;   :config
;;   (setopt spacious-padding-widths
;; 	  '( :internal-border-width 15
;;              :header-line-width 4
;;              :mode-line-width 6
;;              :tab-width 4
;;              :right-divider-width 30
;;              :scroll-bar-width 8
;;              :fringe-width 8)))
;;
;; ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; ;; is very flexible and provides several examples.
;; ;; (setopt spacious-padding-subtle-mode-line
;; ;; 	`( :mode-line-active 'default
;; ;;            :mode-line-inactive vertical-border))
;;
 (spacious-padding-mode 1)

;; Set a key binding if you need to toggle spacious padding.
(define-key global-map (kbd "<f8>") #'spacious-padding-mode)




;;;
;;; functions
;;;

;; collecting most function definitions here. i prefix my functions
;; as troi/. they could be stored in files in 'troi-lisp/' but i
;; prefer them here.


;; this is not mine, i stole from tess o'connor's config.
;; C-l a few times also moves the line to the top.

(defun troi/clear (&optional prefix)
  "Move the line containing point to the top of the window.
With PREFIX, move the line containing point to line PREFIX of the window."
  (interactive "P")
  (recenter (or prefix 0)))

(global-set-key (kbd "C-c t") 'troi/clear)


;; backups turned off ... trying 'no-littering'
;; ;; backups are a pain in the ass. sure, they are needed but let's
;; ;; segregate them by collecting them in one place
;;
;; (defun troi/backup-file-name (fpath)
;;   "Return a new file path of FPATH, creating directories if needed."
;;   (let* ((backup-root-dir "~/.tmp/emacs-backup/")
;;          (backup-file-path (replace-regexp-in-string "//" "/" (concat backup-root-dir fpath "~") )))
;;     (make-directory (file-name-directory backup-file-path) (file-name-directory backup-file-path))
;;     backup-file-path))
;; (setopt make-backup-file-name-function 'troi/backup-file-name)


;; from a multi-window frame, tear off the current window and
;; put it in a new frame. this only works if there are multiple
;; windows in the current frame.
;;
;; from https://stackoverflow.com/a/57318988 how to move a buffer to a
;; new frame. tear-off-window is usually bound to a mouse button but
;; i'm not a heavy mouse user so this function should do the job.

(defun troi/tear-off-window ()
  "Delete the selected window, and create a new frame displaying its buffer."
  (interactive)
  (let ((wc (count-windows)))
    (if (< wc 2)
	(message "only one window")
      (let* ((window (selected-window))
	     (buf (window-buffer window))
	     (frame (make-frame)))
	(select-frame frame)
	(switch-to-buffer buf)
	(delete-window window)))))

(bind-key "C-x 5t" #'troi/tear-off-window)



;;;
;;; os related customization
;;;


;;;
;;; touchpad touchiness
;;;

;; a rather heavy handed (but working) way to stop the mac touchpad
;; from moving things on me. i tried to find way to do this as a doom
;; after! but the double and triple variants kept being active. yes, i
;; searched the source. no, i couldn't find where that was done. the
;; customize interface isn't showing me these options in any way that
;; i understand. the goal here is to prevent my ham handed taps and
;; brushes of the touchpad from moving stuff around. i have mixed
;; feelings about drag-the-scrollbar mouse scrolling, but i don't like
;; the mouse wheel in text editing.

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


;; i keep brushing and confusing my touchpad. this moves the mouse out
;; of the way. i move it up from the lower right corner to avoid
;; pulling up the mac start bar.

(mouse-avoidance-mode 'banish)
(setopt mouse-avoidance-banish-position
	'((frame-or-window . frame) (side . right) (side-pos . 1)
	  (top-or-bottom . bottom) (top-or-bottom-pos . 10)))


;; mac keyboards and keybinds:
;;
;; in addition to changing caps lock to control, a standard keyboard
;; has on the bottom row:
;;
;; fn control os alt spacebar alt(gr) os menu control
;;
;; using karbiner i've remapped the bottom row to remove the need for
;; 'ns-*-modifier' remappings.
;;
;; fn       -> control
;; control  -> fn
;; option   -> command    (ie., super)
;; command  -> alt
;; spacebar -> unchanged
;; command  -> unchanged
;; option   -> unchanged



;;;
;;; easy customization interface
;;;

;; i'm not a fan of splitting things between customization and the
;; declarative files such as 'init.el'. i really hate the default
;; behavior of the customization interface tacking stuff on the tail
;; end of 'init.el'. setting a specific custom file name will have any
;; customizations logged into 'custom.el'.

(setopt custom-file (concat user-emacs-directory "custom.el"))

(load custom-file)

(provide 'init)
