;;; init.el --- Troy Brumley's init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2024-2025 Troy Brumley (aka Troi)

;; Author: Troy Brumley <blametroi@gmail.com>

;; All rights reserved.

;; the same directory as this in my "dotfiles" repository.
;; This file is NOT part of GNU Emacs. The author considers it to be
;; in the public domain.
;;
;; This file is generated from an Org document. That file should be
;; found in the same directory as this in my "dotfiles" repository.

;; I borrow liberally from Protesilaos "Prot" Stavrou's highly
;; instructive literal configuration found on his website:
;;
;; https://protesilaos.com/emacs/dotemacs
;;
;; The clever bits in here are likely from his config. I have changed
;; some names to avoid confusing myself, but I'm not trying to claim
;; his code. Hopefully I've marked these well enough.

;; The `init.el' file is run after `early-init.el'. Here we initialize
;; Emacs 'the application'. Establish package repositories, themes,
;; fonts, visual settings, and load and configure packages.

;;; Code:

;; Compatibility and requirements.

(when (< emacs-major-version 29)
  (error "This configuration requires Emacs 29 or newer!"))

(when (not (display-graphic-p))
  (message "This Emacs configuration assumes you are running a GUI, some things may break.")
  (sleep-for 5))

;; Packaging and Repositories.

;; We have to `require' use-package if we're being compiled. This is
;; also a good place to set some package load behavior defaults.

(eval-when-compile
  (require 'use-package))
(setopt load-prefer-newer t)
(setopt use-package-always-ensure t)
(setopt package-native-compile t)

;; This is separate from the compile tweaks in `early-init.el' to keep
;; it closer to `use-package' setup.
(setq native-comp-jit-compilation t)

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

;; 'compile-angel' opts for compiling most everything. It's still in
;; early development so this section is likely to change.

;; TODO: gate with native compile available check.
(use-package compile-angel
  :ensure t
  :demand t
  :diminish
  ;;:custom
  ;;  (compile-angel-verbose nil)
  :config
  (diminish 'compile-angel-on-load-mode "")
  (diminish 'compile-angel-on-save-mode "")
  (diminish 'compile-angel-on-save-local-mode "")
  (setq compile-angel-excluded-files-regexps '("/cus-load\\.el$"
                                               "/theme-loaddefs\\.el$"
                                               "/loaddefs\\.el\\.gz$"
                                               "/charprop\\.el$"
                                               "/cl-loaddefs\\.el\\.gz$"
					       "custom.el$"
					       "savehist.el$"
					       "recentf-save.el$"))
  ;; (setq compile-angel-predicate-function
  ;; 	(lambda (file)
  ;;         (not (file-in-directory-p file "/opt/.*"))))

  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; not a dashboard.

(setopt initial-scratch-message ";; nothing to see here, move along")

;; Wrapper macro to "no-op" emacs-lisp code. This is from
;; Prot's configuration.

(defmacro troi-emacs-comment (&rest body)
  "Determine what to do with BODY.
If BODY contains an unquoted plist of the form (:eval t) then
return BODY inside a `progn'.

Otherwise, do nothing with BODY and return nil, with no side
effects."
  (declare (indent defun))
  (let ((eval))
    (dolist (element body)
      (when-let* (((plistp element))
                  (key (car element))
                  ((eq key :eval))
                  (val (cadr element)))
        (setq eval val
              body (delq element body))))
    (when eval `(progn ,@body))))

;; No littering to reduce directory clutter.

;; NOTE: Yes, the two `require' calls in that order are deliberate and
;; they seem to work. Without them, not so much.

(use-package no-littering
  :ensure t
  :init
  (require 'no-littering)
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (recentf-mode))

;; Environment variables.

;; Get the correct environment variable values as if this is a login
;; shell. The variable list is hard coded and specific to my needs.

(use-package exec-path-from-shell
  :config
  (declare-function
   exec-path-from-shell-initialize "exec-path-from-shell" ())
  (declare-function
   exec-path-from-shell-copy-envs "exec-path-from-shell")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '(
     ;; Old style Makefile variables for C. I probably don't need
     ;; these anymore.
     "LIBRARY_PATH"
     "CPATH"
     "CDPATH"

     ;; Environment variables specific to compile and build for any
     ;; languages I'm working with.
     "CMAKE_GENERATOR"
     "ODIN_ROOT"

     ;; Where is the documentation? I know MANPATH is not used on all
     ;; operating systems, but it doesn't cause me problems to get it.
     "INFOPATH"
     "MANPATH"

     ;; Apple's libc malloc library emits some informational warnings
     ;; specific to particular allocation pools. They do me know good.
     "MallocNanoZone"
     )))

;; No safety net needed.

(setopt make-backup-files nil)
(setopt backup-inhibited nil) ; Is this redundant?
(setopt create-lockfiles nil)
(setopt auto-save-default nil)

;; Directories and files.

;; Add to the `load-path'. So far this is just my scratch
;; lisp directory.

(add-to-list
 'load-path
 (concat user-emacs-directory "troi-lisp"))

;; Org mode files and directories.

(setq org-dir (substitute-in-file-name "$HOME/org"))
(if (not (file-directory-p org-dir))
    (make-directory org-dir))
(setq org-directory org-dir)
(setq org-agenda-files '(org-directory))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; for dired, use 'gls' if it's available. the default 'ls' in MacOS
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
  (setopt dired-do-revert-buffer t)
  (setopt dired-free-space 'separate))

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;; History and such.

(use-package savehist
  :ensure nil
  :config
  (setopt savehist-additional-variables
          '(compile-command
            kill-ring
            regexp-search-ring))
  (savehist-mode)
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode)
  :custom
  (save-place-limit 1000))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-avoid-polling t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t))

;; Tool-tips (tooltip-mode)

(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;; Set up column numbers and row/column tracking in the
;; mode line.

(setopt apropos-sort-by-scores t)
(setopt blink-matching-delay 0.1)

;; Line numbering in programming modes is the way. Later I set the
;; mode-line format so the format starts counting columns at 1.

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 4)

;; Highlight the cursor line.

(setq global-hl-line-sticky-flag t)
(global-hl-line-mode)

;; Diminish mode indicators.

(use-package diminish
  :ensure t)

;; I run with column information visible full time. And I count from
;; one the way God intended.

(column-number-mode)
(setopt mode-line-position-column-line-format '(" (%l,%C)")) ; %C based 1, %c based 0

;; Display function name in mode line.

(which-function-mode)

;; Highlight the cursor line.

(global-hl-line-mode)

;; I use this rarely.

(use-package hide-mode-line
  :ensure t
  :defer t
  :bind
  ("C-c C-h" . hide-mode-line-mode))

;; Theme and some font/face.

(setopt custom-safe-themes t)
(use-package acme-theme
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'acme t)
  (setopt acme-theme-black-fg t)
  (custom-set-faces
   '(hl-line ((t (
                  :inherit highlight
                  :extend t
                  :background "LightGoldenrod2"
                  :foreground "black"))))
   '(compilation-error ((t (:background "gray80" :foreground "Red"))))
   '(flymake-error ((t (:underline (:color "Red" :style wave :position nil)))))
   '(font-lock-comment-face ((t (:foreground "#707070" :slant oblique))))
   '(font-lock-comment-face ((t (:foreground "#005500" :slant oblique))))))

;; Pulling out of custom.el to control here.

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 190)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height 190)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 230 :weight 'medium)
;; Choose some fonts
;; (set-face-attribute 'default nil :family "Iosevka")
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")

;; This is a straight copy and paste from Prot's config. Someday I'll
;; tune these to my preferences.

(setq light-mode nil)
(if light-mode
    (setq
     ;; TODO States
     todo-color "DarkOrange"
     in-progress-color "DeepSkyBlue3"
     blocked-color "Firebrick1"
     done-color "Green3"
     wont-do-color "Green3"
     ;; Tags
     critical-color "red1"
     easy-color "turquoise4"
     medium-color "turquoise4"
     hard-color "turquoise4"
     work-color "royalblue1"
     home-color "mediumPurple2"
     )
  (setq
   ;; TODO States
   todo-color "GoldenRod"
   in-progress-color "Cyan"
   blocked-color "Red"
   done-color "LimeGreen"
   wont-do-color "LimeGreen"
   ;; Tags
   critical-color "red1"
   easy-color "cyan3"
   medium-color "cyan3"
   hard-color "cyan3"
   work-color "royalblue1"
   home-color "mediumPurple1"
   )
  )

;; Icons

;; The Nerd Icons. Not that these depend upon having the Nerd Fonts on
;; your system.

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :after (dired nerd-icons)
  :diminish
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after (corfu vertico marginalia nerd-icons)
  :diminish
  :config
  (declare-function nerd-icons-completion-mode "nerd-icons-completion")
  (nerd-icons-completion-mode)
  (declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :after (nerd-icons ibuffer)
  :diminish
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-corfu
  :after (nerd-icons corfu))

;; Add kind of item icons to marginalia notes (folder, file, etc).

(use-package kind-icon
  :ensure t
  :after (corfu marginalia vertico)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; add descriptive notes 'in the margin' of various lists/uis.

(use-package marginalia
  :config
  (marginalia-mode))

;; Is there such a thing as a basic or minimal Org configuration?

(use-package org
  :ensure t
  :pin gnu
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  :config
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info))
  (setq org-use-sub-superscripts '{})
  (setq org-highlight-latex-and-related nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?B)
  (setq org-priority-faces nil)
  ;; TODO: is there a better way to do this?
  ;; TODO: move what can be moved into :custom.
  (setq org-log-done 'time)
  (setq org-todo-keywords
  	'((sequence "TODO(t)" "DOING(i@/!)" "BLOCKED(b@/!)"
  		    "|"
  		    "DONE(d@/!)" "WONT-DO(w@/!)" )))
  (setq org-capture-templates
  	'(
          ("t" "TODO Item"
           entry (file "~/org/todos.org")
           "* TODO [#B] %? %^g\n"
           :empty-lines 0)

          ("j" "Journal Entry"
           entry (file+datetree "~/org/journal.org")
           "* %?"
           :empty-lines 1)

          ("n" "Note"
           entry (file+headline "~/org/notes.org" "Random Notes")
           "** %?"
           :empty-lines 0)
          ))
  (setq org-tag-alist
  	'(
          (:startgroup . nil)
          ("easy" . ?e)
          ("medium" . ?m)
          ("difficult" . ?d)
          (:endgroup . nil)

          (:startgroup . nil)
          ("@work" . ?w)
          ("@home" . ?h)
          ("@anywhere" . ?a)
          (:endgroup . nil)

          ("CRITICAL" . ?c)
          ))
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-todo-keyword-faces
  	`(
          ("TODO"        . (:weight bold :foreground ,todo-color        ))
          ("IN-PROGRESS" . (:weight bold :foreground ,in-progress-color ))
          ("BLOCKED"     . (:weight bold :foreground ,blocked-color     ))
          ("DONE"        . (:weight bold :foreground ,done-color        ))
          ("WONT-DO"     . (:weight bold :foreground ,wont-do-color     ))
          )
  	)
  (setq org-tag-faces
  	`(
          ("CRITICAL" . (:weight bold :foreground ,critical-color ))
          ("easy"     . (:weight bold :foreground ,easy-color     ))
          ("medium"   . (:weight bold :foreground ,medium-color   ))
          ("hard"     . (:weight bold :foreground ,hard-color     ))
          ("@work"    . (:weight bold :foreground ,work-color     ))
          ("@home"    . (:weight bold :foreground ,home-color     ))
          )
  	)
  )

;; TODO: set up for my use...
;; (setq org-structure-template-alist
;; 	        '(
;; 	  ("C" . "comment")
;; 	  ("q" . "quote")
;; 	  ("c" . "center")
;; 	  ("v" . "verse")
;;          ("x" . "example")
;;
;; 	  ("a" . "export ASCII")
;;          ("X" . "export")
;;
;; 	  ("s" . "src")
;;          ("e" . "src emacs-lisp")
;;
;;          ("t" . "src emacs-lisp :tangle FILENAME")
;;          ("E" . "src emacs-lisp :results value code :lexical t")
;;          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
;; 	  ))

;; Load `org-modern' and `org-bullets' but do not enable them here. I
;; find outline editing easier without these turned on but there may
;; come a time when have them on all the time.

(use-package org-modern
  :ensure t
  :defer t)

(use-package org-bullets
  :ensure t
  :defer t)

;; Specialized support for the odd language or whatever else comes up.

(use-package ob-sml
  :ensure t)

(use-package ob-typescript
  :ensure t)

;; Org related key-binds.

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Dictionary.

(use-package dictionary
  :ensure nil
  ;;    :bind ("C-c d" . dictionary-search)
  :config
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev"
        dictionary-create-buttons nil
        dictionary-use-single-buffer t))

;; Spell check with flyspell.

(use-package flyspell
  :ensure nil
  ;;     :bind
  ;;     ( :map flyspell-mode-map
  ;;       ("C-;" . nil)
  ;;       :map flyspell-mouse-map
  ;;       ("<mouse-3>" . flyspell-correct-word)
  ;;       :map ctl-x-x-map
  ;;       ("s" . flyspell-mode)) ; C-x x s
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_US"))

;; 'deft' is a light weight free format note application.

(use-package deft
  :config
  (setopt deft-directory
  	  (expand-file-name "~/Notepad"))
  (setopt deft-text-mode (seq-find 'fboundp '(markdown-mode text-mode)))
  (setopt deft-extension
          (assoc-default deft-text-mode '((markdown-mode . "md") (rst-mode . "rst"))
  			 'eq "txt")))

;; I use side-notes as scratch paper in project directories. The notes
;; files aren't stored in Git, I have them excluded in my .gitignore.

(use-package side-notes
  :diminish
  :bind ("M-s n" . side-notes-toggle-notes)
  :custom
  (side-notes-file "side-notes.txt")
  (side-notes-secondary-file "~/general-side-notes.txt"))

;; documentation with 'info' and 'eldoc'. for some reason I'm missing
;; system info from Homebrew. i should probably move this into my
;; zshenv.

(use-package info
  :after exec-path-from-shell
  :custom
  (Info-additional-directory-list '("/opt/homebrew/share/info")))

(use-package eldoc
  :ensure nil
  :diminish
  :config (global-eldoc-mode))

;; `man' (man-pages)

(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'

;; Improve processing of excessively long lines. Forcing left-to-right
;; instead of allowing for right-to-left is apparently a significant
;; improvement for long lines.

(use-package so-long
  :config
  (global-so-long-mode)
  :custom
  (bidi-paragraph-direction 'left-to-right))

;; Treemacs seems useful.

(use-package treemacs
  :ensure t
  :after nerd-icons
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-nerd-icons
  :after treemacs
  :ensure t)

;; completion styles

(use-package minibuffer
  :ensure nil
  :config

   ; Also see `completion-category-overrides'.
  (setq completion-styles '(basic substring initials flex prescient))

  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override everything.
  (setq completion-category-defaults nil)

  ;; A non-exhaustive list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'

  (setq completion-category-overrides
        '((file (styles . (basic partial-completion prescient)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring prescient)))
          (consult-location (styles . (basic substring prescient)))
          (kill-ring (styles . (emacs22 prescient)))
          (eglot (styles . (emacs22 substring prescient))))))

;; Built-in completion dials and switches.

;; TODO: ordering with use package above.
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
;; (setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)

(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt completion-auto-help 'always)
(setopt completions-max-height 7)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)

(use-package mb-depth
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode))

(use-package minibuf-eldef
  :ensure nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29

;; COmpletion in Region FUnctions:

;; Corfu offers popup support both terminal and GUI use, but I do not
;; use the terminal.

(use-package corfu
  :ensure t
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
  :custom
  ;; This hides commands in m-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil          ; This is part of the corfu package.
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Completion At Point Extensions:

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; VERtical Interactive COmpletion.

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Prescient completion candidate sorting and selection.

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode))

(use-package corfu-prescient
  :ensure t
  :after (prescient corfu)
  :config
  (corfu-prescient-mode))

(use-package vertico-prescient-mode
  :ensure nil
  :after (prescient vertico)
  (vertico-prescient-mode))

;; Key binds

;; This makes TAB in the minibuffer behave more like it does in a
;; shell.
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;; Make ESC quit prompts.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; On the Mac s-q is the command-Q equivalent. I use it to close Emacs
;; when I don't use M-x 'save-buffers-kill-emacs'.
(global-unset-key (kbd "C-x C-c"))

;; The number of times I want a dumb list instead of the smart UI for
;; buffers and directories is zero.
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Default search to regexp instead of string. TODO: Provide a toggle
;; or string option.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Zap 'to' not 'through'. This is the way.
(global-set-key "\M-z" 'zap-up-to-char)

;; TODO: are these the bindings I want for these?
;;("M-c" . capitalize-dwim)
;;("M-l" . downcase-dwim) ; "lower" case
;;("M-u" . upcase-dwim)
;;("M-=" . count-words) ;; was count-words-region

;; ("C-M-d" . up-list) ; confusing name for what looks like "down" to me
;; ("<C-M-backspace>" . backward-kill-sexp)
;; Keymap for buffers (Emacs28)
;; :map ctl-x-x-map
;; ("f" . follow-mode)  ; override `font-lock-update'
;; ("r" . rename-uniquely)
;; ("l" . visual-line-mode)

;; Built-in bookmarking framework.

(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-save-flag 1))          ; persist bookmark updates

;; Registers, named holders.

(use-package register
  :ensure nil
  :defer t
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'register-preview-default)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;; Movement and navigation.

(use-package avy
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

(use-package ace-window
  :demand t
  :after avy
  :bind (("C-x o" . ace-window)
  	 ("M-o" . ace-window)))

(use-package dumb-jump
  :hook
  (xref-backend-functions . dump-jump-xref-activate))

;; Eglot

(use-package eglot
  :ensure t
  :pin gnu

  ;; We can start up language servers as sub-processes, be sure we can
  ;; find the executables.
  :after exec-path-from-shell

  :commands (eglot
  	     eglot-ensure
  	     eglot-code-actions
             eglot-rename
             eglot-format-buffer)

  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (odin-mode . eglot-ensure)

  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c r" . eglot-rename))

  ;; if debugging 'eglot' issues, comment out the fset and
  ;; events-buffer-config lines.
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; performance boost-don't log every event
  (setopt jsonrpc-event-hook nil)

  :custom
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)  ; Prevent minibuffer spam
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider)))

;; Treesitter

;; There isn't much configuration to do for Treesitter. The
;; customization options are minimal and it's just "always there."

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)) ; levels 1-3 are useless

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
  :ensure t
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

;; Add frame borders and window dividers to give me a bit of
;; separation from the edge of the screen.

(modify-all-frames-parameters
 '((right-divider-width . 5)
   (internal-border-width . 5)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; Utility function to move a window to a new frame.

(defun troi/tear-off-window ()
  "Move a sub-window to a new frame.
From a multi-window frame, tear off the current window and put
it in a new frame."
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

;; TODO: move key-binds
(bind-key "C-x 5t" #'troi/tear-off-window)

;; Flymake

;; 'flymake' has been a good linter interface. 'eglot' reports issues
;; from 'clang-tidy' through 'flymake'.

(use-package flymake
  :after (exec-path-from-shell odin-mode)

  :hook
  (c-ts-mode . flymake-mode)
  (c++-ts-mode . flymake-mode)
  (emacs-lisp-mode . flymake-mode)
  (odin-mode . flymake-mode)

  :custom
  (flymake-mode-line-lighter "FM")
  :bind
  (:map flymake-mode-map
  	("M-n" . flymake-goto-next-error)
  	("M-p" . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! L" . flymake-show-project-diagnostics)))

;; This is needed to avoid false 'can not find/load' errors on
;; requires that occur before this point in the source.
(with-eval-after-load 'flymake
  (setopt elisp-flymake-byte-compile-load-path load-path))

;; `project'

(use-package project
  :ensure nil
  :config
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers
	'(".projectile" ".project.el" "fpm.toml"))
  (setq project-key-prompt-style t) ; Emacs 30
  (advice-add #'project-switch-project
	      :after #'troi-common-clear-minibuffer-message))

;; `ediff'

(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)

  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

;;;###autoload
(defun troi-common-clear-minibuffer-message (&rest _)
  "Print an empty message to clear the echo area.
Use this as advice :after a noisy function."
  (message ""))

;; `diff-mode'
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (setq diff-default-read-only t))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;; Version control framework (vc.el, vc-git.el, and more)

(use-package vc
  :ensure nil
  :init
  (setq vc-follow-symlinks t)
  :config
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)

  (setq vc-find-revision-no-save t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-log-switches '("--stat"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        `("%d %h %ai %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  (setq vc-git-log-edit-summary-target-len 50)
  (setq vc-git-log-edit-summary-max-len 70))

;; Auto parenthesis matching

(add-hook 'prog-mode-hook 'electric-pair-mode)

;; nicer scrolling

(setopt scroll-margin 0)
(setopt scroll-conservatively 100000)
(setopt scroll-preserve-screen-position 1)
;; DO NOT USE (pixel-scroll-precision-mode) DO NOT USE

(setopt switch-to-buffer-obey-display-actions t)
(setopt help-window-select t)
(setopt help-window-keep-selected t)
(setopt enable-recursive-minibuffers t)
(setopt confirm-kill-emacs 'y-or-n-p)

;; The Emacs gods don't think we should have access to commands
;; that might confuse us. They mark them disabled and issue an
;; 'are you sure' warning.

;; 'put' is used because these are properties of the function
;; name symbol.

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Hide (fold) function bodies.

(use-package auto-hide
  :vc
  (auto-hide :url "https://github.com/BlameTroi/auto-hide.el"
             :branch "main")
  :hook (prog-mode . hs-minor-mode))

;; Random 'no' configuration required modes

(use-package cmake-mode)

(use-package ninja-mode)

(use-package git-modes)

;; C (not C++, C!)

(setopt standard-indent 8)

;; Just set a few options and wire in 'clangd'.
(with-eval-after-load 'c-ts-mode
  (setopt c-ts-mode-enable-doxygen t)
  (setopt c-ts-mode-indent-offset 8)
  (setopt c-ts-mode-indent-style 'linux)
  (keymap-unset c-ts-base-mode-map "C-c C-c")) ; redundant 'comment-region'

;; Configure the 'clangd' language server to my preferences.
;; 'clangd' uses 'CMakeLists.txt' and 'compile_commands.json'
;; to determine what to analyze. There are default settings
;; in my local config as well.
(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '((c-ts-mode c++-ts-mode)
     . ("clangd"
        "-j=4"                   ; async index threads
  	"--log=info"             ; or "error" or "verbose"
  	"--pch-storage=memory"   ; i have plenty
  	"--enable-config"))))

;; Some other clangd options:
;; "--log=error"						 ;;
;; "--background-index"						 ;;
;; "--clang-tidy"                ; but i use 'astyle' to format. ;;
;; "--completion-style=detailed"				 ;;
;; "--header-insertion=never"					 ;;
;; "--header-insertion-decorators=0"

;; I use 'astyle' to format C. The configuration goes in .astylerc
;; in my home directory. My formatting is based on the 'linux'
;; and 'k&r' styles.

(use-package reformatter
  :ensure t
  :after exec-path-from-shell)

(use-package astyle
  :ensure t
  :after reformatter
  :when (executable-find "astyle")
  :diminish (astyle-on-save-mode . "as")
  :hook
  (c-ts-mode . astyle-on-save-mode)
  (c++-ts-mode . astyle-on-save-mode))

;; Odin.

;; Odin mode isn't available as a package yet.

(require 'odin-mode)

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '(odin-mode . ("ols"))))

;; Cobol.

(use-package cobol-mode
  :ensure t
  :defer t
  :mode ("\\.cob\\'" "\\.cbl\\'" "\\.cpy\\'"))

;; Fortran.

;; No setup required. The built-in 'fortran-mode' works with
;; Modern Fortran (f90 file extension).

;; Typescript.

;; To be provided.

;; Assembler.

;; ARM to be provided.

;; IBM to be provided.

;; SML.

;; There is a sml-ts-mode but I haven't used it, this setup
;; worked well enough, but upgrading is an option if I get back
;; to functional programming.

;; The 'smlnj' and 'smlfmt' executables are available from
;; 'brew'.

(use-package sml-mode
  :defer t
  :ensure nil
  :mode "\\.sml\\'"
  :interpreter "sml")

(use-package sml-basis
  :ensure t
  :after sml-mode)

(use-package smlfmt
  :ensure t
  :after sml-mode)

;; Guile or Chez scheme? Nah, Chicken!

;; NOTE: If Geiser sees a 'geiser-somescheme' in your load-path,
;;       it becomes available leading to popups about which
;;       scheme to run. Removing non-active schemes for now.

;;(use-package geiser-chez
;;  :ensure t
;;  :defer t
;;  :custom
;;  (geiser-chez-binary "chez"))

;; (use-package geiser-guile
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-to-list 'geiser-implementations-alist
;; 	       '(((regexp "\\.scm$") guile)
;; 		((regexp "\\.ss$") chez)
;; 		((regexp "\\.rkt$") racket))))
;; 
;; (use-package flymake-guile
;;   :ensure t
;;   :after geiser-guile)

;; (use-package geiser-chicken
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-to-list 'geiser-implementations-alist
;; 	       '(((regexp "\\.scm$") chicken)
;; 		 ((regexp "\\.ss$") chez))))

;; I don't know if I want to get the non elpa-ed flymake or not.

(require 'chicken)
(require 'flymake-chicken)

;(use-package scheme-mode
;  ensure: nil
;  :hook (scheme-mode ))

;; Text display and editing.

(use-package isearch
  :ensure nil
  :demand t
  :config
  ;; "find one two" would find "one two" "one hi there two" etc.
  ;; one `setq' here to make it obvious these are a group.
  (setq search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil)

  (setq isearch-lazy-count t)
  (setq isearch-lazy-highlight t)
  (setq isearch-repeat-on-direction-change t)
  (setq isearch-wrap-pause t)

  (setq search-highlight t)

  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4)

  (add-hook 'occur-mode-hook #'hl-line-mode)

  ) ;; use-package isearch

(use-package re-builder
  :ensure nil
  :commands (re-builder regexp-builder)
  :config
  (setq reb-re-syntax 'read))

(use-package grep
  :ensure nil
  :after exec-path-from-shell
  :commands (grep lgrep rgrep)
  :config
  (setq grep-save-buffers nil)
  (setq grep-use-headings t) ; Emacs 30

  (let ((executable (or (executable-find "rg") "grep"))
        (rgp (string-match-p "rg" grep-program)))
    (setq grep-program executable)
    (setq grep-template
          (if rgp
              "/usr/bin/rg -nH --null -e <R> <F>"
            "/usr/bin/grep <X> <C> -nH --null -e <R> <F>"))
    (setq xref-search-program (if rgp 'ripgrep 'grep))))

;; wgrep (writable grep)
;; See the `grep-edit-mode' for the new built-in feature.

(unless (>= emacs-major-version 31)
  (use-package wgrep
    :ensure t
    :after grep
    :bind
    ( :map grep-mode-map
      ("e" . wgrep-change-to-wgrep-mode)
      ("C-x C-q" . wgrep-change-to-wgrep-mode)
      ("C-c C-c" . wgrep-finish-edit))
    :config
    (setq wgrep-auto-save-buffer t)
    (setq wgrep-change-readonly-file t)))

;; Text and other settings that haven't fit anywhere else yet.

;; Line widths. The `visual-fill-column' package 'narrows' the
;; display when you're using a single window on a wide screen so
;; if you are wrapping text it will wrap at the fill column and
;; not the edge of the screen.

(setq-default tab-width 8)
(setq-default indent-tabs-mode t)

(use-package visual-fill-column
  :ensure t
  :commands (visual-fill-column-mode))

;; visual line mode is OK for text, use (truncate-lines t) for
;; code.

(setq-default fill-column 70)
;; (add-hook 'text-mode-hook 'visual-line-mode)

;; More odds and ends.

(delete-selection-mode +1)
(indent-tabs-mode +1)
(setopt tab-always-indent 'complete)
(setopt comment-empty-lines t)
(setopt require-final-newline t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Parentheses (show-paren-mode)
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay)) ; Emacs 29

;; Plain text (text-mode)
(use-package text-mode
  :ensure nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook
  (text-mode . turn-on-auto-fill)
  :config
  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t))

(use-package which-key
  :ensure nil
  :diminish
  :config (which-key-mode))

(use-package bind-key
  :ensure nil)

(use-package ws-butler
  :ensure t
  :diminish
  :hook (prog-mode . ws-butler-mode))

;; i often use C-l for visual breaks.

(use-package form-feed-st
  :diminish
  :hook
  (prog-mode . form-feed-st-mode)
  (text-mode . form-feed-st-mode))

(provide 'init)
;;; File init.el ends here.
