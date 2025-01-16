;;; init.el --- Troy Brumley's init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2024-2025 Troy Brumley (aka Troi)

;; Author: Troy Brumley <blametroi@gmail.com>

;; All rights reserved.

;; This file is NOT part of GNU Emacs. The author considers it to be
;; in the public domain.

;; This file was originally generated from an Org document. As much as
;; I try, I just can't get into `literate' configurations. It was a
;; good exercise to go through a few configurations and build my own
;; attempt at being literate, but I can't live with it on a regular
;; basis.

;; As I moved this back to being pre-literate I made many changes.
;; Most noticeable of them is the removal of `use-package'. It's a
;; cool tool and it truly is the best thing since sliced bread, but it
;; lets me skip learning things. It may come back into use once I have
;; the confidence that I'm not avoiding learning things.

;; There are several good 'learning' configurations to be found on
;; reddit, github, and elsewhere on the web. Two highly readable
;; configurations that influenced me the most are:
;;
;; 1) Protesilaos "Prot" Stavrou's highly instructive literal
;;    configuration found on his website:
;;
;;    https://protesilaos.com/emacs/dotemacs
;;
;; 2) Ashton Wiersdorf's Emacs-Bedrock  at:
;;
;;    https://codeberg.org/ashton314/emacs-bedrock

;; The `init.el' file is run after `early-init.el'. Here we initialize
;; Emacs 'the application'. Establish package repositories, themes,
;; fonts, visual settings, and load and configure packages.
;;
;; I am running Emacs 30 and its capabilities are assumed throughout. I
;; don't do release checks and fallbacks.

;;; Code:

;;; Compatibility and requirements.

(when (< emacs-major-version 30)
  (error "This configuration requires Emacs 30 or newer!"))

(when (not (display-graphic-p))
  (message "This Emacs configuration assumes you are running a GUI, some things may break.")
  (sleep-for 5))

;;; Packaging and Repositories.

;; We have to `require' use-package if we're being compiled. This is
;; also a good place to set some package load behavior defaults.

(eval-when-compile
  (require 'use-package))
(setopt load-prefer-newer t)
(setopt use-package-always-ensure nil)
(setopt package-native-compile t)

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


;;; Customization and new system setup:

;; The Customization Interface and file. I don't use the file often,
;; and I have commented out the load code. By uncommenting, you can
;; start up a new installation and have Emacs load all your selected
;; packages. When you manually load a package from an archive, it is
;; noted in `custom.el'.
;;
;; `use-package' would load these, but I'm (temporarily?) not using
;; it.

(setopt custom-file (concat user-emacs-directory "custom.el"))
;; (load-file custom-file)
;; (setopt package-install-upgrade-built-in t)
;; (package-install-selected-packages)
;; (setopt debug-on-error t)


;;; Diminish minor mode indicators.

;; Why does almost every minor mode insist on putting an indicator in
;; the mode line? Activate `diminish' (it's still a loaded package)
;; early in initialization and then `(diminish 'mode-name ?)' after
;; each minor mode is `require'd. You could group the `diminish'
;; phrases at the end of `init.el', but I would rather keep them close
;; to their mode's `require'.

(require 'diminish)


;;; Do I want a dashboard?

(setopt initial-scratch-message ";; nothing to see here, move along")


;;; No littering and recent file mode.

(require 'no-littering)
(require 'recentf)
(setopt recentf-max-menu-items 100)
(setopt recentf-max-saved-items 100)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(add-hook 'emacs-startup-hook 'recentf-mode)


;;; Environment variables.

;; Get the correct environment variable values as if this is a login
;; shell. The variable list is hard coded and specific to my needs.

;; MacOS packaging grabs environment variables as requested in a plist
;; within the package. Sometimes this fails, and other times variables
;; I care about are not included.

(require 'exec-path-from-shell)
         (declare-function
           exec-path-from-shell-initialize "exec-path-from-shell" ())
         (declare-function
           exec-path-from-shell-copy-envs "exec-path-from-shell")
         (exec-path-from-shell-initialize)
         (exec-path-from-shell-copy-envs
           '(
             ;; Old style Makefile variables for C. I probably don't
             ;; need these anymore.
             "LIBRARY_PATH"
             "CPATH"
             "CDPATH"

             ;; Environment variables specific to compile and build
             ;; for any languages I'm working with.
             "CMAKE_GENERATOR"
             "ODIN_ROOT"

             ;; Where is the documentation? I know MANPATH is not used
             ;; on all operating systems, but it doesn't cause me
             ;; problems to get it.
             "INFOPATH"
             "MANPATH"

             ;; Apple's libc malloc library emits some informational
             ;; warnings specific to particular allocation pools. They
             ;; do me know good.
             "MallocNanoZone"
             ))


;;; No safety net needed.

(setopt make-backup-files nil)
(setopt backup-inhibited nil) ; Is this redundant?
(setopt create-lockfiles nil)
(setopt auto-save-default nil)


;;; History and persistent positioning in edited files.

(require 'savehist)
(setopt savehist-additional-variables
        '(compile-command
          kill-ring
          regexp-search-ring))
(savehist-mode)
(setq history-length 100)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)

(require 'saveplace)
(add-hook 'emacs-startup-hook 'save-place-mode)
(setopt save-place-limit 1000)


;;; Directories and files.

;; Add my lisp to the `load-path'. Scratch and test code, things in
;; development, etc.

(add-to-list
 'load-path
 (concat user-emacs-directory "troi-lisp"))

;; Org mode files and directories. I am not a heavy Org user, but it
;; is an assumed part of the Emacs infrastructure.

(require 'org)
(require 'org-bullets)
(require 'org-modern)
(setq org-dir (substitute-in-file-name "$HOME/org"))
(if (not (file-directory-p org-dir))
  (make-directory org-dir))
(setq org-directory org-dir)
(setq org-agenda-files '(org-directory))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


;;; Dired.

;; For `dired', use 'gls' if it's available. The default 'ls' in MacOS
;; and some other systems doesn't support all the options that 'dired'
;; wants.

(require 'dired)
(when (executable-find "gls")    ; use GNU ls
  (setopt dired-use-ls-dired nil)
  (setopt ls-lisp-use-insert-directory-program t)
  (setopt insert-directory-program "gls")
  (setopt dired-listing-switches "-alh --group-directories-first"))
(setopt dired-recursive-copies 'always)
(setopt dired-isearch-filenames 'dwim)
(setopt dired-recursive-deletes 'always)
(setopt dired-kill-when-opening-new-dired-buffer t)
(setopt delete-by-moving-to-trash t)
(setopt dired-dwim-target t)
(setopt dired-auto-revert-buffer t)
(setopt dired-do-revert-buffer t)
(setopt dired-free-space 'separate)


;;; Reload buffer if a file changes.

(require 'autorevert)
(add-hook 'emacs-startup-hook 'global-auto-revert-mode)
(setopt auto-revert-avoid-polling t)
(setopt global-auto-revert-non-file-buffers t)
(setopt auto-revert-verbose nil)


;; Tool-tips (tooltip-mode). While I don't think of these as
;; the drop-down menus for completions, apparently they are.

(require 'tooltip)
(add-hook 'emacs-startup-hook 'tooltip-mode)
(setq tooltip-delay 0.5
      tooltip-short-delay 0.5
      tooltip-frame-parameters
      '((name . "tooltip")
        (internal-border-width . 10)
        (border-width . 0)
        (no-special-glyphs . t)))


;;; Mode line and related settings.

;; Line numbering in programming modes is the way.

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 4)

;; I run with column information visible full time. And I count from
;; one the way God intended.

(add-hook 'emacs-startup-hook 'column-number-mode)
(setopt mode-line-position-column-line-format '(" (%l,%C)")) ; %C based 1, %c based 0

;; Display function name in mode line. This also seems to work for Org
;; headers.

(add-hook 'emacs-startup-hook 'which-function-mode)

;; Highlight the cursor line.

(setq global-hl-line-sticky-flag t)
(add-hook 'emacs-startup-hook 'global-hl-line-mode)

;; TODO: rehome these visuals.
(setopt apropos-sort-by-scores t)
(setopt blink-matching-delay 0.1)


;;; Theme and colors and faces:

;; I keep returning to the `acme-theme'. It's very easy on my eyes.
;; I expected to stick with dark black themes but this is clearly
;; better.

(setopt custom-safe-themes t)
(require 'acme-theme)
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'acme t)
(setopt acme-theme-black-fg t)

;; TODO: fix these to `set-face-attribute'
(custom-set-faces
 '(hl-line ((t (
                :inherit highlight
                :extend t
                :background "LightGoldenrod2"
                :foreground "black"))))
 '(compilation-error ((t (:background "gray80" :foreground "Red"))))
 '(flymake-error ((t (:underline (:color "Red" :style wave :position nil)))))
 '(font-lock-comment-face ((t (:foreground "#707070" :slant oblique))))
 '(font-lock-comment-face ((t (:foreground "#005500" :slant oblique)))))

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 190)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height 190)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 230 :weight 'medium)


;;; Don't fill of the edge of the screen.

;; A nice trick from Prot! Add frame borders and window dividers to
;; give me a bit of visual separation between windows and frames and
;; the screen.

(modify-all-frames-parameters
 '((right-divider-width . 5)
   (internal-border-width . 5)))
(dolist (face '(window-divider
                 window-divider-first-pixel
                 window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))


;;; Icons and helpful glyphs and tips.

;; The Nerd Icons. I prefer these to All the Icons. Note that these
;; depend upon having the Nerd Fonts installed on your system.

(require 'nerd-icons)
(require 'nerd-icons-dired)
(require 'nerd-icons-completion)
(require 'nerd-icons-ibuffer)

(add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
(add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)
(add-hook 'emacs-startup-hook 'nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

;; Add descriptive notes 'in the margin' of various lists/uis.

(require 'marginalia)
(require 'kind-icon)


;;; Spell checking.

;; Spell check with flyspell.
;;     ( :map flyspell-mode-map
;;       ("C-;" . nil)
;;       :map flyspell-mouse-map
;;       ("<mouse-3>" . flyspell-correct-word)
;;       :map ctl-x-x-map
;;       ("s" . flyspell-mode)) ; C-x x s

(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_US")


;;; Deft & Side Notes:

;; 'deft' is a light weight free format note application. I find
;; it easier to use than Org and it invites less tweaking.

(require 'deft)
(setopt deft-directory
        (expand-file-name "~/Notepad"))
(setopt deft-text-mode (seq-find 'fboundp '(markdown-mode text-mode)))
(setopt deft-extension
        (assoc-default deft-text-mode '((markdown-mode . "md") (rst-mode . "rst"))
                       'eq "txt"))

;; I use `side-notes' as scratch paper in project directories. The
;; notes files aren't stored in Git, I have them excluded in my
;; `.gitignore'.

(require 'side-notes)
;;:bind ("M-s n" . side-notes-toggle-notes)
(setopt side-notes-file "side-notes.txt")
(setopt side-notes-secondary-file "~/general-side-notes.txt")


;;; Documentation.

;; Read documentation with 'man', 'info', and 'eldoc'. For some reason
;; I'm missing system info from Homebrew. I should probably move this
;; into my `.zshenv'.

(require 'info)
(setopt Info-additional-directory-list '("/opt/homebrew/share/info"))

(require 'eldoc)
(diminish 'eldoc-mode nil)
(add-hook 'emacs-startup-hook 'global-eldoc-mode)

(require 'man)
(setq Man-notify-method 'pushy)


;;; Another speedup.

;; Improve processing of excessively long lines. Forcing left-to-right
;; instead of allowing for right-to-left is apparently a significant
;; improvement for long lines.

(require 'so-long)
(add-hook 'emacs-startup-hook 'global-so-long-mode)
(setopt bidi-paragraph-direction 'left-to-right)


;;; Minibuffer.

;; Streamline and make it more informative.

(require 'minibuffer)
(setopt enable-recursive-minibuffers t)
(setq minibuffer-default-prompt-format " [%s]")

(require 'mb-depth)
(add-hook 'emacs-startup-hook 'minibuffer-depth-indicate-mode)

(require 'minibuf-eldef)
(add-hook 'emacs-startup-hook 'minibuffer-electric-default-mode)

;;;###autoload
(defun troi-common-clear-minibuffer-message (&rest _)
  "Print an empty message to clear the echo area.
Use this as advice :after a noisy function.

My thanks to Prot!"
  (message ""))


;;; Completion.

;; Completion is spread throughout the rest of this configuration.
;; I need to organize this better.

;; TODO: clean up !

;; Built-in completion dials and switches. Also see
;; `completion-category-overrides'.

(setq completion-styles '(basic substring initials flex prescient))
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

;; This hides commands in m-x which do not apply to the current mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)


;;; COmpletion in Region FUnctions:

;; Corfu offers popup support both terminal and GUI use, but I do not
;; use the terminal.

(require 'corfu)
;;(corfu-map
;;      ("SPC" . corfu-insert-separator)
;;      ("C-n" . corfu-next)
;;      ("C-p" . corfu-previous))
(add-hook 'prog-mode-hook 'corfu-mode)
(add-hook 'shell-mode-hook 'corfu-mode)
(add-hook 'eshell-mode-hook 'corfu-mode)
(add-hook 'emacs-startup-hook 'global-corfu-mode)

(require 'corfu-popupinfo)
(add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
(setopt corfu-popupinfo-delay '(0.25 . 0.1))
(setopt corfu-popupinfo-hide nil)

(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
(add-hook 'emacs-startup-hook 'marginalia-mode)


;;; Completion At Point Extensions:

(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)


;;; VERtical Interactive COmpletion.

(require 'vertico)
(add-hook 'emacs-startup-hook 'vertico-mode)

(require 'vertico-directory)
;;:bind (:map vertico-map
;;            ("M-DEL" . vertico-directory-delete-word)))


;;; Prescient completion candidate sorting and selection.

;; `orderless' is another option, but I'm using `prescient' because
;; it requires almost no configuration.

(require 'prescient)
(add-hook 'emacs-startup-hook 'prescient-persist-mode)

(require 'corfu-prescient)
(add-hook 'emacs-startup-hook 'corfu-prescient-mode)

(require 'vertico-prescient)
(add-hook 'emacs-startup-hook 'vertico-prescient-mode)


;;; Drop a pin, or breadcrumbs.

;; Bookmarking is a subset of registers, or maybe registers are a
;; subset of bookmarks. Whichever is true, we get there via `c-x r'.

(require 'bookmark)
(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)
(setq bookmark-save-flag 1)          ; persist bookmark updates

(require 'register)
(setq register-preview-delay 0.8
      register-preview-function #'register-preview-default)

(with-eval-after-load
    'savehist
  (add-to-list 'savehist-additional-variables 'register-alist))


;;; Movement and navigation.

(require 'avy)
(require 'ace-window)
(require 'dumb-jump)
(add-hook 'xref-backend-functions-hook #'dumb-jump-xref-activate)


;;; Eglot. The new imporved LSP support.

(require 'eglot)
(diminish 'eglot-mode "Egl")

;; TODO: rehome these to language specific sections.
;; We can start up language servers as sub-processes, be sure we can
;; find the executables.
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'odin-mode-hook 'eglot-ensure)

;; if debugging 'eglot' issues, comment out the fset and
;; events-buffer-config lines.
(fset #'jsonrpc--log-event #'ignore)  ; performance boost-don't log every event
(setopt jsonrpc-event-hook nil)

(setopt eglot-events-buffer-config '(:size 0 :format short))
(setopt eglot-autoshutdown t)
(setopt eglot-send-changes-idle-time 0.1)
(setopt eglot-extend-to-xref t)
(setopt eglot-report-progress nil)  ; Prevent minibuffer spam
(setopt eglot-ignored-server-capabilities
         '(:documentFormattingProvider
            :documentRangeFormattingProvider
            :documentOnTypeFormattingProvider))


;;; Treesitter. The new improved language mode parser.

;; There isn't much configuration to do for Treesitter. The
;; customization options are minimal and it's just "always there."

(require 'treesit)
         (setopt treesit-font-lock-level 4) ; levels 1-3 are useless

;; Some of these might require M-x treesit-install-language-grammar.

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

(require 'treesit-auto)
(setopt treesit-auto-install 'prompt)

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

(add-hook 'emacs-startup-hook 'global-treesit-auto-mode)


;;; Utility function to move a window to a new frame.

;; I usually run with only one maximized frame with two windows at
;; most. But there are times when multiple frames are warranted. This
;; function takes a window from a multi-window frame and puts it in a
;; new frame.
;;
;; This only works if there are multiple windows in the current frame.
;;
;; The original function is from https://stackoverflow.com/a/57318988
;; _How to move a buffer to a new frame_.

(defun troi/tear-off-window ()
  "Move a sub-window to a new frame.

From a multi-window frame, tear off the current window and
put it in a new frame."
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


;;; Flymake

;; 'flymake' has been a good linter interface. 'eglot' reports issues
;; from 'clang-tidy' through 'flymake'.

(require 'flymake)
(add-hook 'c-ts-mode-hook 'flymake-mode)
(add-hook 'c++-ts-mode-hook 'flymake-mode)
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'odin-mode-hook 'flymake-mode)
(setopt flymake-mode-line-lighter "FM")
;;  :bind
;;(:map flymake-mode-map
;;	("M-n" . flymake-goto-next-error)
;;	("M-p" . flymake-goto-prev-error)
;;    ("C-c ! l" . flymake-show-buffer-diagnostics)
;;  ("C-c ! L" . flymake-show-project-diagnostics)))

;; This is needed to avoid false 'can not find/load' errors on
;; requires that occur before this point in the source.

(with-eval-after-load 'flymake
   (setopt elisp-flymake-byte-compile-load-path load-path))


;;; Emacs views a `project' as a grouping of directories and files.

(require 'project)
(setopt project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (project-dired "Root dired")
          (project-vc-dir "VC-Dir")
          (project-shell "Shell")
          (keyboard-quit "Quit")))
(setq project-vc-extra-root-markers
      '(".projectile" ".project.el" ".project" "fpm.toml"))
(setq project-key-prompt-style t) ; Emacs 30
(advice-add #'project-switch-project
            :after #'troi-common-clear-minibuffer-message)


;;; Difference engines.

;; `ediff'

(require 'ediff)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)

;; `diff-mode'

(require 'diff-mode)
(setq diff-default-read-only t)


;; `diff-hl'

(require 'diff-hl)
(add-hook 'emacs-startup-hook 'global-diff-hl-mode)


;;; Parentheses matching and structural editing.

;; Hopefully `lispy' will do the jobs of both `paredit' and
;; `electric-pair-mode'.

(require 'lispy)

;; This redundant with `paredit'.
;; (add-hook 'prog-mode-hook 'electric-pair-mode)

;; `paredit' provides structural editing, but I still end up fighting
;; with it. I need to change how I think about code ... sexps and not
;; lines.

;; It also remaps so many keybinds without any real customization
;; opportunities (eg., M-q). Is `lispy' better?

;; (require 'paredit)
;; (require 'paredit-menu)
;; (require 'paredit-everywhere)
;; (diminish 'paredit-mode "PE")
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;; (add-hook 'scheme-mode-hook 'paredit-mode)

;; Parentheses (show-paren-mode)
 (require 'paren)
(add-hook 'prog-mode-hook 'show-paren-local-mode)
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery nil)
(setq show-paren-when-point-inside-paren nil)
(setq show-paren-context-when-offscreen 'overlay)


;; This may be a better reformatter for Lisps. It tries to
;; reposition parentheses in a readable manner.

(require 'adjust-parens)
(add-hook 'emacs-lisp-mode-hook #'adjust-parens-mode)
(add-hook 'scheme-mode-hook #'adjust-parens-mode)
;;  (local-set-key (kbd "TAB") 'lisp-indent-adjust-parens)
;;  (local-set-key (kbd


;;; Smooth scrolling.

(setopt scroll-margin 0)
(setopt scroll-conservatively 100000)
(setopt scroll-preserve-screen-position 1)
;; DO NOT USE (pixel-scroll-precision-mode) DO NOT USE


;;; Enable disabled 'confusing' commands.

;; The Emacs gods don't think we should have access to commands that
;; might confuse us. They mark them disabled and issue an 'are you
;; sure' warning.

;; 'put' is used because these are properties of the function name
;; symbol.

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;; Language modes.


;;; Random 'no' configuration required modes

(require 'cmake-mode)
(require 'ninja-mode)
(require 'git-modes)


;;; C (not C++, C!)

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

;; I use 'astyle' to format C. The configuration goes in .astylerc in
;; my home directory. My formatting is based on the 'linux' and 'k&r'
;; styles.

(require 'reformatter)

(require 'astyle)
(diminish 'astyle-mode "AS")
(when (executable-find "astyle")
  (add-hook 'c-ts-mode-hook 'astyle-on-save-mode)
  (add-hook 'c++-ts-mode-hook  'astyle-on-save-mode))


;;; Odin.

;; Odin mode isn't available as a package yet. I have it copied
;; to my `troi-lisp' directory.

(require 'odin-mode)

(with-eval-after-load 'eglot
                      (add-to-list
                        'eglot-server-programs
                        '(odin-mode . ("ols"))))


;;; Geiser and assorted Schemes.

;; I'm only using Chicken right now. I may switch to Chez after
;; I finish _Simply_Scheme_ and _SICP_.

(require 'geiser)
(require 'geiser-chicken)
(require 'srfi)
(add-hook 'geiser-repl-mode-hook 'electric-pair-local-mode)
(setq geiser-connection-timeout 500)
(setopt geiser-repl-startup-time 500)
(setopt geiser-implementations-alist
        '(((regexp "\\.scm$") chicken)
           ((regexp "\\.ss$") chicken)))

(with-eval-after-load 'flymake
  (require 'flymake-chicken))


;;; Searching, grepping, and the like.

(require 'isearch)

;; "find one two" would find "one two" "one hi there two" etc. one
;; `setq' here to make it obvious these are a group. I liked this but
;; there were times I wanted to turn it off. I haven't figured out a
;; clean way to do that.

;;(setq search-whitespace-regexp ".*?"
;;      isearch-lax-whitespace t
;;      isearch-regexp-lax-whitespace nil)

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

(require 're-builder)
(setq reb-re-syntax 'read)

(require 'grep)
(setq grep-save-buffers nil)
(setq grep-use-headings t) ; Emacs 30

(let ((executable (or (executable-find "rg") "grep"))
      (rgp (string-match-p "rg" grep-program)))
  (setq grep-program executable)
  (setq grep-template
        (if rgp
          "/usr/bin/rg -nH --null -e <R> <F>"
          "/usr/bin/grep <X> <C> -nH --null -e <R> <F>"))
  (setq xref-search-program (if rgp 'ripgrep 'grep)))


;; Text and other settings that haven't fit in anywhere else yet.

;; The `visual-fill-column' package 'narrows' the display when you're
;; using a single window on a wide screen so if you are wrapping text
;; it will wrap at the fill column and not the edge of the screen.
;; It has some interaction with other things that I don't like (mouse)
;; so I'll leave this note about the package here but not load it.
;;
;; As of today, it's not been put up on ELPA. So find it on Github.

;; To my amazement, I actually prefer tabs for C like code.

(setq-default tab-width 8)
(setq-default indent-tabs-mode t)

;; visual line mode is OK for text, use (truncate-lines t) for
;; code.

(setq-default fill-column 70)
(add-hook 'text-mode-hook 'visual-line-mode)

;; Plain text (text-mode)

(require 'text-mode)
;;  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq sentence-end-double-space nil)
(setq sentence-end-without-period nil)
(setq colon-double-space nil)
(setq use-hard-newlines nil)
(setq adaptive-fill-mode t)


;;; As yet uncategorized odds and ends.

(delete-selection-mode +1)
(indent-tabs-mode +1)
(setopt tab-always-indent 'complete)
(setopt comment-empty-lines t)
(setopt require-final-newline t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setopt switch-to-buffer-obey-display-actions t)
(setopt help-window-select t)
(setopt help-window-keep-selected t)
(setopt confirm-kill-emacs 'y-or-n-p)

;; Trailing spaces are evil. One reason I don't like Markdown
;; is that trailing spaces are significant!

(require 'ws-butler)
(diminish 'ws-butler-mode nil)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; I often use C-l for visual breaks.

(require 'form-feed-st)
(diminish 'form-feed-st-mode nil)
(add-hook 'prog-mode-hook 'form-feed-st-mode)
(add-hook 'text-mode-hook 'form-feed-st-mode)


;;; All the keybinds.

;; Show me the keys if I don't know them.

(require 'which-key)
(diminish 'which-key-mode nil)
(add-hook 'emacs-startup-hook 'which-key-mode)

;; `bind-key' is key binding helper that I need to learn how to use.
;; `use-package' counts on it, but it can be used on its own.

(require 'bind-key)

;; Motion and swapping via avy and ace.
(global-set-key (kbd "C-c j") 'avy-goto-line)
(global-set-key (kbd "s-j")   'avy-goto-char-timer)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-o")   'ace-window)

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
;; or string option. Perhaps prefix mode?
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
;;:bind (:map eglot-mode-map
;;          ("C-c c a" . eglot-code-actions)
;;        ("C-c c r" . eglot-rename))


(provide 'init)
;;; File init.el ends here.
