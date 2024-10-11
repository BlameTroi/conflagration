;;; init.el --- troy brumley's init.el -*- lexical-binding: t -*-

;;;
;;; Commentary:
;;;

;; just an 'init.el' file.
;;
;; i'm keeping most real 'init' configuration in this file, but some
;; leaks into (ugh) 'custom.el', but i don't like it. it's good for
;; discovery, but i don't think its maintainable.
;;
;; i expect to periodically go through custom.el and review changes
;; and pull them into this init if they are worth keeping.


;;; to do -- review and maybe include
;; https://codeberg.org/ideasman42/emacs-default-font-presets

;; to do -- pull the font face stuff out of custom.el
;; (set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font Mono" :height 160)
;; '(default ((t (:family "FiraCode Nerd Font Mono" :foundry "nil" :slant normal :weight regular :height 190 :width normal)))))


;;;
;;; Code:
;;;

;; i don't move my init around to foreign systems that don't have
;; current emacs builds. no version checks for backwards compatability
;; here!

(when (< emacs-major-version 29)
  (error "This configuration requires Emacs 29 or newer!"))

;; i don't use terminal emacs. i do try to check display-graphic-p to
;; guard some settings where i think it is needed, but a warning never
;; hurts.

(when (not (display-graphic-p))
  (message "This configuration assumes you are running a GUI Emacs, some things may break")
  (sleep-for 5))

;; while use-package is a built in, you have to require it for some of
;; the macro keywords to process. (this may no longer be needed, but
;; it doesn't seem to hurt so i'll leave it in.)
;;
;; a rare case of setting options outside of a use-package here to
;; make sure they are done early.

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

;; i've been seeing inits with "use-package emacs" for collecting many
;; settings and options. this seems like a good idea so i'm giving it
;; at try.

(use-package emacs

  ;; many of the setq and setopt directives can be lumped under
  ;; :custom, remove "setopt" leaving just (variable-name value).
  ;;
  ;; mode setting and other executable directives belong under
  ;; init:.

  :init

  ;; my additional elisp that doesn't need to be right in the init
  ;; file. this is for work in progress, things that i might autoload,
  ;; and things that aren't in (m)elpa.
  ;;
  ;; at the time of this writing, it's empty, but that may change.

  (add-to-list
   'load-path
   (concat user-emacs-directory "troi-lisp"))

  ;; i know the intelligentsia all use org, but i'm not one of them
  ;; and i don't use org. it invites too much fiddling.
  ;;
  ;; for quick reference, find but do not switch to various things
  ;; i want to access quickly ... i'm regexp impaired, so a cheat
  ;; sheet is mandatory.

  (ignore-errors
    (find-file-noselect "~/notepad/regexp.txt")
    (find-file-noselect "~/notepad/todo.txt")
    (find-file-noselect "~/notepad/cheat-sparen.txt"))

  ;; a scratch buffer to call my own.

  (if (file-exists-p (concat user-emacs-directory "initial-scratch-message.txt"))
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
	(insert-file-contents (concat user-emacs-directory "initial-scratch-message.txt"))
	(set-buffer-modified-p nil))
    (setopt initial-scratch-message ";; nothing to see here, move along"))

  ;; this makes TAB in the minibuffer behave more like it does in a
  ;; shell.

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

  ;; completion: i'm turning off consult and the other stuff from
  ;; bedrock to see how things work with built in completion.

  (fido-vertical-mode +1)


  ;; get file system changes as they happen.

  (global-auto-revert-mode +1)

  ;; spruce up the display. in addition to column number mode
  ;; here, don't forget to set display-line-numbers-width and
  ;; mode-line-position-column-line-format. column in particular
  ;; needs to be "%C" and not "%c" if you want counting to start
  ;; from one the way god intended.

  (when (display-graphic-p)
    (context-menu-mode))
  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (column-number-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (add-hook 'prog-mode-hook 'which-function-mode)

  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

  ;; enable the commands that are disabled (training wheels).

  ;; allow horizontal scrolling with "M-x >"
  (put 'scroll-left 'disabled nil)

  ;; enable narrowing commands
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; enabled change region case commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)


  ;;;
  ;;; collecting all the loose setopt/setq from throughout the old
  ;;; init.
  ;;;

  :custom

  ;; the odd globals

  (user-full-name "Troy Brumley")
  (user-mail-address "BlameTroi@gmail.com")
  (auth-sources '("~/.authinfo.gpg"))
  (auth-source-cache-expiry nil)

  ;; i am not liking visual-line-mode *most* of the time. but leaving
  ;; it on again. as a default it can be toggled by flipping the
  ;; comments on these two lines.

  ;; (truncate-lines t)
  (add-hook 'text-mode-hook 'visual-line-mode)

  ;; i'm used to line numbers from my mainframe days. i prefer that
  ;; they be uniform witdth. %C prints the column number starting from
  ;; 1 and not 0.

  (display-line-numbers-width 4)
  (mode-line-position-column-line-format '(" (%l,%C)"))

  ;; this seems to make scroll feel more vim like.

  (scroll-conservatively 10000)

  ;; i've lost this battle, everthing comes at me with one space so
  ;; we'll deal with it.
  ;;
  ;; at least we still have the oxford comma!

  (sentence-end-double-space nil)

  ;; if i want multiple dired buffers i'll open them explicitly.

  (dired-kill-when-opening-new-dired-buffer t)

  ;; when i open a new buffer in the same frame, or a minibuffer comes
  ;; up, i'm generally going to want it selected. this doesn't work
  ;; all the time, but it's a start.

  (switch-to-buffer-obey-display-actions t)
  (help-window-select t)
  (help-wndow-keep-selected t)
  (enable-recursive-minibuffers t)

  (use-dialog-box nil)

  (indicate-buffer-boundaries t)

  (apropos-sort-by-scores t)

  (blink-matching-delay 0.1)

  (delete-by-moving-to-trash t)

  ;; macos better support notifications.

  (auto-revert-avoid-polling t)

  ;; completion. i didn't like orderless when i tried it so
  ;; going back to alphabetical. i need to figure out how to
  ;; get fuzzy matching. i think flex is what i want.
  ;;
  ;; overview at:
  ;; https://www.masteringemacs.org/article/understanding-minibuffer-completion

  (completions-detailed t)
  (completion-cycle-threshold 1)
  ;; (tab-always-indent 'complete)
  ;; completion styles was basic partial-completion emacs22
  (completion-styles '(flex basic initials substring))
  ;; (completion-auto-help 'always)
  (completions-max-height 10)
  (completions-format 'vertical)
  ;; (completions-group t)
  ;; (completions-group-sort 'alphabetical)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;
  ;; mac os specific changes
  ;; ;;;;;;;;;;;;;;;;;;;;;;;

  ;; mac os is a horse of an entirely different color! these are those
  ;; things that are mac specific. the key remaps for various keys that
  ;; the mac desktop wants are a work in progress. i don't gate these by
  ;; operating system, but they could be if needed.

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
  ;; (ns-alternate-modifier 'alt)
  ;; (ns-command-modifier 'meta)
  ;; (ns-function-modifier 'hyper)
  ;; (ns-right-alternate-modifier 'super)

  ;; hiding the menu bar merely dims it, its space appears to always
  ;; be allocated due to the notch.

  (ns-auto-hide-menu-bar t)

  ;; i use treesitter for everything i can. make sure the grammars are
  ;; built.

  ;; my experience is that levels 1-3 pretty much suck.

  (treesit-font-lock-level 4)

  ;; redirect to treesitter.

  (major-mode-remap-alist '((c-mode . c-ts-mode)
			    (c++-mode . c++-ts-mode)
			    (ruby-mode . ruby-ts-mode)))

  ;; i like big tabs and i can not lie.

  (c-ts-mode-indent-offset 8)
  (c-ts-mode-indent-style 'linux))

;;;
;;; themes and colors and related visuals
;;;

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

;; one of the few themes i prefer over acme-theme. i need to tweak
;; some of the highlight/selection colors, and possibly the cursor
;; (it's pink right now), but it's quite usable.

(use-package tomorrow-night-deepblue-theme
  :pin melpa
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'tomorrow-night-deepblue t))

;; nerd-icons render better than all-the-icons in my opinion.

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

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :diminish
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;
;;; mostly stand alone use-package directives.
;;;

;; some obvious rational things to have in any emacs.

(use-package diminish)
(use-package bind-key)
(use-package free-keys
  :defer t)
(use-package which-key
  :diminish
  :init (which-key-mode));

;; minibuffer history.

(use-package savehist
  :defer t
  :config
  (setopt savehist-additional-variables
          '(compile-command
            kill-ring
            regexp-search-ring))
  (savehist-mode 1))

;; and while we're being historical, remember position in
;; files.

(use-package saveplace
  :config
  (setopt save-place-limit 1000)
  (save-place-mode))

;; sometimes a build of emacs doesn't grab the environment correctly, especially
;; when run from a shortcut on macos. make sure the variables i count on are
;; set to match a login shell.

(use-package exec-path-from-shell
  :init
  (declare-function exec-path-from-shell-initialize "exec-path-from-shell" ())
  (declare-function exec-path-from-shell-copy-envs "exec-path-from-shell")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH" "CDPATH"))

  ;; use gls if it's around. the macos supplied ls doesn't suppport all
  ;; the options dired wants.

  (when (executable-find "gls")
    (setopt insert-directory-program "gls")))

;; documentation with info and eldoc. for some reason i'm missing
;; system info from homebrew. i should probably move this into my
;; zshenv.

(use-package info
  :after exec-path-from-shell
  :custom
  (Info-additional-directory-list '("/opt/homebrew/share/info")))

(use-package eldoc
  :diminish
  :init (global-eldoc-mode))

;; i often use C-l for visual breaks.

(use-package form-feed-st
  :diminish
  :hook (prog-mode . form-feed-st-mode) (text-mode . form-feed-st-mode))

;; alfred, take care of that will you?

(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

(use-package so-long
  :config
  (global-so-long-mode))

;;(with-eval-after-load 'eglot
;; (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

;; writeable grep buffer lets you change result lines and save the
;; result to the replace the matched line.
;;
;; i'm not sure i'll use this, i've been using grep as a navigation
;; aid only, and not very often thanks to dumpjump and xref.

(use-package wgrep
  :ensure t
  ;; :bind (:map grep-mode-map
  ;;             ("C-x C-q" . wgrep-change-to-wgrep-mode))
  :config
  (setopt wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :after wgrep)

;; imenu-list puts imenu in a sidebar. very handy.

(use-package imenu-list
  :diminish
  :bind ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

;; side-notes lets you have notes files (name is a convention) in
;; any project or directory. the notes go into a side window like
;; imenu-list.

(use-package side-notes
  :diminish
  :bind ("M-s n" . side-notes-toggle-notes)
  :custom
  (side-notes-file "side-notes.txt")
  (side-notes-secondary-file "~/general-side-notes.txt"))

;; ztree is a tree command for emacs, producing an outline tree of the
;; directories under the selected directory.

(use-package ztree)

;; dot-mode brings the vim '.' to emacs!
;;
;; i may want to customize the keybinds. C-M-. conflicts with
;; xref-find-apropos.

(use-package dot-mode
  :diminish
  :init
  (declare-function global-dot-mode "dot-mode")
  (global-dot-mode t))

;; moving at the speed of 'what was that keybind again?'.

(use-package avy
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

(use-package ace-window
  :after avy
  :bind (("C-x o" . ace-window)
	 ("M-o" . ace-window)))

;; smartparens, i should really learn to use this more and better.

(use-package smartparens
  :defer
  :diminish "SP"
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; dumb-jump is a wonderful dwim for xref.

(use-package dumb-jump
  :hook
  (xref-backend-functions . dump-jump-xref-activate))

;; this looks like the start of the note system i've wanted to write
;; since forever.

(use-package deft
  :defer t
  :config
  (setopt deft-directory
	  (expand-file-name "~/Notepad"))
  (setopt deft-text-mode (seq-find 'fboundp '(markdown-mode text-mode)))
  (setopt deft-extension
        (assoc-default deft-text-mode '((markdown-mode . "md"))
                       'eq "txt"))

  ;; Completely override Deft's keybindings to be more like Dired and
  ;; Gnus:
  ;; (setq deft-mode-map (make-sparse-keymap))
  ;; (define-key deft-mode-map (kbd "a") 'deft-new-file-named)
  ;; (define-key deft-mode-map (kbd "d") 'deft-delete-file)
  ;; (define-key deft-mode-map (kbd "g") 'deft-refresh)
  ;; (define-key deft-mode-map (kbd "n") 'next-line)
  ;; (define-key deft-mode-map (kbd "q") 'quit-window)
  ;; (define-key deft-mode-map (kbd "p") 'previous-line)
  ;; (define-key deft-mode-map (kbd "r") 'deft-rename-file)
  ;; (define-key deft-mode-map (kbd "s") 'deft-filter)
  ;; (define-key deft-mode-map (kbd "z") 'deft-filter-clear)
  ;; (define-key deft-mode-map (kbd "<RET>") 'deft-complete)
  )

;; use astyle to do formatting for c. i have an .astylerc set up with
;; options that match troi-c-style.

(use-package reformatter
  :after exec-path-from-shell)

(use-package astyle
  :after reformatter
  :when (executable-find "astyle")
  :diminish (astyle-on-save-mode . "as")
  :hook
  (c-ts-mode . astyle-on-save-mode)
  (c++-ts-mode . astyle-on-save-mode))

;;;
;;; additional file modes
;;;

;; bsd make is built in, cmake is not.

(use-package cmake-mode
  :defer t)

;; ninja is a make alternative. it runs like a bat out of hell but
;; isn't meant to be manually configured. cmake builds the .ninja
;; files.
;;
;; did i mention that it's like crazy fast?

(use-package ninja-mode)

;; i don't use markdown much, but if i have a file i'll accept it
;; being formatted.

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist
	       '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist
	       '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;; going for a minimal ruby setup with a repl available. i've
;; left some of the settings i found on the web as comments.

(use-package ruby-ts-mode
  :after treesit
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")
;;  :hook (ruby-ts-mode . subword-mode))
;; :bind (:map ruby-ts-mode-map
;;             ("C-c r b" . 'treesit-beginning-of-defun)
;;             ("C-c r e" . 'treesit-end-of-defun))
;; :custom
;; (ruby-indent-level 2)
;; (ruby-indent-tabs-mode nil))

;; ruby repl
;;
;; not sure if i should add the following
;; (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
;; (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter-and-focus)
;; (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(use-package inf-ruby
  :defer t)

;;;
;;; keybinds not in :bind clauses
;;;

;; i don't think i change any from their expected usage, just
;; replace what they do (ibuffer instead of buffer).

;; s-q to close emacs if i don't want to M-x save-buffers-kill-emacs.

(global-unset-key (kbd "C-x C-c"))

;; the number of times i want a dired list instead of the dired smart
;; buffer is zero.

(global-set-key (kbd "C-x C-d") 'dired)

;; ditto with buffers.

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; i still C-x o, but not a often

(global-set-key (kbd "M-o") 'other-window)

;; this is how i expect it to work.

(global-set-key "\M-z" 'zap-up-to-char)

;; default search to regexp instead of string.

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;;;
;;; functions
;;;

;; collecting most function definitions here. i prefix my functions
;; as troi/.

;; this is not mine, i stole from tess o'connor's config.
;; C-l a few times also moves the line to the top.

(defun troi/clear (&optional prefix)
  "Move the line containing point to the top of the window.
With PREFIX, move the line containing point to line PREFIX of the window."
  (interactive "P")
  (recenter (or prefix 0)))

(global-set-key (kbd "C-c c") 'troi/clear)

;; backups are a pain in the ass. sure, they are needed but let's
;; segregate them by collecting them in one place

(defun troi/backup-file-name (fpath)
  "Return a new file path of FPATH, creating directories if needed."
  (let* ((backup-root-dir "~/.tmp/emacs-backup/")
         (backup-file-path (replace-regexp-in-string "//" "/" (concat backup-root-dir fpath "~") )))
    (make-directory (file-name-directory backup-file-path) (file-name-directory backup-file-path))
    backup-file-path))
(setopt make-backup-file-name-function 'troi/backup-file-name)

;; from a multi-window frame, tear off the current window and
;; put it in a new frame. this only works if there are multiple
;; windows in the current frame.

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
;;; programming mode common configuration and helpers
;;;

;; discoverability via go to definition/references and xref seems to
;; work best with eglot instead of the various tagging options.  less
;; configuration and better dwim.

(use-package eglot
  :after exec-path-from-shell
  :pin gnu
  ;; :init
  ;; this breaks flymake for c, but at one time it was needed for
  ;; ruby. not sure if it still is, but i don't do much in the
  ;; way of ruby.
  ;; (setopt eglot-stay-out-of '(flymake))
  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  ;;  (ruby-ts-mode . eglot-ensure) ;; does not behave as expected
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c r" . eglot-rename))
  :custom
  ;; log size 0 disables logging which improves performance
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider)))

;; configure clangd for eglot to my preferences. i was able to avoid
;; the maze of (apparently) cmake generated files for clangd with these
;; options.

(with-eval-after-load 'eglot
  ;; (setopt completion-category-defaults nil)
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
  :after exec-path-from-shell
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
  :after exec-path-from-shell
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

;; ruby is a separate package and something isn't working right.

;; (use-package flymake-ruby
;;   :after flymake
;;   :hook (ruby-mode . flymake-ruby-load)
;;   (ruby-ts-mode . flymake-ruby-load)
;;   )

;; this is needed to avoid false 'can not find/load' errors on
;; requires that occur before this point in the source.

(with-eval-after-load 'flymake
  (setopt elisp-flymake-byte-compile-load-path load-path))

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

;;;
;;; collection of stuff i've commented out
;;;
;;; custom file loading follows, keeping it at the end of init.el.
;;;

;; turning these off and working through the built in stuff
;; to see if i really need this.

;; (use-package consult
;;   :ensure t
;;   :bind (
;;          ;; Drop-in replacements
;;          ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
;;          ("M-y"   . consult-yank-pop)   ; orig. yank-pop
;;          ;; Searching
;;          ("M-s r" . consult-ripgrep)
;;          ("M-s l" . consult-line)       ; alternative: rebind C-s to use
;;          ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
;;          ("M-s L" . consult-line-multi) ; isearch to M-s s
;;          ("M-s o" . consult-outline)    ; was occur regexp
;;          ;; Isearch integration
;;          :map isearch-mode-map
;;          ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
;;          ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
;;          ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
;;          ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
;;          )
;;   :config
;;   (setopt consult-narrow-key "<"))

;; (use-package vertico
;;   :ensure t
;;   :init
;;   (declare-function vertico-mode "vertico")
;;   (vertico-mode))

;; (use-package vertico-directory
;;   :ensure nil
;;   :after vertico
;;   :bind (:map vertico-map
;;               ("M-DEL" . vertico-directory-delete-word)))

;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (declare-function marginalia-mode "marginalia")
;;   (marginalia-mode))

;; (use-package corfu
;;   :ensure t
;;   :init
;;   (declare-function global-corfu-mode "corfu")
;;   (global-corfu-mode)
;;   :bind
;;   (:map corfu-map
;;         ("SPC" . corfu-insert-separator)
;;         ("C-n" . corfu-next)
;;         ("C-p" . corfu-previous)))

;; (use-package corfu-popupinfo
;;   :after corfu
;;   :ensure nil
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :custom
;;   (corfu-popupinfo-delay '(0.25 . 0.1))
;;   (corfu-popupinfo-hide nil)
;;   :config
;;   (corfu-popupinfo-mode))

;; (use-package nerd-icons-corfu
;;   :after nerd-icons corfu
;;   :diminish
;;   :config
;;   (declare-function nerd-icons-corfu-formatter "nerd-icons-corfu")
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; i don't run in terminal, so the corfu terminal popup support isn't needed

;; completion at point, highly configurable, this is minimal
;; (use-package cape
;;   :ensure t
;;   :init
;;   (declare-function cape-dabbrev "cape")
;;   (declare-function cape-file "cape")
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file))

;; (use-package orderless
;;   :ensure t
;;   :config
;;   (setq completion-styles '(orderless)))

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
;;; init.el ends here
