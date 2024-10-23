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
;; i expect to periodically go through 'custom.el' and review changes
;; and pull them into this init if they are worth keeping.

;; i am still not happy with completion, and i'm having issues getting
;; c eglot and cmake to play nice together for me. more surgery
;; incoming!

;;;
;;; Code:
;;;

;; i don't move my init around to foreign systems that don't have
;; current emacs builds.

(when (< emacs-major-version 29)
  (error "This configuration requires Emacs 29 or newer!"))

;; i don't use terminal emacs. i do try to check 'display-graphic-p' to
;; guard some settings where i think it is needed, but a warning never
;; hurts.

(when (not (display-graphic-p))
  (message "This configuration assumes you are running a GUI Emacs, some things may break.")
  (sleep-for 5))

;;;
;;; scaffolding
;;;

;; a rare case of setting options outside of 'use-package' stanzas to
;; make sure they are done early.

(eval-when-compile
  (require 'use-package))
(setopt load-prefer-newer t)
(setopt use-package-always-ensure t)

;; if native compilation is available, use it.

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

;;;
;;; general emacs configuration
;;;

;; i do as much inside 'use-package' stanzas as i can. this resolves
;; ()by removing) the debate about where to put a 'setopt'.

(use-package emacs

  ;;;
  ;;; begin :init
  ;;;

  :init

  ;; add the (usually empty )directory for my own elisp to the
  ;; 'load-path'.

  (add-to-list
   'load-path
   (concat user-emacs-directory "troi-lisp"))

  ;; a scratch buffer to call my own.

  (if (file-exists-p
       (concat user-emacs-directory
	       "initial-scratch-message.txt"))
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
	(insert-file-contents (concat user-emacs-directory
				      "initial-scratch-message.txt"))
	(set-buffer-modified-p nil))
    (setopt initial-scratch-message ";; nothing to see here, move along"))

  ;; this makes TAB in the minibuffer behave more like it does in a
  ;; shell.
  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

  ;; i don't tend to use a mouse on my laptop, but enable the
  ;; context menu anyway.

  (when (display-graphic-p)
    (context-menu-mode))

  ;; i go back and forth on scroll bars.

  (when scroll-bar-mode
    (scroll-bar-mode -1))

  ;; in addition to column number mode here, don't forget
  ;; to set both the 'display-line-numbers-width' and the
  ;; 'mode-line-position-column-line-format'.
  ;;
  ;; column in particular needs to be "%C" and not "%c" if
  ;; you want counting to start from one the way the god's
  ;; intended.

  (column-number-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; this keeps me from scrolling back to answer the 'which function
  ;; am i in' question.

  (add-hook 'prog-mode-hook 'which-function-mode)

  ;; better than hunting for the cursor is hl-line-mode

  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

  ;; this enables some of the commands that are disabled by default in
  ;; emacs.

  (put 'scroll-left 'disabled nil)       ; allow horizonal scrolling with C-x <>
  (put 'narrow-to-region 'disabled nil)  ; narowing commands
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'upcase-region 'disabled nil)     ; case change
  (put 'downcase-region 'disabled nil)

  ;; visual line mode (with or without word wrap) vs truncation...
  ;; i can't settle on how i want this configured yet.
  ;;
  ;; toggle the commenting of these two lines to switch.
  ;; (truncate-lines t)
  (add-hook 'text-mode-hook 'visual-line-mode)

  ;; this is too ingrained to go back to the original emacs way.

  (delete-selection-mode +1)

  ;; one of the things 'golang' got right.
  (indent-tabs-mode +1)

  ;; keyboards and keybinds:
  ;;
  ;; in addition to changing caps lock to control, a standard (non mac)
  ;; keyboard has on the bottom row:
  ;;
  ;; fn control os alt |spacebar| alt(gr) os menu control
  ;;
  ;; using karbiner i've remapped the bottom row and removed the
  ;; need for the 'ns-*-modifier' remappings.
  ;;
  ;; fn       -> control
  ;; control  -> fn
  ;; option   -> command    (ie., super)
  ;; command  -> alt
  ;; spacebar -> unchanged
  ;; command  -> unchanged
  ;; option   -> unchanged

  ;; use s-q to close emacs if i don't want to M-x
  ;; save-buffers-kill-emacs.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ; Because `view-hello-file` is slow, we unbind the keybind that could accidentally trigger it ;;
;; (define-key global-map (kbd "C-h h") nil)							   ;;
;; 												   ;;
;; ; Unbind Emacs news hotkeys									   ;;
;; (define-key global-map (kbd "C-h n") nil)							   ;;
;; (define-key global-map (kbd "C-h C-n") nil)							   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these go elsewhere, just dropping here for now		    ;;
;; (setq flymake-no-changes-timeout 1.0)			    ;;
;; 								    ;;
;; (setq global-eldoc-mode t)					    ;;
;; (setq eldoc-idle-delay 0.5)					    ;;
;; (setq eldoc-echo-area-prefer-doc-buffer t)			    ;;
;; (setq eldoc-documentation-strategy 'eldoc-documentation-compose) ;;
;; (setq eldoc-echo-area-use-multiline-p nil)			    ;;
;; 								    ;;
;; (global-set-key (kbd "M-RET") 'eglot-rename)			    ;;
;; (global-set-key (kbd "M-,") 'xref-find-definitions-other-window) ;;
;; (global-set-key (kbd "M-.") 'xref-find-definitions)		    ;;
;; (global-set-key [C-mouse-1] 'xref-find-defintions-at-mouse)	    ;;
;; (global-set-key (kbd "C-c ?") 'eldoc-print-current-symbol-info)  ;;
;; (global-set-key (kbd "C-M-.") 'consult-eglot-symbols)	    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package eglot						     ;;
;;   ; ...							     ;;
;;   :bind (:map eglot-mode-map					     ;;
;; 	      ("C-c l a" . eglot-code-actions)			     ;;
;; 	      ("C-c l r" . eglot-rename)			     ;;
;; 	      ("C-c l h" . eldoc)				     ;;
;; 	      ("C-c l f" . eglot-format)			     ;;
;; 	      ("C-c l F" . eglot-format-buffer)			     ;;
;; 	      ("C-c l d" . xref-find-definitions-at-mouse)	     ;;
;; 	      ;; sometimes ionide acts up			     ;;
;; 	      ("C-c l R" . eglot-reconnect))			     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; eglot-rename
  ;; eglot-format
  ;; eglot-code-actions -organize-imports, -quickfix, -extract, -inline, -rewrite
  ;; eglot-inlay-hints-mode
  ;; eldoc
  ;; flymake-show-buffer-diagnostics, -project-diagnostics
  ;; xref-find-definitions
  ;; imenu
  ;; completion-at-point
  ;; treesit-beginning-of-defun, -end-
  ;; treesit-forward-sexp
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; c ts mode ... same for emacs lisp mode With modifier C (21 free)                 ;;
  ;; and deft mode and text mode                                                      ;;
  ;; With modifier C (21 free)
  ;; =========================							    ;;
  ;; C-!     C-%     C-&     C-(     C-=     C-}     C-:     C-|     C-<     C-`      ;;
  ;; C-#     C-^     C-*     C-)     C-{     C-;     C-"     C-,     C->     C-~      ;;
  ;; C-$     									    ;;
  ;; 										    ;;
  ;; With modifier M (30 free)							    ;;
  ;; =========================							    ;;
  ;; M-A     M-D     M-G     M-J     M-M     M-P     M-S     M-V     M-Z     M-[      ;;
  ;; M-B     M-E     M-H     M-K     M-N     M-Q     M-T     M-W     M-#     M-]      ;;
  ;; M-C     M-F     M-I     M-L     M-O     M-R     M-U     M-Y     M-*     M-"      ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (leader-def							   ;;
  ;;         :keymaps 'eglot-mode-map				   ;;
  ;;         :wk-full-keys nil					   ;;
  ;;         "g"     (cons "lsp" (make-sparse-keymap))		   ;;
  ;;         "ge" '(eldoc :wk "errors")				   ;;
  ;;         "ga" '(eglot-code-actions :wk "code actions")	   ;;
  ;;         "gR" '(eglot-rename :wk "lsp rename")		   ;;
  ;;         "gd" '(xref-find-definitions :wk "definitions")	   ;;
  ;;         "gD" '(xref-find-declaration :wk "declaration")	   ;;
  ;;         "gr" '(xref-find-references :wk "references")	   ;;
  ;;         "gt" '(eglot-find-typeDefinition :wk "type definition") ;;
  ;;         "gi" '(eglot-find-implementation :wk "implementation")  ;;
  ;;         )							   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ransient							        ;;
;; 								        ;;
;; (setup transient						        ;;
;;   (require 'transient)					        ;;
;;   (:with-map transient-base-map				        ;;
;;     (:bind "<escape>" transient-quit-one)))			        ;;
;; g in normal mode						        ;;
;; 								        ;;
;; (transient-define-prefix g-extra-commands()			        ;;
;;   "Define notes leader-key maps"				        ;;
;;   [["Code find"						        ;;
;;     ("d" "find-definitions" xref-find-definitions)		        ;;
;;     ("D" "find-references" xref-find-references)		        ;;
;;     ("i" "find-impl" eglot-find-implementation)		        ;;
;;     ("s" "find-symbols" xref-find-apropos)			        ;;
;;     ("o" "find-def-other-window" xref-find-definitions-other-window) ;;
;;     ]							        ;;
;;    ["Code action"						        ;;
;;     ("a" "code-actions" eglot-code-actions)			        ;;
;;     ("r" "rename" eglot-rename)				        ;;
;;     ("f" "format-all-region" format-all-region)		        ;;
;;     ("F" "format-all-buffer" format-all-buffer)]		        ;;
;;    ["diagnostic"						        ;;
;;     ("n" "jump-to-next-diagnostic" flymake-goto-next-error)	        ;;
;;     ("N" "jump-to-prev-diagnostic" flymake-goto-prev-error)	        ;;
;;     ("l" "list-diagnostics" consult-flymake)			        ;;
;;     ]							        ;;
;;    ["Navigate"						        ;;
;;     ("m" "consult-mark" consult-mark)			        ;;
;;     ]							        ;;
;;    ["Clue"							        ;;
;;     ("y" "clue-copy" clue-copy)				        ;;
;;     ("p" "clue-yank" clue-paste)				        ;;
;;     ]							        ;;
;;    ["citre"							        ;;
;;     ("c d" "citre-jump" citre-jump)				        ;;
;;     ("c D" "citre-jump-to-reference" citre-jump-to-reference)        ;;
;;     ("c p" "citre-peek" citre-peek)				        ;;
;;     ("c P" "citre-peek-reference" citre-peek-reference)	        ;;
;;     ("c r" "citre-peek-restore" citre-peek-restore)		        ;;
;;     ("c a" "citre-ace-peek" citre-ace-peek)			        ;;
;;     ("c u" "update-tags-file" citre-update-this-tags-file)	        ;;
;;     ("c s" "peek-save-session" citre-peek-save-session)	        ;;
;;     ("c l" "peek-load-session" citre-peek-load-session)	        ;;
;;     ]							        ;;
;;    ])							        ;;
;; find-file							        ;;
;; 								        ;;
;; (transient-define-prefix file-leader-map()			        ;;
;;   "Define leader-key map for file-related functions"		        ;;
;;   [["config"							        ;;
;;     ("p" "personal emacs config" (lambda()			        ;;
;;                                    (interactive)		        ;;
;;                                    (find-file nowis-config-file)))   ;;
;;     ("e" "emacs documents" (lambda()				        ;;
;;                              (interactive)			        ;;
;;                              (find-file nowis-doc-emacs-dir)))       ;;
;;     ("d" "dot files" (lambda()				        ;;
;;                        (interactive)				        ;;
;;                        (find-file "~/dotfiles")))		        ;;
;;     ]							        ;;
;;    ["history"						        ;;
;;     ("r" "recent file" consult-recent-file)]])		        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;
  ;;; end :init
  ;;;



  ;;;
  ;;; begin :custom
  ;;;

  :custom

  ;; the odd globals

  (user-full-name "Troy Brumley")
  (user-mail-address "BlameTroi@gmail.com")
  (auth-sources '("~/.authinfo.gpg"))
  (auth-source-cache-expiry nil)

  ;; i'm used to line numbers from my mainframe days. i prefer that
  ;; they be uniform witdth. %C prints the column number starting from
  ;; 1 and not 0.

  (display-line-numbers-width 4)
  (mode-line-position-column-line-format '(" (%l,%C)"))

  ;; this seems to make scroll feel more vim like.
  ;; nice scrolling
  (scroll-margin 0)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1)

  ;; i've lost this battle, everthing comes at me with one space so
  ;; we'll deal with it. at least we still have the oxford comma!

  (sentence-end-double-space nil)

  ;; if the file has newlines, it should end with one.
  (require-final-newline t)

  ;; when i open a new buffer in the same frame, or a minibuffer comes
  ;; up, i'm generally going to want it selected.
  ;;
  ;; this doesn't work all the time, but it's a start.

  (switch-to-buffer-obey-display-actions t)
  (help-window-select t)
  (help-wndow-keep-selected t)
  (enable-recursive-minibuffers t)

  ;; miscellany

  (use-dialog-box nil)
  (indicate-buffer-boundaries t)
  (apropos-sort-by-scores t)
  (blink-matching-delay 0.1)
  (delete-by-moving-to-trash t)

  ;; completion settings.

  ;; txb: wip
  (completions-detailed t)
  ;;(completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  ;; (tab-first-completion 'word-or-paren-or-punct) ;; only takes effect if tab-always-indent is 'complete
  ;; completion styles was basic partial-completion emacs22
  ;;(completion-styles '(flex basic initials))
  ;; (completion-auto-help 'always)
  (completions-max-height 8)
  (completions-format 'vertical)
  ;;(completions-group t)
  ;; (completions-group-sort 'alphabetical)

  ;; more miscellany

  (comment-empty-lines t)
  (comment-style 'box-multi)
  (confirm-kill-emacs 'y-or-n-p)

  ;;; mac os specific changes.

  (ns-auto-hide-menu-bar t)           ; this gains no space on displays with the notch

  ;; to say that treesitter integration isn't seamless would be
  ;; an understatement. 'treesit-auto' can build the grammars
  ;; for languages as you need them.

  (treesit-font-lock-level 4)           ; levels 1-3 are useless

  (major-mode-remap-alist               ; redirect to the treesitter enabled modes
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode)
     (ruby-mode . ruby-ts-mode)))

  (c-ts-mode-indent-offset 8)           ; turns out i like tabs, who knew?
  (c-ts-mode-indent-style 'linux)

  (standard-indent 8)

  ;;;
  ;;; end custom
  ;;;
  )

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

;; one of the few themes i prefer over 'acme-theme'. i need to tweak
;; some of the highlight/selection colors, and possibly the cursor
;; (it's pink right now), but it's quite usable.

(use-package tomorrow-night-deepblue-theme
  :pin melpa
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'tomorrow-night-deepblue t))

;; 'nerd-icons' render better than 'all-the-icons'.

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
  (nerd-icons-completion-mode))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion") ;;
  ;; (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))	       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :diminish
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;
;;; stand alone package configuration.
;;;

(use-package tldr
  :defer t)

(use-package show-font
  :defer t
  :bind
  (("C-c s f" . show-font-select-preview)
   ("C-c s l" . show-font-list))
  )

(use-package autorevert
  :config

  ;; maybe only use for vc managed files? see magit-auto-rever-mode
  ;; if i start using magit.

  ;; revert buffers automatically when underlying files are changed.
  (global-auto-revert-mode +1)

  :custom

  ;; macos better support notifications.
  (auto-revert-avoid-polling t)

  ;; auto revert dired buffers -- see also use-package dired
  (global-auto-revert-non-file-buffers t)

  ;; turn off auto revert messages?
  (auto-revert-verbose nil)
  )

;; links in buffers become buttons.

(use-package goto-addr
  :hook
  (((text-mode . goto-address-mode))
   ((compilation-mode prog-mode eshell-mode) . goto-address-prog-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline)))

;; some obvious rational things to have in any emacs.

(use-package diminish)
(use-package bind-key)
(use-package free-keys
  :defer t)
(use-package which-key
  :diminish
  :init (which-key-mode));

;; remember where i was and what i did.

(use-package savehist
  :defer t
  :config
  (setopt savehist-additional-variables
          '(compile-command
            kill-ring
            regexp-search-ring))
  (savehist-mode 1))

;; and while we're being historical, remember our position in files.

(use-package saveplace
  :config
  (save-place-mode)
  :custom
  (save-place-limit 1000))

;; sometimes a build of emacs doesn't grab the environment correctly,
;; especially when run from a shortcut on macos. make sure the
;; variables that i count on are set to match my shell login.

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
     "CDPATH")))

;; for dired, use 'gls' if it's available. the 'ls' in macos and some
;; other systems doesn't support all the options that 'dired' wants.

(use-package dired
  :after exec-path-from-shell
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  ;;  :hook ((dired-mode . (lambda () (local-unset-key (kbd "C-t"))))) ; image-dired
  :config
  (setopt
   dired-recursive-copies  'always
   dired-isearch-filenames 'dwim)
  (when (executable-find "gls")
    ;; Use GNU ls when possible.
    (setopt dired-use-ls-dired nil)
    (setopt ls-lisp-use-insert-directory-program t)
    (setopt insert-directory-program "gls")
    (setopt dired-listing-switches "-alh --group-directories-first")
    (setopt dired-kill-when-opening-new-dired-buffer t)
    (setopt dired-auto-revert-buffer t)
    (setopt dired-do-revert-buffer t)))

;; documentation with 'info' and 'eldoc'. for some reason i'm missing
;; system info from homebrew. i should probably move this into my
;; zshenv.

(use-package info
  :after exec-path-from-shell
  :custom
  (Info-additional-directory-list '("/opt/homebrew/share/info")))

(use-package eldoc
  :diminish
  :config (global-eldoc-mode))

;; i often use C-l for visual breaks.

(use-package form-feed-st
  :diminish
  :hook (prog-mode . form-feed-st-mode) (text-mode . form-feed-st-mode))

;; alfred, take care of that will you?

(use-package ws-butlerp
  :diminish
  :hook (prog-mode . ws-butler-mode))

;; 'so-long' handles long lines that are usually found in program
;; source code where unneeded whitespace has been removed. forcing
;; paragraph text direction is reported to also help by removing the
;; checks and scans done for right to left languages.

(use-package so-long
  :config
  (global-so-long-mode)
  :custom
  (bidi-paragraph-direction 'left-to-right))

;; writeable 'grep' buffer lets you change result lines and save the
;; result to the replace the matched line.
;;
;; i'm not sure i'll use this, i've been using 'grep' as a navigation
;; aid only, and not very often thanks to 'dumpjump' and 'xref'.

(use-package wgrep
  :ensure t
  ;; :bind (:map grep-mode-map
  ;;             ("C-x C-q" . wgrep-change-to-wgrep-mode))
  :config
  (setopt wgrep-auto-save-buffer t))

;; 'imenu-list' puts 'imenu' in a sidebar.

(use-package imenu-list
  :diminish
  :bind ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

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

;; 'ztree' is a tree command for emacs.

(use-package ztree)

;; 'dot-mode' brings the vim '.' to emacs.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'key-chord)									     ;;
;; 											     ;;
;; (key-chord-define-global "jj" 'avy-goto-word-1)					     ;;
;; (key-chord-define-global "jl" 'avy-goto-line)					     ;;
;; (key-chord-define-global "jk" 'avy-goto-char)					     ;;
;; (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)			     ;;
;; (key-chord-define-global "uu" 'undo-tree-visualize)					     ;;
;; (key-chord-define-global "xx" 'execute-extended-command)				     ;;
;; (key-chord-define-global "yy" 'browse-kill-ring)					     ;;
;; 											     ;;
;; (defvar key-chord-tips '("Press <jj> quickly to jump to the beginning of a visible word." ;;
;;                          "Press <jl> quickly to jump to a visible line."		     ;;
;;                          "Press <jk> quickly to jump to a visible character."	     ;;
;;                          "Press <JJ> quickly to switch to previous buffer."		     ;;
;;                          "Press <uu> quickly to visualize the undo tree."		     ;;
;;                          "Press <xx> quickly to execute extended command."		     ;;
;;                          "Press <yy> quickly to browse the kill ring."))		     ;;
;; 											     ;;
;; (key-chord-mode +1)									     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :after avy
  :bind (("C-x o" . ace-window)
	 ("M-o" . ace-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; hippie expand is dabbrev expand on steroids			       ;;
;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev		       ;;
;;                                          try-expand-dabbrev-all-buffers     ;;
;;                                          try-expand-dabbrev-from-kill       ;;
;;                                          try-complete-file-name-partially   ;;
;;                                          try-complete-file-name	       ;;
;;                                          try-expand-all-abbrevs	       ;;
;;                                          try-expand-list		       ;;
;;                                          try-expand-line		       ;;
;;                                          try-complete-lisp-symbol-partially ;;
;;                                          try-complete-lisp-symbol))	       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; let's get better sorting of completion lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package vertico					     ;;
;;   :ensure t						     ;;
;;   :init						     ;;
;;   (declare-function vertico-mode "vertico")		     ;;
;;   (vertico-mode))					     ;;
;; 							     ;;
;; (use-package vertico-directory			     ;;
;;   :ensure nil					     ;;
;;   :after vertico					     ;;
;;   :bind (:map vertico-map				     ;;
;;               ("M-DEL" . vertico-directory-delete-word))) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smartparens. leaving off until i start doing some elisp or scheme
;; work.
;;
;; (use-package smartparens
;;   :defer
;;   :diminish "SP"
;;   :hook (prog-mode text-mode markdown-mode)
;;   :config
;;   (require 'smartparens-config))

;; 'dumb-jump' is a dwim for 'xref'.

(use-package dumb-jump
  :hook
  (xref-backend-functions . dump-jump-xref-activate))

;; i know the intelligentsia all use 'org', but i don't. 'org' invites
;; too much fiddling. 'deft' provides a lightweight solution that
;; provides the indexing and access i want.

(use-package deft
  :defer t
  :config
  (setopt deft-directory
	  (expand-file-name "~/Notepad"))
  (setopt deft-text-mode (seq-find 'fboundp '(markdown-mode text-mode)))
  (setopt deft-extension
        (assoc-default deft-text-mode '((markdown-mode . "md") (rst-mode . "rst"))
                       'eq "txt")))

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
;;; programming mode common configuration and helpers
;;;

;; use 'astyle' to do formatting for c. i have an '.astylerc' in my home
;; for this.

(use-package reformatter
  :after exec-path-from-shell)

(use-package astyle
  :after reformatter
  :when (executable-find "astyle")
  :diminish (astyle-on-save-mode . "as")
  :hook
  (c-ts-mode . astyle-on-save-mode)
  (c++-ts-mode . astyle-on-save-mode))

;; magit, i can't escape it forever

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; language: cmake
;;
;; bsd make is built in, cmake is not.

(use-package cmake-mode
  :defer t)

;; language: ninja
;;
;; ninja is a make alternative. it runs like a bat out of hell but
;; isn't meant to be manually configured. cmake builds the .ninja
;; files.

(use-package ninja-mode
  :defer t)

;; language: ruby
;;
;; i'm using ruby as i work through a text, and i also used it briefly
;; in a programming languages course. here is a minimal configuration,
;; followed by configurations that i could not get to coexist with c,
;; which is my primary language.

(use-package ruby-ts-mode
  :defer t
  :after treesit
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")

(use-package inf-ruby
  :defer t)


;; discoverability via go to definition/references and xref seems to
;; work best with 'eglot' instead of the various tagging options.

(use-package eglot
  :after exec-path-from-shell

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; :hook			  ;;
  ;; (c-ts-mode . eglot-ensure)	  ;;
  ;; (c++-ts-mode . eglot-ensure) ;;
  ;; (f90-mode . eglot-ensure)	  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c r" . eglot-rename))

  :custom
  ;; log size 0 disables logging effectively disables logging
  ;; which improves performance. remove if debugging 'eglot'
  ;; issues.
  ;; log while getting clangd configured (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider)))

;; configure 'clangd' for 'eglot' to my preferences.

(with-eval-after-load 'eglot
  ;; (setopt completion-category-defaults nil)
  (add-to-list
   'eglot-server-programs
   '((c-ts-mode c++-ts-mode)
     . ("clangd"
        "-j=4"))))                        ; concurrency
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; "--log=error"						 ;;
        ;; "--background-index"						 ;;
        ;; "--clang-tidy"                ; but i use 'astyle' to format. ;;
        ;; "--completion-style=detailed"				 ;;
        ;; "--pch-storage=memory"					 ;;
        ;; "--header-insertion=never"					 ;;
        ;; "--header-insertion-decorators=0"))))			 ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;; language: markdown
;;
;; i usually just use plain text, but enough markdown is around
;; to warrant a mode.

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer t
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


;; 'flymake' is has been a good linter interface. 'eglot' seems to
;; report issues from 'clang-tidy' through 'flymake'.

(use-package flymake
  :after exec-path-from-shell
  :pin gnu
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
;; flymake wants makefiles but i am using cmake, there are a few
;; packages that attempt to address this. another option may be
;; to just generate the makefiles manually with cmake at the
;; command line.

;; i'm not sure this is really helping, but we'll see.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package cmake-project											   ;;
;;   :defer													   ;;
;;   :config													   ;;
;;   ;; this function is from the project documentation. i've changed the					   ;;
;;   ;; hook to the ts functions.										   ;;
;;   (defun troi/maybe-cmake-project-mode ()									   ;;
;;     (if (or (file-exists-p "CMakeLists.txt")									   ;;
;;             (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))	   ;;
;; 	(cmake-project-mode)))											   ;;
;;   (add-hook 'c-ts-mode-hook 'troi/maybe-cmake-project-mode)							   ;;
;;   (add-hook 'c++-ts-mode-hook 'troi/maybe-cmake-project-mode)						   ;;
;;   :custom													   ;;
;;   (cmake-project-default-build-dir-name "build/")								   ;;
;;   )														   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; custom file loading follows, keeping it at the end of init.el.
;;;
;; ruby configurations that don't work:
;;
;; my experience has been that these changes break 'eglot'
;; and 'flymake' for c.
;;
;; :init -- (setopt eglot-stay-out-of '(flymake))
;; :hook -- (ruby-ts-mode . eglot-ensure)
;; :config -- ?
;; (with-eval-after-load 'eglot
;;   (add-to-list
;;    'eglot-server-programs
;;    '((ruby-mode ruby-ts-mode) "ruby-lsp")))
;; going for a minimal ruby setup with a repl available. i've
;; left some of the settings i found on the web as comments.
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
;;
;; (use-package flymake-ruby
;;   :after flymake
;;   :hook (ruby-mode . flymake-ruby-load)
;;   (ruby-ts-mode . flymake-ruby-load)
;;   )


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
