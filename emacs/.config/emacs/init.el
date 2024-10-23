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

;; pass 1, reduce to a minimum usable init.
;; pass 2, removing use-package emacs, i don't like splitting the
;;         customizations away from related mode settings.

;;;
;;; Code:
;;;

;;;
;;; system compatability checks
;;;
;; system compatability checks: i don't move my init around to foreign
;; systems that don't have current emacs builds. and i always intend
;; to be running with the gui.

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
            ("melpa" . 5)))

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
  
;;;
;;; sane defaults and establish some paths.
;;;

  (add-to-list
   'load-path
   (concat user-emacs-directory "troi-lisp"))

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

  (when (display-graphic-p)
    (context-menu-mode))

  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (column-number-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (add-hook 'prog-mode-hook 'which-function-mode)

  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

  ;; turn on useful but disabled commands.
  (put 'scroll-left 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; visual line mode is ok for text, use (truncate-lines t) for code.
  (add-hook 'text-mode-hook 'visual-line-mode)

  (delete-selection-mode +1)
  (indent-tabs-mode +1)

  ;; use s-q to close emacs if i don't want to M-x
  ;; save-buffers-kill-emacs.
  (global-unset-key (kbd "C-x C-c"))

  ;; the number of times i want a the list instead of the mode is zero.
  (global-set-key (kbd "C-x C-d") 'dired)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (global-set-key (kbd "M-o") 'other-window)

  (global-set-key "\M-z" 'zap-up-to-char)

  ;; default search to regexp instead of string.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)

  (setopt user-full-name "Troy Brumley")
  (setopt user-mail-address "BlameTroi@gmail.com")
  (setopt auth-sources '("~/.authinfo.gpg"))
  (setopt auth-source-cache-expiry nil)

  ;; for line and column number modes, format with %C bases columns
  ;; from 1 and not 0.
  (setopt display-line-numbers-width 4)
  (setopt mode-line-position-column-line-format '(" (%l,%C)"))

  ;; nice scrolling
  (setopt scroll-margin 0)
  (setopt scroll-conservatively 100000)
  (setopt scroll-preserve-screen-position 1)

  (setopt sentence-end-double-space nil)
  (setopt require-final-newline t)

  (setopt switch-to-buffer-obey-display-actions t)
  (setopt help-window-select t)
  (setopt help-wndow-keep-selected t)
  (setopt enable-recursive-minibuffers t)

  (setopt use-dialog-box nil)
  (setopt apropos-sort-by-scores t)
  (setopt blink-matching-delay 0.1)
  (setopt delete-by-moving-to-trash t)

  (setopt tab-always-indent 'complete)

  (setopt comment-empty-lines t)
  (setopt comment-style 'extra-line)

  (setopt confirm-kill-emacs 'y-or-n-p)

  (setopt ns-auto-hide-menu-bar t)           ; this gains no space on displays with the notch

  (setopt standard-indent 8)


;;;
;;; the obviously needed packages.
;;;

  (use-package diminish)

  (use-package bind-key)

  (use-package free-keys)

  (use-package which-key
    :diminish
    :init (which-key-mode))

  (use-package ws-butler
    :diminish
    :hook (prog-mode . ws-butler-mode))

  (use-package savehist
    :config
    (setopt savehist-additional-variables
            '(compile-command
              kill-ring
              regexp-search-ring))
    (savehist-mode 1))

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

  ;; for dired, use 'gls' if it's available. the 'ls' in macos and some
  ;; other systems doesn't support all the options that 'dired' wants.

  (use-package dired
    :after exec-path-from-shell
    :ensure nil
    :config
    (setopt
     dired-recursive-copies  'always
     dired-isearch-filenames 'dwim)
    (when (executable-find "gls")    ; use GNU ls
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
    (nerd-icons-completion-mode))
  (declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :diminish
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; 'so-long' handles long lines that are usually found in program
;; source code where unneeded whitespace has been removed. forcing
;; paragraph text direction is reported to also help by removing the
;; checks and scans done for right to left languages.

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

;; 'dot-mode' brings the vim '.' to emacs.
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

;; magit, i can't escape it forever

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package cmake-mode
  :defer t)

(use-package ninja-mode
  :defer t)

(use-package ruby-ts-mode
  :defer t
  :after treesit
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")

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
