;;; .emacs --- Theresa O'Connor's Emacs configuration -*- emacs-lisp -*-

;; Copyright Â© 1997â€“2022 Theresa O'Connor <tess@oconnor.cx>

;; Author: Theresa O'Connor <tess@oconnor.cx>
;; Keywords: local

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; see <https://www.gnu.org/licenses/>.


;;; Code:

(require 'cl-lib)
(require 'seq)
(unless (require 'xdg nil t)
  (defun xdg-config-home (&rest ignore)
    (expand-file-name ".config" "~")))

(defvar tess-before-local-init-hook nil
  "Hook run before my local init file gets loaded.")
(defvar tess-after-local-init-hook nil
  "Hook run after my local init file gets loaded.")

(setq max-lisp-eval-depth 1500
      max-specpdl-size    3000)

(add-hook 'focus-out-hook 'garbage-collect)

(when (eq system-type 'windows-nt)
  (setenv "XDG_CONFIG_HOME"
          (format "C:\\Users\\%s\\AppData\\Roaming\\" user-login-name))
  ;; Fix the appearance of Emacs' window
  (setq tess-screen-height-to-type-size-ratio 70
        tess-emacs-top 10
        tess-emacs-left 10)
  ;; Fix assumptions about where things are
  (setenv "HOME" (getenv "USERPROFILE"))
  (cd "~/"))

(defconst tess-mac-data-volume
  "/Volumes/data/")

(defconst tess-mac-shared-home
  (let ((dir (expand-file-name (format "Users/%s" user-login-name)
                               tess-mac-data-volume)))
    (if (file-directory-p dir)
        dir
      "~/shared")))

(defconst tess-elisp-dirs
  (mapcar (lambda (entry)
            (let ((name (car entry))
                  (default-dir (cdr entry)))
              (if default-dir
                  (expand-file-name name default-dir)
                nil)))
          (list (cons "emacs" (xdg-config-home))
                (cons ".emacs.d" (xdg-config-home))
                (cons "elisp" tess-mac-shared-home)
                (cons "elisp" tess-mac-data-volume)
                (cons "elisp" "~"))))

(defconst tess-elisp-dir
  (or (catch 'found
        (mapc (lambda (dir)
                (when (and (stringp dir) (file-directory-p dir))
                  (throw 'found dir)))
              tess-elisp-dirs))
      "~/elisp")
  "Where I keep local copies of elisp files, both my own and others'.")

(defconst tess-code-dir "~/code"
  "Where I check out version-controlled things.")

(defun tess-run-executable (executable &rest args)
  ""
  (with-temp-buffer
    (apply 'call-process executable nil t nil args)
    (buffer-substring (point-min) (1- (point-max)))))

(when (file-executable-p "/usr/libexec/path_helper")
  (setq exec-path
        (split-string
         (substring (tess-run-executable "/usr/libexec/path_helper" "-c")
                    13 -2)
         path-separator)))

(defconst tess-sysname
  (if (eq system-type 'windows-nt)
      (downcase (format "%s-%s"
                        (getenv "PROCESSOR_ARCHITECTURE")
                        (getenv "OS")))
    (format
     "%s-%s"
     (cond ((string-match "^arm-apple" system-configuration) "arm64")
           ((string-match "^x86_64" system-configuration) "x86_64")
           ((string-match "^i686" system-configuration) "i686")
           ((string-match "^i386" system-configuration) "i386")
           (t "unknown"))
     (cond ((memq system-type '(cygwin windows-nt)) "windows")
           ((eq system-type 'gnu/linux) "linux")
           (t system-type))))
  "What kind of OS are we running on.")

(add-to-list 'exec-path (expand-file-name "~/bin/"))

(add-to-list 'exec-path
             (expand-file-name "bin"
                               (expand-file-name tess-sysname "~")))

(let ((shared-bin
       (expand-file-name "bin" tess-mac-shared-home)))
  (when (file-directory-p shared-bin)
    (add-to-list 'exec-path shared-bin)))

(defun tess-score-path (path)
  "Score PATH for sorting `exec-path' entries."
  (+ (if (string-match (regexp-quote (expand-file-name "~")) path) 100 0)
     (length path)))

(defun tess-cmp-path (p1 p2)
  "Non-nil iff P1 should appear before P2 in `exec-path'."
  (> (tess-score-path p1) (tess-score-path p2)))

(defun tess-path-fixup ()
  "Update `exec-path' with my preferred ordering."
  (setq exec-path
        (sort (seq-uniq
               (mapcar (lambda (p)
                         (file-name-as-directory (expand-file-name p)))
                       exec-path)
               'string-equal)
              'tess-cmp-path))
  (setenv "PATH" (mapconcat 'identity exec-path path-separator)))

(add-hook 'tess-after-local-init-hook 'tess-path-fixup)

;;; Compatibility, part 2. Checking for the availability of various
;;; functions which I'll be using later on.

;; Not defined in Emacs.
(if (fboundp 'variable-obsolete-p)
    (defalias 'tess-variable-obsolete-p 'variable-obsolete-p)
  (defsubst tess-variable-obsolete-p (variable)
    "Non-nil if VARIABLE is marked as obsolete."
    (get variable 'byte-obsolete-variable)))

;; Not defined in older Emacsen.
(unless (fboundp 'custom-autoload)
  (defun custom-autoload (symbol load)
    "Mark SYMBOL as autoloaded custom variable and add dependency LOAD."
    (put symbol 'custom-autoload t)
    (custom-add-load symbol load)))

(cond ((fboundp 'set-frame-parameter)
       (defalias 'tess-set-frame-parameter 'set-frame-parameter))
      ((fboundp 'set-frame-property)
       (defalias 'tess-set-frame-parameter 'set-frame-property)))

;; Defined in multi-tty Emacs
(if (fboundp 'window-system)
    (defalias 'tess-window-system 'window-system)
  (defun tess-window-system (&optional frame)
    window-system))

(cond ((fboundp 'frame-display) ;; Multi-TTY Emacs
       (defalias 'tess-frame-display 'frame-display))
      (t (defalias 'tess-frame-display 'ignore)))

(setenv "LANG" "en_US.UTF-8")

;;; Utilities.

(defsubst tess-alist (list)
  "Given LIST of the form (A B .. Z), create an alist of the form
\((A . A) (B . B) .. (Z . Z)). If LIST is nil, return nil. Useful
for making arguments for `completing-read'."
  (mapcar (lambda (item) (cons item item)) list))

(defun tess-add-to-list* (list-var &rest directories)
  "Add to the value of LIST-VAR each existing directory in DIRECTORIES.
Effectively a multi-argument version of `add-to-list', but custom-made
for variables like `load-path' and `Info-default-directory-list'."
  (mapc (lambda (directory)
          (when (file-directory-p directory)
            (add-to-list list-var (file-name-as-directory directory))))
        directories))
(put 'tess-add-to-list* 'lisp-indent-function 1)

;;; Frob the Emacs command line.

(defvar tess-server-emacs t
  "If non-null, this emacs should run an edit server.
By edit server, I mean the bit that emacsclient or gnuclient talk to.")

(add-to-list 'command-switch-alist
             '("gnus" . (lambda (&rest ignore)
                          (setq tess-server-emacs nil)
                          ;; Exit Emacs after quitting Gnus
                          (add-hook 'gnus-after-exiting-gnus-hook
                                    'save-buffers-kill-emacs)
                          ;; Start Gnus when Emacs starts
                          (add-hook 'tess-after-local-init-hook 'gnus t))))


;;; Frobbing `load-path' and checking for any third-party elisp files on
;;; this machine.

(mapc (lambda (dir)
        (when (and (stringp dir) (file-directory-p dir))
          (add-to-list 'load-path dir)
          (let ((default-directory dir))
            (load "./subdirs.el" t))))
      tess-elisp-dirs)

(defsubst tess-expand-file-name-in-code-dir (file)
  (expand-file-name file tess-code-dir))

(apply 'tess-add-to-list* 'load-path
  (mapcar 'tess-expand-file-name-in-code-dir
          '("bikeshed/emacs" "editorconfig-emacs" "facebook-el" "gitsum"
            "ljupdate" "markdown-mode" "mediawiki-el" "ninja/misc"
            "slime")))

;; Ducking the incompatible byte-code issue.
(apply 'tess-add-to-list* 'load-path
  (mapcar 'tess-expand-file-name-in-code-dir
          '("37emacs" "auctex" "bbdb/lisp" "circe" "emacs-w3m" "erc"
            "g-client" "gnus/lisp" "js2-mode/build" "nxml")))

(apply 'tess-add-to-list* 'load-path
       (mapcar 'tess-expand-file-name-in-code-dir
               '("gh.el" "gist.el" "logito" "pcache"
                 "tabulated-list.el")))

(apply 'tess-add-to-list* 'Info-default-directory-list
  (mapcar 'tess-expand-file-name-in-code-dir
          '("auctex/doc""bbdb/texinfo" "emacs-w3m/doc" "gnus/texi")))

;; Let's make sure we load the right version of Gnus.
(ignore-errors (require 'gnus-load))

(when (locate-library "package")
  (package-initialize)

  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/"))))

;;; Define various constants and predicates that will be used throughout
;;; this .emacs file to conditionalize code. For instance, I define
;;; `tess-tty-p' so that I can specify particular Emacs configuration
;;; bits for TTYs only.

(if (fboundp 'console-type)
    (defun tess-tty-p (&rest ignore)
      "Is this a TTY that we're on?"
      (eq (console-type) 'tty))
  (defun tess-tty-p (&optional display)
    "Is this a TTY that we're on?"
    (not (display-graphic-p display))))

(defun tess-xterm-p (&optional display)
  "Non-nil if DISPLAY is an xterm.
If DISPLAY is nil, the current displaly is used."
  (and (tess-tty-p display)
       (let ((TERM (getenv "TERM")))
         (if TERM
             (string-match "^xterm.*\\'" TERM)
           nil))))

(defun tess-menu-bar-lines (&optional frame)
  "Returns the number of menu bar lines to be used on FRAME."
  (if (eq (window-system frame) 'ns) 1 0))


;;; Work-arounds for things I find annoying in their default state, and
;;; basic customizations.

(setq read-file-name-completion-ignore-case t)

(setq ns-use-system-highlight-color nil)

(defun tess-run-after-make-frame-functions (&optional frame)
  (run-hook-with-args 'after-make-frame-functions
                      (or frame (selected-frame))))
(add-hook 'tess-after-local-init-hook 'tess-run-after-make-frame-functions)

(setq inhibit-startup-message t
      initial-major-mode      'lisp-interaction-mode)

(setq cd-path '("./" "~/" "~/code" "~/specs"))
(setenv "CDPATH" (mapconcat 'identity cd-path path-separator))

(when (eq system-type 'windows-nt)
  (defun tess-fix-w32-pathname (dir)
    "Munge DIR to be a real dir name."
    (setq dir (or dir ""))
    ;; FIXME: an example of why I do this would be nice.
    (if (string-match "^[\"]\\(.*\\)[\"]/\\'" dir)
        (file-name-as-directory (match-string 1 dir))
      dir))

  (let ((path (parse-colon-path (getenv "PATH"))))
    (setenv "PATH"
            (mapconcat 'tess-fix-w32-pathname path path-separator))))
(global-set-key (kbd "C-c l") 'goto-line)

(setq require-final-newline t)

(setq inhibit-default-init  t)

(defun tess-warn-about-default-init ()
  (when (locate-library "default")
    (message "Warning: `default.el' not loaded")
    (sit-for 2)))

(add-hook 'tess-after-local-init-hook 'tess-warn-about-default-init)

(setq change-log-default-name "ChangeLog")

(when (featurep 'aquamacs)
  (cua-mode 0))
(setq zmacs-regions t)

(when (fboundp 'transient-mark-mode)
  (transient-mark-mode 1)
  (setq highlight-nonselected-windows nil
        mark-even-if-inactive         t))

(mapc (lambda (sym)
        (put sym 'disabled nil))
      '(downcase-region erase-buffer eval-expression narrow-to-page
        narrow-to-region upcase-region))

(put 'overwrite-mode 'disabled t)

(setq enable-recursive-minibuffers t)

(require 'mb-depth nil t)
(cond
 ((fboundp 'minibuffer-depth-indicate-mode)
  (minibuffer-depth-indicate-mode 1))
 ((fboundp 'minibuffer-indicate-depth-mode)
  (minibuffer-indicate-depth-mode 1)))

(setq sentence-end-double-space nil)

(defvar tess-sentence-end-re
  "\\(\\w+\\)\\([.?!]\\)\\B"
  "Regular expression which matches potential sentence endings.")

(defvar tess-words-which-do-not-end-sentences
  '("Mr" "Mrs" "St")
  "Abbreviations which should not be considered to end sentences.")

(defun tess-sentence-end ()
  "Returns the location of the next sentence-end in the current buffer.
Resorts to `point-max' if it can't find a sentence end."
  (or (catch 'found
        (while (re-search-forward tess-sentence-end-re nil t)
          (when (or
                 (member (match-string 2) '("?" "!"))
                 (not (member (match-string 1)
                              tess-words-which-do-not-end-sentences)))
            (throw 'found (point)))))
      (point-max)))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq truncate-partial-width-windows nil)

(setq-default truncate-lines t)

(defun tess-disable-truncate-lines ()
  (setq truncate-lines nil))

(mapc (lambda (hook)
        (add-hook hook 'tess-disable-truncate-lines))
      '(term-mode-hook eshell-mode-hook html-mode-hook))

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(setq visible-cursor nil) ; Emacs 22

(setq-default scroll-step              1
              scroll-conservatively    most-positive-fixnum
              scroll-up-aggressively   0.0
              scroll-down-aggressively 0.0)

(when (require 'multi-region nil t)
  (global-set-key (kbd "C-c 2") multi-region-map))

(when (fboundp 'minibuffer-electric-default-mode)
  (minibuffer-electric-default-mode 1))

(when (fboundp 'temp-buffer-resize-mode)
  (temp-buffer-resize-mode 1))

(cond ((not (tess-variable-obsolete-p 'resize-minibuffer-window-exactly))
       (setq resize-minibuffer-window-exactly t)
       (when (fboundp 'resize-minibuffer-mode)
         (resize-minibuffer-mode 1)))
      (t
       (setq max-mini-window-height 0.30)
       (setq resize-mini-window t)))

(when (ignore-errors (require 'uniquify))
  (setq-default uniquify-buffer-name-style 'forward))

(line-number-mode 1)
(when (fboundp 'column-number-mode)
  (column-number-mode 1))

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " A")
(setcar (cdr (assq 'auto-fill-function minor-mode-alist)) " F")
(let ((el-hook (lambda () (setq mode-name "el"))))
  (add-hook 'emacs-lisp-mode-hook el-hook)
  (add-hook 'lisp-interaction-mode-hook el-hook))
(add-hook 'sh-mode-hook (lambda () (setq mode-name "sh")))

(when (executable-find "xcode-select")
  (let ((xcode-developer-path
         (substring (shell-command-to-string "xcode-select -p") 0 -1)))
    (add-to-list 'exec-path
                 (expand-file-name "usr/bin" xcode-developer-path))))

(when (and (eq system-type 'darwin)
           (boundp 'mac-apple-event-map)
           (keymapp mac-apple-event-map))
  (define-key mac-apple-event-map
    [core-event reopen-application] 'raise-frame))

;; Fix sort order in dired, ls, etc.
(setenv "LC_COLLATE" "C")

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(setq comment-style 'indent)

(setq auto-mode-case-fold t)

(defun tess-insert-local-variables-spec ()
  "Insert a minimal local variables spec for this buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (insert (format "-*- mode: %s; coding: %s -*- "
                      ;; FIXME: only strip when last 5 characters are "-mode"
                      (substring (symbol-name major-mode) 0 -5)
                      ;; FIXME: "-unix" is 5 chars, but what about the others?
                      (substring (symbol-name buffer-file-coding-system) 0 -5)))
      ;; If there's some kind of local comment syntax, ensure the local
      ;; variables spec lives in one.
      (when comment-start
        (comment-region (point-min) (1- (point))))
      (delete-char -1)
      (insert "\n"))))

(setq apropos-do-all t)

(setenv "GIT_PAGER" "cat")


;; completion

(setq completion-ignore-case t)
(define-key minibuffer-local-completion-map (kbd "SPC") nil)

(when (locate-library "pcomplete")
  (setq pcomplete-cycle-completions nil))

(cond ((locate-library "longlines")
       (setq longlines-wrap-follows-window-size t))
      ((fboundp 'visual-line-mode)
       (defalias 'longlines-mode 'visual-line-mode)))

(setq search-highlight t)
(setq-default case-fold-search t)
(eval-after-load "isearch"
  '(define-key isearch-mode-map (kbd "C-h") 'isearch-mode-help))

(setq-default abbrev-mode t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))
(add-hook 'mail-setup-hook 'mail-abbrevs-setup)

(when (fboundp 'show-paren-mode)
  (show-paren-mode 1)
  (make-variable-buffer-local 'show-paren-mode))

;; to make paredit behave.
(setq blink-matching-paren-on-screen nil)
(setq blink-matching-delay 0.125)

;; Emacs.app (Cocoa/GNUStep port) uses mic-paren by default
(when (featurep 'mic-paren)
  (setq paren-sexp-mode nil))

(setq-default auto-fill-function 'do-auto-fill)

(mapc (lambda (mode-hook)
         (add-hook mode-hook 'turn-off-auto-fill))
      '(emacs-lisp-mode-hook sh-mode-hook comint-mode-hook
        shell-mode-hook lisp-mode-hook erc-mode-hook ruby-mode-hook))

(setq-default fill-column 72)
(setq emacs-lisp-docstring-fill-column 72)

(if (boundp 'show-trailing-whitespace)
    (progn
      (setq-default show-trailing-whitespace t)

      (defun tess-hide-trailing-whitespace ()
        "Do not highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace nil))

      (defun tess-show-trailing-whitespace ()
        "Highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace t))

      (defun tess-toggle-show-trailing-whitespace ()
        "Highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace (not show-trailing-whitespace)))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook
                        'tess-hide-trailing-whitespace))
            '(Buffer-menu-mode-hook custom-mode-hook text-mode-hook
              term-mode-hook Info-mode-hook comint-mode-hook
              buffer-menu-mode-hook apropos-mode-hook
              tooltip-show-hook gnus-article-mode-hook mail-mode-hook
              gnus-summary-mode-hook message-mode-hook
              gnus-group-mode-hook eshell-mode-hook w3-mode-hook
              initial-calendar-window-hook))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook
                        'tess-show-trailing-whitespace))
            '(latex-mode-hook LaTeX-mode-hook html-mode-hook)))
  (defalias 'tess-hide-trailing-whitespace 'ignore))

(setq user-mail-address "hober0@gmail.com"
      user-full-name    "Theresa O'Connor")
(setq message-log-max most-positive-fixnum)

(defun tess-clear (&optional prefix)
  "Move the line containing point to the top of the window.
With PREFIX, move the line containing point to line PREFIX of the window."
  (interactive "P")
  (recenter (or prefix 0)))
(global-set-key (kbd "C-c c") 'tess-clear)

(defvar tess-default-network-interface
  (cond
    ((eq system-type 'darwin) "en0")
    ((eq system-type 'gnu/linux) "eth0")
    (t nil)))

(defun tess-ip (&optional interface)
  (interactive (list (completing-read
                      "Interface: "
                      (mapcar 'car (network-interface-list))
                      nil t nil nil
                      tess-default-network-interface)))
  (let ((addresses
         (mapcar (lambda (entry)
                   (let ((formatted
                          (substring (format-network-address
                                      (cdr entry))
                                     0 -2)))
                     formatted))
                 (if (not interface)
                     (network-interface-list)
                   (list (assoc interface (network-interface-list)))))))
    ;; Only return a list of addresses when there's more than one.
    (if (not (cdr addresses))
        (car addresses)
      addresses)))

(defun eshell/ip (&rest arguments)
  (let ((addresses (apply 'tess-ip arguments)))
    (if (stringp addresses)
        addresses
      (mapconcat 'identity addresses "\n"))))

(add-hook 'write-file-hooks 'time-stamp)

(when (fboundp 'goto-address)
  (setq goto-address-fontify-maximum-size most-positive-fixnum)
  (add-hook 'find-file-hooks 'goto-address))

(setq-default indicate-empty-lines       t
              indicate-buffer-boundaries t)

(let ((hook (lambda ()
              (setq indicate-empty-lines       nil
                    indicate-buffer-boundaries nil)))
      (mode-hooks '(shell-mode-hook term-mode-hook gnus-article-mode-hook
                    gnus-summary-mode-hook gnus-group-mode-hook
                    eshell-mode-hook)))
  (mapc (lambda (mode-hook)
          (add-hook mode-hook hook))
        mode-hooks))

(when (fboundp 'auto-insert-mode)
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (let ((atom-template "template.atom"))
    (when (file-exists-p (expand-file-name atom-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.atom\\'" . "Atom") . ,atom-template))))
  (let ((css-template "template.css"))
    (when (file-exists-p (expand-file-name css-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.css\\'" . "CSS") . ,css-template))))

  (let ((html-template "template.html"))
    (when (file-exists-p (expand-file-name html-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.html\\'" . "HTML") . ,html-template))))
  (let ((js-template "template.js"))
    (when (file-exists-p (expand-file-name js-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.js\\'" . "JS") . ,js-template))))

  (let ((latex-template "template.tex"))
    (when (file-exists-p (expand-file-name latex-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.tex\\'" . "LaTeX") . ,latex-template))))

  (let ((py-template "template.py"))
    (when (file-exists-p (expand-file-name py-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.py\\'" . "Python") . ,py-template))))

  (let ((zsh-template "template.zsh"))
    (when (file-exists-p (expand-file-name zsh-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.zsh\\'" . "Z-Shell") . ,zsh-template)))))

(defun kr-major-mode-p (symbol)
  "Return non-nil if SYMBOL is a major mode.
Used in `interactive' forms to read major mode names from the user."
  (and (fboundp symbol)
       (let ((function-name (symbol-name symbol)))
         (and (string-match "-mode\\'" function-name)
              (not (string-match "\\`turn-\\(on\\|off\\)-"
                                 function-name))))
       (not (assq symbol minor-mode-alist))))

(defun tess-read-major-mode ()
  "Read a major mode from the user, and return it.
Based on Kevin Rogers' `edit-region' interactive spec."
  (intern (completing-read
           (format "Major mode (default `%s'): " major-mode)
           obarray 'kr-major-mode-p t nil nil
           (symbol-name major-mode))))

(defun kr-edit-region (&optional edit-mode)
  "Edit the current region in a separate buffer.
With a prefix arg, change `major-mode' to EDIT-MODE."
  (interactive (list (when current-prefix-arg (tess-read-major-mode))))
  (clone-indirect-buffer nil t)
  (narrow-to-region (region-beginning) (region-end))
  (shrink-window-if-larger-than-buffer)
  (when edit-mode (funcall edit-mode)))

(defun tess-kill-mode-buffers (&optional mode)
  "Kill all buffers of this major mode.
With optional argument MODE, all buffers in major mode MODE are killed
instead."
  (interactive (list (when current-prefix-arg (tess-read-major-mode))))
  (setq mode (or mode major-mode))
  (when (or current-prefix-arg
            (y-or-n-p (format "Really kill all %s buffers? " mode)))
    (mapc (lambda (buffer)
            (when (with-current-buffer buffer
                    (eq major-mode mode))
              (kill-buffer buffer)))
          (buffer-list))))

(defun help-default-arg-highlight (arg)
  "Upcase and fontify ARG for use with `eldoc-mode' and help."
  (propertize (upcase arg)
              'face 'font-lock-variable-name-face))
(defun tess-find-mode (extension &optional interactive)
  "Returns the mode in which a file with EXTENSION would be opened."
  (interactive "sExtension: \np")
  (let ((mode (assoc-default (concat "." extension) auto-mode-alist
                             'string-match default-major-mode)))
    (when interactive
      (message "A file with extension .%s would be opened with mode %s"
               extension mode))
    mode))

(defun tess-wash-smart-quotes ()
  "Replace MS smart quotes with normal quotes in this buffer."
  (interactive)
  (save-excursion
    (let ((fixes '((342396 . "\"") (342392 . "'") (342387 . "--")
                   (342397 . "\"") (342393 . "'"))))
      (goto-char (point-min))
      (while (/= (point) (point-max))
        (let* ((this (char-after (point)))
               (match (assq this fixes)))
          (when match
            (delete-char 1)
            (insert (cdr match))))
        (forward-char 1)))))

;; linum from g.e.s -- much better than setnu
(when (and (locate-library "linum") (facep 'fringe))
  (setq linum-format (propertize "%5d " 'face 'fringe)))

(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))

(setq woman-use-own-frame nil)

(defvar tess-expand-emoji-flag t
  "Non-nil iff I should fall back to my emoji expansion code.")

(defun tess-emoji-char-p (char)
  "Non-nil iff CHAR is in the Unicode emoji range."
  (and
   ;; Not in blacklist
   (not (memq char '()))
   ;; Ranges
   (or (and (>= char ?\x1f100)
            (< char ?\x1f700))
       (and (>= char ?\x2600)
            (< char ?\x2800)))))

(defun tess-expand-emoji ()
  "Replace emoji with [unicode name of emoji]."
  (while (< (point) (point-max))
    (let ((char (char-after (point))))
      (cond
       ;; Strip out variation selectors
       ((and (>= char ?\xfe00) (< char ?\xfe10))
        (delete-char 1))
       ;; Apple logo
       ((= char ?\xf8ff)
        (insert "[Apple Inc.]")
        (delete-char 1))
       ;; Expand emoji
       ((tess-emoji-char-p char)
        (insert (format "[%s]"
                        (get-char-code-property (char-after (point)) 'name)))
        (delete-char 1))
       ;; Skip other charaters
       (t
        (goto-char (1+ (point))))))))

(defun tess-fixup-emoji-on-mac ()
  "Make Emacs display emoji correctly on Mac."
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  ;; Disable my fallback emoji expansion hack.
  (setq tess-expand-emoji-flag nil))


;; Customizations which we only want to happen when we're using
;; Emacs on a TTY.
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (when (tess-tty-p (tess-frame-display frame))
     (when (fboundp 'set-terminal-coding-system)
       (set-terminal-coding-system 'utf-8))

     (let ((hober-keymap (getenv "HOBER_KEYMAP"))
           (term (getenv "TERM")))
       (when (and hober-keymap (string-equal hober-keymap "YES")
                  term (string-equal term "cons25"))
         (when (fboundp 'normal-erase-is-backspace-mode)
	   (normal-erase-is-backspace-mode -1))
	 (define-key function-key-map (kbd "ESC [ }") (kbd "<menu>"))
	 (define-key function-key-map (kbd "ESC [ J") 'event-apply-super-modifier)
	 (define-key function-key-map (kbd "ESC [ ~") 'event-apply-hyper-modifier)))

     (setq browse-url-browser-function 'browse-url-lynx-emacs))))

;;; Various buffer-switching enhancements.

(setq mouse-buffer-menu-mode-mult 1)

(cond ((require 'ido nil t)
       (ido-mode 1)
       (setq ido-show-dot-for-dired t)
       (setcar (cddr ido-decorations) ",")
       (setcar (cddr (cdr ido-decorations)) ",â€¦"))
      ((and (fboundp 'icomplete-mode)
            (require 'icomplete nil t)
            ;; Only sufficiently recent versions of icomplete-mode allow
            ;; me to tweak its key bindings to my liking, so we should
            ;; only enable it when that's the case
            (boundp 'icomplete-minibuffer-map))
       (icomplete-mode 1))
      ((fboundp 'iswitchb-mode)
       (iswitchb-mode 1))
      ((fboundp 'iswitchb-default-keybindings)
       (iswitchb-default-keybindings)))
(global-set-key (kbd "C-x C-b") (global-key-binding (kbd "C-x b")))

(setq icomplete-show-matches-on-no-input t
      icomplete-separator ","
      read-buffer-completion-ignore-case t)

(eval-after-load "icomplete"
  '(when (boundp 'icomplete-minibuffer-map)
     (let ((map icomplete-minibuffer-map))
       (define-key map (kbd "C-s") 'icomplete-forward-completions)
       (define-key map (kbd "C-r") 'icomplete-backward-completions)
       (define-key map (kbd "RET") 'minibuffer-force-complete-and-exit))))

(add-hook 'iswitchb-define-mode-map-hook
          (lambda ()
            (mapc (lambda (key)
                    (define-key iswitchb-mode-map key nil))
                  (list (kbd "C-n") (kbd "C-k")))))

(eval-after-load "iswitchb"
  '(add-to-list 'iswitchb-buffer-ignore "[*]Completions[*]"))

(setq
 ibuffer-fontification-alist
 '(;; read-only buffers
   (10 buffer-read-only eshell-ls-readonly-face)
   ;; emacs' "special" buffers
   (15 (string-match "^*" (buffer-name)) eshell-ls-special-face)
   ;; hidden buffers
   (20 (and (string-match "^ " (buffer-name)) (null buffer-file-name))
       eshell-ls-symlink-face)
   ;; help buffers
   (25 (memq major-mode ibuffer-help-buffer-modes)
       eshell-ls-archive-face)
   ;; IRC buffers
   (30 (eq major-mode 'erc-mode) erc-notice-face)
   ;; dired buffers
   (35 (eq major-mode 'dired-mode) eshell-ls-directory-face)))


;;; gnuclient / emacsclient / remote editing

(setq display-buffer-reuse-frames t)
(defun tess-start-server-if-wanted ()
  (when tess-server-emacs
    (server-start)))
(when (or (fboundp 'make-network-process)
          (file-executable-p (expand-file-name "emacsserver"
                                               exec-directory)))
  (add-hook 'tess-after-local-init-hook
            'tess-start-server-if-wanted))

(when (locate-library "tramp")
  (setq tramp-default-method "sudo"))

;;; Key bindings.

(global-set-key (kbd "C-c m") 'set-mark-command)

(when (featurep 'multi-tty)
  (defun tess-delete-frame-or-kill-emacs ()
    (interactive)
    (if (cdr (frame-list)) ; (> (length (frame-list)) 1)
        (delete-frame)
      (save-buffers-kill-emacs)))
  (global-set-key (kbd "C-x C-c") 'tess-delete-frame-or-kill-emacs))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (eq (window-system frame) 'w32)
              (setq w32-pass-lwindow-to-system nil
	            w32-pass-rwindow-to-system nil
	            w32-pass-alt-to-system     nil
	            w32-alt-is-meta            t
	            w32-pass-apps-to-system    nil
	            w32-lwindow-modifier       'super
	            w32-rwindow-modifier       'hyper
	            w32-apps-modifier          nil)
	      (define-key function-key-map (kbd "<apps>") (kbd "<menu>")))
            (when (eq (window-system frame) 'ns)
              (setq ns-alternate-modifier 'super
	            ;; ns-function-modifier  'SOMETHING
	            ;; ns-control-modifier   'SOMETHING
	            ns-command-modifier   'meta))))

(mapc (lambda (key)
        (global-set-key key 'bury-buffer))
      (list (kbd "s-z") (kbd "A-z") (kbd "M-z")))

(global-set-key (kbd "<mode-line> <wheel-down>") 'next-buffer)
(global-set-key (kbd "<mode-line> <wheel-up>") 'previous-buffer)

(when (fboundp 'list-text-properties-at)
  (global-set-key (kbd "C-c p") 'list-text-properties-at))

(when (fboundp 'find-function-setup-keys)
  (find-function-setup-keys))

(global-set-key (kbd "C-c u") 'ucs-insert)

(global-set-key [SunPowerSwitch] 'save-buffers-kill-emacs)
(global-set-key [SunCut]         'clipboard-kill-region)
(global-set-key [SunCopy]        'clipboard-kill-ring-save)
(global-set-key [SunPaste]       'clipboard-yank)
(global-set-key [find]           'apropos)
(global-set-key [SunOpen]        'find-file)
(global-set-key [cancel]         'keyboard-quit)
(global-set-key [SunProps]       'list-text-properties-at)

(global-set-key [f14] 'undo)
(global-set-key [f12] 'repeat)
(global-set-key [f19] 'apropos)
(global-set-key [f17] 'find-file)
(global-set-key [f11] 'keyboard-quit)

(global-set-key [insertchar] 'overwrite-mode)
(when (fboundp 'find-file-at-point)
  (global-set-key (kbd "C-c F") 'find-file-at-point))

(global-set-key (kbd "C-x C-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x f") 'find-file)

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "t") 'string-insert-rectangle)
  (global-set-key (kbd "C-c r") map))

(defun tess-macro-dwim (arg)
  "DWIM keyboard macro recording and executing."
  (interactive "P")
  (if defining-kbd-macro
      (if arg
          (end-kbd-macro arg)
        (end-kbd-macro))
    (if last-kbd-macro
        (call-last-kbd-macro arg)
      (start-kbd-macro arg))))

(defun tess-macro-clear ()
  "Clear out the last keyboard macro."
  (interactive)
  (setq last-kbd-macro nil)
  (message "Last keyboard macro cleared."))

(global-set-key (kbd "<f9>") 'tess-macro-dwim)
(global-set-key (kbd "M-<f9>") 'tess-macro-clear)
(define-key esc-map (kbd "<f9>") 'tess-macro-clear)
(global-set-key (kbd "C-s-x") 'eval-defun)
(global-set-key (kbd "s-%") 'query-replace)
(global-set-key (kbd "s-/") 'dabbrev-expand)
(global-set-key (kbd "s-:") 'eval-expression)
(global-set-key (kbd "s-;") 'comment-dwim)
(global-set-key (kbd "s-<") 'beginning-of-buffer)
(global-set-key (kbd "s-<SPC>") 'just-one-space)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "s->") 'end-of-buffer)
(global-set-key (kbd "s-\\") 'delete-horizontal-space)
(global-set-key (kbd "s-b") 'backward-word)
(global-set-key (kbd "s-d") 'kill-word)
(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-l") 'downcase-word)
(global-set-key (kbd "s-p") (kbd "M-p"))
(global-set-key (kbd "s-q") 'fill-paragraph)
(global-set-key (kbd "s-v") 'scroll-down)
(global-set-key (kbd "s-w") 'kill-ring-save)
(global-set-key (kbd "s-x") 'execute-extended-command)
(global-set-key (kbd "s-~") 'not-modified)

(setq smerge-command-prefix (kbd "C-c s"))

;;; Major Emacs-based applications, and Emacs interfaces to other major
;;; applications.

(when (locate-library "ledger")
  (autoload 'ledger-mode "ledger" nil t))

(when (require 'emms nil t)
  (global-set-key (kbd "s-n") 'emms-next)
  (global-set-key (kbd "s-p") 'emms-previous)
  (global-set-key (kbd "s-s") 'emms-shuffle)
  (global-set-key (kbd "s-<RET>") 'emms-play-directory-tree)
  (global-set-key (kbd "s-<SPC>") 'emms-stop))

;; Emacs games
(add-hook 'tetris-mode-hook 'tess-hide-trailing-whitespace)

(when (locate-library "chess-auto")
  (load-library "chess-auto"))

(when (locate-library "malyon")
  (autoload 'malyon "malyon" nil t)
  (add-hook 'malyon-mode-hook 'tess-hide-trailing-whitespace))

;; Viper, the VI-like editor in Emacs

;; Enable Viper
;; (setq viper-mode t)
;; (add-hook 'tess-after-local-init-hook 'viper-mode)

(setq viper-toggle-key (kbd "M-a"))

(eval-after-load "viper-init"
  '(progn
     (mapc (lambda (hook)
             (remove-hook hook 'viper-restore-cursor-type))
           '(viper-vi-state-hook viper-replace-state-hook
             viper-emacs-state-hook))
     (remove-hook 'viper-insert-state-hook
                  'viper-set-insert-cursor-type)))

(when (and (boundp 'viper-mode) viper-mode (featurep 'aquamacs))
  ;; Fixes some kind of Viper/Aquamacs interaction issue
  (raise-frame))

(when (fboundp 'rsh)
  (setq remote-shell-program "ssh")
  (defalias 'ssh 'rsh))


;; Eshell, the Emacs Shell
(when (locate-library "eshell")
  (when (not (fboundp 'eshell))
    (autoload 'eshell "eshell" nil t))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-a") 'eshell-bol)))

  (setq eshell-cd-on-directory nil)

  (setq eshell-save-history-on-exit t
        eshell-hist-ignoredups      nil)

  (setq eshell-default-target-is-dot t
        eshell-pushd-tohome          t)

  (setq eshell-cmpl-cycle-completions nil)

  (if (memq system-type '(berkeley-unix darwin))
      (defun tess-eshell-C-t ()
        "Request status of the running Eshell command.
  Only works on BSD."
        (interactive)
        ;; An anamorphic `when' would be nice here.
        (let ((proc (eshell-interactive-process)))
          (if proc
              (process-send-string proc (string 20))
            (call-interactively 'transpose-chars))))
    (defun tess-eshell-C-t ()
      (interactive)
      (ding)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-t") 'tess-eshell-C-t)))
  (defconst tess-eshell-prompt-format-string
    (let ((user (or (getenv "USER") (user-login-name) "tess"))
          (host (car (split-string
                      (or (getenv "HOST") (system-name) "unknown")
                      "\\.")))
          (char (if (= (user-uid) 0) ?# ?:)))
      (format "\n%s@%s\n%s%c " user host "%c %s" char)))

  (defun tess-eshell-prompt ()
    (format tess-eshell-prompt-format-string
            (if (= eshell-last-command-status 0) ?âœ“ ?Ã—)
            (abbreviate-file-name default-directory)))

  (setq eshell-prompt-function 'tess-eshell-prompt)
  (setq eshell-prompt-regexp "^[^#:\n]*[#:] ")

  (autoload 'ansi-color-filter-apply "ansi-color")
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "s-p")
                             'eshell-previous-matching-input-from-input)))


  (defun eshell/less (file)
    "Pager view of FILE."
    (view-file file)
    0)
  (defalias 'eshell/more 'eshell/less)

  (defun eshell/rmb ()
    "Remove Emacs backup files in this directory."
    (mapconcat (lambda (filename)
                 (delete-file filename)
                 filename)
               (directory-files default-directory nil "~\\'" t)
               ", "))
  (setq eshell-scroll-show-maximum-output nil)
  (defalias 'eshell/clear 'tess-clear)

  (defun eshell/info (subject)
    "Read the Info manual on SUBJECT."
    (let ((buf (current-buffer)))
      (Info-directory)
      (let ((node-exists (ignore-errors (Info-menu subject))))
        (if node-exists
            0
          (switch-to-buffer buf)
          (eshell-print (format "There is no Info manual on %s.\n"
                                subject))
          1))))

  (defun eshell/emacs (&rest args)
    "Open a file in Emacs. Some habits die hard."
    (if (null args)
        (bury-buffer)
      (mapc 'find-file (mapcar 'expand-file-name
                               (eshell-flatten-list args))))
    0)
    (defalias 'eshell/emacsclient 'eshell/emacs)

  (defun eshell/vi (file)
    "Open a file with Viper."
    (with-current-buffer (find-file file)
      (setq viper-mode t)
      (viper-mode))
    0)

  (defalias 'eshell/concat 'eshell/cat)

  (eval-after-load "em-ls"
    '(progn
       (defvar tess-eshell-ls-keymap
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "RET")      'tess-eshell-ls-find-file-at-point)
           (define-key map (kbd "<return>") 'tess-eshell-ls-find-file-at-point)
           (define-key map (kbd "<mouse-2>")
             'pat-eshell-ls-find-file-at-mouse-click)
           map))

       (defadvice eshell-ls-decorated-name (after tess-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, middle-click: visit this file"
                                    'mouse-face 'highlight
                                    'keymap tess-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)

       (defun tess-eshell-ls-find-file-at-point (point)
         "RET on Eshell's `ls' output to open files."
         (interactive "d")
         (find-file (buffer-substring-no-properties
                     (previous-single-property-change point 'help-echo)
                     (next-single-property-change point 'help-echo))))

       ;; Not defined in Emacs.
       (unless (fboundp 'event-point)
         (defun event-point (event)
           "Return the character position of mouse EVENT."
           (posn-point (event-end event))))

       (defun pat-eshell-ls-find-file-at-mouse-click (event)
         "Middle click on Eshell's `ls' output to open files.
       From Patrick Anderson via the EmacsWiki."
         (interactive "e")
         (tess-eshell-ls-find-file-at-point (event-point event))))))

;; Emacs web browsing, web searching, and more!
(define-key minibuffer-local-must-match-map (kbd "?") nil)
(define-key minibuffer-local-completion-map (kbd "?") nil)

(when (locate-library "w3m")
  (autoload 'w3m "w3m" nil t)
  (autoload 'w3m-goto-url "w3m" nil t)
  (autoload 'w3m-region "w3m")

  (setq w3m-home-page
        (if (file-readable-p "~/html/home.html")
            (concat "file://" (expand-file-name "~/html/home.html"))
          "http://tess.oconnor.cx/home"))

  (setq w3m-use-toolbar t
        w3m-use-tab     nil
        w3m-key-binding 'info)

  (setq w3m-search-default-engine "google")

  (setq w3m-command-arguments       '("-F" "-cookie")
        w3m-mailto-url-function     'compose-mail
        browse-url-browser-function 'w3m
        mm-text-html-renderer       'w3m)

  (add-hook 'w3m-mode-hook 'tess-hide-trailing-whitespace)

  (eval-after-load "w3m"
    '(define-key w3m-mode-map (kbd "z") 'bury-buffer))

  (defalias 'eshell/w3m 'w3m)

  (setq w3m-use-cookies t)
  (setq w3m-cookie-accept-bad-cookies t)

  (defun tess-w3m-edit-emacswiki-page (url)
    (let ((node (substring (substring w3m-current-url
                                      (string-match "wiki[/?][^/&=]+\\'"
                                                    w3m-current-url))
                           5)))
      (w3m-goto-url (concat "http://www.emacswiki.org/cgi-bin/wiki"
                            "?action=edit;id=" node))))

  (eval-after-load "w3m"
    '(progn
       (add-to-list 'w3m-uri-replace-alist
                    '("\\`lj:\\(.+\\)" w3m-pattern-uri-replace
                      "http://www.livejournal.com/users/\\1/"))

       (add-to-list 'w3m-edit-function-alist
                    '(".*emacswiki.org/cgi-bin/wiki.*"
                      . tess-w3m-edit-emacswiki-page)))))

(when (locate-library "backpack")
  (setq backpack-username "hober")

  (defvar tess-backpack-map (make-sparse-keymap))
  (global-set-key (kbd "C-c b") tess-backpack-map)

  (mapc (lambda (cons)
          (let ((command (car cons)))
            (autoload command "backpack" nil t)
            (define-key tess-backpack-map (cdr cons) command)))
        '((backpack-remind             . "r")
          (backpack-remind-from-region . "R"))))

(cond ((locate-library "twitter")
       (autoload 'twitter "twitter" nil t)
       (global-set-key (kbd "C-c t") 'twitter))
      ((locate-library "twit")
       (autoload 'twit-post "twit" nil t)))

(when (locate-library "twittering-mode")
  (autoload 'twit "twittering-mode" nil t)
  ;; Don't forget to set `twittering-password' locally.
  (setq twittering-username "hober")
  ;; Work-around for a bug in `twittering-mode' in some Emacsen.
  (unless (fboundp 'clear-image-cache)
    (defun clear-image-cache (&rest ignore))))

(setq google-referer "http://tess.oconnor.cx/")

(when (ignore-errors (require 'g))
  (setq g-user-email "hober0@gmail.com"))

(when (locate-library "ell")
  (setq ell-locate t)
  (setq ell-goto-addr t)
  (autoload 'ell-packages "ell" nil t))

(when (locate-library "wikiarea")
  (autoload 'wikiarea "wikiarea" nil t)
  (setq wikiarea-managed-directory
        (expand-file-name "emacs-wiki/" tess-elisp-dir)))

(when (require 'oddmuse nil t)
  (setq oddmuse-directory "~/wikis")
  (oddmuse-mode-initialize))

(when (locate-library "wikipedia-mode")
  (autoload 'wikipedia-mode "wikipedia-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.wiki\\'" . wikipedia-mode)))

(when (locate-library "mediawiki-mode")
  (autoload 'mediawiki-mode "mediawiki-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.mw$" . mediawiki-mode)) ; $

  ;; do this in a hook because mediawiki-mode resets its keybindings on
  ;; evey mode change.
  (add-hook 'mediawiki-mode-hook
            (lambda ()
              (define-key mediawiki-mode-map (kbd "C-x C-s") 'save-buffer))))

;; Email.
(when (locate-library "sieve-mode")
  (autoload 'sieve-mode "sieve-mode" nil t)
  (add-to-list 'auto-mode-alist
               '("\\.si\\(eve\\|v\\)\\'" . sieve-mode)))

(setq mail-user-agent   'gnus-user-agent
      read-mail-command 'gnus)

(setq mail-signature t
      mail-yank-prefix "> "
      mail-from-style 'angles)

(add-to-list 'auto-mode-alist '("\\.SCORE\\'" . gnus-score-mode))

(when (locate-library "boxquote")
  (defvar tess-boxquote-map (make-sparse-keymap))
  (global-set-key (kbd "C-c q") tess-boxquote-map)
  (mapc (lambda (cons)
          (let ((command (car cons))
                (key (cdr cons)))
            (autoload command "boxquote" nil t)
            (define-key tess-boxquote-map key command)))
        '((boxquote-region            . "r")
          (boxquote-buffer            . "b")
          (boxquote-insert-file       . "i")
          (boxquote-yank              . "y")
          (boxquote-defun             . "F")
          (boxquote-paragraph         . "p")
          (boxquote-describe-function . "f")
          (boxquote-describe-variable . "v")
          (boxquote-describe-key      . "k")
          (boxquote-kill              . "K")
          (boxquote-unbox             . "u"))))

(when (require 'bbdb nil t)
  (setq bbdb-default-country nil
        bbdb-debug nil
        bbdb-file "~/.bbdb"
        bbdb-completion-display-record nil
        bbdb-quiet-about-name-mismatches 0)

  (when (coding-system-p 'utf-8)
    (setq bbdb-file-coding-system 'utf-8))
  (bbdb-initialize 'sendmail 'gnus 'message)

  (when (fboundp 'eshell)
    (defun eshell/bbdb (regex)
      (bbdb regex nil)))

  (add-hook 'message-setup-hook 'bbdb-define-all-aliases))

(defconst tess-bbdb-flag (featurep 'bbdb))

(when (locate-library "eudc")
  (setq eudc-multiple-match-handling-method 'select)
  ;; Only integrate with Contacts.app when the cli is installed
  ;;     https://github.com/shanecelis/contacts
  (when (and (executable-find "contacts") (require 'eudcb-mab nil t))
    (add-to-list 'eudc-server-hotlist '("localhost" . mab))))


;; Dired, the Emacs directory editor.

(require 'dired-x nil t)

(setq dired-dwim-target t)

(setq dired-recursive-deletes 'top
      dired-recursive-copies  'top)

(when (locate-library "wdired")
  (autoload 'wdired-change-to-wdired-mode "wdired" nil t)
  (add-hook 'dired-load-hook
            (lambda ()
              (define-key dired-mode-map (kbd "r")
                'wdired-change-to-wdired-mode))))


;; ljupdate, an Emacs LiveJournal client
;; When I have a local copy of my website, use the ljupdate there.
(let ((web-lib "~/web/html/ljupdate/"))
  (when (file-directory-p web-lib)
    (add-to-list 'load-path web-lib)))

(when (require 'ljupdate nil t)
  (setq lj-cache-login-information t
        lj-default-username        "hober"
        lj-fill-function           'ignore)
  (global-set-key (kbd "C-c j c") 'lj-compose)
  (global-set-key (kbd "C-c j l") 'lj-login)
  ;; handy for developing / testing in *ielm*
  (setq lj "www.livejournal.com"
        dj "www.deadjournal.com"))

;; Disable trying gnutls when it's not available
(when (and (not (gnutls-available-p))
           (require 'tls nil t))
  (unless (executable-find "gnutls-cli")
    (setq tls-program
          (seq-filter (lambda (candidate)
                       (string-match "gnutls-cli" candidate))
                     tls-program))))

;; ERC, an Emacs IRC client
(when (locate-library "erc")
  (autoload 'erc "erc" nil t)
  (autoload 'erc-select "erc" nil t)
  (autoload 'erc-select-ssl "erc" nil t)

  (setq erc-server                         "irc.w3.org"
        erc-port                           6665
        erc-user-full-name                 "Theresa O'Connor"
        erc-email-userid                   "tess"
        erc-nick                           '("hober" "hober2" "hober3")
        erc-nicklist-use-icons             nil
        erc-password                       nil ; set this in local config
        erc-nickserv-passwords             nil ; set this in local config
        erc-anonymous-login                t
        erc-auto-query                     'bury
        erc-join-buffer                    'bury
        erc-max-buffer-size                30000
        erc-prompt-for-password            nil
        erc-prompt-for-nickserv-password   nil
        erc-command-indicator              "CMD"
        erc-echo-notices-in-current-buffer t
        erc-send-whitespace-lines          nil
        erc-hide-list                      '("JOIN" "PART" "QUIT")
        erc-ignore-list                    '("jibot")
        erc-autojoin-timing                'ident
        erc-autojoin-delay                 30
        erc-autojoin-channels-alist        '())

  (setq erc-quit-reason-various-alist
        '(("brb"    "I'll be right back.")
          ("lunch"  "Having lunch.")
          ("dinner" "Having dinner.")
          ("food"   "Getting food.")
          ("sleep"  "Sleeping.")
          ("work"   "Getting work done.")
          (".*"     (yow))))

  (setq erc-part-reason-various-alist erc-quit-reason-various-alist
        erc-part-reason               'erc-part-reason-various
        erc-quit-reason               'erc-quit-reason-various)
  (defvar tess-erc-autojoin t
    "Whether or not ERC should autojoin on connect.")

  (defvar tess-erc-identify t
    "Whether or not ERC should identify with NickServ on connect.")

  (setq erc-server-alist
        '(("w3c" w3c "irc.w3.org" 6665))
        erc-networks-alist
        '((w3c "w3.org")))

  (defvar tess-erc-default-network "w3c"
    "Which IRC network to connect to by default.")

  (defun tess-erc-nick ()
    (cond
     ((listp erc-nick) (car erc-nick))
     ((stringp erc-nick) erc-nick)
     (t "hober")))

  (defun tess-irc-server-uses-ssl-p (server)
    "If non-null, SERVER requires SSL."
    (eq server 'apple))

  (defun tess-irc (netspec)
    "Interactively select an IRC network to connect to.
  Loosely based on `erc-server-select'."
    (interactive
     (list (assoc (completing-read
                   (format "IRC network (default %s)? "
                           tess-erc-default-network)
                   erc-server-alist nil t nil nil
                   tess-erc-default-network)
                  erc-server-alist)))
    (let* ((network (nth 1 netspec))
           (nick (tess-erc-nick))
           (args (list :server (nth 2 netspec)
                       :port (nth 3 netspec)
                       :nick nick))
           (password (cdr (assoc nick
                                 (cadr (assoc network
                                              erc-nickserv-passwords))))))
      (when password
        (setq args (append (list :password password) args)))
      (setq tess-erc-identify (not (eq network 'freenode)))
      ;; setq not let because of dumb reasons
      (setq erc-server-connect-function
            (if (tess-irc-server-uses-ssl-p network)
                'erc-open-tls-stream
              'open-network-stream))
      (apply 'erc args))))

;; rcirc
(when (locate-library "rcirc")
  (setq rcirc-nick "hober")
  (add-hook 'rcirc-mode-hook 'tess-hide-trailing-whitespace))

(when (locate-library "csv")
  (autoload 'csv-parse-buffer "csv"))

;;; Programming and other forms of document preparation.

(when (locate-library "editorconfig")
  (autoload 'editorconfig-mode "editorconfig" nil t)
  (autoload 'editorconfig-core-get-properties-hash "editorconfig-core")

  (defun tess-enable-editorconfig-mode ()
    (editorconfig-mode 1))

  (add-hook 'tess-after-local-init-hook 'tess-enable-editorconfig-mode)

  (setq editorconfig-mode-lighter " EC"))

(when (locate-library "ninja-mode")
  (autoload 'ninja-mode "ninja-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.ninja\\'" . ninja-mode)))

;; http://article.gmane.org/gmane.emacs.devel/64807
(setq parse-sexp-ignore-comments t)

(ignore-errors (require 'paredit))

(setq diff-switches "-u")

(autoload 'diff-context->unified "diff-mode" nil t)
(autoload 'diff-unified->context "diff-mode" nil t)

(setq vc-follow-symlinks t)

(defun tess-next-warning ()
  "Advance to the next buffer location in `font-lock-warning-face'."
  (interactive)
  (let ((here (point)))
    (condition-case nil
        (progn
          (goto-char (next-property-change (point)))
          (while (not (memq (get-text-property (point) 'face)
                          '(font-lock-warning-face
                            js2-error-face js2-warning-face)))
            (goto-char (next-property-change (point)))))
      (error (goto-char here)
             (error "There are no more warnings in the buffer!")))))

(global-set-key (kbd "C-c n") 'tess-next-warning)

(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

(setq glasses-separator           "-"
      glasses-uncapitalize-regexp ".*"
      glasses-uncapitalize-p      t)

(when (executable-find "xcode-select")
  (let* ((xcode-dir
          (with-temp-buffer
            (call-process "xcode-select" nil t nil "-p")
            (buffer-substring (point-min) (1- (point-max)))))
         (xcode-site-lisp
          (format "%s/Toolchains/XcodeDefault.xctoolchain/%s/"
                  xcode-dir
                  "usr/share/emacs/site-lisp")))
    (add-to-list 'load-path xcode-site-lisp)))

(when (locate-library "swift-mode")
  (autoload 'swift-mode "swift-mode")
  (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode)))

(when (locate-library "lsp")
  (add-hook 'swift-mode-hook 'lsp)
  (let ((sourcekit-lsp (executable-find "sourcekit-lsp")))
    (when (and (locate-library "lsp-sourcekit") sourcekit-lsp)
      (require 'lsp-sourcekit)
      (setq lsp-sourcekit-executable sourcekit-lsp))))

(when (locate-library "gitsum")
  (autoload 'gitsum "gitsum" nil t))

(when (locate-library "gist")
  (ignore-errors (require 'gist)))

(when (locate-library "psvn")
  (autoload 'svn-status "psvn" nil t))

(when (locate-library "vc-svn")
  (unless (memq 'SVN vc-handled-backends)
    (add-to-list 'vc-handled-backends 'SVN)))

(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode))
(defun tess-c-sharp-fix-tab-width ()
  (when (string-match "\\.cs\\'" (buffer-file-name))
    (setq tab-width 2)))

(add-hook 'java-mode-hook 'tess-c-sharp-fix-tab-width)

(defun tess-c-kill-backwards-into-nomenclature ()
  "Delete the CamelCase word before point."
  (interactive)
  (let ((end (point)))
    (c-backward-into-nomenclature 1)
    (kill-region (point) end)))

(defun tess-c-kill-forwards-into-nomenclature ()
  "Delete the CamelCase word after point."
  (interactive)
  (let ((beg (point)))
    (c-forward-into-nomenclature 1)
    (kill-region beg (point))))

(let ((hooks '(c-mode-common-hook espresso-mode-hook js-mode-hook
               js2-mode-hook python-mode-hook swift-mode-hook))
      (enable-subword-bindings
       (cond ((fboundp 'subword-mode)
              (lambda () (subword-mode 1)))
             ((fboundp 'c-subword-mode)
              (lambda () (c-subword-mode 1)))
             (t
              (lambda ()
                (local-set-key (kbd "M-DEL")
                               'tess-c-kill-backwards-into-nomenclature)
                (local-set-key (kbd "M-d")
                               'tess-c-kill-forwards-into-nomenclature))))))
  (mapc (lambda (hook) (add-hook hook enable-subword-bindings)) hooks))

(when (boundp 'auto-coding-alist)
  (add-to-list 'auto-coding-alist '("\\.[jw]ar\\'" . no-conversion))
  (add-to-list 'auto-coding-alist '("\\.[JW]AR\\'" . no-conversion)))
(add-to-list 'auto-mode-alist '("\\.[jw]ar\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.[JW]AR\\'" . archive-mode))


(when (locate-library "haskell-mode")
  (add-to-list 'auto-mode-alist '("\\.\\([hg]s\\|hi\\)\\'" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.l[hg]s\\'" . literate-haskell-mode))

  (autoload 'haskell-mode "haskell-mode" nil t)
  (autoload 'literate-haskell-mode "haskell-mode" nil t)

  (mapc (lambda (hook)
          (add-hook 'haskell-mode-hook hook))
        '(turn-on-haskell-font-lock turn-on-haskell-decl-scan
          turn-on-haskell-doc-mode turn-on-haskell-indent
          turn-on-haskell-hugs)))

;; Moved load of generic-x up before we look for javascript.el: both put
;; entries in `auto-mode-alist' for "\\.js\\'", and I want the entry
;; from javascript.el.
(when (ignore-errors (require 'generic-x))
  (setq default-major-mode 'default-generic-mode))

(defvar tess-xml-mode 'xml-mode
  "Which major-mode to use for editing XML.")

(defvar tess-html-mode 'html-mode
  "Which major-mode to use for editing HTML.")

(defun tess-linkify-region (start end)
    (interactive "r")
    (let ((str (buffer-substring-no-properties start end)))
      (delete-region start end)
      (insert "<a href=\"\">" str "</a>")))

;;; nxml
(when (or (load "rng-auto" t)
          (locate-library "nxml-mode"))

  (unless (fboundp 'nxml-mode)
    (autoload 'nxml-mode "nxml-mode" nil t))

  (unless (fboundp 'rng-validate-mode)
    (autoload 'rng-validate-mode "rng-valid" nil t))

  (setq tess-xml-mode 'nxml-mode)
  ;; (setq tess-html-mode 'nxml-mode)

  (setq nxml-sexp-element-flag t
        nxml-slash-auto-complete-flag t)

  ;; Hack `;' in nxml mode to automatically fix named character entity
  ;; references.
  (defun tess-nxml-semicolon-dwim (&rest ignore)
    "If we've just typed an HTML 4 named character entity reference,
replace it with its numerical equivalent. Otherwise, just insert `;'."
    (interactive)
    (or (tess-numericalize-entity) (insert ";")))

  (when (boundp 'nxml-mode-abbrev-table)
    (add-hook 'nxml-mode-hook
              (lambda ()
                (setq local-abbrev-table nxml-mode-abbrev-table))))

  (eval-after-load "nxml-mode"
    '(progn
       (define-key nxml-mode-map (kbd "<f8>") 'tess-linkify-region)
       (define-key nxml-mode-map (kbd "RET") 'newline-and-indent)
       ;; Install my `;' hack.
       (define-key nxml-mode-map (kbd ";") 'tess-nxml-semicolon-dwim))))

(add-to-list 'auto-mode-alist
             (cons "\\.bs\\'" tess-html-mode))
(add-to-list 'auto-mode-alist
             (cons "\\.\\(x?html\\|xht\\)\\'" tess-html-mode))
(add-to-list 'auto-mode-alist
             (cons "\\.\\(jsp\\|tpl\\|tag\\)\\'" tess-html-mode))
(add-to-list 'auto-mode-alist
             (cons "\\.\\(wsd[dl]\\|tld\\|xslt\\|plist\\)\\'"
                   tess-xml-mode))

(let ((html5-lib "~/code/html5-el/"))
  (when (file-directory-p html5-lib)
    (add-to-list 'load-path html5-lib)
    (eval-after-load "rng-loc"
      '(add-to-list 'rng-schema-locating-files
                    "~/code/html5-el/schemas.xml"))
    (require 'whattf-dt nil t)))

(defvar tess-html4-link-relations
  '("alternate" "stylesheet" "start" "next" "prev" "contents" "index"
    "glossary" "copyright" "chapter" "section" "subsection" "appendix"
    "help" "bookmark")
  "http://www.w3.org/TR/html4/types.html#type-links")

(defvar tess-html5-link-relations
  '("alternate" "archives" "author" "bookmark" "contact" "external"
    "feed" "first" "help" "icon" "index" "last" "license" "next"
    "nofollow" "pingback" "prefetch" "prev" "search" "stylesheet"
    "sidebar" "tag" "up")
  "http://www.whatwg.org/specs/web-apps/current-work/#linkTypes")

(defvar tess-atom-link-relations
  '(;; http://atompub.org/rfc4287.html#rel_attribute
    "alternate" "related" "self" "enclosure" "via"
    ;; http://www.iana.org/assignments/link-relations/
    "current" "edit" "edit-media" "first" "last" "next" "next" "payment"
    "prev" "previous")
  "")

(defvar tess-xfn-link-relations
  '("contact" "acquaintance" "friend"         ; Friendship
    "met"                                     ; Physical
    "co-worker" "colleague"                   ; Professional
    "co-resident" "neighbor"                  ; Geographical
    "child" "parent" "sibling" "spouse" "kin" ; Family
    "muse" "crush" "date" "sweetheart"        ; Romantic
    "me")                                     ; Identity
  "http://gmpg.org/xfn/11")

(defvar tess-uf-link-relations
  '("license"   ; http://microformats.org/wiki/rel-license
    "nofollow"  ; http://microformats.org/wiki/rel-nofollow
    "tag"       ; http://microformats.org/wiki/rel-tag
    ;; Drafts
    "directory" ; http://microformats.org/wiki/rel-directory
    "enclosure" ; http://microformats.org/wiki/rel-enclosure
    "home"      ; http://microformats.org/wiki/rel-home
    "payment"   ; http://microformats.org/wiki/rel-payment
    ;; Exploratory
    "cite"      ; http://microformats.org/wiki/distributed-conversation-brainstorming
    "group"     ; http://microformats.org/wiki/group-brainstorming
    "product"   ; http://microformats.org/wiki/rel-product
    "profile")  ; http://microformats.org/wiki/xmdp-brainstorming#linking_to_the_XMDP
  "")

(defvar tess-custom-link-relations
  '("http://tess.oconnor.cx/link-relations/include"
    "http://tess.oconnor.cx/link-relations/legacy"
    "http://tess.oconnor.cx/link-relations/listening"
    "http://tess.oconnor.cx/link-relations/livejournal"
    "http://tess.oconnor.cx/link-relations/pingback"
    "http://tess.oconnor.cx/link-relations/reddit"
    "http://tess.oconnor.cx/link-relations/stylesheet")
  "http://tess.oconnor.cx/link-relations/")

(defvar tess-link-relations
  (seq-uniq
   (sort (append tess-html4-link-relations tess-html5-link-relations
                 tess-atom-link-relations tess-xfn-link-relations
                 tess-uf-link-relations tess-custom-link-relations)
         'string<))
  "List of Atom and HTML link relations.")

(defun tess-read-link-relation ()
  "Read a link relation from the user, with completion."
  (interactive)
  (completing-read "Link relation: " tess-link-relations nil t))

(defun tess-read-link-relations ()
  "Read link relations from the user until they hit RET."
  (interactive)
  (let ((relations '())
        (relation (tess-read-link-relation)))
    (if (equal relation "me")
        relation
      (while (not (equal relation ""))
        (push relation relations)
        (setq relation (tess-read-link-relation)))
      (mapconcat (lambda (x) x) (sort relations 'string<) " "))))

(define-skeleton tess-rel-expand
  "Expand @rel."
  nil
  "rel=\""
  (tess-read-link-relations)
  "\"")

(defun tess-nuke-nofollow ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s-*rel=[\"']nofollow[\"']" nil t)
      (replace-match ""))))

(defun tess-unescape-html (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "&\\(lt\\|gt\\|quot\\|amp\\);" end t)
      (let* ((entities '(("quot" . "\"") ("amp" . "&")
                         ("lt" . "<") ("gt" . ">")))
             (replacement (assoc (match-string 1) entities)))
        (when replacement
          (replace-match (cdr replacement)))))))

(defvar tess-html4-intrinsic-events
  '("load" "unload" "click" "dblclick" "mousedown" "mouseup" "mouseover"
    "mousemove" "mouseout" "focus" "blur" "keypress" "keydown" "keyup"
    "submit" "reset" "select" "change")
  "HTML4 intrinsic events.")

(defvar tess-html4-entity-map
  '(;; latin-1
    ("nbsp" . 160) ("iexcl" . 161) ("cent" . 162) ("pound" . 163)
    ("curren" . 164) ("yen" . 165) ("brvbar" . 166) ("sect" . 167)
    ("uml" . 168) ("copy" . 169) ("ordf" . 170) ("laquo" . 171)
    ("not" . 172) ("shy" . 173) ("reg" . 174) ("macr" . 175)
    ("deg" . 176) ("plusmn" . 177) ("sup2" . 178) ("sup3" . 179)
    ("acute" . 180) ("micro" . 181) ("para" . 182) ("middot" . 183)
    ("cedil" . 184) ("sup1 #185") ("ordm" . 186) ("raquo" . 187)
    ("frac14" . 188) ("frac12" . 189) ("frac34" . 190) ("iquest" . 191)
    ("Agrave" . 192) ("Aacute" . 193) ("Acirc" . 194) ("Atilde" . 195)
    ("Auml" . 196) ("Aring" . 197) ("AElig" . 198) ("Ccedil" . 199)
    ("Egrave" . 200) ("Eacute" . 201) ("Ecirc" . 202) ("Euml" . 203)
    ("Igrave" . 204) ("Iacute" . 205) ("Icirc" . 206) ("Iuml" . 207)
    ("ETH" . 208) ("Ntilde" . 209) ("Ograve" . 210) ("Oacute" . 211)
    ("Ocirc" . 212) ("Otilde" . 213) ("Ouml" . 214) ("times" . 215)
    ("Oslash" . 216) ("Ugrave" . 217) ("Uacute" . 218) ("Ucirc" . 219)
    ("Uuml" . 220) ("Yacute" . 221) ("THORN" . 222) ("szlig" . 223)
    ("agrave" . 224) ("aacute" . 225) ("acirc" . 226) ("atilde" . 227)
    ("auml" . 228) ("aring" . 229) ("aelig" . 230) ("ccedil" . 231)
    ("egrave" . 232) ("eacute" . 233) ("ecirc" . 234) ("euml" . 235)
    ("igrave" . 236) ("iacute" . 237) ("icirc" . 238) ("iuml" . 239)
    ("eth" . 240) ("ntilde" . 241) ("ograve" . 242) ("oacute" . 243)
    ("ocirc" . 244) ("otilde" . 245) ("ouml" . 246) ("divide" . 247)
    ("oslash" . 248) ("ugrave" . 249) ("uacute" . 250) ("ucirc" . 251)
    ("uuml" . 252) ("yacute" . 253) ("thorn" . 254) ("yuml" . 255)
    ;; special
    ; ("quot" . 34) ("amp" . 38) ("lt" . 60) ("gt" . 62)
    ("OElig" . 338) ("oelig" . 339) ("Scaron" . 352) ("scaron" . 353)
    ("Yuml" . 376) ("circ" . 710) ("tilde" . 732) ("ensp" . 8194)
    ("emsp" . 8195) ("thinsp" . 8201) ("zwnj" . 8204) ("zwj" . 8205)
    ("lrm" . 8206) ("rlm" . 8207) ("ndash" . 8211) ("mdash" . 8212)
    ("lsquo" . 8216) ("rsquo" . 8217) ("sbquo" . 8218) ("ldquo" . 8220)
    ("rdquo" . 8221) ("bdquo" . 8222) ("dagger" . 8224) ("Dagger" . 8225)
    ("permil" . 8240) ("lsaquo" . 8249) ("rsaquo" . 8250) ("euro" . 8364)
    ;; symbol
    ("fnof" . 402) ("Alpha" . 913) ("Beta" . 914) ("Gamma" . 915)
    ("Delta" . 916) ("Epsilon" . 917) ("Zeta" . 918) ("Eta" . 919)
    ("Theta" . 920) ("Iota" . 921) ("Kappa" . 922) ("Lambda" . 923)
    ("Mu" . 924) ("Nu" . 925) ("Xi" . 926) ("Omicron" . 927)
    ("Pi" . 928) ("Rho" . 929) ("Sigma" . 931) ("Tau" . 932)
    ("Upsilon" . 933) ("Phi" . 934) ("Chi" . 935) ("Psi" . 936)
    ("Omega" . 937) ("alpha" . 945) ("beta" . 946) ("gamma" . 947)
    ("delta" . 948) ("epsilon" . 949) ("zeta" . 950) ("eta" . 951)
    ("theta" . 952) ("iota" . 953) ("kappa" . 954) ("lambda" . 955)
    ("mu" . 956) ("nu" . 957) ("xi" . 958) ("omicron" . 959)
    ("pi" . 960) ("rho" . 961) ("sigmaf" . 962) ("sigma" . 963)
    ("tau" . 964) ("upsilon" . 965) ("phi" . 966) ("chi" . 967)
    ("psi" . 968) ("omega" . 969) ("thetasym" . 977) ("upsih" . 978)
    ("piv" . 982) ("bull" . 8226) ("hellip" . 8230) ("prime" . 8242)
    ("Prime" . 8243) ("oline" . 8254) ("frasl" . 8260) ("weierp" . 8472)
    ("image" . 8465) ("real" . 8476) ("trade" . 8482) ("alefsym" . 8501)
    ("larr" . 8592) ("uarr" . 8593) ("rarr" . 8594) ("darr" . 8595)
    ("harr" . 8596) ("crarr" . 8629) ("lArr" . 8656) ("uArr" . 8657)
    ("rArr" . 8658) ("dArr" . 8659) ("hArr" . 8660) ("forall" . 8704)
    ("part" . 8706) ("exist" . 8707) ("empty" . 8709) ("nabla" . 8711)
    ("isin" . 8712) ("notin" . 8713) ("ni" . 8715) ("prod" . 8719)
    ("sum" . 8721) ("minus" . 8722) ("lowast" . 8727) ("radic" . 8730)
    ("prop" . 8733) ("infin" . 8734) ("ang" . 8736) ("and" . 8743)
    ("or" . 8744) ("cap" . 8745) ("cup" . 8746) ("int" . 8747)
    ("there4" . 8756) ("sim" . 8764) ("cong" . 8773) ("asymp" . 8776)
    ("ne" . 8800) ("equiv" . 8801) ("le" . 8804) ("ge" . 8805)
    ("sub" . 8834) ("sup" . 8835) ("nsub" . 8836) ("sube" . 8838)
    ("supe" . 8839) ("oplus" . 8853) ("otimes" . 8855) ("perp" . 8869)
    ("sdot" . 8901) ("lceil" . 8968) ("rceil" . 8969) ("lfloor" . 8970)
    ("rfloor" . 8971) ("lang" . 9001) ("rang" . 9002) ("loz" . 9674)
    ("spades" . 9824) ("clubs" . 9827) ("hearts" . 9829) ("diams" . 9830))
  "Alist mapping HTML 4.01 named character entity references to their
  numerical counterparts. Taken from the HTML 4.01 specification:
    http://www.w3.org/TR/html401/")

(defun tess-numericalize-entity ()
  "Replace the named character entity reference at point with its
numerical equivalent, if known."
  (interactive)
  (let ((end (point))
        (start (search-backward "&" (- (point) 7) t)))
    (when start
      (if (looking-at "&\\([a-z][a-z0-9]+\\)")
          (let* ((name (match-string 1))
                 (num (cdr (assoc name tess-html4-entity-map))))
            (if num
                (progn
                  (delete-region start end)
                  (insert (format "&#%s;" num))
                  t)
              (goto-char end)
              nil))
        (goto-char end)
        nil))))

(setq html-tag-face-alist
      '(("b" . bold)
        ("big" . bold)
        ("blink" . highlight)
        ("h1" bold underline)
        ("h4" . underline)
        ("h5" . underline)
        ("h6" . underline)
        ("rev"  . modeline)
        ("s" . underline)
        ("small" . default)
        ("strong" . bold)
        ("title" bold underline)
        ("tt" . default)
        ("u" . underline)
        ;; Were italic
        ("cite" . default)
        ("em" . bold)
        ("h2" bold underline)
        ("h3" underline)
        ("i" . italic)
        ("var" . default)))

(when (locate-library "css-mode")
  (autoload 'css-mode "css-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (eval-after-load "css-mode"
    '(cond ((boundp 'cssm-indent-function) ; larsga's css-mode.el
              (add-hook 'css-mode-hook
	                (lambda ()
	                  (setq cssm-mirror-mode nil
	                        cssm-newline-before-closing-bracket nil
	                        cssm-indent-function 'cssm-c-style-indenter))))
             ((fboundp 'css-extract-keyword-list) ; monnier's css-mode.el
              (setq css-basic-offset 2
	            css-indent-offset 2))
      (t nil))))

(add-to-list 'auto-mode-alist
             (cons "\\.\\(rdf\\|rss\\|atom\\)\\'" tess-xml-mode))

;;; editing relax-ng compact schema
(when (locate-library "rnc-mode")
  (autoload 'rnc-mode "rnc-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode)))

(defvar tess-javascript-mode 'javascript-generic-mode
  "What major mode should I be using for JavaScript.")

(cond ((locate-library "js")
       (setq tess-javascript-mode 'js-mode)
       (setq js-indent-level 4))
      ((locate-library "espresso")
       (setq tess-javascript-mode 'espresso-mode)
       (autoload 'espresso-mode "espresso" nil t)

       (setq espresso-indent-level 4))
      ((locate-library "js2-mode")
       (setq tess-javascript-mode 'js2-mode)
       (autoload 'js2-mode "js2-mode" nil t)
       (setq js2-highlight-level 3
             js2-cleanup-whitespace nil
             js2-bounce-indent-p nil
             js2-auto-indent-p nil
             js2-indent-on-enter-key t)
       (add-hook 'js2-mode-hook
                 (lambda () (setq mode-name "JS2"))))
      ((locate-library "javascript")
       (setq tess-javascript-mode 'javascript-mode)
       (setq js-indent-level 4
             javascript-indent-level 4
             javascript-auto-indent-flag nil)
       (autoload 'javascript-mode "javascript" nil t)
       (when (boundp 'javascript-mode-abbrev-table)
         (add-hook 'javascript-mode-hook
                   (lambda ()
                     (setq local-abbrev-table javascript-mode-abbrev-table))))))

(add-to-list 'auto-mode-alist (cons "\\.htc\\'" tess-javascript-mode))
(add-to-list 'auto-mode-alist (cons "\\.m?js\\'" tess-javascript-mode))
(add-to-list 'auto-mode-alist (cons "\\.json\\'" tess-javascript-mode))

(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq fill-column most-positive-fixnum)))
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (define-key markdown-mode-map (kbd "<")
                'self-insert-command)))

  (when (executable-find "markdown2")
    (setq markdown-command "markdown2")))

(when (locate-library "quickurl")
  (require 'quickurl)

  (setq quickurl-url-file (expand-file-name "~/.quickurls"))

  (defun tess-quickurl-or-dabbrev-expand ()
    "Perform quickurl or dabbrev expansion at point.

  If the current word has a quickurl defined, insert the quickurl.
  If it doesn't, try dabbrev expansion."
    (interactive)
    (quickurl-load-urls)
    (let* ((end (point))
           (beg (save-excursion (forward-word -1) (point)))
           (keyword (buffer-substring-no-properties beg end))
           (url (quickurl-find-url keyword)))
      (if (null url)
          (call-interactively 'dabbrev-expand)
        (delete-region beg end)
        (quickurl-insert url)
        (insert " "))))

  (defun tess-format-quickurl-in-markdown (url)
    (format "[%s](%s)"
            (quickurl-url-keyword url) (quickurl-url-url url)))

  (defun tess-format-quickurl-in-html (url)
    (format "<a href=\"%s\">%s</a>"
            (quickurl-url-url url) (quickurl-url-keyword url)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (define-key markdown-mode-map (kbd "M-/")
                'tess-quickurl-or-dabbrev-expand)
              (setq-local quickurl-format-function
                         'tess-format-quickurl-in-markdown)))

  (add-hook 'html-mode-hook
            (lambda ()
              (define-key html-mode-map (kbd "M-/")
                'tess-quickurl-or-dabbrev-expand)
              (setq-local quickurl-format-function
                          'tess-format-quickurl-in-html)))
)

(when (locate-library "shr")
  (setq shr-color-visible-distance-min 10
        shr-color-visible-luminance-min 60))

(when (locate-library "bikeshed")
  (autoload 'bikeshed-mode "bikeshed" nil t)
  (add-to-list 'auto-mode-alist '("\\.bs\\'" . bikeshed-mode))
)

(when (locate-library "deft")
  (autoload 'deft "deft" nil t)
  (setq deft-directory (expand-file-name "~/Notes/")
        deft-text-mode (seq-find 'fboundp '(markdown-mode text-mode)))
  (setq deft-extension
        (assoc-default deft-text-mode '((markdown-mode . "md"))
                       'eq "txt"))

  ;; Completely override Deft's keybindings to be more like Dired and
  ;; Gnus:
  (setq deft-mode-map (make-sparse-keymap))
  (define-key deft-mode-map (kbd "a") 'deft-new-file-named)
  (define-key deft-mode-map (kbd "d") 'deft-delete-file)
  (define-key deft-mode-map (kbd "g") 'deft-refresh)
  (define-key deft-mode-map (kbd "n") 'next-line)
  (define-key deft-mode-map (kbd "q") 'quit-window)
  (define-key deft-mode-map (kbd "p") 'previous-line)
  (define-key deft-mode-map (kbd "r") 'deft-rename-file)
  (define-key deft-mode-map (kbd "s") 'deft-filter)
  (define-key deft-mode-map (kbd "z") 'deft-filter-clear)
  (define-key deft-mode-map (kbd "<RET>") 'deft-complete))

(add-to-list 'auto-mode-alist '("\\.arc\\'" . lisp-mode))

;; completion in M-:
(when (keymapp read-expression-map)
  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol))

(defun tess-comment-sexp ()
  (interactive)
  (call-interactively 'mark-sexp)
  (call-interactively 'comment-region))

(defun tess-install-lispy-bindings (map bind-ret)
  "FIXME"
  (define-key map (kbd "M-k") 'kill-sexp)
  (define-key map (kbd "\"")
    (seq-find 'commandp '(paredit-doublequote skeleton-pair-insert-maybe)))
  (define-key map (kbd "C-M-;") 'tess-comment-sexp)
  (when bind-ret
    (define-key map (kbd "RET")
      (seq-find 'commandp '(paredit-newline newline-and-indent))))
  (define-key map (kbd "(")
    (seq-find 'commandp '(paredit-open-parenthesis
                         paredit-open-list
                         insert-parentheses)))
  (define-key map (kbd ")")
    (seq-find 'commandp '(paredit-close-parenthesis-and-newline
                         ;; paredit-close-list-and-newline
                         move-past-close-and-reindent))))
(tess-install-lispy-bindings
 (cond ((boundp 'lisp-mode-shared-map) lisp-mode-shared-map)
       ((boundp 'shared-lisp-mode-map) shared-lisp-mode-map)
       (t emacs-lisp-mode-map))
 t)

(eval-after-load "ielm"
  '(tess-install-lispy-bindings ielm-map nil))

(define-key minibuffer-local-map (kbd "M-i") (lambda () (interactive) (insert ?i)))

(add-to-list 'auto-mode-alist '("\\.elc\\'" . emacs-lisp-mode))

(defun tess-dedangle-parens-in-region (start end)
  "De-dangle close parens between START and END."
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "[ \t\n]+)" end t)
    (replace-match ")")))

(defun tess-make-lisp-idiomatic-in-region (start end)
  "Make the Lisp code from START to END a bit more idiomatic.
You might consider running `checkdoc' as well."
  (interactive "r\nP")
  (save-restriction
    (widen)
    (narrow-to-region start end)
    (setq start (point-min-marker)
          end   (point-max-marker))
    (tess-dedangle-parens-in-region start end)
    (indent-region start end)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p (tess-frame-display frame))
              (require 'parenface nil t))))

(dolist (candidate '("mzscheme"))
  (when (executable-find candidate)
    (setq scheme-program-name candidate)))

(define-skeleton tess-elisp-when-locate-library-skeleton
  "Skeleton for (when (locate-library \"foo\") ... ) forms."
  ;; This `completing-read' form based on `find-library's `interactive'
  ;; spec, but generalized to work under different Emacsen.
  (completing-read "Library name: "
                   (when (fboundp 'locate-file-completion)
                     'locate-file-completion)
                   (cons (if (boundp 'find-function-source-path)
                             find-function-source-path
                           load-path)
                         ;; (find-library-suffixes)
                         '(".el" ".el.gz" ".gz")))
  "when (locate-library \"" str "\")")

(when (locate-library "eldoc")
  (mapc (lambda (mode-hook)
          (add-hook mode-hook 'turn-on-eldoc-mode))
        '(emacs-lisp-mode-hook lisp-interaction-mode-hook
          ielm-mode-hook))

  (setq eldoc-argument-case 'help-default-arg-highlight))

(defun tess-macroexpand-sexp-at-point ()
  "Replace the s-expresion at point with its macroexpansion."
  (interactive)

  (let (pre start end)
    (save-excursion
      (up-list -1)
      (setq start (point))
      (setq pre (sexp-at-point))
      (forward-sexp 1)
      (setq end (point)))

    (goto-char start)
    (kill-region start end)

    (pp (macroexpand pre) (current-buffer))))

(defun tess-indent-containing-sexp ()
  "Fix the indentation of the sexp containing point."
  (interactive)
  (save-excursion
    (up-list -1)
    (indent-sexp)))

(global-set-key (kbd "C-c i") 'tess-indent-containing-sexp)

(setq ielm-prompt "* ")

(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

(add-to-list 'auto-mode-alist '("\\.lisp-expr\\'" . lisp-mode))

(mapc (lambda (hook)
        (add-hook hook
                  (lambda ()
                    (set (make-local-variable 'lisp-indent-function)
                         'common-lisp-indent-function))))
      '(lisp-mode-hook inferior-lisp-mode-hook))

(setq inferior-lisp-program
      (or (executable-find "sbcl")
          (executable-find "lisp")
          (executable-find "openmcl")
          (executable-find "clisp")))

(when (locate-library "slime")
  (autoload 'slime-mode "slime" nil t)
  (add-hook 'lisp-mode-hook (lambda nil (slime-mode 1)))
  (autoload 'inferior-slime-mode "slime" nil t)
  (add-hook 'inferior-lisp-mode-hook
            (lambda nil (inferior-slime-mode 1)))
  (add-hook 'slime-repl-mode-hook 'tess-hide-trailing-whitespace))

(cond ((commandp 'find-library)
       (defalias 'tess-find-library 'find-library))
      ((fboundp 'find-library)
       (defun tess-find-library (library)
         "Open LIBRARY."
         (interactive "sLibrary: ")
         (find-library library)))
      (t
       (defun tess-find-library (library)
         "Open LIBRARY."
         (interactive "sLibrary: ")
         (let ((filename (locate-library (concat library ".el"))))
           (if (stringp filename)
               (find-file filename)
             (message "Library %s not found." library))))))

(global-set-key (kbd "C-c L") 'tess-find-library)

(when (locate-library "dns-mode")
  (add-hook 'dns-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        'dns-mode-soa-increment-serial
                        nil t))))
(when (locate-library "rpm-spec-mode")
  (unless (fboundp 'user-mail-address)
    (defun user-mail-address ()
      "Returns the value of the `user-mail-address' variable."
      user-mail-address))

  (autoload 'rpm-spec-mode "rpm-spec-mode" nil t)
  ;; RPM specfiles are .spec
  (add-to-list 'auto-mode-alist '("\\.spec\\'" . rpm-spec-mode)))

(when (locate-library "rfcview")
  (autoload 'rfcview-mode "rfcview" nil t)
  (add-to-list 'auto-mode-alist '("rfc[0-9]+\\.txt" . rfcview-mode)))

(when (locate-library "cfengine")
  (autoload 'cfengine-mode "cfengine" nil t)
  (add-to-list 'auto-mode-alist
               '("cf\\(\\.\\|agent\\.conf\\)" . cfengine-mode))
  (defalias 'cfengine-beginning-of-line 'beginning-of-line)
  (setq cfengine-indent 4))


(when (locate-library "ruby-mode")
  ;; Autoloads
  (autoload 'ruby-mode "ruby-mode" nil t)

  ;; File associations, etc.
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  ;; fixme: use two-mode-mode when possible
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . html-mode))

  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

  ;; Key bindings
  (eval-after-load "ruby-mode"
    '(define-key ruby-mode-map (kbd "RET")
       'ruby-reindent-then-newline-and-indent))

  ;; Install key bindings for running an inferior Ruby in `ruby-mode'.
  (when (locate-library "inf-ruby")
    (autoload 'run-ruby "inf-ruby" nil t)
    (autoload 'inf-ruby-keys "inf-ruby" nil)
    (add-hook 'ruby-mode-hook 'inf-ruby-keys))

  ;; Skeletons

  (define-skeleton tess-rails-migrate-create-table
    "Skeleton for creating a table in a rails migration."
    "Table name: "
    > "create_table \"" str "\" do |t|" \n
    _ \n
    -2 "end" \n)

  (define-skeleton tess-rails-migrate-drop-table
    "Skeleton for dropping a table in a rails migration."
    "Table name: "
    > "drop_table \"" str "\"" \n)

  (define-skeleton tess-rails-migrate-table-column
    "Skeleton for adding a column in a rails migration."
    "Column name: "
    > "t.column \"" str "\", :" (skeleton-read "Column type: " "string"))

  (define-skeleton tess-rails-migrate-add-column
    "Skeleton for adding a column in a rails migration."
    "Table name: "
    > "add_column \"" str
    "\", \"" (skeleton-read "Column name: ")
    "\", :" (skeleton-read "Column type: " "string"))

  (define-skeleton tess-rails-migrate-remove-column
    "Skeleton for adding a column in a rails migration."
    "Table name: "
    > "remove_column \"" str
    "\", \"" (skeleton-read "Column name: ") "\""))

(when (locate-library "cperl-mode")
  (autoload 'cperl-mode "cperl-mode" nil t)
  (add-to-list 'auto-mode-alist
               '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
  (fset 'perl-mode 'cperl-mode)
  (add-hook 'cperl-mode-hook 'turn-off-auto-fill)
  (setq cperl-hairy t))

;;; Python

(when (locate-library "pymacs")
  (autoload 'pymacs-load "pymacs")
  (defun tess-load-ropemacs ()
    "Load ropemacs iff pymacs is installed and ropemacs isn't loaded."
    (interactive)
    (when (fboundp 'pymacs-load)
      (unless (featurep 'ropemacs)
        (pymacs-load "ropemacs" "rope-" t)
        (ropemacs-mode 1))))

  (add-hook 'python-mode-hook 'tess-load-ropemacs))

(unless (locate-library "python")
  (when (locate-library "python-mode")
    (autoload 'python-mode "python-mode" nil t)
    (add-to-list 'auto-mode-alist
                 '("\\.\\(py\\|tac\\)\\'" . python-mode))
    (add-to-list 'interpreter-mode-alist
                 '("python" . python-mode))))


(add-to-list 'auto-mode-alist '("\\.view\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.psql\\'" . sql-mode))
(setq sql-sqlite-program "sqlite3")

(when (locate-library "maxima")
  (autoload 'maxima "maxima" nil t)
  (add-to-list 'auto-mode-alist '("\\.max\\'" . maxima))
  (autoload 'maxima-mode "maxima" nil t))

(when (locate-library "imaxima")
  (autoload 'imaxima "imaxima" nil t)
  (setq imaxima-pt-size 12
        imaxima-fnt-size "Huge"
        imaxima-image-type 'ps
        imaxima-use-maxima-mode-flag (locate-library "maxima")))

(when (require 'tex-site nil t)
  (setq-default TeX-auto-untabify nil)
  (setq TeX-auto-untabify nil))

;; AUC-TeX
(setq font-latex-script-display nil)
;; TeX mode
(eval-after-load "tex-mode"
  '(defun tex-font-lock-suscript (pos)
     '(face default)))

(when (locate-library "table")
  (autoload 'table-insert "table" nil t)
  (global-set-key (kbd "C-c t") 'table-insert))

(when (and (not (fboundp 'org-mode))
           (locate-library "org"))
  (autoload 'org-mode "org" nil t))

(when (fboundp 'org-mode)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key (kbd "C-c o l") 'org-store-link)
  (global-set-key (kbd "C-c o a") 'org-agenda))

(when (locate-library "noweb-mode")
  (autoload 'noweb-mode "noweb-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.nw\\'" . noweb-mode))
  (setq noweb-mode-prefix (kbd "<f5>"))

  (add-hook 'noweb-select-mode-hook
              (lambda () (visual-line-mode 1)))

  (defadvice noweb-update-chunk-vector (around tess-noweb-redisplay
                                               activate)
    (let ((inhibit-redisplay t)
          (tess-noweb-redisplay-message
           "Updating noweb's chunk vector"))
      (message "%s..." tess-noweb-redisplay-message)
      ad-do-it
      (message "%s...done" tess-noweb-redisplay-message)))

  (eval-after-load "noweb-mode"
    '(fset 'noweb-fill-paragraph-chunk 'fill-paragraph))

  (when (fboundp 'font-lock-add-keywords)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (when (and (boundp 'noweb-mode) noweb-mode)
                  (font-lock-add-keywords
                   nil
                   '(("\\([<][<][^>]*[>][>]\\)"
                      (1 font-lock-string-face))))))))

  (setq-default noweb-default-code-mode 'emacs-lisp-mode
                noweb-code-mode         'emacs-lisp-mode))


;;; Work.

(defun tess-thingatpt-install-apple-uri-schemes ()
  (let ((schemes '("rdar://" "adir://" "afp://" "message:")))
    (mapc (lambda (scheme)
            (add-to-list 'thing-at-point-uri-schemes scheme))
          schemes))
  (setq thing-at-point-url-regexp
        (concat "\\<\\(" (mapconcat 'identity thing-at-point-uri-schemes
                                    "\\|") "\\)"
                thing-at-point-url-path-regexp)))

(eval-after-load "thingatpt"
  '(tess-thingatpt-install-apple-uri-schemes))

(defun tess-intatpt-end-op ()
  (while (looking-at "[0-9]")
    (forward-char 1)))
(put 'integer 'end-op 'tess-intatpt-end-op)

(defun tess-intatpt-beginning-op ()
  (unless (looking-at "[0-9]")
    (backward-char 1))
  (while (looking-at "[0-9]")
    (backward-char 1))
  (forward-char 1))
(put 'integer 'beginning-op 'tess-intatpt-beginning-op)

(defmacro tess-define-bug-linker (cmd docstring format-string)
  `(defun ,cmd (bug-number)
     ,docstring
     (interactive (list (or (thing-at-point 'integer)
                            (read-string "Bug number: "))))
     (browse-url (format ,format-string bug-number))))
(put 'tess-define-bug-linker 'lisp-indent-function 1)
(put 'tess-define-bug-linker 'doc-string-elt 2)

(tess-define-bug-linker tess-moz-bug
  "Browse to the Mozilla bug whose number is at point.
If there's no bug number at point, prompt for one."
  "https://bugzilla.mozilla.org/show_bug.cgi?id=%s")
(tess-define-bug-linker tess-radar-bug
  "Browse to the Radar bug whose number is at point.
If there's no bug number at point, prompt for one."
  "rdar://problem/%s")
(tess-define-bug-linker tess-w3c-bug
  "Browse to the W3C bug whose number is at point.
If there's no bug number at point, prompt for one."
  "http://www.w3.org/Bugs/Public/show_bug.cgi?id=%s")
(tess-define-bug-linker tess-webkit-bug
  "Browse to the WebKit bug whose number is at point.
If there's no bug number at point, prompt for one."
  "https://bugs.webkit.org/show_bug.cgi?id=%s")

(defun tess-read-bug-dwim (&optional num)
  (interactive (list (or (thing-at-point 'integer)
                         (read-string "Bug number: "))))
  (cond
   ((eq (erc-current-network) 'apple)
    (tess-radar-bug num))
   ((save-excursion (re-search-forward "bugs.webkit.org" nil t))
    (tess-webkit-bug num))
   ((save-excursion (re-search-forward "bugzilla.mozilla.org" nil t))
    (tess-moz-bug num))
   ((save-excursion (re-search-forward "www.w3.org[/]Bugs" nil t))
    (tess-w3c-bug num))
   ((save-excursion (re-search-backward "public-html-bugzilla" nil t))
    (tess-w3c-bug num))
   ((save-excursion
      (re-search-backward
       "This notification covers \\(\\d \\)?problemIDs?" nil t))
    (tess-radar-bug num))
   ((and (string-match "^[*]Article" (buffer-name))
         (save-excursion
           (gnus-summary-toggle-header 1)
           (gnus-article-goto-header "Return-path")
           (prog1 (string-equal "bugzilla-daemon@webkit.org"
                                (buffer-substring-no-properties
                                 (+ 2 (point)) (1- (line-end-position))))
             (gnus-summary-toggle-header -1))))
    (tess-webkit-bug num))
   (t (error "Unable to determine bug db for bug #%s" num))))

;;; Customizations which are specific to using Emacs under the various
;;; windowing systems.

(global-set-key (kbd "C-c a") 'browse-url)
(cond ((eq system-type 'darwin)
       (setq browse-url-browser-function
             'browse-url-default-macosx-browser))
      (t
       (dolist (candidate '("chromium-browser" "google-chrome"))
         (when (executable-find candidate)
           (setq browse-url-browser-function 'browse-url-generic
                 browse-url-generic-program candidate)))))


(setq default-frame-alist '())

(defun tess-frob-xterm (frame)
  (when (and (tess-xterm-p) (require 'xterm-frobs nil t))
    (mapc
     (lambda (pair)
       (let ((key (car pair))
             (value (cdr pair)))
         (cond
          ((eq key 'foreground-color) (xterm-set-background-color value))
          ((eq key 'background-color) (xterm-set-foreground-color value))
          ((eq key 'mouse-color) (xterm-set-mouse-foreground-color value))
          ((eq key 'cursor-color) (xterm-set-cursor-color value)))))
     default-frame-alist)))

(add-hook 'after-make-frame-functions 'tess-frob-xterm)

(setq ns-antialias-text t)

(defvar tess-fonts
  '(;; The best
    "Menlo"
    ;; Very nice console fonts from Microsoft
    "Cascadia Code"
    "Consolas"
    ;; Free-as-in-* options
    "Inconsolata"
    "Ubuntu Mono"
    "Monospace"
    "DejaVu Sans Mono"
    ;; Decent defaults on the Mac
    "Andale Mono" "Monaco"
    ;; My old X11 standbys
    "Screen" "Lucida Console"
    ;; A fallback that's on every system ever.
    "Courier")
  "Fonts I like, in decreasing order of preference.")

(defvar tess-typographic-scale
  '(6 7 8 9 10 11 12 14 16 18 21 24)
  "The typographic scale used for selecting a font size.")

(defvar tess-screen-height-to-type-size-ratio 56.0
  "Ratio of screen height (in pixels) to my preferred type size.")

(defun tess-closest-traditional-type-size (size)
  "Return the entry in `tess-typographic-scale' closest to SIZE."
  (caar (sort (mapcar (lambda (candidate)
                        (cons candidate (abs (- size candidate))))
                      tess-typographic-scale)
              (lambda (a b) (< (cdr a) (cdr b))))))

(defun tess-guess-font-size ()
  "Guess an appropriate font size for this display."
  (tess-closest-traditional-type-size
   (/ (display-pixel-height) tess-screen-height-to-type-size-ratio)))

(defvar tess-font-size (if window-system (tess-guess-font-size) nil)
  "My preferred font size.
Can be overridden in local.el.")

(defun tess-font-spec (font size)
  (format "%s-%d" font size))

(defun tess-find-font ()
  "Return the first available font listed in `tess-fonts'."
  (let ((family-list (font-family-list)))
    (seq-find (lambda (font) (member font family-list))
              tess-fonts)))

(defvar tess-font nil
  "My preferred font.
Can be overridden in local.el.")

(defun tess-font-init ()
  (when window-system
    (setq tess-font (tess-find-font))))

(add-hook 'tess-after-local-init-hook 'tess-font-init)

(defun tess-set-font (&optional font size)
  "Figure out and install which font and size I use on this system.
If called interactively, prompts the user for the font and size to use."
  (interactive
   (list (completing-read (format "Font (default %s): " tess-font)
                          (font-family-list) nil t nil nil tess-font)
         (read-number "Size: " (tess-guess-font-size))))
  (let* ((font (or font (tess-find-font)))
         (size (or size tess-font-size))
         (font-spec (tess-font-spec font size)))
    (setq tess-font-size size)
    (add-to-list 'default-frame-alist (cons 'font font-spec))
    (set-frame-font font-spec)))

(unless (tess-tty-p)
  (add-hook 'tess-after-local-init-hook 'tess-set-font)
  ;; Now that we've picked a font, we can fixup Emacs' fontset to
  ;; correctly display emoji.
  (when (and (eq (window-system) 'ns)
             (version<= emacs-version "25.0.91"))
    (add-hook 'tess-after-local-init-hook 'tess-fixup-emoji-on-mac)))

(when (fboundp 'text-scale-increase)
  (global-set-key (kbd "M-=") 'text-scale-increase)
  (global-set-key (kbd "M--") 'text-scale-decrease))

(add-to-list 'default-frame-alist '(wait-for-wm . nil))
(add-to-list 'default-frame-alist
             (cons 'menu-bar-lines (tess-menu-bar-lines)))

(defvar tess-emacs-width nil
  "Iff non-null, the initial width that Emacs should use.")
(defvar tess-emacs-height nil
  "Iff non-null, the initial height that Emacs should use.")
(defvar tess-emacs-top nil
  "Iff non-null, the initial top that Emacs should use.")
(defvar tess-emacs-left nil
  "Iff non-null, the initial left that Emacs should use.")

(defun tess-reset-emacs-geometry ()
  (set-frame-parameter nil 'width tess-emacs-width)
  (set-frame-parameter nil 'height tess-emacs-height)
  (set-frame-parameter nil 'top 0)
  (set-frame-parameter nil 'left 0))

(defun tess-respond-to-screen-geometry ()
  (interactive)
  (tess-set-font nil (tess-guess-font-size))
  (setq tess-emacs-width 81)
  (cond ((= (display-pixel-width) 1280) ;; 12" MacBooks
         (setq tess-emacs-height 46))
        ((and (= (display-pixel-width) 1366) ;; yt
              (eq system-type 'gnu/linux))
         (setq tess-emacs-height 34))
        ((and (= (display-pixel-width) 1366) ;; oban
              (eq system-type 'darwin))
         (setq tess-emacs-height 44))
        ((= (display-pixel-width) 1920) ;; XPS
         (setq tess-emacs-height 24))
        ((= (display-pixel-width) 2240) ;; 21" M1 iMac
         (setq tess-emacs-height 50))
        ((= (display-pixel-width) 2560) ;; 27" iMacs
         (setq tess-emacs-height 47)))
  (tess-reset-emacs-geometry))

(defun tess-init-emacs-geometry ()
  (when window-system
    (when tess-emacs-width
      (add-to-list 'default-frame-alist (cons 'width tess-emacs-width)))
    (when tess-emacs-height
      (add-to-list 'default-frame-alist (cons 'height tess-emacs-height)))
    (when tess-emacs-left
      (add-to-list 'default-frame-alist (cons 'left tess-emacs-left)))
    (when tess-emacs-top
      (add-to-list 'default-frame-alist (cons 'top tess-emacs-top))))
  ;; Finally, ensure both frame alists are the same.
  (setq initial-frame-alist default-frame-alist))

(add-hook 'tess-before-local-init-hook 'tess-init-emacs-geometry)

(when window-system
  (add-hook 'tess-after-local-init-hook 'tess-respond-to-screen-geometry))


(when (fboundp 'menu-bar-mode)
  (menu-bar-mode (tess-menu-bar-lines)))

(when (display-graphic-p)
  (setq frame-title-format
        (concat (if (string-equal (user-login-name) "root")
                    "SU: "
                  "")
                "%b (Emacs on "
                (or (getenv "HOST") (system-name) "unknown")
                ")"))

  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (eq (window-system frame) 'w32)
                (scroll-bar-mode -1))))

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)
    (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))

  (when (featurep 'tooltip)
    (setq tooltip-gud-tips-p t)))

(when (display-graphic-p)
  (setq focus-follows-mouse (eq (window-system) 'x)
        mouse-autoselect-window t)

  (setq-default mouse-yank-at-point t)

  (cond ((fboundp 'mouse-wheel-mode)
         (mouse-wheel-mode 1))
        ((locate-library "mwheel")
         (unless (fboundp 'mwheel-install)
	   (autoload 'mwheel-install "mwheel" nil nil))
	 (setq mwheel-follow-mouse t)
	 (setq mwheel-scroll-amount '(4 . 1))
	 (mwheel-install))))

(setq ns-pop-up-frames nil) ; NSEmacs (Emacs.app, aqua, from NeXT Emacs)
(when (fboundp 'one-buffer-one-frame-mode)
  (setq one-buffer-one-frame nil) ; Aquamacs is dumb.
  (one-buffer-one-frame-mode 0))

(when (featurep 'aquamacs)
  (setq aquamacs-buffer-specific-frame-themes nil
        aquamacs-mode-specific-default-themes nil
        aquamacs-auto-frame-parameters-flag   nil
        special-display-buffer-names          nil
        special-display-regexps               nil)

  (setq kill-emacs-query-functions
        (delq 'aquamacs-ask-to-save-options kill-emacs-query-functions)))
(when (fboundp 'smart-frame-positioning-mode)
  (smart-frame-positioning-mode 0))

;;; Customizing Emacs' colors.

(autoload 'xterm-register-default-colors "term/xterm")
(when (and (not window-system)
           (string-match "256color" (or (getenv "TERMCAP") "")))
  (xterm-register-default-colors xterm-standard-colors))

(when (and (not (display-graphic-p))
           (string-match "screen-256color" (or (getenv "TERMCAP") "")))
  (global-set-key (kbd "M-[ A") (kbd "<up>"))
  (global-set-key (kbd "M-[ B") (kbd "<down>"))
  (global-set-key (kbd "M-[ C") (kbd "<right>"))
  (global-set-key (kbd "M-[ D") (kbd "<left>")))

(when (locate-library "face-list")
  (autoload 'customize-face-at "face-list" nil t)
  (global-set-key (kbd "C-c f c") 'customize-face-at)

  (autoload 'describe-face-at "face-list" nil t)
  (global-set-key (kbd "C-c f d") 'describe-face-at))

(when (display-color-p)
  (when (locate-library "htmlize")
    (autoload 'htmlize-buffer "htmlize" nil t))

  (defun tess-stringify-face-name (face)
    "Blah blah blah."
    (when (listp face)
      (setq face (car (last face))))
    (let ((name (if (symbolp face) (symbol-name face) face)))
      (if (string-match "^\\(font-lock-\\)?\\(.+?\\)\\(-name\\)?\\(-face\\)?$" ; $
                        name)
          (match-string 2 name)
        name)))

  (defvar tess-htmlize-face-translation-map
    '(;; I don't want to highlight parens in lisp
      (paren-face . nil)
      ;; Parts of X{,HT}ML I don't want to highlight
      (nxml-tag-delimiter . nil)
      (nxml-tag-delimiter-face . nil)
      (nxml-tag-slash . nil)
      (nxml-tag-slash-face . nil)
      (nxml-text . nil)
      (nxml-text-face . nil)
      ;; CSS
      (css-property . font-lock-function-name-face)
      ;; Map nXML faces onto standard font-lock faces
      (nxml-attribute-value-delimiter . font-lock-string-face)
      (nxml-attribute-value-delimiter-face . font-lock-string-face)
      (nxml-attribute-value . font-lock-string-face)
      (nxml-attribute-value-face . font-lock-string-face)
      (nxml-attribute-local-name . font-lock-builtin-face)
      (nxml-attribute-local-name-face . font-lock-builtin-face)
      (nxml-element-local-name . font-lock-keyword-face)
      (nxml-element-local-name-face . font-lock-keyword-face)
      ;; Makefiles
      (makefile-targets . font-lock-function-name-face)
      (makefile-shell . nil)
      ;; MML tags should look like types
      (message-mml-face . font-lock-type-face)
      (message-mml . font-lock-type-face))
    "Blah blah blah")

  (defun tess-htmlize-face-at-point (&optional pos)
    "Blah blah blah."
    (unless pos (setq pos (point)))
    (let* ((face (get-text-property pos 'face))
           (translation (member* face tess-htmlize-face-translation-map
                                 :key 'car)))
      (if translation
          (cdar translation)
        face)))

  (defun tess-htmlize-region (start end)
    "Place an htmlized version of the region into the kill ring."
    (interactive "r")
    (let ((code (buffer-substring start end))
          (language (substring (symbol-name major-mode) 0 -5))
          current-face)
      (with-temp-buffer
        (fundamental-mode)
        (font-lock-mode -1)

        (insert code)

        (goto-char (point-min))

        (setq current-face (tess-htmlize-face-at-point))
        (insert (format "<pre><code class=\"%s\">" language))
        (when current-face
          (insert (format "<span class=\"%s\">"
                          (tess-stringify-face-name current-face))))

        (let (n-p-c)
          (while (setq n-p-c (next-property-change (point)))
            (goto-char n-p-c)
            (cond
             ((and current-face (eq current-face (tess-htmlize-face-at-point))))
             ((and current-face (null (tess-htmlize-face-at-point)))
              (insert "</span>")
              (setq current-face nil))
             ((and current-face (tess-htmlize-face-at-point))
              (insert "</span>")
              (setq current-face (tess-htmlize-face-at-point))
              (when current-face
                (insert (format "<span class=\"%s\">"
                                (tess-stringify-face-name current-face)))))
             ((tess-htmlize-face-at-point)
              (setq current-face (tess-htmlize-face-at-point))
              (when current-face
                (insert (format "<span class=\"%s\">"
                                (tess-stringify-face-name current-face))))))))
        (goto-char (point-max))
        (insert "</code></pre>")
        (setq code (buffer-substring-no-properties (point-min) (point-max))))
      (kill-new code)))

  (cond
   ((and (fboundp 'load-theme) (locate-library "hober2-theme"))
    (let ((tess-theme-dir (file-name-directory (locate-library "hober2-theme"))))
      (when (boundp 'custom-theme-load-path)
        (add-to-list 'custom-theme-load-path tess-theme-dir))
      (load "hober2-theme")
      (enable-theme 'hober2)))

   ((require 'color-theme nil t)
     ;; (setq color-theme-is-cumulative nil)
     (require 'color-theme-hober2 nil t)
     (let ((theme (seq-find 'fboundp '(color-theme-hober2 color-theme-resolve color-theme-hober))))
       (when theme
         (add-hook 'tess-after-local-init-hook theme)))))
  (setq ansi-term-color-vector
    [unspecified "black" "orange red"
     "DarkGreen" "sandy brown" "dark slate blue"
     "pale violet red" "cadet blue" "gray"])

  (require 'font-lock)

  (setq font-lock-maximum-size most-positive-fixnum)

  (when (fboundp 'global-font-lock-mode)
    (defun tess-enable-font-lock-globally ()
      (global-font-lock-mode 1))
    (add-hook 'tess-after-local-init-hook
              'tess-enable-font-lock-globally))

  (when (fboundp 'toggle-global-lazy-font-lock-mode)
    (add-hook 'tess-after-local-init-hook
              'toggle-global-lazy-font-lock-mode))

  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;;; Host-specific customizations.

(run-hooks 'tess-before-local-init-hook)
(require 'local nil t)
(run-hooks 'tess-after-local-init-hook)

(setq custom-unlispify-tag-names       nil
      custom-unlispify-menu-entries    nil
      custom-unlispify-remove-prefixes nil)

(setq custom-file
      (expand-file-name (format "Emacs-%d-custom.el" emacs-major-version)
                        tess-elisp-dir))
(load custom-file t)

;;; .emacs ends here
