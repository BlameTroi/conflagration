;;; init.el --- troy brumley's init.el -*- lexical-binding: t -*-


;;; Commentary:

;; just an init file. if you are running with flycheck/flymake, it
;; will report several false positives. this init turns on flymake
;; globally, so you see the warnings. flycheck and flymake aren't
;; meant for init style elisp, but i leave it on to get in the habit
;; of checking lint warnings.

;; * review this literate config, it's pretty clean and clear:
;;   https://whhone.com/emacs-config/

;; i am aware of the formatting preference of no dangling close
;; parens, but i sometimes leave the closing paren in use-package
;; blocks on its own line for the sake of error avoidance when working
;; on the settings and for clarity in diff output. when i'm done
;; tweaking i may undangle them if i see them.

;; breaking out sections to separately loadable files to help with
;; organization. i had a bad habit of not making sure additions to my
;; init.el were in the right place and i hope this keeps me honest.

;; the troi-init subdirectory is not in my load-path so the load by
;; file instead of require approach is used. while flymake complains
;; if i don't have a proper file footer with (provide 'whatever), i
;; wonder if that is still needed when doing things this way?


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

")


;; use-package, not straight or elpaca. it works for me.
(load-file
 (expand-file-name "troi-init/packaging.el" user-emacs-directory))


;; macos is a horse of an entirely different color
(when (eq system-type 'darwin)
  (load-file
   (expand-file-name "troi-init/macos.el" user-emacs-directory)))


;; other directories, load paths, and such
(load-file
 (expand-file-name "troi-init/filesys.el" user-emacs-directory))

;; options (setq/setopt) that don't fit elsewhere.
(load-file
 (expand-file-name "troi-init/setqopts.el" user-emacs-directory))


;; themes, colors, faces, icons
(load-file
 (expand-file-name "troi-init/colorful.el" user-emacs-directory))


;; global keybindings
(load-file
 (expand-file-name "troi-init/keybinding.el" user-emacs-directory))


;; movement
(load-file
 (expand-file-name "troi-init/movement.el" user-emacs-directory))


;; spacing, tabs, trailing whitespace
(load-file
 (expand-file-name "troi-init/spacetab.el" user-emacs-directory))


;; completions
(load-file
 (expand-file-name "troi-init/completion.el" user-emacs-directory))


;; programming infrastructure
(load-file
 (expand-file-name "troi-init/projection.el" user-emacs-directory))
(load-file
 (expand-file-name "troi-init/epiglotus.el" user-emacs-directory))
(load-file
 (expand-file-name "troi-init/magitfisance.el" user-emacs-directory))
(load-file
 (expand-file-name "troi-init/eldocbox.el" user-emacs-directory))


;; editing helpers
(load-file
 (expand-file-name "troi-init/spindle.el" user-emacs-directory))
(load-file
 (expand-file-name "troi-init/hocusfocus.el" user-emacs-directory))
(load-file
 (expand-file-name "troi-init/undone.el" user-emacs-directory))


;; c programming
(load-file
 (expand-file-name "troi-init/lostatc.el" user-emacs-directory))
(load-file
 (expand-file-name "troi-init/cformat.el" user-emacs-directory))


;; fortran (f90) programming
(load-file
 (expand-file-name "troi-init/fartron.el" user-emacs-directory))


;; miscellaneous languages and modes, small stuff
(load-file
 (expand-file-name "troi-init/miscellany.el" user-emacs-directory))


;; org and markdown and other textual modes
(load-file
 (expand-file-name "troi-init/orgness.el" user-emacs-directory))


;; various applications -- turns out i just use the web interfaces for
;; things on a tablet, so these add no value and i'm removing the
;; packages but leaving the init files for reference in case i change
;; my habits.
;;
;;(load-file
;; (expand-file-name "troi-init/feedme.el" user-emacs-directory))
;;(load-file
;; (expand-file-name "troi-init/hn.el" user-emacs-directory))
;;(load-file
;; (expand-file-name "troi-init/tidder.el" user-emacs-directory))
;;(load-file
;; (expand-file-name "troi-init/idohow.el" user-emacs-directory))


;; shell stuff
;;(load-file
;; (expand-file-name "troi-init/shellac.el" user-emacs-directory))


;; finally -- customization

;; i'm not a fan of splitting things between customization and the
;; declarative files such as init.el. i really hate the default
;; behavior of the customization interface tacking stuff on the tail
;; end of init.el. setting a specific custom file name will have any
;; customizations logged into that file but i never load it. the file
;; could be in .gitignore but can be mined for changes to pull into
;; init.el.

(setopt custom-file (concat user-emacs-directory "custom.el"))


(provide 'init)
;;; init.el ends here
