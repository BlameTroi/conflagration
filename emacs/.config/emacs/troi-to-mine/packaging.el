;;; packaging.el --- use package, elpa, and such -*- lexical-binding: t -*-

;;; Commentary:

;; use-package is now part of standard emacs. it can be updated from
;; gnu elpa, as can several other built in packages.

;; sadly that's a manual check against installed/built-ins from
;; list-packages after your first load.

;;; Code:


;; while use-package is a built in, you have to require it for some of
;; the macro keywords to process.

(eval-when-compile
  (require 'use-package))
(setopt load-prefer-newer t)
(setopt use-package-always-ensure t)


;; if native compilation is available, go ahead and use it. my current
;; (29.3) mac build does not have native compile built in.

(if (and (fboundp 'native-compile-available-p)
         (native-compile-available-p))
    (setopt package-native-compile t))


;; gnu and nongnu elpa repositories are available by default. add
;; melpa-stable and melpa but prioritize them below gnu and nongnu.
;; it is also probably a good idea to pin some packages to specific
;; repositories--suspenders and belt.

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-archive-priorities
        '(("gnu" . 10)
          ("nongnu" . 9)
          ("melpa-stable" . 8)
          ("melpa" . 5))))


;; this is a good place to make sure these are done.

(use-package diminish
  :demand t)

(use-package bind-key
  :demand t)


(provide 'packaging)
;; packaging.el ends here
