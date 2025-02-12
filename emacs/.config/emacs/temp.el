(eval-when-compile
  (require 'use-package))
(setopt load-prefer-newer t)
(setopt use-package-always-ensure t)
(setopt package-native-compile t)
(setq native-comp-jit-compilation t)
...
(use-package compile-angel
  :ensure t
  :demand t
  ;;:custom
  ;;  (compile-angel-verbose nil)
  :config
  (setq compile-angel-excluded-files-regexps '("/cus-load\\.el$"
                                            "/theme-loaddefs\\.el$"
                                            "/loaddefs\\.el\\.gz$"
                                            "/charprop\\.el$"
                                            "/cl-loaddefs\\.el\\.gz$"
					    "/opt/.*"
					    "/opt/"
					    "custom.el$"
					    "savehist.el$"
					    "recentf-save.el$"))
  ;; (setq compile-angel-predicate-function
  ;; 	(lambda (file)
  ;;         (not (file-in-directory-p file "/opt/.*"))))
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))
