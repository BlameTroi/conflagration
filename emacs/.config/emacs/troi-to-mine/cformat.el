;;; cformat.el --- formatting, code alignment  -*- lexical-binding: t -*-

;;; Commentary:

;; merely indenting code is not sufficient, and the current state of
;; treesitter user configuration for the c-ts modes is sadly lacking.
;; as i got used to always formatting on save when working with go,
;; let's try to use reformatter to drive astyle for c, and add other
;; languages if i ever feel a need.
;;
;; the astyle reformat package uses customization, but i was able to
;; pull out the settings and put them properly in use-package.

;; i tried rebox2 for comment blocks but it's a bit touchy and seems
;; to have a problem if you mix comment openers in c (/* vs //). i've
;; left the config in but commented out for now.

;;; Code:


(use-package reformatter)

;; i've set up a .astylerc file and am running manually for a bit,
;; but leaving this in place but disabled the on save.

(use-package astyle
        :after reformatter
        :when (executable-find "astyle")
        :diminish (astyle-on-save-mode . "as")
        )
;; (c-ts-mode . astyle-on-save-mode)
;; (c++-ts-mode . astyle-on-save-mode)
;; :custom
;; (astyle-style "knf")              ;; or linux
;; (astyle-indent "tab")             ;; might as well go whole hog
;; (astyle-custom-args '(
;;                              "-xn"          ;; --attach-namespaces
;;                              "-xc"          ;; --attach-classes
;;                              "-xk"          ;; --attach-extern-c
;;                              "-xV"          ;; --attach-closing-while     } while ();
;;                              "-H"           ;; --pad-header               if ()
;;                              "-U"           ;; --unpad-paren
;;                              "-xB"          ;; --break-return-type
;;                              "-xD"          ;; --break-return-type-decl
;;                              )))
;; (astyle-style "attach")              ;; --style=attach
;; (astyle-indent 3)                    ;; -s3
;; (astyle-custom-args '(
;;                       "-xn"          ;; --attach-namespaces
;;                       "-xc"          ;; --attach-classes
;;                       "-xk"          ;; --attach-extern-c
;;                       "-xV"          ;; --attach-closing-while     } while ();
;;                       "-H"           ;; --pad-header               if ()
;;                       "-U"           ;; --unpad-paren
;;                       "-j"           ;; --add-braces
;;                       "-xB"          ;; --break-return-type
;;                       "-xD"          ;; --break-return-type-decl
;;                       "-xg"          ;; --pad-comma
;;                       ;;"-p"           ;; --pad-oper  (implies -xg --pad-comma)
;;                       ))


;; rebox2 kept for future reference if i want to try to get it working.
;; (use-package rebox2
;;   :defer t
;;   :diminish
;;   :hook
;;   (emacs-lisp-mode . (lambda ()
;;                        (set (make-local-variable 'rebox-style-loop) '(27 17 21))
;;                        (set (make-local-variable 'rebox-min-fill-column) 40)
;;                        (rebox-mode 1)))
;;   (c-ts-mode . (lambda ()
;;                  (setq comment-start "/* "
;;                        comment-end " */")
;;                  (set (make-local-variable 'rebox-style-loop) '(11 15 35 21 41))
;;                  (set (make-local-variable 'rebox-min-fill-column) 60)
;;                  (rebox-mode 1)))
;;   )


(provide 'cformat)
;;; cformat.el ends here
