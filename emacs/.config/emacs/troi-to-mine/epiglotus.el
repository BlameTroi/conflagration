;;; epiglotus.el --- eglot and friends   -*- lexical-binding: t -*-

;;; Commentary:

;; eglot is to winning me over as it's easier to manage for the places
;; i use it. it's not as polished as lsp-mode, but it's smaller. i
;; think treesitter actually handles some of what lsp-mode gets used
;; for better anyway.

;; language server program configuration goes here even though you
;; want to think of that in the context of the specific language
;; settings elsewhere throughout this init.

;; flymake lags flycheck in many areas, but its integration with eglot
;; makes it an obvious choice since i don't need the languages and
;; features suppported by flycheck.

;;; Code:


;; i'm lean toward minimalism, but i'm not a luddite. probably the
;; thing i do here that might be considered odd is turning off all
;; lsp formatting. not all servers do it, the configuration would
;; likely be a nightmare to manage (see clangd and clang-format),
;; and in some cases they don't over styles i use or find tolerable.

(use-package eglot
  :pin gnu
  ;; TODO eglot-ensure for all prog-modes considered bad, be specific.
  :hook
  (c-mode . eglot-ensure) (c++-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure) (c++-ts-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c o" . eglot-code-actions-organize-imports)
              ("C-c c r" . eglot-rename))
  :custom
  ;; log size 0 disables logging
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider)))

;; configure clangd for eglot to my preferences. i was able to avoid
;; the maze of (apparently) cmake generate files for clangd with these
;; options.

;; fortran defaults to fortls and its defaults work fine.

(with-eval-after-load 'eglot
  (setopt completion-category-defaults nil)
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "-j=4"
                    "--log=error"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0"))))


(use-package flymake
  :pin gnu
  :hook (prog-mode . flymake-mode)
  :custom (flymake-mode-line-lighter "FM")
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics)))


(provide 'epiglotus)
;;; epiglotus.el ends here
