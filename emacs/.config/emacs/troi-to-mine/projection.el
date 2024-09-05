;;; projection.el --- project mismanagement -*- lexical-binding: t -*-

;;; Commentary:

;; my use is simple enough that i don't need full blown projectile. i
;; do set some non vc project roots. fpm adds a .git repo by default
;; but i tend to remove that and put .git higher up as i use fpm for
;; sub projects.

;;; Code:


(use-package project
  :pin gnu
  :custom
  (project-vc-extra-root-markers '(".projectile" ".project.el" "fpm.toml")))


(provide 'projection)
;;; projection.el ends here
