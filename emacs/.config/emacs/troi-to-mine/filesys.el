;;; filesys.el --- file system behavior -*- lexical-binding: t -*-

;;; Commentary:

;; backups, directories, and behavior related to files. TODO should
;; i have standard variables for common directories? org, documents,
;; projects?

;; i have an extension in my load path for local lisp that isn't
;; managed with use-package. i don't tend to put anything in it but
;; it's available.

;;; Code:


(add-to-list 'load-path
             (concat user-emacs-directory "troi-lisp"))


;; use gls if it's around. this could be gated to be used only on the
;; mac. the mac supplied ls doesn't suppport all the options dired
;; wants.

(when (executable-find "gls")
  (setopt insert-directory-program "gls"))


;; one last chance to un-delete a file.

(setopt delete-by-moving-to-trash t)


;; collect backups and such in one place

(defun troi/backup-file-name (fpath)
  "Return a new file path of FPATH, creating directories if needed."
  (let* ((backupRootDir "~/.tmp/emacs-backup/")
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir fpath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'troi/backup-file-name)


(provide 'filesys)
;; filesys.el ends here
