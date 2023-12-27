;;; idcase.el -- Minor mode id-case-significant. -*- lexical-binding: t -*-
;; Copyright (c) 1991 Inge Frick (I would not mind moving the copyright to FSF)
;; Address: i...@nada.kth.se
;; Version: 1.00

;; This file is not part of GNU Emacs (yet).
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  Neither the author nor any distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; id-case-significant is a minor mode that is used by pascal mode but it
;; could also be used by other language modes.

(defvar id-case-significant nil
  "If nil, map all words to lower case. Command id-case-significant toggles.
If T, case in keywords and identifiers matter.")

(make-variable-buffer-local 'id-case-significant)

(if (not (assq 'id-case-significant minor-mode-alist))
    (setq minor-mode-alist
         (cons '(id-case-significant " Id-Case") minor-mode-alist)))


(defvar id-case-change-hook nil
  "Functions to call if id-case-significant is changed")

(defun id-case-significant (&optional sw force)
  "Toggle status of id-case-significant"
  (interactive "P")
  (let ((old id-case-significant))
    (setq id-case-significant
         (if sw (> (prefix-numeric-value sw) 0)
           (not id-case-significant)))
    (if (or force (if id-case-significant (not old) old))
       (run-hooks 'id-case-change-hook))))

(defun id-case (string)
  "Return downcase(STRING) if id-case-significant is false else return STRING."
  (if id-case-significant string (downcase string)))

(provide 'idcase)
