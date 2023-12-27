;;; troi-init-fortran.el --- Fortran and F90 mode settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: local, languages, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Just setting up Fortran and F90 to my preferences.

;;; Code:


;; Fortran settings
(setq-default fortran-continuation-string "&")
(setq-default fortran-do-indent 4)
(setq-default fortran-if-indent 4)
(setq-default fortran-structure-indent 4)


;; Fortran 90 settings
(setq-default f90-do-indent 4)
(setq-default f90-if-indent 4)
(setq-default f90-type-indent 4)
(setq-default f90-program-indent 4)
(setq-default f90-continuation-indent 6)
(setq-default f90-smart-end 'blink)


;; Some F90 specific changes, including a swap of Return and C-j
;; in Fortran 90 mode. Return will also indent, C-j does not. I'm
;; not sure i'll like this, but we'll see how it goes.
(add-hook 'f90-mode-hook
          (lambda ()
            (define-key f90-mode-map [return] 'f90-indent-new-line)
            (define-key f90-mode-map "\C-j" 'newline)
            (setq fill-column 100)
            (abbrev-mode)))

;; Read in handy abbreviations for Fortran
(require 'init-abbrev-f90)


(provide 'init-f90)
;;; troi-init-fortran.el ends here
