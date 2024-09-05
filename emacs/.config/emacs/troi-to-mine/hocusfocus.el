;;; hocusfocus.el --- hocus pocus bring it into focus   -*- lexical-binding: t -*-

;;; Commentary:

;; this is from https://speechcode.com/blog/narrow-to-focus/ by arthur
;; a. gleckler.

;;; Code:


(defun troi/narrow-to-focus (start end)
  "If the region is active, narrow to region, and mark it.
If the mark is not active, narrow to the region that was
the most recent focus. START and END define the active
region."
  (interactive "r")
  (cond ((use-region-p)
         (remove-overlays (point-min) (point-max) 'focus t)
         (let ((overlay (make-overlay start end)))
           (overlay-put overlay 'focus t)
           (narrow-to-region start end)))
        (t (let ((focus
                  (seq-find (lambda (o) (overlay-get o 'focus))
                            (overlays-in (point-min) (point-max)))))
             (when focus
               (narrow-to-region (overlay-start focus)
                                 (overlay-end focus)))))))

(define-key global-map "\C-xnf" 'troi/narrow-to-focus)


(provide 'hocusfocus)
;;; hocusfocus.el ends here
