(defun my-pascal-helper ()
  (interactive)
  (setq compilation-error-regexp-alist-alist
        (cons '(pascal "^\\(.*\\)(\\([0-9]+\\)+\\,\\([0-9]+\\)).*"
                       1 ;; file
                       2 ;; line
                       3 ;; column? 
                       )
              compilation-error-regexp-alist-alist))

  (setq compilation-error-regexp-alist
        (cons 'pascal compilation-error-regexp-alist)))


(add-hook 'pascal-mode-hook (lambda ()                         
                              (my-pascal-helper)
                              (flycheck-mode -1)))

(provide 'my-pascal-helper)
