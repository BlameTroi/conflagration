;;; screening.el --- el screen  -*- lexical-binding: t -*-

;;; Commentary:

;; i don't run client server, but sometimes different layouts might make sense.

;;; Code:

(use-package elscreen
  :defer t
  :custom
  (elscreen-tab-display-control nil))

;; TODO not using at the moment so disabling the elscreen-start
;;      until i have more time to learn it.

;; (elscreen-start)

(provide 'screening)
;;; screening.el ends here
