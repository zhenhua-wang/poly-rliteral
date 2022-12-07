;;; poly-rmarkdown.el --- A simplified verision of poly-R -*- lexical-binding: t -*-
;;
;; Author: Zhenhua Wang
;;
;;; Code:

(require 'polymode)
(require 'ess-mode)
(require 'ess-r-mode nil t)

;; hostmode
(define-hostmode poly-rmarkdown-hostmode
  :mode 'gfm-mode)

;; minor mode
(define-innermode poly-rmarkdown-innermode
  :mode 'ess-r-mode
  :head-matcher (cons "^[ \t]*\\(```{?[rR].*\n\\)" 1)
  :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
  :head-mode 'host
  :tail-mode 'host)

;; polymode
(define-polymode poly-rmarkdown-mode
  :hostmode 'poly-rmarkdown-hostmode
  :innermodes '(poly-rmarkdown-innermode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-rmarkdown-mode))

(provide 'poly-rmarkdown)
;;; poly-R.el ends here
