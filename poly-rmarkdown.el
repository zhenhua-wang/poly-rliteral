;;; poly-rmarkdown.el --- A simplified verision of poly-R -*- lexical-binding: t -*-
;;
;; Author: Zhenhua Wang
;;
;;; Code:

(require 'polymode)
(require 'ess-mode)
(require 'ess-r-mode nil t)

;; poly-rmarkdown-mode
(define-hostmode poly-rmarkdown-hostmode
  :mode 'gfm-mode)

(defun pm--rmarkdown-tail-matcher (ahead)
  (when (< ahead 0)
    (error "Backwards tail match not implemented"))
  ;; (beg, end + nextline)
  (when (re-search-forward "^[ \t]*\\(```\\)[ \t]*$")
    (cons (match-beginning 0) (+ 1 (match-end 0)))))

(define-innermode poly-rmarkdown-innermode
  :mode 'ess-r-mode
  :head-matcher (cons "^[ \t]*\\(```{?[rR].*\n\\)" 1)
  :tail-matcher 'pm--rmarkdown-tail-matcher
  :head-mode 'gfm-mode
  :tail-mode 'gfm-mode
  :adjust-face 'markdown-code-face
  :head-adjust-face 'markdown-code-face
  :tail-adjust-face 'markdown-code-face)

(define-polymode poly-rmarkdown-mode
  :hostmode 'poly-rmarkdown-hostmode
  :innermodes '(poly-rmarkdown-innermode))

;; poly-rnw-mode
(define-hostmode poly-rnw-hostmode nil
  :mode 'latex-mode
  :protect-font-lock t
  :protect-syntax t
  :protect-indent nil)

(define-innermode poly-rnw-innermode nil
  :mode 'ess-r-mode
  :head-matcher (cons "^[ \t]*\\(<<\\(.*\\)>>=.*\n\\)" 1)
  :tail-matcher (cons "^[ \t]*\\(@.*\\)$" 1))

(define-polymode poly-rnw-mode
  :hostmode 'poly-rnw-hostmode
  :innermodes '(poly-rnw-innermode))

;; export
(defvar poly-r-export-buffer "*poly-r-export*")
(add-to-list 'display-buffer-alist
             `(,poly-r-export-buffer
               (display-buffer-in-side-window)
               (window-height . 0.2)
               (side . top)
               (slot . 1)))

(defun poly-r-output-path ()
  (with-current-buffer poly-r-export-buffer
    (goto-char (point-min))
    (when (or (re-search-forward "Output created: +\\(.*\\)" nil t)
              (re-search-forward "Output file: +\\(.*\\)" nil t))
      (expand-file-name (match-string 1)))))

(defun poly-async-callback (process signal)
  (when (memq (process-status process) '(exit signal))
    (with-selected-window (get-buffer-window poly-r-export-command-buffer)
      (find-file (poly-r-output-path)))
    (shell-command-sentinel process signal)))

(defvar poly-r-export-command-buffer nil)
(defun poly-r-export (shell-command)
  (let ((output-buffer (get-buffer-create poly-r-export-buffer)))
    (setq poly-r-export-command-buffer (current-buffer))
    (async-shell-command shell-command output-buffer)
    (let ((proc (get-buffer-process output-buffer)))
      (set-process-sentinel proc #'poly-async-callback))))

(defun poly-rmd-knit ()
  "Knit Rmarkdown file."
  (interactive)
  (if (and (boundp poly-rmarkdown-mode) poly-rmarkdown-mode)
      (poly-r-export
       (format "R -e \"rmarkdown::render(\'%s\')\"" (buffer-file-name)))
    (message "Knit outside of Rmarkdown file is not supported")))
(define-key poly-rmarkdown-mode-map (kbd "C-c C-e") #'poly-rmd-knit)

(defun poly-rnw-sweave ()
  "Sweave Rnw file."
  (interactive)
  (if (and (boundp poly-rnw-mode) poly-rnw-mode)
      (poly-r-export
       (format "R CMD Sweave --pdf %s" (buffer-file-name)))
    (message "Sweave outside of Rnw file is not supported")))
(define-key poly-rnw-mode-map (kbd "C-c C-e") #'poly-rnw-sweave)

;; alias
(add-to-list 'polymode-mode-abbrev-aliases '("ess-r" . "R"))

;; R eval inside code block
(advice-add 'ess-eval-paragraph :around 'pm-execute-narrowed-to-span)
(advice-add 'ess-eval-buffer :around 'pm-execute-narrowed-to-span)
(advice-add 'ess-beginning-of-function :around 'pm-execute-narrowed-to-span)

;; polymode eval region function
(defun poly-rmarkdwon-eval-region (beg end msg)
  (let ((ess-inject-source t))
    (ess-eval-region beg end nil msg)))

(defun poly-rmarkdwon-mode-setup ()
  (when (equal ess-dialect "R")
    (setq-local polymode-eval-region-function #'poly-rmarkdwon-eval-region)))

(add-hook 'ess-mode-hook #'poly-rmarkdwon-mode-setup)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-rmarkdown-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . poly-rnw-mode))


(provide 'poly-rmarkdown)
;;; poly-R.el ends here
