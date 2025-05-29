;;; poly-rliteral.el --- Literal programming with R -*- lexical-binding: t -*-
;;
;; Author: Zhenhua Wang
;;
;;; Code:

(require 'polymode)
(require 'ess-mode)
(require 'ess-r-mode nil t)

;; poly-rliteral-rmd-mode
(define-hostmode poly-rliteral-rmd-hostmode
  :mode 'gfm-mode)

(defun poly-rliteral-rmd--tail-matcher (ahead)
  (when (< ahead 0)
    (error "Backwards tail match not implemented"))
  ;; (beg, end + nextline)
  (when (re-search-forward "^[ \t]*\\(```\\)[ \t]*$")
    (cons (match-beginning 0) (+ 1 (match-end 0)))))

(define-innermode poly-rliteral-rmd-innermode
  :mode 'ess-r-mode
  :head-matcher (cons "^[ \t]*\\(```{?[rR].*\n\\)" 1)
  :tail-matcher 'poly-rliteral-rmd--tail-matcher
  :head-mode 'gfm-mode
  :tail-mode 'gfm-mode
  :adjust-face 'markdown-code-face
  :head-adjust-face 'markdown-code-face
  :tail-adjust-face 'markdown-code-face)

(define-polymode poly-rliteral-rmd-mode
  :hostmode 'poly-rliteral-rmd-hostmode
  :innermodes '(poly-rliteral-rmd-innermode))

;; poly-rliteral-rnw-mode
(define-hostmode poly-rliteral-rnw-hostmode nil
  :mode 'LaTeX-mode
  :protect-font-lock t
  :protect-syntax t
  :protect-indent nil)

(defun poly-rliteral-rnw--tail-matcher (ahead)
  (when (< ahead 0)
    (error "Backwards tail match not implemented"))
  ;; (beg, end + nextline)
  (when (re-search-forward "^[ \t]*\\(@.*\\)$")
    (cons (match-beginning 0) (+ 1 (match-end 0)))))

(define-innermode poly-rliteral-rnw-innermode nil
  :mode 'ess-r-mode
  :head-matcher (cons "^[ \t]*\\(<<\\(.*\\)>>=.*\n\\)" 1)
  :tail-matcher 'poly-rliteral-rnw--tail-matcher
  :head-mode 'LaTeX-mode
  :tail-mode 'LaTeX-mode
  :adjust-face 'org-block
  :head-adjust-face 'org-block
  :tail-adjust-face 'org-block)

(define-polymode poly-rliteral-rnw-mode
  :hostmode 'poly-rliteral-rnw-hostmode
  :innermodes '(poly-rliteral-rnw-innermode))

;; export
(defvar poly-rliteral--export-buffer "*poly-r-export*")
(add-to-list 'display-buffer-alist
             `(,poly-rliteral--export-buffer
               (display-buffer-in-side-window)
               (window-height . 0.2)
               (side . top)
               (slot . 1)))

(defun poly-rliteral--export-path ()
  (with-current-buffer poly-rliteral--export-buffer
    (goto-char (point-min))
    (when (or (re-search-forward "Output created: +\\(.*\\)" nil t)
              (re-search-forward "Output file: +\\(.*\\)" nil t))
      (expand-file-name (match-string 1)))))

(defun poly-rliteral--async-callback-find-file (path)
  (display-buffer (find-file-noselect path)))

(defun poly-rliteral--async-callback (process signal)
  (when (memq (process-status process) '(exit signal))
    (poly-rliteral--async-callback-find-file
     (poly-rliteral--export-path))
    (shell-command-sentinel process signal)))

(defun poly-rliteral-export (shell-command)
  (let ((output-buffer (get-buffer-create poly-rliteral--export-buffer)))
    (async-shell-command shell-command output-buffer)
    (let ((proc (get-buffer-process output-buffer)))
      (set-process-sentinel proc #'poly-rliteral--async-callback))))

(defun poly-rliteral-rmd-knit ()
  "Knit Rmarkdown file."
  (interactive)
  (if (and (boundp poly-rliteral-rmd-mode) poly-rliteral-rmd-mode)
      (poly-rliteral-export
       (format "R -e \"rmarkdown::render(\'%s\')\"" (buffer-file-name)))
    (message "Knit outside of Rmarkdown file is not supported")))
(define-key poly-rliteral-rmd-mode-map (kbd "C-c C-e") #'poly-rliteral-rmd-knit)

(defun poly-rliteral-rnw-sweave ()
  "Sweave Rnw file."
  (interactive)
  (if (and (boundp poly-rliteral-rnw-mode) poly-rliteral-rnw-mode)
      (poly-rliteral-export
       (format "R CMD Sweave --pdf %s" (buffer-file-name)))
    (message "Sweave outside of Rnw file is not supported")))
(define-key poly-rliteral-rnw-mode-map (kbd "C-c C-e") #'poly-rliteral-rnw-sweave)

;; alias
(add-to-list 'polymode-mode-abbrev-aliases '("ess-r" . "R"))

;; R eval inside code block
(advice-add 'ess-eval-paragraph :around 'pm-execute-narrowed-to-span)
(advice-add 'ess-eval-buffer :around 'pm-execute-narrowed-to-span)
(advice-add 'ess-beginning-of-function :around 'pm-execute-narrowed-to-span)

;; polymode eval region function
(defun poly-rliteral-eval-region (beg end msg)
  (let ((ess-inject-source t))
    (ess-eval-region beg end nil msg)))

(defun poly-rliteral-setup ()
  (when (equal ess-dialect "R")
    (setq-local polymode-eval-region-function #'poly-rliteral-eval-region)))

(add-hook 'ess-mode-hook #'poly-rliteral-setup)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-rliteral-rmd-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . poly-rliteral-rnw-mode))


(provide 'poly-rliteral)
;;; poly-R.el ends here
