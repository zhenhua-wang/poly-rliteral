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
  :tail-mode 'host
  :adjust-face 'markdown-code-face
  :head-adjust-face nil
  :tail-adjust-face nil)

;; polymode
(define-polymode poly-rmarkdown-mode
  :hostmode 'poly-rmarkdown-hostmode
  :innermodes '(poly-rmarkdown-innermode))

;; export
(defun pm--rmarkdown-output-file-sniffer ()
  (goto-char (point-min))
  (let (files)
    (while (re-search-forward "Output created: +\\(.*\\)" nil t)
      (push (expand-file-name (match-string 1)) files))
    (reverse (delete-dups files))))

(defun pm--rmarkdown-output-file-from-.Last.value ()
  (ess-get-words-from-vector "print(.Last.value)\n"))

(defun pm--rmarkdown-shell-auto-selector (action &rest _ignore)
  (cl-case action
    (doc "AUTO DETECT")
    (command "Rscript -e \"rmarkdown::render('%i', output_format = 'all')\"")
    (output-file #'pm--rmarkdown-output-file-sniffer)))

(defun pm--rmarkdown-shell-default-selector (action &rest _ignore)
  (cl-case action
    (doc "DEFAULT")
    (command "Rscript -e \"rmarkdown::render('%i', output_format = NULL)\"")
    (output-file #'pm--rmarkdown-output-file-sniffer)))

(defun pm--rmarkdown-callback-auto-selector (action &rest _ignore)
  (cl-case action
    (doc "AUTO DETECT")
    ;; last file is not auto-detected unless we cat new line
    (command "rmarkdown::render('%I', output_format = 'all', knit_root_dir=getwd())")
    (output-file #'pm--rmarkdown-output-file-from-.Last.value)))

(defun pm--rmarkdown-callback-default-selector (action &rest _ignore)
  (cl-case action
    (doc "DEFAULT")
    ;; last file is not auto-detected unless we cat new line
    (command "rmarkdown::render('%I', output_format = NULL, knit_root_dir=getwd())")
    (output-file #'pm--rmarkdown-output-file-from-.Last.value)))

(defcustom poly-rmarkdwon-exporter
  (pm-shell-exporter :name "Rmarkdown"
                     :from
                     '(("Rmarkdown"  "\\.[rR]?md\\|rapport\\'" "R Markdown"
                        "Rscript -e \"rmarkdown::render('%i', output_format = '%t', output_file = '%o')\""))
                     :to
                     '(("auto" . pm--rmarkdown-shell-auto-selector)
                       ("default" . pm--rmarkdown-shell-default-selector)
                       ("html" "html" "html document" "html_document")
                       ("pdf" "pdf" "pdf document" "pdf_document")
                       ("word" "docx" "word document" "word_document")
                       ("md" "md" "md document" "md_document")
                       ("ioslides" "html" "ioslides presentation" "ioslides_presentation")
                       ("slidy" "html" "slidy presentation" "slidy_presentation")
                       ("beamer" "pdf" "beamer presentation" "beamer_presentation")))
  "R Markdown exporter.
  Please not that with 'AUTO DETECT' export options, output file
  names are inferred by Rmarkdown from YAML description
  block. Thus, output file names don't comply with
  `polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(defcustom poly-rmarkdwon-ess-exporter
  (pm-callback-exporter :name "Rmarkdown-ESS"
                        :from
                        '(("Rmarkdown" "\\.[rR]?md\\|rapport\\'" "R Markdown"
                           "rmarkdown::render('%I', output_format = '%t', output_file = '%O', knit_root_dir=getwd())\n"))
                        :to
                        '(("auto" . pm--rmarkdown-callback-auto-selector)
                          ("html" "html" "html document" "html_document")
                          ("pdf" "pdf" "pdf document" "pdf_document")
                          ("default" . pm--rmarkdown-callback-default-selector)
                          ("word" "docx" "word document" "word_document")
                          ("md" "md" "md document" "md_document")
                          ("ioslides" "html" "ioslides presentation" "ioslides_presentation")
                          ("slidy" "html" "slidy presentation" "slidy_presentation")
                          ("beamer" "pdf" "beamer presentation" "beamer_presentation"))
                        :function 'pm--ess-run-command
                        :callback 'pm--ess-callback)
  "R Markdown exporter.
  Please not that with 'AUTO DETECT' export options, output file
  names are inferred by Rmarkdown from YAML description
  block. Thus, output file names don't comply with
  `polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter poly-rmarkdwon-ess-exporter nil
                            poly-rmarkdown-polymode)
(polymode-register-exporter poly-rmarkdwon-exporter nil
                            poly-rmarkdown-polymode)

;; ESS command
(declare-function ess-async-command "ess-inf.el")
(declare-function ess-force-buffer-current "ess-inf.el")
(declare-function ess-process-get "ess-inf.el")
(declare-function ess-process-put "ess-inf.el")
(declare-function comint-previous-prompt "comint.el")

(defun pm--ess-callback (proc string)
  (let ((ofile (process-get proc :output-file)))
    ;; This is getting silly. Ess splits output for optimization reasons. So we
    ;; are collecting output from 3 places:
    ;;   - most recent STRING
    ;;   - string in accumulation buffer
    ;;   - string already in output buffer
    (with-current-buffer (if (fboundp 'ess--accumulation-buffer)
                             (ess--accumulation-buffer proc)
                           (process-get proc 'accum-buffer-name))
      (setq string (concat (buffer-string) string)))
    (with-current-buffer (process-buffer proc)
      (setq string (concat (buffer-substring (or ess--tb-last-input (comint-previous-prompt)) (point-max))
                           string)))
    (with-temp-buffer
      (setq ess-dialect "R"
            ess-local-process-name (process-name proc))
      (insert string)
      (when (string-match-p "Error\\(:\\| +in\\)" string)
        (user-error "Errors durring ESS async command"))
      (when (functionp ofile)
        (setq ofile (funcall ofile))))
    ofile))

(defun pm--ess-run-command (command callback &rest _ignore)
  (require 'ess)
  (let ((ess-eval-visibly t)
        (ess-dialect "R"))
    (ess-force-buffer-current)
    (with-current-buffer (ess-get-process-buffer)
      (unless (and (boundp 'goto-address-mode)
                   goto-address-mode)
        ;; mostly for shiny apps (added in ESS 19)
        (goto-address-mode 1)))
    (ess-process-put :output-file pm--output-file)
    (when callback
      (ess-process-put 'callbacks (list callback))
      (ess-process-put 'interruptable? t)
      (ess-process-put 'running-async? t))
    (ess-eval-linewise command)
    (display-buffer (ess-get-process-buffer))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-rmarkdown-mode))

(provide 'poly-rmarkdown)
;;; poly-R.el ends here
