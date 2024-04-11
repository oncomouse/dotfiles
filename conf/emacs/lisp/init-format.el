(defvar +format-functions
  '()
  "A list of functions to run when formatting a buffer or region.
Each function is given three arguments: the starting point, end point, and a
symbol indicating the type of operation being requested (as a symbol: either
`region' or `buffer').

The first function to return non-nil will abort all functions after it,
including Apheleia itself.")

(defun +format--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (current-indentation)))

(defun +format-region (start end &optional callback)
  "Format from START to END with `apheleia'."
  (when-let* ((command (apheleia--get-formatters
                        (if current-prefix-arg
                            'prompt
                          'interactive)))
              (cur-buffer (current-buffer))
              (formatted-buffer (get-buffer-create " *apheleia-formatted*"))
              (indent 0))
    (with-current-buffer formatted-buffer
      (erase-buffer)
      (unless (featurep :system 'windows)
        (setq-local coding-system-for-read 'utf-8)
        (setq-local coding-system-for-write 'utf-8))
      ;; Ensure this temp buffer seems as much like the origin buffer as
      ;; possible, in case the formatter is an elisp function, like `gofmt'.
      (cl-loop for (var . val)
               in (cl-remove-if-not #'listp (buffer-local-variables cur-buffer))
               ;; Making enable-multibyte-characters buffer-local causes an
               ;; error.
               unless (eq var 'enable-multibyte-characters)
               ;; Using setq-local would quote var.
               do (set (make-local-variable var) val))
      ;;
      (insert-buffer-substring-no-properties cur-buffer start end)
      ;; Since we're piping a region of text to the formatter, remove any
      ;; leading indentation to make it look like a file.
      (setq indent (+format--current-indentation))
      (when (> indent 0)
        (indent-rigidly (point-min) (point-max) (- indent)))
      ;;
      (apheleia-format-buffer
       command
       (lambda ()
         (with-current-buffer formatted-buffer
           (when (> indent 0)
             ;; restore indentation without affecting new
             ;; indentation
             (indent-rigidly (point-min) (point-max)
                             (max 0 (- indent (+format--current-indentation)))))
           (set-buffer-modified-p nil))
         (with-current-buffer cur-buffer
           (delete-region start end)
           (insert-buffer-substring-no-properties formatted-buffer)
           (when callback (funcall callback))
           (kill-buffer formatted-buffer)))))))

;;
;;; Commands

(defun +format/buffer (&optional arg)
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive "P")
  (or (run-hook-with-args-until-success '+format-functions (point-min) (point-max) 'buffer)
      (call-interactively #'apheleia-format-buffer)))

(defun +format/region (beg end &optional arg)
  "Runs the active formatter on the lines within BEG and END.

WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (or (run-hook-with-args-until-success '+format-functions beg end 'region)
      (+format-region beg end)))

(defun +format/region-or-buffer ()
  "Runs the active formatter on the selected region (or whole buffer, if nothing
is selected)."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'+format/region
     #'+format/buffer)))

(use-package apheleia
  :straight t
  :bind (("C-c f" . +format/region-or-buffer))
  :diminish apheleia-mode)

(provide 'init-format)
;; init-format.el ends here
