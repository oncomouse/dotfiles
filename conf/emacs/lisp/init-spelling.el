;;; init-spelling.el --- Configure spellchecking
;;; Commentary:
;;; Code:

(defun ap/spell-fu-error (arg)
  "Move between spell-fu errors, ARG controls moving backward or forward."
  (interactive "p")
  (if (= arg 4)
      (spell-fu-goto-previous-error)
    (spell-fu-goto-next-error)))

(require 'ispell)

(use-package spell-fu
  :straight t
  :init
  (setq spell-fu-word-delimit-camel-case t)
  (setq spell-fu-global-ignore-buffer (lambda (buf) (buffer-local-value 'buffer-read-only buf)))
  (defvar +spell-excluded-faces-alist
    '((markdown-mode
       . (markdown-code-face
          markdown-html-attr-name-face
          markdown-html-attr-value-face
          markdown-html-tag-name-face
          markdown-inline-code-face
          markdown-link-face
          markdown-markup-face
          markdown-plain-url-face
          markdown-reference-face
          markdown-url-face))
      (org-mode
       . (org-block
          org-block-begin-line
          org-block-end-line
          org-cite
          org-cite-key
          org-code
          org-date
          org-footnote
          org-formula
          org-inline-src-block
          org-latex-and-related
          org-link
          org-meta-line
          org-property-value
          org-ref-cite-face
          org-special-keyword
          org-tag
          org-todo
          org-todo-keyword-done
          org-todo-keyword-habt
          org-todo-keyword-kill
          org-todo-keyword-outd
          org-todo-keyword-todo
          org-todo-keyword-wait
          org-verbatim))
      (latex-mode
       . (font-latex-math-face
          font-latex-sedate-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-variable-name-face)))
    "Faces in certain major modes that spell-fu will not spellcheck.")
  :config
  (defun +spell-init-excluded-faces-h ()
    "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
    (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
      (setq-local spell-fu-faces-exclude excluded)))
  (add-hook 'spell-fu-mode-hook '+spell-init-excluded-faces-h)
  (spell-fu-global-mode)
  (define-key global-map (kbd "C-,") 'ap/spell-fu-error))

(provide 'init-spelling)
;;; init-spelling.el ends here
