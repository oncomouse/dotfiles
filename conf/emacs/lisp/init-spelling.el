;;; init-spelling.el --- Configure spellchecking
;;; Commentary:
;;; Code:

(defun ap/spell-fu-error (arg)
  "Move between spell-fu errors, ARG controls moving backward or forward."
  (interactive "p")
  (if (= arg 4)
      (spell-fu-goto-previous-error)
    (spell-fu-goto-next-error)))

(defun ap/configure-ispell (&optional code-spell)
  "Configure `ispell'. Set options for code when CODE-SPELL is non-nil."
  (interactive)
  (cond
   ((and (not code-spell) (executable-find "hunspell"))
    (setq-local ispell-program-name "hunspell")
    (setq-local ispell-local-dictionary "en_US")
    (setq-local ispell-local-dictionary-alist
                ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
                ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
                '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
    ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
    ;; If it's nil, Emacs tries to automatically set up the dictionaries.
    (when (boundp 'ispell-hunspell-dictionary-alist)
      (setq-local ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))
   ((executable-find "aspell")
    (setq-local ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq-local ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
    (when code-spell
      (setq-local ispell-extra-args '("--camel-case" "--run-together" "--run-together-limit=16"))))
   (t (setq-local ispell-program-name nil))))

(require 'ispell)

(use-package spell-fu
  :straight t
  :hook (((text-mode) . ap/configure-ispell)
         ((prog-mode) . (lambda () (ap/configure-ispell t))))
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
