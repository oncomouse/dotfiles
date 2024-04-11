;;; init-flycheck.el --- flycheck!
;;; Commentary:
;;; Code:

(use-package flycheck
  :straight t
  :hook (((prog-mode) . flycheck-mode))
  :config
  (flycheck-define-checker lua-selene
    "A lua syntax checker using selene"
    :command ("selene" "--display-style" "quiet" source)
    :enable t
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": warning" (message) line-end)
     (error line-start (file-name) ":" line ":" column ": error" (message) line-end))
    :modes (lua-mode lua-ts-mode))
  (push 'lua-selene flycheck-checkers))

(use-package flycheck-eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode))

(use-package consult-flycheck
  :straight t
  :after consult)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
