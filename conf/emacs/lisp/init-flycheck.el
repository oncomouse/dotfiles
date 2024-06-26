;;; init-flycheck.el --- flycheck!
;;; Commentary:
;;; Code:

(use-package flycheck
  :straight t
  :hook (((prog-mode) . flycheck-mode)))

(use-package flycheck-eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode))

(use-package consult-flycheck
  :straight t
  :after consult)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
