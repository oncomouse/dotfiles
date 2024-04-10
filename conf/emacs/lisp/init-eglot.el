;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package eglot
  :straight t)

(use-package consult-eglot
  :straight t
  :after (consult eglot))

(use-package flycheck-eglot
  :straight t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'init-eglot)
;;; init-eglot.el ends here
