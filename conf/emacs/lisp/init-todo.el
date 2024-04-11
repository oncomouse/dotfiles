;;; init-todo.el --- hl-todo and supporting packages
;;; Commentary:
;;; Code:

(use-package hl-todo
  :straight t
  :hook ((after-init . global-hl-todo-mode))
  :bind (("C-c t f" . consult-todo)
         ("C-c t a" . consult-todo-all)
         ("C-c t n" . hl-todo-next)
         ("C-c t p" . hl-todo-previous)))

(use-package flycheck-hl-todo
  :ensure t
  :defer 5 ; Need to be initialized after the rest of checkers
  :straight (:host github :repo "alvarogonzalezsotillo/flycheck-hl-todo")
  :config
  (flycheck-hl-todo-setup))

(use-package consult-todo
  :straight t
  :after consult)

(provide 'init-todo)
;;; init-todo.el ends here
