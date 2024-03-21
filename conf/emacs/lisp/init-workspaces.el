;;; init-workspaces.el --- Workspace management using bufler and tab-bar
;;; Commentary:
;;; Code:
;; use this next line only if you also use straight, otherwise ignore it.
;;   :straight (:type git :host github :repo "mclear-tools/tabspaces")
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   (tabspaces-initialize-project-with-todo t)
;;   (tabspaces-todo-file-name "project-todo.org")
;;   ;; sessions
;;   (tabspaces-session t)
;;   (tabspaces-session-auto-restore t)
;;   (tabspaces-keymap-prefix "C-c w")
;;   :config
;;   (defvar tabspaces-command-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "C") 'tabspaces-clear-buffers)
;;       (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
;;       (define-key map (kbd "d") 'tabspaces-close-workspace)
;;       (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
;;       (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
;;       (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
;;       (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
;;       (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
;;       (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
;;       map)
;;     "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'."))

(provide 'init-workspaces)
;;; init-workspaces.el ends here
