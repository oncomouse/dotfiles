;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;; Purcell configuration + customizations
;;; Code:

(use-package projectile
  :straight t
  :hook (after-init . projectile-mode)
  :init
  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; Patch for fish and ripgrep
  (setq projectile-globally-ignored-directories
        '(".idea"
          ".vscode"
          ".ensime_cache"
          ".eunit"
          ".git"
          ".hg"
          ".fslckout"
          "_FOSSIL_"
          ".bzr"
          "_darcs"
          ".tox"
          ".svn"
          ".stack-work"
          ".ccls-cache"
          ".cache"
          ".clangd"))

  ;; Override search menu:
  (define-key projectile-command-map (kbd "s") (cond
                                                ((executable-find "rg") 'projectile-ripgrep)
                                                ((executable-find "ag") 'projectile-ag)
                                                ((executable-find "grep") 'projectile-grep)
                                                (t 'projectile-find-references)))

  (defun ap/projectile-smart-switch ()
    "Open latest edited buffer when switched the  exist project, find files when switched to a new project."
    (if
        (null (projectile-project-buffer-files))
        (projectile-find-file)
      (call-interactively #'projectile-switch-to-buffer)))
  (setq projectile-switch-project-action #'ap/projectile-smart-switch)

  ;; Configure marginalia for projectile
  (with-eval-after-load 'marginalia
    (setq marginalia-command-categories
          (append '((projectile-find-file . project-file)
                    (projectile-find-dir . project-file)
                    (projectile-switch-project . file)
                    (projectile-recentf . project-file)
                    (projectile-switch-to-buffer . buffer)
                    (projectile-find-file . project-file)
                    (projectile-recentf . project-file)
                    (projectile-switch-to-buffer . buffer)
                    (projectile-switch-project . project-file))
                  marginalia-command-categories))))

(use-package ibuffer-projectile
  :straight t
  :after projectile
  :config
  (advice-add 'ibuffer-set-up-preferred-filters :override (lambda (&rest _)
                                                            (ibuffer-projectile-set-filter-groups)
                                                            (unless (eq ibuffer-sorting-mode 'alphabetic)
                                                              (ibuffer-do-sort-by-alphabetic)))))
(with-eval-after-load 'consult
  (defun projectile-consult-ripgrep (&optional initial)
    "Search current project (or prompt user for a project), with INITIAL as the starting prompt."
    (interactive)
    (consult-ripgrep (projectile-acquire-root) initial))
  (consult-customize projectile-consult-ripgrep :preview-key "M-P")
  (define-key global-map [remap projectile-ripgrep] 'projectile-consult-ripgrep))

(provide 'init-projectile)
;;; init-projectile.el ends here
