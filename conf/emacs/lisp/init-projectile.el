;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;; Purcell configuration + customizations
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))

(with-eval-after-load 'projectile
  ;; Use projectile for ibuffer sorting:
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

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

  ;; Combined project search instead of individual programs:
  (defun ap/project-search (search-term &optional arg)
    (interactive (list (projectile--read-search-string-with-default
                        (format "%s for" (if current-prefix-arg "Regexp search" "Search")))
                       current-prefix-arg))
    "Run a searching with SEARCH-TERM at current project root using best search program.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work."
    (cond
     ((executable-find "rg") (projectile-ripgrep search-term arg))
     ((executable-find "ag") (projectile-ag search-term arg))
     ((executable-find "grep") (projectile-grep search-term arg))
     (t (projectile-find-references search-term arg))))

  ;; Override search menu:
  (define-key projectile-command-map (kbd "s") 'ap/project-search)

  ;; Switch to a project buffer if one is open, otherwise run find files:
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

(provide 'init-projectile)
;;; init-projectile.el ends here
