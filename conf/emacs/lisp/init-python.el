;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; I use nix + direnv instead of virtualenv/pyenv/pyvenv, and it is an
;; approach which extends to other languages too. I recorded a
;; screencast about this: https://www.youtube.com/watch?v=TbIHRHy7_JM


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

;; (when (maybe-require-package 'flymake-ruff)
;;   (defun sanityinc/flymake-ruff-maybe-enable ()
;;     (when (executable-find "ruff")
;;       (flymake-ruff-load)))
;;   (add-hook 'python-mode-hook 'sanityinc/flymake-ruff-maybe-enable))

(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)

  ;; Use something adapted to your config to add `python-ruff' to `flycheck-checkers'
  ;; This is an MVP example:
  (setq python-mode-hook
        (list (defun my-python-hook ()
                (unless (bound-and-true-p org-src-mode)
                  (when (buffer-file-name)
                    (setq-local flycheck-checkers '(python-ruff))
                    (flycheck-mode)))))))

(maybe-require-package 'ruff-format)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(provide 'init-python)
;;; init-python.el ends here
