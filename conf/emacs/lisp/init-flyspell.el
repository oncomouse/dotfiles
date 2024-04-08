;;; init-flyspell.el --- Configure flyspell
;;; Commentary:
;;; Code:

;; Adapted from: http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs/
(defun ap/configure-flyspell (&optional code-spell)
  "Configure `flyspell'. Set code when CODE-SPELL is non-nil."
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

(use-package flyspell
  :straight t
  :hook (
         ((markdown-mode org-mode text-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         ((markdown-mode org-mode text-mode) . ap/configure-flyspell)
         (prog-mode . (lambda () (ap/configure-flyspell t))))
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-M-i" . nil)))

(use-package flyspell-lazy
  :straight t
  :hook ((flyspell-mode) . (lambda () (flyspell-lazy-mode 1))))

(provide 'init-flyspell)
;; init-flyspell.el ends here
