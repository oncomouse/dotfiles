;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :straight t
  :hook (after-init . vertico-mode)
  :config
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package embark
  :straight t
  :after vertico
  :bind (("C-;" . embark-act)
         :map vertico-map
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-act))
  :config

  ;; Use helpful instead of help:
  (with-eval-after-load 'helpful
    (define-key embark-symbol-map (kbd "h") 'helpful-symbol))

  ;; Use embark for paging key help instead of which-key:
  (with-eval-after-load 'which-key
    (setq prefix-help-command #'embark-prefix-help-command))

  ;; Use which-key for embark suggestions:
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package consult
  :straight t
  :custom
  (consult-narrow-key "<")
  (consult-widen-key ">")
  :bind(([remap switch-to-buffer] . consult-buffer)
        ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
        ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
        ([remap goto-line] . consult-goto-line)
        ("C-x C-r" . consult-recent-file))
  :config
  (defmacro sanityinc/no-consult-preview (&rest cmds)
    `(with-eval-after-load 'consult
       (consult-customize ,@cmds :preview-key "M-P")))

  (sanityinc/no-consult-preview
   consult-ripgrep
   consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

  (when (and (executable-find "rg"))
    (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
      (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                      (symbol-name s))))
      (consult-ripgrep dir initial))
    (sanityinc/no-consult-preview sanityinc/consult-ripgrep-at-point)
    (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point))

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c l .") #'consult-org-heading)
    (define-key org-mode-map (kbd "C-c l /") #'consult-org-agenda)
    (define-key org-mode-map (kbd "C-c l g g") #'consult-org-heading)
    (define-key org-mode-map (kbd "C-c l g G") #'consult-org-agenda)))

(use-package embark-consult
  :straight t
  :hook (embark-collect-mode . embark-consult-preview-minor-mode)
  :after (embark consult)
  :config
  (require 'embark-consult))

(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
