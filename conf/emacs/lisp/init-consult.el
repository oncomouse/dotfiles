;; Enable vertico
(use-package vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :elpaca (vertico :files (:defaults "extensions/*")
                   :includes (vertico-indexed
                              vertico-flat
                              vertico-grid
                              vertico-mouse
                              vertico-quick
                              vertico-buffer
                              vertico-repeat
                              vertico-reverse
                              vertico-directory
                              vertico-multiform
                              vertico-unobtrusive
                              ))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 13)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  (setq vertico-multiform-categories
        '((t reverse))) ;; Reverse everything by default
  :config
  (vertico-multiform-mode)
  (general-define-key
   :states '(normal insert visual motion)
   "M-." #'vertico-repeat)
  (general-define-key
   :keymaps 'vertico-map
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group
   "<backspace>" #'vertico-directory-delete-char
   "C-w" #'vertico-directory-delete-word
   "C-<backspace>" #'vertico-directory-delete-word
   "RET" #'vertico-directory-enter)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand))))

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package consult
  :config

  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fr" 'consult-recent-file
   "fF" 'consult-find-file
   "a"  'consult-buffer)
  (general-define-key
   "C-x b" 'consult-buffer
   "C-x C-r" 'consult-recent-file))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package consult-projectile
  :after projectile)

(provide 'init-consult)
