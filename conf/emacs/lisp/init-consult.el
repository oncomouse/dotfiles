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
  (setq vertico-multiform-commands
        '((execute-extended-command flat)))
  :general
  (:states '(normal)
   "M-." #'vertico-repeat)
  (:keymaps 'vertico-map
   "TAB" #'vertico-insert
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group
   "C-d"   #'vertico-scroll-up
   "C-u"   #'vertico-scroll-down
   "C-f"   'universal-argument ;; Use C-f (alt universal) here too
   "<backspace>" #'vertico-directory-delete-char
   "C-w" #'vertico-directory-delete-word
   "C-<backspace>" #'vertico-directory-delete-word
   "RET" #'vertico-directory-enter
   "M-i" #'vertico-quick-insert
   "C-o" #'vertico-quick-exit
   "M-G" #'vertico-multiform-grid
   "M-F" #'vertico-multiform-flat
   "M-R" #'vertico-multiform-reverse
   "M-U" #'vertico-multiform-unobtrusive)
  :config
  (vertico-multiform-mode)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand))))

(use-package consult
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  :general
  (:states 'normal
   :prefix "SPC")
  ("M-g i" 'consult-imenu ;; Use consult-imenu instead of imenu
   "C-x b" 'consult-buffer
   "C-x C-r" 'consult-recent-file)
  :config
  ;; Disable preview without M-.
  (consult-customize
   consult-buffer consult-recent-file consult-buffer consult-ripgrep
   consult-projectile
   :preview-key "M-."))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package consult-projectile
  :after projectile)

(provide 'init-consult)
