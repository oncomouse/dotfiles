;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )
;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
   ;; Configure a custom style dispatcher (see the Consult wiki)
   ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
		completion-category-defaults nil
		completion-category-overrides '((file (styles partial-completion)))))
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  :config
  (general-define-key
   :keymaps '(completion-list-mode-map minibuffer-local-map)
   "M-a" marginalia-cycle)

(use-package consult
  :config
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

(defun ap/find-file ()
  (interactive)
  (call-interactively 'consult-find-file))
(defun ap/find-recent-file ()
  (interactive)
  (call-interactively 'consult-recent-file))

(provide 'init-consult)
