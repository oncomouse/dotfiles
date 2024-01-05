;; CORFU
(use-package! corfu
  :custom
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-popupinfo-delay (cons nil 1.0))
  :init
  (global-corfu-mode)
  :config
  (defun +corfu--enable-in-minibuffer ()
    "Enable Corfu completion in the minibuffer, e.g., `eval-expression'."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto t)
      (corfu-mode 1)))
  (defun corfu-move-to-minibuffer ()
      "Move current completions to the minibuffer"
      (interactive)
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data)))
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (add-hook 'minibuffer-setup-hook #'+corfu--enable-in-minibuffer)

  ;; Temporary fix for 'corfu--setup having a new signature
  (after! evil
    (advice-remove 'corfu--setup 'evil-normalize-keymaps)
    (advice-remove 'corfu--teardown 'evil-normalize-keymaps)
    (advice-add 'corfu--setup :after (lambda (&rest r) (evil-normalize-keymaps)))
    (advice-add 'corfu--teardown :after  (lambda (&rest r) (evil-normalize-keymaps)))))

(map!
 :after (corfu evil)
 (:i "C-x C-o" 'completion-at-point)
 (:map corfu-map
  :i "SPC" #'corfu-insert-separator
  :i "C-n" #'corfu-next
  :i "C-p" #'corfu-previous
  :i "C-y" #'corfu-insert
  :i "C-c" #'corfu-quit
  :i "M-m" #'corfu-move-to-minibuffer
  :i "M-a" #'corfu-popupinfo-toggle
  :i "C-u" #'corfu-popupinfo-scroll-up
  :i "C-d" #'corfu-popupinfo-scroll-down))

(after! lsp-mode
  (setq lsp-completion-provider :none)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  (add-hook! lsp-completion-mode #'my/lsp-mode-setup-completion)
  )

(use-package! cape
  :config
  (map!
   (:prefix "C-x"
    :i "C-t" 'complete-tag
    :i "C-n" 'cape-dabbrev
    :i "C-p" 'cape-abbrev
    :i "C-f" 'cape-file
    :i "C-l" 'cape-line
    :i "C-k" 'cape-dict)))
;; END CORFU

;; Add nerd-icons to corfu
(use-package! nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'config-corfu)
