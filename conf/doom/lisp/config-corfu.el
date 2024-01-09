;;; config-corfu  --- Corfu configuration for Doom Emacs
;;; Commentary:
;;; Code:
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
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties (nth 4 completion-in-region--data))
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region (butlast completion-in-region--data)))))
  (require 'corfu-popupinfo)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  (corfu-popupinfo-mode)

  ;; Temporary fix for 'corfu--setup having a new signature
  (after! evil
    (advice-remove 'corfu--setup 'evil-normalize-keymaps)
    (advice-remove 'corfu--teardown 'evil-normalize-keymaps)
    (advice-add 'corfu--setup :after (lambda (&rest r) (evil-normalize-keymaps)))
    (advice-add 'corfu--teardown :after  (lambda (&rest r) (evil-normalize-keymaps))))
  (after! orderless
    (setq orderless-component-separator #'orderless-escapable-split-on-space)))

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
  :i "M-a" #'corfu-popupinfo-toggle)
 (:after corfu-popupinfo
  :map corfu-popupinfo-map
  "C-<up>" #'corfu-popupinfo-scroll-down
  "C-<down>" #'corfu-popupinfo-scroll-up
  "C-S-p" #'corfu-popupinfo-scroll-down
  "C-S-n" #'corfu-popupinfo-scroll-up
  "C-S-u" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-down corfu-popupinfo-min-height))
  "C-S-d" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-up corfu-popupinfo-min-height))))

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

;; Add nerd-icons to corfu
(use-package! nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'config-corfu)
;;; config-corfu.el ends here
