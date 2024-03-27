;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)

(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

;; (when (maybe-require-package 'corfu)
;;   (setq-default corfu-auto t)
;;   (with-eval-after-load 'eshell
;;     (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
;;   (setq-default corfu-quit-no-match 'separator)
;;   (add-hook 'after-init-hook 'global-corfu-mode)



;;   (with-eval-after-load 'corfu
;;     (corfu-popupinfo-mode))

;;   ;; Make Corfu also work in terminals, without disturbing usual behaviour in GUI
;;   (when (maybe-require-package 'corfu-terminal)
;;     (with-eval-after-load 'corfu
;;       (corfu-terminal-mode)))

;;   ;; TODO: https://github.com/jdtsmith/kind-icon
;;   )

(use-package orderless
  :straight t
  :after vertico
  :config
  (require 'orderless)
  (setq completion-styles '(orderless basic)))

(use-package corfu
  :straight t
  :hook (after-init . global-corfu-mode)
  :bind (("C-M-i" . completion-at-point)
         :map corfu-map
         ("C-c" . corfu-quit)
         ("C-g" . corfu-quit)
         ("C-y" . corfu-insert))
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match 'separator)
  :config
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (corfu-popupinfo-mode)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package corfu-terminal
  :straight t
  :after corfu
  :config
  (corfu-terminal-mode))

(provide 'init-corfu)
;;; init-corfu.el ends here
