;;; init-spelling.el --- Configure spellchecking
;;; Commentary:
;;; Code:

(use-package spell-fu
  :straight t
  :init
  (setq spell-fu-word-delimit-camel-case t)
  (setq spell-fu-global-ignore-buffer (lambda (buf) (buffer-local-value 'buffer-read-only buf)))
  :config
  (spell-fu-global-mode))

(provide 'init-spelling)
;; init-spelling.el ends here
