;;; init-mmm.el --- Multiple Major Modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mmm-mode
  :straight t
  :custom
  (mmm-global-mode 'buffers-with-submode-classes)
  (mmm-submode-decoration-level 2)
  :config
  (require 'mmm-auto))

(provide 'init-mmm)
;;; init-mmm.el ends here
