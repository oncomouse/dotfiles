(use-package ido
  :elpaca nil
  :config
  (ido-mode +1)
  (fido-mode)
  (setq ido-everywhere t
  ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode +1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package ido-completing-read+
  :config (ido-ubiquitous-mode +1))

(use-package flx-ido :config (flx-ido-mode +1))

(defun ap/find-file ()
  (interactive)
  (call-interactively 'ido-find-file))
(defun ap/find-recent-file ()
  (interactive)
  (call-interactively 'recentf))

(provide 'init-ido)
