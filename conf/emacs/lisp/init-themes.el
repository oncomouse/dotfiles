;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(use-package catppuccin-theme
  :straight t
  :custom
  (custom-safe-themes t)
  (catppuccin-flavor 'latte)
  :init
  (setq-default custom-enabled-themes '(catppuccin))
  :hook (after-init . reapply-themes))

;; Toggle between light and dark
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq catppuccin-flavor 'latte)
  (catppuccin-reload))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

(use-package dimmer
  :straight t
  :hook (after-init . dimmer-mode)
  :init
  (setq-default dimmer-fraction 0.15)
  :config
  (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p)))
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p))

(provide 'init-themes)
;;; init-themes.el ends here
