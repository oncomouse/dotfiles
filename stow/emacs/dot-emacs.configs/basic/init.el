(defconst *is-a-mac* (eq system-type 'darwin))
; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; Fonts
(add-to-list 'default-frame-alist
             '(font . "FiraCode Nerd Font-18"))

; Load Catppuccin
(straight-use-package 'catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)

; Load which-key
(straight-use-package 'which-key)
(which-key-mode)

; Disable visual elements:
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(when *is-a-mac*
  (when (straight-use-package 'ns-auto-titlebar)
    (ns-auto-titlebar-mode)))

(provide 'init)
