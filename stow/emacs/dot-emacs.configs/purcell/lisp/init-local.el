(when *is-a-mac*
  (setq mac-command-modifier 'none)
  (setq mac-option-modifier 'meta))

; Load Catppuccin
; (package! catppuccin
;   :recipe (:host github :repo "catppuccin/emacs"))
(download-site-lisp-module 'catppuccin-theme "https://raw.githubusercontent.com/catppuccin/emacs/main/catppuccin-theme.el")
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)


(provide 'init-local)
