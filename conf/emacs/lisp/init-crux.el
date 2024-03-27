;;; init-crux.el --- Config for crux       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package crux
  :straight t
  :bind (("C-k" . crux-smart-kill-line)
         ("C-o" . crux-smart-open-line)
         ("C-S-o" . crux-smart-open-line-above)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-^" . crux-top-join-line)
         :map ap/leader-open-map
         ("o" . crux-open-with)
         :map ctl-x-map
         ("C-u" . crux-upcase-region)
         ("C-l" . crux-downcase-region)
         ("M-c" . crux-capitalize-region)))

(provide 'init-crux)
;;; init-crux.el ends here
