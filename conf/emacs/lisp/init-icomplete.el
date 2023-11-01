(use-package icomplete-vertical
  :ensure t
  :demand t
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  :general
  (:states 'normal
   :prefix "SPC"
   "fr" 'recentf
   "a"  'switch-to-buffer
   "i"  'imenu)
  (:keymaps 'icomplete-minibuffer-map
     "<down>" 'icomplete-forward-completions
     "C-n"    'icomplete-forward-completions
     "<up>"   'icomplete-backward-completions
     "C-p"    'icomplete-backward-completions
     "RET"    'icomplete-force-complete-and-exit
     "C-v"    'icomplete-vertical-toggle))

(provide 'init-icomplete)
