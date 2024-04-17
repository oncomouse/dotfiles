;;; init-terminals.el --- Terminal emulators          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package vterm
  :straight t
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  :bind (:map vterm-mode-map
              ("M-v" . scroll-up-command)
              ("C-v" . scroll-down-command)
              ("M-g" . nil)
              ("C-c C-c" . (lambda (_) (interactive "p") (vterm-send "C-c")))))

(use-package vterm-toggle
  :straight t
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-fullscreen-p nil)
  :bind (:map ap/leader-open-map
              ("t" . vterm-toggle)
              ("T" . ap/open-fullscreen-vterm)
              :map vterm-mode-map
              ("s-n" . vterm-toggle-forward)
              ("s-p" . vterm-toggle-backward))
  :config
  ;; Display vterm at the bottom:
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p "*vterm*" (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.35))))

(defun ap/open-fullscreen-vterm ()
  "Open a vterm in fullscreen mode"
  (interactive)
  (let ((vterm-toggle-fullscreen-p t)
        (display-buffer-alist nil))
    (vterm)))


(provide 'init-terminals)
;;; init-terminals.el ends here
