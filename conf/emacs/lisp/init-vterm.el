;;; init-vterm.el --- Vterm configuration
;;; Commentary:
;;; Code:
(when (require-package 'vterm)
  (add-hook 'vterm-mode-hook (lambda () ""
                               (define-key vterm-mode-map "M-g" nil)
                               (define-key vterm-mode-map "C-c C-c" (lambda (_) (interactive "p") (vterm-send "C-c")))))
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000))

(when (require-package 'vterm-toggle)
  (define-key ap/leader-open-map (kbd "t") 'vterm-toggle)
  (define-key ap/leader-open-map (kbd "T") 'vterm-toggle-cd)
  (add-hook 'vterm-mode-hook (lambda () ""
                               (define-key vterm-mode-map (kdb "s-n") 'vterm-toggle-forward)
                               (define-key vterm-mode-map (kdb "s-p") 'vterm-toggle-backward)))
  (setq vterm-toggle-fullscreen-p nil)

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
(provide 'init-vterm)
;;; init-vterm.el ends here
