;; init-helpful.el --- Support for helpful

(when (require-package 'helpful)
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)

  ;; Source: https://github.com/Wilfred/helpful/issues/250
  (defvar *helpful-buffer-ring-size 5
    "How many buffers are stored for use with `*helpful-next'.")

  (defvar *helpful--buffer-ring (make-ring *helpful-buffer-ring-size)
    "Ring that stores the current Helpful buffer history.")

  (defun *helpful--buffer-index (&optional buffer)
    "If BUFFER is a Helpful buffer, return it’s index in the buffer ring."
    (let ((buf (or buffer (current-buffer))))
      (and (eq (buffer-local-value 'major-mode buf) 'helpful-mode)
           (seq-position (ring-elements *helpful--buffer-ring) buf #'eq))))

  (advice-add 'helpful--buffer
              :filter-return (lambda (help-buf)
                               (let ((buf-ring *helpful--buffer-ring))
                                 (let ((newer-buffers (or (*helpful--buffer-index) 0)))
                                   (dotimes (_ newer-buffers) (ring-remove buf-ring 0)))
                                 (when (/= (ring-size buf-ring) *helpful-buffer-ring-size)
                                   (ring-resize buf-ring *helpful-buffer-ring-size))
                                 (ring-insert buf-ring help-buf))))

  (defun *helpful--next (&optional buffer)
    "Return the next live Helpful buffer relative to BUFFER."
    (let ((buf-ring *helpful--buffer-ring)
          (index (or (*helpful--buffer-index buffer) -1)))
      (cl-block nil
        (while (> index 0)
          (cl-decf index)
          (let ((buf (ring-ref buf-ring index)))
            (if (buffer-live-p buf) (cl-return buf)))
          (ring-remove buf-ring index)))))

  (defun *helpful--previous (&optional buffer)
    "Return the previous live Helpful buffer relative to BUFFER."
    (let ((buf-ring *helpful--buffer-ring)
          (index (1+ (or (*helpful--buffer-index buffer) -1))))
      (cl-block nil
        (while (< index (ring-length buf-ring))
          (let ((buf (ring-ref buf-ring index)))
            (if (buffer-live-p buf) (cl-return buf)))
          (ring-remove buf-ring index)))))

  (defun *helpful-next ()
    "Go to the next Helpful buffer."
    (interactive)
    (when-let (buf (*helpful--next))
      (funcall helpful-switch-buffer-function buf)))

  (defun *helpful-previous ()
    "Go to the previous Helpful buffer."
    (interactive)
    (when-let (buf (*helpful--previous))
      (funcall helpful-switch-buffer-function buf)))

  (with-eval-after-load 'helpful (define-key helpful-mode-map (kbd "C-x C-b") '*helpful-previous)
                        (define-key helpful-mode-map (kbd "C-x C-f") '*helpful-next)))

;; Pop-up support for documentation windows
(add-to-list 'display-buffer-alist
             '("^\\*\\([Hh]elp\\|Apropos\\|info\\)" (display-buffer-reuse-window display-buffer-in-side-window)
               (quit . t)
               (transient . t)
               (side . bottom)
               (slot . 0)
               (window-height . 0.33)))

(provide 'init-helpful)
;;; init-helpful.el ends here
