;;; init-undo.el -*- lexical-binding: t; -*-

(use-package undo-fu
  :straight t
  :hook (after-init . undo-fu-mode)
  :custom
  ;; Increase undo history limits to reduce likelihood of data loss
  (undo-limit 400000)           ; 400kb (default is 160kb)
  (undo-strong-limit 3000000)   ; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  :config
  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))

(use-package undo-fu-session
  :straight t
  :hook (undo-fu-mode  . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory (concat user-emacs-directory "undo-fu-session/"))
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst)))

(provide 'init-undo)
;;; init-undo.el ends here
