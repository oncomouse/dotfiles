;;; config-corfu  --- Corfu configuration for Doom Emacs
;;; Commentary:
;;; Code:
(after! corfu
  ;; Temporary fix for 'corfu--setup having a new signature
  (after! evil
    (advice-remove 'corfu--setup 'evil-normalize-keymaps)
    (advice-remove 'corfu--teardown 'evil-normalize-keymaps)
    (advice-add 'corfu--setup :after (lambda (&rest r) (evil-normalize-keymaps)))
    (advice-add 'corfu--teardown :after  (lambda (&rest r) (evil-normalize-keymaps))))
  ;; Configure orderless:
  (after! orderless
    (setq orderless-component-separator #'orderless-escapable-split-on-space)))

;; Disable autocompletion
(after! corfu
  (setq corfu-auto nil)
  (remove-hook 'post-command-hook #'corfu--auto-post-command 'local))

(map! :when (modulep! :completion corfu)
      :after corfu
      (:map corfu-mode-map
            "C-M-i" #'completion-at-point
            (:prefix "C-x"
             :i "C-l" #'cape-line
             :i "C-k" #'cape-keyword
             :i "C-f" #'cape-file
             :i "s"   #'cape-dict
             :i "C-s" #'yasnippet-capf
             :i "C-o" #'completion-at-point
             :i "C-n" #'cape-dabbrev
             :i "C-p" #'cape-history))
      (:map corfu-map
            "SPC" #'corfu-insert-separator
            "C-y" #'corfu-insert
            "M-m" #'+corfu-move-to-minibuffer
            "C-c" #'corfu-quit
            "C-p" #'corfu-previous
            "C-n" #'corfu-next
            (:when (modulep! :completion corfu +orderless)
              "<remap> <completion-at-point>" #'+corfu-smart-sep-toggle-escape)
            (:when (modulep! :completion corfu +tng)
              [tab] #'corfu-next
              "TAB" #'corfu-next
              [backtab] #'corfu-previous
              "S-TAB" #'corfu-previous))
      (:after corfu-popupinfo
       :map corfu-popupinfo-map
       "C-S-h" #'corfu-popupinfo-toggle
       "C-<up>" #'corfu-popupinfo-scroll-down
       "C-<down>" #'corfu-popupinfo-scroll-up
       "C-S-p" #'corfu-popupinfo-scroll-down
       "C-S-n" #'corfu-popupinfo-scroll-up
       "C-S-u" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-down corfu-popupinfo-min-height))
       "C-S-d" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-up corfu-popupinfo-min-height)))
      (:map corfu-map
       :gi "C-<return>" '(menu-item "Conclude the minibuffer" exit-minibuffer
                          :enable (active-minibuffer-window))
       :gi "S-<return>" '(menu-item "Insert completion and conclude" +corfu-complete-and-exit-minibuffer
                          :enable (active-minibuffer-window))))

;; (use-package! corfu
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-separator ?\s)
;;   (corfu-quit-at-boundary 'separator)
;;   (corfu-quit-no-match 'separator)
;;   (corfu-preview-current 'insert)
;;   (corfu-popupinfo-delay (cons nil 1.0))
;;   :init
;;   (global-corfu-mode)
;;   :config
;;   (defun corfu-move-to-minibuffer ()
;;     (interactive)
;;     (when completion-in-region--data
;;       (let ((completion-extra-properties (nth 4 completion-in-region--data))
;;             completion-cycle-threshold completion-cycling)
;;         (apply #'consult-completion-in-region (butlast completion-in-region--data)))))
;;   (require 'corfu-popupinfo)
;;   (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
;;   (corfu-popupinfo-mode)

;;   ;; Temporary fix for 'corfu--setup having a new signature
;;   (after! evil
;;     (advice-remove 'corfu--setup 'evil-normalize-keymaps)
;;     (advice-remove 'corfu--teardown 'evil-normalize-keymaps)
;;     (advice-add 'corfu--setup :after (lambda (&rest r) (evil-normalize-keymaps)))
;;     (advice-add 'corfu--teardown :after  (lambda (&rest r) (evil-normalize-keymaps))))
;;   (after! orderless
;;     (setq orderless-component-separator #'orderless-escapable-split-on-space)))

;; (map!
;;  :after (corfu evil)
;;  (:i "C-x C-o" 'completion-at-point)
;;  (:map corfu-map
;;   :i "SPC" #'corfu-insert-separator
;;   :i "C-n" #'corfu-next
;;   :i "C-p" #'corfu-previous
;;   :i "C-y" #'corfu-insert
;;   :i "C-c" #'corfu-quit
;;   :i "M-m" #'corfu-move-to-minibuffer
;;   :i "M-a" #'corfu-popupinfo-toggle)
;;  (:after corfu-popupinfo
;;   :map corfu-popupinfo-map
;;   "C-<up>" #'corfu-popupinfo-scroll-down
;;   "C-<down>" #'corfu-popupinfo-scroll-up
;;   "C-S-p" #'corfu-popupinfo-scroll-down
;;   "C-S-n" #'corfu-popupinfo-scroll-up
;;   "C-S-u" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-down corfu-popupinfo-min-height))
;;   "C-S-d" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-up corfu-popupinfo-min-height))))

;; (after! lsp-mode
;;   (setq lsp-completion-provider :none)
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))) ;; Configure orderless
;;   (add-hook! lsp-completion-mode #'my/lsp-mode-setup-completion)
;;   )

;; (use-package! cape
;;   :defer t
;;   :init
;;   (add-hook! prog-mode
;;     (defun +corfu-add-cape-file-h ()
;;       (add-hook 'completion-at-point-functions #'cape-file -10 t)))
;;   (add-hook! (org-mode markdown-mode)
;;     (defun +corfu-add-cape-elisp-block-h ()
;;       (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t)))
;;   ;; Enable Dabbrev completion basically everywhere as a fallback.
;;   ;; Set up `cape-dabbrev' options.
;;   (defun +dabbrev-friend-buffer-p (other-buffer)
;;     (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))
;;   (after! dabbrev
;;     (setq cape-dabbrev-check-other-buffers t
;;           dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
;;           dabbrev-ignored-buffer-regexps
;;           '("^ "
;;             "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
;;           dabbrev-upcase-means-case-search t)
;;     (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)

;;     (add-hook! (prog-mode text-mode conf-mode comint-mode minibuffer-setup
;;                           eshell-mode)
;;       (defun +corfu-add-cape-dabbrev-h ()
;;         (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))))
;;   ;; Complete emojis :).
;;   (add-hook! (prog-mode conf-mode)
;;     (defun +corfu-add-cape-emoji-h ()
;;       (add-hook 'completion-at-point-functions
;;                 (cape-capf-inside-faces
;;                  (cape-capf-prefix-length #'cape-emoji 1)
;;                  ;; Only call inside comments and docstrings.
;;                  'tree-sitter-hl-face:doc 'font-lock-doc-face
;;                  'font-lock-comment-face 'tree-sitter-hl-face:comment)
;;                 10 t))
;;     (add-hook! text-mode
;;       (defun +corfu-add-cape-emoji-text-h ()
;;         (add-hook 'completion-at-point-functions
;;                   (cape-capf-prefix-length #'cape-emoji 1) 10 t))))
;;   ;; Enable dictionary-based autocompletion.
;;   (add-hook! (prog-mode conf-mode)
;;     (defun +corfu-add-cape-dict-h ()
;;       (add-hook 'completion-at-point-functions
;;                 (cape-capf-inside-faces
;;                  ;; Only call inside comments and docstrings.
;;                  #'cape-dict 'tree-sitter-hl-face:doc 'font-lock-doc-face
;;                  'font-lock-comment-face 'tree-sitter-hl-face:comment)
;;                 40 t))
;;     (add-hook! text-mode
;;       (defun +corfu-add-cape-dict-text-h ()
;;         (add-hook 'completion-at-point-functions #'cape-dict 40 t))))

;;   ;; Make these capfs composable.
;;   (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
;;   (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
;;   (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
;;   (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
;;   ;; From the `cape' readme. Without this, Eshell autocompletion is broken on
;;   ;; Emacs28.
;;   (when (< emacs-major-version 29)
;;     (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;;     (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
;;   (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

;;   (map!
;;    :after cape
;;    :prefix "C-x"
;;     :i "C-t" 'complete-tag
;;     :i "C-n" 'cape-dabbrev
;;     :i "C-p" 'cape-abbrev
;;     :i "C-f" 'cape-file
;;     :i "C-l" 'cape-line
;;     :i "C-k" 'cape-dict)

;; ;; Add nerd-icons to corfu
;; (use-package! nerd-icons-corfu
;;   :after (nerd-icons corfu)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'config-corfu)
;;; config-corfu.el ends here
