;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-dabbrev'.")

;;
;;; Packages
(use-package! corfu
  :defer t
  :hook (doom-first-input . global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        global-corfu-modes '((not
                              erc-mode
                              circe-mode
                              help-mode
                              gud-mode
                              vterm-mode)
                             t)
        corfu-cycle t
        corfu-separator (when (modulep! +orderless) ?\s)
        corfu-preselect (if (modulep! +tng) 'prompt 'valid)
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (modulep! +orderless) 'separator t)
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))

  (add-to-list 'corfu-continue-commands #'+corfu-move-to-minibuffer)

  (add-hook! 'minibuffer-setup-hook
    (defun +corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point (list (current-local-map)))
        (setq-local corfu-echo-delay nil)
        (corfu-mode +1))))

  (after! evil
    (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

  (when (modulep! +icons)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (when (modulep! +orderless)
    (after! orderless
      (setq orderless-component-separator #'orderless-escapable-split-on-space))))

(use-package! cape
  :defer t
  :init
  (add-hook! prog-mode
    (defun +corfu-add-cape-file-h ()
      (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  (add-hook! (org-mode markdown-mode)
    (defun +corfu-add-cape-elisp-block-h ()
      (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t)))
  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (when (modulep! +dabbrev)
    ;; Set up `cape-dabbrev' options.
    (defun +dabbrev-friend-buffer-p (other-buffer)
      (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))
    (after! dabbrev
      (setq cape-dabbrev-check-other-buffers t
            dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
            dabbrev-ignored-buffer-regexps
            '("^ "
              "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
            dabbrev-upcase-means-case-search t)
      (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)

      (add-hook! (prog-mode text-mode conf-mode comint-mode minibuffer-setup
                            eshell-mode)
        (defun +corfu-add-cape-dabbrev-h ()
          (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))))
  ;; Complete emojis :).
  (when (and (modulep! +emoji) (> emacs-major-version 28))
    (add-hook! (prog-mode conf-mode)
      (defun +corfu-add-cape-emoji-h ()
        (add-hook 'completion-at-point-functions
                  (cape-capf-inside-faces
                   (cape-capf-prefix-length #'cape-emoji 1)
                   ;; Only call inside comments and docstrings.
                   'tree-sitter-hl-face:doc 'font-lock-doc-face
                   'font-lock-comment-face 'tree-sitter-hl-face:comment)
                  10 t)))
    (add-hook! text-mode
      (defun +corfu-add-cape-emoji-text-h ()
        (add-hook 'completion-at-point-functions
                  (cape-capf-prefix-length #'cape-emoji 1) 10 t))))
  ;; Enable dictionary-based autocompletion.
  (when (modulep! +dict)
    (add-hook! (prog-mode conf-mode)
      (defun +corfu-add-cape-dict-h ()
        (add-hook 'completion-at-point-functions
                  (cape-capf-inside-faces
                   ;; Only call inside comments and docstrings.
                   #'cape-dict 'tree-sitter-hl-face:doc 'font-lock-doc-face
                   'font-lock-comment-face 'tree-sitter-hl-face:comment)
                  40 t)))
    (add-hook! text-mode
      (defun +corfu-add-cape-dict-text-h ()
        (add-hook 'completion-at-point-functions #'cape-dict 40 t))))

  ;; Make these capfs composable.
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  ;; From the `cape' readme. Without this, Eshell autocompletion is broken on
  ;; Emacs28.
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package! yasnippet-capf
  :when (modulep! :editor snippets)
  :defer t
  :init
  (add-hook! 'yas-minor-mode-hook
    (defun +corfu-add-yasnippet-capf-h ()
      (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))))

(use-package! corfu-terminal
  :when (not (display-graphic-p))
  :hook ((corfu-mode . corfu-terminal-mode)))

;;
;;; Extensions

(use-package! corfu-history
  :hook ((corfu-mode . corfu-history-mode))
  :config
  (after! savehist (add-to-list 'savehist-additional-variables 'corfu-history)))


(use-package! corfu-popupinfo
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))
