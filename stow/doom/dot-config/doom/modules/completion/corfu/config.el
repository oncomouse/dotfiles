;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-dabbrev'.")

(defvar +corfu-want-minibuffer-completion t
  "Whether to enable Corfu in the minibuffer.
Setting this to `aggressive' will enable Corfu in more commands which
use the minibuffer such as `query-replace'.")

(defvar +corfu-want-ret-to-confirm t
  "Configure how the user expects RET to behave.
Possible values are:
- t (default): Insert candidate if one is selected, pass-through otherwise;
- `minibuffer': Insert candidate if one is selected, pass-through otherwise,
              and immediatelly exit if in the minibuffer;
- nil: Pass-through without inserting.")

;;
;;; Packages
(use-package! corfu
  :hook (doom-first-input . global-corfu-mode)
  :init
  (add-hook! 'minibuffer-setup-hook
    (defun +corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer."
      (when (pcase +corfu-want-minibuffer-completion
              ('aggressive
               (not (or (bound-and-true-p mct--active)
                        (bound-and-true-p vertico--input)
                        (eq (current-local-map) read-passwd-map)
                        (and (featurep 'helm-core) (helm--alive-p))
                        (and (featurep 'ido) (ido-active))
                        (where-is-internal 'minibuffer-complete
                                           (list (current-local-map)))
                        (memq #'ivy--queue-exhibit post-command-hook))))
              ('nil nil)
              (_ (where-is-internal #'completion-at-point
                                    (list (current-local-map)))))
        (setq-local corfu-echo-delay nil)
        (corfu-mode +1))))
  (when (modulep! +orderless)
    (after! orderless
      (setq orderless-component-separator #'orderless-escapable-split-on-space)))
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
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (modulep! +orderless) 'separator t)
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        tab-always-indent 'complete)
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))
  (add-to-list 'corfu-auto-commands #'lispy-colon)
  (add-to-list 'corfu-continue-commands #'+corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'+corfu-smart-sep-toggle-escape)
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  (when (modulep! +icons)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  ;; If you want to update the visual hints after completing minibuffer commands
  ;; with Corfu and exiting, you have to do it manually.
  (defadvice! +corfu--insert-before-exit-minibuffer-a ()
    :before #'exit-minibuffer
    (when (or (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))
              (and (featurep 'corfu-terminal)
                   (popon-live-p corfu-terminal--popon)))
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer))))))

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
    (add-hook! (prog-mode text-mode conf-mode comint-mode minibuffer-setup
                          eshell-mode)
      (defun +corfu-add-cape-dabbrev-h ()
        (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))
    (after! dabbrev
      (setq cape-dabbrev-check-other-buffers t
            dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
            dabbrev-ignored-buffer-regexps
            '("^ "
              "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
            dabbrev-upcase-means-case-search t)
      (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)))

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
