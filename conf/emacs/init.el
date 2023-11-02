(add-to-list 'load-path "~/dotfiles/conf/emacs/lisp/")
(defconst *is-a-mac* (eq system-type 'darwin))

(setq dotfiles-seadrive-path (cond (*is-a-mac* "~/Library/CloudStorage/SeaDrive-oncomouse(seafile.jetbear.us)/My Libraries") (t "~/SeaDrive/My Libraries")))

;; Whoami
(setq user-full-name "Andrew Pilsch"
    user-mail-address "apilsch@tamu.edu")

;; Line numbers + relative line numbers
(setq display-line-numbers-type 'relative)
;; Disable for the following modes
(dolist (mode '(term-mode-hook
    vterm-mode-hook
    shell-mode-hook
    treemacs-mode-hook
    eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

(setq completions-detailed t)

;; Tabs
(setq tab-width 4)

;; Show stray whitespace.
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Add a newline automatically at the end of a file while saving.
(setq-default require-final-newline t)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Run a server, so we can open things from the command line:
(server-start)

(require 'elpaca)

(use-package general
  :demand t
  :config
  (general-evil-setup))

(use-package diminish
  :config
  (diminish 'flyspell-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

(use-package no-littering
  :config
  (no-littering-theme-backups))
(elpaca-wait)

;; Recent Files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(general-define-key
 "C-x C-r" 'recentf)

; Fonts
(add-to-list 'default-frame-alist
       (cond
         (*is-a-mac* '(font . "FiraCode Nerd Font-18"))
         (t '(font . "FiraCode Nerd Font-16"))))

; Load Catppuccin
(use-package catppuccin-theme
       :init
      (setq catppuccin-flavor 'mocha)
      (load-theme 'catppuccin :no-confirm))

; Load which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

;; Disable visual elements:
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(when *is-a-mac*
  (when (use-package ns-auto-titlebar)
  (ns-auto-titlebar-mode)))
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(menu-bar-mode -1)
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(require 'init-consult)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
		completion-category-defaults nil
		completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode)
  :config
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file)
                  (projectile-recentf . project-file)
                  (projectile-switch-to-buffer . buffer))
                marginalia-command-categories))
  :general
  (:keymaps '(completion-list-mode-map minibuffer-local-map)
   "M-a" 'marginalia-cycle))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))


(use-package eglot
  :elpaca nil
  :hook
  (
   (lua-mode . eglot)
   (html-mode . eglot)
   (css-mode . eglot)
   (ruby-mode . eglot)
   (json-mode . eglot)
   (javascript-mode . eglot))
  :config
  (add-to-list 'eglot-server-programs
                '(ruby-mode . ("standardrb" "--lsp"))))

(use-package flycheck
  :general
  (:states 'normal
           "] d" 'flycheck-next-error
           "[ d" 'flycheck-previous-error)
  :hook (
         (after-init . global-flycheck-mode)))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package consult-flycheck
  :after (flycheck consult)
  :general
  (:states 'normal :prefix "SPC"
           "d" 'consult-flycheck))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :elpaca nil
  :init
  (savehist-mode 1))

;; A few more useful configurations...
(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
          (replace-regexp-in-string
           "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
           crm-separator)
          (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package embark
  :ensure t

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :general
  ("C-." 'embark-act
   "C-;" 'embark-dwim
   "C-h B" 'embark-bindings)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
         '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
     nil
     (window-parameters (mode-line-format . none)))))

(use-package rg)
(use-package wgrep)

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-globally-ignored-directories
  '(".idea"
    ".vscode"
    ".ensime_cache"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    "_FOSSIL_"
    ".bzr"
    "_darcs"
    ".tox"
    ".svn"
    ".stack-work"
    ".ccls-cache"
    ".cache"
    ".clangd"))
  ;; Recommended keymap prefix on macOS
  (when *is-a-mac* (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
      ;; Recommended keymap prefix on Windows/Linux
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-popupinfo-delay (cons nil 1.0))
  :init
  (global-corfu-mode)
  :general
  (:states 'insert
   "C-x C-o" 'completion-at-point)
  (:keymaps 'corfu-map
   :states 'insert
   "SPC" #'corfu-insert-separator
   "C-n" #'corfu-next
   "C-p" #'corfu-previous
   "C-y" #'corfu-insert
   "C-c" #'corfu-quit)
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))

(use-package cape
  :general
  (:states 'insert
   :prefix "C-x"
   "C-t" 'complete-tag
   "C-n" 'cape-dabbrev
   "C-p" 'cape-abbrev
   "C-f" 'cape-file
   "C-l" 'cape-line
   "C-k" 'cape-dict))

(use-package nerd-icons)
(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Bibliography
;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography (concat dotfiles-seadrive-path "/My Library/Documents/Academic Stuff/library.bib"))
(eval-after-load 'reftex-vars
  '(progn
     (setq reftex-cite-format '((?\C-m . "[@%l]")))))
(add-hook 'markdown-mode-hook
    (lambda () (define-key markdown-mode-map "\C-c["
         (lambda ()
           (interactive)
           (let ((reftex-cite-format "[@%l]"))
             (reftex-citation))))))

;; Citar for advanced citation:
(use-package citar
  :custom
  (citar-bibliography reftex-default-bibliography)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup))
(use-package citar-embark
  :diminish citar-embark-mode
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;; Other filetype modes:
(use-package markdown-mode)
(use-package lua-mode
  :init
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;; Org
(general-define-key
 :prefix "C-c"
 "a" 'org-agenda
 "c" 'org-capture
 "l" 'org-store-link)
(setq org-directory (concat dotfiles-seadrive-path "/Todo/org"))
(setq org-agenda-files (list
      (concat dotfiles-seadrive-path "/Todo/todo.org")
      (concat dotfiles-seadrive-path "/Todo/inbox.org")))
(setq org-default-notes-file (concat dotfiles-seadrive-path "/Todo/inbox.org"))
(setq org-indent-mode "noindent")
(setq org-refile-targets
      '((nil :maxlevel . 2)
  (org-agenda-files :maxlevel . 2)))
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

(require 'init-evil)

;; (use-package highlight-indent-guides
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :custom
;;   (highlight-indent-guides-responsive 'top)
;;   (highlight-indent-guides-method 'character)
;;   (highlight-indent-guides-auto-character-face-perc 75)
;;   (highlight-indent-guides-auto-top-character-face-perc 80))

(use-package expand-region
       :bind
       ("C-=" . 'er/expand-region))

(use-package magit)

(use-package vterm :config)

(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :custom
  (dtrt-indent-max-lines 2000)
  (dtrt-indent-run-after-smie t)
  :config
  (dtrt-indent-global-mode)
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

;; Flyspell
(dolist (hook '(text-mode-hook markdown-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1)))
  (add-hook hook 'turn-on-visual-line-mode))
(use-package flyspell-correct
  :after flyspell
  :general
  (:states 'normal
            "z=" 'flyspell-correct-wrapper))

;; Electric Pair
(electric-pair-mode)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO
;; Lsp
;; Treesitter
;; Formatting
;; Linting
;; lexima
;; mini.surround custom targets
;; statusline
