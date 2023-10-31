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

; https://stackoverflow.com/questions/29934968/how-to-remove-the-temporary-files-starting-and-ending-with-created-by-emacs
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

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

(require 'elpaca)

(use-package general
  :demand t
  :config
  (general-evil-setup))
(elpaca-wait)

;; Recent Files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(general-define-key
 "C-x C-r" 'recentf)

; Fonts
(add-to-list 'default-frame-alist
       '(font . "FiraCode Nerd Font-18"))

; Load Catppuccin
(use-package catppuccin-theme
       :init
      (setq catppuccin-flavor 'mocha)
      (load-theme 'catppuccin :no-confirm))

; Load which-key
(use-package which-key
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

(require 'init-icomplete)
;; (require 'init-ido)
;; (require 'init-consult)

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
  (general-define-key
   :keymaps '(completion-list-mode-map minibuffer-local-map)
   "M-a" 'marginalia-cycle))

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

  :config
  (general-define-key
   "C-." 'embark-act
   "C-;" 'embark-dwim
   "C-h B" 'embark-bindings)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
         '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
     nil
     (window-parameters (mode-line-format . none)))))


(use-package projectile
  :config
  (projectile-mode +1)
  ;; Recommended keymap prefix on macOS
  (when *is-a-mac* (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
      ;; Recommended keymap prefix on Windows/Linux
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; ;; Company for completion (works like omnifunc in vim)
;; (use-package company
;;        :config
;;        (setq company-idle-delay nil)
;;        (global-company-mode t)
;;        (general-define-key
;;         :keymaps 'company-active-map
;;         "RET" nil
;;         "C-y" 'company-complete-selection)
;;        (general-define-key
;;         :states 'insert
;;         "C-x C-o" 'company-complete))
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

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

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-separator ?\s)
  :init
  (global-corfu-mode)
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (general-define-key
   :states 'insert
   "C-x C-o" 'completion-at-point)
  (general-define-key
   :keymaps 'corfu-map
   "C-y" 'corfu-insert
   "C-c" 'corfu-quit))

(use-package markdown-mode)
(use-package lua-mode
  :init
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

; Org
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

;; (use-package devil
;;   :after evil
;;   :hook
;;    ((evil-insert-state-entry . devil-mode)
;;    (evil-insert-state-exit . devil-mode)))

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
(with-eval-after-load 'evil-collection (evil-collection-vterm-setup))

(use-package dtrt-indent
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
