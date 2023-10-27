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

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Add a newline automatically at the end of a file while saving.
(setq-default require-final-newline t)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Recent Files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf) ; Load recent-files

(require 'elpaca)

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

(use-package ido
  :elpaca nil
  :config
  (ido-mode +1)
  (fido-mode)
  (setq ido-everywhere t
	ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode +1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package ido-completing-read+
  :config (ido-ubiquitous-mode +1))

(use-package flx-ido :config (flx-ido-mode +1))

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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (setq embark-indicators '(embark-minimal-indicator))
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

;; Company for completion (works like omnifunc in vim)
(use-package company
	     :bind (:map company-active-map
			 ("RET" . nil)
			 ("C-y" . company-complete-selection)
		    :map evil-insert-state-map
		    ("C-x C-o" . company-complete))
	     :config
	     (setq company-idle-delay nil)
	     (global-company-mode t))
(use-package company-box
  :hook (company-mode . company-box-mode))

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

(use-package markdown-mode)

; Org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-directory (concat dotfiles-seadrive-path "/Todo/org"))
(setq org-agenda-files (list
			(concat dotfiles-seadrive-path "/Todo/todo.org")
			(concat dotfiles-seadrive-path "/Todo/inbox.org")
			(concat dotfiles-seadrive-path "/Todo/org")))
(setq org-default-notes-file (concat dotfiles-seadrive-path "/Todo/inbox.org"))
(setq org-indent-mode "noindent")
(setq org-refile-targets
      '((nil :maxlevel . 2)
	(org-agenda-files :maxlevel . 2)))

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

; Evil
(setq evil-want-keybinding nil)
(use-package evil
  :bind (:map evil-normal-state-map
	      ("M-j" . evil-collection-unimpaired-move-text-down)
	      ("M-k" . evil-collection-unimpaired-move-text-up))
	    :custom
	    (evil-undo-system 'undo-redo)
	    (evil-want-C-u-scroll t)
	    :config
	    (evil-mode 1)
	    (evil-define-key 'normal 'global "ESC" 'keyboard-quit)
	    (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
		(define-key evil-insert-state-map (kbd "C-n") nil)
		(define-key evil-insert-state-map (kbd "C-p") nil)))

;; https://blog.meain.io/2020/emacs-highlight-yanked/
(with-eval-after-load 'evil
  (defun meain/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around 'meain/evil-yank-advice))
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package evil-collection
  :config
  (evil-collection-init))
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
	      "fr" 'recentf
	      "ff" 'projectile-find-file
	      "fF" 'ido-find-file
	      "fp" 'projectile-switch-project
	      "a"  'switch-to-buffer
	      "oa" 'org-agenda
	      "oc" 'org-capture
	      "ol" 'org-store-link
	      "oo" 'org-open-at-point
	      "o*" 'org-toggle-heading
	      "or" 'org-refile
	      "/"  'projectile-ripgrep
	      "k"  'kill-buffer))
(use-package evil-commentary
	     :config   (evil-commentary-mode))
(use-package evil-numbers
  :bind (:map evil-normal-state-map
	      ("C-c C-A" . evil-numbers/inc-at-pt)
	      ("C-c C-X" . evil-numbers/dec-at-pt)
	 :map evil-visual-state-map
	      ("C-c C-A" . evil-numbers/inc-at-pt)
	      ("C-c C-X" . evil-numbers/dec-at-pt)
	      ("g C-A" . evil-numbers/inc-at-pt-incremental)
	      ("g C-X" . evil-numbers/dec-at-pt-incremental)
	 ))

(use-package evil-org
		:after org
		:hook (org-mode . evil-org-mode)
		:bind (:map org-agenda-mode-map
			    ("q" . org-agenda-exit))
		:config
		(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
		; (evil-set-initial-state 'org-agenda-mode 'motion)
		; 	(evil-define-key 'motion org-agenda-mode-map
		; 	   "j" 'org-agenda-next-line
		;    "k" 'org-agenda-previous-line
		;    "gj" 'org-agenda-next-item
		;    "gk" 'org-agenda-previous-item
		;    "gH" 'evil-window-top
		;    "gM" 'evil-window-middle
		;    "gL" 'evil-window-bottom)
		)

(use-package devil
  :after evil
  :hook
   ((evil-insert-state-entry . devil-mode)
   (evil-insert-state-exit . devil-mode)))

(use-package expand-region
			 :bind
			 ("C-=" . 'er/expand-region))

(use-package magit)

(use-package vterm
  :config
  (evil-collection-vterm-setup))

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
