(defconst *is-a-mac* (eq system-type 'darwin))

(setq dotfiles-seadrive-path (cond (*is-a-mac* "~/Library/CloudStorage/SeaDrive-oncomouse(seafile.jetbear.us)/My Libraries") (t "~/SeaDrive/My Libraries")))

; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Line numbers + relative line numbers
(display-line-numbers-mode)
(setq display-line-numbers 'relative)

; https://stackoverflow.com/questions/29934968/how-to-remove-the-temporary-files-starting-and-ending-with-created-by-emacs
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Set ido to use fuzzy matching
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(setq completions-detailed t)

          ; Line numbers + relative line numbers
(display-line-numbers-mode)
(setq display-line-numbers 'relative)

; Install straight.el
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

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


; Disable visual elements:
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; (use-package 'corfu
;;   :init
;;   (global-corfu-mode))
;; (with-eval-after-load 'corfu
;;                       (corfu-popupinfo-mode))

;; (mapcar #'straight-use-package '(prescient
;; 				 corfu-prescient
;; 				 ivy-prescient))
;; (with-eval-after-load 'corfu
;;                       (corfu-prescient-mode))
;; (with-eval-after-load 'ivy
;;                       (ivy-prescient-mode))

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

;; ; Ivy + Counsel + Swiper
;; (use-package ivy)
;; (use-package swiper)
;; (use-package counsel)
;; (ivy-mode)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; ;; enable this if you want `swiper' to use it
;; ;; (setq search-default-mode #'char-fold-to-regexp)
;; (global-set-key "\C-s" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'projectile-ripgrep)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; ;; ivy-rich mode
;; (use-package ivy-rich)
;; (ivy-rich-mode 1)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package consult)

(use-package projectile
             :config
            (projectile-mode +1)
            ;; Recommended keymap prefix on macOS
            (when *is-a-mac* (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
            ;; Recommended keymap prefix on Windows/Linux
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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
(use-package evil)
(use-package evil-surround)
(use-package evil-collection)
(use-package evil-leader)
(use-package evil-commentary)
(with-eval-after-load 'evil
                      (evil-mode 1)
                      (global-evil-surround-mode 1)
                      (evil-collection-init)
                      (global-evil-leader-mode)
                      (evil-leader/set-leader "<SPC>")
                      (evil-leader/set-key
                                  "p"  'projectile-find-file
                                  "a"  'ido-switch-buffer
                                  "oa" 'org-agenda
                                  "oc" 'org-capture
                                  "ol" 'org-store-link
                                  "/"  'projectile-ripgrep
                                  "k" 'kill-buffer)
                      (evil-commentary-mode)
                      (defun meain/evil-yank-advice (orig-fn beg end &rest args)
                        (pulse-momentary-highlight-region beg end)
                        (apply orig-fn beg end args))
                      (advice-add 'evil-yank :around 'meain/evil-yank-advice))
(use-package evil-org
            :after org
            :config
            (add-hook 'org-mode-hook 'evil-org-mode)
            (add-hook
             'evil-org-mode-hook
             (lambda ()
               (evil-org-set-key-theme)))
            (require 'evil-org-agenda)
            (evil-org-agenda-set-keys))
          ;; (straight-use-package
          ;;   '(target-el :type git :host github :repo "noctuid/targets.el"))
          ;; (targets-setup t
          ;;                :last-key 'N
          ;;                :inside-key nil
          ;;                :around-key nil
          ;;                :remote-key nil)
          ;; https://blog.meain.io/2020/emacs-highlight-yanked/

; (use-package devil)
; (global-devil-mode)
; (global-set-key (kbd "C-,") 'global-devil-mode)

(use-package expand-region
             :bind
             ("C-=" . 'er/expand-region))

(use-package magit)

(provide 'init)
