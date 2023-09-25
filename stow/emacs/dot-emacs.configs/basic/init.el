(defconst *is-a-mac* (eq system-type 'darwin))

; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

; https://stackoverflow.com/questions/29934968/how-to-remove-the-temporary-files-starting-and-ending-with-created-by-emacs
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Set ido to use fuzzy matching
(ido-mode t)
(setq ido-enable-flex-matching t)

; Line numbers + relative line numbers
(display-line-numbers-mode)
(setq display-line-numbers 'relative)

; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

; Fonts
(add-to-list 'default-frame-alist
             '(font . "FiraCode Nerd Font-18"))

; Load Catppuccin
(straight-use-package 'catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)

; Load which-key
(straight-use-package 'which-key)
(which-key-mode)

; Disable visual elements:
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(straight-use-package 'corfu)
(global-corfu-mode)
(with-eval-after-load 'corfu
                      (corfu-popupinfo-mode))

(straight-use-package 'prescient)
(straight-use-package 'corfu-prescient)
(straight-use-package 'ivy-prescient)
(with-eval-after-load 'corfu
                      (corfu-prescient-mode))
(with-eval-after-load 'ivy
                      (ivy-prescient-mode))

(when *is-a-mac*
  (when (straight-use-package 'ns-auto-titlebar)
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

; Ivy + Counsel + Swiper
(straight-use-package 'ivy)
(straight-use-package 'swiper)
(straight-use-package 'counsel)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'projectile-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'projectile-ripgrep)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(straight-use-package 'projectile)
(projectile-mode +1)
;; Recommended keymap prefix on macOS
(when *is-a-mac* (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(straight-use-package 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library.bib"))
(eval-after-load 'reftex-vars
  '(progn 
     (setq reftex-cite-format '((?\C-m . "[@%l]")))))
(add-hook 'markdown-mode-hook
      (lambda () (define-key markdown-mode-map "\C-c["
                   (lambda ()
                     (interactive)
                     (let ((reftex-cite-format "[@%l]"))
                       (reftex-citation))))))

(straight-use-package 'markdown-mode)

; Org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-directory "~/SeaDrive/My Libraries/Todo/org")
(setq org-agenda-files '(
                         "~/SeaDrive/My Libraries/Todo/todo.org"
                         "~/SeaDrive/My Libraries/Todo/inbox.org"
                         "~/SeaDrive/My Libraries/Todo/org"))
(setq org-default-notes-file "~/SeaDrive/My Libraries/Todo/inbox.org")
(setq org-indent-mode "noindent")

; Evil
(setq evil-want-keybinding nil)
(mapcar #'straight-use-package '(evil
                                 evil-surround
                                 evil-collection
                                 evil-leader
                                 evil-commentary))
(straight-use-package
 '(evil-org-mode :type git :host github :repo "Somelauw/evil-org-mode"))
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
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

(straight-use-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)



(provide 'init)
