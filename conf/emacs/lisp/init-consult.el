;;; init-consult.el -- Enable vertico + consult
;;; Commentary:
;;; Code:
(use-package vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :straight (vertico :files (:defaults "extensions/*")
                   :includes (vertico-indexed
                              vertico-flat
                              vertico-grid
                              vertico-mouse
                              vertico-quick
                              vertico-buffer
                              vertico-repeat
                              vertico-reverse
                              vertico-directory
                              vertico-multiform
                              vertico-unobtrusive
                              ))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 13)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  (setq vertico-multiform-commands
        '((execute-extended-command flat)))
  :general
  (:states '(normal)
	   "M-." #'vertico-repeat)
  (:keymaps 'vertico-map
	    "TAB" #'vertico-insert
	    "C-M-n" #'vertico-next-group
	    "C-M-p" #'vertico-previous-group
	    "C-d"   #'vertico-scroll-up
	    "C-u"   #'vertico-scroll-down
	    "C-f"   'universal-argument ;; Use C-f (alt universal) here too
	    "<backspace>" #'vertico-directory-delete-char
	    "C-w" #'vertico-directory-delete-word
	    "C-<backspace>" #'vertico-directory-delete-word
	    "RET" #'vertico-directory-enter
	    "M-i" #'vertico-quick-insert
	    "C-o" #'vertico-quick-exit
	    "M-G" #'vertico-multiform-grid
	    "M-F" #'vertico-multiform-flat
	    "M-R" #'vertico-multiform-reverse
	    "M-U" #'vertico-multiform-unobtrusive)
  :config
  (vertico-multiform-mode)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
		 cand))))

(use-package consult
  :straight t
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  :general
  (:states 'normal
   :prefix "SPC")
  ("M-g i" 'consult-imenu ;; Use consult-imenu instead of imenu
   "C-x b" 'consult-buffer
   "C-x C-r" 'consult-recent-file)
  :config
  ;; Disable preview without M-.
  (consult-customize
   consult-buffer consult-recent-file consult-buffer consult-ripgrep
   consult-projectile
   :preview-key "M-."))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-projectile
  :straight t
  :after projectile)

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
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
  :straight t
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package embark
  :straight t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly


  :general
  ("C-;" 'embark-act
   "C-h B" 'embark-bindings)
  :config

  ;; (require 'config-vertico)
  ;; Swap out the version of which-key support found in the embark wiki for the
  ;; one included with Doom. I think the slowness problem is coming from there.
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-remove #'embark-completing-read-prompter
                 #'+vertico--embark-which-key-prompt-a)
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
	         nil
	         (window-parameters (mode-line-format . none)))))


(provide 'init-consult)
;;; init-consult.el ends here
