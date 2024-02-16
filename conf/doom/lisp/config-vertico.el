;;; config-vertico.el -- Enable vertico + consult
;;; Commentary:
;;; Code:

(defun +vertico/jump-list (jump)
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive
   (let (buffers)
     (require 'consult)
     (unwind-protect
         (list
          (consult--read
           ;; REVIEW Refactor me
           (nreverse
            (delete-dups
             (delq
              nil (mapcar
                   (lambda (mark)
                     (when mark
                       (cl-destructuring-bind (path pt _id) mark
                         (let* ((visiting (find-buffer-visiting path))
                                (buf (or visiting (find-file-noselect path t)))
                                (dir default-directory))
                           (unless visiting
                             (push buf buffers))
                           (with-current-buffer buf
                             (goto-char pt)
                             (font-lock-fontify-region
                              (line-beginning-position) (line-end-position))
                             (format "%s:%d: %s"
                                     (car (cl-sort (list (abbreviate-file-name (buffer-file-name buf))
                                                         (file-relative-name (buffer-file-name buf) dir))
                                                   #'< :key #'length))
                                     (line-number-at-pos)
                                     (string-trim-right (or (thing-at-point 'line) ""))))))))
                   (cddr (better-jumper-jump-list-struct-ring
                          (better-jumper-get-jumps (better-jumper--get-current-context))))))))
           :prompt "jumplist: "
           :sort nil
           :require-match t
           :category 'jump-list))
       (mapc #'kill-buffer buffers))))
  (if (not (string-match "^\\([^:]+\\):\\([0-9]+\\): " jump))
      (user-error "No match")
    (let ((file (match-string-no-properties 1 jump))
          (line (match-string-no-properties 2 jump)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (string-to-number line)))))

(use-package vertico
  :hook (minibuffer-setup . vertico-repeat-save)
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

  :config
  (vertico-multiform-mode)
  (map!
   "M-." #'vertico-repeat
   (:map vertico-map
	 "TAB" #'vertico-insert
	 "C-M-n" #'vertico-next-group
	 "C-M-p" #'vertico-previous-group
	 "M-v"   #'vertico-scroll-up
	 "C-v"   #'vertico-scroll-down
	 "<backspace>" #'vertico-directory-delete-char
	 ;; "C-w" #'vertico-directory-delete-word
	 ;; "C-<backspace>" #'vertico-directory-delete-word
	 "RET" #'vertico-directory-enter
	 "M-i" #'vertico-quick-insert
	 "C-o" #'vertico-quick-exit
	 "M-G" #'vertico-multiform-grid
	 "M-F" #'vertico-multiform-flat
	 "M-R" #'vertico-multiform-reverse
	 "M-U" #'vertico-multiform-unobtrusive))
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
		 cand))))

(use-package! consult
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  :config
  ;; Disable preview without M-.
  (consult-customize
   consult-buffer consult-recent-file consult-buffer consult-ripgrep
   consult-projectile
   :preview-key "M-."))

(defun +vertico-embark-which-key-indicator ()
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

(use-package! embark
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)
  (after! which-key
    (defadvice! +vertico--embark-which-key-prompt-a (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      :around #'embark-completing-read-prompter
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))
    (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(map!
 "C-;" 'embark-act)

;; Consult users will also want the embark-consult package.
(use-package! embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package! consult-projectile
  :after projectile)

(use-package! orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))


(use-package! marginalia
  :init
  (marginalia-mode)
  :config
  (advice-add #'marginalia--project-root :override #'doom-project-root)
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file)
                  (projectile-recentf . project-file)
                  (projectile-switch-to-buffer . buffer)
                  (+default/find-file-under-here . file)
                  (doom/find-file-in-emacsd . project-file)
                  (doom/find-file-in-other-project . project-file)
                  (doom/find-file-in-private-config . file)
                  (doom/describe-active-minor-mode . minor-mode)
                  (flycheck-error-list-set-filter . builtin)
                  (persp-switch-to-buffer . buffer)
                  (projectile-find-file . project-file)
                  (projectile-recentf . project-file)
                  (projectile-switch-to-buffer . buffer)
                  (projectile-switch-project . project-file))
                marginalia-command-categories))
  (map! :map (completions-list-mode-map minibuffer-local-map)
	"M-a" 'marginalia-cycle))

(use-package! nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package! wgrep
  :config (setq wgrep-auto-save-buffer t))

(provide 'config-vertico)
;;; config-vertico.el ends here
