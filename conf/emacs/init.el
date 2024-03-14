;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Auto generate site-lisp directory:
(let ((site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
  (unless (file-directory-p site-lisp-dir)
    (make-directory site-lisp-dir)))

;; Update purcell's configuration:
(defun ap/update-purcell ()
  "Update purcell/emacs.d on this computer, if possible"
  (interactive)
  (start-process-shell-command "Updating purcell/emacs.d" (get-buffer-create "*git log*")
                               (format "cd %s && git pull" (expand-file-name "purcell-emacs" user-emacs-directory))))

;; Load purcell's emacs config:
(let ((purcell-path (expand-file-name "purcell-emacs" user-emacs-directory)))
  (unless (file-directory-p purcell-path) (call-process-shell-command
                                           (format "git clone https://github.com/purcell/emacs.d %s" purcell-path)))
  (add-to-list 'load-path (format "%s/lisp" purcell-path)))

;; Set the location for our custom configuration:
(add-to-list 'load-path  "~/dotfiles/conf/emacs/lisp")

(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bootstrap config

;; Line numbers + relative line numbers
(setq display-line-numbers-type 'visual)
;; Disable for the following modes
(dolist (mode '(term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
;; (require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-straight)
(require 'init-exec-path) ;; Set up $PATH

;; General performance tuning
(when (require-package 'gcmh)
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode)
                               (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)

(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
;; (require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
;; (require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
;; (require 'init-erlang)
(require 'init-javascript)
;; (require 'init-php)
(require 'init-org)
;; (require 'init-nxml)
(require 'init-html)
(require 'init-css)
;; (require 'init-haml)
(require 'init-http)
(require 'init-python)
;; (require 'init-haskell)
;; (require 'init-elm)
;; (require 'init-purescript)
;; (require 'init-ruby)
;; (require 'init-rails)
;; (require 'init-sql)
;; (require 'init-ocaml)
;; (require 'init-j)
;; (require 'init-nim)
;; (require 'init-rust)
;; (require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
;; (require 'init-terraform)
;; (require 'init-nix)
(maybe-require-package 'nginx-mode)
;; (maybe-require-package 'just-mode)
;; (maybe-require-package 'justl)

(require 'init-paredit)
(require 'init-lisp)
;; (require 'init-sly)
;; (require 'init-clojure)
;; (require 'init-clojure-cider)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)
;; (require 'init-dash)

;; (require 'init-ledger)
(require 'init-lua)
;; (require 'init-uiua)
;; (require 'init-terminals)

;; Extra packages which don't require any configuration

(require-package 'sudo-edit)
;; (require-package 'gnuplot)
(require-package 'htmlize)
(when *is-a-mac*
  (require-package 'osx-location))
;; (maybe-require-package 'dotenv-mode)
;; (maybe-require-package 'shfmt)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-direnv)

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))

(require 'init-ligature)



;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)


;; Local

(require 'init-secret)

;; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Set font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140)

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(require-package 'hydra)

(defvar ap/leader-map (make-sparse-keymap))
(define-key global-map (kbd "C-c") ap/leader-map)

(defvar ap/localleader-map (make-sparse-keymap))
(define-key global-map (kbd "C-c l") ap/localleader-map)

(defvar ap/move-map (make-sparse-keymap))
(define-key global-map (kbd "M-g") ap/move-map)

(defvar ap/leader-open-map (make-sparse-keymap))
(define-key ap/leader-map (kbd "o") ap/leader-open-map)

(define-key global-map (kbd "C-c c") nil)
(define-key global-map (kbd "C-c a") nil)

(defvar ap/leader-code-map (make-sparse-keymap))
(define-key ap/leader-map (kbd "c") ap/leader-code-map)

;; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Reload files when the change on disk
(global-auto-revert-mode t)

;; Enable repeat-mode
(repeat-mode)
;; Enable repeat for the mark-ring
(setq set-mark-command-repeat-pop t)

;; Repeat isearch with s/r
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

;; Resize window using hydras
(defhydra hydra-window-resizer (:columns 2)
  "Window Sizing"
  ("-" shrink-window-horizontally "horizontal shrink")
  ("=" enlarge-window-horizontally "horizontal enlarge")
  ("_" shrink-window "vertical shrink")
  ("+" enlarge-window "vertical enlarge"))

(defun aw-window-resize (window)
  (aw-switch-to-window window)
  (hydra-window-resizer/body))

(when (require-package 'ace-window)
  (define-key global-map (kbd "M-o") 'ace-window)
  (define-key global-map (kbd "C-x o") 'other-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?B aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?r aw-window-resize "Resize Window")
          (?? aw-show-dispatch-help))))

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
(setq enable-recursive-minibuffers t)

(require 'init-format)
(require 'init-electric)

;; Restore default move-dup bindings:
(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

(define-key global-map (kbd "C-;") 'embark-act)

(define-key sanityinc/org-global-prefix-map (kbd "a") 'org-agenda)
(define-key sanityinc/org-global-prefix-map (kbd "c") 'org-capture)

(when (require-package 'crux)
  (define-key global-map (kbd "C-k") 'crux-smart-kill-line)
  (define-key global-map (kbd "C-o") 'crux-smart-open-line)
  (define-key global-map (kbd "C-S-o") 'crux-smart-open-line-above)
  (define-key ap/leader-open-map (kbd "o") 'crux-open-with)
  (define-key global-map (kbd "C-<backspace>") 'crux-kill-line-backwards)
  (define-key ctl-x-map (kbd "C-u") 'crux-upcase-region)
  (define-key ctl-x-map (kbd "C-l") 'crux-downcase-region)
  (define-key ctl-x-map (kbd "M-c") 'crux-capitalize-region)
  (keymap-set global-map "<remap> <move-beginning-of-line>" #'crux-move-beginning-of-line)
  (keymap-set global-map "<remap> <kill-whole-line>" #'crux-kill-whole-line))

;; Use isearch in other windows
(defun isearch-forward-other-window (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix -1 1)))
        (other-window next)
        (isearch-forward)
        (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-r") 'isearch-backward-other-window)

(defun ap/join-line (&optional c)
  "Vim-style join-line, that merges lines to the end of the line at point.

If mark is active, merge lines in the current region."
  (interactive "p")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1))))
  (dotimes (_ c)
    (join-line t)))

(define-key global-map (kbd "C-^") 'ap/join-line)

;; Transient-mark-mode supporting push-mark
;; Source: https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(define-key global-map (kbd "C-`") 'push-mark-no-activate)
(define-key global-map (kbd "M-`") 'jump-to-mark)

(require 'init-avy)

;; Navigate headings in the outline with universal arguments
(defun ap/outline-next-heading (&optional c)
  "Interactive, count-able heading motion"
  (interactive "p")
  (dotimes (_ c)
    (outline-next-heading)))

(defun ap/outline-previous-heading (&optional c)
  "Interactive, count-able heading motion"
  (interactive "p")
  (dotimes (_ c)
    (outline-previous-heading)))

(define-key ap/move-map (kbd "h") 'ap/outline-next-heading)
(define-key ap/move-map (kbd "H") 'ap/outline-previous-heading)
(defvar-keymap outline-heading-repeat-map
  :repeat t
  "h" 'ap/outline-next-heading
  "H" 'ap/outline-previous-heading)

(when (require-package 'surround)
  (surround-make-keymap)
  (define-key global-map (kbd "M-'") surround-keymap)
  (define-key surround-keymap (kbd "*") (lambda (&rest _) (interactive) (surround-mark "*")))
  (define-key surround-keymap (kbd "/") (lambda (&rest _) (interactive) (surround-mark "/"))))

(when (require-package 'vterm)
  (add-hook 'vterm-mode-hook (lambda () ""
                               (define-key vterm-mode-map "M-g" nil)
                               (define-key vterm-mode-map "C-c C-c" (lambda (_) (interactive "p") (vterm-send "C-c")))))
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000))

(when (require-package 'vterm-toggle)
  (define-key ap/leader-open-map (kbd "t") 'vterm-toggle)
  (define-key ap/leader-open-map (kbd "T") 'vterm-toggle-cd)
  (add-hook 'vterm-mode-hook (lambda () ""
                               (define-key vterm-mode-map (kdb "s-n") 'vterm-toggle-forward)
                               (define-key vterm-mode-map (kdb "s-p") 'vterm-toggle-backward))))

;; Patch for fish and ripgrep
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

;; Doom's version control menu
(defvar ap/vc-map (make-sparse-keymap))
(define-key ap/leader-map (kbd "v") ap/vc-map)
(define-key ap/vc-map (kbd "R") #'vc-revert)

(define-key ap/vc-map (kbd "r") #'diff-hl-revert-hunk)
(define-key ap/vc-map (kbd "s") #'diff-hl-stage-current-hunk)
(define-key ap/vc-map (kbd "t") #'git-timemachine-toggle)
(define-key ap/vc-map (kbd "n") #'diff-hunk-next)
(define-key ap/vc-map (kbd "p") #'diff-hunk-prev)

(define-key ap/vc-map (kbd "/") #'magit-dispatch)
(define-key ap/vc-map (kbd ".") #'magit-file-dispatch)
(define-key ap/vc-map (kbd "'") #'forge-dispatch)
(define-key ap/vc-map (kbd "g") #'magit-status)
(define-key ap/vc-map (kbd "G") #'magit-status-here)
(define-key ap/vc-map (kbd "x") #'magit-file-delete)
(define-key ap/vc-map (kbd "B") #'magit-blame-addition)
(define-key ap/vc-map (kbd "C") #'magit-clone)
(define-key ap/vc-map (kbd "F") #'magit-fetch)
(define-key ap/vc-map (kbd "L") #'magit-log-buffer-file)
(define-key ap/vc-map (kbd "S") #'magit-stage-file)
(define-key ap/vc-map (kbd "U") #'magit-unstage-file)
(define-key ap/vc-map (kbd "f f") #'magit-find-file)
(define-key ap/vc-map (kbd "f g") #'magit-find-git-config-file)
(define-key ap/vc-map (kbd "f c") #'magit-show-commit)
(define-key ap/vc-map (kbd "f i") #'forge-visit-issue)
(define-key ap/vc-map (kbd "f p") #'forge-visit-pullreq)

(define-key ap/vc-map (kbd "o .") #'+vc/browse-at-remote)
(define-key ap/vc-map (kbd "o h") #'+vc/browse-at-remote-homepage)
(define-key ap/vc-map (kbd "o r") #'forge-browse-remote)
(define-key ap/vc-map (kbd "o c") #'forge-browse-commit)
(define-key ap/vc-map (kbd "o i") #'forge-browse-issue)
(define-key ap/vc-map (kbd "o p") #'forge-browse-pullreq)
(define-key ap/vc-map (kbd "o I") #'forge-browse-issues)
(define-key ap/vc-map (kbd "o P") #'forge-browse-pullreqs)

(define-key ap/vc-map (kbd "l r") #'magit-list-repositories)
(define-key ap/vc-map (kbd "l s") #'magit-list-submodules)
(define-key ap/vc-map (kbd "l i") #'forge-list-issues)
(define-key ap/vc-map (kbd "l p") #'forge-list-pullreqs)
(define-key ap/vc-map (kbd "l n") #'forge-list-notifications)
(define-key ap/vc-map (kbd "c r") #'magit-init)
(define-key ap/vc-map (kbd "c R") #'magit-clone)
(define-key ap/vc-map (kbd "c c") #'magit-commit-create)
(define-key ap/vc-map (kbd "c f") #'magit-commit-fixup)
(define-key ap/vc-map (kbd "c i") #'forge-create-issue)
(define-key ap/vc-map (kbd "c p") #'forge-create-pullreq)


(with-eval-after-load 'org
  (setq org-capture-templates
        `(("t" "todo" entry (file+headline "" "Inbox")  ; "" => `org-default-notes-file'
           "* TODO %?\n%U\n%i\n")
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ))
  (setq
   org-beamer-mode t ;; Export to beamer
   org-complete-tags-always-offer-all-agenda-tags t ;; Always use all tags from Agenda files in capture
   org-hide-leading-stars nil
   org-startup-indented nil
   org-directory (concat dotfiles-seadrive-path "/Todo/org")
   org-agenda-files (list
		     (concat dotfiles-seadrive-path "/Todo/todo.org")
		     (concat dotfiles-seadrive-path "/Todo/inbox.org"))
   org-default-notes-file (concat dotfiles-seadrive-path "/Todo/inbox.org")
   org-indent-mode "noindent"
   org-refile-targets
   '((nil :maxlevel . 2)
     (org-agenda-files :maxlevel . 2))
   org-imenu-depth 2
   org-agenda-span 7
   org-agenda-start-on-weekday 1
   org-agenda-start-day nil)
  (defun ap/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise.

N-DONE is the number of done elements; N-NOT-DONE is the number of
not done."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (defun ap/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked.

Switch to TODO otherwise"
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (save-excursion
	  (org-back-to-heading t)
	  (setq beg (point))
	  (end-of-line)
	  (setq end (point))
	  (goto-char beg)
	  (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
			         end t)
              (if (match-end 1)
		  (if (equal (match-string 1) "100%")
		      (unless (string-equal todo-state "DONE")
		        (org-todo 'done))
		    (unless (string-equal todo-state "TODO")
		      (org-todo 'todo)))
	        (if (and (> (match-end 2) (match-beginning 2))
		         (equal (match-string 2) (match-string 3)))
		    (unless (string-equal todo-state "DONE")
		      (org-todo 'done))
		  (unless (string-equal todo-state "TODO")
		    (org-todo 'todo)))))))))

  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'org-after-todo-statistics-hook 'ap/org-summary-todo)
  (add-hook 'org-checkbox-statistics-hook 'ap/org-checkbox-todo)
  (add-to-list 'org-file-apps '("\\.docx\\'" . "open %s"))
  (defun ap/bookmark-before-org-agenda (&rest _)
    "Set a bookmark before opening 'org-agenda', for jumping across workspaces."
    (when (buffer-file-name) (bookmark-set "org-agenda-lastpos"))
    )

  (defun ap/jump-to-admin-workspace (&rest _)
    "Move to the admin workspace when opening agenda, rather than open agenda in the current workspace."
    (interactive "p")
    (let ((inhibit-message t))
      ;; (+workspace/switch-to 0)
      ))
  (advice-add 'org-agenda-list :before 'ap/bookmark-before-org-agenda)
  (advice-add 'org-agenda-switch-to :before 'ap/jump-to-admin-workspace)

  ;; Use C-S-Up/Down to navigate headlines when not using to change clocks
  (defun ap/shiftcontroldown (&optional n)
    "Re-implement 'org-shiftcontroldown' and pass N to it.
If not in a clock, move to next headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (org-shiftcontroldown n)
      (dotimes (_ n) (outline-next-heading))))

  (defun ap/shiftcontrolup (&optional n)
    "Re-implement 'org-shiftcontrolup' and pass N to it.
If not in a clock, move to next headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (org-shiftcontrolup n)
      (dotimes (_ n) (outline-previous-heading))))

  (define-key org-mode-map (kbd "C-S-<down>") 'ap/shiftcontroldown)
  (define-key org-mode-map (kbd "C-S-<up>") 'ap/shiftcontrolup)

  (defun ap/wrap-dotimes (fn)
    "Wrap FN in a dotimes loop to make it repeatable with universal arguments."
    (lexical-let ((fn fn)) #'(lambda (&optional c)
                               (interactive "p")
                               (dotimes (_ c) (funcall fn)))))

  (define-key org-mode-map (kbd "M-<up>") 'org-metaup)
  (define-key org-mode-map (kbd "M-<up>") 'org-metaup)
  (define-key org-mode-map (kbd "C-z") 'org-cycle-list-bullet)
  )


(when (require-package 'helpful)
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

;; Source: https://github.com/Wilfred/helpful/issues/250
(defvar *helpful-buffer-ring-size 5
  "How many buffers are stored for use with `*helpful-next'.")

(defvar *helpful--buffer-ring (make-ring *helpful-buffer-ring-size)
  "Ring that stores the current Helpful buffer history.")

(defun *helpful--buffer-index (&optional buffer)
  "If BUFFER is a Helpful buffer, return it’s index in the buffer ring."
  (let ((buf (or buffer (current-buffer))))
    (and (eq (buffer-local-value 'major-mode buf) 'helpful-mode)
         (seq-position (ring-elements *helpful--buffer-ring) buf #'eq))))

(advice-add 'helpful--buffer
            :filter-return (lambda (help-buf)
                             (let ((buf-ring *helpful--buffer-ring))
                               (let ((newer-buffers (or (*helpful--buffer-index) 0)))
                                 (dotimes (_ newer-buffers) (ring-remove buf-ring 0)))
                               (when (/= (ring-size buf-ring) *helpful-buffer-ring-size)
                                 (ring-resize buf-ring *helpful-buffer-ring-size))
                               (ring-insert buf-ring help-buf))))

(defun *helpful--next (&optional buffer)
  "Return the next live Helpful buffer relative to BUFFER."
  (let ((buf-ring *helpful--buffer-ring)
        (index (or (*helpful--buffer-index buffer) -1)))
    (cl-block nil
      (while (> index 0)
        (cl-decf index)
        (let ((buf (ring-ref buf-ring index)))
          (if (buffer-live-p buf) (cl-return buf)))
        (ring-remove buf-ring index)))))

(defun *helpful--previous (&optional buffer)
  "Return the previous live Helpful buffer relative to BUFFER."
  (let ((buf-ring *helpful--buffer-ring)
        (index (1+ (or (*helpful--buffer-index buffer) -1))))
    (cl-block nil
      (while (< index (ring-length buf-ring))
        (let ((buf (ring-ref buf-ring index)))
          (if (buffer-live-p buf) (cl-return buf)))
        (ring-remove buf-ring index)))))

(defun *helpful-next ()
  "Go to the next Helpful buffer."
  (interactive)
  (when-let (buf (*helpful--next))
    (funcall helpful-switch-buffer-function buf)))

(defun *helpful-previous ()
  "Go to the previous Helpful buffer."
  (interactive)
  (when-let (buf (*helpful--previous))
    (funcall helpful-switch-buffer-function buf)))

(define-key helpful-mode-map (kbd "C-x C-b") '*helpful-previous)
(define-key helpful-mode-map (kbd "C-x C-f") '*helpful-next)

;; Pop-up support for documentation windows
(add-to-list 'display-buffer-alist
             '("^\\*\\([Hh]elp\\|Apropos\\|info\\)" (display-buffer-reuse-window display-buffer-in-side-window)
               (quit . t)
               (transient . t)
               (side . bottom)
               (slot . 0)
               (window-height . 0.33)))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
