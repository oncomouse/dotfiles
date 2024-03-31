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
(use-package gcmh
  :straight t
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024))
  :hook
  (after-init . (lambda ()
                  (gcmh-mode)
                  (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(use-package diminish :straight t)
(use-package scratch :straight t)
(use-package command-log-mode :straight t)
(use-package hydra :straight t)

(require 'init-frame-hooks)
;; (require 'init-xterm)
(require 'init-themes)

;; (require 'init-osx-keys)
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
(use-package org :straight t)
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
(use-package nginx-mode :straight t)
;; (require 'init-paredit)
(setq sanityinc/lispy-modes-hook '(sanityinc/enable-check-parens-on-save))
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

(use-package sudo-edit :straight t)
(use-package htmlize :straight t)
(when *is-a-mac*
  (use-package osx-location :straight t))

(use-package uptimes
  :straight t
  :init
  (setq-default uptimes-keep-count 200)
  :hook
  (after-init . (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-direnv)

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))




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

;; Local Packages
(require 'init-format)
(require 'init-electric)
(require 'init-crux)
(require 'init-ace-window)
(require 'init-undo)
(require 'init-helpful)
(require 'init-workspaces)
(require 'init-vterm)
(require 'init-avy)
(require 'init-ligature)

;; Repeat isearch with s/r
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

;; Restore default move-dup bindings:
(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

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

;; Indent with C-</>
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(defun ap/countable-wrapper (oldfun &optional c &rest r)
  "Wrap OLDFUN in a loop and perform it C times.  Supply R if provided."
  (interactive "p")
  (dotimes (_ (or c 1))
    (apply oldfun (unless (eq 0 (car (func-arity oldfun))) r))))

(advice-add 'outline-previous-heading :around 'ap/countable-wrapper)
(advice-add 'outline-next-heading :around 'ap/countable-wrapper)
(define-key ap/move-map (kbd "h") 'outline-next-heading)
(define-key ap/move-map (kbd "H") 'outline-previous-heading)
(defvar-keymap outline-heading-repeat-map
  :repeat t
  "h" 'outline-next-heading
  "H" 'outline-previous-heading)

(use-package surround
  :straight t
  :bind-keymap ("M-'" . surround-keymap)
  :bind (:map surround-keymap
              ("*" . (lambda (&rest _) (interactive) (surround-mark "*")))
              ("/" . (lambda (&rest _) (interactive) (surround-mark "/")))))

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)))

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

;; Run markdown in visual line mode:
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; It's not the 80s, emacs.
(setq sentence-end-double-space nil)

(with-eval-after-load 'org
  (define-key ap/leader-open-map (kbd "j") 'org-clock-goto)
  (define-key ap/leader-open-map (kbd "l") 'org-clock-in-last)
  (define-key ap/leader-open-map (kbd "i") 'org-clock-in)
  (define-key ap/leader-open-map (kbd "o") 'org-clock-out)
  (define-key ap/leader-open-map (kbd "a") 'org-agenda)
  (define-key ap/leader-open-map (kbd "c") 'org-capture)

  (setq org-capture-templates
        `(("t" "todo" entry (file+headline "" "Inbox") ; "" => `org-default-notes-file'
           "* TODO %?\n%U\n%i\n")
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "|" "DONE(d!/!)")
                ))
        org-todo-repeat-to-state "TODO")
  (setq org-log-into-drawer nil
        org-agenda-sticky nil)
  ;; Open file links in the same frame:
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
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
    (let (org-log-done org-log-states)  ; turn off logging
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
  (defun ap/shiftcontroldown (oldfunc &optional n)
    "Re-implement 'org-shiftcontroldown' and pass N to it.
If not in a clock, move to next headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (oldfunc n)
      (dotimes (_ n) (outline-forward-same-level))))

  (defun ap/shiftcontrolup (oldfunc &optional n)
    "Re-implement 'org-shiftcontrolup' and pass N to it.
If not in a clock, move to previous headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (oldfunc n)
      (dotimes (_ n) (outline-backward-same-level))))

  (advice-add 'org-shiftcontrolup :around 'ap/shiftcontrolup)
  (advice-add 'org-shiftcontroldown :around 'ap/shiftcontroldown)

  (defun ap/wrap-dotimes (fn)
    "Wrap FN in a dotimes loop to make it repeatable with universal arguments."
    (let ((fn fn)) #'(lambda (&optional c)
                       (interactive "p")
                       (dotimes (_ c) (funcall fn)))))

  (define-key org-mode-map (kbd "M-<up>") (ap/wrap-dotimes 'org-metaup))
  (define-key org-mode-map (kbd "M-<down>") (ap/wrap-dotimes 'org-metadown))
  (define-key org-mode-map (kbd "C-z") 'org-cycle-list-bullet)

  ;; Doom local-leader for org-mode
  (define-key org-mode-map (kbd "C-c #") #'org-update-statistics-cookies)
  (define-key org-mode-map (kbd "C-c l '") #'org-edit-special)
  (define-key org-mode-map (kbd "C-c l *") #'org-ctrl-c-star)
  (define-key org-mode-map (kbd "C-c l +") #'org-ctrl-c-minus)
  (define-key org-mode-map (kbd "C-c l ,") #'org-switchb)
  (define-key org-mode-map (kbd "C-c l .") #'org-goto)
  (define-key org-mode-map (kbd "C-c l @") #'org-cite-insert)
  (define-key org-mode-map (kbd "C-c l A") #'org-archive-subtree-default)
  (define-key org-mode-map (kbd "C-c l e") #'org-export-dispatch)
  (define-key org-mode-map (kbd "C-c l f") #'org-footnote-action)
  (define-key org-mode-map (kbd "C-c l h") #'org-toggle-heading)
  (define-key org-mode-map (kbd "C-c l i") #'org-toggle-item)
  (define-key org-mode-map (kbd "C-c l I") #'org-id-get-create)
  (define-key org-mode-map (kbd "C-c l k") #'org-babel-remove-result)
  ;; (define-key org-mode-map (kbd "C-c l K") #'+org/remove-result-blocks)
  (define-key org-mode-map (kbd "C-c l n") #'org-store-link)
  (define-key org-mode-map (kbd "C-c l o") #'org-set-property)
  (define-key org-mode-map (kbd "C-c l q") #'org-set-tags-command)
  (define-key org-mode-map (kbd "C-c l t") #'org-todo)
  (define-key org-mode-map (kbd "C-c l T") #'org-todo-list)
  (define-key org-mode-map (kbd "C-c l x") #'org-toggle-checkbox)
  (define-key org-mode-map (kbd "C-c l a a") #'org-attach)
  (define-key org-mode-map (kbd "C-c l a d") #'org-attach-delete-one)
  (define-key org-mode-map (kbd "C-c l a D") #'org-attach-delete-all)
  (define-key org-mode-map (kbd "C-c l a f") #'+org/find-file-in-attachments)
  (define-key org-mode-map (kbd "C-c l a l") #'+org/attach-file-and-insert-link)
  (define-key org-mode-map (kbd "C-c l a n") #'org-attach-new)
  (define-key org-mode-map (kbd "C-c l a o") #'org-attach-open)
  (define-key org-mode-map (kbd "C-c l a O") #'org-attach-open-in-emacs)
  (define-key org-mode-map (kbd "C-c l a r") #'org-attach-reveal)
  (define-key org-mode-map (kbd "C-c l a R") #'org-attach-reveal-in-emacs)
  (define-key org-mode-map (kbd "C-c l a u") #'org-attach-url)
  (define-key org-mode-map (kbd "C-c l a s") #'org-attach-set-directory)
  (define-key org-mode-map (kbd "C-c l a S") #'org-attach-sync)
  (define-key org-mode-map (kbd "C-c l b -") #'org-table-insert-hline)
  (define-key org-mode-map (kbd "C-c l b a") #'org-table-align)
  (define-key org-mode-map (kbd "C-c l b b") #'org-table-blank-field)
  (define-key org-mode-map (kbd "C-c l b c") #'org-table-create-or-convert-from-region)
  (define-key org-mode-map (kbd "C-c l b e") #'org-table-edit-field)
  (define-key org-mode-map (kbd "C-c l b f") #'org-table-edit-formulas)
  (define-key org-mode-map (kbd "C-c l b h") #'org-table-field-info)
  (define-key org-mode-map (kbd "C-c l b s") #'org-table-sort-lines)
  (define-key org-mode-map (kbd "C-c l b r") #'org-table-recalculate)
  (define-key org-mode-map (kbd "C-c l b R") #'org-table-recalculate-buffer-tables)
  (define-key org-mode-map (kbd "C-c l b d c") #'org-table-delete-column)
  (define-key org-mode-map (kbd "C-c l b d r") #'org-table-kill-row)
  (define-key org-mode-map (kbd "C-c l b i c") #'org-table-insert-column)
  (define-key org-mode-map (kbd "C-c l b i h") #'org-table-insert-hline)
  (define-key org-mode-map (kbd "C-c l b i r") #'org-table-insert-row)
  (define-key org-mode-map (kbd "C-c l b i H") #'org-table-hline-and-move)
  (define-key org-mode-map (kbd "C-c l b t f") #'org-table-toggle-formula-debugger)
  (define-key org-mode-map (kbd "C-c l b t o") #'org-table-toggle-coordinate-overlays)
  (define-key org-mode-map (kbd "C-c l c c") #'org-clock-cancel)
  (define-key org-mode-map (kbd "C-c l c d") #'org-clock-mark-default-task)
  (define-key org-mode-map (kbd "C-c l c e") #'org-clock-modify-effort-estimate)
  (define-key org-mode-map (kbd "C-c l c E") #'org-set-effort)
  (define-key org-mode-map (kbd "C-c l c g") #'org-clock-goto)
  (define-key org-mode-map (kbd "C-c l c G") (lambda (&rest _) (interactive) (org-clock-goto 'select)))
  ;; (define-key org-mode-map (kbd "C-c l c l") #'+org/toggle-last-clock)
  (define-key org-mode-map (kbd "C-c l c i") #'org-clock-in)
  (define-key org-mode-map (kbd "C-c l c I") #'org-clock-in-last)
  (define-key org-mode-map (kbd "C-c l c o") #'org-clock-out)
  (define-key org-mode-map (kbd "C-c l c r") #'org-resolve-clocks)
  (define-key org-mode-map (kbd "C-c l c R") #'org-clock-report)
  (define-key org-mode-map (kbd "C-c l c t") #'org-evaluate-time-range)
  (define-key org-mode-map (kbd "C-c l c =") #'org-clock-timestamps-up)
  (define-key org-mode-map (kbd "C-c l c -") #'org-clock-timestamps-down)
  (define-key org-mode-map (kbd "C-c l d d") #'org-deadline)
  (define-key org-mode-map (kbd "C-c l d s") #'org-schedule)
  (define-key org-mode-map (kbd "C-c l d t") #'org-time-stamp)
  (define-key org-mode-map (kbd "C-c l d T") #'org-time-stamp-inactive)
  (define-key org-mode-map (kbd "C-c l g c") #'org-clock-goto)
  (define-key org-mode-map (kbd "C-c l g C") (lambda (&rest _) (interactive) (org-clock-goto 'select)))
  (define-key org-mode-map (kbd "C-c l g i") #'org-id-goto)
  (define-key org-mode-map (kbd "C-c l g r") #'org-refile-goto-last-stored)
  ;; (define-key org-mode-map (kbd "C-c l g v") #'+org/goto-visible)
  (define-key org-mode-map (kbd "C-c l g x") #'org-capture-goto-last-stored)
  (define-key org-mode-map (kbd "C-c l l c") #'org-cliplink)
  ;; (define-key org-mode-map (kbd "C-c l l d") #'+org/remove-link)
  (define-key org-mode-map (kbd "C-c l l i") #'org-id-store-link)
  (define-key org-mode-map (kbd "C-c l l l") #'org-insert-link)
  (define-key org-mode-map (kbd "C-c l l L") #'org-insert-all-links)
  (define-key org-mode-map (kbd "C-c l l s") #'org-store-link)
  (define-key org-mode-map (kbd "C-c l l S") #'org-insert-last-stored-link)
  (define-key org-mode-map (kbd "C-c l l t") #'org-toggle-link-display)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "C-c l l g") #'org-mac-link-get-link))
  (define-key org-mode-map (kbd "C-c l P a") #'org-publish-all)
  (define-key org-mode-map (kbd "C-c l P f") #'org-publish-current-file)
  (define-key org-mode-map (kbd "C-c l P p") #'org-publish)
  (define-key org-mode-map (kbd "C-c l P P") #'org-publish-current-project)
  (define-key org-mode-map (kbd "C-c l P s") #'org-publish-sitemap)
  (define-key org-mode-map (kbd "C-c l r") #'org-refile)
  (define-key org-mode-map (kbd "C-c l R") #'org-refile-reverse)
  (define-key org-mode-map (kbd "C-c l s a") #'org-toggle-archive-tag)
  (define-key org-mode-map (kbd "C-c l s b") #'org-tree-to-indirect-buffer)
  (define-key org-mode-map (kbd "C-c l s c") #'org-clone-subtree-with-time-shift)
  (define-key org-mode-map (kbd "C-c l s d") #'org-cut-subtree)
  (define-key org-mode-map (kbd "C-c l s h") #'org-promote-subtree)
  (define-key org-mode-map (kbd "C-c l s j") #'org-move-subtree-down)
  (define-key org-mode-map (kbd "C-c l s k") #'org-move-subtree-up)
  (define-key org-mode-map (kbd "C-c l s l") #'org-demote-subtree)
  (define-key org-mode-map (kbd "C-c l s n") #'org-narrow-to-subtree)
  (define-key org-mode-map (kbd "C-c l s r") #'org-refile)
  (define-key org-mode-map (kbd "C-c l s s") #'org-sparse-tree)
  (define-key org-mode-map (kbd "C-c l s A") #'org-archive-subtree-default)
  (define-key org-mode-map (kbd "C-c l s N") #'widen)
  (define-key org-mode-map (kbd "C-c l s S") #'org-sort)
  (define-key org-mode-map (kbd "C-c l p d") #'org-priority-down)
  (define-key org-mode-map (kbd "C-c l p p") #'org-priority)
  (define-key org-mode-map (kbd "C-c l p u") #'org-priority-up))

(use-package org-appear
  :straight t
  :custom
  (org-appear-trigger 'always)
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  :hook
  (org-mode . org-appear-mode))

(use-package ox-pandoc
  :straight t
  :after ox
  :init
  (add-to-list 'org-export-backends 'pandoc)
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (variable . "revealjs-url=https://revealjs.com"))))

;; Bibliography
;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography (concat dotfiles-seadrive-path "/My Library/Documents/Academic Stuff/library.bib"))

;; Configure org-cite
(setq org-cite-global-bibliography (list reftex-default-bibliography))

(eval-after-load 'reftex-vars
  '(progn
     (setq reftex-cite-format '((?\C-m . "[@%l]")))))
;; Basic markdown citation mapping:
(add-hook 'markdown-mode-hook
	  (lambda () (define-key markdown-mode-map (kbd "C-c @")
				 (lambda ()
				   (interactive)
				   (let ((reftex-cite-format "[@%l]"))
				     (reftex-citation))))))

;; Citar for advanced citation:
(use-package citar
  :straight t
  :after org
  :hook ((org-mode markdown-mode latex-mode) . citar-capf-setup)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :config
  ;; Run `citar-org-update-pre-suffix' after inserting a citation to immediately
  ;; set its prefix and suffix
  (advice-add 'org-cite-insert :after #'(lambda (args)
                                          (save-excursion
                                            (left-char) ; First move point inside citation
                                            (citar-org-update-pre-suffix))))
  :bind
  (:map org-mode-map :package org ("C-c l @" . 'citar-insert-citation)
        :map markdown-mode-map :package markdown ("C-c @" . 'citar-insert-citation)))

(use-package citar-embark
  :straight t
  :after (citar org)
  :diminish citar-embark-mode
  :custom
  (citar-at-point-function 'embark-act)
  :config
  (citar-embark-mode))


;; emacs-lisp local map:
(defvar ap/emacs-lisp-map (make-sparse-keymap))
(define-key emacs-lisp-mode-map (kbd "C-c l") ap/emacs-lisp-map)
(define-key lisp-interaction-mode-map (kbd "C-c l") ap/emacs-lisp-map)
(define-key ap/emacs-lisp-map (kbd "e b") #'eval-buffer)
(define-key ap/emacs-lisp-map (kbd "e d") #'eval-defun)
(define-key ap/emacs-lisp-map (kbd "e e") #'eval-last-sexp)
(define-key ap/emacs-lisp-map (kbd "e r") #'eval-region)
(define-key ap/emacs-lisp-map (kbd "e l") #'load-library)
(define-key ap/emacs-lisp-map (kbd "g f") #'find-function)
(define-key ap/emacs-lisp-map (kbd "g v") #'find-variable)
(define-key ap/emacs-lisp-map (kbd "g l") #'find-library)

(defun ap/project-buffers (&optional sources)
  "Display buffers using consult-buffer, narrowed to only project files.

Pass SOURCES to consult-buffer, if provided."
  (interactive)
  (setq unread-command-events (append unread-command-events (list ?p 32)))
  (consult-buffer sources))

(define-key global-map (kbd "C-x b") 'ap/project-buffers)
(define-key global-map (kbd "C-x B") 'consult-buffer)

;; Configure cape
(defun ap/writing-cape ()
  "Custom completion-at-point-function for use in writing environments."
  (cape-capf-super #'cape-dict #'cape-dabbrev #'cape-keyword))
(defun ap/--set-cape ()
  (add-hook 'completion-at-point-functions #'ap/writing-cape 0 t))
(use-package cape
  :straight t
  :hook ((markdown-mode org-mode text-mode) . ap/--set-cape))

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))
(with-eval-after-load 'paredit
  (diminish 'paredit-mode))
(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode))
(with-eval-after-load 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))
(with-eval-after-load 'smartparens
  (diminish 'smartparens-mode))
(with-eval-after-load 'apheleia
  (diminish 'apheleia-mode))

(require 'init-flyspell)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
