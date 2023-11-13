;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defconst *is-a-mac* (eq system-type 'darwin))
(defvar dotfiles-seadrive-path (cond (*is-a-mac* "~/Library/CloudStorage/SeaDrive-oncomouse(seafile.jetbear.us)/My Libraries") (t "~/SeaDrive/My Libraries")))
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


(setq doom-localleader-key ",")

;; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(use-package! catppuccin-theme
  :init
  (setq catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size (if *is-a-mac* 16 18))
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size (if *is-a-mac* 16 18)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; Line numbers + relative line numbers
(setq display-line-numbers-type 'visual)
;; Disable for the following modes
(dolist (mode '(term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; company configuration
(setq company-idle-delay nil)
(after! company (map!
                 (:map company-active-map
                  :i "RET" nil
                  :i "C-y" 'company-complete-selection)
                 (:i "C-x C-o" 'company-complete)))

;; consult configuration
(after! consult
  (consult-customize
   consult-buffer consult-recent-file consult-buffer consult-ripgrep
   consult-projectile
   :preview-key "M-."))

(map!
 (:leader
  :n "a" 'consult-buffer
  :n "e" 'embark-act
  :n "fp" 'consult-projectile)
 :nvi "C-x C-r" 'consult-recent-file)

;; org configuration
(defun dotfiles/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun dotfiles/org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
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

(after! org
  (setq
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
   org-agenda-span 7
   org-agenda-start-on-weekday 1
   org-agenda-start-day nil))

(add-hook! 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook! 'org-after-todo-statistics-hook 'dotfiles/org-summary-todo)
(add-hook! 'org-checkbox-statistics-hook 'dotfiles/org-checkbox-todo)

(use-package! org-appear
  :custom
  (org-appear-trigger 'manual)
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  :hook
  (org-mode . org-appear-mode)
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
                                       #'org-appear-manual-start
                                       nil
                                       t)
                             (add-hook 'evil-insert-state-exit-hook
                                       #'org-appear-manual-stop
                                       nil
                                       t))))
;; org-mode keys
(map!
 (:n "C-c a" 'org-agenda
  :n "C-c c" 'org-capture
  :n "C-c l" 'org-store-link)
 (:leader
  :desc "Agenda View" :n "oaa" 'org-agenda-list)
 (:after org
         (:map org-mode-map :leader
          :n "oo" 'org-open-at-point
          :n "o*" 'org-toggle-heading
	  :n "oc" 'org-capture
          :desc "Org Refile" "or" 'org-refile)
         (:map org-mode-map
          :i "C-z" 'org-cycle-list-bullet)
         (:map org-mode-map
          :n "cit" 'org-todo)))

(map! :after org-agenda
      :map org-agenda-mode-map
      :nvm "q" 'org-agenda-exit
      :nvm "f" 'org-agenda-later
      :nvm "b" 'org-agenda-earlier)

(after! evil-org
  (evil-define-key 'motion 'org-agenda-mode
    "f" 'org-agenda-later)
  (evil-define-key 'normal 'visual-line-mode
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-define-key 'normal 'evil-org-mode
    "c" 'evil-change))

;; mini.nvim's Bdelete command, which preserves window layout
(after! evil
  (evil-define-command dotfiles/evil-delete-buffer (buffer &optional bang)
    "Delete a buffer.
Don't close any open windows."
    (interactive "<b><!>")
    (with-current-buffer (or buffer (current-buffer))
      (when bang
	(set-buffer-modified-p nil)
	(dolist (process (process-list))
          (when (eq (process-buffer process) (current-buffer))
            (set-process-query-on-exit-flag process nil))))
      (if (and (bound-and-true-p server-buffer-clients)
               (fboundp 'server-edit))
          (server-edit)
        (kill-buffer nil))))
  (evil-ex-define-cmd "Bd[elete]" 'dotfiles/evil-delete-buffer))

;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(after! evil
  (defun evil-collection-unimpaired--move-text (arg)
    "Move text down if ARG is positive, otherwise move text up."
    (cond
     ((and mark-active transient-mark-mode)
      (when (> (point) (mark))
        (exchange-point-and-mark))
      (let ((column (current-column))
            (text (delete-and-extract-region (point) (mark))))
        (forward-line arg)
        (move-to-column column :force)
        (set-mark (point))
        (insert text)
        (exchange-point-and-mark)
        (setq deactivate-mark nil)))
     (t
      (let ((column (current-column)))
        (beginning-of-line)
        (when (or (> arg 0) (not (bobp)))
          (forward-line)
          (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
          (forward-line -1))
        (move-to-column column)))))

  (defun evil-collection-unimpaired-move-text-down (arg)
    "Move region (transient-mark-mode active) or current line ARG lines down."
    (interactive "*p")
    (evil-collection-unimpaired--move-text arg))

  (defun evil-collection-unimpaired-move-text-up (arg)
    "Move region (transient-mark-mode active) or current line ARG lines up."
    (interactive "*p")
    (evil-collection-unimpaired--move-text (- arg)))
  (map!
   (:n "M-j" 'evil-collection-unimpaired-move-text-down
    :n "M-k" 'evil-collection-unimpaired-move-text-up)))

(after! evil
  (evil-ex-define-cmd "Format" 'apheleia-format-buffer))

(after! evil
  (evil-define-operator evil-operator-replace (beg end _ register)
    :move-point nil
    (interactive "<R>")
    (let* ((text (if register
                     (evil-get-register register)
                   (current-kill 0))))

      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text))))
  (map! :n "gr" 'evil-operator-replace))

(after! flycheck
  (flycheck-define-checker lua-selene
    "A lua syntax checker using selene"
    :command ("selene" "--display-style" "quiet" source)
    :enable t
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": warning" (message) line-end)
     (error line-start (file-name) ":" line ":" column ": error" (message) line-end))
    :modes (lua-mode))
  (push 'lua-selene flycheck-checkers))

(map!
 :nv "C-A" 'evil-numbers/inc-at-pt
 :nv "C-c C-A" 'evil-numbers/inc-at-pt
 :nv "C-c C-X" 'evil-numbers/dec-at-pt
 :v "g C-A" 'evil-numbers/inc-at-pt-incremental
 :v "g C-X" 'evil-numbers/dec-at-pt-incremental)

;; Preferred diagnostic mappings:
(map!
 :n "]d" 'next-error
 :n "[d" 'previous-error)
(map!
 :after consult-flycheck
 :leader
 :n "d" 'consult-flycheck)

;; CORFU
(use-package! corfu
  :custom
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-popupinfo-delay (cons nil 1.0))
  :init
  (global-corfu-mode)
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))

(map!
 :after (corfu evil)
 (:i "C-x C-o" 'completion-at-point)
 (:map corfu-map
  :i "SPC" #'corfu-insert-separator
  :i "C-n" #'corfu-next
  :i "C-p" #'corfu-previous
  :i "C-y" #'corfu-insert
  :i "C-c" #'corfu-quit
  :i "M-a" #'corfu-popupinfo-toggle
  :i "C-u" #'corfu-popupinfo-scroll-up
  :i "C-d" #'corfu-popupinfo-scroll-down))

(after! lsp-mode
  (setq lsp-completion-provider :none)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  (add-hook! lsp-completion-mode #'my/lsp-mode-setup-completion)
  )

(use-package! cape)
(map!
 (:prefix "C-x"
  :i "C-t" 'complete-tag
  :i "C-n" 'cape-dabbrev
  :i "C-p" 'cape-abbrev
  :i "C-f" 'cape-file
  :i "C-l" 'cape-line
  :i "C-k" 'cape-dict))
;; END CORFU

;; Add nerd-icons to corfu
(use-package! nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add nerd-icons to completion
(use-package! nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

;; Send C-c with C-c C-c
(after! vterm
  (map! :map vterm-mode-map "C-c C-c" (lambda (_) (interactive "p") (vterm-send "C-c"))))

;; Reload files when the change on disk
(global-auto-revert-mode t)

;; affe provides native-emacs FZF-like searching
(after! affe
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (map!
   :leader
   :nv "ff" 'affe-find))

;; configure additional marginalia settings:
(after! marginalia
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file)
                  (projectile-recentf . project-file)
                  (projectile-switch-to-buffer . buffer))
                marginalia-command-categories))
  (map! :map minibuffer-local-map
        "M-a" 'marginalia-cycle))

;; additional vertico bindings and enable multiform-mode
(after! vertico
  (vertico-multiform-mode)
  (map! :map vertico-map
        "C-u" 'universal-argument
	"M-i" 'vertico-quick-insert
	"C-o" 'vertico-quick-exit
	"M-G" 'vertico-multiform-grid
	"M-F" 'vertico-multiform-flat
	"M-R" 'vertico-multiform-reverse
	"M-U" 'vertico-multiform-unobtrusive))

;; Electric Pair
(electric-pair-mode)
;; Add electric-pairs for major-modes
(defmacro spw/add-mode-pairs (hook pairs)
  `(add-hook ,hook
             (lambda ()
               (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
               (setq-local electric-pair-text-pairs electric-pair-pairs))))
(spw/add-mode-pairs 'emacs-lisp-mode-hook '((?` . ?')))
(spw/add-mode-pairs 'markdown-mode-hook '((?* . ?*)))
(spw/add-mode-pairs 'org-mode-hook '((?* . ?*)))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
