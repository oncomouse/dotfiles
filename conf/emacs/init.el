;;; init.el -- Custom init file
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/dotfiles/conf/emacs/lisp/")

(defconst dotfiles-data-dir "~/.config/emacs/.local/etc/")

(require 'init-secret)

;; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Line numbers + relative line numbers
(setq display-line-numbers-type 'visual)
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

;; Reload files when the change on disk
(global-auto-revert-mode t)

;; Run a server, so we can open things from the command line:
(server-start)

;; Enable repeat-mode
(repeat-mode)
;; Enable repeat for the mark-ring
(setq set-mark-command-repeat-pop t)

;; Repeat isearch with s/r
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

;; Load straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package general
  :demand t
  :straight t
  :config
  (general-evil-setup))

(use-package hydra
  :straight t)

;; Better (more vim-like) newline with C-o/C-S-o
(defun ap/open-line-above ()
  (interactive)
  (back-to-indentation)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

(defun ap/open-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (indent-according-to-mode))

(general-define-key
 "C-o" 'ap/open-line
 "C-S-o" 'ap/open-line-above)

;; Window Movements
(general-define-key
 :prefix "M-g w"
 "h"   'windmove-left
 "j"   'windmove-down
 "k"   'windmove-up
 "l"   'windmove-right
 "s"   'split-window-below
 "S"   'split-window-right
 "q"   '+workspace/close-window-or-workspace
 "r"   'hydra-window-resizer/body)
(defvar-keymap windmove-repeat-map
  :repeat t
  "h"   'windmove-left
  "j"   'windmove-down
  "k"   'windmove-up
  "l"   'windmove-right)

;; Resize window using hydras
(defhydra hydra-window-resizer (:columns 2)
  "Window Sizing"
  ("-" shrink-window-horizontally "horizontal shrink")
  ("=" enlarge-window-horizontally "horizontal enlarge")
  ("_" shrink-window "vertical shrink")
  ("+" enlarge-window "vertical enlarge"))

(use-package diminish
  :straight t
  :hook (
         (auto-revert-mode . (lambda () (diminish 'auto-revert-mode))))
  :config
  (diminish 'flyspell-mode)
  (diminish 'eldoc-mode))

(use-package no-littering
  :straight t
  :config
  (no-littering-theme-backups))

;; Recent Files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(general-define-key
 "C-x C-r" 'recentf)

					; Fonts
(cond
 (*is-a-mac* (set-frame-font "FiraCode Nerd Font 18"))
 (t (set-frame-font "FiraCode Nerd Font 16")))

;; Other filetype modes:
(use-package markdown-mode :straight t)
(use-package lua-mode :straight t
  :init
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
(use-package json-mode :straight t)
(use-package toml-mode :straight t)
(use-package js2-mode :straight t)
(use-package php-mode :straight t)

					; Load Catppuccin
(use-package catppuccin-theme
  :straight t
  :init
  (setq catppuccin-flavor 'latte)
  (load-theme 'catppuccin :no-confirm))

					; Load which-key
(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
  (which-key-mode))

(require 'init-consult)

(use-package eglot
  :straight t
  :hook
  (
   (lua-mode . (lambda ()
                 (setq flycheck-eglot-exclusive nil)
                 (eglot-ensure)))
   (html-mode . eglot-ensure)
   (css-mode . eglot-ensure)
   (ruby-mode . eglot-ensure)
   (json-mode . eglot-ensure)
   (javascript-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(ruby-mode . ("standardrb" "--lsp"))))

(use-package flycheck
  :straight t
  :init
  (setq flycheck-global-modes '(not org-mode))
  (global-flycheck-mode)
  :general
  (:states 'normal
           "] d" 'flycheck-next-error
           "[ d" 'flycheck-previous-error)
  :config
  (flycheck-define-checker lua-selene
    "A lua syntax checker using selene"
    :command ("selene" "--display-style" "quiet" source)
    :enable t
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": warning" (message) line-end)
     (error line-start (file-name) ":" line ":" column ": error" (message) line-end))
    :modes (lua-mode))
  (push 'lua-selene flycheck-checkers))

(use-package flycheck-eglot
  :straight t
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package consult-flycheck
  :straight t
  :after (flycheck consult)
  :general
  (:states 'normal :prefix "SPC"
           "d" 'consult-flycheck))

;; Format with format-all
(use-package
  :straight t
  format-all)
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "Format" 'format-all-region-or-buffer))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode 1))

;; A few more useful configurations...
(use-package emacs
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

;; Enable prettyify symbols
(global-prettify-symbols-mode)

;; Rainbow delimiters:
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rg :straight t)
(use-package wgrep :straight t)

(use-package projectile
  :straight t
  :diminish projectile-mode
  :general
  (:keymaps 'project-mode-map
            "C-c p"  'projectile-command-map)
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
	  ".clangd")))

(use-package corfu
  :straight t
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
  ("C-M-i" 'completion-at-point)
  (:keymaps 'corfu-map
   "SPC" #'corfu-insert-separator
   "C-n" #'corfu-next
   "C-p" #'corfu-previous
   "C-y" #'corfu-insert
   "C-c" #'corfu-quit
   "M-a" #'corfu-popupinfo-toggle)
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode))

(use-package cape
  :straight t
  :general
  (:states 'insert
   :prefix "C-x"
   "C-t" 'complete-tag
   "C-n" 'cape-dabbrev
   "C-p" 'cape-abbrev
   "C-f" 'cape-file
   "C-l" 'cape-line
   "C-k" 'cape-dict))

(use-package nerd-icons :straight t)
(use-package nerd-icons-corfu
  :straight t
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
  :straight t
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

;; Treesitter
(require 'init-treesitter)

;; Org

(require 'init-org)

;; Writeroom mode for distraction-free writing
(use-package writeroom-mode
  :requires visual-fill-column)

(use-package magit :straight t)

(use-package vterm
  :straight t
  :general
  (:keymaps 'vterm-mode-map
            "M-g" nil
            "C-c C-c" (lambda (_) (interactive "p") (vterm-send "C-c")))
  :config
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after vterm
  :straight t
  :general
  (:prefix "C-c o"
           "t" 'vterm-toggle
           "T" 'vterm-toggle-cd)
  (:keymaps 'vterm-mode-map
            [(control return)] #'vterm-toggle-insert-cd
            "s-n" 'vterm-toggle-forward
            "s-p" 'vterm-toggle-backward))

;; Workspaces
(defun dotfiles/quit-and-restart (&rest _)
  (interactive)
  (tabspaces-save-session)
  (restart-emacs))

(use-package tabspaces
  :straight t
  :hook (tabspaces-mode . +consult-tabspaces-setup)
  :hook (window-setup . tabspaces-session-auto-restore)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-default-tab "*default*")
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  ;; (tabspaces-session-file (concat dotfiles-data-dir "tabspaces/session.el"))
  :general
  (:prefix "C-c q"
    "t" #'tabspaces-save-session
    "T" #'tabspaces-restore-session
    "p" #'tabspaces-save-current-project-session)
  (:prefix "C-c w"
    "TAB" '(tabspaces-switch-or-create-workspace :w "Switch or create")
    "o" '(tabspaces-open-or-create-project-and-workspace :wk "Open or create project")
    "f" '(tabspaces-project-switch-project-open-file :wk "Switch project & open file")
    "d" #'tabspaces-close-workspace
    "b" #'tabspaces-switch-to-buffer
    "t" #'tabspaces-switch-buffer-and-tab
    "C" #'tabspaces-clear-buffers
    ;; "r" #'tabspaces-remove-current-buffer
    "r" #'tab-bar-rename-tab
    "R" #'tabspaces-remove-selected-buffer
    "k" #'(tabspaces-kill-buffers-close-workspace :wk "Kill buffers & close WS"))
  (:prefix "C-c q"
           "r" 'dotfiles/quit-and-restart)
  :config
  (make-directory tabspaces-session-file t)
  ;; Ensure reading project list
  (require 'project)
  (project--ensure-read-project-list)

  (defun +consult-tabspaces-setup ()
    "Deactivate isolated buffers when not using tabspaces."
    (require 'consult)
    (cond (tabspaces-mode
           ;; hide full buffer list (still available with "b")
           (consult-customize consult--source-buffer :hidden t :default nil)
           (add-to-list 'consult-buffer-sources '+consult--source-workspace))
          (t
           ;; reset consult-buffer to show all buffers
           (consult-customize consult--source-buffer :hidden nil :default t)
           (setq consult-buffer-sources (remove #'+consult--source-workspace consult-buffer-sources)))))

  (with-eval-after-load 'consult
    ;; Hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; Set consult-workspace buffer list
    (defvar +consult--source-workspace
      (list :name "Workspace Buffers"
            :narrow   '(?w . "Workspace")
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items
            (lambda ()
              (consult--buffer-query
               :predicate #'tabspaces--local-buffer-p
               :sort      'visibility
               :as        #'buffer-name))))

    (add-to-list 'consult-buffer-sources '+consult--source-workspace))

  ;; Switch to the scratch buffer after creating a new workspace
  (advice-add
   'tabspaces-switch-or-create-workspace :around
   (defun +tabspaces--switch-to-scratch-after-create:around-a (origfn &rest workspace)
     (let ((before-list (tabspaces--list-tabspaces)))
       (apply origfn workspace)
       ;; Created a new empty workspace
       (when-let ((new-ws (cl-set-difference (tabspaces--list-tabspaces) before-list :test #'string=)))
         (+scratch-open-buffer nil nil 'same-window)))))

  (tabspaces-mode 1)

  ;; Rename the first tab to `tabspaces-default-tab'
  (tab-bar-rename-tab tabspaces-default-tab))

(use-package dtrt-indent
  :straight t
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
  :straight t
  :after flyspell
  :general
  (:states 'normal
           "z=" 'flyspell-correct-wrapper))
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-c $") nil))

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
(spw/add-mode-pairs 'org-mode-hook '((?* . ?*)(?/ . ?/)))
;; Match before, after, and around point with regexp:
(defun general-electric/match-before-point (pattern)
  "Compare the line from up to point against pattern using (string-match)"
  (string-match pattern (buffer-substring (line-beginning-position) (point))))
(defun general-electric/match-after-point (pattern)
  "Compare the line after point against pattern using (string-match)"
  (string-match pattern (buffer-substring (point) (line-end-position))))
(defun general-electric/match-around-point (before after)
  "Compare the line before point to before and the line after point using after."
  (and (general-electric/match-after-point after) (general-electric/match-before-point before)))

(defun dotfiles/sp-point-in-org-cookie-p (id action _context)
  "Return t if the point is inside an org-mode statistics cookie."
  (when (eq action 'insert)
    (general-electric/match-around-point (concat "\\[" (regexp-quote id) "$") "^\\]")))

(defun dotfiles/sp-point-at-headline-p (id action _context)
  "Return t if the point is after a set of 0 or more asterisks at the start
of a line (ie. an org-mode headline)."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "^\\**" (regexp-quote id)))))

(defun dotfiles/point-at-org-mode-list-p (id)
  (or
   (general-electric/match-before-point (concat "^\\s-*[0-9]\\.\\s-+" (regexp-quote id) "$"))
   (general-electric/match-before-point (concat "^\\s-*[+-]\\s-+" (regexp-quote id) "$"))))

(defun dotfiles/sp-handle-checkbox (id action _context)
  "When a bracket is inserted after a bullet, create a checkbox and move on."
  (when (and (eq action 'insert) (dotfiles/point-at-org-mode-list-p id))
    (insert " ")
    (right-char 1)
    (insert " ")))

(defun dotfiles/delete-org-checkbox (_arg &optional _killp)
  "Remove the rest of an org-mode checkbox when the closing bracket is removed."
  (when (and
         (eq major-mode 'org-mode)
         (or
          (general-electric/match-before-point (concat "^\\s-*[0-9]\\.\\s-+\\[ $"))
          (general-electric/match-before-point (concat "^\\s-*[+-]\\s-+\\[ $"))))
    (delete-char -2)))

;; TODO: delete bullets

(defun dotfiles/sp-handle-bullets (id action _context)
  (when (and (eq action 'insert) (sp-point-after-bol-p id action _context))
    (delete-char 1)
    (insert " ")))

(defun dotfiles/sp-move-point-right (&rest _r)
  "Move the point right one"
  (right-char 1))

(defun dotfiles/sp-delete (&rest _r)
  "Delete one character after the point"
  (delete-char 1))

(defun dotfiles/sp-handle-org-fraction-cookie (id action _context)
  "If // is inserted inside an org cookie, remove trailing slash and exit cookie."
  (when (and (eq action 'insert)
             (general-electric/match-around-point
              (concat "\\[" (regexp-quote id) "$")
              (concat "^" (regexp-quote id) "\\]")))
    (delete-char 1)
    (right-char 1)))

(after! smartparens
  (advice-add 'delete-backward-char :after 'dotfiles/delete-org-checkbox)

  (sp-with-modes 'org-mode
    (sp-local-pair "-" " "
                   :when '(sp-point-after-bol-p)

                   :post-handlers '(dotfiles/sp-move-point-right))
    (sp-local-pair "+" "+" ;; TODO: don't pair when inside a date
                   :post-handlers '(dotfiles/sp-handle-bullets))
    (sp-local-pair "[" nil
                   :post-handlers '(dotfiles/sp-handle-checkbox))
    (sp-local-pair "*" "*"
                   :unless '(dotfiles/sp-point-at-headline-p))
    (sp-local-pair "~" "~"
                   :unless '(sp-point-after-word-p))
    (sp-local-pair "_" "_"
                   :unless '(sp-point-after-word-p))
    (sp-local-pair "%" " "
                   :when '(dotfiles/sp-point-in-org-cookie-p)
                   :post-handlers '(dotfiles/sp-delete dotfiles/sp-move-point-right))
    (sp-local-pair "/" "/" ;; TODO: insert one slash and move right when inside a cookie
                   :post-handlers '(dotfiles/sp-handle-org-fraction-cookie)
                   :unless '(sp-point-after-word-p)
                   :actions '(insert autoskip wrap navigate))))

(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%c"))
(use-package doom-modeline
  :straight t
  :ensure t
  :init
  (doom-modeline-mode))

(use-package ligature
  :straight t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  ;; (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 't
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
			    "www"
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; (use-package helpful
;;   :general
;;   (:states 'normal
;;    :prefix "C-h"
;;    "f" #'helpful-callable
;;    "v" #'helpful-variable
;;    "k" #'helpful-key
;;    "x" #'helpful-command
;;    "F" #'helpful-function)
;;   (:prefix "C-c"
;;    "C-d" #helpful-at-point))

;; midnight-mode, to run code repeatedly
(use-package midnight
  :straight t
  :config
  (midnight-mode))

(use-package expand-region
  :straight t
  :general
  (:prefix "C-c m"
           "r" 'er/expand-region
           "s" 'er/mark-sentence
           "w" 'er/mark-word
           "f" 'er/mark-defun
           "p" 'er/mark-paragraph))


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

(general-define-key
 "C-`" 'push-mark-no-activate
 "M-`" 'jump-to-mark)

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

(general-define-key :prefix "M-g"
      "h" 'ap/outline-next-heading
      "H" 'ap/outline-previous-heading)

(defvar-keymap outline-heading-repeat-map
  :repeat t
  "h" 'ap/outline-next-heading
  "H" 'ap/outline-previous-heading)

(use-package surround
  :straight t
  :bind-keymap ("M-'" . surround-keymap)
  :config
  (general-define-key
   :keymaps 'surround-keymap
   "*" #'(lambda (&rest _) (interactive) (surround-mark "*"))
   "/" #'(lambda (&rest _) (interactive) (surround-mark "/"))
   ))

(use-package drag-stuff
  :straight t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package crux
  :straight t
  :general
  ("C-k" 'crux-smart-kill-line
  "C-o" 'crux-smart-open-line
  "C-S-o" 'crux-smart-open-line-above)
  (:prefix "C-c o" "o" 'crux-open-with)
  ("C-<backspace>" 'crux-kill-line-backwards)
  (:prefix "C-x"
           "C-u" 'crux-upcase-region
           "C-l" 'crux-downcase-region
           "M-c" 'crux-capitalize-region)
  ([remap move-beginning-of-line] #'crux-move-beginning-of-line
  [remap kill-whole-line] #'crux-kill-whole-line))

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

(general-define-key
 "C-M-s" 'isearch-forward-other-window
 "C-M-r" 'isearch-backward-other-window)

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

(general-define-key
 "C-^" 'ap/join-line)

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
;; lexima
;; mini.surround custom targets
;; statusline
;; Relative line numbering and evil jumping doesn't work
;; hydra?

;;; init.el ends here
