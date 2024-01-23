(add-to-list 'load-path "~/dotfiles/conf/doom/lisp/")

;; Detect system type:
(defconst *is-a-mac* (eq system-type 'darwin))
(require 'config-secret)

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Reload files when the change on disk
(global-auto-revert-mode t)

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
(setq doom-theme 'dichromacy)

;; (use-package! catppuccin-theme
;;   :init
;;   (setq catppuccin-flavor 'mocha)
;;   :config
;;   (load-theme 'catppuccin :no-confirm))

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

;; Enable word count in modeline:
(setq doom-modeline-enable-word-count t)

;; Use visual line movements in visual-line-mode
(map!
 :after evil
 :mode (visual-line-mode evil-org-mode markdown-mode)
 :n "j" 'evil-next-visual-line
 :n "<down>" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line
 :n "<up>" 'evil-previous-visual-line)

;; Restore yank function:
(map!
 :after evil
 :i "C-S-v" 'yank
 :i "C-y" 'yank)

;; tmux-style vsplit
(map!
 :after evil
 :nv "C-w S" 'evil-window-vsplit)

;; consult configuration
(after! consult
  (consult-customize
   consult-buffer consult-recent-file consult-buffer consult-ripgrep
   consult-projectile
   :preview-key "M-."))
(map!
 (:leader
  :desc "Find file in project" :n "fp" 'consult-projectile)
 :desc "Find recent files" :nvi "C-x C-r" 'consult-recent-file)

;; Better utilize workspaces
(map!
 (:leader :prefix "TAB"
  :desc "Switch workspace" "TAB" '+workspace/switch-to
  :desc "Display workspace bar" "." '+workspace/display))

;; localleader bindings:
(map!
 :localleader
 ":"  'eval-expression
 ";" 'embark-act)

;; Restore C-d for delete
(map! :after org :map evil-org-mode-map :i "C-d" (lambda (&rest _) (interactive "p") (delete-char -1)))
(map! :i "C-d" (lambda (&rest _) (interactive "p") (delete-char -1)))
;; Other indentation bindings
(map! :i "C->" 'evil-shift-right-line)
(map! :i "C-<" 'evil-shift-left-line)

(require 'config-org)

;; configure markdown-mode for pandoc:
(after! markdown-mode
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(require 'config-bibliography)

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

;; :Format to format buffer
(after! evil
  (evil-ex-define-cmd "Format" 'apheleia-format-buffer))

;; gs/gS for sorting and reverse sorting:
(after! evil
  ;; https://www.emacswiki.org/emacs/SortWords
  (defun sort-words (reverse beg end)
    "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
    (interactive "*P\nr")
    (sort-regexp-fields reverse "\\w+" "\\&" beg end))
  (defun __sort-lines (reverse beg end)
    (if (= 1 (count-lines beg end)) (sort-words reverse beg end) (sort-lines reverse beg end)))
  (evil-define-operator evil-operator-sort (beg end _)
    (__sort-lines nil beg end))
  (evil-define-operator evil-operator-sort-reverse (beg end _)
    (__sort-lines t beg end))
  (map!
   :n "gs" 'evil-operator-sort
   :n "gS" 'evil-operator-sort-reverse))

;; gr for replace with register
(after! evil
  (evil-define-operator evil-operator-replace-with-register (beg end _ register)
    :move-point nil
    (interactive "<R>")
    (let* ((text (if register
                     (evil-get-register register)
                   (current-kill 0))))

      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text))))
  (map! :n "gr" 'evil-operator-replace-with-register))

(setq flycheck-global-modes '(not org-mode))
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

(require 'config-corfu)

;; Configure vterm
;; Send C-c with C-c C-c
(after! vterm
  (map! :map vterm-mode-map "C-c C-c" (lambda (_) (interactive "p") (vterm-send "C-c"))))

;; configure marginalia:
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
	"M-U" 'vertico-multiform-unobtrusive
        ;; PageUp / PageDown for vertico
        "<next>" (lambda () (interactive) (vertico-next vertico-count))
        "<prior>" (lambda () (interactive) (vertico-previous vertico-count))
        "C-d" (lambda () (interactive) (vertico-next vertico-count))
        "C-u" (lambda () (interactive) (vertico-previous vertico-count))))

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

;; Add nerd-icons to completion
(use-package! nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

;; Configure treesit-auto
(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package! targets
  :config
  (targets-setup t
                 :last-key "N")
  (targets-define-composite-to anyquote
    (("\"" "\"" quote)
     ("'" "'" quote)
     ("`" "`" quote)
     ("“" "”" pair)
     ("‘" "’" pair))
    :bind t
    :keys "q")
  (targets-define-composite-to anyblock
    (("(" ")" pair)
     ("[" "]" pair)
     ("{" "}" pair)
     ("<" ">" pair)
     ("\"" "\"" quote)
     ("'" "'" quote)
     ("`" "`" quote)
     ("“" "”" pair)
     ("‘" "’" pair))
    :bind t
    :hooks (prog-mode-hook)
    :keys "b")
  (targets-define-composite-to anyblock-org
    (("*" "*" quote)
     ("*" "*" quote)
     ("/" "/" quote)
     ("+" "+" quote)
     ("~" "~" quote))
    :bind t
    :hooks (org-mode-hook)
    :keys "b")
  (targets-define-to italics-raw
		     "*" nil quote :hooks (markdown-mode-hook org-mode-hook))
  (targets-define-to italics
		     "*" nil quote :bind t :hooks (markdown-mode-hook) :keys "i"))
(use-package! cyclekey
  :config
  (setq cyclekey-languages '("Spanish" "German" "French")))
(map!
 :after cyclekey
 :i "M-o" 'cyclekey-cycle)

;; magit
(setq magit-clone-default-directory "~/Projects/")

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
