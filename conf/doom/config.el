(add-to-list 'load-path "~/dotfiles/conf/doom/lisp/")

;; Detect system type:
(defconst *is-a-mac* (eq system-type 'darwin))
(require 'config-secret)

(when (modulep! :editor evil)
  (setq doom-localleader-key ",")
  (setq doom-localleader-alt-key "M-,"))

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
;; (setq doom-theme 'doom-one-light)

;; (set-face-foreground 'org-scheduled-today "#50a14f")
;; (set-face-foreground 'org-scheduled "#50a14f")
;; (set-face-foreground 'org-scheduled-previously "#0184bc")
;; (set-face-foreground 'org-upcoming-deadline "#e45649")
;; (set-face-foreground 'org-todo "#da8548")
;; (set-face-background 'corfu-current "#b332b332b332")

(use-package! catppuccin-theme
  :init
  (setq catppuccin-flavor 'latte)
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

(map!
 :when (not (modulep! :editor evil))
 "C-o" 'ap/open-line
 "C-S-o" 'ap/open-line-above)

;; Use visual line movements in visual-line-mode
(map!
 :when (modulep! :editor evil)
 :after evil
 :mode (visual-line-mode evil-org-mode markdown-mode)
 :n "j" 'evil-next-visual-line
 :n "<down>" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line
 :n "<up>" 'evil-previous-visual-line)

;; Restore yank function:
(map!
 :when (modulep! :editor evil)
 :after evil
 :i "C-S-v" 'yank
 :i "C-y" 'yank)

(map!
 :when (not (modulep! :editor evil))
 :leader
 :prefix "w"
 "`"   '+workspace/other
 "."   '+workspace/display
 "n"   '+workspace/new
 "d"   '+workspace/delete
 "TAB" '+workspace/switch-to)

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

(use-package! ace-window
  :config
  (map!
   "M-o" 'ace-window)
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

;; Window Movements
(map!
 :prefix "M-g"
 "s"   'split-window-below
 "S"   'split-window-right)

(map!
 :when (modulep! :editor evil)
 :after evil
 :nv "C-w S" 'evil-window-vsplit)

;; consult configuration
(after! consult
  (consult-customize
   consult-buffer consult-recent-file consult-buffer consult-ripgrep
   consult-projectile
   :preview-key "M-."))
(map!
 (:leader :prefix "f"
  :desc "Find file in project" :n "p" 'consult-projectile)
 :desc "Find recent files" :nvi "C-x C-r" 'consult-recent-file)

;; Better utilize workspaces
(map!
 (:leader :prefix "TAB" :when (modulep! :editor evil)
  :desc "Switch workspace" "TAB" '+workspace/switch-to
  :desc "Display workspace bar" "." '+workspace/display))

;; <leader>SPC switches buffers, not finds files:
(map! :leader "SPC" '+vertico/switch-workspace-buffer)

;; localleader bindings:
(map!
 :localleader
 ":"  'eval-expression
 ";" 'embark-act)

;; Restore C-d for delete and add backup indentation bindings
(map!
 :when (modulep! :editor evil)
 :after evil-org :map evil-org-mode-map
 :i "C-d" (lambda (&rest _) (interactive "p") (delete-char 1))
 :i "C->" 'org-shiftmetaright
 :i "C-<" 'org-shiftmetaleft)
(map! :i "C-d" (lambda (&rest _) (interactive "p") (delete-char 1))
      :i "C->" 'evil-shift-right-line
      :i "C-<" 'evil-shift-left-line)

(require 'config-org)

;; configure markdown-mode for pandoc:
(after! markdown-mode
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(require 'config-bibliography)

(defun ap/kill-buffer-preserve-layout (&optional buffer bang)
  (interactive)
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

;; Kill buffer and preserve layout
(map! :leader
      :prefix "b"
      "d" 'ap/kill-buffer-preserve-layout)

;; mini.nvim's Bdelete command, which preserves window layout
(after! evil
  (evil-define-command dotfiles/evil-delete-buffer (buffer &optional bang)
                       "Delete a buffer.
Don't close any open windows."
                       (interactive "<b><!>")
                       (ap/kill-buffer-preserve-layout buffer bang))
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
   :when (modulep! :editor evil)
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
  (map! :when (modulep! :editor evil) :n "gr" 'evil-operator-replace-with-register))

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
 :when (modulep! :editor evil)
 :nv "C-A" 'evil-numbers/inc-at-pt
 :nv "C-c C-A" 'evil-numbers/inc-at-pt
 :nv "C-c C-X" 'evil-numbers/dec-at-pt
 :v "g C-A" 'evil-numbers/inc-at-pt-incremental
 :v "g C-X" 'evil-numbers/dec-at-pt-incremental)

(map!
 :when (not (modulep! :editor evil))
 :leader
 :after org
 "C-A" 'org-increase-number-at-point
 "C-X" 'org-decrease-number-at-point)

;; Preferred diagnostic mappings:
(map!
 :when (modulep! :editor evil)
 :n "]d" 'next-error
 :n "[d" 'previous-error)

(map!
 :after consult-flycheck
 :leader
 "d" 'consult-flycheck)

;; (require 'config-vertico)
;; Swap out the version of which-key support found in the embark wiki for the
;; one included with Doom. I think the slowness problem is coming from there.
(after! embark
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
              :around #'embark-hide-which-key-indicator))

(require 'config-corfu)

;; Configure vterm
;; Send C-c with C-c C-c
(after! vterm
  (map! :map vterm-mode-map
        "C-c C-c" (lambda (_) (interactive "p") (vterm-send "C-c"))
        "M-g" nil))

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

(when (modulep! :editor evil)
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
		       "*" nil quote :bind t :hooks (markdown-mode-hook) :keys "i")))
(use-package! cyclekey
  :config
  (setq cyclekey-languages '("Spanish" "German" "French")))
(map!
 :after cyclekey
 :i "M-o" 'cyclekey-cycle)

;; magit
(setq magit-clone-default-directory "~/Projects/")

;; codeium
(use-package! codeium
  :config
  (add-hook! prog-mode (add-hook 'completion-at-point-functions #'codeium-completion-at-point 100 t)))

;; Correctly bind completion-at-point
(after! flyspell
  (map! (:map flyspell-mode-map "C-M-i" nil)
        "C-M-i" 'completion-at-point))

;; Include capf definitions
(after! corfu
  (require 'config-capf))

;; midnight-mode, to run code repeatedly
(use-package! midnight
  :config
  (midnight-mode))

;; Expand-region map
(map!
 (:leader :prefix "m" :when (not (modulep! :editor evil))
          "r" 'er/expand-region
          "s" 'er/mark-sentence
          "w" 'er/mark-word
          "f" 'er/mark-defun
          "p" 'er/mark-paragraph)
 (:prefix "M-m"
  :when (modulep! :editor evil)
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
(map!
 "C-`" 'push-mark-no-activate
 "M-`" 'jump-to-mark)


(require 'config-avy)

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

(map! :prefix "M-g"
      "h" 'ap/outline-next-heading
      "H" 'ap/outline-previous-heading)

(defvar-keymap outline-heading-repeat-map
  :repeat t
  "h" 'ap/outline-next-heading
  "H" 'ap/outline-previous-heading)

(when (not (modulep! :editor evil))
  (use-package! surround
    :bind-keymap ("M-'" . surround-keymap)
    :config
    (map!
     :map surround-keymap
     "*" (lambda (&rest _) (interactive) (surround-mark "*"))
     "/" (lambda (&rest _) (interactive) (surround-mark "/"))
     )))

;; Crux: helpful updates of many commands
(when (not (modulep! :editor evil))
  (use-package! crux
    :config
    (map!
     "C-k" 'crux-smart-kill-line
     "C-o" 'crux-smart-open-line
     "C-S-o" 'crux-smart-open-line-above
     (:leader :prefix "o" "o" 'crux-open-with)
     "C-<backspace>" 'crux-kill-line-backwards
     (:prefix "C-x"
              "C-u" 'crux-upcase-region
              "C-l" 'crux-downcase-region
              "M-c" 'crux-capitalize-region)
     [remap move-beginning-of-line] #'crux-move-beginning-of-line
     [remap kill-whole-line] #'crux-kill-whole-line
     )
    ))

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

(map!
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

(map!
 :when (not (modulep! :editor evil))
 "C-^" 'ap/join-line)

;; Proper help navigation
(map! :map help-map "C-C C-b" 'help-go-back "C-c C-f" 'help-go-forward)

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
