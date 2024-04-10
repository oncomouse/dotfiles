;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package unfill :straight t)

(use-package list-unicode-display :straight t)


;;; Some basic preferences

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)


;; Huge files

(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

(use-package vlf :straight t)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :straight t
  :hook (after-init . mode-line-bell-mode))


;;; Newline behaviour (see also electric-indent-mode, enabled above)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(with-eval-after-load 'subword
  (diminish 'subword-mode))



(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))



(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?┊)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))



(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))


;;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)


(use-package browse-kill-ring
  :straight t
  :custom (browse-kill-ring-separator "\f")
  :bind (("M-Y" . browse-kill-ring)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit)
         ("M-n" . browse-kill-ring-previous)
         ("M-p" . browse-kill-ring-forward))
  :config
  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode)))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

(when (fboundp 'repeat-mode)
  (add-hook 'after-init-hook 'repeat-mode))


;;; Handy key bindings

(with-eval-after-load 'help
  (define-key help-map "A" 'describe-face))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)



;;; Page break lines
(use-package page-break-lines
  :straight t
  :hook (after-init . global-page-break-lines-mode)
  :diminish page-break-lines-mode)


;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

(use-package move-dup
  :straight t
  :bind (([M-S-up] . move-dup-move-lines-up)
         ([M-S-down] . move-dup-move-lines-down)
         ("C-c u" . move-dup-duplicate-up)
         ("C-c d" . move-dup-duplicate-down)))


;;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up



;;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :straight t
  :hook (after-init . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)


;; M-^ is inconvenient, so also bind M-j
(global-set-key (kbd "M-j") 'join-line)


;; Random line sorting
(defun sanityinc/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))


(use-package highlight-escape-sequences
  :straight t
  :hook (after-init . hes-mode))


(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.75)
  :diminish which-key-mode)


(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)

;; smartparens
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

(use-package smartparens
  :straight t
  :hook ((text-mode prog-mode org-mode markdown-mode) . smartparens-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp))
  :config
  (advice-add 'delete-backward-char :after 'dotfiles/delete-org-checkbox)
  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "`" "'"
                   :when '(sp-in-string-quotes-p))
    (sp-local-pair "'" "'" :actions nil))
  (sp-with-modes 'org-mode
    (sp-local-pair "-" " "
                   :when '(sp-point-after-bol-p)
                   :post-handlers '(dotfiles/sp-move-point-right))
    (sp-local-pair "+" "+" ;; TODO: don't pair when inside a date
                   :post-handlers '(dotfiles/sp-handle-bullets))
    (sp-local-pair "[" nil
                   :post-handlers '(dotfiles/sp-handle-checkbox))
    (sp-local-pair "*" "*"
                   :unless '(sp-point-after-word-p
                             sp-point-at-bol-p
                             dotfiles/sp-point-at-headline-p))
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


(use-package surround
  :straight t
  :bind-keymap ("M-'" . surround-keymap)
  :bind (:map surround-keymap
              ("*" . (lambda (&rest _) (interactive) (surround-mark "*")))
              ("/" . (lambda (&rest _) (interactive) (surround-mark "/")))))

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)))

(use-package rainbow-mode
  :straight t
  :hook ((prog-mode) . rainbow-mode)
  :diminish rainbow-mode)

(use-package volatile-highlights
  :straight t
  :diminish volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode))

(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode)
  (add-hook 'prog-mode #'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
