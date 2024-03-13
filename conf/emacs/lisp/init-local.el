;;; init-local.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

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

;;; Code:
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

(when (require-package 'drag-stuff)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(define-key global-map (kbd "C-;") 'embark-act)
(define-key ap/leader-map (kbd "a") nil)
(define-key ap/leader-map (kbd "c") nil)

(define-key ap/leader-open-map (kbd "a") 'org-agenda)
(define-key ap/leader-open-map (kbd "c") 'org-capture)

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

(provide 'init-local)
