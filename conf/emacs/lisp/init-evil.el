;;; init-evil.el -- Configure evil-mode
;;; Commentary:
;;; Code:
(defvar evil-want-keybinding nil)
(use-package evil
  :diminish evil-mode
  :init
  (setq
   evil-respect-visual-line-mode nil
   evil-undo-system 'undo-redo
   evil-want-C-u-scroll t
   evil-want-Y-yank-to-eol t
   evil-want-C-h-delete t)
  :config
  (evil-mode 1)
  ;; Use general to define our evil-leader
  :general
  (:states 'normal
	   "C-w S" 'evil-window-vsplit
           "C-f" 'universal-argument)
  (:states 'insert
	   "C-e" 'end-of-line)
  (:keymaps 'universal-argument-map
            "C-u" nil
            "C-f" 'universal-argument-more)
  (:states 'normal
	   :prefix "SPC"
	   "ff" 'projectile-find-file
	   "fF" 'ido-find-file
	   "fp" 'consult-projectile
	   "fr" 'consult-recent-file
	   "k"  'kill-buffer
	   "a"  'consult-buffer
	   "i"  'consult-imenu
	   "/"  'consult-ripgrep)
  (:states 'insert
	   "C-S-v" 'evil-paste-after-cursor-after))

;; https://blog.meain.io/2020/emacs-highlight-yanked/
(with-eval-after-load 'evil
  (defun meain/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around 'meain/evil-yank-advice))
;; Enable embark-act instead of evil-repeat-op
(with-eval-after-load 'evil-maps
  (general-define-key
   :states 'normal
   "C-n"   'next-line
   "C-p"   'previous-line
   "C-." 'embark-act))
;; Evil bindings for elpaca-ui:
(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui   (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))

;; mini.nvim's Bdelete command, which preserves window layout
(with-eval-after-load 'evil
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

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :general
  (:states 'normal
	   "M-j" #'evil-collection-unimpaired-move-text-down
	   "M-k" #'evil-collection-unimpaired-move-text-up)
  :init
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config (evil-commentary-mode))
(use-package evil-numbers
  :after evil
  :general
  (:states '(normal visual)
           "C-c C-A" 'evil-numbers/inc-at-pt
           "C-c C-X" 'evil-numbers/dec-at-pt)
  (:states 'visual
	   "g C-A" 'evil-numbers/inc-at-pt-incremental
	   "g C-X" 'evil-numbers/dec-at-pt-incremental))

(use-package evil-org
  :diminish evil-org-mode
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :general
  (:keymaps 'org-agenda-mode-map
	    :states 'motion
	    "q" 'org-agenda-exit
	    "f" 'org-agenda-later
	    "b" 'org-agenda-earlier))
;; (defun dotfiles/evil-append-line (count)
;;   "Use end of line + append to make append-line work correctly"
;;   (interactive "p")
;;   (evil-end-of-line count) (evil-append count))
;; (with-eval-after-load 'evil-org
;;   (advice-add 'evil-org-end-of-line :override 'evil-end-of-line)
;;   (advice-add 'evil-org-append-line :override 'dotfiles/evil-append-line)
;;   (advice-add 'evil-org-beginning-of-line :override 'evil-beginning-of-line))
;; Use visual j/k (but nothing else) when in visual-line-mode
(with-eval-after-load 'evil-org
  (evil-define-key 'normal 'visual-line-mode
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-define-key 'normal 'evil-org-mode
    "c" 'evil-change))

(use-package targets
  :elpaca (targets :host github :repo "noctuid/targets.el")
  :config
  (targets-setup t
                 :last-key "N")
  (targets-define-composite-to anyblock
    (("(" ")" pair)
     ("[" "]" pair)
     ("{" "}" pair)
     ("<" ">" pair)
     ("\"" "\"" quote)
     ("'" "'" quote)
     ("`" "`" quote)
     ("“" "”" quote))
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
		     "*" nil quote :hooks (markdown-mode-hook))
  (targets-define-to italics
		     "*" nil quote :bind t :hooks (markdown-mode-hook) :keys "i"))

;; ii/ai textobjects
(use-package evil-indent-plus
  :after evil
  :general
  (:keymaps 'evil-inner-text-objects-map
            "i" 'evil-indent-plus-i-indent
            "I" 'evil-indent-plus-i-indent-up
            "J" 'evil-indent-plus-i-indent-up-down)
  (:keymaps 'evil-outer-text-objects-map
            "i" 'evil-indent-plus-a-indent
            "I" 'evil-indent-plus-a-indent-up
            "J" 'evil-indent-plus-a-indent-up-down))

(provide 'init-evil)
;;; init-evil.el ends here
