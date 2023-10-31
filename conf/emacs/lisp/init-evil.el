; Evil
(setq evil-want-keybinding nil)
(use-package evil
  :diminish evil-mode
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  ;; Use general to define our evil-leader
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fr" 'ap/find-recent-file
   "fF" 'ap/find-file
   "a"  'ap/switch-to-buffer
   "ff" 'projectile-find-file
   "fp" 'projectile-switch-project
   "oa" 'org-agenda
   "oc" 'org-capture
   "ol" 'org-store-link
   "oo" 'org-open-at-point
   "o*" 'org-toggle-heading
   "or" 'org-refile
   "/"  'projectile-ripgrep
   "k"  'kill-buffer)
  (general-define-key
    :states 'insert
    "C-H" 'delete-backward-char
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
   "C-." 'embark-act))
;; Evil bindings for elpaca-ui:
(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui   (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))


(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :config
  (general-define-key
   :states 'normal
   "M-j" 'evil-collection-unimpaired-move-text-down
   "M-k" 'evil-collection-unimpaired-move-text-up)
  (evil-collection-init))
(use-package evil-commentary
       :config   (evil-commentary-mode))
(use-package evil-numbers
  :config
  (general-define-key
   :states '(normal visual)
        "C-c C-A" 'evil-numbers/inc-at-pt
        "C-c C-X" 'evil-numbers/dec-at-pt)
  (general-define-key
   :states 'visual
   "g C-A" 'evil-numbers/inc-at-pt-incremental
   "g C-X" 'evil-numbers/dec-at-pt-incremental))

(use-package evil-org
  :diminish evil-org-mode
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states 'motion
     "q" 'org-agenda-exit
     "f" 'org-agenda-later
     "b" 'org-agenda-earlier))

(provide 'init-evil)
