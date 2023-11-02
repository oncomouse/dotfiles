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
  :general
  (:states 'normal
   :prefix "SPC"
   "ff" 'projectile-find-file
   "fF" 'ido-find-file
   "fp" 'consult-projectile
   "oa" 'org-agenda
   "oc" 'org-capture
   "ol" 'org-store-link
   "oo" 'org-open-at-point
   "o*" 'org-toggle-heading
   "or" 'org-refile
   "k"  'kill-buffer
   "fr" 'consult-recent-file
   "fF" 'consult-find
   "a"  'consult-buffer
   "i"  'consult-imenu
   "/"  'consult-ripgrep)
  (:states 'insert
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
  :general
  (:states 'normal
   "M-j" 'evil-collection-unimpaired-move-text-down
   "M-k" 'evil-collection-unimpaired-move-text-up)
  :config (evil-collection-init))
(use-package evil-commentary
       :config (evil-commentary-mode))
(use-package evil-numbers
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
  :after org
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
    :keys "b"))

;; ii/ai textobjects
(use-package evil-indent-plus
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
