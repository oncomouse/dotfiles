;;; init-org.el -- Configuration for org-mode
;;; Commentary:
;;; Code:
(use-package org
  :elpaca nil
  :hook
  (org-mode . turn-on-visual-line-mode)
  (org-after-todo-statistics . dotfiles/org-summary-todo)
  (org-checkbox-statistics . dotfiles/org-checkbox-todo)
  :general
  (:prefix "C-c"
	   "a" 'org-agenda
	   "c" 'org-capture
	   "l" 'org-store-link)
  (:states 'normal
	   :prefix "SPC"
	   "oa" 'org-agenda
	   "oc" 'org-capture
	   "ol" 'org-store-link)
  (:states 'normal
	   :prefix "SPC"
	   :keymaps 'org-mode-map
	   "oo" 'org-open-at-point
	   "o*" 'org-toggle-heading
	   "or" 'org-refile)
  (:states 'insert
	   :keymaps 'org-mode-map
	   "C-z" 'org-cycle-list-bullet)
  (:states 'normal
	   :keymaps 'org-mode-map
	   "cit" 'org-todo)
  :init
  (setq-default org-pretty-entities t
		org-use-sub-superscripts "{}"
		org-hide-emphasis-markers t
		org-startup-with-inline-images t
		org-image-actual-width '(300))
  (setq org-directory (concat dotfiles-seadrive-path "/Todo/org")
	org-agenda-files (list
			  (concat dotfiles-seadrive-path "/Todo/todo.org")
			  (concat dotfiles-seadrive-path "/Todo/inbox.org"))
	org-default-notes-file (concat dotfiles-seadrive-path "/Todo/inbox.org")
	org-indent-mode "noindent"
	org-refile-targets
	'((nil :maxlevel . 2)
	  (org-agenda-files :maxlevel . 2))))

(use-package org-appear
  :custom
  (org-appear-trigger 'manual)
  (org-appear-autolinks t)
  (org-appear-autoentities t)
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

(provide 'init-org)
;;; init-org.el ends here
