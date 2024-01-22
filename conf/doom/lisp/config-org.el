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
   org-beamer-mode t ;; Export to beamer
   org-complete-tags-always-offer-all-agenda-tags t ;; Always use all tags from Agenda files in capture
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
   org-imenu-depth 2
   org-agenda-span 7
   org-agenda-start-on-weekday 1
   org-agenda-start-day nil
   org-capture-templates
   '(("t" "Personal todo" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("n" "Personal notes" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
     ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\n%i\n%a" :prepend t)

     ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
     ;; {todo,notes,changelog}.org file is found in a parent directory.
     ;; Uses the basename from `+org-capture-todo-file',
     ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
     ("p" "Templates for projects")
     ("pt" "Project-local todo" entry  ; {project-root}/todo.org
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("pn" "Project-local notes" entry  ; {project-root}/notes.org
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\n%i\n%a" :prepend t)
     ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
      (file+headline +org-capture-project-changelog-file "Unreleased")
      "* %U %?\n%i\n%a" :prepend t)

     ;; Will use {org-directory}/{+org-capture-projects-file} and store
     ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
     ;; support `:parents' to specify what headings to put them under, e.g.
     ;; :parents ("Projects")
     ("o" "Centralized templates for projects")
     ("ot" "Project todo" entry
      (function +org-capture-central-project-todo-file)
      "* TODO %?\n %i\n %a"
      :heading "Tasks"
      :prepend nil)
     ("on" "Project notes" entry
      (function +org-capture-central-project-notes-file)
      "* %U %?\n %i\n %a"
      :heading "Notes"
      :prepend t)
     ("oc" "Project changelog" entry
      (function +org-capture-central-project-changelog-file)
      "* %U %?\n %i\n %a"
      :heading "Changelog"
      :prepend t))
   +org-capture-todo-file "../inbox.org"))

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
         (:leader
	  :n :desc "Org Capture" "oc" 'org-capture)
         (:map org-mode-map
          :i "C-z" 'org-cycle-list-bullet
          :n "cit" 'org-todo
         (:localleader
          "&" 'org-mark-ring-goto
          "%" 'org-mark-ring-push))))

;; Use Word / libreoffice to bypass doc-view-mode when opening docx files from org:
(after! org
  (add-to-list 'org-file-apps '("\\.docx\\'" . "open %s")))

(map! :after org-agenda
      :map org-agenda-mode-map
      :nvm "q" 'org-agenda-exit
      :nvm "f" 'org-agenda-later
      :nvm "b" 'org-agenda-earlier)

(after! evil-org
  (evil-define-key 'motion 'org-agenda-mode
    "f" 'org-agenda-later)
  (evil-define-key 'normal 'evil-org-mode
    "c" 'evil-change))

(advice-add 'org-meta-return :after (lambda (&optional _)
                                      (when (eq evil-state 'normal) (evil-append-line 1))))

(use-package! org-roam
  :config
  (setq org-roam-directory (concat dotfiles-seadrive-path "/org-roam"))
  (org-roam-db-autosync-mode))

(map! :after org-roam
      (:leader
       (:prefix "o" "r" nil)
       (:prefix ("or" . "org-roam")
                "b" #'org-roam-buffer-toggle
                "f" #'org-roam-node-find
                "c" #'org-roam-capture))
      (:localleader
       (:prefix "l"
                "o" #'org-open-at-point)
       (:prefix ("R" . "org-roam")
                "i" #'org-roam-node-insert)))

(provide 'config-org)
;;; config-org.el ends here
