;; org configuration

(defun ap/set-org-capture-templates(&rest _)
  "(Re)load org-capture templates."
  (interactive "p")
  (setq org-capture-templates
        (doct `(("Personal todo" :keys "t"
                 :icon ("nf-oct-checklist" :set "octicon" :color "green")
                 :file +org-capture-todo-file
                 :prepend t
                 :headline "Inbox"
                 :type entry
                 :template ("* TODO %?"
                            "%U"
                            "%i"))
                ("Denote" :keys "n"
                 :type plain
                 :icon ("nf-fa-sticky_note_o" :set "faicon" :color "green")
                 :file denote-last-path
                 :template denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)
                ("Project" :keys "p"
                 :icon ("nf-oct-repo" :set "octicon" :color "silver")
                 :prepend t
                 :type entry
                 :headline "Inbox"
                 :template ("* %{time-or-todo} %?"
                            "%i"
                            "%a")
                 :file ""
                 :custom (:time-or-todo "")
                 :children (("Project-local todo" :keys "t"
                             :icon ("nf-oct-checklist" :set "octicon" :color "green")
                             :time-or-todo "TODO"
                             :file +org-capture-project-todo-file)
                            ("Project-local note" :keys "n"
                             :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                             :time-or-todo "%U"
                             :file +org-capture-project-notes-file)
                            ("Project-local changelog" :keys "c"
                             :icon ("nf-fa-list" :set "faicon" :color "blue")
                             :time-or-todo "%U"
                             :heading "Unreleased"
                             :file +org-capture-project-changelog-file)))
                ("\tCentralised project templates"
                 :keys "o"
                 :type entry
                 :prepend t
                 :template ("* %{time-or-todo} %?"
                            "%i"
                            "%a")
                 :children (("Project todo"
                             :keys "t"
                             :prepend nil
                             :time-or-todo "TODO"
                             :heading "Tasks"
                             :file +org-capture-central-project-todo-file)
                            ("Project note"
                             :keys "n"
                             :time-or-todo "%U"
                             :heading "Notes"
                             :file +org-capture-central-project-notes-file)
                            ("Project changelog"
                             :keys "c"
                             :time-or-todo "%U"
                             :heading "Unreleased"
                             :file +org-capture-central-project-changelog-file)))))))

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
   +org-capture-todo-file "../inbox.org")
  (require 'org-indent))

(defun ap/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun ap/org-checkbox-todo ()
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

(add-hook! 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook! 'org-after-todo-statistics-hook 'ap/org-summary-todo)
(add-hook! 'org-checkbox-statistics-hook 'ap/org-checkbox-todo)

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
         (:map evil-org-mode-map
          :i "C-z" 'org-cycle-list-bullet
          :n "cit" 'org-todo
          (:localleader
           "&" 'org-mark-ring-goto
           "%" 'org-mark-ring-push))))

(map!
 (:after org
  :map evil-org-mode-map
  :mn "[h" 'org-previous-visible-heading
  :mn "]h" 'org-next-visible-heading
  :mn "[H" 'org-backward-heading-same-level
  :mn "]H" 'org-forward-heading-same-level))

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

(map! :after evil-org
      :mode org-mode
      :localleader
      :prefix "l"
      "o" #'org-open-at-point)

(advice-add 'org-meta-return :after (lambda (&optional _)
                                      (when (eq evil-state 'normal) (evil-append-line 1))))

(after! (:and org)
  (ap/set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (ap/set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))

;; denote.el
(use-package! denote
  :hook (doom-first-buffer . denote-modules-global-mode)
  :config
  (setq denote-known-keywords '("emacs" "nvim" "elisp" "macOS")
        denote-infer-keywords t
        denote-sort-keywords t
        denote-directory (concat dotfiles-seadrive-path "/My Libraries/Todo/org/denote")
        denote-date-prompt-use-org-read-date t))
(after! dired
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))
(after! xref
  (setq xref-search-program 'ripgrep))
(after! org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?"))

(map!
 :after denote
 (:mode (org-mode markdown-mode text-mode)
  :localleader
  "n" nil
  (:prefix ("n" . "denote")
  "N" #'denote-type
  "d" #'denote-date
  "s" #'denote-subdirectory
  "i"  #'denote-link ; "insert" mnemonic
  "I"  #'denote-link-add-links
  "b"  #'denote-link-backlinks
  (:prefix ("f" . "find note")
           "f" #'denote-link-find-file
           "b" #'denote-link-find-backlink)))
 (:leader
  :prefix "n"
  "n" #'denote
  "r" #'denote-rename-file)
 (:map dired-mode-map
       "C-c C-d C-i" #'denote-link-dired-marked-notes
       "C-c C-d C-r" #'denote-dired-rename-marked-files))

(use-package! consult-notes
  :hook (denote-modules-mode . consult-notes-denote-mode)
  :init
  (setq consult-notes-use-rg t
        consult-notes-max-relative-age (* 60 60 24 7)))
(map!
 :after consult-notes
 :leader
 :prefix "n"
 "." #'consult-notes
 ";" #'consult-notes-search-in-all-notes)

(after! org-capture
  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "nerd-icons-" (plist-get declaration :set))))
          (face (intern (concat "nerd-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates)))

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (nerd-icons-octicon "nf-oct-stop" :face 'nerd-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold nerd-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)

(provide 'config-org)
;;; config-org.el ends here
