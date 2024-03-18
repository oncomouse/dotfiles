;;; config-org.el -- org configuration
;;; Commentary:
;;; Code:
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
        `(("t" "todo" entry (file+headline "" "Inbox") ; "" => `org-default-notes-file'
           "* TODO %?\n%U\n%i\n%a" :prepend t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t))
   +org-capture-todo-file "../inbox.org")
  (require 'org-indent))

(defun ap/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise.

N-DONE is the number of done elements; N-NOT-DONE is the number of
not done."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun ap/org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked.

Switch to TODO otherwise"
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
  (when (modulep! :editor evil)
    (add-hook 'org-mode-hook (lambda ()
                               (add-hook 'evil-insert-state-entry-hook
                                         #'org-appear-manual-start
                                         nil
                                         t)
                               (add-hook 'evil-insert-state-exit-hook
                                         #'org-appear-manual-stop
                                         nil
                                         t)))))
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
         (:when (modulep! :editor evil) :map evil-org-mode-map
           :i "C-z" 'org-cycle-list-bullet
           (:localleader
            "&" 'org-mark-ring-goto
            "%" 'org-mark-ring-push))))

(map!
 (:after org
  :when (modulep! :editor evil)
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

(when (modulep! :editor evil)
  (advice-add 'org-meta-return :after (lambda (&optional _)
                                        (when (eq evil-state 'normal) (evil-append-line 1)))))

;; denote.el
(use-package! denote
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

(defun ap/bookmark-before-org-agenda (&rest _)
  "Set a bookmark before opening 'org-agenda', for jumping across workspaces."
  (when (buffer-file-name) (bookmark-set "org-agenda-lastpos"))
  )

(defun ap/jump-to-admin-workspace (&rest _)
  "Move to the admin workspace when opening agenda, rather than open agenda in the current workspace."
  (interactive "p")
  (let ((inhibit-message t))
        (+workspace/switch-to 0)))
  (advice-add 'org-agenda-list :before 'ap/bookmark-before-org-agenda)
  (advice-add 'org-agenda-switch-to :before 'ap/jump-to-admin-workspace)

  (map!
   :when (not (modulep! :editor evil))
   :leader
   :prefix "o"
   "a" 'org-agenda)

  ;; Use C-S-Up/Down to navigate headlines when not using to change clocks
  (defun ap/shiftcontroldown (&optional n)
    "Re-implement 'org-shiftcontroldown' and pass N to it.
If not in a clock, move to next headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (org-shiftcontroldown n)
      (dotimes (_ n) (outline-next-heading))))

  (defun ap/shiftcontrolup (&optional n)
    "Re-implement 'org-shiftcontrolup' and pass N to it.
If not in a clock, move to next headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (org-shiftcontrolup n)
      (dotimes (_ n) (outline-previous-heading))))

  (after! org
    (map! :map org-mode-map
          "C-S-<down>" 'ap/shiftcontroldown
          "C-S-<up>" 'ap/shiftcontrolup))

  (defun ap/wrap-dotimes (fn)
    "Wrap FN in a dotimes loop to make it repeatable with universal arguments."
    (lexical-let ((fn fn)) #'(lambda (&optional c)
                               (interactive "p")
                               (dotimes (_ c) (funcall fn)))))

  (after! org
    (map! :map org-mode-map
          "M-<up>" (ap/wrap-dotimes 'org-metaup)
          "M-<down>" (ap/wrap-dotimes 'org-metadown)))

  (after! org
    (map! :map org-mode-map
          "C-z" 'org-cycle-list-bullet))

  (provide 'config-org)
;;; config-org.el ends here
