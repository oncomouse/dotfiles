;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:
(defvar ap/org-mode-local-map (make-sparse-keymap))

(use-package org
  :straight t
  :hook ((org-agenda-mode . hl-line-mode)
         (org-agenda-mode . (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
         (org-mode .   turn-on-visual-line-mode))
  :init
  (setq org-log-done t
        org-catch-invisible-edits 'show
        org-export-coding-system 'utf-8
        org-fast-tag-selection-single-key 'expert
        org-html-validation-link nil
        org-export-kill-product-buffer-when-displayed t
        org-tags-column 80
        org-beamer-mode t ;; Export to beamer
        org-complete-tags-always-offer-all-agenda-tags t ;; Always use all tags from Agenda files in capture
        org-hide-leading-stars nil
        org-startup-indented nil
        org-indent-mode "noindent"
        org-imenu-depth 2
        org-support-shift-select t
        org-refile-use-cache nil
        org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        ;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes 'confirm
        org-todo-keywords (quote ((sequence "TODO(t" "|" "DONE(d!/!)")))
        org-todo-repeat-to-state "TODO"
        org-agenda-compact-blocks t
        org-agenda-start-on-weekday 1
        org-agenda-start-day nil
        org-agenda-include-diary nil
        org-agenda-sorting-strategy '((agenda habit-down time-up user-defined-up effort-up category-keep
                                              (todo category-up effort-up)
                                              (tags category-up effort-up)
                                              (search category-up)))
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)
        org-agenda-window-setup 'current-window
        org-clock-persist t
        org-clock-in-resume t
        ;; Save clock data and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;; Removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;; Show clock sums as hours and minutes, not "n days" etc.
        org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archive")

  (setq org-directory (concat dotfiles-seadrive-path "/Todo/org")
        org-default-notes-file (concat dotfiles-seadrive-path "/Todo/inbox.org"))
  (setq org-agenda-files (list
                          (concat dotfiles-seadrive-path "/Todo/todo.org")
                          (concat dotfiles-seadrive-path "/Todo/inbox.org")))

  (setq org-capture-templates
        `(("t" "todo" entry (file+headline "" "Inbox" ; "" => `org-default-notes-file'
                                           "* TODO %?\n%T\n%i\n")
           ("n" "note" entry (file "")
            "* %? :NOTE:\n%T\n%a\n" :clock-resume t)
           )))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map ap/leader-open-map
         ("j" . org-clock-goto)
         ("l" . org-clock-in-last)
         ("i" . org-clock-in)
         ("o" . org-clock-out)
         ("a" . org-agenda)
         ("c" . org-capture)
         :map org-mode-map
         ("C-M-<up>" . org-up-element)
         ("C-z" . org-cycle-list-bullet)
         ("C-c #" . org-update-statistics-cookies)
         :map ap/org-mode-local-map
         ;; Doom local-leader for org-mode
         ("'" . org-edit-special)
         ("*" . org-ctrl-c-star)
         ("+" . org-ctrl-c-minus)
         ("," . org-switchb)
         ("." . org-goto)
         ("@" . org-cite-insert)
         ("A" . org-archive-subtree-default)
         ("e" . org-export-dispatch)
         ("f" . org-footnote-action)
         ("h" . org-toggle-heading)
         ("i" . org-toggle-item)
         ("I" . org-id-get-create)
         ("k" . org-babel-remove-result)
         ("n" . org-store-link)
         ("o" . org-set-property)
         ("q" . org-set-tags-command)
         ("t" . org-todo)
         ("T" . org-todo-list)
         ("x" . org-toggle-checkbox)
         ("a a" . org-attach)
         ("a d" . org-attach-delete-one)
         ("a D" . org-attach-delete-all)
         ("a n" . org-attach-new)
         ("a o" . org-attach-open)
         ("a O" . org-attach-open-in-emacs)
         ("a r" . org-attach-reveal)
         ("a R" . org-attach-reveal-in-emacs)
         ("a u" . org-attach-url)
         ("a s" . org-attach-set-directory)
         ("a S" . org-attach-sync)
         ("b -" . org-table-insert-hline)
         ("b a" . org-table-align)
         ("b b" . org-table-blank-field)
         ("b c" . org-table-create-or-convert-from-region)
         ("b e" . org-table-edit-field)
         ("b f" . org-table-edit-formulas)
         ("b h" . org-table-field-info)
         ("b s" . org-table-sort-lines)
         ("b r" . org-table-recalculate)
         ("b R" . org-table-recalculate-buffer-tables)
         ("b d c" . org-table-delete-column)
         ("b d r" . org-table-kill-row)
         ("b i c" . org-table-insert-column)
         ("b i h" . org-table-insert-hline)
         ("b i r" . org-table-insert-row)
         ("b i H" . org-table-hline-and-move)
         ("b t f" . org-table-toggle-formula-debugger)
         ("b t o" . org-table-toggle-coordinate-overlays)
         ("c c" . org-clock-cancel)
         ("c d" . org-clock-mark-default-task)
         ("c e" . org-clock-modify-effort-estimate)
         ("c E" . org-set-effort)
         ("c g" . org-clock-goto)
         ("c G" . (lambda (&rest _) (interactive) (org-clock-goto 'select)))
         ("c i" . org-clock-in)
         ("c I" . org-clock-in-last)
         ("c o" . org-clock-out)
         ("c r" . org-resolve-clocks)
         ("c R" . org-clock-report)
         ("c t" . org-evaluate-time-range)
         ("c =" . org-clock-timestamps-up)
         ("c -" . org-clock-timestamps-down)
         ("d d" . org-deadline)
         ("d s" . org-schedule)
         ("d t" . org-time-stamp)
         ("d T" . org-time-stamp-inactive)
         ("g c" . org-clock-goto)
         ("g C" . (lambda (&rest _) (interactive) (org-clock-goto 'select)))
         ("g i" . org-id-goto)
         ("g r" . org-refile-goto-last-stored)
         ("g x" . org-capture-goto-last-stored)
         ("l c" . org-cliplink)
         ("l i" . org-id-store-link)
         ("l l" . org-insert-link)
         ("l L" . org-insert-all-links)
         ("l s" . org-store-link)
         ("l S" . org-insert-last-stored-link)
         ("l t" . org-toggle-link-display)
         ("P a" . org-publish-all)
         ("P f" . org-publish-current-file)
         ("P p" . org-publish)
         ("P P" . org-publish-current-project)
         ("P s" . org-publish-sitemap)
         ("r" . org-refile)
         ("R" . org-refile-reverse)
         ("s a" . org-toggle-archive-tag)
         ("s b" . org-tree-to-indirect-buffer)
         ("s c" . org-clone-subtree-with-time-shift)
         ("s d" . org-cut-subtree)
         ("s h" . org-promote-subtree)
         ("s j" . org-move-subtree-down)
         ("s k" . org-move-subtree-up)
         ("s l" . org-demote-subtree)
         ("s n" . org-narrow-to-subtree)
         ("s r" . org-refile)
         ("s s" . org-sparse-tree)
         ("s A" . org-archive-subtree-default)
         ("s N" . widen)
         ("s S" . org-sort)
         ("p d" . org-priority-down)
         ("p p" . org-priority)
         ("p u" . org-priority-up))
  :config
  (when *is-a-mac*
    (define-key ap/org-mode-local-map (kbd "l g") #'org-mac-link-get-link))
  (define-key org-mode-map (kbd "C-c l") ap/org-mode-local-map)
  ;; Open file links in the same frame:
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  ;; (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  ;; Exclude DONE state tasks from refile targets
  (defun sanityinc/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

  (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
    "A version of `org-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-refile goto default-buffer rfloc msg)))

  (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
    "A version of `org-agenda-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-agenda-refile goto rfloc no-update)))

  (org-clock-persistence-insinuate)
  ;;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

  (with-eval-after-load 'org-clock
    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

  (when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
    (add-hook 'org-clock-in-hook
              (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                       (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
    (add-hook 'org-clock-out-hook
              (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                       "tell application \"org-clock-statusbar\" to clock out"))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t))))

  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'grab-mac-link))

  (defun ap/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise.

N-DONE is the number of done elements; N-NOT-DONE is the number of
not done."
    (let (org-log-done org-log-states)  ; turn off logging
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

  (add-hook 'org-after-todo-statistics-hook 'ap/org-summary-todo)
  (add-hook 'org-checkbox-statistics-hook 'ap/org-checkbox-todo)
  (add-to-list 'org-file-apps '("\\.docx\\'" . "open %s"))
  (defun ap/bookmark-before-org-agenda (&rest _)
    "Set a bookmark before opening 'org-agenda', for jumping across workspaces."
    (when (buffer-file-name) (bookmark-set "org-agenda-lastpos"))
    )

  (defun ap/jump-to-admin-workspace (&rest _)
    "Move to the admin workspace when opening agenda, rather than open agenda in the current workspace."
    (interactive "p")
    (let ((inhibit-message t))
      ;; (+workspace/switch-to 0)
      ))
  (advice-add 'org-agenda-list :before 'ap/bookmark-before-org-agenda)
  (advice-add 'org-agenda-switch-to :before 'ap/jump-to-admin-workspace)

  ;; Use C-S-Up/Down to navigate headlines when not using to change clocks
  (defun ap/shiftcontroldown (oldfunc &optional n)
    "Re-implement 'org-shiftcontroldown' and pass N to it.
If not in a clock, move to next headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (oldfunc n)
      (dotimes (_ n) (outline-forward-same-level))))

  (defun ap/shiftcontrolup (oldfunc &optional n)
    "Re-implement 'org-shiftcontrolup' and pass N to it.
If not in a clock, move to previous headline."
    (interactive "p")
    (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
        (oldfunc n)
      (dotimes (_ n) (outline-backward-same-level))))

  (advice-add 'org-shiftcontrolup :around 'ap/shiftcontrolup)
  (advice-add 'org-shiftcontroldown :around 'ap/shiftcontroldown)

  (defun ap/wrap-dotimes (fn)
    "Wrap FN in a dotimes loop to make it repeatable with universal arguments."
    (let ((fn fn)) #'(lambda (&optional c)
                       (interactive "p")
                       (dotimes (_ c) (funcall fn)))))

  (define-key org-mode-map (kbd "M-<up>") (ap/wrap-dotimes 'org-metaup))
  (define-key org-mode-map (kbd "M-<down>") (ap/wrap-dotimes 'org-metadown)))

(use-package grab-mac-link
  :straight t
  :when *is-a-mac*)

(use-package org-cliplink :straight t)

(use-package writeroom-mode :straight t)

(use-package org-pomodoro
  :straight t
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t)
  :bind (:map org-agenda-mode-map ("P" . org-pomodoro)))

(use-package org-appear
  :straight t
  :custom
  (org-appear-trigger 'always)
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  :hook
  (org-mode . org-appear-mode))

(use-package ox-pandoc
  :straight t
  :after ox
  :init
  (add-to-list 'org-export-backends 'pandoc)
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (variable . "revealjs-url=https://revealjs.com"))))

;; Bibliography


;; Bibliography
;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography (concat dotfiles-seadrive-path "/My Library/Documents/Academic Stuff/library.bib"))

;; Configure org-cite
(setq org-cite-global-bibliography (list reftex-default-bibliography))

(eval-after-load 'reftex-vars
  '(progn
     (setq reftex-cite-format '((?\C-m . "[@%l]")))))
;; Basic markdown citation mapping:
(add-hook 'markdown-mode-hook
          (lambda () (define-key markdown-mode-map (kbd "C-c @")
                                 (lambda ()
                                   (interactive)
                                   (let ((reftex-cite-format "[@%l]"))
                                     (reftex-citation))))))

;; Citar for advanced citation:
(use-package citar
  :straight t
  :after org
  :hook ((org-mode markdown-mode latex-mode) . citar-capf-setup)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :config
  ;; Run `citar-org-update-pre-suffix' after inserting a citation to immediately
  ;; set its prefix and suffix
  (advice-add 'org-cite-insert :after #'(lambda (args)
                                          (save-excursion
                                            (left-char) ; First move point inside citation
                                            (citar-org-update-pre-suffix))))
  :bind
  (:map ap/org-mode-local-map :package org
        ("@" . 'citar-insert-citation)
        :map markdown-mode-map :package markdown
        ("C-c @" . 'citar-insert-citation)))

(use-package citar-embark
  :straight t
  :after (citar org)
  :diminish citar-embark-mode
  :custom
  (citar-at-point-function 'embark-act)
  :config
  (citar-embark-mode))

(provide 'init-org)
;;; init-org.el ends here
