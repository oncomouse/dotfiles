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
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c l") ap/org-mode-local-map))

(use-package org
  :straight t)

(use-package grab-mac-link
  :straight t
  :when *is-a-mac*)

(use-package org-cliplink
  :straight t)

;; (define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
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
      org-imenu-depth 2)

;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))




(use-package writeroom-mode :straight t)

;;; Disable electric-indent mode in org
(add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file+headline "" "Inbox") ; "" => `org-default-notes-file'
         "* TODO %?\n%t\n%i\n")
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%t\n%a\n" :clock-resume t)
        ))


;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

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

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d!/!)")
              ))
      org-todo-repeat-to-state "TODO")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))



;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(setq org-agenda-compact-blocks t
      org-agenda-start-on-weekday 1
      org-agenda-span 7
      org-agenda-start-day nil
      org-agenda-include-diary t
      org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up effort-up category-keep)
        (todo category-up effort-up)
        (tags category-up effort-up)
        (search category-up))
      org-agenda-window-setup 'current-window
      org-agenda-custom-commands
      `(("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



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



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")



(use-package org-pomodoro
  :straight t
  :init
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro)))

;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'grab-mac-link)))

(with-eval-after-load 'org
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
      (sqlite . t)))))

(with-eval-after-load 'org
  (define-key ap/leader-open-map (kbd "j") 'org-clock-goto)
  (define-key ap/leader-open-map (kbd "l") 'org-clock-in-last)
  (define-key ap/leader-open-map (kbd "i") 'org-clock-in)
  (define-key ap/leader-open-map (kbd "o") 'org-clock-out)
  (define-key ap/leader-open-map (kbd "a") 'org-agenda)
  (define-key ap/leader-open-map (kbd "c") 'org-capture))

;; Open file links in the same frame:
(with-eval-after-load 'org
  (setf (alist-get 'file org-link-frame-setup) #'find-file))


(setq
 org-directory (concat dotfiles-seadrive-path "/Todo/org")
 org-agenda-files (list
                   (concat dotfiles-seadrive-path "/Todo/todo.org")
                   (concat dotfiles-seadrive-path "/Todo/inbox.org"))
 org-default-notes-file (concat dotfiles-seadrive-path "/Todo/inbox.org"))



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

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'org-after-todo-statistics-hook 'ap/org-summary-todo)
  (add-hook 'org-checkbox-statistics-hook 'ap/org-checkbox-todo))

(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.docx\\'" . "open %s")))



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

(with-eval-after-load 'org
  (advice-add 'org-shiftcontrolup :around 'ap/shiftcontrolup)
  (advice-add 'org-shiftcontroldown :around 'ap/shiftcontroldown))

(defun ap/wrap-dotimes (fn)
  "Wrap FN in a dotimes loop to make it repeatable with universal arguments."
  (let ((fn fn)) #'(lambda (&optional c)
                     (interactive "p")
                     (dotimes (_ c) (funcall fn)))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<up>") (ap/wrap-dotimes 'org-metaup))
  (define-key org-mode-map (kbd "M-<down>") (ap/wrap-dotimes 'org-metadown))
  (define-key org-mode-map (kbd "C-z") 'org-cycle-list-bullet)
  (define-key org-mode-map (kbd "C-c #") #'org-update-statistics-cookies))

;; Doom local-leader for org-mode
(with-eval-after-load 'org
  (define-key ap/org-mode-local-map (kbd "'") #'org-edit-special)
  (define-key ap/org-mode-local-map (kbd "*") #'org-ctrl-c-star)
  (define-key ap/org-mode-local-map (kbd "+") #'org-ctrl-c-minus)
  (define-key ap/org-mode-local-map (kbd ",") #'org-switchb)
  (define-key ap/org-mode-local-map (kbd ".") #'org-goto)
  (define-key ap/org-mode-local-map (kbd "@") #'org-cite-insert)
  (define-key ap/org-mode-local-map (kbd "A") #'org-archive-subtree-default)
  (define-key ap/org-mode-local-map (kbd "e") #'org-export-dispatch)
  (define-key ap/org-mode-local-map (kbd "f") #'org-footnote-action)
  (define-key ap/org-mode-local-map (kbd "h") #'org-toggle-heading)
  (define-key ap/org-mode-local-map (kbd "i") #'org-toggle-item)
  (define-key ap/org-mode-local-map (kbd "I") #'org-id-get-create)
  (define-key ap/org-mode-local-map (kbd "k") #'org-babel-remove-result)
  (define-key ap/org-mode-local-map (kbd "n") #'org-store-link)
  (define-key ap/org-mode-local-map (kbd "o") #'org-set-property)
  (define-key ap/org-mode-local-map (kbd "q") #'org-set-tags-command)
  (define-key ap/org-mode-local-map (kbd "t") #'org-todo)
  (define-key ap/org-mode-local-map (kbd "T") #'org-todo-list)
  (define-key ap/org-mode-local-map (kbd "x") #'org-toggle-checkbox)
  (define-key ap/org-mode-local-map (kbd "a a") #'org-attach)
  (define-key ap/org-mode-local-map (kbd "a d") #'org-attach-delete-one)
  (define-key ap/org-mode-local-map (kbd "a D") #'org-attach-delete-all)
  (define-key ap/org-mode-local-map (kbd "a n") #'org-attach-new)
  (define-key ap/org-mode-local-map (kbd "a o") #'org-attach-open)
  (define-key ap/org-mode-local-map (kbd "a O") #'org-attach-open-in-emacs)
  (define-key ap/org-mode-local-map (kbd "a r") #'org-attach-reveal)
  (define-key ap/org-mode-local-map (kbd "a R") #'org-attach-reveal-in-emacs)
  (define-key ap/org-mode-local-map (kbd "a u") #'org-attach-url)
  (define-key ap/org-mode-local-map (kbd "a s") #'org-attach-set-directory)
  (define-key ap/org-mode-local-map (kbd "a S") #'org-attach-sync)
  (define-key ap/org-mode-local-map (kbd "b -") #'org-table-insert-hline)
  (define-key ap/org-mode-local-map (kbd "b a") #'org-table-align)
  (define-key ap/org-mode-local-map (kbd "b b") #'org-table-blank-field)
  (define-key ap/org-mode-local-map (kbd "b c") #'org-table-create-or-convert-from-region)
  (define-key ap/org-mode-local-map (kbd "b e") #'org-table-edit-field)
  (define-key ap/org-mode-local-map (kbd "b f") #'org-table-edit-formulas)
  (define-key ap/org-mode-local-map (kbd "b h") #'org-table-field-info)
  (define-key ap/org-mode-local-map (kbd "b s") #'org-table-sort-lines)
  (define-key ap/org-mode-local-map (kbd "b r") #'org-table-recalculate)
  (define-key ap/org-mode-local-map (kbd "b R") #'org-table-recalculate-buffer-tables)
  (define-key ap/org-mode-local-map (kbd "b d c") #'org-table-delete-column)
  (define-key ap/org-mode-local-map (kbd "b d r") #'org-table-kill-row)
  (define-key ap/org-mode-local-map (kbd "b i c") #'org-table-insert-column)
  (define-key ap/org-mode-local-map (kbd "b i h") #'org-table-insert-hline)
  (define-key ap/org-mode-local-map (kbd "b i r") #'org-table-insert-row)
  (define-key ap/org-mode-local-map (kbd "b i H") #'org-table-hline-and-move)
  (define-key ap/org-mode-local-map (kbd "b t f") #'org-table-toggle-formula-debugger)
  (define-key ap/org-mode-local-map (kbd "b t o") #'org-table-toggle-coordinate-overlays)
  (define-key ap/org-mode-local-map (kbd "c c") #'org-clock-cancel)
  (define-key ap/org-mode-local-map (kbd "c d") #'org-clock-mark-default-task)
  (define-key ap/org-mode-local-map (kbd "c e") #'org-clock-modify-effort-estimate)
  (define-key ap/org-mode-local-map (kbd "c E") #'org-set-effort)
  (define-key ap/org-mode-local-map (kbd "c g") #'org-clock-goto)
  (define-key ap/org-mode-local-map (kbd "c G") (lambda (&rest _) (interactive) (org-clock-goto 'select)))
  (define-key ap/org-mode-local-map (kbd "c i") #'org-clock-in)
  (define-key ap/org-mode-local-map (kbd "c I") #'org-clock-in-last)
  (define-key ap/org-mode-local-map (kbd "c o") #'org-clock-out)
  (define-key ap/org-mode-local-map (kbd "c r") #'org-resolve-clocks)
  (define-key ap/org-mode-local-map (kbd "c R") #'org-clock-report)
  (define-key ap/org-mode-local-map (kbd "c t") #'org-evaluate-time-range)
  (define-key ap/org-mode-local-map (kbd "c =") #'org-clock-timestamps-up)
  (define-key ap/org-mode-local-map (kbd "c -") #'org-clock-timestamps-down)
  (define-key ap/org-mode-local-map (kbd "d d") #'org-deadline)
  (define-key ap/org-mode-local-map (kbd "d s") #'org-schedule)
  (define-key ap/org-mode-local-map (kbd "d t") #'org-time-stamp)
  (define-key ap/org-mode-local-map (kbd "d T") #'org-time-stamp-inactive)
  (define-key ap/org-mode-local-map (kbd "g c") #'org-clock-goto)
  (define-key ap/org-mode-local-map (kbd "g C") (lambda (&rest _) (interactive) (org-clock-goto 'select)))
  (define-key ap/org-mode-local-map (kbd "g i") #'org-id-goto)
  (define-key ap/org-mode-local-map (kbd "g r") #'org-refile-goto-last-stored)
  (define-key ap/org-mode-local-map (kbd "g x") #'org-capture-goto-last-stored)
  (define-key ap/org-mode-local-map (kbd "l c") #'org-cliplink)
  (define-key ap/org-mode-local-map (kbd "l i") #'org-id-store-link)
  (define-key ap/org-mode-local-map (kbd "l l") #'org-insert-link)
  (define-key ap/org-mode-local-map (kbd "l L") #'org-insert-all-links)
  (define-key ap/org-mode-local-map (kbd "l s") #'org-store-link)
  (define-key ap/org-mode-local-map (kbd "l S") #'org-insert-last-stored-link)
  (define-key ap/org-mode-local-map (kbd "l t") #'org-toggle-link-display)
  (when *is-a-mac*
    (define-key ap/org-mode-local-map (kbd "l g") #'org-mac-link-get-link))
  (define-key ap/org-mode-local-map (kbd "P a") #'org-publish-all)
  (define-key ap/org-mode-local-map (kbd "P f") #'org-publish-current-file)
  (define-key ap/org-mode-local-map (kbd "P p") #'org-publish)
  (define-key ap/org-mode-local-map (kbd "P P") #'org-publish-current-project)
  (define-key ap/org-mode-local-map (kbd "P s") #'org-publish-sitemap)
  (define-key ap/org-mode-local-map (kbd "r") #'org-refile)
  (define-key ap/org-mode-local-map (kbd "R") #'org-refile-reverse)
  (define-key ap/org-mode-local-map (kbd "s a") #'org-toggle-archive-tag)
  (define-key ap/org-mode-local-map (kbd "s b") #'org-tree-to-indirect-buffer)
  (define-key ap/org-mode-local-map (kbd "s c") #'org-clone-subtree-with-time-shift)
  (define-key ap/org-mode-local-map (kbd "s d") #'org-cut-subtree)
  (define-key ap/org-mode-local-map (kbd "s h") #'org-promote-subtree)
  (define-key ap/org-mode-local-map (kbd "s j") #'org-move-subtree-down)
  (define-key ap/org-mode-local-map (kbd "s k") #'org-move-subtree-up)
  (define-key ap/org-mode-local-map (kbd "s l") #'org-demote-subtree)
  (define-key ap/org-mode-local-map (kbd "s n") #'org-narrow-to-subtree)
  (define-key ap/org-mode-local-map (kbd "s r") #'org-refile)
  (define-key ap/org-mode-local-map (kbd "s s") #'org-sparse-tree)
  (define-key ap/org-mode-local-map (kbd "s A") #'org-archive-subtree-default)
  (define-key ap/org-mode-local-map (kbd "s N") #'widen)
  (define-key ap/org-mode-local-map (kbd "s S") #'org-sort)
  (define-key ap/org-mode-local-map (kbd "p d") #'org-priority-down)
  (define-key ap/org-mode-local-map (kbd "p p") #'org-priority)
  (define-key ap/org-mode-local-map (kbd "p u") #'org-priority-up))

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
