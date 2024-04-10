;;; init-vc.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Most version control packages are configured separately: see
;; init-git.el, for example.

;;; Code:

(use-package diff-hl
  :straight t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode))
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk)
              ("M-C-]" . diff-hl-next-hunk)
              ("M-C-[" . diff-hl-previous-hunk)))

;; Doom's version control menu
(defvar ap/vc-map (make-sparse-keymap))
(define-key ap/leader-map (kbd "v") ap/vc-map)
(define-key ap/vc-map (kbd "R") #'vc-revert)
(define-key ap/vc-map (kbd "r") #'diff-hl-revert-hunk)
(define-key ap/vc-map (kbd "s") #'diff-hl-stage-current-hunk)
(define-key ap/vc-map (kbd "t") #'git-timemachine-toggle)
(define-key ap/vc-map (kbd "n") #'diff-hunk-next)
(define-key ap/vc-map (kbd "p") #'diff-hunk-prev)
(define-key ap/vc-map (kbd "/") #'magit-dispatch)
(define-key ap/vc-map (kbd ".") #'magit-file-dispatch)
(define-key ap/vc-map (kbd "'") #'forge-dispatch)
(define-key ap/vc-map (kbd "g") #'magit-status)
(define-key ap/vc-map (kbd "G") #'magit-status-here)
(define-key ap/vc-map (kbd "x") #'magit-file-delete)
(define-key ap/vc-map (kbd "B") #'magit-blame-addition)
(define-key ap/vc-map (kbd "C") #'magit-clone)
(define-key ap/vc-map (kbd "F") #'magit-fetch)
(define-key ap/vc-map (kbd "L") #'magit-log-buffer-file)
(define-key ap/vc-map (kbd "S") #'magit-stage-file)
(define-key ap/vc-map (kbd "U") #'magit-unstage-file)
(define-key ap/vc-map (kbd "f f") #'magit-find-file)
(define-key ap/vc-map (kbd "f g") #'magit-find-git-config-file)
(define-key ap/vc-map (kbd "f c") #'magit-show-commit)
(define-key ap/vc-map (kbd "f i") #'forge-visit-issue)
(define-key ap/vc-map (kbd "f p") #'forge-visit-pullreq)
(define-key ap/vc-map (kbd "o .") #'+vc/browse-at-remote)
(define-key ap/vc-map (kbd "o h") #'+vc/browse-at-remote-homepage)
(define-key ap/vc-map (kbd "o r") #'forge-browse-remote)
(define-key ap/vc-map (kbd "o c") #'forge-browse-commit)
(define-key ap/vc-map (kbd "o i") #'forge-browse-issue)
(define-key ap/vc-map (kbd "o p") #'forge-browse-pullreq)
(define-key ap/vc-map (kbd "o I") #'forge-browse-issues)
(define-key ap/vc-map (kbd "o P") #'forge-browse-pullreqs)
(define-key ap/vc-map (kbd "l r") #'magit-list-repositories)
(define-key ap/vc-map (kbd "l s") #'magit-list-submodules)
(define-key ap/vc-map (kbd "l i") #'forge-list-issues)
(define-key ap/vc-map (kbd "l p") #'forge-list-pullreqs)
(define-key ap/vc-map (kbd "l n") #'forge-list-notifications)
(define-key ap/vc-map (kbd "c r") #'magit-init)
(define-key ap/vc-map (kbd "c R") #'magit-clone)
(define-key ap/vc-map (kbd "c c") #'magit-commit-create)
(define-key ap/vc-map (kbd "c f") #'magit-commit-fixup)
(define-key ap/vc-map (kbd "c i") #'forge-create-issue)
(define-key ap/vc-map (kbd "c p") #'forge-create-pullreq)

(provide 'init-vc)
;;; init-vc.el ends here
