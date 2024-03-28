;;; init-ace-window.el --- Config for ace-window completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Resize window using hydras
(defhydra hydra-window-resizer (:columns 2)
  "Window Sizing."
  ("-" shrink-window-horizontally "horizontal shrink")
  ("=" enlarge-window-horizontally "horizontal enlarge")
  ("_" shrink-window "vertical shrink")
  ("+" enlarge-window "vertical enlarge"))

(defun aw-window-resize (window)
  "Resize WINDOW using `hydra-window-resizer/body'."
  (aw-switch-to-window window)
  (hydra-window-resizer/body))

(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-alist
   '((?x aw-delete-window "Delete Window")
     (?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?B aw-switch-buffer-in-window "Select Buffer")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?c aw-split-window-fair "Split Fair Window")
     (?v aw-split-window-vert "Split Vert Window")
     (?b aw-split-window-horz "Split Horz Window")
     (?o delete-other-windows "Delete Other Windows")
     (?r aw-window-resize "Resize Window")
     (?? aw-show-dispatch-help))))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
