;;; config-avy.el -- Configure avy with custom dispatch actions
;;; Commentary:
;;; Based on <https://karthinks.com/software/avy-can-do-anything/>
;;; Code:
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)
(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-flyspell (pt)
  (save-excursion
    (goto-char pt)
    (when (require 'flyspell nil t)
      (flyspell-correct-at-point)))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(use-package avy
  :straight t
  :general
  ("M-j" 'avy-goto-char-timer)
  (:prefix "M-g"
           "b" 'avy-pop-mark
           "l" 'avy-goto-line
           "g" 'avy-goto-char-timer)
  :config
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?. avy-dispatch-alist) 'avy-action-flyspell
        (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  (setq avy-keys (delete ?k avy-keys))
  )

(provide 'init-avy)
;;; init-avy.el ends here
