;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-complete-and-exit-minibuffer ()
  (interactive)
  (if (>= corfu--index 0)
      (corfu-complete)
    (corfu-insert))
  (exit-minibuffer))

;;;###autoload
(defun +corfu-move-to-minibuffer ()
  ;; Adapted from Corfu's README.
  (interactive)
  (pcase completion-in-region--data
    (`(,_ ,_ ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (completing-read "Completion: " table pred nil nil 'corfu-history)))))

;;;###autoload
(defun +corfu-smart-sep-toggle-escape ()
  "Insert `corfu-separator' or toggle escape if it's already there."
  (interactive)
  (cond ((and (char-equal (char-before) corfu-separator)
              (char-equal (char-before (1- (point))) ?\\))
         (save-excursion (delete-char -2)))
        ((char-equal (char-before) corfu-separator)
         (save-excursion (backward-char 1)
                         (insert-char ?\\)))
        (t
         ;; Without this corfu quits immediately.
         (setq this-command #'corfu-insert-separator)
         (call-interactively #'corfu-insert-separator))))
