;;; init-oncomark --- Fancy marking and saving to kill-ring
;;; Commentary:
;;; Code:

(require 'pulse)
(require 'hydra)

(defun ap/mark-word (&optional toend) "Mark the word.

If TOEND is non-nil, only mark from point to end of paragraph."
       (interactive)
       (mark-word)
       (unless toend (backward-word)))

(defun ap/mark-paragraph (&optional toend) "Mark the paragraph.

If TOEND is non-nil, only mark from point to end of pargraph."
       (interactive)
       (unless (region-active-p) (call-interactively 'set-mark-command))
       (cond
        (toend (forward-paragraph) (backward-char))
        (t (mark-paragraph))))

(defun ap/mark-line (&optional toend) "Mark the line.

If TOEND is non-nil, only mark from point to the end of the line."
       (interactive)
       (unless (region-active-p) (call-interactively 'set-mark-command))
       (let ((visual-line-mode nil))
         (move-end-of-line nil)
         (unless toend (exchange-point-and-mark)
                 (move-beginning-of-line nil))))

(defun ap/mark-sentence (&optional toend) "Mark the sentence at point.

If TOEND is non-nil, only mark from point to end of sentence."
       (interactive)
       (unless (region-active-p) (call-interactively 'set-mark-command))
       (forward-sentence)
       (exchange-point-and-mark)
       (unless toend (backward-sentence)))

(defun ap/mark-clause (&optional toend) "Mark the current clause (between commas).

If TOEND is non-nil, only mark from point to end of clause."
       (interactive)
       ;; Three modes:
       ;; 1) point is between start of sentence and a comma
       ;; 2) point is between a comma and the end of the sentence
       ;; 3) point is between two commas
       (let ((cur-point (point)) sen-start sen-end comma1 comma2)
         (backward-sentence)
         (setq sen-start (point))
         (forward-sentence)
         (setq sen-end (point))
         (goto-char cur-point)
         (setq comma1 (search-backward "," sen-start t))
         (unless (eq comma1 nil)
           (setq comma1 (point)))
         (goto-char cur-point)
         (setq comma2 (search-forward "," sen-end t))
         (unless (eq comma2 nil)
           (setq comma2 (point)))
         (call-interactively 'set-mark-command)
         (cond
          ((and (numberp comma1) (numberp comma2)) (goto-char (if toend cur-point comma1)) (exchange-point-and-mark) (goto-char comma2) (message (format "%d %d" comma1 comma2)))
          ((numberp comma1) (goto-char (if toend cur-point comma1)) (exchange-point-and-mark) (goto-char sen-end))
          ((numberp comma2) (goto-char (if toend cur-point sen-start)) (exchange-point-and-mark) (goto-char comma2))
          (t (got-char (if toend cur-point sen-start)) (exchange-point-and-mark) (got-char sen-end)))))

(defun ap/save-mark-to-kill-ring (markfun &optional toend)
  "Save region marked by MARKFUN to the `kill-ring'.

If TOEND is non-nil, only save from point to end of region."
  (interactive)
  (save-excursion
    (funcall markfun toend)
    (pulse-momentary-highlight-region (point) (mark))
    (whole-line-or-region-wrap-region-kill 'kill-ring-save 0)))

(defhydra ap/mark-hydra (:color blue)
  ("w" ap/mark-word "whole word")
  ("W" (lambda () (interactive) (ap/mark-word t)) "end of word")
  ("s" ap/mark-sentence "whole sentence")
  ("S" (lambda () (interactive) (ap/mark-sentence t)) "end of sentence")
  ("l" ap/mark-line "whole line")
  ("L" (lambda () (interactive) (ap/mark-line t)) "end of line")
  ("c" ap/mark-clause "whole clause")
  ("C" (lambda () (interactive) (ap/mark-clause t)) "end of clause")
  ("p" ap/mark-paragraph "whole paragraph")
  ("P" (lambda () (interactive) (ap/mark-paragraph t)) "end of paragraph"))

(defhydra ap/save-to-kill-ring (:color blue)
  ("w" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-word)) "whole word")
  ("W" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-word t)) "end of word")
  ("s" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-sentence)) "whole sentence")
  ("S" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-sentence t)) "end of sentence")
  ("l" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-line)) "whole line")
  ("L" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-line t)) "end of line")
  ("c" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-clause)) "whole clause")
  ("C" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-clause t)) "end of clause")
  ("p" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-paragraph)) "whole paragraph")
  ("P" (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-paragraph t)) "end of paragraph"))

;; (defvar ap/mark-word-map (make-sparse-keymap))
;; (define-key ap/mark-word-map (kbd "w") 'ap/mark-word)
;; (define-key ap/mark-word-map (kbd "W") (lambda () (interactive) (ap/mark-word t)))
;; (define-key ap/mark-word-map (kbd "s") 'ap/mark-sentence)
;; (define-key ap/mark-word-map (kbd "S") (lambda () (interactive) (ap/mark-sentence t)))
;; (define-key ap/mark-word-map (kbd "l") 'ap/mark-line)
;; (define-key ap/mark-word-map (kbd "L") (lambda () (interactive) (ap/mark-line t)))
;; (define-key ap/mark-word-map (kbd "c") 'ap/mark-clause)
;; (define-key ap/mark-word-map (kbd "C") (lambda () (interactive) (ap/mark-clause t)))
;; (define-key ap/mark-word-map (kbd "p") 'ap/mark-paragraph)
;; (define-key ap/mark-word-map (kbd "P") (lambda () (interactive) (ap/mark-paragraph t)))

;; (defvar ap/kill-ring-save-map (make-sparse-keymap))
;; (define-key ap/kill-ring-save-map (kbd "w") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-word)))
;; (define-key ap/kill-ring-save-map (kbd "W") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-word t)))
;; (define-key ap/kill-ring-save-map (kbd "s") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-sentence)))
;; (define-key ap/kill-ring-save-map (kbd "S") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-sentence t)))
;; (define-key ap/kill-ring-save-map (kbd "l") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-line)))
;; (define-key ap/kill-ring-save-map (kbd "L") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-line t)))
;; (define-key ap/kill-ring-save-map (kbd "c") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-clause)))
;; (define-key ap/kill-ring-save-map (kbd "C") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-clause t)))
;; (define-key ap/kill-ring-save-map (kbd "p") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-paragraph)))
;; (define-key ap/kill-ring-save-map (kbd "P") (lambda () (interactive) (ap/save-mark-to-kill-ring 'ap/mark-paragraph t)))

(defun ap/whole-line-or-region-kill-ring-save (p)
  "Save P whole lines to the `kill-ring' or activate `ap/save-to-kill-ring'."
  (interactive "P")
  (if (region-active-p)
      (whole-line-or-region-wrap-region-kill 'kill-ring-save p)
    (ap/save-to-kill-ring/body)))

;; (define-key global-map (kbd "M-m") ap/mark-word-map)
(define-key global-map (kbd "M-m") 'ap/mark-hydra/body)
(advice-add 'whole-line-or-region-kill-ring-save :override 'ap/whole-line-or-region-kill-ring-save)

(provide 'init-oncomark)
;;; init-oncomark.el ends here
