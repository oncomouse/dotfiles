;; Electric Pair
(electric-pair-mode)
;; Add electric-pairs for major-modes
(defmacro spw/add-mode-pairs (hook pairs)
  `(add-hook ,hook
             (lambda ()
               (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
               (setq-local electric-pair-text-pairs electric-pair-pairs))))
(spw/add-mode-pairs 'emacs-lisp-mode-hook '((?` . ?')))
(spw/add-mode-pairs 'markdown-mode-hook '((?* . ?*)))
(spw/add-mode-pairs 'org-mode-hook '((?* . ?*)(?/ . ?/)))
;; Match before, after, and around point with regexp:
(defun general-electric/match-before-point (pattern)
  "Compare the line from up to point against pattern using (string-match)"
  (string-match pattern (buffer-substring (line-beginning-position) (point))))
(defun general-electric/match-after-point (pattern)
  "Compare the line after point against pattern using (string-match)"
  (string-match pattern (buffer-substring (point) (line-end-position))))
(defun general-electric/match-around-point (before after)
  "Compare the line before point to before and the line after point using after."
  (and (general-electric/match-after-point after) (general-electric/match-before-point before)))

(defun dotfiles/sp-point-in-org-cookie-p (id action _context)
  "Return t if the point is inside an org-mode statistics cookie."
  (when (eq action 'insert)
    (general-electric/match-around-point (concat "\\[" (regexp-quote id) "$") "^\\]")))

(defun dotfiles/sp-point-at-headline-p (id action _context)
  "Return t if the point is after a set of 0 or more asterisks at the start
of a line (ie. an org-mode headline)."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "^\\**" (regexp-quote id)))))

(defun dotfiles/point-at-org-mode-list-p (id)
  (or
   (general-electric/match-before-point (concat "^\\s-*[0-9]\\.\\s-+" (regexp-quote id) "$"))
   (general-electric/match-before-point (concat "^\\s-*[+-]\\s-+" (regexp-quote id) "$"))))

(defun dotfiles/sp-handle-checkbox (id action _context)
  "When a bracket is inserted after a bullet, create a checkbox and move on."
  (when (and (eq action 'insert) (dotfiles/point-at-org-mode-list-p id))
    (insert " ")
    (right-char 1)
    (insert " ")))

(defun dotfiles/delete-org-checkbox (_arg &optional _killp)
  "Remove the rest of an org-mode checkbox when the closing bracket is removed."
  (when (and
         (eq major-mode 'org-mode)
         (or
          (general-electric/match-before-point (concat "^\\s-*[0-9]\\.\\s-+\\[ $"))
          (general-electric/match-before-point (concat "^\\s-*[+-]\\s-+\\[ $"))))
    (delete-char -2)))

;; TODO: delete bullets

(defun dotfiles/sp-handle-bullets (id action _context)
  (when (and (eq action 'insert) (sp-point-after-bol-p id action _context))
    (delete-char 1)
    (insert " ")))

(defun dotfiles/sp-move-point-right (&rest _r)
  "Move the point right one"
  (right-char 1))

(defun dotfiles/sp-delete (&rest _r)
  "Delete one character after the point"
  (delete-char 1))

(defun dotfiles/sp-handle-org-fraction-cookie (id action _context)
  "If // is inserted inside an org cookie, remove trailing slash and exit cookie."
  (when (and (eq action 'insert)
             (general-electric/match-around-point
              (concat "\\[" (regexp-quote id) "$")
              (concat "^" (regexp-quote id) "\\]")))
    (delete-char 1)
    (right-char 1)))

(with-eval-after-load 'smartparens
  (advice-add 'delete-backward-char :after 'dotfiles/delete-org-checkbox)

  (sp-with-modes 'org-mode
                 (sp-local-pair "-" " "
                                :when '(sp-point-after-bol-p)

                                :post-handlers '(dotfiles/sp-move-point-right))
                 (sp-local-pair "+" "+" ;; TODO: don't pair when inside a date
                                :post-handlers '(dotfiles/sp-handle-bullets))
                 (sp-local-pair "[" nil
                                :post-handlers '(dotfiles/sp-handle-checkbox))
                 (sp-local-pair "*" "*"
                                :unless '(dotfiles/sp-point-at-headline-p))
                 (sp-local-pair "~" "~"
                                :unless '(sp-point-after-word-p))
                 (sp-local-pair "_" "_"
                                :unless '(sp-point-after-word-p))
                 (sp-local-pair "%" " "
                                :when '(dotfiles/sp-point-in-org-cookie-p)
                                :post-handlers '(dotfiles/sp-delete dotfiles/sp-move-point-right))
                 (sp-local-pair "/" "/" ;; TODO: insert one slash and move right when inside a cookie
                                :post-handlers '(dotfiles/sp-handle-org-fraction-cookie)
                                :unless '(sp-point-after-word-p)
                                :actions '(insert autoskip wrap navigate))))

(provide 'init-electric)
