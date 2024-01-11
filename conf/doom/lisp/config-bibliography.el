;; Bibliography
;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography (concat dotfiles-seadrive-path "/My Library/Documents/Academic Stuff/library.bib"))
(eval-after-load 'reftex-vars
  '(progn
     (setq reftex-cite-format '((?\C-m . "[@%l]")))))
(add-hook 'markdown-mode-hook
	  (lambda () (define-key markdown-mode-map "\C-c["
				 (lambda ()
				   (interactive)
				   (let ((reftex-cite-format "[@%l]"))
				     (reftex-citation))))))

;; Citar for advanced citation:
(setq citar-bibliography reftex-default-bibliography)
(after! citar
  (add-hook! (LaTex-mode org-mode markdown-mode) 'citar-capf-setup)
  (citar-embark-mode)
  ;; Run `citar-org-update-pre-suffix' after inserting a citation to immediately
  ;; set its prefix and suffix
  (advice-add 'org-cite-insert :after #'(lambda (args)
                                          (save-excursion
                                            (left-char) ; First move point inside citation
                                            (citar-org-update-pre-suffix))))
  )

(map!
 :after citar
 :mode org-mode
 :mode markdown-mode
 :i "C-c c" 'citar-insert-citation)

(after! org
  (setq org-cite-global-bibliography (list reftex-default-bibliography)))

(provide 'config-bibliography)
