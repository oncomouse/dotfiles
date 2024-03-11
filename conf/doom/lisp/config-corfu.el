;;; config-corfu  --- Corfu configuration for Doom Emacs
;;; Commentary:
;;; Code:
(after! corfu
  ;; Temporary fix for 'corfu--setup having a new signature
  (after! evil
    (advice-remove 'corfu--setup 'evil-normalize-keymaps)
    (advice-remove 'corfu--teardown 'evil-normalize-keymaps)
    (advice-add 'corfu--setup :after (lambda (&rest r) (evil-normalize-keymaps)))
    (advice-add 'corfu--teardown :after  (lambda (&rest r) (evil-normalize-keymaps))))
  ;; Configure orderless:
  (after! orderless
    (setq orderless-component-separator #'orderless-escapable-split-on-space)))


;; Disable autocompletion
(after! corfu
  (setq corfu-auto nil)
  (remove-hook 'post-command-hook #'corfu--auto-post-command 'local))

(map! :when (modulep! :completion corfu)
      (:map corfu-map
            "C-S-s" #'+corfu-move-to-minibuffer
            "C-p" #'corfu-previous
            "C-n" #'corfu-next
            "S-TAB" #'corfu-previous
            [backtab] #'corfu-previous
            "TAB" #'corfu-next
            [tab] #'corfu-next
            (:when (modulep! :completion corfu +orderless)
              [remap corfu-insert-separator] #'+corfu-smart-sep-toggle-escape)))

(let ((cmds-del
       `(menu-item "Reset completion" corfu-reset
         :filter ,(lambda (cmd)
                    (interactive)
                    (when (and (>= corfu--index 0)
                               (eq corfu-preview-current 'insert))
                      cmd))))
      (cmds-ret
       `(menu-item "Insert completion DWIM" corfu-insert
         :filter ,(lambda (cmd)
                    (interactive)
                    (cond ((null +corfu-want-ret-to-confirm)
                           (corfu-quit))
                          ((or (not (minibufferp nil t))
                               (eq +corfu-want-ret-to-confirm t))
                           (when (>= corfu--index 0) cmd))
                          ((eq +corfu-want-ret-to-confirm 'minibuffer)
                           (funcall-interactively cmd)
                           nil)
                          (t cmd))))))
  (map! :when (modulep! :completion corfu)
        :map corfu-map
        [backspace] cmds-del
        "DEL" cmds-del
        [return] cmds-ret
        "RET" cmds-ret
        "C-y" cmds-ret))

(map! :when (modulep! :completion corfu)
      :after corfu
      "C-M-i" #'completion-at-point
      (:map corfu-map
            "SPC" #'corfu-insert-separator
            "C-y" #'corfu-insert
            "M-m" #'+corfu-move-to-minibuffer
            "C-c" #'corfu-quit
            "C-p" #'corfu-previous
            "C-n" #'corfu-next)
      (:after corfu-popupinfo
       :map corfu-popupinfo-map
       "C-S-h" #'corfu-popupinfo-toggle
       "C-<up>" #'corfu-popupinfo-scroll-down
       "C-<down>" #'corfu-popupinfo-scroll-up
       "C-S-p" #'corfu-popupinfo-scroll-down
       "C-S-n" #'corfu-popupinfo-scroll-up
       "C-S-u" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-down corfu-popupinfo-min-height))
       "C-S-d" (cmd! (funcall-interactively #'corfu-popupinfo-scroll-up corfu-popupinfo-min-height)))
      (:map corfu-map
            "C-<return>" '(menu-item "Conclude the minibuffer" exit-minibuffer
                           :enable (active-minibuffer-window))
            "S-<return>" '(menu-item "Insert completion and conclude" +corfu-complete-and-exit-minibuffer
                           :enable (active-minibuffer-window))))

(provide 'config-corfu)
;;; config-corfu.el ends here
