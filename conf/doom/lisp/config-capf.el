;;; config-capf.el -- Configure specific completion-at-point-functions, as needed
;;; Commentary:
;;; Code:

;; Org mode completion functions:
(add-hook! org-mode (add-hook
                     'completion-at-point-functions
                     (cape-capf-super #'cape-dict #'cape-dabbrev #'cape-keyword) 0 t))

(provide 'config-capf)
;;; config-capf.el ends here
