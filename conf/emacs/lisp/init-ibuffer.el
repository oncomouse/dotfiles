;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:

;; (require-package 'ibuffer-vc)

;; (defun ibuffer-set-up-preferred-filters ()
;;   (ibuffer-vc-set-filter-groups-by-vc-root)
;;   (unless (eq ibuffer-sorting-mode 'filename/process)
;;     (ibuffer-do-sort-by-filename/process)))

;; (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)


(require-package 'ibuffer-projectile)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(setq-default ibuffer-show-empty-filter-groups nil)

(sanityinc/fullframe-mode 'ibuffer-mode)

(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
