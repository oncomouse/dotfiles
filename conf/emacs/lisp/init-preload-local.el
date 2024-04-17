;;; init-preload-local.el --- Things that need setting before all the packages
;;; Code:
;;; Commentary:

;; savehist was a problem on macos:
(when *is-a-mac*
  (setq history-length 100)
  (put 'minibuffer-history 'history-length 50)
  (put 'kill-ring 'history-length 25))
;; Surpress nativecomp warnings:
(setq native-comp-async-report-warnings-errors nil)

(when *is-a-mac*
  (use-package ns-auto-titlebar
    :straight t
    :config
    (ns-auto-titlebar-mode)))

;; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Set font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height (if *is-a-mac* 155 140))
(unless *is-a-mac*
  (set-face-attribute 'italic nil :font "Fira Mono" :slant 'italic :height 145))

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

(defvar ap/leader-map (make-sparse-keymap))
(define-key global-map (kbd "C-c") ap/leader-map)

(defvar ap/move-map (make-sparse-keymap))
(define-key global-map (kbd "M-g") ap/move-map)

(defvar ap/leader-open-map (make-sparse-keymap))
(define-key ap/leader-map (kbd "o") ap/leader-open-map)

;; clm mode overrides our open map:
(setq command-log-mode-key-binding-open-log nil)

(defvar ap/leader-code-map (make-sparse-keymap))
(define-key ap/leader-map (kbd "c") ap/leader-code-map)

;; Whoami
(setq user-full-name "Andrew Pilsch"
      user-mail-address "apilsch@tamu.edu")

;; Reload files when the change on disk
(global-auto-revert-mode t)

;; Enable repeat-mode
(repeat-mode)
;; Enable repeat for the mark-ring
(setq set-mark-command-repeat-pop t)
(require 'init-secret)

(provide 'init-preload-local)
;;; init-preload-local.el ends here
