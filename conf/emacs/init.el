;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Auto generate site-lisp directory:
(let ((site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
  (unless (file-directory-p site-lisp-dir)
    (make-directory site-lisp-dir)))

;; Update purcell's configuration:
(defun ap/update-purcell ()
  "Update purcell/emacs.d on this computer, if possible"
  (interactive)
  (start-process-shell-command "Updating purcell/emacs.d" (get-buffer-create "*git log*")
                               (format "cd %s && git pull" (expand-file-name "purcell-emacs" user-emacs-directory))))

;; Load purcell's emacs config:
(let ((purcell-path (expand-file-name "purcell-emacs" user-emacs-directory)))
  (unless (file-directory-p purcell-path) (call-process-shell-command
                                           (format "git clone https://github.com/purcell/emacs.d %s" purcell-path)))
  (add-to-list 'load-path (format "%s/lisp" purcell-path)))

;; Set the location for our custom configuration:
(add-to-list 'load-path  "~/dotfiles/conf/emacs/lisp")

(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bootstrap config

;; Line numbers + relative line numbers
(setq display-line-numbers-type 'visual)
;; Disable for the following modes
(dolist (mode '(term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		treemacs-mode-hook
                eat-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-straight)
(require 'init-exec-path) ;; Set up $PATH

;; General performance tuning
(use-package gcmh
  :straight t
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024))
  :hook
  (after-init . (lambda ()
                  (gcmh-mode)
                  (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(use-package diminish :straight t)
(use-package scratch :straight t)
(use-package command-log-mode :straight t)
(use-package hydra :straight t)

(require 'init-frame-hooks)
(require 'init-themes)

(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)
(require 'init-format)
(require 'init-crux)
(require 'init-undo)
(require 'init-helpful)
(require 'init-avy)
(require 'init-flycheck)
(require 'init-todo)

(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-ligature)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-org)
(require 'init-html)
(require 'init-css)
(require 'init-http)
(require 'init-python)
(require 'init-ruby)
(require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
(use-package nginx-mode :straight t)
(setq sanityinc/lispy-modes-hook '(sanityinc/enable-check-parens-on-save))
(require 'init-lisp)
(require 'init-lua)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)

(require 'init-terminals)

;; Extra packages which don't require any configuration

(use-package sudo-edit :straight t)
(use-package htmlize :straight t)
(when *is-a-mac*
  (use-package osx-location :straight t))

(use-package uptimes
  :straight t
  :init
  (setq-default uptimes-keep-count 200)
  :hook
  (after-init . (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))




;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

(require 'init-oncomark)
(require 'init-holidays)

;; Repeat isearch with s/r
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

;; Use isearch in other windows
(defun isearch-forward-other-window (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix -1 1)))
        (other-window next)
        (isearch-forward)
        (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-r") 'isearch-backward-other-window)

;; Transient-mark-mode supporting push-mark
;; Source: https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(define-key global-map (kbd "C-`") 'push-mark-no-activate)
(define-key global-map (kbd "M-`") 'jump-to-mark)

;; Indent with C-</>
(global-set-key (kbd "C->") #'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") #'indent-rigidly-left-to-tab-stop)

(defun ap/countable-wrapper (oldfun &optional c &rest r)
  "Wrap OLDFUN in a loop and perform it C times.  Supply R if provided."
  (interactive "p")
  (dotimes (_ (or c 1))
    (apply oldfun (unless (eq 0 (car (func-arity oldfun))) r))))

(advice-add 'outline-previous-heading :around 'ap/countable-wrapper)
(advice-add 'outline-next-heading :around 'ap/countable-wrapper)
(define-key ap/move-map (kbd "h") 'outline-next-heading)
(define-key ap/move-map (kbd "H") 'outline-previous-heading)
(defvar-keymap outline-heading-repeat-map
  :repeat t
  "h" 'outline-next-heading
  "H" 'outline-previous-heading)

;; It's not the 80s, emacs.
(setq sentence-end-double-space nil)

(defun ap/project-buffers (&optional sources)
  "Display buffers using  `consult-buffer', narrowed to only project files.

Pass SOURCES to consult-buffer, if provided."
  (interactive)
  (setq unread-command-events (append unread-command-events (list ?p 32)))
  (consult-buffer sources))

(define-key global-map (kbd "C-x b") 'ap/project-buffers)
(define-key global-map (kbd "C-x B") 'consult-buffer)

;; Configure cape
(defun ap/add-capf (func)
  "Add FUNC to the `completion-at-point-functions' for the buffer."
  (let ((funcs func))
    (when (not (listp func)) (setq funcs (list func)))
    (mapcar (lambda (x) (add-hook 'completion-at-point-functions x 0 t)) funcs)))

(use-package cape
  :straight t
  :hook ((markdown-mode org-mode text-mode) .
         (lambda () (ap/add-capf (list #'cape-keyword #'cape-dabbrev #'cape-dict)))))

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
