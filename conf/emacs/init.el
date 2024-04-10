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
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
;; (require 'init-elpa)      ;; Machinery for installing required packages
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
;; (require 'init-xterm)
(require 'init-themes)

;; (require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
;; (require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
;; (require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
;; (require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
;; (require 'init-erlang)
(require 'init-javascript)
;; (require 'init-php)
(require 'init-org)
;; (require 'init-nxml)
(require 'init-html)
(require 'init-css)
;; (require 'init-haml)
(require 'init-http)
(require 'init-python)
;; (require 'init-haskell)
;; (require 'init-elm)
;; (require 'init-purescript)
;; (require 'init-ruby)
;; (require 'init-rails)
;; (require 'init-sql)
;; (require 'init-ocaml)
;; (require 'init-j)
;; (require 'init-nim)
;; (require 'init-rust)
;; (require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
;; (require 'init-terraform)
;; (require 'init-nix)
(use-package nginx-mode :straight t)
;; (require 'init-paredit)
(setq sanityinc/lispy-modes-hook '(sanityinc/enable-check-parens-on-save))
(require 'init-lisp)
;; (require 'init-sly)
;; (require 'init-clojure)
;; (require 'init-clojure-cider)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)
;; (require 'init-dash)

;; (require 'init-ledger)
(require 'init-lua)
;; (require 'init-uiua)
;; (require 'init-terminals)

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

(require 'init-direnv)

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

;; Local Packages
(require 'init-format)
;; (require 'init-electric)
(require 'init-crux)
(require 'init-ace-window)
(require 'init-undo)
(require 'init-helpful)
(require 'init-workspaces)
(require 'init-vterm)
(require 'init-avy)
(require 'init-ligature)
(require 'init-flycheck)
(require 'init-oncomark)
(require 'init-holidays)

;; Repeat isearch with s/r
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

;; Restore default move-dup bindings:
(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

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

;; Run markdown in visual line mode:
(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; It's not the 80s, emacs.
(setq sentence-end-double-space nil)

;; emacs-lisp local map:
(defvar ap/emacs-lisp-map (make-sparse-keymap))
(define-key emacs-lisp-mode-map (kbd "C-c l") ap/emacs-lisp-map)
(define-key lisp-interaction-mode-map (kbd "C-c l") ap/emacs-lisp-map)
(define-key ap/emacs-lisp-map (kbd "e b") #'eval-buffer)
(define-key ap/emacs-lisp-map (kbd "e d") #'eval-defun)
(define-key ap/emacs-lisp-map (kbd "e e") #'eval-last-sexp)
(define-key ap/emacs-lisp-map (kbd "e r") #'eval-region)
(define-key ap/emacs-lisp-map (kbd "e l") #'load-library)
(define-key ap/emacs-lisp-map (kbd "g f") #'find-function)
(define-key ap/emacs-lisp-map (kbd "g v") #'find-variable)
(define-key ap/emacs-lisp-map (kbd "g l") #'find-library)

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
(with-eval-after-load 'paredit
  (diminish 'paredit-mode))
(with-eval-after-load 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))
(with-eval-after-load 'smartparens
  (diminish 'smartparens-mode))
(with-eval-after-load 'apheleia
  (diminish 'apheleia-mode))

;; (use-package markdown-ts-mode
;;   :straight (markdown-ts-mode :type git :host github :repo "LionyxML/markdown-ts-mode")
;;   :mode ("\\.md\\'" . markdown-ts-mode)
;;   :config
;;   (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/ikatyang/tree-sitter-markdown" "master" "src")))

(defun ap/close-window (&optional arg)
  "Combines `delete-window' and `delete-other-windows'.
When C-u (ARG is 4) is pressed, delete other windows."
  (interactive "p")
  (if (= arg 4) (delete-other-windows) (delete-window)))

(define-key global-map (kbd "C-x 0") 'ap/close-window)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
