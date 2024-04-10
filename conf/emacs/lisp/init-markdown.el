;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :straight t
  :hook (markdown-mode . visual-line-mode)
  :config
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

;; (use-package markdown-ts-mode
;;   :straight (markdown-ts-mode :type git :host github :repo "LionyxML/markdown-ts-mode")
;;   :mode ("\\.md\\'" . markdown-ts-mode)
;;   :config
;;   (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/ikatyang/tree-sitter-markdown" "master" "src")))

(provide 'init-markdown)
;;; init-markdown.el ends here
