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
(after! citar
  (setq citar-bibliography reftex-default-bibliography)
  (add-hook! (LaTex-mode org-mode markdown-mode) 'citar-capf-setup)
  (citar-embark-mode))

(after! org
  (setq org-cite-global-bibliography (list reftex-default-bibliography)))

(defcustom ebib-zotero-translation-server "http://127.0.0.1:1969"
  "The address of Zotero translation server."
  :group 'ebib
  :type 'string)

(defun ebib-zotero-translate (item server-path &optional export-format)
  "Convert item to EXPORT-FORMAT entry through `ebib-zotero-translation-server'."
  (let ((export-format (or export-format
                           (downcase (symbol-name (intern-soft bibtex-dialect))))))
    (shell-command-to-string
     (format "curl -s -d '%s' -H 'Content-Type: text/plain' '%s/%s' | curl -s -d @- -H 'Content-Type: application/json' '%s/export?format=%s'" item ebib-zotero-translation-server server-path ebib-zotero-translation-server export-format))))

(defun ebib-zotero-import-url (url)
  "Fetch a entry from zotero translation server via a URL.
The entry is stored in the current database."
  (interactive "MURL: ")
  (with-temp-buffer
    (insert (ebib-zotero-translate url "web"))
    (ebib-import-entries ebib--cur-db)))

(defun ebib-zotero-import-identifier (identifier)
  "Fetch a entry from zotero translation server via an IDENTIFIER.
The entry is stored in the current database,
and the identifier can be DOI, ISBN, PMID, or arXiv ID."
  (interactive "MIDENTIFIER: ")
  (with-temp-buffer
    (insert (ebib-zotero-translate identifier "search"))
    (ebib-import-entries ebib--cur-db)))

(use-package! ebib
  :config
  (map! :nvi "C-c e" 'ebib)
  (setq ebib-preload-bib-files (list reftex-default-bibliography)))

(provide 'config-bibliography)
