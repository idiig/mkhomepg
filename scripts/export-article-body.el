#!/usr/bin/env -S emacs --script
;; export-article-body.el
;; Export org file to HTML body only

(require 'org)
(require 'ox-html)

(defun my-org-link-filter (text backend info)
  "Convert article org file links to HTML links.
  Replaces 'articles/*.org' with '/articles/*.html' in exported HTML."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "href=\"articles/\([^\"]+\)\.org\""
     "href=\"/articles/\\1.html\""
     text)))

(defun export-org-to-html-body (org-file output-file)
  "Export ORG-FILE to HTML body (no header/footer) as OUTPUT-FILE."
  (with-current-buffer (find-file-noselect org-file)
    (setq org-html-preamble nil)
    (setq org-html-postamble nil)
    (setq org-html-head-include-default-style nil)
    (setq org-html-head-include-scripts nil)
    
    ;; Add link filter
    (add-to-list 'org-export-filter-final-output-functions
                 'my-org-link-filter)
    
    (org-html-export-to-html nil nil nil t)
    (let ((generated-file (concat (file-name-sans-extension org-file) ".html")))
      (when (file-exists-p generated-file)
        (rename-file generated-file output-file t)))))

;; Main
(let ((args command-line-args-left))
  (if (< (length args) 2)
      (progn
        (message "Usage: export-article-body.el ORG_FILE OUTPUT_FILE")
        (kill-emacs 1))
    (let ((org-file (car args))
          (output-file (cadr args)))
      (export-org-to-html-body org-file output-file)
      (message "Exported %s to %s" org-file output-file))))
