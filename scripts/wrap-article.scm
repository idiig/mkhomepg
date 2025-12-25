#!/usr/bin/env guile
!#
;;; wrap-article.scm
;;; Wrap org-exported HTML body with site header/footer using M4

(add-to-load-path (dirname (current-filename)))
(use-modules (site-utils)
             (ice-9 rdelim))

(define (read-html-body html-file)
  "Read HTML body from temporary file."
  (call-with-input-file html-file
    (lambda (port)
      (read-string port))))

;; Main
(define (main args)
  (if (< (length args) 4)
      (begin
        (display "Usage: wrap-article.scm ORG_FILE HTML_BODY OUTPUT_FILE\n" (current-error-port))
        (exit 1))
      (let* ((org-file (list-ref args 1))
             (html-body-file (list-ref args 2))
             (output-file (list-ref args 3))
             (metadata (extract-article-metadata org-file))
             (title (or (get-property metadata ':title) "Untitled"))
             (body (read-html-body html-body-file)))
        (call-m4 `(("PAGE_TITLE" . ,title)
                   ("ARTICLE_BODY" . ,body))
                 '("src/m4/layout.m4" "src/m4/article.m4")
                 output-file)
        (delete-file html-body-file)
        (format #t "Generated ~a\n" output-file))))

(main (program-arguments))
