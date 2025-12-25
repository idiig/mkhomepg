#!/usr/bin/env guile
!#
;;; update-article-index.scm
;;; Read article metadata from elisp and update index with M4

(add-to-load-path (dirname (current-filename)))
(use-modules (site-utils)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1))

(define (sort-articles articles)
  "Sort articles by date (descending)."
  (sort articles
        (lambda (a b)
          (string>? (get-property a ':date)
                   (get-property b ':date)))))

(define (format-tags tags)
  "Format tags list as string."
  (if (or (not tags) (null? tags) (eq? tags 'nil))
      ""
      (string-append "<br><small>Tags: " (string-join tags ", ") "</small>")))

(define (format-links links)
  "Format links list as HTML."
  (if (or (not links) (null? links) (eq? links 'nil))
      ""
      (string-append "<br><small>Links: "
                     (string-join
                      (map (lambda (link)
                             (let ((url (get-property link ':url))
                                   (text (get-property link ':text)))
                               (format #f "<a href=\"~a\">~a</a>" url text)))
                           links)
                      ", ")
                     "</small>")))

(define (generate-html-list articles)
  "Generate HTML list from articles."
  (if (null? articles)
      "<p><em>No articles yet.</em></p>"
      (string-append
       "<ul>\n"
       (string-join
        (map (lambda (art)
               (let ((title (get-property art ':title))
                     (date (get-property art ':date))
                     (file (get-property art ':file))
                     (tags (get-property art ':tags))
                     (links (get-property art ':links)))
                 (format #f "  <li><a href=\"/articles/~a.html\">~a</a>~a~a~a</li>"
                         file title
                         (if (string=? date "") "" (string-append " - " date))
                         (format-tags tags)
                         (format-links links))))
             articles)
        "\n")
       "\n</ul>")))

;; Main
(let* ((articles (extract-all-articles-metadata))
       (sorted (sort-articles articles))
       (html-list (generate-html-list sorted))
       (final-html (call-m4 `(("PAGE_TITLE" . "Articles")
                              ("ARTICLE_LIST" . ,html-list))
                            '("src/m4/layout.m4" "src/articles-index.m4"))))
  (call-with-output-file "public/articles/index.html"
    (lambda (port)
      (display final-html port))))
