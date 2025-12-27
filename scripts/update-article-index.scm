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
          (string>? (assoc-ref a 'date)
                   (assoc-ref b 'date)))))

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
                             (let ((url (assoc-ref link 'url))
                                   (text (assoc-ref link 'text)))
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
               (let ((title (assoc-ref art 'title))
                     (date (assoc-ref art 'date))
                     (file (assoc-ref art 'file))
                     (tags (assoc-ref art 'tags))
                     (links (assoc-ref art 'links)))
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
       (html-list (generate-html-list sorted)))
  (m4->file `(("PAGE_TITLE" . "Articles")
              ("ARTICLE_LIST" . ,html-list))
            '("src/m4/layout.m4" "src/articles-index.m4")
            "public/articles/index.html"))
