#!/usr/bin/env guile
!#

(define-module (site-utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (get-property
            extract-article-metadata
            extract-all-articles-metadata
            call-m4
            ask-yes-no
            ask-string
            current-date-string
            non-empty-line?
            filter-non-empty-lines))

;;; Plist operations

(define (get-property plist key)
  "Get property from plist."
  (let loop ((lst plist))
    (cond
     ((null? lst) #f)
     ((eq? (car lst) key) (cadr lst))
     (else (loop (cddr lst))))))

;;; Metadata extraction

(define (extract-all-articles-metadata)
  "Extract metadata for all articles using elisp script."
  (let* ((pipe (open-input-pipe "emacs --batch --script scripts/extract-articles-metadata.el"))
         (data (read pipe)))
    (close-pipe pipe)
    data))

(define (extract-article-metadata org-file)
  "Extract metadata for a single article."
  (let* ((all-data (extract-all-articles-metadata))
         (base-name (basename org-file ".org")))
    (find (lambda (article)
            (string=? (get-property article ':file) base-name))
          all-data)))

;;; M4 calling

(define* (call-m4 defines files #:optional output-file)
  "Call M4 with safe parameter passing.

  defines: Association list of (name . value) pairs for M4 defines
  files: List of M4 input files to process
  output-file: Optional output file path (if omitted, returns output as string)

  Example:
    (call-m4 '((\"PAGE_TITLE\" . \"My Page\")
               (\"CONTENT\" . \"<p>Hello</p>\"))
             '(\"layout.m4\" \"page.m4\")
             \"output.html\")"
  (let* ((m4-args (append
                  (apply append
                         (map (lambda (def)
                                (list "-D" (string-append (car def) "=" (cdr def))))
                              defines))
                  files))
         (pipe (apply open-pipe* OPEN_READ "m4" m4-args))
         (output (read-string pipe)))
    (close-pipe pipe)
    (if output-file
        (begin
          (call-with-output-file output-file
            (lambda (port)
              (display output port)))
          output-file)
        output)))

;;; User interaction

(define (ask-yes-no prompt)
  "Ask user a yes/no question."
  (display prompt)
  (force-output)
  (let ((answer (read-line)))
    (member answer '("y" "Y" "yes" "Yes" "YES"))))

(define (ask-string prompt default)
  "Ask user for input with default value."
  (display (format #f "~a [~a]: " prompt default))
  (force-output)
  (let ((answer (read-line)))
    (if (string=? answer "")
        default
        answer)))

;;; Date utilities

(define (current-date-string)
  "Get current date in YYYY-MM-DD format."
  (strftime "%Y-%m-%d" (localtime (current-time))))

;;; News file operations

(define (non-empty-line? line)
  "Check if line is non-empty (not just whitespace)."
  (and (string? line)
       (not (string-match "^[[:space:]]*$" line))))

(define (filter-non-empty-lines lines)
  "Filter out empty lines from a list."
  (filter non-empty-line? lines))

(define (parse-news-file file)
  "Parse news-index.m4 into sections.
  
  Returns: (header latest old older footer)"
  (call-with-input-file file
    (lambda (port)
      (let loop ((section 'header)
                 (header '())
                 (latest '())
                 (old '())
                 (older '())
                 (footer '()))
        (let ((line (read-line port)))
          (cond
           ((eof-object? line)
            (list (reverse header)
                  (reverse latest)
                  (reverse old)
                  (reverse older)
                  (reverse footer)))
           ((string-match "^BEGNEWS" line)
            (loop 'latest (cons line header) latest old older footer))
           ((string-match "^OLD" line)
            (loop 'old header latest (cons line old) older footer))
           ((string-match "^ODD" line)
            (loop 'older header latest old (cons line older) footer))
           ((string-match "^ENDNEWS" line)
            (loop 'footer header latest old older (cons line footer)))
           ((eq? section 'header)
            (loop section (cons line header) latest old older footer))
           ((eq? section 'latest)
            (loop section header (cons line latest) old older footer))
           ((eq? section 'old)
            (loop section header latest (cons line old) older footer))
           ((eq? section 'older)
            (loop section header latest old (cons line older) footer))
           ((eq? section 'footer)
            (loop section header latest old older (cons line footer)))))))))

(define (rebuild-news-file sections)
  "Rebuild news file from sections.
  
  sections: (header latest old older footer)"
  (let ((header (car sections))
        (latest (cadr sections))
        (old (caddr sections))
        (older (cadddr sections))
        (footer (car (cddddr sections))))
    (string-join
     (append header
             (list "")
             latest
             (list "")
             old
             (list "")
             older
             (list "")
             footer)
     "\n"
     'suffix)))
