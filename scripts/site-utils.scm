#!/usr/bin/env guile
!#

(define-module (site-utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (extract-all-articles-metadata
            extract-article-metadata
            m4->string
            m4->file
            prompt-yes-no
            prompt-string
            current-date-string
            non-empty-line?
            filter-non-empty-lines))

;;; Resource management

(define (call-with-input-pipe cmd proc)
  "Execute CMD and call PROC with the input pipe, ensuring proper cleanup."
  (let ((p (open-input-pipe cmd)))
    (dynamic-wind
      (lambda () #t)
      (lambda () (proc p))
      (lambda () (close-pipe p)))))

(define (call-with-m4-port args proc)
  "Call m4 with ARGS and invoke PROC with the input pipe."
  (let ((p (apply open-pipe* OPEN_READ "m4" args)))
    (dynamic-wind
      (lambda () #t)
      (lambda () (proc p))
      (lambda () (close-pipe p)))))

;;; Metadata extraction

(define (extract-all-articles-metadata)
  "Extract metadata for all articles using elisp script."
  (call-with-input-pipe
    "emacs --batch --script scripts/extract-articles-metadata.el"
    (lambda (p) (read p))))

(define (extract-article-metadata org-file)
  "Extract metadata for a single article."
  (let* ((all-data (extract-all-articles-metadata))
         (base-name (basename org-file ".org")))
    (find (lambda (article)
            (string=? (assoc-ref article 'file) base-name))
          all-data)))

;;; M4 calling

(define (defines->m4-args defines)
  "Convert defines alist to m4 command-line arguments."
  (append-map
    (lambda (kv)
      (match kv
        ((name . value)
         (list "-D" (format #f "~a=~a" name value)))))
    defines))

(define (m4->string defines files)
  "Call M4 with defines and files, return output as string.

  Example:
    (m4->string '((\"PAGE_TITLE\" . \"My Page\"))
                '(\"layout.m4\" \"page.m4\"))"
  (let ((args (append (defines->m4-args defines) files)))
    (call-with-m4-port args
      (lambda (p) (get-string-all p)))))

(define (m4->file defines files output-file)
  "Call M4 with defines and files, write output to file.

  Example:
    (m4->file '((\"PAGE_TITLE\" . \"My Page\"))
              '(\"layout.m4\" \"page.m4\")
              \"output.html\")"
  (call-with-output-file output-file
    (lambda (out)
      (display (m4->string defines files) out)))
  output-file)

;;; User interaction

(define (prompt-yes-no prompt)
  "Ask user a yes/no question."
  (display prompt)
  (force-output)
  (member (read-line) '("y" "Y" "yes" "Yes" "YES")))

(define* (prompt-string prompt #:key (default ""))
  "Ask user for input with default value."
  (display (format #f "~a [~a]: " prompt default))
  (force-output)
  (let ((answer (read-line)))
    (if (string=? answer "") default answer)))

;;; Date utilities

(define (current-date-string)
  "Get current date in YYYY-MM-DD format."
  (strftime "%Y-%m-%d" (localtime (current-time))))

;;; String utilities

(define (non-empty-line? line)
  "Check if line is non-empty (not just whitespace)."
  (and (string? line)
       (not (string-match "^[[:space:]]*$" line))))

(define (filter-non-empty-lines lines)
  "Filter out empty lines from a list."
  (filter non-empty-line? lines))
