#!/usr/bin/env guile
!#
;;; update-news.scm
;;; Add article to news with overflow cleanup

(add-to-load-path (dirname (current-filename)))
(use-modules (detect-file-changes)
             (site-utils)
	     (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1))

;; Configuration: maximum items in Latest News section
(define max-latest-lines 3)

;;; News file parsing and rebuilding

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

;;; Overflow cleanup logic

(define (read-non-empty-lines file)
  "Read all non-empty lines from file."
  (call-with-input-file file
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (if (non-empty-line? line)
                  (loop (cons line lines))
                  (loop lines))))))))

(define (find-marker-positions lines)
  "Find positions of BEGNEWS, OLD, ODD, ENDNEWS."
  (let loop ((idx 0)
             (remaining lines)
             (begnews #f)
             (old #f)
             (odd #f)
             (endnews #f))
    (if (null? remaining)
        (list begnews old odd endnews)
        (let ((line (car remaining)))
          (cond
           ((string-match "^BEGNEWS" line)
            (loop (+ idx 1) (cdr remaining) idx old odd endnews))
           ((string-match "^OLD" line)
            (loop (+ idx 1) (cdr remaining) begnews idx odd endnews))
           ((string-match "^ODD" line)
            (loop (+ idx 1) (cdr remaining) begnews old idx endnews))
           ((string-match "^ENDNEWS" line)
            (loop (+ idx 1) (cdr remaining) begnews old odd idx))
           (else
            (loop (+ idx 1) (cdr remaining) begnews old odd endnews)))))))

(define (cleanup-overflow news-file max-lines)
  "Ensure OLD marker is at the correct position after BEGNEWS."
  (let* ((lines (read-non-empty-lines news-file))
         (positions (find-marker-positions lines))
         (begnews-pos (car positions))
         (old-pos (cadr positions))
         (target-old-pos (+ begnews-pos max-lines 1)))
    
    (if (not begnews-pos)
        (display "Warning: BEGNEWS marker not found!\n")
        (unless (= old-pos target-old-pos)
          (let* ((lines-without-old 
                  (append (take lines old-pos)
                          (drop lines (+ old-pos 1))))
                 (new-lines 
                  (append (take lines-without-old target-old-pos)
                          (list "OLD")
                          (drop lines-without-old target-old-pos))))
            (call-with-output-file news-file
              (lambda (port)
                (for-each (lambda (line)
                            (display line port)
                            (newline port))
                          new-lines)))
            (display (format #f "Cleaned up overflow: moved OLD from line ~a to ~a\n"
                            (+ old-pos 1) (+ target-old-pos 1))))))))

;;; Main news update logic

(define (add-news-item org-file news-file status)
  "Add article to news with user interaction."
  (let* ((metadata (extract-article-metadata org-file))
         (title (get-property metadata ':title))
         (base-name (basename org-file ".org"))
         (date (current-date-string))
         (status-msg (case status
                      ((new) "New article created")
                      ((modified) "Article modified")
                      (else "Article unchanged")))
         (default-desc (case status
                        ((new) (format #f "I wrote a new post regarding ~a" title))
                        ((modified) (format #f "I updated my post regarding ~a" title))
                        (else (format #f "New post: ~a" title))))
         (prompt (format #f "~a: ~a\nAdd to news? (y/n): " status-msg title)))
    
    (when (ask-yes-no prompt)
      (let* ((description (ask-string "News description" default-desc))
             (new-item (format #f "ITEM([<b>~a</b>: ~a <a href=\"/articles/~a.html\">[Read more]</a>])"
                              date description base-name))
             (sections (parse-news-file news-file))
             (header (car sections))
             (latest-lines (cadr sections))
             (old-lines (caddr sections))
             (older-lines (cadddr sections))
             (footer (car (cddddr sections)))
             (new-latest (cons new-item latest-lines))
             (new-sections (list header new-latest old-lines older-lines footer)))
        
        (call-with-output-file news-file
          (lambda (port)
            (display (rebuild-news-file new-sections) port)))
        
        (display (format #f "Added news item for: ~a\n" title))
        
        (cleanup-overflow news-file max-latest-lines)
        
        (display "Rebuilding news pages...\n")
        (system "make pages")))))

;; Main
(define (main args)
  (if (< (length args) 3)
      (begin
        (display "Usage: update-news.scm ORG_FILE HTML_FILE\n" (current-error-port))
        (exit 1))
      (let* ((org-file (cadr args))
             (html-file (caddr args))
             (news-file "src/news-index.m4")
             (status (file-status org-file html-file)))
        
        (case status
          ((new modified)
           (add-news-item org-file news-file status))
          (else
           (display (format #f "Article unchanged, skipping news update.\n")))))))

(main (command-line))
