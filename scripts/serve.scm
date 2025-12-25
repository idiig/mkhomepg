#!/usr/bin/env guile
!#
;;; serve.scm
;;; Simple HTTP server for local preview

(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (ice-9 ftw)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (srfi srfi-1))

(define document-root "public")
(define port 8000)

(define (get-mime-type path)
  "Return MIME type based on file extension."
  (cond
   ((string-suffix? ".html" path) '(text/html (charset . "utf-8")))
   ((string-suffix? ".css" path) '(text/css (charset . "utf-8")))
   ((string-suffix? ".js" path) '(application/javascript (charset . "utf-8")))
   ((string-suffix? ".png" path) '(image/png))
   ((string-suffix? ".jpg" path) '(image/jpeg))
   ((string-suffix? ".jpeg" path) '(image/jpeg))
   ((string-suffix? ".gif" path) '(image/gif))
   ((string-suffix? ".svg" path) '(image/svg+xml))
   ((string-suffix? ".txt" path) '(text/plain (charset . "utf-8")))
   (else '(application/octet-stream))))

(define (file-exists-in-root? path)
  "Check if file exists in document root."
  (let ((full-path (string-append document-root path)))
    (and (file-exists? full-path)
         (not (file-is-directory? full-path)))))

(define (read-file-binary path)
  "Read file as bytevector."
  (call-with-input-file path
    (lambda (port)
      (get-bytevector-all port))))

(define (handle-request request body)
  "Handle HTTP request and return response."
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (path (if (string=? path "/") "/index.html" path))
         (full-path (string-append document-root path)))
    
    (format #t "~a ~a\n" (request-method request) path)
    
    (cond
     ;; File exists
     ((file-exists-in-root? path)
      (values (build-response
               #:code 200
               #:headers `((content-type . ,(get-mime-type path))))
              (read-file-binary full-path)))
     
     ;; 404 Not Found
     (else
      (values (build-response #:code 404)
              (string->utf8 "404 Not Found"))))))

;; Start server
(format #t "Starting Guile HTTP server at http://localhost:~a~%" port)
(format #t "Serving files from: ~a~%" document-root)
(format #t "Press Ctrl+C to stop~%~%")

(run-server handle-request 'http `(#:port ,port))
