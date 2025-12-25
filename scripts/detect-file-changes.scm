#!/usr/bin/env guile
!#
;;; detect-file-changes.scm
;;; Content-based file change detection using SHA256 hashing

(define-module (detect-file-changes)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ftw)
  #:export (file-status
            get-cached-hash
            update-hash-cache))

(define cache-dir ".cache")
(define cache-file (string-append cache-dir "/file-hashes.scm"))

(define (ensure-cache-dir)
  "Ensure cache directory exists."
  (unless (file-exists? cache-dir)
    (mkdir cache-dir)))

(define (file-hash file)
  "Get SHA256 hash of file content."
  (if (file-exists? file)
      (let* ((pipe (open-input-pipe (format #f "sha256sum ~a" file)))
             (output (read-line pipe)))
        (close-pipe pipe)
        (if (eof-object? output)
            #f
            (car (string-split output #\space))))
      #f))

(define (load-hash-cache)
  "Load hash cache from file."
  (ensure-cache-dir)
  (if (file-exists? cache-file)
      (call-with-input-file cache-file read)
      '()))

(define (save-hash-cache cache)
  "Save hash cache to file."
  (ensure-cache-dir)
  (call-with-output-file cache-file
    (lambda (port)
      (write cache port)
      (newline port))))

(define (get-cached-hash file)
  "Get cached hash for file."
  (let ((cache (load-hash-cache)))
    (assoc-ref cache file)))

(define (update-hash-cache file hash)
  "Update hash cache for file."
  (let* ((cache (load-hash-cache))
         (new-cache (assoc-set! cache file hash)))
    (save-hash-cache new-cache)))

(define* (file-status org-file html-file #:optional (update-cache? #t))
  "Determine file status: 'new, 'modified, or 'unchanged.
  
  Compares org file content hash with cached hash.
  If update-cache? is #t, updates the cache with new hash."
  (let ((html-exists (file-exists? html-file))
        (org-hash (file-hash org-file))
        (cached-hash (get-cached-hash org-file)))
    (cond
     ;; HTML doesn't exist - new article
     ((not html-exists)
      (when update-cache?
        (update-hash-cache org-file org-hash))
      'new)
     ;; No cached hash - treat as modified
     ((not cached-hash)
      (when update-cache?
        (update-hash-cache org-file org-hash))
      'modified)
     ;; Compare hashes
     ((not (string=? org-hash cached-hash))
      (when update-cache?
        (update-hash-cache org-file org-hash))
      'modified)
     ;; Unchanged
     (else 'unchanged))))
