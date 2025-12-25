#!/usr/bin/env -S emacs --script
;; extract-articles-metadata.el
;; Extract article metadata as S-expression

(require 'org)
(require 'org-element)

(defun extract-links (tree)
  "Extract all links from org syntax tree."
  (let ((links '()))
    (org-element-map tree 'link
      (lambda (link)
        (let* ((type (org-element-property :type link))
               (path (org-element-property :path link))
               (raw-desc (car (org-element-contents link)))
               ;; Remove text properties to get plain text only
               (desc (if raw-desc
                        (substring-no-properties raw-desc)
                      path))
               (full-path (cond
                          ;; Org file links: convert .org to .html
                          ((and (string= type "file") 
                                (string-suffix-p ".org" path))
                           (concat "/articles/" 
                                  (file-name-base path) ".html"))
                          ;; Online links: keep as-is
                          ((member type '("http" "https"))
                           (concat type ":" path))
                          ;; Other file links
                          ((string= type "file")
                           path)
                          (t nil))))
          (when full-path
            (push (list :url full-path :text desc) links)))))
    (reverse links)))

(defun collect-articles (dir)
  "Collect all articles from DIR with metadata."
  (let ((articles '()))
    (dolist (file (directory-files dir t "\\.org$"))
      (unless (string-match-p "/\\." (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          (let* ((tree (org-element-parse-buffer))
                 (title (org-element-map tree 'keyword
                          (lambda (kw)
                            (when (string= (org-element-property :key kw) "TITLE")
                              (org-element-property :value kw)))
                          nil t))
                 (author (org-element-map tree 'keyword
                           (lambda (kw)
                             (when (string= (org-element-property :key kw) "AUTHOR")
                               (org-element-property :value kw)))
                           nil t))
                 (date (org-element-map tree 'keyword
                         (lambda (kw)
                           (when (string= (org-element-property :key kw) "DATE")
                             (org-element-property :value kw)))
                         nil t))
                 (tags-str (org-element-map tree 'keyword
                            (lambda (kw)
                              (when (string= (org-element-property :key kw) "TAGS")
                                (org-element-property :value kw)))
                            nil t))
                 (tags (if tags-str
                          (split-string tags-str "[ ,]+" t)
                        '()))
                 (links (or (extract-links tree) '()))
                 (base (file-name-base file)))
            (when title
              (push (list :title title
                         :author (or author "")
                         :date (or date "")
                         :file base
                         :tags tags
                         :links links)
                    articles))))))
    articles))

(defun output-article-data (article)
  "Output article data in Guile-readable format."
  (let ((title (plist-get article :title))
        (author (plist-get article :author))
        (date (plist-get article :date))
        (file (plist-get article :file))
        (tags (plist-get article :tags))
        (links (plist-get article :links)))
    (format "(:title %S :author %S :date %S :file %S :tags %S :links %S)"
            title author date file tags links)))

;; Output as S-expression
(let ((articles (collect-articles "src/articles")))
  (princ "(")
  (let ((first t))
    (dolist (article articles)
      (unless first (princ " "))
      (princ (output-article-data article))
      (setq first nil)))
  (princ ")\n"))
