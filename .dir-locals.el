;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((eval . (progn
                        ;; Set project root path
                        (defvar my-site-root
                          (locate-dominating-file default-directory ".dir-locals.el")
                          "Project root directory.")
                        
                        ;; Configure org HTML export settings
                        (setq org-html-preamble nil)
                        (setq org-html-postamble nil)
                        (setq org-html-head-include-default-style nil)
                        (setq org-html-head-include-scripts nil)
                        (setq org-html-html5-fancy t)
                        (setq org-html-doctype "html5")
                        (setq org-html-validation-link nil)
                        (setq org-html-head "<link rel=\"stylesheet\" href=\"/css/style.css\">")
                        
                        ;; Build current article using Makefile
                        (defun my-site/build-current-article ()
                          "Build current org article using Makefile."
                          (interactive)
                          (let* ((org-file (buffer-file-name))
                                 (base-name (file-name-base org-file))
                                 (target (concat "public/articles/" base-name ".html")))
                            (compile (format "cd %s && make %s" my-site-root target))))
                        
                        ;; Build static pages
                        (defun my-site/build-pages ()
                          "Build index and old news pages."
                          (interactive)
                          (compile (format "cd %s && make pages" my-site-root)))
                        
                        ;; Build everything
                        (defun my-site/build-all ()
                          "Build entire site."
                          (interactive)
                          (compile (format "cd %s && make all" my-site-root))))))))
