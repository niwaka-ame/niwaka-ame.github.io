(require 'ox-publish)

(defvar my-html-blog-postamble
  "<div class='footer'> © Yu Huo 2022. Created %d, Last updated %C, built with %c</div>"
  "Adapted From https://ravi.pro/blog/blogging-with-emacs-org-mode.html#org0ff31cb")
(defvar my-blog-base-dir "~/org/blog/post/")
(defvar my-blog-publishing-dir "~/org/blog/public/")

(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-doctype "html5"
      org-html-head "<link href=\"https://cdn.simplecss.org/simple.min.css\" rel=\"stylesheet\" type=\"text/css\" />")

(defun my-format-entry (entry style project)
  "Adapted From https://ravi.pro/blog/blogging-with-emacs-org-mode.html#org0ff31cb"
  (if (file-directory-p (org-publish--expand-file-name entry project))
                                        ; handle directory entries
      (format "%s" (capitalize (substring entry 0 -1)))
    (format "[[file:%s][%s]] --- %s"
            entry
            (org-publish-find-title entry project)
            (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))))

(setq org-publish-project-alist
      (list
       (list "post"
             :base-directory my-blog-base-dir
             :base-extension "org"
             :recursive t
             :publishing-function '(org-html-publish-to-html)
             :publishing-directory my-blog-publishing-dir
             :exclude (regexp-opt '("README" "draft"))
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title "Yu Huo"
             :sitemap-style 'tree
             :sitemap-sort-files 'anti-chronologically
             :sitemap-format-entry #'my-format-entry
             :html-postamble my-html-blog-postamble
             :html-link-up "/"
             :html-link-home "/"
             :with-toc nil
             :section-numbers t
             )
       (list "site" :components '("post"))))
