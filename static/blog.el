;; OX-HTML SETUP
(after! ox-html
  (setq org-html-metadata-timestamp-format "%Y-%m-%d"
        org-html-head-include-default-style nil
        org-html-htmlize-output-type 'css
        org-html-validation-link nil
        org-html-prefer-user-labels t
        org-html-head-include-scripts t
        org-html-wrap-src-lines nil
        org-html-checkbox-type 'html
        org-html-checkbox-types '((unicode
                                   (on . "&#x2611;")
                                   (off . "&#x2610;")
                                   (trans . "&#x2610;"))
                                  (ascii
                                   (on . "<code>[X]</code>")
                                   (off . "<code>[&#xa0;]</code>")
                                   (trans . "<code>[-]</code>"))
                                  (html
                                   (on . "<input type='checkbox' checked='checked'
onclick=\"return false;\"/>")
                                   (off . "<input type='checkbox'
onclick=\"return false;\"/>")
                                   (trans . "<input type='checkbox'
onclick=\"return false;\"/>")))
        org-html-link-home ""
        org-html-link-up ""
        org-html-postamble nil)

  (org-export-define-derived-backend 'blog 'html
    :translate-alist '((src-block . eli/org-blog-src-block)
                       (footnote-reference . eli/org-blog-footnote-reference)
                       (template . eli/org-blog-template))))

;; ORG ARTICLES EXPORT

(defun eli/org-blog-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
           (code (org-html-format-code src-block info))
           (label (let ((lbl (org-html--reference src-block info t)))
                    (if lbl (format " id=\"%s\"" lbl) "")))
           (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
                                           "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<pre class=\"example\"%s><code>\n%s</code></pre>" label code)
        (format "<div class=\"org-src-container\">\n%s%s\n</div>"
                ;; Build caption.
                (let ((caption (or (org-export-get-caption src-block)
                                   (org-element-property :name src-block))))
                  (if (not caption) ""
                    (let ((listing-number
                           (format
                            "<span class=\"listing-number\">%s </span>"
                            "Listing: ")))
                      (format "<div class=\"org-src-name\">%s%s</div>"
                              listing-number
                              (org-trim (org-export-data caption info))))))
                ;; Contents.
                (if klipsify
                    (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
                            lang
                            label
                            (if (string= lang "html")
                                " data-editor-type=\"html\""
                              "")
                            code)
                  (format "<pre class=\"src src-%s\"%s><code>%s</code></pre>"
                          lang label code)))))))


(defun eli/org-blog-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
          (id (format "fnr.%d%s"
                      n
                      (if (org-export-footnote-first-reference-p
                           footnote-reference info)
                          ""
                        ".100"))))
     (format
      (concat (plist-get info :html-footnote-format)
              "<input id=\"%s\" class=\"footref-toggle\" type=\"checkbox\">")
      (format "<label for=\"%s\" class=\"footref\">%s</label>"
              id n)
      id))))


(defun eli/org-blog-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (setq eli-test info)
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
            (decl (or (and (stringp xml-declaration) xml-declaration)
                      (cdr (assoc (plist-get info :html-extension)
                                  xml-declaration))
                      (cdr (assoc "html" xml-declaration))
                      "")))
       (when (not (or (not decl) (string= "" decl)))
         (format "%s\n"
                 (format decl
                         (or (and org-html-coding-system
                                  ;; FIXME: Use Emacs 22 style here, see `coding-system-get'.
                                  (coding-system-get org-html-coding-system 'mime-charset))
                             "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
           (cond ((org-html-xhtml-p info)
                  (format
                   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                   (plist-get info :language) (plist-get info :language)))
                 ((org-html-html5-p info)
                  (format " lang=\"%s\"" (plist-get info :language))))
           ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
         (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
               (or link-up link-home)
               (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
                       (plist-get info :title)))
           (subtitle (plist-get info :subtitle))
           (html5-fancy (org-html--html5-fancy-p info)))
       (when title
         (format
          (if html5-fancy
              "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
            "<h1 class=\"title\">%s%s</h1>\n")
          (org-export-data title info)
          (if subtitle
              (format
               (if html5-fancy
                   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
                 (concat "\n" (org-html-close-tag "br" nil info) "\n"
                         "<span class=\"subtitle\">%s</span>\n"))
               (org-export-data subtitle info))
            "")))))
   ;; add article status
   (eli/blog-build-article-status info)
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; gisus
   (eli/blog-build-giscus info)
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
             "</script><script src=\""
             org-html-klipse-js
             "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
             org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

(defvar eli/blog-status-format "<span><i class='bx bx-calendar'></i>
<span>%d</span></span>\n<span><i class='bx bx-edit'></i><span>%C</span></span>")
(defvar eli/blog-history-base-url "https://github.com/niwaka-ame/niwaka-ame.github.io/commits/master/orgs/")

(defun eli/blog-build-article-status (info)
  (let ((input-file (file-name-nondirectory (plist-get info :input-file))))
    (unless (string-equal input-file eli/blog-sitemap)
      (let ((spec (org-html-format-spec info))
            (history-url (concat eli/blog-history-base-url input-file)))
        (concat
         "<div class=\"post-status\">"
         (format-spec eli/blog-status-format spec)
         (format "<span><i class='bx bx-history'></i><span><a href=\"%s\">history</a></span></span>"
                 history-url)
         "</div>")))))

(defvar eli/blog-giscus-script "<script src=\"https://giscus.app/client.js\"
          data-repo=\"Elilif/Elilif.github.io\"
          data-repo-id=\"MDEwOlJlcG9zaXRvcnkyOTgxNjM5ODg=\"
          data-category=\"Announcements\"
          data-category-id=\"DIC_kwDOEcWfFM4Cdz5V\"
          data-mapping=\"pathname\"
          data-strict=\"0\"
          data-reactions-enabled=\"1\"
          data-emit-metadata=\"0\"
          data-input-position=\"top\"
          data-theme=\"light\"
          data-lang=\"zh-CN\"
          crossorigin=\"anonymous\"
          async>
  </script>")

(defun eli/blog-build-giscus (info)
  (let ((input-file (file-name-nondirectory (plist-get info :input-file))))
    (unless (string-equal input-file eli/blog-sitemap)
      eli/blog-giscus-script)))

;;;###autoload
(defun eli/org-blog-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blog filename
                      (concat (when (> (length org-html-extension) 0) ".")
                              (or (plist-get plist :html-extension)
                                  org-html-extension
                                  "html"))
                      plist pub-dir))
;; SITEMAP
(defvar eli/blog-tags nil)

(after! ox
  (add-to-list 'org-export-global-macros
               '("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@"))
  (add-to-list 'org-export-global-macros
               '("tags" . "@@html:<span class=\"tags\" data-tags=\"$1\"></span>@@"))
  (add-to-list 'org-export-global-macros
               '("kbd" . "@@html:<kbd>$1</kbd>@@")))

(defun eli/org-publish-sitemap (title list)
  "Generate the sitemap with title."
  (setq org-html-head-extra
        (format "<style>\n%s\n%s\n</style>"
                ".content:has([value=\"all\"]:checked) li{display: list-item;}\n"
                (mapconcat
                 (lambda (tag)
                   (format ".content:has([value=\"%s\"]:checked)
 li:has([data-tags~=\"%s\"]){display: list-item;}"
                           tag (concat "#" tag)))
                 eli/blog-tags "\n")))
  (concat "#+TITLE: " title
          "\n"
          "#+DATE: 2023-10-10"
          "\n"
          (format "#+BEGIN_EXPORT html
<section class=\"filter\">\n%s\n%s</section>
#+END_EXPORT"
                  "<label class=\"category\">
<input type=\"radio\" name=\"tag\" value=\"all\" checked/>
<span>All</span>
</label>"
                  (mapconcat
                   (lambda (tag)
                     (format "<label class=\"category\">
<input type=\"radio\" name=\"tag\" value=\"%s\"/>
<span>%s</span>
</label>"
                             tag tag))
                   eli/blog-tags "\n"))
          "\n"
          (org-list-to-org list)))

(defun eli/sitemap-dated-entry-format (entry _style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (let* ((file (org-publish--expand-file-name entry project))
         (parsed-title (org-publish-find-property file :title project))
         (title
          (if parsed-title
              (org-no-properties
               (org-element-interpret-data parsed-title))
            (file-name-nondirectory (file-name-sans-extension file))))
         (tags (org-publish-find-property file :filetags project))
         (tags-string (mapconcat
                       (lambda (tag)
                         (concat "#" tag))
                       tags " ")))
    (dolist (tag tags)
      (cl-pushnew tag eli/blog-tags :test #'string=))
    (org-publish-cache-set-file-property file :title title)
    (if (= (length title) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}}   [[file:%s][%s]] {{{tags(%s)}}}"
              (car (org-publish-find-property file :date project))
              (concat "articles/" entry)
              title
              tags-string))))

(defun eli/blog-publish-completion (project)
  (let* ((publishing-directory (plist-get project :publishing-directory))
         (sitemap (file-name-with-extension eli/blog-sitemap "html"))
         (orig-file (expand-file-name sitemap publishing-directory))
         (target-file (expand-file-name
                       sitemap
                       (file-name-directory publishing-directory))))
    (rename-file orig-file target-file t)))

(defun eli/kill-sitemap-buffer (project)
  (let* ((sitemap-filename (plist-get project :sitemap-filename))
         (base-dir (plist-get project :base-directory))
         (sitemap-filepath (expand-file-name sitemap-filename base-dir)))
    (when-let ((sitemap-buffer (find-buffer-visiting sitemap-filepath)))
      (kill-buffer sitemap-buffer))))

;; OVERRIDE DATE
(defun eli/org-publish-find-date (file project)
  "Find the date of FILE in PROJECT.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case use the file system's modification time.  Return
time in `current-time' format."
  (let ((file (org-publish--expand-file-name file project)))
    (or (org-publish-cache-get-file-property file :date nil t)
        (org-publish-cache-set-file-property
         file :date
         (if (file-directory-p file)
             (file-attribute-modification-time (file-attributes file))
           (let ((date (org-publish-find-property file :date project)))
             ;; DATE is a secondary string.  If it contains
             ;; a time-stamp, convert it to internal format.
             ;; Otherwise, use FILE modification time.
             (cond ((let ((ts (and (consp date) (assq 'timestamp date))))
                      (and ts
                           (let ((value (org-element-interpret-data ts)))
                             (and (org-string-nw-p value)
                                  (org-time-string-to-time value))))))
                   (date
                    (org-time-string-to-time (car date)))
                   ((file-exists-p file)
                    (file-attribute-modification-time (file-attributes file)))
                   (t (error "No such file: \"%s\"" file)))))))))

(advice-add 'org-publish-find-date :override 'eli/org-publish-find-date)

;; PUBLISH POSTS AND INDEX
(setq eli/blog-base-dir "~/blog/orgs"
      eli/blog-publish-dir "~/blog/articles"
      eli/blog-sitemap "index.org")

(setq org-publish-project-alist
      (list
       (list "blog articles"
             :base-directory eli/blog-base-dir
             :publishing-directory eli/blog-publish-dir
             :base-extension "org"
             :recursive nil
             :htmlized-source t
             :publishing-function 'eli/org-blog-publish-to-html
             :exclude "rss.org"

             :auto-sitemap t
             :preparation-function 'eli/kill-sitemap-buffer
             :completion-function 'eli/blog-publish-completion
             :sitemap-filename eli/blog-sitemap
             :sitemap-title "Yu Huo's Blog"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-function 'eli/org-publish-sitemap
             :sitemap-format-entry 'eli/sitemap-dated-entry-format

             :html-head "<link rel=\"icon\" href=\"/static/favion.png\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/styles.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/htmlize.css\" />
                  <script src=\"/scripts/script.js\"></script>
                  <script src=\"/scripts/toc.js\"></script>
<link href='https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css' rel='stylesheet'>"
             :html-preamble t
             :html-preamble-format '(("en" "<nav class=\"nav\">
   <a href=\"/index.html\" class=\"button\">Home</a>
   <a href=\"/articles/about.html\" class=\"button\">About Me</a>
   <a href=\"/articles/software.html\" class=\"button\">Softwares</a>
   <a href=\"/rss.xml\" class=\"button\">RSS</a>
 </nav>
 <hr>"))
             :html-postamble t
             :html-postamble-format '(("en" "<hr class=\"Solid\">
 <div class=\"info\">
   <span class=\"author\">Author: %a (%e)</span>
   <span class=\"date\">Create Date: %d</span>
   <span class=\"date\">Last modified: %C</span>
   <span>Creator: %c</span>
 </div>"))
             :with-creator nil
             )))

;; (org-publish-remove-all-timestamps)

;; RSS
(use-package! ox-rss)
(setq eli/blog-rss-dir "~/blog")

(defun eli/org-publish-rss-feed (plist filename dir)
  "Publish PLIST to Rss when FILENAME is rss.org.
DIR is the location of the output."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-publish-org-to
       'rss filename (concat "." org-rss-extension) plist dir)))

(defun eli/org-publish-rss-sitemap (title list)
  "Generate a sitemap of posts that is exported as a RSS feed.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include.  PROJECT is the current
project."
  (concat
   "#+TITLE: " title
   "\n\n"
   (org-list-to-subtree list)))

(defun eli/blog-get-abstract (file)
  "Get the contents of abstract block in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (re-search-forward "^#\\+begin_abstract\n" nil t))
          (end (progn (re-search-forward "^#\\+end_abstract$" nil t)
                      (match-beginning 0))))
      (if beg
          (buffer-substring beg end)
        ""))))

(defun eli/org-publish-rss-entry (entry _style project)
  "Format ENTRY for the posts RSS feed in PROJECT."
  (let* ((file (org-publish--expand-file-name entry project))
         (abstract (eli/blog-get-abstract file))
         (parsed-title (org-publish-find-property file :title project))
         (title
          (if parsed-title
              (org-no-properties
               (org-element-interpret-data parsed-title))
            (file-name-nondirectory (file-name-sans-extension file))))
         (root (org-publish-property :html-link-home project))
         (link (concat
                "articles/"
                (file-name-sans-extension entry) ".html"))
         (pubdate (car (org-publish-find-property file :date project))))
    (org-publish-cache-set-file-property file :title title)
    (format "%s
:properties:
:rss_permalink: %s
:pubdate: %s
:end:\n%s\n[[%s][Read More]]"
            title
            link
            pubdate
            abstract
            (concat
             root
             link))))

(add-to-list 'org-publish-project-alist
             (list "blog rss"
                   :preparation-function #'eli/kill-sitemap-buffer
                   :publishing-directory eli/blog-rss-dir
                   :base-directory eli/blog-base-dir
                   :rss-extension "xml"
                   :base-extension "org"
                   :html-link-home "https://niwaka-ame.github.io/"
                   :html-link-use-abs-url t
                   :html-link-org-files-as-html t
                   :include '("rss.org")
                   :exclude eli/blog-sitemap

                   :publishing-function #'eli/org-publish-rss-feed
                   :auto-sitemap t
                   :sitemap-function #'eli/org-publish-rss-sitemap
                   :sitemap-title "Yu Huo's Blog"
                   :sitemap-filename "rss.org"
                   :sitemap-sort-files #'anti-chronologically
                   :sitemap-format-entry #'eli/org-publish-rss-entry))


;; ABOUT PAGES
(setq yh/blog-about-dir "~/blog/about")
(add-to-list 'org-publish-project-alist
             (list "about"
                   :base-directory yh/blog-about-dir
                   :publishing-directory eli/blog-publish-dir
                   :base-extension "org"
                   :recursive nil
                   :htmlized-source t
                   :publishing-function 'eli/org-blog-publish-to-html
                   ;; :exclude "rss.org"

                   :auto-sitemap nil

                   :html-head "<link rel=\"icon\" href=\"/static/favion.png\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/styles.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/htmlize.css\" />
                  <script src=\"/scripts/script.js\"></script>
                  <script src=\"/scripts/toc.js\"></script>
<link href='https://unpkg.com/boxicons@2.1.4/css/boxicons.min.css' rel='stylesheet'>"
                   :html-preamble t
                   :html-preamble-format '(("en" "<nav class=\"nav\">
   <a href=\"/index.html\" class=\"button\">Home</a>
   <a href=\"/articles/about.html\" class=\"button\">About Me</a>
   <a href=\"/articles/software.html\" class=\"button\">Softwares</a>
   <a href=\"/rss.xml\" class=\"button\">RSS</a>
 </nav>
 <hr>"))
                   :html-postamble t
                   :html-postamble-format '(("en" "<hr class=\"Solid\">
 <div class=\"info\">
   <span class=\"author\">Author: %a (%e)</span>
   <span class=\"date\">Create Date: %d</span>
   <span class=\"date\">Last modified: %C</span>
   <span>Creator: %c</span>
 </div>"))
                   :with-creator nil))

;; PUBLISH ALL COMPONENTS
(add-to-list 'org-publish-project-alist
             (list "Eli's blog"
                   :components '("blog articles" "about" "blog rss")))
