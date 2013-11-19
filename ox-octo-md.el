
;;; Dependencies

(eval-when-compile (require 'cl))
(require 'ox-md)



(defgroup org-export-octo-md nil
  "Options for exporting Org mode files to octo-md Markdown."
  :tag "Org Octo-Md"
  :group 'org-export
  :version "24.2")

(defcustom org-octo-md-layout "post"
  "Default layout used in Octo-Md article."
  :group 'org-export-octo-md
  :type 'string)

(defcustom org-octo-md-extension "markdown"
  "Default extension for markdown output files. It does not trailing period."
  :group 'org-export-octo-md
  :type 'string)
  
(defcustom org-octo-md-categories ""
  "Default space-separated categories in Octo-Md article."
  :group 'org-export-octo-md
  :type 'string)

(defcustom org-octo-md-icon ""
  "Icon for the post."
  :group 'org-export-octo-md
  :type 'string)

(defcustom org-octo-md-published "true"
  "Default publish status in Octo-Md article."
  :group 'org-export-octo-md
  :type 'string)

(defcustom org-octo-md-coding-system 'utf-8
  "Coding system for octopress markdown. Use utf-8 as the default value."
  :group 'org-export-octo-md
  :type 'coding-system)


;;; Define Back-End

(org-export-define-derived-backend 'octo-md 'md
  :export-block '("MD" "MARKDOWN" "OCTO-MD")
  :menu-entry
  '(?o "Octo-Md: export to Markdown with YAML front matter."
       ((?H "As Markdown buffer" org-octo-md-export-as-md)
        (?h "As Markdown file" org-octo-md-export-to-md)))
  :translate-alist
  '((template . org-octo-md-template) ;; add YAML front matter.
    (inner-template . org-octo-md-inner-template)) ;; force body-only
  :options-alist
  '((:octo-md-layout "OCTO-MD_LAYOUT" nil org-octo-md-layout)
    (:octo-md-icon "OCTO-MD_ICON" nil org-octo-md-icon)
    (:octo-md-categories "OCTO-MD_CATEGORIES" nil org-octo-md-categories)
    (:octo-md-published "OCTO-MD_PUBLISHED" nil org-octo-md-published)))



(defun org-octo-md-template (contents info)
  "Return complete document string after MARKDOWN conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   (org-octo-md--yaml-front-matter info)
   contents))


(defun org-octo-md-inner-template (contents info)
  "Return body of document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
;   (let ((depth (plist-get info :with-toc)))
;     (when depth (org-html-toc depth info)))
   ;; PREVIEW mark on the top of article.
   (unless (equal "true" (plist-get info :octo-md-published))
     "<span style=\"background: red;\">PREVIEW</span>")
   ;; Document contents.
   contents
   ;; Footnotes section.
;;   (org-html-footnote-section info)
   ;; Bibliography.
;;   (org-html-bibliography)
    ))


(defun org-octo-md--get-option-export (info property-name &optional default)
  (let ((property (org-export-data (plist-get info property-name) info)))
    (format "%s" (or property default ""))))

(defun org-octo-md--get-option (info property-name &optional default)
  (let ((property (plist-get info property-name)))
    (format "%s" (or property default ""))))


(defun org-octo-md--yaml-front-matter (info)
  (let ((title
         (org-octo-md--get-option-export info :title))
        (date
         (org-octo-md--get-option-export info :date))
        (layout
         (org-octo-md--get-option info :octo-md-layout org-octo-md-layout))
        (icon
         (org-octo-md--get-option info :octo-md-icon org-octo-md-icon))
        (categories
         (org-octo-md--get-option info :octo-md-categories org-octo-md-categories))
        (published
         (org-octo-md--get-option info :octo-md-published org-octo-md-published))
        (comments
         (org-octo-md--get-option info :octo-md-comments)))
    (unless (equal published "true")
      (setq title (concat "[PREVIEW] " title)))
    (concat
     "---"
     "\ntitle: \""    title
     "\"\ndate: "     date
     "\nlayout: "     layout
     "\ncategories: " categories
     (if (string= icon "") "" (concat "\nimage: /assets/images/" icon))
     "\npublished: "  published
     "\ncomments: "   comments
     "\n---\n")))

;;; Filename and Date Helper

(defun org-octo-md-date-from-filename (&optional filename)
  (let ((fn (file-name-nondirectory (or filename (buffer-file-name)))))
    (if (string-match "^[0-9]+-[0-9]+-[0-9]+" fn)
        (match-string 0 fn)
      nil)))

(defun org-octo-md-property-list (&optional filename)
  (let ((backend 'octo-md) plist)
    (if filename
        (with-temp-buffer
          (insert-file filename)
          (org-mode)
          (setq plist (org-export-get-environment backend))
          (setq plist (plist-put plist :input-file filename)))
      (setq plist (org-export-backend-options backend))
      plist)))

(defun org-octo-md-property (keys &optional filename)
 (let ((plist (org-octo-md-property-list filename)))
   (mapcar (lambda (key)
             (if (eq key :input-file)
                 (plist-get plist key) 
               (org-export-data-with-backend (plist-get plist key) 'octo-md plist)))
           keys)))

(defun org-octo-md-date-from-property (&optional filename)
  (let ((plist (org-octo-md-property filename)))
    (org-read-date
     nil nil
     (org-export-data-with-backend (plist-get plist :date) backend plist))))

(defun org-octo-md-create-filename ()
  (let ((date (org-octo-md-date-from-property))
        (file (file-name-nondirectory (buffer-file-name)))
        (dir  (file-name-directory (buffer-file-name))))
    (expand-file-name
     (replace-regexp-in-string "^[0-9]+-[0-9]+-[0-9]+" date file)
     dir)))

;;; End-User functions


(defun org-octo-md-export-as-md
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Markdown buffer adding some YAML front matter."
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Octo-Md Markdown Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
	      (set-auto-mode t)
              (org-export-add-to-stack (current-buffer) 'octo-md)))
        `(org-export-as 'octo-md ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer
                   'octo-md "*Org Octo-Md Markdown Export*"
                   subtreep visible-only body-only ext-plist)))
      ;; Set major mode.
      (with-current-buffer outbuf (set-auto-mode t))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))


(defun org-octo-md-export-as-md
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Markdown buffer adding some YAML front matter."
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Octo-Md Markdown Export*")
              (erase-buffer)
              (insert output)
	      (set-auto-mode t)
              (funcall org-html-display-buffer-mode)
              (org-export-add-to-stack (current-buffer) 'octo-md)))
        `(org-export-as 'octo-md ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer
                   'octo-md "*Org Octo-Md Markdown Export*"
                   subtreep visible-only body-only ext-plist)))
      ;; Set major mode.
      (with-current-buffer outbuf (set-auto-mode t))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-octo-md-export-to-md
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Markdown file adding some YAML front matter."
  (interactive)
  (let* ((extension (concat "." org-octo-md-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-octo-md-coding-system))
    (if async
        (org-export-async-start
            (lambda (f) (org-export-add-to-stack f 'octo-md))
          (let ((org-export-coding-system org-octo-md-coding-system))
            `(expand-file-name
              (org-export-to-file
               'octo-md ,file ,subtreep ,visible-only ,body-only ',ext-plist))))
      (let ((org-export-coding-system org-octo-md-coding-system))
        (org-export-to-file
         'octo-md file subtreep visible-only body-only ext-plist)))))

;;;###autoload
(defun org-octo-md-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown with YAML front matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'octo-md filename ".md" plist pub-dir))

;;;###autoload
(defun org-octo-md-insert-export-options-template
  (&optional title date setupfile categories published layout)
  "Insert a settings template for Octo-Md exporter."
  (interactive)
  (let ((layout     (or layout org-octo-md-layout))
        (published  (or published org-octo-md-published))
        (icon       (or published org-octo-md-icon))
        (categories (or categories org-octo-md-categories)))
    (save-excursion
      (insert (format (concat
                       "#+TITLE: "             title
                       "\n#+DATE: "              date
                       "\n#+SETUPFILE: "         setupfile
                       "\n#+OCTO-MD_LAYOUT: "     layout
                       "\n#+OCTO-MD_CATEGORIES: " categories
                       "\n#+OCTO-MD_ICON: "       icon
                       "\n#+OCTO-MD_PUBLISHED: "  published
                       "\n\n* \n\n{{{more}}}"))))))

;;; provide

(provide 'ox-octo-md)
