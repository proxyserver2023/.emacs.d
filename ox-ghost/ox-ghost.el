;;; ox-ghost.el --- Ghost Markdown Back-End for Org Export Engine

;; Author: Arjen Wiersma <arjen _at! wiersma.org>
;; Based on ox-leanpub by Juan Reyero <juan _at! juanreyero.com>
;; Keywords: org, wp, markdown, ghost

;;; Commentary:

;;; Small adaptation of ox-md.el to make the exported markdown work
;;; better for Ghost publication.  It handles footnotes, and makes
;;; source code separated from its output, and the output does not
;;; display line numbers.  Html blocks are ignored.  Links with IDs
;;; work.  Tables are exported as they are in orgmode, which is pretty
;;; much what Ghost's markdown accepts.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)

;;; Define Back-End

(org-export-define-derived-backend 'ghost 'md
  :menu-entry
  '(?G "Export to Ghost Markdown"
       ((?G "To temporary buffer"
	    (lambda (a s v b) (org-ghost-export-as-markdown a s v)))
	(?g "To file" (lambda (a s v b) (org-ghost-export-to-markdown a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-ghost-export-to-markdown t s v)
		(org-open-file (org-ghost-export-to-markdown nil s v)))))))
  :translate-alist '((fixed-width . org-ghost-fixed-width-block)
                     (example-block . org-ghost-fixed-width-block)
                     (src-block . org-ghost-src-block)
                     (plain-text . org-ghost-plain-text)
                     (inner-template . org-ghost-inner-template)
                     (footnote-reference . org-ghost-footnote-reference)
                     (headline . org-ghost-headline)
                     (link . org-ghost-link)
                     (latex-fragment . org-ghost-latex-fragment)
                     (table . org-ghost-table)
                     ;; Will not work with ghost:
                     (export-block . org-ghost-ignore))) ; #+html

(defun org-ghost-table (table contents info)
  "Transcode a table object from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information.
Add an #+attr_ghost: line right before the table with the formatting info that you want to pass to markdown, like

#+attr_ghost: {title=\"Figure 32\",width=\"60%\"}
| a table | second col |
|---------+------------|
| second  | line       |
| Third   | line       |
"
  (replace-regexp-in-string "^\\#\\+attr_ghost:\s*" ""
                            (buffer-substring (org-element-property :begin table)
                                              (org-element-property :end table))))

(defun org-ghost-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "{$$}%s{/$$}"
          ;; Removes the \[, \] and $ that mark latex fragments
          (replace-regexp-in-string
           "\\\\\\[\\|\\\\\\]\\|\\$" ""
           (org-element-property :value latex-fragment))))

;;; Adding the id so that crosslinks work.
(defun org-ghost-headline (headline contents info)
  (concat (let ((id (org-element-property :ID headline)))
            (if id
                (format "{#L%s}\n" id)
              ""))
          (org-md-headline headline contents info)))

(defun org-ghost-inner-template (contents info)
  "Return complete document string after markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.  Required in order to add footnote
definitions at the end."
  (concat
   contents
   "\n\n"
   (let ((definitions (org-export-collect-footnote-definitions
		       info
                       (plist-get info :parse-tree))))
     ;; Looks like ghost do not like : in labels.
     (mapconcat (lambda (ref)
                  (let ((id (format "[^%s]: " (replace-regexp-in-string
                                               ":" "_"
                                               (let ((label (cadr ref)))
                                                (if label
                                                    label
                                                  (car ref)))))))
                    (let ((def (nth 2 ref)))
                      (concat id (org-export-data def info)))))
                definitions "\n\n"))))

(defun org-ghost-footnote-reference (footnote contents info)
  ;; Looks like ghost do not like : in labels.
  (format "[^%s]"
          (replace-regexp-in-string
           ":" "_"
           (let ((label (org-element-property :label footnote)))
             (if label
                 label
               (org-export-get-footnote-number footnote info))))))

(defun org-ghost-ignore (src-block contents info)
  "")

(defun org-ghost-plain-text (text info)
  text)

;;; {lang="python"}
;;; ~~~~~~~~
;;; def longitude_circle(diameter):
;;;     return math.pi * diameter
;;; longitude(10)
;;; ~~~~~~~~
(defun org-ghost-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (org-element-property :language src-block)))
    (format "\n```%s\n%s```"
            lang
            (org-remove-indentation
             (org-element-property :value src-block)))))

;;; A> {linenos=off}
;;; A> ~~~~~~~~
;;; A> 123.0
;;; A> ~~~~~~~~
(defun org-ghost-fixed-width-block (src-block contents info)
  "Transcode FIXED-WIDTH-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "A> "
   (format "{linenos=off}\n~~~~~~~~\n%s~~~~~~~~"
           (org-remove-indentation
            (org-element-property :value src-block)))))

(defun org-ghost-link (link contents info)
  "Transcode a link object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((id (org-element-property :path link)))
             (format "[%s](#L%s)" contents id)))
          ((org-export-inline-image-p link org-html-inline-image-rules)
           (let ((path (let ((raw-path (org-element-property :path link)))
                         (if (not (file-name-absolute-p raw-path)) raw-path
                           (expand-file-name raw-path)))))
             (format "![%s](%s)"
                     (let ((caption (org-export-get-caption
                                     (org-export-get-parent-element link))))
                       (if caption
                           (org-export-data caption info)
                         ""))
                     path)))
          (t (let* ((raw-path (org-element-property :path link))
                    (path (if (member type '("http" "https" "ftp"))
                              (concat type ":" raw-path)
                            nil)))
               (if path
                   (if (not contents) (format "<%s>" path)
                     (format "[%s](%s)" contents path))
                 contents))))))

;;; Interactive function

;;;###autoload
(defun org-ghost-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'ghost "*Org GHOST Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-ghost-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Ghost's compatible Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'ghost outfile async subtreep visible-only)))

;;;###autoload
(defun org-ghost-publish-to-ghost (plist filename pub-dir)
  "Publish an org file to ghost.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'ghost filename ".md" plist pub-dir))

(provide 'ox-ghost)

;;; ox-ghost.el ends here
