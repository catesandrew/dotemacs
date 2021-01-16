;;; markdown-mime.el ---markdown html export for text/html MIME emails

;; Copyright (C) 2021  Andrew Cates

;; Author: Andrew Cates <catesandrew@gmail.com>
;; Keywords: mime, mail, email, html, markdown

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; WYSIWYG, html mime composition using markdown-mode

;;; Code:
(require 'cl-lib)
(require 'outline)
(require 'org)
(require 'ox-org)

(defcustom markdown-mime-beautify-quoted-mail t
  "Beautify quoted mail in more clean HTML, like Gmail."
  :group 'markdown-mime
  :type 'boolean)

(defcustom markdown-mime-use-property-inheritance nil
  "Non-nil means al MAIL_ properties apply also for sublevels."
  :group 'markdown-mime
  :type 'boolean)

(defcustom markdown-mime-default-header
  "#+OPTIONS: latex:t toc:nil H:3\n"
  "Default header to control html export options.
And ensure first line isn't assumed to be a title line."
  :group 'markdown-mime
  :type 'string)

(defcustom markdown-mime-library 'mml
  "Library to use for marking up MIME elements."
  :group 'markdown-mime
  :type '(choice 'mml 'semi 'vm))

(defcustom markdown-mime-export-ascii nil
  "ASCII export options for text/plain.
Default (nil) selects the original org file."
  :group 'markdown-mime
  :type '(choice 'ascii 'latin1 'utf-8))

(defcustom markdown-mime-preserve-breaks t
  "Temporary value of `org-export-preserve-breaks' during mime encoding."
  :group 'markdown-mime
  :type 'boolean)

(defcustom markdown-mime-fixedwith-wrap
  "<pre style=\"font-family: courier, monospace;\">\n%s</pre>\n"
  "Format string used to wrap a fixedwidth HTML email."
  :group 'markdown-mime
  :type 'string)

(defcustom markdown-mime-find-html-start 'identity
  "Call back to search the new HTML start for htmlize in message buffer."
  :group 'markdown-mime
  :type 'sexp)

(defcustom markdown-mime-org-html-with-latex-default 'dvipng
  "Default value of `org-html-with-latex'."
  :group 'markdown-mime
  :type 'sexp)

(defcustom markdown-mime-mail-signature-separator "^--\s?$"
  "Default mail signature separator."
  :group 'markdown-mime
  :type 'string)

(defvar markdown-mime-export-options '(:with-latex dvipng)
  "Default export options which may override org buffer/subtree options.
You avoid exporting section-number/author/toc with the setup below,
`(setq markdown-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil))'")

(defvar markdown-mime-html-hook nil
  "Hook to run over the html buffer before attachment to email.
This could be used for example to post-process html elements.")

(defvar markdown-mime-pre-html-hook nil
  "Hook to run before html export.
Functions should take no arguments and will be run in a
buffer holding the text to be exported.")

(defvar markdown-mime-send-buffer-hook nil
  "Hook to run in the Org-mode file before export.")

(defvar markdown-mime-debug nil
  "Enable debug logger.")

;; internal variables
(defvar markdown-mime-src--overlay nil)
(defvar markdown-mime-src--beg-marker nil)
(defvar markdown-mime-src--end-marker nil)
(defvar markdown-mime--saved-temp-window-config nil)
(defconst markdown-mime-src--hint "## markdown-mime hint: Press C-c C-c to commit change.\n")

(defun markdown-mime-get-export-options (subtreep)
  "SUBTREEP is t if current node is subtree."
  (cond
   (subtreep
    (or markdown-mime-export-options
        (if (fboundp 'org-export--get-subtree-options)
            (org-export--get-subtree-options))))
   (t
    (or markdown-mime-export-options
        (if (fboundp 'org-export--get-inbuffer-options)
            (org-export--get-inbuffer-options))))))

(defun markdown-mime-current-line ()
  "Get current line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun markdown-mime-use-ascii-charset ()
  "Return nil unless markdown-mime-export-ascii is set to a valid value."
  (car (memq markdown-mime-export-ascii '(ascii utf-8 latin1))))

(defun markdown-mime-export-buffer-or-subtree (subtreep)
  "Similar to `org-html-export-as-html' and `org-org-export-as-org'.
SUBTREEP is t if current node is subtree."
  (let* (
         (ascii-charset (markdown-mime-use-ascii-charset))
         (opts (markdown-mime-get-export-options subtreep))
         (plain (if ascii-charset
                    (progn
                      (setq org-ascii-charset ascii-charset)
                      (org-export-string-as (buffer-string) 'ascii nil opts))
                  (buffer-string)))
         (buf (org-export-to-buffer 'html "*Org Mime Export*"
                nil subtreep nil opts))
         (body (prog1
                   (with-current-buffer buf
                     (buffer-string))
                 (kill-buffer buf))))
    (cons body plain)))

(defun markdown-mime-export-string (string &optional opts)
  "Export STRING into html.
OPTS is export options."
  ;; Emacs 25+ prefer exporting drawer by default
  ;; obviously not acceptable in exporting to mail body
  (let* ((org-export-with-drawers nil))
    ;; we won't export title from org file anyway
    (if opts (setq opts (plist-put opts 'title nil)))
    ;; emacs24.4+
    (org-export-string-as string 'html t (or markdown-mime-export-options opts))))

;; example hook, for setting a dark background in
;; <pre style="background-color: #EEE;"> elements
(defun markdown-mime-change-element-style (element style)
  "Set <ELEMENT> elements in exported html with new default html STYLE."
  (while (re-search-forward (format "<%s" element) nil t)
    (replace-match (format "<%s style=\"%s\"" element style))))

(defun markdown-mime-change-class-style (class style)
  "CLASS is used for new default html STYLE in exported html."
  (while (re-search-forward (format "class=\"%s\"" class) nil t)
    (replace-match (format "class=\"%s\" style=\"%s\"" class style))))

(defun markdown-mime-file (ext path id)
  "Markup a file with EXT, PATH and ID for attachment."
  (when markdown-mime-debug (message "markdown-mime-file called => %s %s %s" ext path id))
  (cl-case markdown-mime-library
    (mml (format "<#part type=\"%s\" filename=\"%s\" disposition=inline id=\"<%s>\">\n<#/part>\n"
                 ext path id))
    (semi (concat
           (format "--[[%s\nContent-Disposition: inline;\nContent-ID: <%s>][base64]]\n"
                   ext id)
           (base64-encode-string
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally path)
              (buffer-string)))))
    (vm "?")))

(defun markdown-mime-beautify-quoted (html)
  "Clean up quoted mail in modern UI style.
HTML is the body of the message."
  (let ((quote-depth 0)
        (line-depth 0)
        (in-quote-p nil)
        (quote-opening "<blockquote class=\"gmail_quote\" style=\"margin:0 0 0 .8ex;border-left:1px #ccc solid;padding-left:1ex\">\n\n<div>")
        (quote-closing "\n</div></blockquote>\n"))
    (with-temp-buffer
      ;; clean title of quoted
      (insert (replace-regexp-in-string
               "<p>[\n\r]*&gt;&gt;&gt;&gt;&gt; .* == \\([^\r\n]*\\)[\r\n]*</p>"
               "<div class=\"gmail_quote\">\\1</div>"
               html))
      (goto-char (point-min))
      (while (not (eobp))
        (setq line-depth 0)
        (setq in-quote-p nil)
        (while (looking-at "&gt;[ \t]*")
          (setq in-quote-p t)
          (replace-match "")
          (cl-incf line-depth))
        (cond
         ((< quote-depth line-depth)
          (while (< quote-depth line-depth)
            (insert quote-opening)
            (cl-incf quote-depth)))
         ((> quote-depth line-depth)
          (while (> quote-depth line-depth)
            (insert quote-closing)
            (cl-decf quote-depth))))
        (if (and in-quote-p (looking-at "^[ \t]*$"))
            (progn
              (insert "</div>\n<div>")
              (forward-line)
              (insert "<br /></div>\n<div>"))
          (forward-line)))
      (buffer-substring (point-min) (point-max)))))

(defun markdown-mime-multipart (plain html &optional images)
  "Markup PLAIN body a multipart/alternative with HTML alternatives.
If html portion of message includes IMAGES they are wrapped in multipart/related part."
  (cl-case markdown-mime-library
    (mml (concat "<#multipart type=alternative>\n<#part type=text/plain>\n"
                 plain
                 (when images "<#multipart type=related>")
                 "<#part type=text/html>\n"
                 (if markdown-mime-beautify-quoted-mail
                     (markdown-mime-beautify-quoted html)
                   html)
                 images
                 (when images "<#/multipart>\n")
                 "<#/multipart>\n"))
    (semi (concat
           "--" "<<alternative>>-{\n"
           "--" "[[text/plain]]\n" plain
           (when images (concat "--" "<<alternative>>-{\n"))
           "--" "[[text/html]]\n"  html
           images
           (when images (concat "--" "}-<<alternative>>\n"))
           "--" "}-<<alternative>>\n"))
    (vm "?")))

(defun markdown-mime-url-to-path (url current-file)
  "If URL is file path, convert to valid path.
Or else use CURRENT-FILE to calculate path."
  (let* ((dir (file-name-directory current-file))
         (path (expand-file-name url dir)))
    (cond
     ((string-match-p "^file:///" url)
      (replace-regexp-in-string "^file://" "" url))
     ((file-exists-p path)
      path)
     (t
      (expand-file-name url default-directory)))))

(defun markdown-mime-replace-images (str current-file)
  "Replace images in STR with cid links.
CURRENT-FILE is used to calculate full path of images."
  (when markdown-mime-debug (message "markdown-mime-replace-images called => %s" current-file))
  (let* (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
        (format
         "src=\"cid:%s\""
         (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                          (match-string 1 text)))
                (path (markdown-mime-url-to-path url current-file))
                (ext (file-name-extension path))
                (id (replace-regexp-in-string "[\/\\\\]" "_" path)))

           ;; Catch non-existent files here. Otherwise users get an error on sending.
           (unless (file-exists-p path)
             (user-error "Path: %s does not exist" path))

           ;; Do it
           (add-to-list 'html-images
                        (markdown-mime-file (concat "image/" ext) path id))
           id)))
      str)
     html-images)))

(defun markdown-mime-insert-html-content (plain file html &optional opts)
  "Insert PLAIN into FILE with HTML content and OPTS."
  (let* (;; dvipng for inline latex because MathJax doesn't work in mail
         ;; Also @see https://github.com/markdown-mime/markdown-mime/issues/16
         ;; (setq org-html-with-latex nil) sometimes useful
         (org-html-with-latex markdown-mime-org-html-with-latex-default)
         ;; we don't want to convert org file links to html
         (org-html-link-org-files-as-html nil)
         (org-link-file-path-type 'absolute)
         ;; prettify reply with ">"
         (org-export-preserve-breaks markdown-mime-preserve-breaks)
         ;; org 9
         (org-html-htmlize-output-type 'inline-css)
         ;; org 8
         (org-export-htmlize-output-type 'inline-css)
         (html-and-images (markdown-mime-replace-images html file))
         (images (cdr html-and-images))
         (html (markdown-mime-apply-html-hook (car html-and-images))))
    (insert (markdown-mime-multipart plain
                                html
                                (if images (mapconcat 'identity images "\n"))))))

(defun markdown-mime-mail-body-begin ()
  "Get begin of mail body."
  (save-excursion
    (goto-char (point-min))
    (search-forward mail-header-separator)
    (+ (point) 1)))

(defun markdown-mime-mail-signature-begin ()
  "Find start of signature line in email."
  (save-excursion
    (goto-char (point-max))
    (re-search-backward markdown-mime-mail-signature-separator nil t nil)))

;;;###autoload
(defun markdown-mime-htmlize ()
  "Export a portion of an email to html using `markdown-mode'.
If called with an active region only export that region, otherwise entire body."
  (interactive)
  (when markdown-mime-debug (message "markdown-mime-htmlize called"))
  (let* ((region-p (region-active-p))
         (html-start (funcall markdown-mime-find-html-start
                              (or (and region-p (region-beginning))
                                  (markdown-mime-mail-body-begin))))
         (html-end (or (and region-p (region-end))
                       (or
                        (markdown-mime-mail-signature-begin)
                        (point-max))))
         (md-text (buffer-substring html-start html-end))
         (html (markdown-mime-export-string (concat markdown-mime-default-header md-text)))
         (file (make-temp-name (expand-file-name
                                "mail" temporary-file-directory))))

    ;; delete current region
    (delete-region html-start html-end)
    (goto-char html-start)
    ;; insert new current
    (markdown-mime-insert-html-content md-text file html)))

;; todo need to use convert code below ↓ and move it up into htmlize above ↑
(defun markdown-mime-convert-message-to-markdown ()
  "Convert the message in the current buffer to a multipart HTML email.

The HTML is rendered by treating the message content as Markdown."
  (interactive)
  (unless (executable-find "pandoc")
    (error "Pandoc not found, unable to convert message"))
  (let* ((begin
           (save-excursion
             (goto-char (point-min))
             (search-forward mail-header-separator)))
          (end (point-max))
          (html-buf (generate-new-buffer "*mail-md-output*"))
          (exit-code
            (call-process-region begin end "pandoc" nil html-buf nil
              "--quiet" "-f" "gfm" "-t" "html"))
          (html (format "<html>\n<head></head>\n<body>\n%s\n</body></html>\n"
                  (with-current-buffer html-buf
                    (buffer-substring (point-min) (point-max)))))
          (raw-body (buffer-substring begin end)))
    (when (not (= exit-code 0))
      (error "Markdown conversion failed, see %s" (buffer-name html-buf)))
    (with-current-buffer html-buf
      (set-buffer-modified-p nil)
      (kill-buffer))
    (undo-boundary)
    (delete-region begin end)
    (save-excursion
      (goto-char begin)
      (newline)
      (insert (markdown-mime-multipart-html-message raw-body html)))))

(defun markdown-mime-multipart-html-message (plain html)
  "Creates a multipart HTML email with a text part and an html part."
  (concat "<#multipart type=alternative>\n"
    "<#part type=text/plain>"
    plain
    "<#part type=text/html>\n"
    html
    "<#/multipart>\n"))

(defun markdown-mime-apply-html-hook (html)
  "Apply HTML hook."
  (if markdown-mime-html-hook
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (run-hooks 'markdown-mime-html-hook)
      (buffer-string))
    html))

(provide 'markdown-mime)
;; Local Variables:
;; coding: utf-8
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;;; markdown-mime.el ends here
