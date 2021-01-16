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

;; ;; example addition to `markdown-mime-html-hook' adding a dark background
;; ;; color to <pre> elements
;; (add-hook 'markdown-mime-html-hook
;;           (lambda ()
;;             (markdown-mime-change-element-style
;;              "pre" (format "color: %s; background-color: %s;"
;;                            "#E6E1DC" "#232323"))
;;             (markdown-mime-change-class-style
;;              "verse" "border-left: 2px solid gray; padding-left: 4px;")))

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

(defun markdown-mime-extract-non-image-files ()
  "Extract non-image links in current buffer."
  (cond
   ((>= (markdown-mime-org-major-version) 9)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (and (string= (org-element-property :type link) "file")
                   (not (string-match
                         (cdr (assoc "file" org-html-inline-image-rules))
                         (org-element-property :path link))))
          (org-element-property :path link)))))
   (t
    (message "Warning: org-element-map is not available. File links will not be attached.")
    nil)))

(defun markdown-mime-insert-html-content (plain file html opts)
  "Insert PLAIN into FILE with HTML content and OPTS."
  (let* ((files (markdown-mime-extract-non-image-files))
         ;; dvipng for inline latex because MathJax doesn't work in mail
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

    ;; If there are files that were attached, we should remove the links,
    ;; and mark them as attachments. The links don't work in the html file.
    (when files
      (mapc (lambda (f)
              (setq html (replace-regexp-in-string
                          (format "<a href=\"%s\">%s</a>"
                                  (regexp-quote f) (regexp-quote f))
                          (format "%s (attached)" (file-name-nondirectory f))
                          html)))
            files))

    (insert (markdown-mime-multipart plain
                                html
                                (if images (mapconcat 'identity images "\n"))))

    ;; Attach any residual files
    (when files
      (mapc (lambda (f)
              (when markdown-mime-debug (message "attaching: %s" f))
              (mml-attach-file f))
            files))))

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
  "Export a portion of an email to html using `org-mode'.
If called with an active region only export that region, otherwise entire body."
  (interactive)
  (when markdown-mime-debug (message "markdown-mime-htmlize called"))
  (let* ((region-p (org-region-active-p))
         (html-start (funcall markdown-mime-find-html-start
                              (or (and region-p (region-beginning))
                                  (markdown-mime-mail-body-begin))))
         (html-end (or (and region-p (region-end))
                       (or
                        (markdown-mime-mail-signature-begin)
                        (point-max))))
         (org-text (buffer-substring html-start html-end))
;; to hold attachments for inline html images
         (opts (if (fboundp 'org-export--get-inbuffer-options)
                   (org-export--get-inbuffer-options)))
         (ascii-charset (markdown-mime-use-ascii-charset))
         (plain (if ascii-charset
                    (progn
                      (setq org-ascii-charset ascii-charset)
                      (org-export-string-as (concat markdown-mime-default-header org-text) 'ascii nil opts))
                  org-text))
         (html (markdown-mime-export-string (concat markdown-mime-default-header org-text) opts))
         (file (make-temp-name (expand-file-name
                                "mail" temporary-file-directory))))

    ;; delete current region
    (delete-region html-start html-end)
    (goto-char html-start)
    ;; insert new current
    (markdown-mime-insert-html-content plain file html opts)))

(defun markdown-mime-apply-html-hook (html)
  "Apply HTML hook."
  (if markdown-mime-html-hook
      (with-temp-buffer
        (insert html)
        (goto-char (point-min))
        (run-hooks 'markdown-mime-html-hook)
        (buffer-string))
    html))

(defun markdown-mime--get-buffer-title ()
  "Get buffer title."
  (let* ((tmp (if (fboundp 'org-export--get-inbuffer-options)
                  (plist-get (org-export--get-inbuffer-options) :title))))
    (when tmp
      (let ((txt (car tmp)))
        (set-text-properties 0 (length txt) nil txt)
        txt))))

(defun markdown-mime-compose (exported file to subject headers subtreep)
  "Create mail body from EXPORTED in FILE with TO, SUBJECT, HEADERS.
If SUBTREEP is t, curret org node is subtree."
  ;; start composing mail
  (let* ((html (car exported))
         (plain (cdr exported))
         (export-opts (markdown-mime-get-export-options subtreep))
         patched-html)
    (compose-mail to subject headers nil)
    (message-goto-body)
    (setq patched-html (with-temp-buffer
                         (insert html)
                         (goto-char (point-min))
                         (run-hooks 'markdown-mime-pre-html-hook)
                         (buffer-string)))
    ;; insert text
    (markdown-mime-insert-html-content plain file patched-html export-opts)))

(defun markdown-mime-extract-keywords ()
  "Extract keywords."
  (cond
   ((>= (markdown-mime-org-major-version) 9)
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (keyword)
        (cons (org-element-property :key keyword)
              (org-element-property :value keyword)))))
   (t
    (message "Warning: org-element-map is not available. File keywords will not work.")
    '())))

(defun markdown-mime-build-mail-other-headers (cc bcc from)
  "Build mail header from CC, BCC, and FROM."
  (let* ((arr (list (cons "Cc" cc) (cons "Bcc" bcc)  (cons "From" from )))
         rlt)
    (dolist (e arr)
      (when (cdr e)
        (push e rlt)))
    rlt))

;;;###autoload
(defun markdown-mime-org-buffer-htmlize ()
  "Create an email buffer of the current org buffer.
The email buffer will contain both html and in org formats as mime
alternatives.

The following file keywords can be used to control the headers:
#+MAIL_TO: some1@some.place
#+MAIL_SUBJECT: a subject line
#+MAIL_CC: some2@some.place
#+MAIL_BCC: some3@some.place
#+MAIL_FROM: sender@some.place

The cursor ends in the TO field."
  (interactive)
  (run-hooks 'markdown-mime-send-buffer-hook)
  (let* ((org-html-klipsify-src nil)
         (region-p (org-region-active-p))
         (file (buffer-file-name (current-buffer)))
         (keywords (markdown-mime-extract-keywords))
         (subject (or (cdr (assoc "MAIL_SUBJECT" keywords))
                      (markdown-mime--get-buffer-title)
                      (if (not file) (buffer-name (buffer-base-buffer))
                        (file-name-sans-extension
                         (file-name-nondirectory file)))))
         (exported (markdown-mime-export-buffer-or-subtree nil))
         (to (cdr (assoc "MAIL_TO" keywords)))
         (cc (cdr (assoc "MAIL_CC" keywords)))
         (bcc (cdr (assoc "MAIL_BCC" keywords)))
         (from (cdr (assoc "MAIL_FROM" keywords)))
         (other-headers (markdown-mime-build-mail-other-headers cc bcc from)))
    (markdown-mime-compose exported file to subject other-headers nil)
    (message-goto-to)))

(defun markdown-mime-org-major-version ()
  "Get Org major version."
  (string-to-number (car (split-string (org-release) "\\."))))

(defun markdown-mime-attr (property)
  "Get org mime PROPERTY."
  (org-entry-get nil property markdown-mime-use-property-inheritance))

;;;###autoload
(defun markdown-mime-org-subtree-htmlize (&optional htmlize-first-level)
  "Create an email buffer from current subtree.
If HTMLIZE-FIRST-LEVEL is t, first level subtree of current node is htmlized.

Following headline properties can determine the mail headers.
* subtree heading
  :PROPERTIES:
  :MAIL_SUBJECT: mail title
  :MAIL_TO: person1@gmail.com
  :MAIL_CC: person2@gmail.com
  :MAIL_BCC: person3@gmail.com
  :MAIL_FROM: sender@gmail.com
  :END:"
  (interactive "P")
  (save-excursion
    (org-back-to-heading)

    (when (and htmlize-first-level
               (not (string-match "^\\* " (markdown-mime-current-line))))
      ;; go back to the 1st level substree
      (re-search-backward "^\\* ")
      (org-back-to-heading))

    (when (outline-on-heading-p nil)
      (let* ((file (buffer-file-name (current-buffer)))
             (subject (or (markdown-mime-attr "MAIL_SUBJECT")
                          (nth 4 (org-heading-components))))
             (to (markdown-mime-attr "MAIL_TO"))
             (cc (markdown-mime-attr "MAIL_CC"))
             (bcc (markdown-mime-attr "MAIL_BCC"))
             (from (markdown-mime-attr "MAIL_FROM"))
             ;; Thanks to Matt Price improving handling of cc & bcc headers
             (other-headers (markdown-mime-build-mail-other-headers cc bcc from))
             (org-export-show-temporary-export-buffer nil)
             (subtree-opts (when (fboundp 'org-export--get-subtree-options)
                             (org-export--get-subtree-options)))
             (org-export-show-temporary-export-buffer nil)
             (org-major-version (markdown-mime-org-major-version))
             ;; I wrap these bodies in export blocks because in markdown-mime-compose
             ;; they get exported again. This makes each block conditionally
             ;; exposed depending on the backend.
             (exported (save-restriction (org-narrow-to-subtree)
                                          (markdown-mime-export-buffer-or-subtree t))))
        (save-restriction
          (org-narrow-to-subtree)
          (markdown-mime-compose exported file to subject other-headers t))
        (message-goto-to)))))

(defun markdown-mime-src--remove-overlay ()
  "Remove overlay from current source buffer."
  (when (overlayp markdown-mime-src--overlay)
    (delete-overlay markdown-mime-src--overlay)))

(defun markdown-mime-edited-code ()
  "Get edited code."
  (save-excursion
    (goto-char (point-min))
    (search-forward markdown-mime-src--hint (point-max) t)
    (goto-char (line-beginning-position))
    (buffer-substring-no-properties (point) (point-max))))

(defun markdown-mime-edit-src-save ()
  "Save parent buffer with current state source-code buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (let* ((edited-code (markdown-mime-edited-code))
         (beg markdown-mime-src--beg-marker)
         (end markdown-mime-src--end-marker)
         (overlay markdown-mime-src--overlay))
    (with-current-buffer (marker-buffer markdown-mime-src--beg-marker)
      (undo-boundary)
      (goto-char beg)
      ;; Temporarily disable read-only features of OVERLAY in order to
      ;; insert new contents.
      (delete-overlay overlay)
      (delete-region beg end)
      (let* ((expecting-bol (bolp)))
        (insert edited-code)
        (when (and expecting-bol (not (bolp))) (insert "\n")))
      (save-buffer)
      (move-overlay overlay beg (point))))
  ;; `write-contents-functions' requires the function to return
  ;; a non-nil value so that other functions are not called.
  t)

(defun markdown-mime-src-mode-configure-edit-buffer ()
  "Set up clean up functions when editing source code."
  (add-hook 'kill-buffer-hook #'markdown-mime-src--remove-overlay nil 'local)
  (setq buffer-offer-save t)
  (setq-local write-contents-functions '(markdown-mime-edit-src-save)))

(defvar markdown-mime-src-mode-hook nil
  "Hook run after switching embedded org code to its `org-mode'.")

(defun markdown-mime-edit-src-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  (let* ((beg markdown-mime-src--beg-marker)
         (end markdown-mime-src--end-marker)
         (edit-buffer (current-buffer))
         (source-buffer (marker-buffer beg)))
    (markdown-mime-edit-src-save)
    (unless source-buffer (error "Source buffer disappeared.  Aborting"))
    ;; Insert modified code.  Ensure it ends with a newline character.
    (kill-buffer edit-buffer)

    ;; to the beginning of the block opening line.
    (goto-char beg)

    ;; Clean up left-over markers and restore window configuration.
    (set-marker beg nil)
    (set-marker end nil)
    (when markdown-mime--saved-temp-window-config
      (set-window-configuration markdown-mime--saved-temp-window-config)
      (setq markdown-mime--saved-temp-window-config nil))))

(defvar markdown-mime-src-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'markdown-mime-edit-src-exit)
    (define-key map (kbd "C-x C-s") 'markdown-mime-edit-src-save)
    map))

(define-minor-mode markdown-mime-src-mode
  "Minor mode for org major mode buffers generated from mail body."
  nil " OrgMimeSrc" nil
  )
(add-hook 'markdown-mime-src-mode-hook #'markdown-mime-src-mode-configure-edit-buffer)

(defun markdown-mime-src--make-source-overlay (beg end)
  "Create overlay between BEG and END positions and return it."
  (let* ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'secondary-selection)
    (let ((read-only
           (list
            (lambda (&rest _)
              (user-error
               "Cannot modify an area being edited in a dedicated buffer")))))
      (overlay-put overlay 'modification-hooks read-only)
      (overlay-put overlay 'insert-in-front-hooks read-only)
      (overlay-put overlay 'insert-behind-hooks read-only))
    overlay))

(defun markdown-mime-edit-mail-in-org-mode ()
  "Call a special editor to edit the mail body in `org-mode'."
  (interactive)
  ;; see `org-src--edit-element'
  (cond
   ((eq major-mode 'org-mode)
    (message "This command is not for `org-mode'."))
   (t
    (setq markdown-mime--saved-temp-window-config (current-window-configuration))
    (let* ((beg (copy-marker (markdown-mime-mail-body-begin)))
           (end (copy-marker (point-max)))
           (bufname "OrgMimeMailBody")
           (buffer (generate-new-buffer bufname))
           (overlay (markdown-mime-src--make-source-overlay beg end))
           (text (buffer-substring-no-properties beg end)))

      (setq markdown-mime-src--beg-marker beg)
      (setq markdown-mime-src--end-marker end)
      ;; don't use local-variable because only user can't edit multiple emails
      ;; or multiple embedded org code in one mail
      (setq markdown-mime-src--overlay overlay)

      (save-excursion
        (delete-other-windows)
        (org-switch-to-buffer-other-window buffer)
        (erase-buffer)
        (insert markdown-mime-src--hint)
        (insert text)
        (goto-char (point-min))
        (org-mode)
        (markdown-mime-src-mode))))))

(defun markdown-mime-revert-to-plain-text-mail ()
  "Revert mail body to plain text."
  (interactive)
  (let* ((txt-sep "<#part type=text/plain")
         (html-sep "<#part type=text/html>")
         mail-beg
         mail-text
         txt-beg
         txt-end)
    (save-excursion
      (goto-char (point-min))
      (setq mail-beg (search-forward mail-header-separator (point-max) t))
      (setq txt-beg (search-forward txt-sep (point-max) t))
      (setq txt-end (search-forward html-sep (point-max) t)))
    (cond
     ((and mail-beg txt-beg txt-end (< mail-beg txt-beg txt-end))
      ;; extract text mail
      (setq mail-text (buffer-substring-no-properties txt-beg
                                                      (- txt-end (length html-sep))))
      ;; delete html mail
      (delete-region mail-beg (point-max))
      ;; restore text mail
      (insert mail-text))
     (t
      (message "Can not find plain text mail.")))))

(defun markdown-mime-confirm-when-no-multipart ()
  "Prompts whether to send email if the buffer is not html-ized."
  (let ((found-multipart (save-excursion
                           (save-restriction
                             (widen)
                             (goto-char (point-min))
                             (search-forward "<#multipart type=alternative>" nil t)))))
    (when (and (not found-multipart)
               (not (y-or-n-p "It seems `markdown-mime-htmlize' is NOT called; send anyway? ")))
      (setq quit-flag t))))

































(defun markdown-mime-multipart-html-message (plain html)
  "Creates a multipart HTML email with a text part and an html part."
  (concat "<#multipart type=alternative>\n"
    "<#part type=text/plain>"
    plain
    "<#part type=text/html>\n"
    html
    "<#/multipart>\n"))

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

(defun markdown-mime-message-md-send (&optional arg)
  "Convert the current buffer and send it.
If given prefix arg ARG, skips markdown conversion."
  (interactive "P")
  (unless arg
    (markdown-mime-convert-message-to-markdown))
  (message-send))

(defun markdown-mime-message-md-send-and-exit (&optional arg)
  "Convert the current buffer and send it, then exit from mail buffer.
If given prefix arg ARG, skips markdown conversion."
  (interactive "P")
  (unless arg
    (markdown-mime-convert-message-to-markdown))
  (message-send-and-exit))

(with-eval-after-load 'message
  (define-key message-mode-map (kbd "C-c C-s") #'markdown-mime-message-md-send)
  (define-key message-mode-map (kbd "C-c C-c") #'markdown-mime-message-md-send-and-exit))





(provide 'markdown-mime)
;; Local Variables:
;; coding: utf-8
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;;; markdown-mime.el ends here
