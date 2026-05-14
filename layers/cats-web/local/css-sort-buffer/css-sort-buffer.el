;;; css-sort-buffer.el --- Sort CSS declarations in a buffer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This local copy replaces the old EmacsWiki-backed package recipe, which is
;; no longer reliably fetchable in unattended installs.

;;; Code:

(defgroup css-sort-buffer nil
  "Sort CSS declarations in a buffer."
  :group 'editing)

(defcustom css-sort-buffer-attributes-order
  '("content"
    "display"
    "position"
    "font"
    "font-family"
    "font-size"
    "font-weight"
    "color"
    "background"
    "background-color"
    "background-image"
    "background-repeat"
    "background-position"
    "top"
    "bottom"
    "left"
    "right"
    "width"
    "line-width"
    "height"
    "line-height"
    "min-width"
    "min-height"
    "max-width"
    "max-height"
    "padding"
    "padding-top"
    "padding-bottom"
    "padding-left"
    "padding-right"
    "float"
    "clear"
    "flex-direction"
    "visibility"
    "opacity"
    "margin"
    "margin-top"
    "margin-bottom"
    "margin-left"
    "margin-right"
    "border"
    "border-radius"
    "border-top"
    "border-bottom"
    "border-left"
    "border-right"
    "border-width"
    "border-height"
    "border-top-width"
    "border-bottom-width"
    "border-left-width"
    "border-right-width"
    "border-style"
    "border-top-style"
    "border-bottom-style"
    "border-left-style"
    "border-right-style"
    "border-color"
    "border-top-color"
    "border-bottom-color"
    "border-left-color"
    "border-right-color"
    "box-shadow"
    "outline"
    "cursor"
    "overflow"
    "list-style"
    "list-style-type"
    "caption-side"
    "table-layout"
    "border-collapse"
    "border-spacing"
    "empty-cells"
    "vertical-align"
    "text-align"
    "text-indent"
    "text-transform"
    "text-decoration"
    "transform"
    "word-spacing"
    "letter-spacing"
    "white-space"
    "z-index"
    "align-items"
    "justify-content"
    "quotes")
  "Preferred declaration order for `css-sort-buffer'."
  :type '(repeat string)
  :group 'css-sort-buffer)

(defun css-sort-buffer--trim (string)
  "Trim leading and trailing whitespace from STRING."
  (replace-regexp-in-string
   (rx (or (: bos (* (any " \t\n")))
           (: (* (any " \t\n")) eos)))
   ""
   string))

(defun css-sort-buffer--attribute-name (line)
  "Return the CSS property name found in LINE."
  (css-sort-buffer--trim (car (split-string line ":" t))))

(defun css-sort-buffer--attribute-index (line)
  "Return the configured sort index for LINE.
Unknown properties sort before known ones to preserve previous behavior."
  (or (cl-position (css-sort-buffer--attribute-name line)
                   css-sort-buffer-attributes-order
                   :test #'string=
                   :from-end nil)
      -1))

(defun css-sort-buffer--compare-lines (a b)
  "Return non-nil when declaration line A should sort before B."
  (< (css-sort-buffer--attribute-index a)
     (css-sort-buffer--attribute-index b)))

(defun css-sort-buffer--block-start (position)
  "Return the first declaration line for the block around POSITION."
  (save-excursion
    (goto-char position)
    (search-backward "{")
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun css-sort-buffer--block-end (position)
  "Return the last declaration line end for the block around POSITION."
  (save-excursion
    (goto-char position)
    (re-search-forward "[{}]")
    (forward-line -1)
    (end-of-line)
    (point)))

;;;###autoload
(defun css-sort-buffer ()
  "Sort CSS declarations in each rule block of the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s-+{" nil t)
      (let* ((current (point))
             (start (css-sort-buffer--block-start current))
             (end (css-sort-buffer--block-end current))
             (lines (split-string (buffer-substring-no-properties start end) "\n"))
             (sorted-lines (sort lines #'css-sort-buffer--compare-lines)))
        (delete-region start end)
        (goto-char start)
        (insert (mapconcat #'identity sorted-lines "\n"))))))

(provide 'css-sort-buffer)

;;; css-sort-buffer.el ends here
