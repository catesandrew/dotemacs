;;; config.el --- cats: Configuration

;;; Commentary:

;; My personal configuration.

;;; Code:

(require 'time-date)

(defvar cats/verbose nil)

(defconst emacs-version-short
  (replace-regexp-in-string
   "\\([0-9]+\\)\\.\\([0-9]+\\).*"
   "\\1_\\2" emacs-version))

;; TODO Are these even necessary?
;; Reuse frames if possible
;; (add-to-list 'display-buffer-alist
;;              '("." nil (reusable-frames . visible))
;;              'append)

;; TODO Set these in text-mode
;; More space between lines, makes text easier to read
;; (setq-default line-spacing 0.1
;;               highlight-tabs t
;;               tab-width 8
;;               sentence-end-double-space nil)

;; Jump to bug references from code
(add-hook 'text-mode-hook #'bug-reference-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

;; --no-color, oddly enough, is required to allow emacs to colorize the output
(defvar cats/git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'.")


;;; config.el ends here
