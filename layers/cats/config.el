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

;; Jump to bug references from code
(add-hook 'text-mode-hook #'bug-reference-mode)

;; --no-color, oddly enough, is required to allow emacs to colorize the output
(defvar cats/git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'.")

;;; config.el ends here
