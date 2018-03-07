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

(when (spacemacs/window-system-is-mac)
  ;; To edit OSX binary and xml plist files, use the compressed file framework and
  ;; the plutil provided with OSX. Emacs provides jka-compr which decompresses a
  ;; file to stdout for reading, and compresses the data from stdin to write the
  ;; file back out again.
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])

  ;;It is necessary to perform an update!
  (jka-compr-update)
)

;;; config.el ends here
