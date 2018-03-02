;;; funcs.el --- cats: Functions

;;; Commentary:

;; Personal functions

;;; Code:

(require 'subr-x)
(require 'lisp-mnt)
(require 'find-func)

(defun cats//insert-page-break-line ()
  "Insert a new line with a page break at point."
  (interactive)
  (insert "\n\n"))

(defun cats//insert-current-date (iso)
  "Insert the current date at point.

When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

(defun cats/snapshot-version-p (version)
  "Whether VERSION is an Emacs snapshot version."
  (pcase-let ((`(,_ ,_ ,build) (version-to-list version)))
    ;; Snapshots with build numbers > 90 are pretests which come from proper
    ;; release tarballs and don't need to be rebuild weekly
    (and (>= build 50) (<= build 90))))

;; More refined font setup, providing math and emoji support.  Needs:
;;
;; - XITS Math (https://github.com/khaledhosny/xits-math) as fallback for math
;;
;; Currently this setup only works for OS X, as we rely on Apple's Emoji and
;; Symbol fonts.

;; Font setup
(defun cats-configure-fonts (frame)
  "Set up fonts for FRAME.

Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ

  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(when (spacemacs/window-system-is-mac)
  (when-let* ((frame (selected-frame)))
    (cats-configure-fonts frame))
  (add-hook 'after-make-frame-functions #'cats-configure-fonts))


;; utils
(defun chomp (str)
  "Chomp leading and tailing whitespace from `str'."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun empty-string-p (string)
  "Return true if the string is empty or nil. Expects string."
  (or (null string)
      (zerop (length (string-trim string)))))

;; grep
(defun rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

(defun cats//grep-set-find-executable (find)
  "Set the `find-program' setting in `find' with `FIND'."
  (setq find-program find))


;; osx
(when (eq system-type 'darwin)
  (defun open-current-file ()
    "Open current file using shell `open` command"
    (interactive)
    (shell-command (concat "open " (buffer-file-name))))

  (defun get-keychain-password (account-name)
    "Get `account-name' keychain password from OS X Keychain"
    (interactive "sAccount name: ")
    (when (executable-find "security")
      (chomp
       (shell-command-to-string
        (concat
         "security find-generic-password -wa "
         account-name))))))


;; helm
(defun helm-projectile-git-ls-files ()
  "Runs `helm-multi-files`, but first primes the `helm-ls-git` file lists."
  (interactive)
  (cats/helm-ls-git-ls)
  (helm-multi-files))

(defun cats/helm-ls-git-ls ()
  (interactive)
  (when (not (helm-ls-git-not-inside-git-repo))
    (unless (and helm-source-ls-git
                 helm-source-ls-git-buffers)
      (setq helm-source-ls-git (helm-make-source "Git files" 'helm-ls-git-source
                                 :fuzzy-match helm-ls-git-fuzzy-match)
            helm-source-ls-git-buffers (helm-make-source "Buffers in project" 'helm-source-buffers
                                         :header-name #'helm-ls-git-header-name
                                         :buffer-list (lambda () (helm-browse-project-get-buffers
                                                             (helm-ls-git-root-dir))))))))


;; flycheck
(defun cats//set-tidy-executable (tidy)
  "Set the `flycheck-html-tidy-executable' setting with `TIDY'."
  (setq flycheck-html-tidy-executable tidy))

(defun cats//flycheck-turn-on-maybe ()
  (unless
      (or
       buffer-read-only
       (hardhat-buffer-included-p (current-buffer))
       (cats//current-buffer-remote-p))
    (flycheck-mode)))

(defun flycheck-remove-next-checker (checker next)
  "Remove `NEXT' from `CHECKER'."
  (let ((next-checkers (flycheck-checker-get checker 'next-checkers)))
    (when (member next next-checkers)
      (setf (get checker 'flycheck-next-checkers) (remove next next-checkers))
      ;; (setq nth 0)
      ;; (dolist (next-checker next-checkers)
      ;;   (when (equal next-checker next)
      ;;     (if (zerop nth)
      ;;         (setf (get checker 'flycheck-next-checkers) '())
      ;;       (let ((last (nthcdr (1- nth) next-checkers)))
      ;;         (setcdr last (cddr last))
      ;;         next-checkers)))
      ;;   (setq nth (+ nth 1)))
      )))

(defun flycheck-add-next-checker (checker next)
  "Add `NEXT' to `CHECKER'."
  (let ((next-checkers (flycheck-checker-get checker 'next-checkers)))
    (unless (member next next-checkers)
      (flycheck-add-next-checker checker next 'append))))


;; tramp
(defun cats//current-buffer-remote-p ()
  "Is the current buffer remote?"
  ;; (file-remote-p buffer-file-name 'method)
  (-any? 'file-remote-p
         (remove nil (list
                      buffer-file-name
                      list-buffers-directory
                      default-directory))))



;; yasnippet

;; whitespace removing functions from Magnar Sveen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas-s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun yas-s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun yas-s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (yas-s-trim-left (yas-s-trim-right s)))

(defun yas-string-reverse (str)
  "Reverse a string STR manually to be compatible with emacs versions < 25."
  (apply #'string
         (reverse
          (string-to-list str))))

(defun yas-trimmed-comment-start ()
  "This function returns `comment-start' trimmed by whitespaces."
  (yas-s-trim comment-start))

(defun yas-trimmed-comment-end ()
  "This function returns `comment-end' trimmed by whitespaces if `comment-end' is not empty.
Otherwise the reversed output of function `yas-trimmed-comment-start' is returned."
  (if (eq (length comment-end) 0)
      (yas-string-reverse (yas-trimmed-comment-start))
    (yas-s-trim comment-end)))

;;; funcs.el ends here
