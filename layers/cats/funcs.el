;;; funcs.el --- cats: Functions

;;; Commentary:

;; Personal functions

;;; Code:

(require 'subr-x)
(require 'lisp-mnt)
(require 'find-func)

(defun cats/insert-page-break-line ()
  "Insert a new line with a page break at point."
  (interactive)
  (insert "\n\n"))

(defun cats/insert-current-date (iso)
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
(defun cats//helm-wrap-with-boring-buffer-regexp-list (orig-fun &rest args)
  (let* ((helm-boring-buffer-regexp-list
           (append
             (butlast spacemacs-useless-buffers-regexp)
             helm-boring-buffer-regexp-list))
          (helm-ff-skip-boring-files t))
    (apply orig-fun args)))

(defun cats//add-advice-around-helm-buffers-list ()
  (advice-add 'helm-buffers-list :around 'cats//helm-wrap-with-boring-buffer-regexp-list))

(defun cats//add-advice-around-helm-mini ()
  (advice-add 'helm-mini :around 'cats//helm-wrap-with-boring-buffer-regexp-list))

(defun cats//add-advice-around-helm-multi-files ()
  (advice-add 'helm-multi-files :around 'cats//helm-wrap-with-boring-buffer-regexp-list))

(defun helm-projectile-git-ls-files ()
  "Runs `helm-multi-files`, but first primes the `helm-ls-git` file lists."
  (interactive)
  (cats/helm-ls-git-ls)
  (helm-multi-files))

(defun cats/helm-ls-git-ls ()
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

(defun cats//flycheck-remove-next-checker (checker next)
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

(defun cats//flycheck-add-next-checker (checker next)
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


;; compilation
(defun cats/open-compilation-window ()
  "Open the window containing the '*compilation*' buffer."
  (interactive)
  (with-current-buffer "*compilation*"
    (switch-to-buffer (current-buffer))))


;; magit-repos

(defun cats/open-in-git-link (&optional _button)
  "Open the github project page for the repository at point."
  (interactive)
  (require 'git-link)
  (--if-let (tabulated-list-get-id)
    (let ((default-directory (expand-file-name it)))
      (let* ((remote (git-link--select-remote))
              (remote-host (git-link--remote-host (git-link--select-remote)))
              (handler (git-link--handler git-link-remote-alist remote-host)))
        (cond
          ((null remote-host)
            (message "Remote `%s' is unknown or contains an unsupported URL" remote))
          ((not (functionp handler))
            (message "No handler for %s" remote-host))
          ((git-link--new
             (funcall handler
               remote-host
               (git-link--remote-dir remote)))))))
    (user-error "There is no repository at point")))

(defun cats/open-in-projectile (&optional _button)
  "Open the projectile project for the repository at point."
  (interactive)
  (--if-let (tabulated-list-get-id)
    (projectile-switch-project-by-name (expand-file-name it))
    ;; (magit-status-internal (expand-file-name it))
    (user-error "There is no repository at point")))

(defun cats/repolist-pull-ff-only ()
  "Fetch all remotes in repositories returned by `magit-list-repos'.
Fetching is done synchronously."
  (interactive)
  (run-hooks 'magit-credential-hook)
  (let* ((repos (magit-list-repos))
          (l (length repos))
          (i 0))
    (dolist (repo repos)
      (let* ((default-directory (file-name-as-directory repo))
              (msg (format "(%s/%s) Pulling --ff-only in %s..."
                     (cl-incf i) l default-directory)))
        (message msg)
        (magit-run-git "pull" "--ff-only" "origin" (magit-fetch-arguments))
        (message (concat msg "done")))))
  (magit-refresh))

(defun cats/repolist-fetch ()
  "Fetch all remotes in repositories returned by `magit-list-repos'.
Fetching is done synchronously."
  (interactive)
  (run-hooks 'magit-credential-hook)
  (let* ((repos (magit-list-repos))
         (l (length repos))
         (i 0))
    (dolist (repo repos)
      (let* ((default-directory (file-name-as-directory repo))
             (msg (format "(%s/%s) Fetching in %s..."
                          (cl-incf i) l default-directory)))
        (message msg)
        (magit-run-git "remote" "update" (magit-fetch-arguments))
        (message (concat msg "done")))))
  (magit-refresh))

(defun cats/repolist-fetch-async ()
  "Fetch all remotes in repositories returned by `magit-list-repos'.
Fetching is done asynchronously."
  (interactive)
  (run-hooks 'magit-credential-hook)
  (dolist (repo (magit-list-repos))
    (let ((default-directory (file-name-as-directory repo)))
      (magit-run-git-async "remote" "update" (magit-fetch-arguments)))))

(defun cats/magit-repolist-call-command (command)
  "Read a command and run it in repositories returned by `magit-list-repos'.

If the COMMAND does its job asynchronously, then that likely
won't be done for all repositories by the time this function
returns.  If it does its job synchronously, then doing it
many times might take a long time."
  (interactive (list (read-command "Call in all repositories: ")))
  (magit-with-repositories
    (call-interactively command))
  (magit-refresh))


;; gh
(defun cats//toggle-gh-profile (dir frame-name)
  (when (string= frame-name (cats//frame-name nil))
    (let* ((remote (git-link--select-remote))
            (id (gh-profile-get-remote-profile (git-link--remote-url remote))))
      (when id
        (setq gh-profile-current-profile id)
        (setq gh-profile-default-profile id)))))

;;; funcs.el ends here
