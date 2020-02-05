;;; funcs.el --- cats: Functions

;;; Commentary:

;; Personal functions

;;; Code:

(defun cats//current-file ()
  "Gets the \"file\" of the current buffer.

The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (derived-mode-p 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun cats/find-user-init-file-other-window ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun cats/recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun cats/insert-relative-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point, takes ARGS.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
          (insert (expand-file-name filename)))
    ((not (null args))
      (insert filename))
    (t
      (insert (file-relative-name filename)))))


;;; Working with file names

(defun cats/copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With \\[universal-argument] copy the absolute file name.  With
numeric prefix ARG copy the file name relative to the current
Projectile project, or to the current buffer's
`default-directory' if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let* ((file-name (cats//current-file))
           (name-to-copy
            (cond
             ((numberp arg)
              (let* ((projectile-require-project-root nil)
                     (directory (and (fboundp 'projectile-project-root)
                                     (projectile-project-root))))
                (file-relative-name file-name directory)))
             ((consp arg) file-name)
             (t (file-name-nondirectory file-name)))))
      (progn
        (kill-new name-to-copy)
        (message "%s" name-to-copy))
    (user-error "This buffer is not visiting a file")))

;;; Working with the current file
;;;###autoload
(defun cats/rename-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (t
      (rename-file filename new-name 'force-overwrite)
      (set-visited-file-name new-name 'no-query 'along-with-file)))))

;;;###autoload
(defun cats/delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (t
      (delete-file filename)
      (kill-buffer)))))

;;;###autoload
(defun cats/launch-dwim ()
  "Open the current file externally."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((marked-files (dired-get-marked-files)))
        (if marked-files
            (launch-files marked-files 'confirm)
          (launch-directory (dired-current-directory))))
    (if (buffer-file-name)
        (launch-file (buffer-file-name))
      (user-error "The current buffer is not visiting a file"))))

;;; Intellij integration
(defun cats//intellij-project-root-p (directory)
  "Determine whether DIRECTORY is an IntelliJ project root."
  (and (file-directory-p directory)
       (directory-files directory nil (rx "." (or "iml" "idea") string-end)
                        'nosort)))

(defun cats//intellij-project-root ()
  "Get the path to the nearest IntelliJ project root.

Return the absolute file name of the project root directory, or
nil if no project root was found."
  (when-let* (file-name (buffer-file-name))
    (locate-dominating-file file-name #'cats//intellij-project-root-p)))

(defun cats//intellij-launcher ()
  "Get the IntelliJ launcher for the current system."
  (pcase system-type
    (`darwin
     (when-let* (bundle (dotemacs/path-of-bundle "com.jetbrains.intellij"))
       (expand-file-name "Contents/MacOS/idea" bundle)))
    (_ (user-error "No launcher for system %S" system-type))))

(defun cats/open-in-intellij ()
  "Open the current file in IntelliJ IDEA."
  (interactive)
  (let ((idea (executable-find "idea")))
    (unless idea
      (user-error "IntelliJ launcher does not exist.
Create with Tools -> Create Command-line launcher in IntelliJ"))
    (unless (= 0 (call-process idea nil nil nil
                               "--line" (number-to-string (line-number-at-pos))
                               (expand-file-name (buffer-file-name))))
      (error "IntelliJ failed"))))

;;; URLs and browsing
(defun cats/browse-feature-url (feature)
  "Browse the URL of the given FEATURE.

Interactively, use the symbol at point, or prompt, if there is
none."
  (interactive
   (let ((symbol (or (symbol-at-point)
                     (completing-read "Feature: " features nil
                                      'require-match))))
     (list symbol)))
  (let* ((library (if (symbolp feature) (symbol-name feature) feature))
         (library-file (find-library-name library)))
    (when library-file
      (with-temp-buffer
        (insert-file-contents library-file)
        (let ((url (lm-header "URL")))
          (if url
              (browse-url url)
            (user-error "Library %s has no URL header" library)))))))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun cats//create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))


;; ignoramus/projectile integration

(defun cats//projectile-ignored-directory-p (fcn directory)
  (let ((case-fold-search ignoramus-case-insensitive))
    (string-match-p ignoramus-boring-dir-regexp directory)))

(defun cats//projectile-ignored-file-p (fcn file)
  (let ((case-fold-search ignoramus-case-insensitive))
    (string-match-p ignoramus-boring-file-regexp file)))


;; neotree/projectile integration

(defun cats//neotree-dir-from-projectile-root (dir frame-name)
  "Neotree responds to projectile root `DIR' when opening if `FRAME-NAME'."
  ;; (princ (format "frame-name: `%s''\n" frame-name))
  (when (and (string= frame-name (cats//frame-name nil))
          (and (fboundp 'projectile-project-root)
            (fboundp 'neotree-dir)))
    (let ((proj-root (or dir (projectile-project-root)))
           (neo-open (neo-global--window-exists-p)))

      (if (eq (current-buffer) (neo-global--get-buffer))
        (neo-buffer--refresh t)
        (save-excursion
          (let ((cw (selected-window)))  ;; save current window
            (let ((origin-buffer-file-name (buffer-file-name)))
              (neo-global--open-dir proj-root)
              (neotree-find proj-root)
              (neotree-find origin-buffer-file-name)
              (call-interactively 'neotree-enter))
            (recenter))))
      (unless neo-open
        (neotree-hide)))))


;; jumping, mark ring navigation

;; Allows you to navigate forward on the mark ring, while using
;; pop-to-mark-command to navigate backward. Other variants are discussed here
;; http://stackoverflow.com/questions/3393834
(defun unpop-to-mark-command ()
  "Unpop off mark ring.  Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun cats/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      (apply orig-fun args)
      (apply orig-fun args)
      (apply orig-fun args))
    (dotimes (i 10)
      (when (= p (point)) (apply orig-fun args)))))


;; projectile
(defun cats//projectile-set-find-executable (find)
  "Set the `projectile-generic-command' setting in `find' with `FIND'."
  (setq projectile-indexing-method 'alien
        projectile-generic-command (concat find " . -type f print0")))


;; zel/projectile integration
(defun projectile-zel-frecent ()
  "Show a list of recently visited files in a project by rank."
  (interactive)
  (if (boundp 'zel--frecent-list)
    (find-file (projectile-expand-root
                 (projectile-completing-read
                   "Recently visited files: "
                   (projectile-zel-frecent-files))))
    (message "zel is not enabled")))

(defun projectile-zel-frecent-files ()
  "Return a list of recently visited files in a project by rank."
  (and (boundp 'zel--frecent-list)
    (let ((project-root (projectile-project-root))
           (frecent-list (zel-frecent-file-paths)))
      (mapcar
        (lambda (f) (file-relative-name f project-root))
        (cl-remove-if-not
          (lambda (f) (string-prefix-p project-root f))
          frecent-list)))))

;;; funcs.el ends here
