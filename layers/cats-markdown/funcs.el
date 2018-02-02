;;; funcs.el --- cats-markdown: Functions

;;; Commentary:

;; Markdown functions.

;;; Code:

(require 'skeleton)
(require 'rx)
(require 'subr-x)

(define-skeleton cats-markdown/post-header
  "Insert a header for blog posts."
  (read-from-minibuffer "Title: ")
  "---\n"
  "title: " str "\n"
  "---\n\n"
  -)

(defun cats-markdown/publish-jekyll-draft ()
  "Publish a Jekyll draft in this buffer as a post."
  (interactive)
  (let ((current-dir (directory-file-name (file-name-directory (buffer-file-name))))
        (source-dir (locate-dominating-file (buffer-file-name) "_config.yml")))
    (unless source-dir
      (user-error "Failed to find _config.yml.  This file does not seem to be part of a Jekyll site"))
    (unless (string= (file-name-base current-dir) "_drafts")
      (user-error "This buffer is not a draft"))
    (let* ((post-dir (expand-file-name "_posts/" source-dir))
           (new-name (concat (format-time-string "%F-")
                             (file-name-nondirectory (buffer-file-name))))
           (target (expand-file-name new-name post-dir)))
      (rename-file (buffer-file-name) target)
      (set-visited-file-name target 'no-query 'along-with-file)
      (message "Moved to %s" target))))


;; handlebars
(defun cats//locate-handlebars-from-projectile (&optional dir)
  "Use local handlebars from DIR or `projectile-project-root'."
  (let ((proj-root (or dir (projectile-project-root)))
        (proj-type (projectile-detect-project-type)))
    (when (string= proj-type "npm")
      (async-start
       `(lambda ()
          (set  'proj-root ,proj-root)
          (let ((default-directory proj-root))
            (executable-find "handlebars")))
       (lambda (result)
         (if result
             (cats/set-executable-handlebars result)
           (message "Unable to locate handlebars."))
         )))))

(defun cats//hbs-set-handlebars-executable (handlebars)
  "Set the `flycheck-handlebars-executable' setting in `handlebarse' with `HANDLEBARS'."
  (setq flycheck-handlebars-executable handlebars))

;;; funcs.el ends here
