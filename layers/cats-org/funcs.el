;;; funcs.el --- cats-org

;;; Commentary:

;; Personal functions

;;; Code:



;; Define some shortcuts to access major org files.

(defun cats/org-find-notes ()
  (interactive)
  (find-file "~/org/notes.org"))

(defun cats/org-find-private ()
  (interactive)
  (find-file "~/org/private.org"))

(defun cats/org-find-workhours ()
  (interactive)
  (find-file "~/org/workhours.org"))

(defun cats/org-find-bba ()
  (interactive)
  (find-file "~/org/bba.org"))

(defun cats/org-find-work ()
  (interactive)
  (if (equal "work" (getenv "SYSENV"))
    (find-file "~/workorg/work.org")
    (find-file "~/org/work.org")))

(defun cats/org-find-emacs ()
  (interactive)
  (if (file-exists-p "~/org/emacs.org")
    (find-file "~/org/emacs.org")
    (find-file "~/.emacs.d/org/emacs.org")))


;; personal org-mode defaults

(defun cats/org-mode-defaults ()
  "Default coding hook, useful with `org-mode'."
  (unless (bound-and-true-p my-omh-ran)
    (set (make-local-variable 'my-omh-ran) t)

    (cats/highlight-org-mode-words)
    ;; prettify and enable locally
    (when (member major-mode
                  '(org-mode))
      (cats/pretty-symbols pretty-symbols/org))
    (spacemacs/toggle-prettify-symbols-mode-on)
    ))


;; prettify, font-lock
(defun cats/highlight-org-mode-words ()
  "Highlight keywords in comments."
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|WAITING\\|CANCELLED\\|NEXT\\|IMPEDED\\|DONE\\|INPR\\|TODO\\|NOTE\\|INBOX\\|NOTES\\|MEETING\\|JOURNAL\\)\\>:?\\)"
          1 font-lock-warning-face t)))

  (font-lock-add-keywords
   'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                (1 font-lock-comment-face prepend)
                (2 font-lock-function-name-face)
                (3 font-lock-comment-face prepend)))))
;;; funcs.el ends here
