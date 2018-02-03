
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
