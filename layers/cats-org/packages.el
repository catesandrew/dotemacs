;;; packages.el --- cats: org

;;; Commentary:

;;; Code:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-org-packages
  '(
    (org :location built-in)
    org-bullets
    ))


(defun cats-org/pre-init-org ()
  "Add org mode hooks."
  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      ;; bootstrap with our defaults
      (add-hook 'cats/org-mode-hook 'cats/org-mode-defaults)
      ;; run our cats/org-mod-hooks with org-mode
      (add-hook 'org-mode-hook (lambda () (run-hooks #'cats/org-mode-hook))))
    :post-config
    (progn
      ;; Open links and files with RET in normal state
      (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

      (unless (file-exists-p org-directory)
        (make-directory org-directory))

      (setq my-inbox-org-file (concat org-directory "/inbox.org")
            org-indent-indentation-per-level 2
            org-use-fast-todo-selection t
            org-completion-use-ido t
            org-treat-S-cursor-todo-selection-as-state-change nil
            org-agenda-files `(,org-directory))

      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline my-inbox-org-file "INBOX")
               "* TODO %?\n%U\n%a\n")
              ("n" "Note" entry (file+headline my-inbox-org-file "NOTES")
               "* %? :NOTE:\n%U\n%a\n")
              ("m" "Meeting" entry (file my-inbox-org-file)
               "* MEETING %? :MEETING:\n%U")
              ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
               "* %?\n%U\n")))

      ;; org-mode colors
      (setq org-todo-keyword-faces
            '(
              ("INPR" . (:foreground "yellow" :weight bold))
              ("DONE" . (:foreground "green" :weight bold))
              ("IMPEDED" . (:foreground "red" :weight bold))
              ))

      (setq org-refile-targets '((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)))

      (setq org-todo-keywords
            '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

      (setq org-todo-state-tags-triggers
            ' (("CANCELLED" ("CANCELLED" . t))
               ("WAITING" ("WAITING" . t))
               ("TODO" ("WAITING") ("CANCELLED"))
               ("NEXT" ("WAITING") ("CANCELLED"))
               ("DONE" ("WAITING") ("CANCELLED"))))
      )))


;; org-bullets
(defun cats-org/pre-init-org-bullets ()
  (spacemacs|use-package-add-hook org-bullets
    :post-init
    (progn
      (setq org-bullets-bullet-list '("✿" "❀" "☢" "☯" "✸" )))))


;;; packages.el ends here
