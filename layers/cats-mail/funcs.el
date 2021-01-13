;;; funcs.el --- cats-mail


;; mu4e

(defun cats/enter-mu4e-context-work ()
  (setq mu4e-sent-folder   "/work/Sent"
    mu4e-drafts-folder "/work/Drafts"
    mu4e-trash-folder  "/work/Trash"
    mu4e-refile-folder "/work/All"
    mu4e-change-filenames-when-moving t
    mu4e-maildir-shortcuts
    '((:maildir "/work/Inbox"       :key ?i)
       (:maildir "/work/Sent"  :key ?s)
       (:maildir "/work/All"   :key ?a)
       (:maildir "/work/Trash" :key ?t))))

(defun cats/enter-mu4e-context-gmail ()
  (setq mu4e-sent-folder   "/gmail/Sent"
    mu4e-drafts-folder "/gmail/Drafts"
    mu4e-trash-folder  "/gmail/Trash"
    mu4e-refile-folder "/gmail/All"
    mu4e-change-filenames-when-moving t
    mu4e-maildir-shortcuts
    '((:maildir "/gmail/Inbox"       :key ?i)
       (:maildir "/gmail/Sent"  :key ?s)
       (:maildir "/gmail/All"   :key ?a)
       (:maildir "/gmail/Trash" :key ?t))))
