;;; funcs.el --- cats-mail


;; mu4e

(defun cats/gmail-integration ()
  "Gmail integration"
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete

    ;; don't need to run cleanup after indexing for gmail
    mu4e-index-cleanup nil

    ;; because gmail uses labels as folders we can use lazy check since
    ;; messages don't really "move"
    mu4e-index-lazy-check t)

  ;; In my workflow, emails won't be moved at all. Only their flags/labels are
  ;; changed. Se we redefine the trash and refile marks not to do any moving.
  ;; However, the real magic happens in `+mu4e-gmail-fix-flags-h'.
  ;;
  ;; Gmail will handle the rest.
  (defun +mu4e--mark-seen (docid _msg target)
    (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))

  (delq! 'delete mu4e-marks #'assq)
  (setf (alist-get 'trash mu4e-marks)
    (list :char '("d" . "▼")
      :prompt "dtrash"
      :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
      :action #'+mu4e--mark-seen)
    ;; Refile will be my "archive" function.
    (alist-get 'refile mu4e-marks)
    (list :char '("r" . "▼")
      :prompt "rrefile"
      :dyn-target (lambda (_target msg) (mu4e-get-refile-folder msg))
      :action #'+mu4e--mark-seen))

  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (add-hook 'mu4e-mark-execute-pre-hook
    (defun +mu4e-gmail-fix-flags-h (mark msg)
      (pcase mark
        (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
        (`refile (mu4e-action-retag-message msg "-\\Inbox"))
        (`flag   (mu4e-action-retag-message msg "+\\Starred"))
        (`unflag (mu4e-action-retag-message msg "-\\Starred"))))))

(defun cats/enter-mu4e-context-work ()
  (setq mu4e-sent-folder "/work/Sent"
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
  (setq mu4e-sent-folder "/gmail/Sent"
    mu4e-drafts-folder "/gmail/Drafts"
    mu4e-trash-folder "/gmail/Trash"
    mu4e-refile-folder "/gmail/All"
    mu4e-change-filenames-when-moving t
    mu4e-maildir-shortcuts
    '((:maildir "/gmail/Inbox"       :key ?i)
       (:maildir "/gmail/Sent"  :key ?s)
       (:maildir "/gmail/All"   :key ?a)
       (:maildir "/gmail/Trash" :key ?t))))
