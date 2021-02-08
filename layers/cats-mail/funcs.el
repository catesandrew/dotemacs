;;; funcs.el --- cats-mail


;; mu4e

(defun cats/gmail-integration ()
  "Gmail integration"
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

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


;; slack to org-mode
;; [[https://ag91.github.io/blog/2020/09/12/org-mode-links-for-emacs-slack/][Org mode links for Emacs Slack - Where parallels cross]]


;; slack quote region
;; https://medium.com/@justincbarclay/hacking-emacs-to-send-text-to-slack-the-quickening-1f4258b88db8
(defun cats/slack-quote-region ()
  (with-temp-buffer
    (insert region)
    (goto-char 1)
    (while (> (point-max) (point))
      (beginning-of-line)
      (insert "> ")
      (forward-line 1))
    (buffer-string)))

(defun cats//decorate-text (text)
  (let* ((decorators '(("None" . (lambda (text) text))
                        ("Code"  . (lambda (text) (concat "```" text "```")))
                        ("Quote"  . (lambda (text) (cats/slack-quote-region text)))))
          (decoration (completing-read "Select decoration: "
                        decorators
                        nil
                        t)))
    (funcall (cdr (assoc decoration decorators)) text)))

(defun cats/send-region-to-slack ()
  "Send region to slack as code, quote or plain."
  (interactive)
  (let* ((team (slack-team-select))
          (room (slack-room-select
                  (cl-loop for team in (list team)
                    append (append (slack-team-ims team)
                             (slack-team-groups team)
                             (slack-team-channels team)))
                  team)))
    (slack-message-send-internal (cats//decorate-text (filter-buffer-substring
                                                     (region-beginning) (region-end)))
      room
      team)))
