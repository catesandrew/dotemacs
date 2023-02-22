;;; packages.el --- cats: org

;;; Commentary:

;;; Code:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-mail-packages
  '(
     ;; (mu4e :location site)
     slack
     persp-mode
     (notmuch :location built-in)
     ;; mu4e-views
     ;; (markdown-mime :location local)
     ))



;; mu4e-views
(defun cats-mail/init-mu4e-views ()
  (use-package mu4e-views
    :after mu4e
    :config
    ;; use ivy for completion
    (setq mu4e-views-completion-method 'helm)
    ;; make xwidgets default
    (setq mu4e-views-default-view-method "html")
    ;; when pressing n and p stay in the current window
    (mu4e-views-mu4e-use-view-msg-method "html")
    ;; select the default when pressing n and p stay in the current window
    (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
    ;; automatically open messages when moving in the headers view
    (setq mu4e-views-auto-view-selected-message t)))


;; markdown-mime
(defun cats-mail/init-markdown-mime ()
  (use-package markdown-mime
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'message-mode
        "em" 'markdown-mime-htmlize)
      (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
        "em" 'markdown-mime-org-buffer-htmlize))))


;; mu4e

;; mu init --maildir=~/.mail --my-address=catesandrew@gmail.com --my-address=acates@happymoney.com
;; mu index
(defun cats-mail/pre-init-mu4e ()
  (spacemacs|use-package-add-hook mu4e
    :post-init
    (progn
      ;; Set up some common mu4e variables
      (setq mu4e-maildir (expand-file-name "~/.mail")
        ;; note: org-msg-mode has to be enabled first before composing and set mail user agent to
        ;; mail-user-agent 'mu4e-user-agent
        ;; mu4e-org-support nil
        mu4e-compose-dont-reply-to-self t
        ;; every new email composition gets its own frame!
        ;; mu4e-compose-in-new-frame t
        mu4e-headers-include-related t
        mu4e-headers-visible-lines 20
        ;; enable inline images
        mu4e-attachment-dir
          (lambda (&rest _)
            (expand-file-name ".attachments" (mu4e-root-maildir)))
        mu4e-headers-auto-update nil
        org-mu4e-convert-to-html t
        mu4e-compose-signature-auto-include nil
        mu4e-compose-format-flowed t
        ;; don't keep message buffers around
        message-kill-buffer-on-exit t
        mu4e-installation-path "/usr/local/share/emacs/site-lisp"
        mu4e-use-maildirs-extension t
        mu4e-enable-notifications nil
        mu4e-date-format "%y/%m/%d"
        mu4e-headers-date-format "%Y/%m/%d"
        mu4e-view-prefer-html t
        mu4e-show-images t
        mu4e-view-image-max-width 800
        mu4e-enable-mode-line nil
        ;; mu4e-html2text-command "html2text"
        ;; mu4e-enable-async-operations t
        ;; get-mail-command set to true because mail sync is happening via
        ;; external processes but we still need to re-index the mailstore to
        ;; realize the updates
        mu4e-get-mail-command "true"
        ;; mu4e-context-policy 'pick-first
        mu4e-confirm-quit nil
        ;; mu4e-update-interval 300
        ;; mu4e-compose-signature-auto-include nil
        ;; mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-headers-fields
        '((:account . 12)
           (:human-date . 12)
           (:flags . 4)
           (:from . 25)
           (:subject)))

      ;; Use fancy icons
      (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark '("D" . "")
        mu4e-headers-flagged-mark '("F" . "")
        mu4e-headers-new-mark '("N" . "")
        mu4e-headers-passed-mark '("P" . "")
        mu4e-headers-replied-mark '("R" . "")
        mu4e-headers-seen-mark '("S" . "")
        mu4e-headers-trashed-mark '("T" . "")
        mu4e-headers-attach-mark '("a" . "")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark '("s" . "")
        mu4e-headers-unread-mark '("u" . ""))

      ;; My spelling is terrile a little joke
      (add-hook 'mu4e-compose-mode-hook 'turn-on-flyspell)
      (add-hook 'mu4e-compose-mode-hook 'turn-on-auto-fill)

      (setq sendmail-program "msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail))
    :post-config
    (progn
      ;; Add a column to display what email account the email belongs to.
      (add-to-list 'mu4e-header-info-custom
        '(:account
           :name "Account"
           :shortname "Account"
           :help "Which account this email belongs to"
           :function
           (lambda (msg)
             (let ((maildir (mu4e-message-field msg :maildir)))
               (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

      ;; Html mails might be better rendered in a browser
      (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))

      (cats/gmail-integration)
      (setq mu4e-contexts
        `(,(make-mu4e-context
             :name "work"
             :enter-func (lambda () (progn
                                 (mu4e-message "Entering work context")
                                 (cats/enter-mu4e-context-work)))
             :leave-func (lambda () (progn
                                 ;; (mu4e-clear-caches)
                                 (mu4e-message "Leave work context")))
             ;; We match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/work" (mu4e-message-field msg :maildir))
                             ;; (mu4e-message-contact-field-matches msg :to "acates@happymoney.com")
                             ))
             :vars '((user-mail-address . "acates@happymoney.com"  )
                      (user-full-name . "Andrew Cates" )))
           ,(make-mu4e-context
              :name "gmail"
              :enter-func (lambda () (progn
                                  (mu4e-message "Entering gmail context")
                                  (cats/enter-mu4e-context-gmail)))
              :leave-func (lambda () (progn
                                  ;; (mu4e-clear-caches)
                                  (mu4e-message "Leave gmail context")))
              ;; We match based on the contact-fields of the message
              :match-func (lambda (msg)
                            (when msg
                              (string-match-p "^/gmail" (mu4e-message-field msg :maildir))
                              ;; (mu4e-message-contact-field-matches msg :to "catesandrew@gmail.com")
                              ))
              :vars '((user-mail-address . "catesandrew@gmail.com"  )
                       (user-full-name . "Andrew Cates")))

           ;; ,(make-mu4e-context
           ;;   :name "gmail-folder"
           ;;   :enter-func (lambda () (mu4e-message "Switch to the GMail context"))
           ;;   ;; no leave-func
           ;;   ;; we match based on the maildir of the message
           ;;   ;; this matches maildir /gmail and its sub-directories
           ;;   :match-func (lambda (msg)
           ;;                 (when msg
           ;;                   (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
           ;;   :vars '((user-mail-address	. "catesandrew@gmail.com")
           ;;           (user-full-name	  . "Andrew Cates")))

           ;; ,(make-mu4e-context
           ;;   :name "work-folder"
           ;;   :enter-func (lambda () (mu4e-message "Switch to the work context"))
           ;;   ;; no leave-func
           ;;   ;; we match based on the maildir of the message
           ;;   ;; this matches maildir /gmail and its sub-directories
           ;;   :match-func (lambda (msg)
           ;;                 (when msg
           ;;                   (string-match-p "^/work" (mu4e-message-field msg :maildir))))
	         ;;   :vars '( ( user-mail-address	     . "acates@happymoney.com" )
		       ;;            ( user-full-name	     . "Andrew Cates" )))
           ))

      ;; Bookmarks
      (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
           ("date:today..now" "Today's messages" ?t)
           ("date:7d..now" "Last 7 days" ?w)
           ("mime:image/*" "Messages with images" ?p)
           (,(mapconcat 'identity
               (mapcar
                 (lambda (maildir)
                   (concat "maildir:" (car maildir)))
                 mu4e-maildir-shortcuts) " OR ")
             "All inboxes" ?i)))
      )))



;; slack
(defun cats-mail/pre-init-slack ()
  (spacemacs|use-package-add-hook slack
    :post-init
    (progn
      (with-eval-after-load 'alert
        (add-to-list
          'alert-user-configuration
          '(((:message . "@acates\\")
              (:title . "\\(uie\\|frontend-eng\\|skynet\\|frontend-eng-redalert\\)")
              (:category . "slack"))
             libnotify nil)))

      (setq
        ;; Ensure the buffer exists when a message
        ;; arrives on a channel that wasn't open.
        slack-buffer-create-on-notify t
        slack-buffer-emojify t
        slack-render-image-p nil
        slack-prefer-current-team t
        slack-display-team-name nil)
      )
    :post-config
    (progn
      (slack-register-team
        :name "Happy Money"
        :default t
        :client-id "acates@happymoney.com"
        :token (password-store-get "work/slack-token")
        :full-and-display-names t
        :subscribed-channels '(uie frontend-eng))

      ;; I’ll never know who thought user statuses were a good idea for Slack.
      ;; But, thanks to a tip by _asummers on HackerNews, I can live in a world
      ;; where they don’t exist.
      (defun slack-user-status (_id _team) "")

      ;; I like notifications with minimal titles, and the package is kind enough
      ;; to make these configurable.

      ;; Channels
      (setq slack-message-notification-title-format-function
        (lambda (_team room threadp)
          (concat (if threadp "Thread in #%s") room)))

      (defun endless/-cleanup-room-name (room-name)
        "Make group-chat names a bit more human-readable."
        (replace-regexp-in-string
          "--" " "
          (replace-regexp-in-string "#Happy Money -" "" room-name)))

      ;; Private messages and group chats
      (setq slack-message-im-notification-title-format-function
        (lambda (_team room threadp)
          (concat (if threadp "Thread in %s")
            (endless/-cleanup-room-name room))))

      ;; Go to any channel with `C-x j'.
      ;; (define-key ctl-x-map "j" #'slack-select-rooms)
      ;; I thumbs-up a lot. Don't judge me.
      ;; (define-key slack-mode-map (kbd "C-;") ":+1:")

      ;; Bring up the mentions menu with `@', and insert a space afterwards.
      (define-key slack-mode-map "@"
        (defun endless/slack-message-embed-mention ()
          (interactive)
          (call-interactively #'slack-message-embed-mention)
          (insert " ")))
      )))

(defun cats-mail/pre-init-persp-mode ()
  ;; add 10 sec delay before showing list of channels
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (spacemacs|define-custom-layout slack-spacemacs-layout-name
        :binding slack-spacemacs-layout-binding
        :body
        (progn
          (add-hook 'slack-mode #'spacemacs//slack-buffer-to-persp)
          (call-interactively 'slack-start)
          (sleep-for 10)
          (call-interactively 'slack-channel-select)))

      ;; add org-msg buffers to perspective
      (spacemacs|define-custom-layout mu4e-spacemacs-layout-name
        :binding mu4e-spacemacs-layout-binding
        :body
        (progn
          (defun spacemacs-layouts/add-mu4e-buffer-to-persp ()
            (persp-add-buffer (current-buffer)
              (persp-get-by-name
                mu4e-spacemacs-layout-name)))
          (spacemacs/add-to-hooks 'spacemacs-layouts/add-mu4e-buffer-to-persp
            '(mu4e-main-mode-hook
               mu4e-headers-mode-hook
               mu4e-view-mode-hook
               mu4e-compose-mode-hook
               org-msg-edit-mode-hook))
          (call-interactively 'mu4e)
          (call-interactively 'mu4e-update-index)

          (define-advice mu4e~stop (:after nil kill-mu4e-layout-after-mu4e~stop)
            (when mu4e-spacemacs-kill-layout-on-exit
              (persp-kill mu4e-spacemacs-layout-name))))))))


;; notmuch
(defun cats-mail/pre-init-notmuch ()
  (spacemacs|use-package-add-hook notmuch
    :post-init
    (progn
      (setq notmuch-fcc-dirs nil
        notmuch-show-logo nil
        notmuch-message-headers-visible nil
        message-kill-buffer-on-exit t
        message-send-mail-function 'message-send-mail-with-sendmail
        notmuch-search-oldest-first nil
        send-mail-function 'sendmail-send-it
        ;; sendmail-program "/usr/local/bin/msmtp"
        notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-72s ")
          ("tags" . "(%s)"))
        notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread)))
        notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-alltags)
        notmuch-saved-searches
        '((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged"             :key "f")
          (:name "sent"    :query "tag:sent"                :key "s")
          (:name "drafts"  :query "tag:draft"               :key "d"))
        notmuch-archive-tags '("-inbox" "-unread"))

      ;; only unfold unread messages in thread by default
      (add-hook 'notmuch-show-hook #'+notmuch-show-expand-only-unread-h))))

;;; packages.el ends here
