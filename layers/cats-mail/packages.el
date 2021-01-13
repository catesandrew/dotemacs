;;; packages.el --- cats: org

;;; Commentary:

;;; Code:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-mail-packages
  '(
     (mu4e :location site)
     ))


;; mu4e

;; mu init --maildir=~/.mail --my-address=catesandrew@gmail.com --my-address=acates@happymoney.com
;; mu index

(defun cats-mail/pre-init-mu4e ()
  (spacemacs|use-package-add-hook mu4e
    :post-init
    (progn


      ;; Set up some common mu4e variables
      (setq mu4e-maildir "~/.mail"
        ;; mu4e-context-policy 'pick-first
        ;; mu4e-confirm-quit nil
        ;; mu4e-get-mail-command "mbsync -a"
        ;; mu4e-update-interval nil
        ;; mu4e-update-interval 120
        ;; mu4e-compose-signature-auto-include nil
        ;; mu4e-view-show-images t
        ;; mu4e-view-show-addresses t
        )

      ;; Bookmarks
      ;; (setq mu4e-bookmarks
      ;;   `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
      ;;      ("date:today..now" "Today's messages" ?t)
      ;;      ("date:7d..now" "Last 7 days" ?w)
      ;;      ("mime:image/*" "Messages with images" ?p)
      ;;      (,(mapconcat 'identity
      ;;          (mapcar
      ;;            (lambda (maildir)
      ;;              (concat "maildir:" (car maildir)))
      ;;            mu4e-maildir-shortcuts) " OR ")
      ;;        "All inboxes" ?i)))

      (setq sendmail-program "msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail))
    :post-config
    (progn
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
      )))

;;; packages.el ends here
