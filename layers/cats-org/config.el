;;; config.el --- cats-org

;;; Commentary:

;; Personal functions

;;; Code:

(defvar cats//org-dir "~/org")

(defvar cats//org-mobile-dir
  (concat cats//org-dir "/mobile"))

(defvar cats//org-journal-dir
  (concat cats//org-dir "/journal"))

(defvar cats//org-mobile-inbox-file
  (concat cats//org-dir "/flagged.org"))

(defvar cats//org-gtd-file
  (concat cats//org-dir "/gtd.org"))

(defvar cats/org-habits-file
  (concat cats//org-dir "/habits.org"))

(defvar cats//org-calendar-file
  (concat cats//org-dir "/calendar.org"))

(defvar cats//org-capture-file
  (concat cats//org-dir "/capture.org"))

(defvar cats//org-logbook-file
  (concat cats//org-dir "/logbook.org"))

(defvar cats//org-inbox-file
  (concat cats//org-dir "/inbox.org"))

(defvar cats//org-refile-file
  (concat cats//org-dir "/refile.org"))

(defvar cats//org-notes-file
  (concat cats//org-dir "/notes.org"))

(defvar cats//keep-clock-running nil)

(defvar cats//org-properties-string "
:PROPERTIES:
:CREATED: %U
:END:")

;; Thanks to
;; [[http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html][endlessparentheses]]:
;; Youtube makes it pretty simple to embed videos, they give you the entire
;; iframe HTML code to use, but this wouldn't really be Emacs if we couldn't
;; make things just a little bit easier.
(defvar cats//org-yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
    " height=\"335\""
    " src=\"https://www.youtube.com/embed/%s\""
    " frameborder=\"0\""
    " allowfullscreen>%s</iframe>"))

(defvar pretty-symbols/org
  '((">=" . ?≥)
    ("<=" . ?≤)
    ("\\geq" . ?≥)
    ("\\leq" . ?≤)
    ("\\neg" . ?¬)
    ("\\rightarrow" . ?→)
    ("\\leftarrow" . ?←)
    ("\\infty" . ?∞)
    ("-->" . ?→)
    ("<--" . ?←)
    ("\\exists" . ?∃)
    ("\\nexists" . ?∄)
    ("\\forall" . ?∀)
    ("\\or" . ?∨)
    ("\\and" . ?∧)
    (":)" . ?☺)
    ("):" . ?☹)
    (":D" . ?☺)
    ("\\checkmark" . ?✓)
    ("\\check" . ?✓)
    ("1/4" . ?¼)
    ("1/2" . ?½)
    ("3/4" . ?¾)
    ("1/7" . ?⅐)
    ("1/5" . ?⅕)
    ("2/5" . ?⅖)
    ("3/5" . ?⅗)
    ("4/5" . ?⅘)
    ("1/6" . ?⅙)
    ("1/6" . ?⅚)
    ("1/6" . ?⅛)
    ("1/6" . ?⅜)
    ("1/6" . ?⅝)
    ("1/6" . ?⅞)
    ("ae" . ?æ)
    ("^_^" . ?☻))
  "Symbols for org mode.")

(defvar org-enable-jira-support nil
  "If non-nil Jira related packages are configured.")

;; (defvar bh/organization-task-id "20140625-424242-424242")

;; (defvar bh/insert-inactive-timestamp t)

;;; config.el ends here
