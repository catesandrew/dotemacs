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
    (spacemacs/toggle-prettify-symbols-mode-on)))

(defun cats/highlight-org-mode-words ()
  "Highlight keywords in comments."
  (interactive)
  (font-lock-add-keywords
    nil '(("\\<\\(\\(IDEA\\|RESEARCH\\|STARTED\\|WAIT\\|BACKLOG\\|HANDLED\\|EXPIRED\\|CANCELLED\\|NEXT\\|IMPEDED\\|DONE\\|INPR\\|TODO\\|NOTE\\|INBOX\\|NOTES\\|MEETING\\|JOURNAL\\)\\>:?\\)"
          1 font-lock-warning-face t)))

  (font-lock-add-keywords
   'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                (1 font-lock-comment-face prepend)
                (2 font-lock-function-name-face)
                (3 font-lock-comment-face prepend)))))


;; ox-jira
(defun cats/org-export-jira-clipboard()
  "narrow to org subtree and store content in jira format in clipboard."
  (interactive)
  (org-narrow-to-subtree)
  (cats/export-jira-org)
  (let ((org-export-use-babel nil))
    (ox-jira-export-as-jira))
  (delete-region (point) (progn (forward-line 1)(point)))
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min))
  (kill-ring-save 1 1 1)
  (other-window -1)
  (widen)
  (other-window 1))

;; Besides the clipboard we want also an org file in =/tmp/= to attach to the
;; jira ticket. Of course some boiler plate for the poor souls who never heard
;; of orgmode.
(defun cats/export-jira-org()
  "export current narrowed view to file in tmp and open a finder window on OS-X."
  (interactive)
  (goto-char (point-min))
  (insert "# This file is just plain text called orgmode")
  (newline)
  (insert "# https://en.wikipedia.org/wiki/Org-mode")
  (newline)
  (insert "# You can open it in any text editor or file reader.")
  (newline)
  (insert "# You might want to use Emacs for best experience.")
  (newline)
  (if (re-search-forward "jira:" nil t 1)
      (if (org-in-regexp org-bracket-link-regexp 1)
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description (last (split-string (if (match-end 3)
                                                     (match-string-no-properties 3)
                                                   (match-string-no-properties 1)) ":"))))
            (push-mark (point))
            (push-mark (point-max) nil t)
            (goto-char (point-min))
            (write-region (mark) (point) (concat "/tmp/" (car description) ".org") nil nil )
            (deactivate-mark)
            (if (string-equal system-type "darwin")(shell-command "open /tmp/")))))
  (goto-char (point-min))
  (kill-line 4))

(defun cats/ox-clip-formatted-copy()
  "wrapper for ox-clip-formatted-copy to disable org-export-use-babel."
  (interactive)
  (let ((org-export-use-babel nil))
    (ox-clip-formatted-copy (mark) (point))))

(defun cats/create-ticket-tmp-dir-open-dir-screen()
  "Create directory for the current ticket in tmp if not exist.
Open the directory of the current ticket in iterm screen via keyboard maestro."
  (interactive)
  (org-narrow-to-subtree)
  (let ((beg (point)))
    (goto-char (point-min))
    (if (re-search-forward "jira:" nil t 1)
        (if (org-in-regexp org-bracket-link-regexp 1)
            (let ((remove (list (match-beginning 0) (match-end 0)))
                  (description (last (split-string (if (match-end 3)
                                                       (match-string-no-properties 3)
                                                     (match-string-no-properties 1)) ":"))))
              (unless (file-exists-p (concat "~/tmp/" (downcase (car description))))
                (mkdir (concat "~/tmp/" (downcase (car description))))
                )
              (kill-new (concat "~/tmp/" (downcase (car description))))
              )))
    (goto-char beg)
    )
  (widen)
  (shell-command "osascript -e \'tell app \"Keyboard Maestro Engine\" to do script \"screen-start-cd-to-clipboard\"'"))


;; clock stuff
(defun cats//is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun cats//clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
    Skips capture tasks, projects, and subprojects.
    Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (cats/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (cats//is-project-p))
      "TODO"))))

(defun cats/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun cats/punch-in (arg)
  "Start continuous clocking and set the default task to the
    selected task.  If no task is selected set the Organization task
    as the default task."
  (interactive "p")
  (setq cats//keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (cats/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (cats/clock-in-organization-task-as-default)))))

(defun cats/punch-out ()
  (interactive)
  (setq cats//keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun cats/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
                       (org-clock-in))))

(defun cats/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
                               (org-clock-in))
          (when cats//keep-clock-running
            (cats/clock-in-default-task)))))))

(defun cats/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find cats/organization-task-id 'marker)
                     (org-clock-in '(16))))

(defun cats/clock-out-maybe ()
  (when (and cats//keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (cats/clock-in-parent-task)))


(defun cats/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq cats/insert-inactive-timestamp (not cats/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if cats/insert-inactive-timestamp "ON" "OFF")))

(defun cats/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun cats/insert-heading-inactive-timestamp ()
  (save-excursion
    (when cats/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (cats/insert-inactive-timestamp))))


;; refile
(defun cats//verify-refile-target () ;; Exclude DONE state tasks from refile targets
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))


;; ivan malison

(defun org-archive-if (condition-function)
  (if (funcall condition-function)
    (let ((next-point-marker
            (save-excursion (org-forward-heading-same-level 1) (point-marker))))
      (org-archive-subtree)
      (setq org-map-continue-from (marker-position next-point-marker)))))

(defun org-archive-if-completed ()
  (interactive)
  (org-archive-if 'org-entry-is-done-p))

(defun org-archive-completed-in-buffer ()
  (interactive)
  (org-map-entries 'org-archive-if-completed))

(defun org-todo-force-notes ()
  (interactive)
  (let ((org-todo-log-states
          (mapcar (lambda (state)
                    (list state 'note 'time))
            (apply 'append org-todo-sets))))
    (cond ((eq major-mode 'org-mode)  (org-todo))
      ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

(defun org-make-habit ()
  (interactive)
  (org-set-property "STYLE" "habit"))

(defun org-insert-habit ()
  (interactive)
  (org-insert-todo-heading nil)
  (org-make-habit))

(defun org-todo-at-date (date)
  (interactive (list (org-time-string-to-time (org-read-date))))
  (cl-flet ((org-current-effective-time (&rest r) date)
             (org-today (&rest r) (time-to-days date)))
    (cond ((eq major-mode 'org-mode) (org-todo))
      ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

(defun cats/compare-int-list (a b)
  (when (and a b)
    (cond ((> (car a) (car b)) 1)
      ((< (car a) (car b)) -1)
      (t (cats/compare-int-list (cdr a) (cdr b))))))

(defun org-cmp-creation-times (a b)
  (let ((a-created (get-date-created-from-agenda-entry a))
         (b-created (get-date-created-from-agenda-entry b)))
    (cats/compare-int-list a-created b-created)))

(defun org-agenda-done (&optional arg)
  "Mark current TODO as done.
  This changes the line at point, all other lines in the agenda referring to
  the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))


;; babel


;; ox-html

;; Use my own default naming scheme for org-headings. First we define a function
;; that will generate a sanitized version of the heading as its link target.
(defun cats//org-get-raw-value (item)
  (when (listp item)
    (let* ((property-list (cadr item)))
      (when property-list (plist-get property-list :raw-value)))))

(defun cats//sanitize-name (name)
  (replace-regexp-in-string "[^[:alpha:]]" "" (s-downcase name)))

(defun cats//generate-name (datum cache)
  (let ((raw-value (cats//org-get-raw-value datum)))
    (if raw-value
      (cats//sanitize-name raw-value)
      ;; This is the default implementation from org
      (let ((type (org-element-type datum)))
        (format "org%s%d"
          (if type
            (replace-regexp-in-string "-" "" (symbol-name type))
            "secondarystring")
          (incf (gethash type cache 0)))))))

;; This function replaces the default naming scheme with a call to
;; ~cats//generate-name~, and uses a slightly different uniquify approach.
(defun org-export-get-reference (datum info)
  "Return a unique reference for DATUM, as a string.
DATUM is either an element or an object.  INFO is the current
export state, as a plist.  Returned reference consists of
alphanumeric characters only."
  (let ((type (org-element-type datum))
         (cache (or (plist-get info :internal-references)
                  (let ((h (make-hash-table :test #'eq)))
                    (plist-put info :internal-references h)
                    h)))
         (reverse-cache (or (plist-get info :taken-internal-references)
                          (let ((h (make-hash-table :test 'equal)))
                            (plist-put info :taken-internal-references h)
                            h))))
    (or (gethash datum cache)
      (let* ((name (cats//generate-name datum cache))
              (number (+ 1 (gethash name reverse-cache -1)))
              (new-name (format "%s%s" name (if (< 0 number) number ""))))
        (puthash name number reverse-cache)
        (puthash datum new-name cache)
        new-name))))


;; org-agenda
(defun cats//opened-org-agenda-files ()
  (let ((files (org-agenda-files)))
    (setq cats-opened-org-agenda-files nil)
    (mapcar
      (lambda (x)
        (when (get-file-buffer x)
          (push x cats-opened-org-agenda-files)))
      files)))

(defun cats//kill-org-agenda-files ()
  (let ((files (org-agenda-files)))
    (mapcar
      (lambda (x)
        (when
          (and
            (get-file-buffer x)
            (not (member x cats-opened-org-agenda-files)))
          (kill-buffer (get-file-buffer x))))
      files)))

(defun cats//get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun cats//register-org-agenda-file-regexp (regexp)
  "Register REGEXP to the global list."
  (unless (member regexp cats//org-agenda-file-regexp-list)
    (push regexp cats//org-agenda-file-regexp-list)))

(defun cats//register-org-agenda-file (file)
  "Register FILE to the global list of FILEs CATS//ORG-AGENDA-LIST."
  (unless (member file cats//org-agenda-list)
    (push file cats//org-agenda-list)))

(defun cats//set-org-agenda-file-regexps (incoming-regexps &optional reset)
  (if reset
    (setq org-agenda-file-regexp
      (reduce (lambda (a b)
                (concatenate 'string a "\\|" b))
        (if reset
          incoming-regexps
          (append `(,org-agenda-file-regexp) incoming-regexps))))))

(defun cats//set-org-agenda-files (incoming-files)
  (setq org-agenda-files
    (delete-dups
      (cl-loop for filepath in (append org-agenda-files incoming-files)
        when (and filepath (file-exists-p (file-truename filepath)))
        collect (file-truename filepath)))))

(defun cats//make-org-linked-todo-template ()
  (cats//make-org-todo-template "[#C] %? %A"))

(cl-defun cats//make-org-template (&key (content "%?"))
  (with-temp-buffer
    (org-mode)
    (insert content)
    (org-set-property "CREATED"
      (with-temp-buffer
        (org-insert-time-stamp
          (org-current-effective-time) t t)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cats//make-org-template-from-file (filename)
  (cats//make-org-template (cats//get-string-from-file filename)))

(cl-defun cats//make-org-todo-template
  (&key (content "%?") (creation-state "TODO"))
  (with-temp-buffer
    (org-mode)
    (org-insert-heading)
    (insert content)
    (org-todo creation-state)
    (org-set-property "CREATED"
      (with-temp-buffer
        (org-insert-time-stamp
          (org-current-effective-time) t t)))
    (remove-hook 'post-command-hook 'org-add-log-note)
    (let ((org-log-note-purpose 'state)
           (org-log-note-return-to (point-marker))
           (org-log-note-marker (progn (goto-char (org-log-beginning t))
                                  (point-marker)))
           (org-log-note-state creation-state))
      (org-add-log-note))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cats//org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
            (my-pre-fg (face-foreground 'default)))
      (setq
        org-html-head-extra
        (concat
          org-html-head-extra
          (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
            my-pre-bg my-pre-fg))))))


;; ox-html
;; Add link icons in headings that lead to themselves
(defun cats//set-current-html-headline (headline &rest args)
  (setq cats//current-html-headline headline))

(defun cats//clear-current-html-headline (&rest args)
  (setq cats//current-html-headline nil))

(defun cats//org-html-format-heading-function (todo todo-type priority text tags info)
  (let* ((reference (when cats//current-html-headline
                      (org-export-get-reference cats//current-html-headline info)))
          ;; Don't do anything special if the current headline is not set
          (new-text (if reference
                      (format "%s <a href=\"#%s\">%s</a>" text reference cats//link-svg-html)
                      text)))
    (org-html-format-headline-default-function
      todo todo-type priority new-text tags info)))


;; org-journal
(defun cats/journal-file-insert ()
  "Insert's the journal heading based on the file's name.

In case I decide to export my journal, I wanted each file to have
a title with the date. Also, I really wanted to have this
information inserted automatically without having to trigger the
snippet.

This is similar to `org-journal-date-prefix' but offers more flexibility."
  (interactive)
  (when (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
          (buffer-name))
    (let ((year  (string-to-number (match-string 1 (buffer-name))))
           (month (string-to-number (match-string 2 (buffer-name))))
           (day   (string-to-number (match-string 3 (buffer-name))))
           (datim nil))
      (setq datim (encode-time 0 0 0 day month year))
      (insert ("%s %s\n\n" cats-org-journal-date-prefix
                (format-time-string "%s %Y-%b-%d (%A)" datim))))))


;; org-capture
(defun cats//org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order
  ;; to inhibit inserting the heading; org-capture will insert the
  ;; heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))



;; misc
(defun spacemacs//add-org-keybindings (mode)
  "Add shortcuts for org to `MODE' from spacemacs."
  (dolist (prefix '(
                     ("mb" . "babel")
                     ("mC" . "clocks")
                     ("md" . "dates")
                     ("me" . "export")
                     ("mi" . "insert")
                     ("miD" . "download")
                     ("ms" . "trees/subtrees")
                     ("mT" . "toggles")
                     ("mt" . "tables")
                     ("mtd" . "delete")
                     ("mti" . "insert")
                     ("mtt" . "toggle")
                     ("mx" . "text")))
    (spacemacs/declare-prefix-for-mode mode (car prefix) (cdr prefix)))

  (spacemacs/set-leader-keys-for-major-mode mode
    "'" 'org-edit-special
    "c" 'org-capture
    "Cc" 'org-clock-cancel
    "Ci" 'org-clock-in
    "Co" 'org-clock-out
    "Cr" 'org-resolve-clocks
    "dd" 'org-deadline
    "ds" 'org-schedule
    "dt" 'org-time-stamp
    "dT" 'org-time-stamp-inactive
    "ee" 'org-export-dispatch

    "a" 'org-agenda

    "Tc" 'org-toggle-checkbox
    "Te" 'org-toggle-pretty-entities
    "Ti" 'org-toggle-inline-images
    "Tl" 'org-toggle-link-display
    "Tt" 'org-show-todo-tree
    "TT" 'org-todo
    "TV" 'space-doc-mode
    "Tx" 'org-toggle-latex-fragment

    ;; More cycling options (timestamps, headlines, items, properties)
    "L" 'org-shiftright
    "H" 'org-shiftleft
    "J" 'org-shiftdown
    "K" 'org-shiftup

    ;; Change between TODO sets
    "C-S-l" 'org-shiftcontrolright
    "C-S-h" 'org-shiftcontrolleft
    "C-S-j" 'org-shiftcontroldown
    "C-S-k" 'org-shiftcontrolup

    ;; Subtree editing
    "sa" 'org-toggle-archive-tag
    "sA" 'org-archive-subtree
    "sb" 'org-tree-to-indirect-buffer
    "sh" 'org-promote-subtree
    "sj" 'org-move-subtree-down
    "sk" 'org-move-subtree-up
    "sl" 'org-demote-subtree
    "sn" 'org-narrow-to-subtree
    "sN" 'widen
    "sr" 'org-refile
    "ss" 'org-sparse-tree
    "sS" 'org-sort

    ;; tables
    "ta" 'org-table-align
    "tb" 'org-table-blank-field
    "tc" 'org-table-convert
    "tdc" 'org-table-delete-column
    "tdr" 'org-table-kill-row
    "te" 'org-table-eval-formula
    "tE" 'org-table-export
    "th" 'org-table-previous-field
    "tH" 'org-table-move-column-left
    "tic" 'org-table-insert-column
    "tih" 'org-table-insert-hline
    "tiH" 'org-table-hline-and-move
    "tir" 'org-table-insert-row
    "tI" 'org-table-import
    "tj" 'org-table-next-row
    "tJ" 'org-table-move-row-down
    "tK" 'org-table-move-row-up
    "tl" 'org-table-next-field
    "tL" 'org-table-move-column-right
    "tn" 'org-table-create
    "tN" 'org-table-create-with-table.el
    "tr" 'org-table-recalculate
    "ts" 'org-table-sort-lines
    "ttf" 'org-table-toggle-formula-debugger
    "tto" 'org-table-toggle-coordinate-overlays
    "tw" 'org-table-wrap-region

    ;; Source blocks / org-babel
    "bp" 'org-babel-previous-src-block
    "bn" 'org-babel-next-src-block
    "be" 'org-babel-execute-maybe
    "bo" 'org-babel-open-src-block-result
    "bv" 'org-babel-expand-src-block
    "bu" 'org-babel-goto-src-block-head
    "bg" 'org-babel-goto-named-src-block
    "br" 'org-babel-goto-named-result
    "bb" 'org-babel-execute-buffer
    "bs" 'org-babel-execute-subtree
    "bd" 'org-babel-demarcate-block
    "bt" 'org-babel-tangle
    "bf" 'org-babel-tangle-file
    "bc" 'org-babel-check-src-block
    "bj" 'org-babel-insert-header-arg
    "bl" 'org-babel-load-in-session
    "bi" 'org-babel-lob-ingest
    "bI" 'org-babel-view-src-block-info
    "bz" 'org-babel-switch-to-session
    "bZ" 'org-babel-switch-to-session-with-code
    "ba" 'org-babel-sha1-hash
    "bx" 'org-babel-do-key-sequence-in-edit-buffer
    "b." 'spacemacs/org-babel-transient-state/body

    ;; Multi-purpose keys
    (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
    "*" 'org-ctrl-c-star
    "-" 'org-ctrl-c-minus
    "#" 'org-update-statistics-cookies
    "RET"   'org-ctrl-c-ret
    "M-RET" 'org-meta-return
    ;; attachments
    "A" 'org-attach
    ;; insertion
    "id" 'org-insert-drawer
    "ie" 'org-set-effort
    "if" 'org-footnote-new
    "ih" 'org-insert-heading
    "iH" 'org-insert-heading-after-current

    "il" 'org-insert-link
    "ip" 'org-set-property
    "is" 'org-insert-subheading
    "it" 'org-set-tags
    ;; region manipulation
    "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
    "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
    "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
    "xo" 'org-open-at-point
    "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
    "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
    "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
    "xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=)))

(defun cats//add-org-keybindings (mode)
  "Add shortcuts for org to `MODE'."
  (dolist (prefix '(
                     ("mj" . "journal")
                     ))
    (spacemacs/declare-prefix-for-mode mode (car prefix) (cdr prefix)))

  (spacemacs/set-leader-keys-for-major-mode mode
    ;; journal
    "jh" 'cats/journal-file-insert
    "js" 'org-journal-search-forever
    "jj" 'org-journal-new-entry
    "jn" 'org-journal-open-next-entry
    "jp" 'org-journal-open-previous-entry
    "j." 'spacemacs/org-journal-transient-state/body

    ;; insertion
    "iO" 'org-insert-todo-subheading
    "io" 'org-insert-todo-heading
    "ia" 'org-insert-habit
    "iA" 'org-make-habit
    "iS" 'cats/insert-inactive-timestamp
    "TS" 'cats/toggle-insert-inactive-timestamp
    (kbd "T C-t") 'org-todo-force-notes))

;;; funcs.el ends here
