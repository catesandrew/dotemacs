;;; module-org.el --- ORG Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-transient-state)
(require 'core-auto-completion)
(require 'core-keybindings)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

(defvar org-enable-github-support t
  "If non-nil Github related packages are configured.")

(dotemacs-defvar-company-backends org-mode)

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook org-mode)
      (push 'company-capf company-backends-org-mode)))
  (dotemacs-use-package-add-hook company-emoji
    :post-init
    (progn
      (push 'company-emoji company-backends-org-mode))))

(dotemacs-use-package-add-hook emoji-cheat-sheet-plus
  :post-init
  (add-hook 'org-mode-hook 'dotemacs/delay-emoji-cheat-sheet-hook))

(use-package evil-org
  :load-path "evil/"
  :commands evil-org-mode
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  :config
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "C" 'evil-org-recompute-clocks)
    (evil-define-key 'normal evil-org-mode-map
      "O" 'evil-open-above)
    (dotemacs-diminish evil-org-mode " ⓔ" " e")))

(dotemacs-use-package-add-hook evil-surround
  :post-init
  (defun dotemacs/add-org-surrounds ()
    (push '(?: . dotemacs//surround-drawer) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'dotemacs/add-org-surrounds)
  (defun dotemacs/add-org-src-surrounds ()
    (push '(?! . ( "#+begin_src" . "#+end_src")) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'dotemacs/add-org-src-surrounds)
  (defun dotemacs//surround-drawer ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format ":%s:" (or dname "")) ":END:"))))

(dotemacs-use-package-add-hook flyspell
  :post-init
  (spell-checking/add-flyspell-hook 'org-mode-hook))

(use-package gnuplot
  :defer t
  :ensure t
  :init (dotemacs-set-leader-keys-for-major-mode 'org-mode
          "tp" 'org-plot/gnuplot))

(dotemacs-use-package-add-hook org-plus-contrib
  :post-init
  (progn
    (add-hook 'org-mode-hook 'dotemacs/org-setup-pretty-symbols)
    (defun dotemacs/org-setup-pretty-symbols ()
      (dotemacs/pretty-symbols
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
         ("^_^" . ?☻))))))

(use-package org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
  :ensure t
  :defer t
  :init
  (progn

    (setq org-startup-indented t
          org-clock-persist-file
          (concat dotemacs-cache-directory "org-clock-save.el")
          org-id-locations-file
          (concat dotemacs-cache-directory ".org-id-locations")
          org-log-done t
          org-startup-with-inline-images t
          org-src-fontify-natively t ;; Fontify org-mode code blocks
          ;; this is consistent with the value of `helm-org-headings-max-depth'.
          org-imenu-depth 8)

    (with-eval-after-load 'org-indent
      (dotemacs-hide-lighter org-indent-mode))
    (let ((dir (concat user-emacs-directory "etc/")))
      (setq org-export-async-init-file (concat dir "org-async-init.el")))
    (defmacro dotemacs-org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))

    ;; Follow the confirm and abort conventions
    (with-eval-after-load 'org-capture
      (dotemacs-set-leader-keys-for-minor-mode 'org-capture-mode
        dotemacs-major-mode-leader-key 'org-capture-finalize
        "c" 'org-capture-finalize
        "k" 'org-capture-kill
        "a" 'org-capture-kill
        "r" 'org-capture-refile))

    (with-eval-after-load 'org-src
      (dotemacs-set-leader-keys-for-minor-mode 'org-src-mode
        "'" 'org-edit-src-exit
        "c" 'org-edit-src-exit
        "a" 'org-edit-src-abort
        "k" 'org-edit-src-abort))

    ;; Insert key for org-mode and markdown a la C-h k
    ;; from SE endless http://emacs.stackexchange.com/questions/2206/
    (defun dotemacs/insert-keybinding-org (key)
      "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
      (interactive "kType key sequence: ")
      (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
        (if (null (equal key "\r"))
            (insert
             (format tag (help-key-description key nil)))
          (insert (format tag ""))
          (forward-char -8))))

    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "'" 'org-edit-special
      "c" 'org-capture
      "d" 'org-deadline
      "e" 'org-export-dispatch
      "f" 'org-set-effort
      "P" 'org-set-property
      ":" 'org-set-tags

      "a" 'org-agenda
      "b" 'org-tree-to-indirect-buffer
      "A" 'org-archive-subtree
      "l" 'org-open-at-point
      "T" 'org-show-todo-tree

      "." 'org-time-stamp
      "!" 'org-time-stamp-inactive

      ;; headings
      "hi" 'org-insert-heading-after-current
      "hI" 'org-insert-heading

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
      "Sl" 'org-demote-subtree
      "Sh" 'org-promote-subtree
      "Sj" 'org-move-subtree-down
      "Sk" 'org-move-subtree-up

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

      ;; Multi-purpose keys
      (or dotemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
      "*" 'org-ctrl-c-star
      " RET" 'org-ctrl-c-ret
      "-" 'org-ctrl-c-minus
      "^" 'org-sort
      "/" 'org-sparse-tree

      "I" 'org-clock-in
      "n" 'org-narrow-to-subtree
      "N" 'widen
      "O" 'org-clock-out
      "q" 'org-clock-cancel
      "R" 'org-refile
      "s" 'org-schedule

      ;; insertion of common elements
      "il" 'org-insert-link
      "if" 'org-footnote-new
      "ik" 'dotemacs/insert-keybinding-org

      ;; images and other link types have no commands in org mode-line
      ;; could be inserted using yasnippet?
      ;; region manipulation
      "xb" (dotemacs-org-emphasize dotemacs-org-bold ?*)
      "xc" (dotemacs-org-emphasize dotemacs-org-code ?~)
      "xi" (dotemacs-org-emphasize dotemacs-org-italic ?/)
      "xr" (dotemacs-org-emphasize dotemacs-org-clear ? )
      "xs" (dotemacs-org-emphasize dotemacs-org-strike-through ?+)
      "xu" (dotemacs-org-emphasize dotemacs-org-underline ?_)
      "xv" (dotemacs-org-emphasize dotemacs-org-verbose ?=))

    ;; (require 'ox-md)
    ;; (require 'ox-ascii)
    ;; (require 'ox-confluence)
    ;; (require 'ox-html)

    ;; Add global evil-leader mappings. Used to access org-agenda
    ;; functionalities – and a few others commands – from any other mode.
    (dotemacs-declare-prefix "ao" "org")
    (dotemacs-set-leader-keys
      ;; org-agenda
      "ao#" 'org-agenda-list-stuck-projects
      "ao/" 'org-occur-in-agenda-files
      "aoa" 'org-agenda-list
      "aoe" 'org-store-agenda-views
      "aom" 'org-tags-view
      "aoo" 'org-agenda
      "aos" 'org-search-view
      "aot" 'org-todo-list
      ;; other
      "aoO" 'org-clock-out
      "aoc" 'org-capture
      "aol" 'org-store-link)

    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key global-map "\C-cc" 'org-capture))
  :config
  (progn
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

    (setq org-default-notes-file "notes.org")
    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                  (1 font-lock-comment-face prepend)
                  (2 font-lock-function-name-face)
                  (3 font-lock-comment-face prepend))))

    ;; Open links and files with RET in normal state
    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

    ;; We add this key mapping because an Emacs user can change
    ;; `dotemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
    ;; C-c ' is shadowed by `dotemacs/default-pop-shell', effectively making
    ;; the Emacs user unable to exit src block editing.
    (define-key org-src-mode-map (kbd (concat dotemacs-major-mode-emacs-leader-key " '")) 'org-edit-src-exit)

    (dotemacs-set-leader-keys
      "Cc" 'org-capture)

    ;; Evilify the calendar tool on C-c .
    (unless (eq 'emacs dotemacs-editing-style)
      (define-key org-read-date-minibuffer-local-map (kbd "M-h")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-l")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-k")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-j")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-H")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-L")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-K")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
      (define-key org-read-date-minibuffer-local-map (kbd "M-J")
        (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1)))))))

(use-package org-agenda
  :defer t
  :init
  (progn
    (setq org-agenda-restore-windows-after-quit t)
    (dotemacs-set-leader-keys-for-major-mode 'org-agenda-mode
      ":" 'org-agenda-set-tags
      "a" 'org-agenda
      "d" 'org-agenda-deadline
      "f" 'org-agenda-set-effort
      "I" 'org-agenda-clock-in
      "O" 'org-agenda-clock-out
      "P" 'org-agenda-set-property
      "q" 'org-agenda-refile
      "Q" 'org-agenda-clock-cancel
      "s" 'org-agenda-schedule)
    (dotemacs-define-transient-state org-agenda
      :title "Org-agenda transient state"
      :on-enter (setq which-key-inhibit t)
      :on-exit (setq which-key-inhibit nil)
      :foreign-keys run
      :doc
      "
Headline^^            Visit entry^^               Filter^^                    Date^^               Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------  -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule      [_tf_] follow        [_vd_] day         [_ci_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dd_] set deadline  [_tl_] log           [_vw_] week        [_co_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dt_] timestamp     [_ta_] archive       [_vt_] fortnight   [_ck_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_+_]  do later      [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_hT_] set tags       ^^                          [_fx_] by regexp            [_-_]  do earlier    [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   ^^                   ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
      :bindings
      ;; Entry
      ("ht" org-agenda-todo)
      ("hk" org-agenda-kill)
      ("hr" org-agenda-refile)
      ("hA" org-agenda-archive-default)
      ("hT" org-agenda-set-tags)
      ("hp" org-agenda-priority)

      ;; Visit entry
      ("SPC" org-agenda-show-and-scroll-up)
      ("<tab>" org-agenda-goto :exit t)
      ("TAB" org-agenda-goto :exit t)
      ("RET" org-agenda-switch-to :exit t)
      ("o"   link-hint-open-link :exit t)

      ;; Date
      ("ds" org-agenda-schedule)
      ("dd" org-agenda-deadline)
      ("dt" org-agenda-date-prompt)
      ("+" org-agenda-do-date-later)
      ("-" org-agenda-do-date-earlier)

      ;; View
      ("vd" org-agenda-day-view)
      ("vw" org-agenda-week-view)
      ("vt" org-agenda-fortnight-view)
      ("vm" org-agenda-month-view)
      ("vy" org-agenda-year-view)
      ("vn" org-agenda-later)
      ("vp" org-agenda-earlier)
      ("vr" org-agenda-reset-view)

      ;; Toggle mode
      ("tf" org-agenda-follow-mode)
      ("tl" org-agenda-log-mode)
      ("ta" org-agenda-archives-mode)
      ("tr" org-agenda-clockreport-mode)
      ("td" org-agenda-toggle-diary)

      ;; Filter
      ("ft" org-agenda-filter-by-tag)
      ("fr" org-agenda-filter-by-tag-refine)
      ("fc" org-agenda-filter-by-category)
      ("fh" org-agenda-filter-by-top-headline)
      ("fx" org-agenda-filter-by-regexp)
      ("fd" org-agenda-filter-remove-all)

      ;; Clock
      ("ci" org-agenda-clock-in :exit t)
      ("co" org-agenda-clock-out)
      ("ck" org-agenda-clock-cancel)
      ("cj" org-agenda-clock-goto :exit t)

      ;; Other
      ("q" nil :exit t)
      ("gr" org-agenda-redo)
      ("." org-agenda-goto-today)
      ("gd" org-agenda-goto-date)))
  :config
  (evilified-state-evilify-map org-agenda-mode-map
    :mode org-agenda-mode
    :bindings
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "M-j") 'org-agenda-next-item
    (kbd "M-k") 'org-agenda-previous-item
    (kbd "M-h") 'org-agenda-earlier
    (kbd "M-l") 'org-agenda-later
    (kbd "gd") 'org-agenda-toggle-time-grid
    (kbd "gr") 'org-agenda-redo
    (kbd "M-RET") 'org-agenda-show-and-scroll-up
    (kbd "RET") 'org-agenda-goto
    (kbd "M-SPC") 'dotemacs/org-agenda-transient-state/body
    (kbd "s-M-SPC") 'dotemacs/org-agenda-transient-state/body))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (progn
    (setq org-bullets-bullet-list '("✿" "❀" "☢" "☯" "✸" ))
    (add-hook 'org-mode-hook 'org-bullets-mode)))

(use-package org-mime
  :defer t
  :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
  :init
  (progn
    (dotemacs-set-leader-keys-for-major-mode 'message-mode
      "M" 'org-mime-htmlize)
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "m" 'org-mime-org-buffer-htmlize)))

(use-package org-pomodoro
  :defer t
  :ensure t
  :init
  (progn
    (when (dotemacs/system-is-mac)
      (setq org-pomodoro-audio-player "/usr/bin/afplay"))
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "p" 'org-pomodoro)))

(use-package org-present
  :defer t
  :ensure t
  :init
  (progn
    (evilified-state-evilify nil org-present-mode-keymap
      "h" 'org-present-prev
      "l" 'org-present-next
      "q" 'org-present-quit)
    (defun dotemacs//org-present-start ()
      "Initiate `org-present' mode."
      (org-present-big)
      (org-display-inline-images)
      (org-present-hide-cursor)
      (org-present-read-only)
      (evil-evilified-state))
    (defun dotemacs//org-present-end ()
      "Terminate `org-present' mode."
      (org-present-small)
      (org-remove-inline-images)
      (org-present-show-cursor)
      (org-present-read-write)
      (evil-normal-state))
    (add-hook 'org-present-mode-hook 'dotemacs//org-present-start)
    (add-hook 'org-present-mode-quit-hook 'dotemacs//org-present-end)))

(use-package org-repo-todo
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs-set-leader-keys
      "Ct"  'ort/capture-todo
      "CT"  'ort/capture-checkitem)
    (dotemacs-set-leader-keys-for-major-mode 'org-mode
      "gt" 'ort/goto-todos)))

(use-package ox-gfm
  :if org-enable-github-support
  :defer t
  :ensure t
  :init
  (progn
    ;; seems to be required otherwise the extension is not
    ;; loaded properly by org
    (with-eval-after-load 'org (require 'ox-gfm))
    (autoload 'org-gfm-export-as-markdown "ox-gfm" "\
 Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org GFM Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

      (autoload 'org-gfm-convert-region-to-md "ox-gfm" "\
Assume the current region has org-mode syntax, and convert it
to Github Flavored Markdown.  This can be used in any buffer.
For example, you can write an itemized list in org-mode syntax in
a Markdown buffer and use this command to convert it.

\(fn)" t nil)))

;; (dotemacs-use-package-add-hook persp-mode
;;   :post-init
  (dotemacs-define-custom-layout "@Org"
    :binding "o"
    :body
    (find-file (first (org-agenda-files))))
  ;; )

(use-package toc-org
  :ensure t
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package htmlize
  :ensure t
  :defer t)

(provide 'module-org)
;;; module-org.el ends here
