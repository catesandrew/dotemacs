;;; packages.el --- cats: org

;;; Commentary:

;;; Code:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-org-packages
  '(
     autoinsert
     helm-bibtex
     ;; company                            ;; defined in spacemacs org
     ;; company-emoji                      ;; defined in spacemacs org
     ;; emoji-cheat-sheet-plus             ;; defined in spacemacs org
     ;; evil-surround                      ;; defined in spacemacs org
     ;; gnuplot                            ;; defined in spacemacs org
     ;; htmlize                            ;; defined in spacemacs org
     ;; ob, org and org-agenda are installed by `org-plus-contrib' from spacemacs-org
     (ob :location built-in)            ;; defined in spacemacs org
     (ob-ditta :location built-in)
     (org :location built-in)           ;; defined in spacemacs org
     (org-agenda :location built-in)    ;; defined in spacemacs org
     org-bullets
     ;; (org-brain :location built-in)     ;; defined in spacemacs org
     ;; (org-expiry :location built-in)    ;; defined in spacemacs org
     (org-faces :location built-in)
     (org-clock :location built-in)
     (org-id :location built-in)
     (org-indent :location built-in)
     (org-list :location built-in)
     (org-src :location built-in)
     (org-capture :location built-in)
     (org-habit :location built-in)
     (org-mobile :location built-in)
     ;; (helm-org :location built-in)
     org-super-agenda
     org-jira
     ;; org-caldav
     ;; org-notify
     ;; org-ehtml
     org-journal                        ;; defined in spacemacs org
     ;; org-download                       ;; defined in spacemacs org
     ;; org-mime                           ;; defined in spacemacs org
     ;; org-pomodoro                       ;; defined in spacemacs org
     ;; org-present                        ;; defined in spacemacs org
     org-projectile                     ;; defined in spacemacs org
     org-roam                           ;; defined in spacemacs
     (org-projectile-helm :requires org-projectile)
     (ox :toggle org-enable-ox-support :location built-in)
     (ox-ascii :toggle org-enable-ox-ascii-support :location built-in)
     (ox-confluence :location built-in)

     (ox-beamer :toggle org-enable-ox-beamer-support :location built-in)
     (ox-bibtex :toggle org-enable-ox-bibtex-support :location built-in)
     ;; ox-gfm                             ;; defined in spacemacs org
     (ox-html :toggle org-enable-ox-html-support :location built-in)
     ;; ox-hugo                            ;; defined in spacemacs org
     ox-jira
     (ox-latex :toggle org-enable-ox-latex-support :location built-in)
     (ox-md :toggle org-enable-ox-md-support :location built-in)
     (ox-publish :toggle org-enable-ox-publish-support :location built-in)
     ox-reveal                          ;; defined in spacemacs org
     ;; ox-twbs                            ;; defined in spacemacs org
     which-key
     deft
     zetteldeft
     org-roam-server
     ;; org-roam-protocol
     org-roam-bibtex
     (org-pandoc-import
       :location (recipe
                   :fetcher github
                   :repo "tecosaur/org-pandoc-import"
                   :files ("*.el" "filters" "confident")))
     ))


;; NOTE: org-capture throws json-readtable-error
;; sudo apt-get -y install ipython ipython-notebook
;; sudo -H pip install jupyter
;; or, brew install jupyter


;; org-pandoc-import
(defun cats-org/init-org-pandoc-import ()
  (use-package org-pandoc-import
    :after org
    :defer t
    :init
    (progn)))


;; ox-roam
(defun cats-org/pre-init-org-roam ()
  (spacemacs|use-package-add-hook org-roam
    :post-init
    (progn
      (setq
        org-roam-directory cats//org-roam-dir
        org-roam-dailies-directory cats//org-roam-dailies-dir
        org-roam-db-location (concat cats//org-dir "refs/org-roam.db")
        org-roam-index-file (concat cats//org-roam-dir "index.org")
        org-roam-verbose nil
        ;; appends a  `º` to each Roam link.
        org-roam-link-title-format "%sº"
        ;; make org-roam buffer sticky
        org-roam-buffer-no-delete-other-windows t)

      ;; Create org roam directory
      (unless (file-exists-p org-roam-directory)
        (make-directory org-roam-directory t))

      ;; add roam to agenda
      (cats//register-org-agenda-file org-roam-directory)

      ;; Create org roam dailies directory
      (unless (file-exists-p org-roam-dailies-directory)
        (make-directory org-roam-dailies-directory t))

      ;; add dailies to agenda
      (cats//register-org-agenda-file org-roam-dailies-directory)

      (with-eval-after-load 'org-roam-server
        (org-roam-server-mode))

      (spacemacs/set-leader-keys
        "aorm" 'org-roam-jump-to-index
        "aor." 'spacemacs/org-roam-transient-state/body)

      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rm" 'org-roam-jump-to-index
        "rd." 'spacemacs/org-roam-transient-state/body)

      (spacemacs|define-transient-state org-roam
        :title "Org Roam Dailies Transient State"
        :doc "
[_n_/_p_] navigate notes   [_h_/_j_/_k_] navigate dailies
[_i_] new entry            [_/_] search forever
[_q_] quit"
        :bindings
        ("q" nil :exit t)
        ("h" org-roam-dailies-find-today)
        ("j" org-roam-dailies-find-tomorrow)
        ("k" org-roam-dailies-find-yesterday)
        ("n" org-roam-dailies-find-next-note)
        ("p" org-roam-dailies-find-previous-note)
        ("i" org-roam-dailies-capture-today)
        ("/" org-roam-dailies-find-date)))
    :post-config
    (progn
      (setq org-roam-dailies-capture-templates
        '(
           ;; default
           ("d" "default" entry #'org-roam-capture--get-point
             "* %?"
             :file-name "daily/%<%Y-%m-%d>"
             :head "#+title: %<%Y-%m-%d>")

           ("j" "journal entry" plain (function cats//org-journal-find-location)
             "* %(format-time-string org-journal-time-format)%\n^{Title}\n%i%?"
             :immediate-finish t
             :file-name "daily/%<%Y-%m-%d>"
             :head "#+TITLE: %<%Y-%m-%d>")
           ))

      (setq org-roam-capture-templates
        `(
           ;; bug fixes
           ("b" "bug fixes" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:lead:
:jira:
:jira+:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "\"Bug Fixes\"")
             :unnarrowed t)

           ;; business
           ("B" "business" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "Business")
             :unnarrowed t)

           ;; default
           ("d" "default" plain (function org-roam--capture-get-point)
             "* Notes

%?"
             :file-name "${slug}" ;; "%<%Y%m%d%H%M%S>-${slug}"
             :head ,(cats//org-roam-template-head "Inbox")
             :unnarrowed t)

           ;; done
           ("D" "done" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "Done")
             :unnarrowed t)

           ;; feature
           ("f" "feature" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:lead:
:jira:
:jira+:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "Features")
             :unnarrowed t)

           ;; general
           ("g" "general" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "General")
             :unnarrowed t)

           ;; issue
           ("i" "issue" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:lead:
:jira:
:jira+:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "Issues")
             :unnarrowed t)

           ;; interviews
           ("I" "interviews" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "Interviews")
             :unnarrowed t)

           ;; log
           ("L" "log" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "log/%<%Y-%m-%d-%H%M>-${slug}"
             :head ,(cats//org-roam-template-head "log")
             :unnarrowed t)

           ;; product develpment
           ("p" "project" entry (function org-roam--capture-get-point)
             "* ${title}%?
  :PROPERTIES:
  :FOR_EXPORT_COLUMNS:  %50ITEM %8Effort(Estimate){:}
  :COLUMNS:  %50ITEM %8Effort(Estimate){:} %8LEAD(Lead) %5CLOCKSUM(Clocked) %10TODO(State)
  :ID:       %(cats//org-roam-filename \"${title}\")
  :EXPORT_TITLE: ${title}
  :EXPORT_OPTIONS: toc:nil
  :LEAD:
  :LEAD_ALL: agrandle sbranch jweimer kwhite ajames gmorales acates
  :EPIC:
  :FEATURE-FLAG:
  :FEATURE-FLAG+:
  :COMPLETE-DATE:
  :END:
** Resources
** Tasks
*** First task
    :PROPERTIES:
    :Effort:   01:00
    :END:
    - Simple task
** Notes
** Estimates
#+BEGIN: columnview :hlines 2 :vlines t :id \"%(cats//org-roam-filename \"${title}\")\" :skip-empty-rows t

#+END:

"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "\"Product Development\"")
             :unnarrowed t)

           ;; backlop - ready
           ("r" "backlog - ready" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "\"Backlog - Ready\"")
             :unnarrowed t)

           ;; releases
           ("R" "releases" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "Releases")
             :unnarrowed t)

           ;; current sprint
           ("S" "sprint - current" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "\"Current Sprint\"")
             :unnarrowed t)

           ;; past sprints
           ("P" "sprints - past" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "\"Past Sprints\"")
             :unnarrowed t)

           ;; backlop - wip
           ("w" "backlog - wip" entry (function org-roam--capture-get-point)
             ":PROPERTIES:
:END:

* Resources
* Notes

%?"
             :file-name "${slug}"
             :head ,(cats//org-roam-template-head "\"Backlog - WIP\"")
             :unnarrowed t)

           ;; capture template to grab websites. Requires org-roam protocol.
           ("W" "website" entry (function org-roam--capture-get-point)
             "* Resources
* Notes

%?"
             :file-name "websites/${slug}"
             :head ,(cats//org-roam-template-ref-head "website")
             :unnarrowed t)


           ))
      )))


;; helm-bibtex

;; * The template includes the `orb-process-file-field` function to allow
;;   selecting the `pdf` to be used with `org-noter`
;; * The `file` field is specified to work with the `.bib` file generated by
;;   Zotero
;; * `helm-bibtex` allows for any of the keys in a `.bib` file to be used in a
;;   template, and an overly expressive one is more useful
;; * The `ROAM_KEY` is defined to ensure that cite backlinks work correctly
;;   with `org-roam`
;; * As I prefer to have one notes file per `pdf`, I have only configured
;;   the `bibtex-completion-notes-template-multiple-files` variable
(defun cats-org/pre-init-helm-bibtex ()
  (spacemacs|use-package-add-hook helm-bibtex
    :post-init
    (progn
      (unless (file-exists-p (concat cats//org-dir "refs/notes"))
        (make-directory (concat cats//org-dir "refs/notes") t))

      ;; Create org bibtex library directory
      (unless (file-exists-p cats//bibtex-library-dir)
        (make-directory cats//bibtex-library-dir t))

      (setq bibtex-format-citation-functions
        '((org-mode . (lambda (x) (insert (concat
                                       "\\cite{"
                                       (mapconcat 'identity x ",")
                                       "}")) ""))))

      (setq
        bibtex-completion-library-path cats//bibtex-library-dir
        bibtex-completion-bibliography (concat cats//org-dir "refs/library.bib")
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files (concat
          "#+TITLE: ${title}\n"
          "#+ROAM_KEY: cite:${=key=}\n"
          "* TODO Notes\n"
          ":PROPERTIES:\n"
          ":Custom_ID: ${=key=}\n"
          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
          ":AUTHOR: ${author-abbrev}\n"
          ":JOURNAL: ${journaltitle}\n"
          ":DATE: ${date}\n"
          ":YEAR: ${year}\n"
          ":DOI: ${doi}\n"
          ":URL: ${url}\n"
          ":END:\n\n"
          )
        )
      )
    :post-config
    (progn)
    )
  )


;; org-roam-bibtex

(defun cats-org/init-org-roam-bibtex ()
  "Use org-roam-bibtex."
  (use-package org-roam-bibtex
    :after org-roam
    :init
    (progn
      (setq org-roam-bibtex-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords")))
    :config
    (progn
      (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
            ""
            :file-name "${slug}"
            :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}
* ${title}
:PROPERTIES:
:Custom_ID: ${=key=}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
:NOTER_PAGE:
:END:"

            :unnarrowed t)))
      )
    ))



;; zetteldeft

(defun cats-org/pre-init-zetteldeft ()
  (spacemacs|use-package-add-hook zetteldeft
    :post-init
    (progn
      ;; In such Zettelkasten links are often wrapped in square brackets. This
      ;; can be easily achieved by setting the zetteldeft-link-indicator and
      ;; zetteldeft-link-suffix.
      (setq zetteldeft-link-indicator "[["
            zetteldeft-link-suffix "]]")

      ;; To make sure that your Markdown notes start with correct title syntax,
      ;; customize the zetteldeft-title-prefix.
      (setq zetteldeft-title-prefix "# ")

      ;; When using zetteldeft-insert-list-links, you might want to change a
      ;; list entry to correct Markdown syntax, like so:
      (setq zetteldeft-list-prefix "- ")

      (with-eval-after-load 'zetteldeft
        ;; To highlight links you need to set up font-lock keywords for
        ;; markdown-mode.
        (font-lock-add-keywords 'markdown-mode
          `((,zetteldeft-id-regex
              . font-lock-warning-face)))

        ;; Alternatively, if you want to highlight the brackets as well, you need
        ;; to escape them like so:
        (font-lock-add-keywords 'markdown-mode
          `((,(concat "\\[\\["
                zetteldeft-id-regex
                "\\]\\]")
              . font-lock-warning-face))))
      )))


;; org-roam-server

(defun cats-org/init-org-roam-server ()
  "Use org-roam-server."
  (use-package org-roam-server
    :after org-roam
    :init
    (progn
      (setq org-roam-server-host "127.0.0.1"
            org-roam-server-port 8080
            org-roam-server-export-inline-images t
            org-roam-server-authenticate nil
            org-roam-server-label-truncate t
            org-roam-server-label-truncate-length 60
            org-roam-server-label-wrap-length 20)
      )
    :config
    (progn
      (defun cats/org-roam-server-open ()
        "Ensure the server is active, then open the roam graph."
        (interactive)
        (org-roam-server-mode 1)
        (browse-url (format "http://localhost:%d" org-roam-server-port))))
    ))


;; org-roam-protocol

(defun cats-org/init-org-roam-protocol ()
  "Use org-roam-protocol."
  (use-package org-roam-protocol
    :after org-protocol))


;; deft

;; Allows me to quickly search through recently created org-roam files.
;; Configured to only look into my roam folder.
(defun cats-org/pre-init-deft ()
  (spacemacs|use-package-add-hook deft
    :post-init
    (progn
      (setq deft-directory (concat cats//org-dir "refs/notes"))
      ;; Create deft directory
      (unless (file-exists-p deft-directory)
        (make-directory deft-directory t))

      (setq
        deft-recursive t
        ;; de-couples filename and note title:
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
           (nospace . "-")
           (case-fn . downcase)))

      (setq deft-extensions '("org" "md" "txt")))
    :post-config
    (progn
      (add-to-list 'deft-extensions "tex")
      )))


;; org-bullets
(defun cats-org/pre-init-org-bullets ()
  (spacemacs|use-package-add-hook org-bullets
    :post-init
    (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "❀"))))


;; which-key
(defun cats-org/post-init-which-key ()
  "Replace rules for better naming of functions."
  (let ((new-descriptions
          '(("org-jira-\\(.+\\)" . "oi:\\1")
             ("org-journal-\\(.+\\)" . "oj:\\1")
             ("org-babel-\\(.+\\)" . "ob:\\1")
             ("org-table-\\(.+\\)" . "ot:\\1")
             ("org-roam-jump-to-index" . "or:jump-to-main")
             ("org-roam-dailies-\\(.+\\)" . "ord:\\1")
             ("org-roam-\\(.+\\)" . "or:\\1")
             ("org-agenda-\\(.+\\)" . "oa:\\1"))))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
        which-key-replacement-alist))))


;; ox-jira
(defun cats-org/pre-init-ox-jira ()
  (spacemacs|use-package-add-hook ox-jira
    :post-config
    (progn
      (define-key org-mode-map
        (kbd "M-S w") 'cats/ox-clip-formatted-copy)
      (define-key org-mode-map
        (kbd "C-c J") 'cats/org-export-jira-clipboard))))


;; ox-publish
(defun cats-org/pre-init-ox-publish ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-publish)))

(defun cats-org/init-ox-publish ()
  (use-package ox-publish
    :defer t
    :init
    (progn
    ;; Association list to control publishing behavior. Each element of the
    ;; alist is a publishing project.
    (setq org-publish-project-alist
      '(
         ("html-static"
           :base-directory "/data/www/static_html/"
           :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m"
           :include (".htaccess")
           :publishing-directory "/data/www/public_html/"
           :recursive t
           :publishing-function org-publish-attachment
           )

         ("pdf"
           :base-directory  "/data/org-mode/"
           :base-extension "org"
           :publishing-directory "/data/org-mode/pdf"
           :publishing-function org-latex-publish-to-pdf
           )

         ("org-notes"
           :base-directory "/data/www/org"
           :base-extension "org"
           :publishing-directory "/data/www/public_html/org"
           :recursive t
           :exclude ".*-reveal\.org"        ; exclude org-reveal slides
           :publishing-function org-html-publish-to-html
           :headline-levels 2               ; Just the default for this project.
           :auto-sitemap t                  ; Generate sitemap.org automagically...
           :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
           :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
           :with-creator nil    ; Disable the inclusion of "Created by Org" in the postamble.
           :with-email nil      ; Disable the inclusion of "(your email)" in the postamble.
           :with-author nil       ; Enable the inclusion of "Author: Your Name" in the postamble.
           :auto-preamble t;         ; Enable auto preamble
           :auto-postamble t         ; Enable auto postamble
           :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
           :toc-levels 2               ; Just the default for this project.
           :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
           :html-head-include-default-style nil ;Disable the default css style
           :html-head-include-scripts nil ;Disable the default javascript snippet
           :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.i3s.unice.fr/~malapert/css/worg.min.css\"/>\n<script type=\"text/javascript\" src=\"http://www.i3s.unice.fr/~malapert/js/ga.min.js\"></script>" ;Enable custom css style and other tags
           :html-link-home "index.html"    ; Just the default for this project.
           :html-link-up "../index.html"    ; Just the default for this project.
           )

         ("org-static"
           :base-directory "/data/www/org"
           :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m"
           :publishing-directory "/data/www/public_html/org/"
           :recursive t
           :publishing-function org-publish-attachment
           :exclude "Rplots.pdf"
           )

         ("org"
           :components ("org-notes" "org-static" "html-static")
           )

         ("_org-notes"
           :base-directory "/data/www/_org/"
           :base-extension "org"
           :publishing-directory "/data/www/private_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 2               ; Just the default for this project.
           :auto-preamble t
           :auto-sitemap nil                  ; Do NOT Generate sitemap.org automagically...
           :with-creator nil    ; Disable the inclusion of "Created by Org" in the postamble.
           :with-email nil      ; Disable the inclusion of "(your email)" in the postamble.
           :with-author nil       ; Enable the inclusion of "Author: Your Name" in the postamble.
           :auto-preamble t;         ; Enable auto preamble
           :auto-postamble t         ; Enable auto postamble
           :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
           :toc-levels 2               ; Just the default for this project.
           :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
           :html-head-include-default-style nil ;Disable the default css style
           :html-head-include-scripts nil ;Disable the default javascript snippet
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.i3s.unice.fr/~malapert/css/worg.min.css\"/>" ;Enable custom css style
           )

         ("_org-static"
           :base-directory "/data/www/_org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz"
           :publishing-directory "/data/www/private_html"
           :recursive t
           :publishing-function org-publish-attachment
           :exclude "Rplots.pdf"
           )

         ("_org"
           :components ("_org-notes" "_org-static")
           )
         )
      )
      )
    :config
    (progn
      )))


;; ox-md
(defun cats-org/pre-init-ox-md ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-md)))

(defun cats-org/init-ox-md ()
  (use-package ox-md
    :defer t
    :init (progn)
    :config (progn)))


;; ox-beamer
(defun cats-org/pre-init-ox-beamer ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-beamer)))

(defun cats-org/init-ox-beamer ()
  (use-package ox-beamer
    :defer t
    :init (progn)
    :config (progn)))


;; ox-bibtex
(defun cats-org/pre-init-ox-bibtex ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-bibtex)))

(defun cats-org/init-ox-bibtex ()
  (use-package ox-bibtex
    :defer t
    :init (progn)
    :config (progn)))


;; ox-latex
(defun cats-org/pre-init-ox-latex ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-latex)))

(defun cats-org/init-ox-latex ()
  (use-package ox-latex
    :defer t
    :init
    (progn
      ;; LaTeX compiler to use.
      (setq org-latex-compiler "latexmk"))
    :config
    (progn)))


;; ox
(defun cats-org/pre-init-ox ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox)))

(defun cats-org/init-ox ()
  (use-package ox
    :defer t
    :init
    (progn
      ;; Activate smart quotes during export
      (setq org-export-with-smart-quotes t)
      ;; Underscore in export
      (setq org-export-with-sub-superscripts nil)
      ;; The last level which is still exported as a headline.
      (setq org-export-headline-levels 3)
      ;; Set Background Color of Source Blocks for Export.This was taken from
      ;; [[http://emacs.stackexchange.com/questions/3374/][here]].
      (add-hook 'org-export-before-processing-hook 'cats//org-inline-css-hook)
      )
    :config (progn)))


;; org-present
(defun cats-org/pre-init-org-present ()
  (spacemacs|use-package-add-hook org-present
    :post-init
    ()))


;; org-pomodoro
(defun cats-org/pre-init-org-pomodoro ()
  (spacemacs|use-package-add-hook org-pomodoro
    :post-init
    ()))


;; org-ehtml
(defun cats-org/init-org-html ()
  (use-package org-ehtml
    :defer t
    :config
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'cats-org)))
        (setq org-ehtml-docroot (concat dir "ehtml")))
      (setq org-ehtml-allow-agenda t)
      (setq org-ehtml-editable-headlines t)
      (setq org-ehtml-everything-editable t))))


;; ox-html
(defun cats-org/pre-init-ox-html ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-html)))

(defun cats-org/init-ox-html ()
  (use-package ox-html
    :defer t
    :init
    (progn
      ;; Coding system for HTML export.
      (setq org-html-coding-system 'utf-8-unix)
      ;; Non-nil means include the default style in exported HTML files.
      (setq org-html-head-include-default-style nil)
      ;; Non-nil means include the JavaScript snippets in exported HTML files.
      (setq org-html-head-include-scripts nil)
      )
    :config
    (progn
      ;; Default attributes and values which will be used in table tags. This is
      ;; a plist where attributes are symbols, starting with colons, and values
      ;; are strings.
      (setq org-html-table-default-attributes '(
                                                 :border "0"
                                                 :cellspacing "0"
                                                 :cellpadding "6"
                                                 :rules "none"
                                                 :frame "none"))
      ;; Options for MathJax setup.
      (setf org-html-mathjax-options
        '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
           (scale "100")
           (align "center")
           (indent "2em")
           (mathml nil)))

      ;; The MathJax template.
      (setf org-html-mathjax-template
        "<script type=\"text/javascript\" src=\"%PATH\"></script>")

      ;; Allow with query params in image extentions
      (setq org-html-inline-image-rules
        '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\(\\?.*?\\)?\\'")
           ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\(\\?.*?\\)?\\'")
           ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\(\\?.*?\\)?\\'")))

      ;; Function to format headline text.
      (setq org-html-format-headline-function
        'cats//org-html-format-heading-function)

      ;; Add link icons in headings that lead to themselves. This is set before
      ;; and cleared afterwards, so that we know when we are generating the text
      ;; for the headline itself and when we are not.
      (advice-add 'org-html-headline :before 'cats//set-current-html-headline)
      (advice-add 'org-html-headline :after 'cats//clear-current-html-headline))))


;; ox-ascii
(defun cats-org/pre-init-ox-ascii ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-ascii)))

(defun cats-org/init-ox-ascii ()
  (use-package ox-ascii
    :defer t
    :init
    (progn
      (setq org-ascii-headline-spacing (quote (1 . 1)))
      (setq org-ascii-links-to-notes nil)
      )
    :config
    (progn
      )))


;; ox-confluence
(defun cats-org/pre-init-ox-confluence ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-confluence))

  (spacemacs|use-package-add-hook ox-confluence
    :post-init
    (progn
      (add-hook 'org-mode-hook
        (lambda ()
          (local-set-key (kbd "C-c C-h") 'org-toggle-link-display)

          (defun better-confluence-item (item contents info)
            (let* ((plain-list (org-export-get-parent item))
                    (type (org-element-property :type plain-list)))
              (case type
                (ordered
                  (concat (make-string (1+ (org-confluence--li-depth item)) ?\#)
                    " "
                    (org-trim contents)))
                (unordered (org-export-with-backend 'confluence item contents info))
                (descriptive (org-export-with-backend 'confluence item contents info)))))

          (org-export-define-derived-backend 'better-confluence 'confluence
            :translate-alist '((item . better-confluence-item)))

          (defun org-better-confluence-export-as-conf
            (&optional async subtreep visible-only body-only ext-plist)
            (interactive)
            (org-export-to-buffer 'better-confluence "*org CONFLUENCE Export*"
              async subtreep visible-only body-only ext-plist (lambda () (text-mode))))
          (setq org-startup-align-all-tables t)))
      )
    :post-config
    (progn
      ))
  )


;; ox-reveal
(defun cats-org/post-init-ox-reveal ()
  (use-package ox-reveal
    :init
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'cats-org)))
        (setq org-reveal-root (concat "file://" dir "reveal/reveal.js")))
      )))


;; org-notify
(defun cats-org/init-org-notify ()
  (use-package org-notify
    :disabled t
    :after org
    :config
    (progn
      (defun cats//org-notify-notification-handler (plist)
        (sauron-add-event 'org-notify 4 (format "%s, %s.\n" (plist-get plist :heading)
                                          (org-notify-body-text plist))))

      (setq org-show-notification-handler 'cats//org-notify-notification-handler)

      (org-notify-add 'default '(:time "1h" :actions cats//org-notify-notification-handler
                                  :period "2m" :duration 60))
      (org-notify-add 'default '(:time "100m" :actions cats//org-notify-notification-handler
                                  :period "2m" :duration 60))
      (org-notify-add 'urgent-second '(:time "3m" :actions (-notify/window -ding)
                                        :period "15s" :duration 10))
      (org-notify-add 'minute '(:time "5m" :actions -notify/window
                                 :period "100s" :duration 70))
      (org-notify-add '12hours
        '(:time "3m" :actions (-notify/window -ding)
           :period "15s" :duration 10)
        '(:time "100m" :actions -notify/window
           :period "2m" :duration 60)
        '(:time "12h" :actions -notify/window :audible nil
           :period "10m" :duration 200))
      (org-notify-add '5days
        '(:time "100m" :actions -notify/window
           :period "2m" :duration 60)
        '(:time "2d" :actions -notify/window
           :period "15m" :duration 100)
        '(:time "5d" :actions -notify/window
           :period "2h" :duration 200))
      (org-notify-add 'long-20days
        '(:time "2d" :actions -notify/window
           :period "15m" :duration 60)
        '(:time "5d" :actions -notify/window
           :period "2h" :duration 60)
        '(:time "20d" :actions -email :period "2d" :audible nil))
      (org-notify-add 'long-50days
        '(:time "4d" :actions -notify/window
           :period "30m" :duration 100)
        '(:time "10d" :actions -notify/window
           :period "4h" :duration 200)
        '(:time "50d" :actions -email :period "3d" :audible nil))
      (org-notify-add 'long-100days
        '(:time "2d" :actions -notify/window
           :period "1h" :duration 200)
        '(:time "10d" :actions -notify/window
           :period "10h" :duration 300)
        '(:time "50d" :actions -email :period "3d" :audible nil)
        '(:time "100d" :actions -email :period "5d" :audible nil))
      (org-notify-start 10))))


;; org-caldav
(defun cats-org/init-org-caldav ()
  (use-package org-caldav
    :defer t
    :config
    (progn)
    :init
    (progn)))


;; org-projectile
(defun cats-org/pre-init-org-projectile ()
  "Add `org-projectile' mode hooks."
  (spacemacs|use-package-add-hook org-projectile
    :pre-init
    (progn
      ;; Use <LEADER po> to go to project TODOs
      (setq org-confirm-elisp-link-function nil)
      ;; <LEADER aop> invokes with org-projectile/capture
      (setq org-projectile-capture-template
        (format "%s%s" "* TODO %?" cats//org-properties-string)))
    :post-config
    (progn
      ;; (require 'org-projectile-helm)

      (add-to-list 'org-capture-templates
        (org-projectile-project-todo-entry
          :capture-character "l"
          :capture-heading "Linked Project TODO"))

      (add-to-list 'org-capture-templates
        (org-projectile-project-todo-entry
          :capture-character "p"))

      (let* ((files (org-projectile-todo-files)))
        (dolist (file files)
          (cats//register-org-agenda-file file)))

      (with-eval-after-load 'org-agenda
        (cats//set-org-agenda-files cats//org-agenda-list))

      (if (file-name-absolute-p org-projectile-file)
          ;; one todo for all projects
          (progn
            )
        ;; per repo todo files
        (progn
          )))))


;; org-projectile-helm
(defun cats-org/init-org-projectile-helm ()
  "Add `org-projectile-helm' mode hooks."
  (use-package org-projectile-helm
    :commands (org-projectile-helm-template-or-project)
    :init
    (progn
      (spacemacs/set-leader-keys
        "pO" 'org-projectile-helm-template-or-project))))


;; org-agenda
(defun cats-org/pre-init-org-agenda ()
  (spacemacs|use-package-add-hook org-agenda
    :post-init
    (progn
      (cats//register-org-agenda-file cats//org-dir)

      ;; If `org-agenda-start-on-weekday` is set to an integer (by default it's
      ;; set to 1, corresponding to Monday), and `org-agenda-span` is set to
      ;; *either* `'week` or `7`, org will always start the agenda on the day
      ;; specified by `org-agenda-start-on-weekday`. To always start yesterday,
      ;; you must set `org-agenda-start-day` to `"-1d"` *and* do one of the
      ;; following:
      ;; 1. Set `org-agenda-start-on-weekday` to `nil`. Then, the first day of
      ;;    the agenda will be determined by `org-agenda-start-day`.
      ;; 2. Set `org-agenda-span` to a value that isn't `'week`. For example,
      ;;    setting it to `5` will show five days, and then it will respect
      ;;    `org-agenda-start-day`.

      ;; Setting: show five days starting yesterday
      (setq org-agenda-start-day "-1d")
      (setq org-agenda-span 5)
      (setq org-agenda-start-on-weekday nil)
      ;; Setting: Start on monadys
      ;; (setq org-agenda-span 7)
      ;; (setq org-agenda-start-on-weekday 1)

      (setq org-agenda-skip-scheduled-if-done t)
      (setq org-agenda-skip-deadline-if-done t))
    :pre-config
    (progn
      (cats//set-org-agenda-file-regexps cats//org-agenda-file-regexp-list t)
      (cats//set-org-agenda-files cats//org-agenda-list))
    :post-config
    (progn
      ;; (evilified-state-evilify-map org-agenda-mode-map
      ;;   :mode org-agenda-mode
      ;;   :bindings
      ;;   (kbd "q") 'org-agenda-exit
      ;;   (kbd "gr") 'org-agenda-redo)

      ;; My priority system:
      ;;
      ;; A - Absolutely MUST, at all costs, be completed by the provided due
      ;;     date. TODO: implement some type of extreme nagging system that
      ;;     alerts in an intrusive way for overdue A priority tasks.
      ;;
      ;; B - Should be given immediate attention if the due date is any time in
      ;;     the next two days. Failure to meet due date would be bad but not
      ;;     catastrophic.
      ;;
      ;; C - The highest priority to which tasks for which failure to complete
      ;;     on time would not have considerable significant consequences. There
      ;;     is still significant reason to prefer the completion of these tasks
      ;;     sooner rather than later.
      ;;
      ;; D - Failure to complete within a few days (or ever) of any deadline
      ;;     would be completely okay. As such, any deadline present on such a
      ;;     task is necessarily self imposed. Still probably worth doing
      ;;
      ;; E - Potentially not even worth doing at all, but worth taking a note
      ;;     about in case it comes up again, or becomes more interesting later.
      ;;
      ;; F - Almost certainly not worth attempting in the immediate future. Just
      ;;     brain dump.
      ;;
      ;; Priorities are somewhat contextual within each category. Things in the
      ;; gtd or work categories are generally regarded as much more important
      ;; than things with the same priority from the dotfiles category.
      ;;
      ;; Items without deadlines or scheduled times of a given priority can be
      ;; regarded as less important than items that DO have deadlines of that
      ;; same priority.

      (setq org-lowest-priority 69) ;; The character E
      (setq org-enforce-todo-dependencies t)
      (setq org-deadline-warning-days 0)
      (setq org-default-priority ?D)

      (let ((this-week-high-priority
              ;; The < in the following line has behavior that is opposite
              ;; to what one might expect.
              '(tags-todo "+PRIORITY<\"C\"+DEADLINE<\"<+1w>\"DEADLINE>\"<+0d>\""
                 ((org-agenda-overriding-header
                    "Upcoming high priority tasks:"))))
             (due-today '(tags-todo
                           "+DEADLINE=<\"<+1d>\"|+SCHEDULED=<\"<+0d>\""
                           ((org-agenda-overriding-header
                              "Due today:"))))
             (recently-created '(tags-todo
                                  "+CREATED=>\"<-30d>\""
                                  ((org-agenda-overriding-header "Recently created:")
                                    (org-agenda-cmp-user-defined 'org-cmp-creation-times)
                                    (org-agenda-sorting-strategy '(user-defined-down)))))
             (next '(todo "NEXT"))
             (started '(todo "STARTED"))
             (missing-deadline
               '(tags-todo "-DEADLINE={.}/!"
                  ((org-agenda-overriding-header
                     "These don't have deadlines:"))))
             (missing-priority
               '(tags-todo "-PRIORITY={.}/!"
                  ((org-agenda-overriding-header
                     "These don't have priorities:")))))

        ;; (add-to-list 'org-agenda-custom-commands
        ;;   `("c" "Current Status" ((agenda ""
        ;;       ((org-agenda-overriding-header "⚡ Agenda:")
        ;;        (org-agenda-remove-tags t)
        ;;        (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
        ;;        (org-agenda-prefix-format "  %-12s %-12t %-8c ")
        ;;        (org-agenda-todo-keyword-format "")))
        ;;      (todo "WAIT"
        ;;        ((org-agenda-overriding-header "⚡ Waiting for Someone Else:")
        ;;         (org-agenda-todo-ignore-deadlines (quote future))
        ;;         (org-agenda-todo-ignore-scheduled (quote future))
        ;;         (org-agenda-remove-tags t)
        ;;         (org-agenda-prefix-format "  %-8c ")
        ;;         (org-agenda-todo-keyword-format "")))
        ;;      (tags-todo "@phone|@email"
        ;;        ((org-agenda-overriding-header "⚡ Phone Calls to Make, Emails to Send:")
        ;;         (org-agenda-prefix-format "  %-8c ")
        ;;         (org-agenda-remove-tags t)
        ;;         (org-agenda-todo-keyword-format "")
        ;;         (org-agenda-todo-ignore-deadlines (quote all))
        ;;         (org-agenda-todo-ignore-scheduled (quote all))))
        ;;      (stuck ""
        ;;        ((org-agenda-overriding-header "⚡ Stuck Projects:")))
        ;;      (tags "+inbox+LEVEL=2|+orgzly+LEVEL=1"
        ;;        ((org-agenda-overriding-header "⚡ Inbox Tasks to Process:")
        ;;         (org-agenda-prefix-format "  %-8c ")
        ;;         (org-agenda-todo-keyword-format "")))
        ;;      (todo "NEXT"
        ;;        ((org-agenda-overriding-header "⚡ Next Actions:")
        ;;         (org-agenda-prefix-format "  %-8c ")
        ;;         (org-agenda-remove-tags t)
        ;;         (org-agenda-todo-keyword-format "")
        ;;         (org-agenda-todo-ignore-deadlines (quote all))
        ;;         (org-agenda-todo-ignore-scheduled (quote all)))))))

        ;; (add-to-list 'org-agenda-custom-commands
        ;;   `("p" "Project List"
        ;;      ((tags "+project+LEVEL=3"))))

        ;; (add-to-list 'org-agenda-custom-commands
        ;;   `("e" "Tasks by Energy Level"
        ;;      ((tags-todo "5m")
        ;;        (tags-todo "30m")
        ;;        (tags-todo "1h"))
        ;;      ((org-agenda-todo-ignore-deadlines nil))))

        ;; (add-to-list 'org-agenda-custom-commands
        ;;   `("o" "Offline Tasks"
        ;;      ((tags-todo "+@offline"))))

        ;; (add-to-list 'org-agenda-custom-commands
        ;;   `("T" "Travel Schedule"
        ;;      ((tags "+travel+TIMESTAMP>=\"<now>\""))
        ;;      ((org-agenda-view-columns-initially t))))


        (add-to-list 'org-agenda-custom-commands
          `("h" "A, B priority:" tags-todo "+PRIORITY<\"C\""
             ((org-agenda-overriding-header
                "High Priority:"))))

        (add-to-list 'org-agenda-custom-commands
          `("c" "At least priority C:" tags-todo "+PRIORITY<\"D\""
             ((org-agenda-overriding-header
                "At least priority C:"))))

        (add-to-list 'org-agenda-custom-commands
          `,(cons "r" (cons "Recently created" recently-created)))

        (add-to-list 'org-agenda-custom-commands
          `,(cons "d" (cons "Overdue tasks and due today" due-today)))

        (add-to-list 'org-agenda-custom-commands
          `,(cons "A" (cons "High priority upcoming" this-week-high-priority)))

        (add-to-list 'org-agenda-custom-commands
          `("M" "Main agenda view"
             ((agenda ""
                ((org-agenda-overriding-header "Agenda:")
                  (org-agenda-ndays 5)
                  (org-deadline-warning-days 0)))
               ,due-today
               ,next
               ,started
               ,this-week-high-priority
               ,recently-created)
             nil nil)))

      (defadvice org-agenda-list (around opened-org-agenda-list-around activate)
        (cats//opened-org-agenda-files)
        ad-do-it
        (cats//kill-org-agenda-files))

      (defadvice org-search-view (around org-search-view-around activate)
        (cats//opened-org-agenda-files)
        ad-do-it
        (cats//kill-org-agenda-files))

      (defadvice org-tags-view (around org-tags-view-around activate)
        (cats//opened-org-agenda-files)
        ad-do-it
        (cats//kill-org-agenda-files))

      (org-super-agenda-mode))))


;; org-super-agenda
(defun cats-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :commands (org-super-agenda-mode)
    :init
    (progn
        (spacemacs|add-toggle org-super-agenda
          :status org-super-agenda-mode
          :on (progn
                (when (bound-and-true-p org-super-agenda-mode)
                  (org-super-agenda-mode -1))
                (org-super-agenda-mode))
          :off (org-super-agenda-mode -1)
          :documentation "Org Super agenda."
          :evil-leader-for-mode (org-agenda-mode . "Ts"))

      (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
           (:name "Today"  ; Optionally specify section name
             :time-grid t  ; Items that appear on the time grid
             :todo "TODAY")  ; Items that have this TODO keyword
           (:name "Important"
             ;; Single arguments given alone
             :tag "bills"
             :priority "A")
           ;; Set order of multiple groups at once
           (:order-multi (2 (:name "Shopping in town"
                              ;; Boolean AND group matches items that match all subgroups
                              :and (:tag "shopping" :tag "@town"))
                           (:name "Food-related"
                             ;; Multiple args given in list with implicit OR
                             :tag ("food" "dinner"))
                           ;; (:name "Personal"
                           ;;   :habit t
                           ;;   :tag "personal")
                           (:name "Space-related (non-moon-or-planet-related)"
                             ;; Regexps match case-insensitively on the entire entry
                             :and (:regexp ("space" "NASA")
                                    ;; Boolean NOT also has implicit OR between selectors
                                    :not (:regexp "moon" :tag "planet")))))
           ;; Groups supply their own section names when none are given
           (:todo "WAIT" :order 8)  ; Set order of this section
           (:todo ("EXPIRED" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
             ;; Show this group at the end of the agenda (since it has the
             ;; highest number). If you specified this group last, items with
             ;; these todo keywords that e.g. have priority A would be displayed
             ;; in that group instead, because items are grouped out in the
             ;; order the groups are listed.
             :order 9)
           (:priority<= "B"
             ;; Show this section after "Today" and "Important", because their
             ;; order is unspecified, defaulting to 0. Sections are displayed
             ;; lowest-number-first.
             :order 1)
           ;; My 2 configs
           (:order-multi (1 (:name "High priority"
                              :priority> "C")))
           (:order-multi (1 (:name "Done today"
                              :and (:regexp "State \"DONE\""
                                     :log t))))
           ;; After the last group, the agenda will display items that didn't
           ;; match any of these groups, with the default order position of 99
           )))))


;; org-jira
(defun cats-org/pre-init-org-jira ()
  (spacemacs|use-package-add-hook org-jira
    :post-init
    (progn
      (setq org-jira-working-dir cats//org-jira-dir
        org-jira-deadline-duedate-sync-p nil
        org-jira-worklog-sync-p nil
        org-jira-property-overrides nil
        ;; Default jql for querying your Jira tickets.
        org-jira-default-jql cats-jira-default-jql)

      ;; Create org roam dailies directory
      (unless (file-exists-p org-jira-working-dir)
        (make-directory org-jira-working-dir t))

      ;; jira req https://jira.int.payoff.com/rest/api/2/issuetype | jq '.[] | .name'
      ;; issue_types=("Bug" "Improvement" "Epic" "Task" "Sub-task" "Story" "User Story" "Research" "Data Pull" "Dashboard" "Automation" "Reporting" "Change Request" "SoftwareDeployment" "InfrastructureDeployment" "ProductionConfigChange" "PermissionsChange" "QA Sub-task" "Analysis" "Modeling" "Training" "Access" "IT Help" "UIE - Design" "Purchase" "Change" "Fault" "Off-Boarding" "LegacyIssue" "NewPlatformIssue" "MemberDataExport" "MemberDataDelete" "HMA" "Product Task" "Training Task" "QA task" "New Feature" "Service Request")
      ;; jira req https://jira.int.payoff.com/rest/api/2/priority | jq '.[] | .name'
      ;; priority_types=("P1" "P2" "P3" "P4" "P5" "Blocker" "High" "Medium" "Low" "Minor")
      ;; jira req https://jira.int.payoff.com/rest/api/2/status | jq '.[] | .name'
      ;; done_status_types=("Closed" "Fix_Resolve" "Resolved" "Deployed" "Deploy" "Done" "Cancelled" "Canceled")
      ;; jira req https://jira.int.payoff.com/rest/api/2/resolution | jq '.[] | .name

      (setq org-jira-custom-jqls
        '(
           (:jql " project = UIE AND issuetype in (Story, Task, Bug, Research) AND (Sprint = EMPTY OR Sprint not in (openSprints(), futureSprints())) AND resolution = Unresolved AND status not in ('Canceled', 'Cancelled', 'Closed', 'Deploy', 'Deployed', 'Done', 'Fix_Resolve', 'Resolved') ORDER BY Rank ASC "
             :limit 200
             :filename "backlog")
           (:jql " reporter = currentUser() order by created DESC "
             :limit 100
             :filename "reported-by-me")
           (:jql " assignee = currentUser() AND resolution = Unresolved order by updated DESC "
             :limit 100
             :filename "my-open-issues")
           (:jql " sprint in (openSprints()) AND 'Story Points' is EMPTY AND assignee = currentUser() "
             :limit 100
             :filename "my-current-tickets-without-estimates")
           (:jql " sprint in (futureSprints()) AND 'Story Points' is EMPTY AND assignee = currentUser() "
             :limit 100
             :filename "my-future-tickets-without-estimates")
           (:jql " status changed to done after -1d  AND project in (projectsLeadByUser()) "
             :limit 100
             :filename "completed-in-last-24-hours")
           (:jql " created >= -1d AND project in (projectsLeadByUser()) ORDER BY sprint ASC, created DESC "
             :limit 100
             :filename "added-in-the-last-24-hours")
           (:jql " (summary ~ currentUser() OR description ~ currentUser() OR comment ~ currentUser()) AND status not in ('Canceled', 'Cancelled', 'Closed', 'Deploy', 'Deployed', 'Done', 'Fix_Resolve', 'Resolved') AND updatedDate >= -7d ORDER BY lastViewed ASC "
             :limit 100
             :filename "mentions-me")
           (:jql " (status not in ('Canceled', 'Cancelled', 'Closed', 'Deploy', 'Deployed', 'Done', 'Fix_Resolve', 'Resolved') OR updatedDate >= -7d) AND (assignee in (currentUser()) OR (summary ~ currentUser() OR description ~ currentUser() OR comment ~ currentUser()) AND status not in ('Canceled', 'Cancelled', 'Closed', 'Deploy', 'Deployed', 'Done', 'Fix_Resolve', 'Resolved') AND updatedDate >= -7d AND Sprint in (openSprints())) AND (sprint not in closedSprints() OR sprint is EMPTY) AND issuetype != Epic ORDER BY duedate ASC, priority DESC, updated ASC, createdDate ASC "
             :limit 100
             :filename "my-open-tasks")
           (:jql " sprint in openSprints() AND priority not in (Low, Medium, Minor, P5, P4, P3, P2) AND assignee in (currentUser()) "
             :limit 100
             :filename "high-priority-and-in-an-open-sprint")
           (:jql " updatedDate >= -7d AND Sprint in openSprints() AND assignee in (currentUser()) "
             :limit 100
             :filename "recently-updated-and-in-open-sprint")
           (:jql " project = UIE AND status = Closed AND resolved >= -1w ORDER BY updated DESC "
             :limit 100
             :filename "closed-this-week-(awesome-for-weekly-reports)")
           (:jql " project = UIE AND status not in (Canceled, Cancelled, Closed, Deploy, Deployed, Done, Fix_Resolve, Resolved) AND 'Epic Link' is EMPTY AND type != Epic "
             :limit 100
             :filename "open-tickets-without-epics")
           ;; Tickets created since 2018 which have been closed which did not get dev attention (Duplicate, Won't Fix, Incomplete definition).
           ;; (:jql " project = UIE AND created >= '2018/01/01 00:00' AND Resolution not in (Complete, Fixed, Done, Passed) AND status in (Canceled, Cancelled, Closed, Deploy, Deployed, Done, Fix_Resolve, Resolved) ORDER BY updated DESC "
           ;;   :limit 100
           ;;   :filename "tickets-created-that-been-closed-but-did-not-get-dev-attention")
           ;; All new tickets opened during the pilot window (includes duplicates, and incomplete tickets reported through the JIRA public feedback form).
           ;; (:jql " project = UIE AND created >= '2015/02/23 00:00' AND created <= '2015/03/06 00:00' ORDER BY updated DESC "
           ;;   :limit 100
           ;;   :filename "all-new-tickets-opened-during-the-pilot-window")
           ;; Tickets which were closed during the pilot window (regardless of when they were created).
           ;; (:jql " project = UIE AND resolved >= '2015/02/23 00:00' AND resolved <= '2015/03/06 00:00' ORDER BY updated DESC "
           ;;   :limit 100
           ;;   :filename "tickets-which-were-closed-during-the-pilot-window")
           ;; Tickets that were opened during the a specific window AND resolved during this time.
           ;; (:jql " project = UIE AND created >= '2015/02/23 00:00' AND resolved <= '2015/03/06 00:00' ORDER BY updated DESC "
           ;;   :limit 100
           ;;   :filename "tickets-that-were-opened-during")

           ;; bitbucket props
           ;; issue.property[development].commits
           ;; issue.property[development].prs
           ;; issue.property[development].openprs
           ;; issue.property[development].reviews
           ;; issue.property[development].failingbuilds
           (:jql " project = UIE AND Sprint in openSprints() AND issue.property[development].openprs = 0 "
             :limit 100
             :filename "all-current-tasks-without-an-open-pr")

           ;; monitor if any tickets have been created and moved into active sprint (week 1)
           (:jql " project = UIE AND sprint in openSprints() AND createdDate > startOfWeek() ORDER BY createdDate DESC "
             :limit 100
             :filename "tickets-created-and-moved-into-sprint-week1")
           ;; monitor if any tickets have been created and moved into active sprint (week 2)
           (:jql " project = UIE AND sprint in openSprints() AND createdDate > startOfWeek(-1) ORDER BY createdDate DESC "
             :limit 100
             :filename "tickets-created-and-moved-into-sprint-week2")
           ;; tickets have been created, worked on, and not moved into sprint (week 1)
           (:jql " project = UIE AND (Sprint = EMPTY OR Sprint not in (openSprints(), futureSprints())) AND status not in ('Open(Backlog)') AND createdDate > startOfWeek() ORDER BY createdDate DESC "
             :limit 100
             :filename "tickets-created-worked-on-not-moved-into-sprint-week1")
           ;; tickets have been created, worked on, and not moved into sprint (week 2)
           (:jql " project = UIE AND (Sprint = EMPTY OR Sprint not in (openSprints(), futureSprints())) AND status not in ('Open(Backlog)') AND createdDate > startOfWeek(-1) ORDER BY createdDate DESC "
             :limit 100
             :filename "tickets-created-worked-on-not-moved-into-sprint-week2")
           ;; monitor if any tickets have their status changed during active sprint, but not in it (week 1)
           (:jql " project = UIE AND (Sprint = EMPTY OR Sprint not in (openSprints(), futureSprints())) AND status not in ('Open(Backlog)') AND status changed after startOfWeek() ORDER BY createdDate DESC "
             :limit 100
             :filename "tickets-status-changed-but-not-in-sprint-week1")
           ;; monitor if any tickets have their status changed during active sprint, but not in it (week 2)
           ;; status changed during: (startOfWeek(-1), now())
           (:jql " project = UIE AND (Sprint = EMPTY OR Sprint not in (openSprints(), futureSprints())) AND status not in ('Open(Backlog)') AND status changed after startOfWeek(-1) ORDER BY createdDate DESC "
             :limit 100
             :filename "tickets-status-changed-but-not-in-sprint-week2")
           ))
      )
    :post-config
    (progn
      )))


;; org
(defun cats-org/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :pre-config
    (progn)
    :post-config
    (progn
      ;; (evilified-state-evilify-map org-mode-map
      ;;   :mode org-mode
      ;;   :bindings
      ;;   (kbd "gr") 'org-reload)

      ;; When you perform a text search (the "s" selection from the `org-agenda`
      ;; pop-up), include the archives for all of the files in Org's agenda
      ;; files list. If you archive things regularly, which I do, this helps you
      ;; dig stuff out of there when you're looking for it.
      ;; (setq org-agenda-text-search-extra-files '(agenda-archives))

      ;; I tend to leave a blank line at the end of the content of each task
      ;; entry. This causes Org to automatically place a blank line before a new
      ;; heading or plain text list item, just the way I like it.
      ;; (setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))

      ;; This one is pretty awesome; it forces you to mark all child tasks as
      ;; "DONE" before you can mark the parent as "DONE." The agenda view
      ;; already has the notion of "blocked" tasks (those with incomplete child
      ;; tasks), which should appear dimmed (that, of course, is also
      ;; customizable). This makes it even harder to slack off on your work.
      ;; (setq org-enforce-todo-dependencies t)

      ;; I like to know when tasks have changed status. Setting this option
      ;; causes Org to insert an annotation in a task when it is marked as done
      ;; including a timestamp of when exactly that happened.
      ;; (setq org-log-done (quote time))

      ;; Adding yet further auditing, this option causes Org to insert
      ;; annotations when you change the deadline of a task, which will note the
      ;; previous deadline date and when it was changed. Very useful for
      ;; figuring out how many times you "kicked the can down the road."
      ;; (setq org-log-redeadline (quote time))

      ;; This does the same as above, but for the scheduled dates, which I use
      ;; more often.
      ;; (setq org-log-reschedule (quote time))

      ;; https://orgmode.org/worg/org-contrib/babel/languages.html
      (add-to-list 'org-babel-load-languages '(org . t))
      (add-to-list 'org-babel-load-languages '(C . t))
      (add-to-list 'org-babel-load-languages '(dot . t))
      (add-to-list 'org-babel-load-languages '(awk . t))
      (add-to-list 'org-babel-load-languages '(ditaa . t))
      (add-to-list 'org-babel-load-languages '(gnuplot . t))
      (add-to-list 'org-babel-load-languages '(maxima . t))
      (add-to-list 'org-babel-load-languages '(octave . t))
      (add-to-list 'org-babel-load-languages '(perl . t))
      (add-to-list 'org-babel-load-languages '(sql . t))
      (add-to-list 'org-babel-load-languages '(sqlite . t))

      ;; Alist between context and visibility span when revealing a location.
      (add-to-list 'org-show-context-detail '(org-goto . lineage))

      ;; Modules that should always be loaded together with org.el.
      (add-to-list 'org-modules 'org-habit)
      (add-to-list 'org-modules 'org-expiry)
      (add-to-list 'org-modules 'org-notify))
    :post-init
    (progn
      (add-hook 'org-mode-hook 'cats/org-mode-local)
      (cats//add-org-keybindings 'org-mode)

      (let ((dir (configuration-layer/get-layer-local-dir 'cats-org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))

      ;; Directory with Org files.
      (setq org-directory cats//org-dir)
      ;; Create org-directory
      (unless (file-exists-p org-directory)
        (make-directory org-directory))

      ;; Default target for storing notes.
      (setq org-default-notes-file
        (expand-file-name
          (concat cats//org-dir cats//org-notes-file)))

      (setq org-ellipsis "⤵")
      ;; When nil, these commands will be disabled, so that you never
      ;; accidentally set a priority.
      (setq org-enable-priority-commands nil)
      ;; Non-nil means fast tag selection exits after first change.
      (setq org-fast-tag-selection-single-key t)
      ;; Information to record when a task moves to the DONE state.
      (setq org-log-done t)
      ;; When non-nil, fontify code in code blocks.
      (setq org-src-fontify-natively t)
      ;; Non-nil means switching TODO states with S-cursor counts as state
      ;; change. This is the default behavior. However, setting this to nil
      ;; allows a convenient way to select a TODO state and bypass any logging
      ;; associated with that.
      (setq org-treat-S-cursor-todo-selection-as-state-change nil)
      ;; Non-nil means use the fast todo selection scheme with org-todo. This
      ;; variable describes if and under what circumstances the cycling
      ;; mechanism for TODO keywords will be replaced by a single-key, direct
      ;; selection scheme.
      (setq org-use-fast-todo-selection t)
      ;; The maximum level for Imenu access to Org headlines. This also applied
      ;; for speedbar access. This is consistent with the value of
      ;; `helm-org-headings-max-depth'.
      (setq org-imenu-depth 10)
      ;; hide emphasis-markers
      (setq org-hide-emphasis-markers t)
      ;; Check if in invisible region before inserting or deleting a character.
      (setq org-catch-invisible-edits 'smart)
      ;; https://github.com/dakrone/eos/blob/master/eos-org.org
      ;; Special begin/end of line to skip tags and stars, `C-a' and `C-e' behave
      ;; specially in headlines and items.
      (setq org-special-ctrl-a/e t)
      ;; Special keys for killing a headline. Non-nil means `C-k' will behave
      ;; specially in headlines.
      (setq org-special-ctrl-k t)
      ;; Provide control over stripping of leading/trailing blank lines from code
      ;; blocks. Blank lines are removed when exiting the code edit buffer
      (setq org-src-strip-leading-and-trailing-blank-lines t)
      ;; Return on a link breaks the link? Just follow it. Non-nil means on links
      ;; RET will follow the link. In tables, the special behavior of RET has
      ;; precedence."
      (setq org-return-follows-link t)
      ;; Smart yanking:
      ;; https://www.gnu.org/software/emacs/manual/html_node/org/Structure-editing.html
      (setq org-yank-adjusted-subtree t)
      ;; The column to which tags should be indented in a headline.
      (setq org-tags-column -80)
      ;; Non-nil means entering Org mode will switch to OVERVIEW.
      (setq org-startup-folded t)
      ;; Indentation for the content of a source code block.
      (setq org-edit-src-content-indentation 0)
      ;; The default interface to be used for `org-goto'.
      (setq org-goto-interface 'outline-path-completion)
      ;; Maximum target level when running `org-goto' with refile interface."
      (setq org-goto-max-level 10)
      ;; List of property/value pairs that can be inherited by any entry.
      (setq org-global-properties
        '(quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                  ("STYLE_ALL" . "habit"))))
      ;; The TODO state to which a repeater should return the repeating task.
      (setq org-todo-repeat-to-state "TODO")
      ;; The default column format, if no other format has been defined. This
      ;; variable can be set on the per-file basis by inserting a line
      (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
      ;; Name of the log drawer, as a string, or nil.
      (setq org-log-into-drawer t)
      ;; Information to record when the scheduling date of a tasks is modified.
      (setq org-log-reschedule t)
      ;; Information to record when the deadline date of a tasks is modified.
      (setq org-log-redeadline t)
      ;; Non-nil means inserting a TODO heading is treated as state change.
      (setq org-treat-insert-todo-heading-as-state-change t)

      ;; (add-hook 'org-insert-heading-hook 'cats/insert-heading-inactive-timestamp 'append)

      (with-eval-after-load 'ispell
        (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
        (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
        (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))))
    :pre-init
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'cats-utils)))
        (setq org-plantuml-jar-path (concat dir "plantuml/plantuml.jar")))))

  (spacemacs|use-package-add-hook org
    "List of TODO entry keyword sequences and their interpretation. "
    :post-init
    (progn
      ;; What follows is a description of the significance of each of the values
      ;; available in `org-todo-keywords'. All headings with one of these
      ;; keywords deal with the concept of the completion of some task or
      ;; collection of tasks to bring about a particular state of affairs. In
      ;; some cases, the actual tasks involved may not be known at the time of
      ;; task creation.

      ;; Incomplete States:

      ;; IDEA - This TODO exists in only the most abstract sense: it is an
      ;; imagined state of affairs that requires tasks that are either not yet
      ;; known, or have not thoroughly been considered.

      ;; RESEARCH - This TODO needs to be investigated further before action can
      ;; be taken to achieve the desired outcome. It is not known how much time
      ;; and effort will be consumed in the actual completion of the task.

      ;; TODO - The scope and work involved in this TODO are well understood,
      ;; but for some reason or another, it is not something that should be
      ;; attempted in the immediate future. Typically this is because the task
      ;; is not considered a top priority, but it may also be for some other
      ;; reason.

      ;; NEXT - This TODO is immediately actionable and should be started in the
      ;; immediate future.

      ;; STARTED - Work on this TODO has already started, further work is
      ;; immediately actionable.

      ;; WAIT - The work involved in this TODO is well understood, but it is
      ;; blocked for the time being.

      ;; BACKLOG - While technically actionable, this task is not only not worth
      ;; pursuing in the immediate future, but the foreseable future. It exists
      ;; as a task mostly as a note/reminder, in case it becomes higher priority
      ;; in the future.

      ;; Complete States:

      ;; DONE - This TODO has been completed exactly as imagined.

      ;; HANDLED - This TODO was completed in spirit, though not by the means
      ;; that were originally imagined/outlined in the TODO.

      ;; EXPIRED - The owner of this TODO failed to take action on it within the
      ;; appropriate time period, and there is now no point in attempting it.

      ;; CANCELLED - For whatever reason, this TODO should no longer be
      ;; attempted. This TODO is typically used in contrast to the EXPIRED TODO
      ;; to indicate that the owner is not necessarily to blame.

      ;; Each keyword can optionally specify a character for fast state
      ;; selection and specifiers for state change logging.
      ;;
      ;; - "WAIT(w)" says that the WAIT state can be selected with the "w" key.
      ;; - "WAIT(w!)" indicates to record a time stamp each when selected.
      ;; - "WAIT(w@/!)"
      ;;     - "@" means to add a note (with time),
      ;;     - "!" means to record only the time of the state change.
      ;;     - With X and Y being either "@" or "!", "X/Y" means use X when
      ;;       entering the state, and use Y when leaving the state if and only
      ;;       if the *target* state does not define X.
      ;;     - You may omit any of the fast-selection key or X or /Y, so
      ;;       WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.
      (setq org-todo-keywords
        '((sequence
            "IDEA(i!)"
            "RESEARCH(r!)"
            "TODO(t!)"
            "NEXT(n!)"
            "STARTED(s!)"
            "WAIT(w!)"
            "BACKLOG(b!)"
            "|"
            "DONE(d!)"
            "HANDLED(h!)"
            "EXPIRED(e!)"
            "CANCELLED(c!)"
            )))

      ;; '(org-stuck-projects
      ;;    (quote ("+project+LEVEL=3-notes-TODO=\"DONE\""
      ;;             ("NEXT" "WAIT" "BACKLOG") nil "")))

      ;; Tag changes that should be triggered by TODO state changes.
      (setq org-todo-state-tags-triggers
        ' (("CANCELLED" ("CANCELLED" . t))
            ("WAIT" ("WAIT" . t))
            ("TODO" ("WAIT") ("CANCELLED"))
            ("NEXT" ("WAIT") ("CANCELLED"))
            ("DONE" ("WAIT") ("CANCELLED"))))

      ;; Faces for specific TODO keywords.
      (setq org-todo-keyword-faces
        '(
           ("CANCELLED" . (:foreground "LimeGreen" :weight bold))
           ("HANDLED" . (:foreground "LimeGreen" :weight bold))
           ("EXPIRED" . (:foreground "LimeGreen" :weight bold))
           ("DONE" . (:foreground "green" :weight bold))
           ("IDEA" . (:foreground "GoldenRod" :weight bold))
           ("IMPEDED" . (:foreground "red" :weight bold))
           ("INPR" . (:foreground "yellow" :weight bold))
           ("NEXT" . (:foreground "IndianRed1" :weight bold))
           ("STARTED" . (:foreground "OrangeRed" :weight bold))
           ("WAIT" . (:foreground "coral" :weight bold))))))

  (spacemacs|use-package-add-hook org
    "Tags in Org files."
    :post-init
    (progn
      ;; Tags always available in Org files
      (setq org-tag-persistent-alist
        '((:startgroup . nil)
           ("HOME" . ?h)
           ("RESEARCH" . ?r)
           ("TEACHING" . ?t)
           (:endgroup . nil)
           (:startgroup . nil)
           ("OS" . ?o)
           ("DEV" . ?d)
           ("WWW" . ?w)
           (:endgroup . nil)
           (:startgroup . nil)
           ("EASY" . ?e)
           ("MEDIUM" . ?m)
           ("HARD" . ?a)
           (:endgroup . nil)
           ("URGENT" . ?u)
           ("KEY" . ?k)
           ("BONUS" . ?b)
           ("noexport" . ?x)))

      ;; Default tags available in Org files. (tasks with GTD contexts)
      (setq org-tag-alist '(("@work" . ?b)
                             ("@home" . ?h)
                             ("@errands" . ?e)
                             ("@coding" . ?c)
                             ("@phone" . ?p)
                             ("@email" . ?m)
                             ("@reading" . ?r)
                             ("@computer" . ?l)))))

  (spacemacs|use-package-add-hook org
    "Options concerning refiling entries in Org mode."
    :post-init
    (progn
      ;; Targets include this file and any file contributing to the agenda,
      ;; Up to 9 levels deep
      (setq org-refile-targets '((nil :maxlevel . 9)
                                  (org-agenda-files :maxlevel . 9)))
      ;; Use full outline paths for refile targets - we file directly with IDO
      (setq org-refile-use-outline-path t)
      ;; Targets complete directly with IDO
      (setq org-outline-path-complete-in-steps nil)
      ;; Allow refile to create parent tasks with confirmation
      (setq org-refile-allow-creating-parent-nodes 'confirm)
      (setq org-refile-target-verify-function 'cats//verify-refile-target)))

  (spacemacs|use-package-add-hook org
    "Links in Org files."
    :post-config
    (progn
      ;; YouTube videos
      ;;
      ;; To use this, just write your org links in the following way (optionally
      ;; adding a description). [[yt:A3JAlWM8qRM]]
      ;;
      ;; When you export to HTML, this will produce that same inlined snippet that
      ;; Youtube specifies. The advantage (over simply writing out the iframe) is
      ;; that this link can be clicked in org-mode, and can be exported to other
      ;; formats as well.
      (org-link-set-parameters "yt"
        :follow (lambda (handle)
                  (browse-url
                    (concat "https://www.youtube.com/embed/"
                      handle)))
        :export (lambda (path desc backend)
                  (cl-case backend
                    (html (format cats//org-yt-iframe-format
                            path (or desc "")))
                    (latex (format "\href{%s}{%s}"
                             path (or desc "video"))))))
      )
    :post-init
    (progn
      ;; Define some handy link abbreviations
      ;; example: [[bmap:space needle]]
      (setq org-link-abbrev-alist '(
         ("bing" . "http://www.bing.com/search?q=%sform=OSDSRC")
         ("cpan" . "http://search.cpan.org/search?query=%s&mode=all")
         ("google" . "http://www.google.com/search?q=")
         ("gmap" . "http://maps.google.com/maps?q=%s")
         ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
         ("bmap" . "http://www.bing.com/maps/default.aspx?q=%s&mkt=en&FORM=HDRSC4")
         ("wiki" . "http://en.wikipedia.org/wiki/")
         ("rfc" . "http://tools.ietf.org/rfc/rfc%s.txt")
         ("ads" . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")))
      ))
  )


;; org-faces
(defun cats-org/init-org-faces ()
  (use-package org-faces
    :defer t
    :init
    (progn
      ;; Faces for specific tags.
      (setq org-tag-faces
        '(
           ("HOME" . (:foreground "GoldenRod" :weight bold))
           ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
           ("TEACHING" . (:foreground "GoldenRod" :weight bold))
           ("OS" . (:foreground "IndianRed1" :weight bold))
           ("DEV" . (:foreground "IndianRed1" :weight bold))
           ("WWW" . (:foreground "IndianRed1" :weight bold))
           ("URGENT" . (:foreground "Red" :weight bold))
           ("KEY" . (:foreground "Red" :weight bold))
           ("EASY" . (:foreground "OrangeRed" :weight bold))
           ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
           ("HARD" . (:foreground "OrangeRed" :weight bold))
           ("BONUS" . (:foreground "GoldenRod" :weight bold))
           ("noexport" . (:foreground "LimeGreen" :weight bold))))
      )
    :config
    (progn)
    ))


;; org-clock
(defun cats-org/init-org-clock ()
  (use-package org-clock
    :defer t
    :init
    (progn
      ;; Some clock stuff. taken from http://doc.norang.ca/org-mode.org
      ;;
      ;; Save the running clock and all clock history when exiting Emacs, load
      ;; it on startup
      (setq org-clock-persist t)
      ;; Resume clocking task when emacs is restarted
      (org-clock-persistence-insinuate)
      ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
      (setq org-clock-history-length 23)
      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)
      ;; Change tasks to NEXT when clocking in
      (setq org-clock-in-switch-to-state 'cats//clock-in-to-next)
      ;; Separate drawers for clocking and logs
      (setq org-drawers (quote ("PROPERTIES" "LOGBOOK" "RESULTS")))
      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)
      ;; Clock out when moving task to a done state
      (setq org-clock-out-when-done t)
      ;; Do not prompt to resume an active clock
      (setq org-clock-persist-query-resume nil)
      ;; Enable auto clock resolution for finding open clocks
      (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
      ;; Include current clocking task in clock reports
      (setq org-clock-report-include-clocking-task t)
      (setq org-duration-format '(("d" . nil) ("h" . t) ("min" . t)))
      (setq cats//keep-clock-running nil)
      ;; (add-hook 'org-clock-out-hook 'cats/clock-out-maybe 'append)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "CI" 'cats/punch-in
        "CO" 'cats/punch-out)
      )
    :config
    (progn)
    ))


;; org-id
(defun cats-org/init-org-id ()
  (use-package org-id
    :defer t
    :init
    (progn
      (setq org-id-method 'uuidgen)
      )
    :config
    (progn)
    ))


;; org-indent
(defun cats-org/init-org-indent ()
  (use-package org-indent
    :defer t
    :init
    (progn
      (setq org-indent-indentation-per-level 2)
      )
    :config
    (progn)
    ))


;; org-list
(defun cats-org/init-org-list ()
  (use-package org-list
    :defer t
    :init
    (progn
      ;; Default bullet type installed when demoting an item. This is an
      ;; association list, for each bullet type, this alist will point to the
      ;; bullet that should be used when this item is demoted.
      (setq org-list-demote-modify-bullet '(
                                             ("+" . "-")
                                             ("*" . "-")
                                             ("1." . "-")
                                             ("1)" . "a)")))
      )
    :config
    (progn)
    ))


;; org-src
(defun cats-org/init-org-src ()
  (use-package org-src
    :defer t
    :init
    (progn
      ;; If non-nil, the effect of TAB in a code block is as if it were issued in
      ;; the language major mode buffer.
      (setq org-src-tab-acts-natively t)

      ;; How the source code edit buffer should be displayed.
      (setq org-src-window-setup 'current-window)

      ;; IvanMalison/frame-mode: `frame-mode` configures `display-buffer-alist`
      ;; so that calls to display-buffer that result in a buffer being displayed
      ;; somewhere that does not replace the current buffer always use a
      ;; different frame instead of using a different window
      ;; (when frame-mode
      ;;   (progn
      ;;     (setcdr (assoc 'file org-link-frame-setup) 'find-file-other-frame)))

      ;; If non-nil preserve leading whitespace characters on export.
      (setq org-src-preserve-indentation t)
      )
    :config
    (progn
      ;; Alist mapping languages to their major mode.
      (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

      ;; (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
      ;;   dotspacemacs-major-mode-leader-key 'org-edit-src-exit
      ;;   "c" 'org-edit-src-exit
      ;;   "a" 'org-edit-src-abort
      ;;   "k" 'org-edit-src-abort)
      )
    ))


;; org-capture
(defun cats-org/init-org-capture ()
  (use-package org-capture
    :defer t
    :init
    (progn
      (setq cats//org-gtd-file
        (expand-file-name
          (concat cats//org-dir cats//org-gtd-file)))

      (setq cats/org-habits-file
        (expand-file-name
          (concat cats//org-dir cats/org-habits-file)))

      (setq cats//org-calendar-file
        (expand-file-name
          (concat cats//org-dir cats//org-calendar-file)))

      (setq cats//org-capture-file
        (expand-file-name
          (concat cats//org-dir cats//org-capture-file)))

      (setq cats//org-logbook-file
        (expand-file-name
          (concat cats//org-dir cats//org-logbook-file)))

      (setq cats//org-inbox-file
        (expand-file-name
          (concat cats//org-dir cats//org-inbox-file)))

      (setq cats//org-refile-file
        (expand-file-name
          (concat cats//org-dir cats//org-refile-file)))

      (setq cats//org-journal-file
        (expand-file-name
          (concat cats//org-dir cats//org-journal-file))))
    :config
    (progn
      ;; Templates for the creation of new entries.
      (unless (boundp 'org-capture-templates)
        (defvar org-capture-templates nil))

      ;; Follow the confirm and abort conventions
      ;; (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
      ;;   dotspacemacs-major-mode-leader-key 'org-capture-finalize
      ;;   "a" 'org-capture-kill
      ;;   "c" 'org-capture-finalize
      ;;   "k" 'org-capture-kill
      ;;   "r" 'org-capture-refile)

      (add-to-list 'org-capture-templates
        `("c" "Calendar entry" entry
           ,(format "%s\n%s\n%s" "* %?" cats//org-properties-string "%^T")))

      (add-to-list 'org-capture-templates
        `("C" "Calendar entry (Linked)" entry
           (file ,cats//org-calendar-file)
           ,(format "%s%s\n%s" "* %? %A" cats//org-properties-string "%^T")))

      (add-to-list 'org-capture-templates
        `("G" "GTD Todo (Linked)" entry (file ,cats//org-gtd-file)
           (function cats//make-org-linked-todo-template)))

      (add-to-list 'org-capture-templates
        `("g" "GTD Todo" entry (file ,cats//org-gtd-file)
           (function cats//make-org-todo-template)))

      ;; (add-to-list 'org-capture-templates
      ;;   `("h" "Habit" entry
      ;;      (file ,cats/org-habits-file)
      ;;      "* TODO\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:END:"))

      (add-to-list 'org-capture-templates
        `("j" "Journal entry" entry
           (function cats//org-journal-find-location)
           "* %(format-time-string org-journal-time-format)%\n^{Title}\n%i%?"))

      (add-to-list 'org-capture-templates
        `("l" "Logbook" entry
           (file ,cats//org-logbook-file)
           "* LOGBOOK %? :LOGBOOK:\n%U"))

      ;; (add-to-list 'org-capture-templates
      ;;   `("l" "Links (it)" entry
      ;;      (file+headline ,cats//org-refile-file "Links")
      ;;      "** %c\n\n  %u\n  %i"
      ;;      :empty-lines 1))

      (add-to-list 'org-capture-templates
        `("m" "Meeting" entry
           (file ,cats//org-inbox-file)
           "* MEETING %? :MEETING:\n%U"))

      ;; (add-to-list 'org-capture-templates
      ;;   `("m" "Meeting" entry
      ;;      (file ,cats//org-refile-file)
      ;;      "* MEETING with %? :MEETING:\n%U"
      ;;      :clock-in t
      ;;      :clock-resume t))

      (add-to-list 'org-capture-templates
        `("n" "Note" entry
           (file+headline ,cats//org-inbox-file "NOTES")
           "* %? :NOTE:\n%U\n%a\n"))

      ;; (add-to-list 'org-capture-templates
      ;;   `("n" "note" entry
      ;;      (file+headline ,cats//org-refile-file "Note")
      ;;      "* NOTE %?\n%U\n\n\n%i\n\n%a"))

      (add-to-list 'org-capture-templates
        `("p" "Protocol" entry
           (file+headline ,cats//org-capture-file "Inbox")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))

      (add-to-list 'org-capture-templates
        `("P" "Protocol Link" entry
           (file+headline ,cats//org-capture-file "Inbox")
           "* %? [[%:link][%:description]]\n"))

      (add-to-list 'org-capture-templates
        `("t" "Todo" entry
           (file+headline ,cats//org-inbox-file "INBOX")
           "* TODO %?\n%U\n%a\n"))

      ;; (add-to-list 'org-capture-templates
      ;;   `("t" "todo" entry
      ;;      (file+headline ,cats//org-refile-file "Tasks")
      ;;      "* TODO %^{Task}\n%U\n\n\n%i\n\n%a\n\n%?"))

      ;; (add-to-list 'org-capture-templates
      ;;   `("w" "work todo" entry
      ;;      (file+headline "~/workorg/work.org" "Tasks")
      ;;      "* TODO %^{Task}\n%U\n\n\n%i\n%a\n\n%?"))

      )
    ))


;; org-habit
(defun cats-org/init-org-habit ()
  (use-package org-habit
    :defer t
    :init
    (progn
      ;; The absolute column at which to insert habit consistency graphs. Note
      ;; that consistency graphs will overwrite anything else in the buffer."
      (setq org-habit-graph-column 50)
      ;; If non-nil, only show habits on today's agenda, and not for future
      ;; days. Note that even when shown for future days, the graph is always
      ;; relative to the current effective date.
      (setq org-habit-show-habits-only-for-today t)
      )
    :config
    (progn
      )
    ))


;; org-mobile
(defun cats-org/init-org-mobile ()
  (use-package org-mobile
    :defer t
    :init
    (progn
      (setq org-mobile-directory
        (expand-file-name
          (concat cats//org-dir cats//org-mobile-dir)))
      (setq org-mobile-inbox-for-pull
        (expand-file-name
          (concat org-mobile-directory cats//org-mobile-inbox-file))))
    :config
    (progn
      )
    ))


;; helm-org
(defun cats-org/init-helm-org ()
  (use-package helm-org
    :defer t
    :init
    (progn
      (setq helm-org-headings-fontify t)
      (setq helm-org-headings-max-depth 10)


      )
    :config
    (progn
      )
    ))


;; ob
(defun cats-org/pre-init-ob ()
  (spacemacs|use-package-add-hook ob
    :post-init
    (progn)
    :pre-init
    (progn)))


;; ob-ditta
(defun cats-org/init-ob-ditta ()
  (use-package ob-ditta
    :defer t
    :init
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'cats-org)))
        (setq org-ditaa-jar-path (concat dir "ditta/ditaa0_9.jar")))
      )
    :config
    (progn)
    ))


;; org-brain
(defun cats-org/pre-init-org-brain ()
  (uspacemacs|use-package-add-hook org-brain
    :post-init
    ()))


;; org-expiry
(defun cats-org/pre-init-org-expiry ()
  (spacemacs|use-package-add-hook org-expiry
    ))


;; org-download
(defun cats-org/pre-init-org-download ()
  (spacemacs|use-package-add-hook org-download
    :post-init
    ()))


;; org-mime
(defun cats-org/pre-init-org-mime ()
  (spacemacs|use-package-add-hook org-mime
    :post-init
    ()))


;; autoinsert
(defun cats-org/pre-init-autoinsert ()
  (spacemacs|use-package-add-hook autoinsert
    :post-config
    (progn
      (add-to-list 'auto-insert-alist
        '("[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]$" .
           cats/journal-file-insert))
      )))


;; org-journal

(defun cats-org/pre-init-org-journal ()
  ;; *Warning:* setting `org-journal-file-format` to include a file extension
  ;; like `%Y-%m-%d.org` breaks calender search functionality. The time stamp
  ;; for the files name is YYYY-MM-DD.
  (setq-default org-journal-file-format "%Y-%m-%d")
  ;; Where journal files are stored, `~/org/journal`
  ;; per https://org-roam.discourse.group/t/org-journal-vs-org-roam-dailies/384/6 use org-journal
  ;; TODO add check that org-roam is being used first though
  ;; otherwise use (concat cats//org-dir cats//org-journal-dir)
  (setq-default org-journal-dir
    (expand-file-name
      (concat cats//org-roam-dir cats//org-journal-dir)))

  (spacemacs|use-package-add-hook org :post-config (require 'org-journal))

  (spacemacs|use-package-add-hook org-journal
    :post-init
    (progn
      ;; the time stamp for the files name is YYYY-MM-DD
      (add-to-list 'auto-mode-alist
        '("\\(?1:[0-9]\\{4\\}\\)-\\(?2:[0-9][0-9]\\)-\\(?3:[0-9][0-9]\\)\\'" . org-journal-mode))

      ;; Where journal files are stored, `~/org/journal`
      ;; per https://org-roam.discourse.group/t/org-journal-vs-org-roam-dailies/384/6 use org-journal
      ;; TODO add check that org-roam is being used first though
      ;; otherwise use (concat cats//org-dir cats//org-journal-dir)
      (setq org-journal-dir
        (expand-file-name
          (concat cats//org-roam-dir cats//org-journal-dir)))

      ;; I create a lot of TODOs to my journal entries so this will
      ;; automatically add these files to my agenda.
      (setq org-journal-enable-agenda-integration t)
      ;; *Warning:* setting `org-journal-file-format` to include a file
      ;; extension like `%Y-%m-%d.org` breaks calender search functionality.
      (setq org-journal-file-format "%Y-%m-%d.org")
      ;; carry everything over to new entry except for items marked "DONE"
      ;; (setq org-journal-carryover-items "-TODO=\"DONE\"|-TODO=\"EXPIRED\"|-TODO=\"CANCELLED\"|-TODO=\"HANDLED\"")
      (setq org-journal-carryover-items nil)
      ;; String that is put before every date at the top of a journal file. By
      ;; default, this is a org-mode heading. Another good idea would be
      ;; "#+TITLE: " for org titles.
      (setq org-journal-date-prefix "#+TITLE: ")
      ;; The function to use when opening an entry. Set this to ‘find-file‘ if
      ;; you don’t want org-journal to split your window.
      (setq org-journal-find-file 'find-file)

      ;; To use the agenda search, you can add all the calendar files to your
      ;; org-agenda by adding org-journal-dir to org-agenda-files and setting
      ;; org-agenda-file-regexp to include files with an YYYY-MM-DD name. That
      ;; way, you can use org-agenda to search for TODO items or tagged items in
      ;; your org-journal.
      ;; (cats//register-org-agenda-file org-journal-dir)
      (cats//register-org-agenda-file-regexp
        "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'")
      (cats//register-org-agenda-file-regexp
        "\\`[^.].*\\.org\\'")

      (when (configuration-layer/package-usedp 'org-journal)
        (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
          "j" nil
          "n" nil
          "p" nil))

      (spacemacs//add-org-keybindings 'org-journal-mode)
      (cats//add-org-keybindings 'org-journal-mode)
      (spacemacs/set-leader-keys-for-major-mode 'calendar-mode
        "RET" 'org-journal-display-entry)

      (spacemacs|define-transient-state org-journal
        :title "Org Journal Transient state"
        :doc "
[_n_/_p_] navigate entries   [_j_/_k_] navigate entries
[_i_] new entry              [_/_] search forever
[_q_] quit"
        :bindings
        ("q" nil :exit t)
        ("j" org-journal-open-next-entry)
        ("k" org-journal-open-previous-entry)
        ("n" org-journal-open-next-entry)
        ("p" org-journal-open-previous-entry)
        ("i" org-journal-new-entry)
        ("/" org-journal-search-forever)))
    :post-config
    (progn
      (unless (file-exists-p org-journal-dir)
        (make-directory org-journal-dir)))))

;;; packages.el ends here
