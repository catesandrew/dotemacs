;;; packages.el --- cats: Packages

;;; Commentary:

;; Personal packages.
;;
;; This is a catch-all layer holding all packages that fit no more specific
;; layer.

;;; Code:

(require 'subr-x)

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-packages
  '(beacon
     (calendar :location built-in)
     company
     company-emoji
     (conf-mode :location built-in)
     copyright
     dash
     (delsel :location built-in)
     (dired :location built-in)
     disable-mouse
     doc-view
     editorconfig
     encourage-mode
     engine-mode
     evil
     exec-path-from-shell
     fancy-battery
     flycheck
     focus-autosave-mode
     git-commit
     gh
     (grep :location built-in)
     hardhat
     helm
     helm-ls-git
     helm-open-github
     helm-projectile
     hungry-delete
     ibuffer
     linum-relative
     list-environment
     (locate :location built-in)
     magit
     (magit-repos :location built-in)
     nlinum-relative
     paradox
     remember
     spaceline
     sx
     (time :location built-in)
     (tramp :location built-in)
     which-key
     whitespace
     writeroom-mode
     ))


;; conf-mode
(defun cats/post-init-conf-mode ()
  (add-to-list 'auto-mode-alist '("\\.env$" . conf-mode)))


;; which-key
(defun cats/post-init-which-key ()
  "Replace rules for better naming of functions."
  (let ((new-descriptions
          '(("cats/\\(.+\\)" . "\\1"))))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
        which-key-replacement-alist)))

  (spacemacs/set-leader-keys
    "ohb" 'which-key-show-keymap
    "ohk" 'which-key-show-full-keymap
    "ohM" 'which-key-show-major-mode
    "ohm" 'which-key-show-minor-mode-keymap))


;; helm open github
(defun cats/init-helm-open-github ()
  ""
  (use-package helm-open-github
    :commands (helm-open-github-from-commit helm-open-github-from-file helm-open-github-from-issues helm-open-github-from-pull-requests)
    :ensure t
    :init
    (progn
      (spacemacs/declare-prefix "go" "github open")
      (spacemacs/declare-prefix "gof" "github from file")
      (spacemacs/declare-prefix "goc" "open from commit")
      (spacemacs/declare-prefix "goi" "open from issues")
      (spacemacs/declare-prefix "gop" "open from pull requests")
      (spacemacs/set-leader-keys
        "gof" 'helm-open-github-from-file
        "goc" 'helm-open-github-from-commit
        "goi" 'helm-open-github-from-issues
        "gop" 'helm-open-github-from-pull-requests))))


;; search-engine
(defun cats/pre-init-engine-mode ()
  (spacemacs|use-package-add-hook engine-mode
    :post-init
    (progn
      (setq browse-url-browser-function 'browse-url-generic)
      (setq engine/browser-function 'browse-url-generic)
      (setq browse-url-generic-program (cond
                                        ((spacemacs/system-is-mac)
                                         "open")
                                        ((spacemacs/system-is-linux)
                                         (executable-find "firefox"))))
      (setq search-engine-alist
            '((amazon
               :name "Amazon"
               :keybinding "a"
               :browser 'browse-url-generic
               :url "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s")
              (bing
               :browser 'eww-browse-url
               :keybinding "b"
               :name "Bing"
               :url "http://www.bing.com/search?q=%s")
              (duck-duck-go
               :name "Duck Duck Go"
               :keybinding "d"
               :browser 'browse-url-generic
               :url "https://duckduckgo.com/?q=%s")
              (google
               :keybinding "g"
               :browser 'browse-url-generic
               :name "Google"
               :url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
              (google-images
               :name "Google Images"
               :keybinding "i"
               :browser 'browse-url-generic
               :url "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
              (github
               :browser 'eww-browse-url
               :keybinding "G"
               :name "Github"
               :url "https://github.com/search?ref=simplesearch&q=%s")
              (google-maps
               :keybinding "m"
               :browser 'browse-url-generic
               :name "Google Maps"
               :url "http://maps.google.com/maps?q=%s")
              (twitter
               :name "Twitter"
               :keybinding "t"
               :browser 'browse-url-generic
               :url "https://twitter.com/search?q=%s")
              (project-gutenberg
               :name "Project Gutenberg"
               :keybinding "p"
               :browser 'browse-url-generic
               :url "http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")
              (youtube
               :keybinding "y"
               :browser 'browse-url-generic
               :name "YouTube"
               :url "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
              (stack-overflow
               :name "Stack Overflow"
               :keybinding "o"
               :browser 'browse-url-generic
               :url "https://stackoverflow.com/search?q=%s")
              (tex-stack-exchange
               :name "TeX Stack Exchange"
               :browser 'browse-url-generic
               :keybinding "x"
               :url "https://tex.stackexchange.com/search?q=%s")
              (wikipedia
               :browser 'eww-browse-url
               :keybinding "w"
               :name "Wikipedia"
               :url "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
              (wolfram-alpha
               :name "Wolfram Alpha"
               :browser 'browse-url-generic
               :url "http://www.wolframalpha.com/input/?i=%s")))
      (dolist (engine search-engine-alist)
        (let ((func (intern (format "engine/search-%S" (car engine)))))
          (autoload func "engine-mode" nil 'interactive))))
    :post-config
    (progn
      (engine/set-keymap-prefix (kbd "C-c /"))
      (dolist (engine search-engine-alist)
        (let* ((cur-engine (car engine))
               (engine-url (plist-get (cdr engine) :url))
               (engine-keybinding (plist-get (cdr engine) :keybinding))
               (engine-browser (plist-get (cdr engine) :browser)))
          (eval `(defengine ,cur-engine ,engine-url :keybinding ,engine-keybinding :browser ,engine-browser))))
      )))


;; editorconfig
(defun cats/pre-init-editorconfig ()
  "EditorConfig plugin for emacs."
  (spacemacs|use-package-add-hook editorconfig
    :post-init (add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-unix-mode))))

(defun cats/init-copyright ()
  "Deal with copyright notices."
  (use-package copyright
    ;; Update copyright when visiting files
    :init (add-hook 'find-file-hook 'copyright-update)
    ;; Use ranges to denote consecutive years
    :config (setq copyright-year-ranges t
                  copyright-names-regexp (regexp-quote user-full-name))))

(defun cats/init-visual-fill-column ()
  "Emacs mode for wrapping function `visual-line-mode' buffers at `fill-column'."
  (use-package visual-fill-column
    :ensure t
    :defer t
    :init
    (progn
      (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
      (if (configuration-layer/package-usedp 'zoom-frm)
          (with-eval-after-load 'zoom-frm
            (advice-add 'zoom-in/out :after
                        #'visual-fill-column-adjust))
        (advice-add 'text-scale-adjust :after
                    #'visual-fill-column-adjust)))))

(defun cats/init-delsel ()
  "Delete the selection instead of insert."
  (use-package delsel
    :defer t
    :init (delete-selection-mode)))

(defun cats/init-dash ()
  (use-package dash
    :defer t))

(defun cats/pre-init-ibuffer ()
  (spacemacs|use-package-add-hook ibuffer
    :pre-init
    (progn
      )
    :pre-config
    (progn
      ;; Since we could override `,` with <leader>, let's make `;` do that
      ;; functionality
      (when (equal dotspacemacs-leader-key ",")
        (define-key ibuffer-mode-map
          (kbd ";") 'ibuffer-toggle-sorting-mode)
        (define-key ibuffer-mode-map
          (kbd ",") nil)))))

(defun cats/pre-init-linum-relative ()
  (spacemacs|use-package-add-hook linum-relative
    :pre-init
    (progn
      )
    :post-config
    (progn
      (setq linum-relative-current-symbol "")
      )))

(defun cats/pre-init-nlinum-relative ()
  (spacemacs|use-package-add-hook nlinum-relative
    :post-config
    (progn
      (setq nlinum-relative-current-symbol ""))))

(defun cats/post-init-evil ()
  ;; this binding change can cause brittle behavior
  ;; (define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)

  ;; c-k/c-j for page down/up
  ;;
  ;; One thing that surprised me considering how complete Evil is, is the lack
  ;; of Vim's Control-d/Control-u for page down/up. Probably because C-u is
  ;; pretty important in Emacs (it's the shortcut to give a numeric parameter
  ;; to other commands). I've in fact these mapped on my .vimrc to c-k/c-j
  ;; (because I think they're more consistent with Vim's j/k movement keys) so
  ;; that's how I mapped them in Emacs:
  (define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-up)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-down)

  ;; It's better that the default value is too small than too big
  (setq-default evil-shift-width 2))


;; exec-path-from-shell
(defun cats/pre-init-exec-path-from-shell ()
  (setq-default exec-path-from-shell-check-startup-files nil)
  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-config
    (progn
      ;; (setq exec-path-from-shell-check-startup-files nil)
      (dolist
        (var '(
                "ANDROID_HOME"
                "ANDROID_SDK_ROOT"
                "BREW_HOME"
                "EMAIL"
                "GITHUB_TOKEN"
                "GITLAB_PRIVATE_TOKEN"
                "HOME"
                "HOMEBREW_GITHUB_API_TOKEN"
                "HTML_TIDY"
                "IRC_CLIENT"
                "MANPATH"
                "SBT_OPTS"
                "XML_CATALOG_FILES"
                ) exec-path-from-shell-variables)
        (unless (or (member var exec-path-from-shell-variables) (getenv var))
          (push var exec-path-from-shell-variables)))

      ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
      ;; already initializes info, we need to explicitly add the $INFOPATH
      ;; directories to `Info-directory-list'.  We reverse the list of info paths
      ;; to prepend them in proper order subsequently
      (with-eval-after-load 'info
        (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
          (when dir
            (add-to-list 'Info-directory-list dir)))))
    :post-config
    (exec-path-from-shell-initialize)))

(defun cats/post-init-exec-path-from-shell ()
  (cats//locate-email)
  (cats//locate-name))

(defun cats/init-focus-autosave-mode ()
  (use-package focus-autosave-mode
    :init (focus-autosave-mode)
    :config (spacemacs|hide-lighter focus-autosave-mode)))

(defun cats/pre-init-spaceline ()
  (spacemacs|use-package-add-hook spaceline-config
    :post-config
    ;; Always show default input method
    (setq spaceline-show-default-input-method t)
    (spacemacs/toggle-mode-line-battery-off)
    (spacemacs/toggle-mode-line-minor-modes-off)

    ;; 1 set srgb to nil to prevent emacs crashing after setting 2 and 3
    (setq ns-use-srgb-colorspace nil)
    ;; 2 use utf-8 separators because they look best
    ;; (setq powerline-default-separator 'utf-8)
    ))

(defun cats/init-remember ()
  (use-package remember
    ;; Persistent scratch buffer. Still disabled because this configuration
    ;; doesn't yet override *scratch* properly. Need to investigate
    :disabled t
    :init
    (setq initial-buffer-choice 'remember-notes
          remember-notes-buffer-name "*scratch*")
    :config
    (setq remember-data-file (expand-file-name "~/ownCloud/notes.md")
          remember-notes-initial-major-mode
          (if (configuration-layer/package-usedp 'markdown-mode)
              'markdown-mode 'text-mode))))


;; company
(defun cats/pre-init-company ()
  (spacemacs|use-package-add-hook company
    :post-config
    ;; Auto-complete less aggressively
    (setq company-idle-delay 0.5)))

(defun cats/post-init-company ()
  ;; Enable auto-completion everywhere!
  (global-company-mode))


;; company-emoji
(defun cats/post-init-company-emoji ()
  ;; Enable Company Emoji everywhere
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-emoji))
  ;; Re-enable unicode emoji.  It's 2015, dammit
  (setq company-emoji-insert-unicode t))


;; editing
(defun cats/whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(defun cats/pre-init-whitespace ()
  (spacemacs|use-package-add-hook whitespace
    :post-config
    (progn
      ;; Cleanup all whitespace
      (spacemacs/set-leader-keys "xdw" #'whitespace-cleanup)

      ;; Use less aggressive whitespace highlighting, and disable Spacemacs own
      ;; whitespace highlighting
      (setq spacemacs-show-trailing-whitespace nil
            whitespace-style '(face indentation space-after-tab space-before-tab
                                    tab-mark empty trailing lines-tail)
            whitespace-line-column nil))))

(defun cats/post-init-whitespace ()
  ;; Enable whitespace mode after local variables were setup because whitespace
  ;; mode doesn't handle local variables well :(
  (spacemacs/add-to-hooks #'cats/whitespace-mode-local
                          '(prog-mode-hook text-mode-hook conf-mode-hook)))

(defun cats/post-init-hungry-delete ()
  (global-hungry-delete-mode))

(defun cats/init-copyright ()
  (use-package copyright
    :defer t
    :init
    (progn
      (defun cats/copyright-update ()
        (interactive)
        (unless buffer-read-only
          (copyright-update nil 'interactive)
          (unless copyright-update
            ;; Fix years when the copyright was updated
            (copyright-fix-years))))
      (add-hook 'find-file-hook #'cats/copyright-update))
    :config (setq copyright-year-ranges t)))

(defun cats/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "tob")

      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)))

(defun cats/init-writeroom-mode ()
  (use-package writeroom-mode
    :init (spacemacs|add-toggle writeroom
            :status writeroom-mode
            :on (writeroom-mode)
            :off (writeroom-mode -1)
            :documentation "Enable distraction-free editing"
            :evil-leader "tow")))


;; flycheck
(defun cats/pre-init-flycheck ()
  "Pre init for flycheck."
  (spacemacs|use-package-add-hook flycheck
    :post-init
    (progn
      (defadvice flycheck-mode (around flycheck-turn-on-maybe activate)
        (unless
            (or
             buffer-read-only
             (hardhat-buffer-included-p (current-buffer))
             (cats//current-buffer-remote-p))
          ad-do-it))
      ;; (ad-activate 'flycheck-mode)

      (add-hook 'cats/tidy-executable-hook
         'cats//set-tidy-executable)
      (setq flycheck-global-modes t))))

(defun cats/post-init-git-commit ()
  ;; Support Git Commit Mode for external `git commit'
  (global-git-commit-mode))


;; gh
(defun cats/init-gh ()
  (use-package gh
    :ensure t
    :defer t
    :init (progn)
    :config (progn)))


;; magit-repos
(defun cats/init-magit-repos ()
  (use-package magit-repos
    :commands (magit-repolist-mode)
    :init
    (progn
      (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident
            ())
           ("Version" 25 magit-repolist-column-version
             ())
           ("D"        1 magit-repolist-column-dirty
             ())
           ("L<U"      3 magit-repolist-column-unpulled-from-upstream
             ((:right-align t)
               (:help-echo "Upstream changes not in branch")))
           ("L>U"      3 magit-repolist-column-unpushed-to-upstream
             ((:right-align t)
               (:help-echo "Local changes not in upstream")))
           ("Path"    99 magit-repolist-column-path
             ())))

      (spacemacs|hide-lighter magit-repolist-mode)
      (dolist (mode '(magit-repolist-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "f" 'cats/repolist-fetch
          "F" 'cats/repolist-fetch-async
          "p" 'cats/open-in-projectile
          "l" 'cats/open-in-git-link
          "r" 'cats/repolist-pull-ff-only
          "c" 'cats/magit-repolist-call-command)))
    :config
    (progn
      (require 'magit-remote)
      (evilified-state-evilify-map magit-repolist-mode-map
        :mode magit-repolist-mode
        :bindings
        (kbd "q") 'quit-window
        (kbd "RET") 'magit-repolist-status))))


;; magit
(defun cats//toggle-gh-profile (dir frame-name)
  (when (string= frame-name (cats//get-frame-name nil))
    (require 'gh-profile)
    (require 'git-link)
    (let* ((remote (git-link--select-remote))
            (id (gh-profile-get-remote-profile (git-link--remote-url remote))))
      (when id
        ;; (set (make-local-variable 'js2-mode-show-parse-errors) t)
        (setq gh-profile-default-profile id)))))

(defun cats/pre-init-magit ()
  "Please, no gravatars. Thanks"
  (spacemacs|use-package-add-hook magit
    :post-init
    (progn
      (add-hook 'cats/project-hook 'cats//toggle-gh-profile)

      (setq magit-revision-show-gravatars nil)
      ;; For annotated tags prepare message with commit messages since last tag.
      (add-hook 'git-commit-mode-hook
         (lambda()
           (when (equal "TAG_EDITMSG" (buffer-name))
             (progn
               (insert (shell-command-to-string "git log --pretty=format:\"* %s\" `git rev-list --tags --max-count=1`..HEAD" ))
               (newline)
               (goto-char (point-min))
               (newline)
               (goto-char (point-min)))))))))


;; fancy-battery
(defun cats/post-init-fancy-battery ()
  (spacemacs/toggle-mode-line-battery-on))

(defun cats/init-list-environment ()
  (use-package list-environment
    :init (spacemacs/set-leader-keys "oE" 'list-environment)
    :config (evil-set-initial-state 'list-environment-mode 'emacs)))

(defun cats/pre-init-doc-view ()
  (spacemacs|use-package-add-hook doc-view
    :post-config
    (progn
      ;; Render PDFs at 300dpi
      (setq doc-view-resolution 300)

      (defun cats/doc-view-mutool-draw-1 (pdf png page callback
                                                   &optional resolution)
        (doc-view-start-process
         "pdf->png" "mutool"
         `("draw"
           ,(concat "-o" png)
           ,(format "-r%d" (round (or resolution doc-view-resolution)))
           ,pdf
           ,@(if page `(,(format "%d" page))))
         callback))

      (if (fboundp 'doc-view-multiplex-conversion)
          ;; Emacs MAC port multiplexes for high-res support
          (defun cats/doc-view-mutool-draw (pdf png page callback)
            (doc-view-multiplex-conversion
             #'cats/doc-view-mutool-draw-1 pdf png page callback))
        ;; On standard Emacs, just use the normal conversion process
        (defalias 'cats/doc-view-mutool-draw
          'cats/doc-view-mutool-draw-1))

      ;; Warn if Doc View falls back to Ghostscript for rendering
      (if (executable-find "mutool")
          (setq doc-view-pdf->png-converter-function
                #'cats/doc-view-mutool-draw)
        (unless (eq doc-view-pdf->png-converter-function
                    'doc-view-pdf->png-converter-mupdf)
          (warn "Doc View is not using mupdf.
Install mudraw with brew install mupdf-tools"))))))

(defun cats/post-init-paradox ()
  "Make the spinner fancy and don't star packages automatically."
  (setq paradox-spinner-type 'moon
        paradox-automatically-star nil))

(defun cats/pre-init-dired ()
  "Dired configuration."
  (spacemacs|use-package-add-hook dired
    :pre-config
    (progn
      (setq dired-auto-revert-buffer t    ; Revert on re-visiting
            ;; Inhibit prompts for simple recursive operations
            dired-recursive-copies 'always
            ;; Auto-copy to other Dired split window
            dired-dwim-target t))))

(defun cats/init-sx ()
  (use-package sx
    :defer t
    :init
    (spacemacs/declare-prefix "os" "sx")
    (spacemacs/declare-prefix "ost" "sx-tabs")
    (spacemacs/set-leader-keys
      "osa"  'sx-ask
      "oss"  'sx-search
      "osta" 'sx-tab-all-questions
      "ostt" 'sx-tab-unanswered-my-tags
      "ostu" 'sx-tab-unanswered
      "osts" 'sx-tab-starred)
    :config
    (spacemacs/add-to-hook 'sx-compose-mode-hook
                           '(visual-line-mode turn-off-auto-fill))

    (defun cats/sx-cleanup-whitespace-before-send ()
      "Cleanup whitespace before sending a question."
      (whitespace-cleanup)
      t)

    (add-hook 'sx-compose-mode-before-send-hook
              'cats/sx-cleanup-whitespace-before-send)))


;; hardhat
(defun cats/init-hardhat ()
  "Protect user-writable files."
  (use-package hardhat
    :ensure t
    :commands (hardhat-buffer-included-p)
    :init
    (progn
      ;; (global-hardhat-mode)
      (setq hardhat-buffer-protected-functions '(hardhat-protected-by-ignoramus)))
    :config (setq hardhat-mode-lighter "🔒")))


;; tramp
(defun cats/init-tramp ()
  "Access remote files."
  (use-package tramp
    :defer t
    :init
    (progn
      (setq tramp-ssh-controlmaster-options
            (concat
             "-o ControlPath=~/.ssh/conn-%%r@%%h:%%p"))
      (setq tramp-default-method "ssh"
            vc-ignore-dir-regexp
            (format "\\(%s\\)\\|\\(%s\\)"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp)))
    :config
    ;; Store auto-save files locally
    (setq tramp-auto-save-directory (concat spacemacs-cache-directory "tramp-auto-save"))))



;; grep
(defun cats/post-init-grep ()
  (add-hook 'cats/find-executable-hook 'cats//grep-set-find-executable))

(defun cats/pre-init-grep ()
  (spacemacs|use-package-add-hook grep
    :post-config
    (define-key grep-mode-map "q" 'rgrep-quit-window)
    (define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)
    (grep-compute-defaults)
    (when-let* ((gnu-xargs (and (eq system-type 'darwin)
                                (executable-find "gxargs"))))
      (setq xargs-program gnu-xargs))))


;; locate
(defun cats/init-locate ()
  "Search files on the system."
  (use-package locate
    :defer t
    :config
    ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
    (when-let* ((mdfind (and (eq system-type 'darwin) (executable-find "mdfind"))))
      (setq locate-command mdfind))))


;; helm-ag

;; Use `grep-find-ignored-files' and `grep-find-ignored-directories' as
;; ignore pattern, but does not seem to be working, need to confirm
;; (setq helm-ag-use-grep-ignore-list nil)
;; example: (helm-ag-ignore-patterns '("*.md" "*.el"))
;; (setq helm-ag-ignore-patterns '(append grep-find-ignored-files
;;                                        grep-find-ignored-directories))


;; calendar
(defun cats/init-calendar ()
  "Built-in calendar."
  (use-package calendar
    :defer t
    :init
    ;; I start on Monday
    (setq calendar-week-start-day 1)))


;; time
(defun cats/init-time ()
  "Show current time."
  (use-package time
    :defer t
    :init
    (setq display-time-world-time-format "%H:%M %Z, %d. %b"
          display-time-world-list
          '(("Europe/Berlin"    "Berlin")
            ("Europe/London"    "London")
            ("Europe/Istanbul"  "Istanbul")
            ("America/Winnipeg" "Winnipeg (CA)")
            ("America/New_York" "New York (USA)")
            ("Asia/Tokyo"       "Tokyo (JP)")))))


;; disable-mouse
(defun cats/init-disable-mouse ()
  "http://endlessparentheses.com/disable-mouse-only-inside-emacs.html"
  (use-package disable-mouse
    :defer t
    :if (eq system-type 'darwin)
    :ensure t
    :init
    (progn
      ;; https://xivilization.net/~marek/blog/2015/06/22/disabling-mouse-in-spacemacs/
      ;; Every time I switch focus to Emacs by clicking in a random place on the
      ;; window, the cursor moves to this place. Incredibly inconvenient, since I
      ;; don’t want to move my cursor accidentally. Since I never use the mouse
      ;; for anything in my editor anyway, I decided to disable it. Turn’s out, it
      ;; is not that easy, since it is not a global key binding, but one that is
      ;; local to the Evil mode. Frustrating to figure out.
      (defun dotemacs/silence ()
        (interactive))

      (with-eval-after-load 'evil
        ;; don't jump the cursor around in the window on clicking
        (define-key evil-motion-state-map [down-mouse-1] 'dotemacs/silence)
        (define-key evil-normal-state-map [down-mouse-1] 'dotemacs/silence)
        (define-key evil-visual-state-map [down-mouse-1] 'dotemacs/silence)
        ;; also avoid any '<mouse-1> is undefined' when setting to 'undefined
        (define-key evil-motion-state-map [mouse-1] 'dotemacs/silence)
        (define-key evil-normal-state-map [mouse-1] 'dotemacs/silence)
        (define-key evil-visual-state-map [mouse-1] 'dotemacs/silence))

      (spacemacs|add-toggle disable-mouse-mode
        :status disable-mouse-mode
        :on (progn
              (when (bound-and-true-p disable-mouse-mode)
                (disable-mouse-mode -1))
              (disable-mouse-mode))
        :off (disable-mouse-mode -1)
        :documentation "Disable mouse."
        :evil-leader "tom")
      (spacemacs|add-toggle global-disable-mouse-mode
        :status global-disable-mouse-mode
        :on (progn
              (when (bound-and-true-p global-disable-mouse-mode)
                (global-disable-mouse-mode -1))
              (global-disable-mouse-mode))
        :off (global-disable-mouse-mode -1)
        :documentation "Disable mouse globally."
        :evil-leader "to C-m")
      (spacemacs/toggle-global-disable-mouse-mode-on))
    :config (spacemacs|hide-lighter disable-mouse-mode)))


;; helm
(defun cats/pre-init-helm ()
  "Helm: Unite/CtrlP style fuzzy file/buffer/anything searcher on steroids.

Helm does the same thing as Unite/CtrlP on Vim and does it really
well. You can also enable Helm to manage the command buffer,
which is pretty awesome with: (helm-mode 1)"
  (spacemacs|use-package-add-hook helm
    :post-init
    (progn
      (with-eval-after-load 'helm-config
        (warn "`helm-config' loaded! Get rid of it ASAP!"))

      ;; NOTE: Apple OS X users also need a version of grep that accepts --exclude-dir
      ;; brew tap homebrew/dupes
      ;; brew install homebrew/dupes/grep
      (when-let* ((gnu-grep (and (eq system-type 'darwin)
                                 (executable-find "ggrep"))))
        (setq helm-grep-default gnu-grep)
        (setq helm-grep-default-command (concat gnu-grep " --color=never -a -d skip %e -n%cH -e %p %f"))
        (setq helm-grep-default-recurse-command (concat gnu-grep " --color=never -a -d recurse %e -n%cH -e %p %f")))

      (setq helm-split-window-inside-p t))
    :post-config
    (progn
      ;; Use recentf to find recent files
      (setq helm-ff-file-name-history-use-recentf t)
      ;; Find library from `require', `declare-function' and friends
      (setq helm-ff-search-library-in-sexp t))))


;; helm-projectile
(defun cats//on-helm-projectile-project-hook (dir frame-name)
  ;; (princ (format "frame-name: `%s''\n" frame-name))
  (when (string= frame-name (cats//get-frame-name nil))
    (if (empty-string-p dir)
      (helm-projectile-off)
      (helm-projectile-on))))

(defun cats/pre-init-helm-projectile ()
  (spacemacs|use-package-add-hook helm-projectile
    :post-init
    (progn
      (add-hook 'cats/project-hook 'cats//on-helm-projectile-project-hook)

      ;; https://github.com/syl20bnr/spacemacs/issues/1544
      ;; Vim users are used to CtrlP plugin.
      (setq helm-for-files-preferred-list '(helm-source-buffers-list
                                            helm-source-buffer-not-found
                                            helm-source-ls-git
                                            helm-source-ls-git-buffers
                                            helm-source-projectile-projects
                                            helm-source-projectile-files-list
                                            helm-source-recentf
                                            helm-source-bookmarks
                                            helm-source-file-cache
                                            helm-source-files-in-current-dir))

      ;; evil-mode (normal/visual) binds `C-p' to `evil-paste-pop' also `M-y'.
      ;; evil-mode (insert) binds `C-p' to `evil-complete-previous'.
      ;; (define-key evil-normal-state-map (kbd "C-p") 'helm-git-ls-files)

      ;; overwrite projectile settings
      (spacemacs|use-package-add-hook projectile
        :post-init
        (progn
          (setq projectile-switch-project-action 'helm-projectile)
          (spacemacs/set-leader-keys
            "pe"  'helm-projectile-git-ls-files))))))


;; helm-ls-git
(defun cats/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :ensure t
    :commands (helm-ls-git-not-inside-git-repo helm-ls-git-ls helm-browse-project)
    :init
    (progn
      (setq helm-ls-git-show-abs-or-relative 'relative))))


;; encourage-mode
(defun cats/init-encourage-mode ()
  (use-package encourage-mode
    :defer t
    :commands encourage-mode
    :init
    (progn
      (spacemacs|add-toggle encourage-mode
        :status encourage-mode
        :on (progn
              (when (bound-and-true-p encourage-mode)
                (encourage-mode -1))
              (encourage-mode))
        :off (encourage-mode -1)
        :documentation "Disable encourage mode."
        :evil-leader "toe")
      (spacemacs/toggle-encourage-mode-off)
      (spacemacs/toggle-encourage-mode-on))
    :config (spacemacs|hide-lighter encourage-mode)))

;;; packages.el ends here
