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
  '(focus-autosave-mode
    exec-path-from-shell
    ibuffer
    evil
    linum-relative
    nlinum-relative
    dash
    spaceline
    remember
    ;; Editing
    whitespace
    hungry-delete
    copyright
    beacon
    writeroom-mode
    company
    company-emoji
    ;; Tools
    flycheck
    git-commit
    magit
    fancy-battery
    list-environment
    ;; Applications
    doc-view
    paradox
    (dired :location built-in)
    sx
    dash
    (delsel :location built-in)
    ;; visual-fill-column
    copyright
    editorconfig
    ;; whitespace-cleanup-mode
    hardhat
    (tramp :location built-in)
    (grep :location built-in)
    (locate :location built-in)
    ;; helm-ag
    (calendar :location built-in)
    (time :location built-in)
    disable-mouse
    helm
    helm-projectile
    helm-ls-git
    encourage-mode
    ))

(defun cats/init-whitespace-cleanup-mode()
  "Cleanup whitespace in buffers."
  (use-package whitespace-cleanup-mode
    :ensure t
    :defer t
    :init
    (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
      (add-hook hook 'whitespace-cleanup-mode))
    :diminish (whitespace-cleanup-mode . "⌫")))

(defun cats/init-editorconfig ()
  "EditorConfig plugin for emacs."
  (use-package editorconfig
    :defer t
    :ensure t
    :init (add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-unix-mode))))

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

(defun cats/pre-init-exec-path-from-shell ()
  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-init
    (progn
      (when (string-match-p "/bash$" (getenv "SHELL"))
        ;; Use a non-interactive login shell. A login shell, because my
        ;; environment variables are mostly set in `.bashrc'.
        (setq exec-path-from-shell-arguments '("-l"))))
    :post-config
    (progn
      (dolist
          (var '(
                 "ANDROID_HOME"
                 "ANDROID_SDK_ROOT"
                 "BREW_HOME"
                 "DOCKER_CERT_PATH"
                 "DOCKER_COMPLETION_TLS"
                 "DOCKER_HOST"
                 "DOCKER_MACHINE_NAME"
                 "DOCKER_NAMESPACE"
                 "DOCKER_PREFIX"
                 "DOCKER_REGISTRY"
                 "DOCKER_TLS_VERIFY"
                 "EMAIL"
                 "GITHUB_TOKEN"
                 "GITLAB_PRIVATE_TOKEN"
                 "GOPATH"
                 "GOROOT"
                 "HOME"
                 "HOMEBREW_GITHUB_API_TOKEN"
                 "HTML_TIDY"
                 "INFOPATH"
                 "IRC_CLIENT"
                 "JAVA_OPTS"
                 "KUBECONFIG"
                 "MANPATH"
                 "MINIKUBE_HOME"
                 "NODE_REPL_HISTORY_FILE"
                 "NODE_REPL_MODE"
                 "NVM_BIN"
                 "NVM_DIR"
                 "NVM_PATH"
                 "NVM_TARGET"
                 "PACKER_CACHE_DIR"
                 "PYENV_HOME"
                 "PYENV_ROOT"
                 "PYENV_SHELL"
                 "PYTHONPATH"
                 "RBENV_HOME"
                 "RBENV_ROOT"
                 "RBENV_SHELL"
                 "SBT_OPTS"
                 "VAGRANT_CHECKPOINT_DISABLE"
                 "VAGRANT_DOTFILE_PATH"
                 "VAGRANT_HOME"
                 "VAGRANT_VMWARE_CLONE_DIRECTORY"
                 "XML_CATALOG_FILES"
                 ))
        (add-to-list 'exec-path-from-shell-variables var))

      (exec-path-from-shell-initialize)

      ;; TODO: get vars from system or env vars, below are two ways with osx:
      ;; finger `whoami` | awk -F: '{ print $3 }' | head -n1 | sed 's/^ //'
      ;; dscl . read /Users/`whoami` RealName | grep -v RealName | cut -c 2-
      (setq user-full-name "Andrew Cates")
      (setq user-mail-address (getenv "EMAIL"))
      (setq user-mail-address "andrew@cates.io")

      ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
      ;; already initializes info, we need to explicitly add the $INFOPATH
      ;; directories to `Info-directory-list'.  We reverse the list of info paths
      ;; to prepend them in proper order subsequently
      (with-eval-after-load 'info
        (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
          (when dir
            (add-to-list 'Info-directory-list dir)))))))

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

(defun cats/pre-init-company ()
  (spacemacs|use-package-add-hook company
    :post-config
    ;; Auto-complete less aggressively
    (setq company-idle-delay 0.5)))

(defun cats/post-init-company ()
  ;; Enable auto-completion everywhere!
  (global-company-mode))

(defun cats/post-init-company-emoji ()
  ;; Enable Company Emoji everywhere
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-emoji))
  ;; Re-enable unicode emoji.  It's 2015, dammit
  (setq company-emoji-insert-unicode t))

;; Editing
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

(defun cats/pre-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    ;; Enable Flycheck everywhere
    :post-init
    (progn
      (add-hook 'cats/tidy-executable-hook
         'cats//set-tidy-executable)
      (setq flycheck-global-modes t))))

(defun cats/post-init-git-commit ()
  ;; Support Git Commit Mode for external `git commit'
  (global-git-commit-mode))

(defun cats/pre-init-magit ()
  ;; Please, no gravatars.  Thanks
  (spacemacs|use-package-add-hook magit
    :post-config
    (setq magit-revision-show-gravatars nil)))

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
(defun cats/init-grep ()
  (use-package grep
    :defer t
    :init
    (progn
      (add-hook 'cats/find-executable-hook 'cats//grep-set-find-executable))
    :config
    (progn
      (define-key grep-mode-map "q" 'rgrep-quit-window)
      (define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)
      (grep-compute-defaults)
      (when-let* ((gnu-xargs (and (eq system-type 'darwin)
                                  (executable-find "gxargs"))))
        (setq xargs-program gnu-xargs)))))


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
(defun cats//on-helm-projectile-project-hook (&optional dir)
  (if (empty-string-p dir)
      (helm-projectile-off)
    (helm-projectile-on)))

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
    :ensure t
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
      (spacemacs/toggle-encourage-mode-on))
    :config (spacemacs|hide-lighter encourage-mode)))

;;; packages.el ends here