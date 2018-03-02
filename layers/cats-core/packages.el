;;; packages.el --- cats: Core

;;; Commentary:

;;; Code:

(require 'subr-x)

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-core-packages
  '(projectile
    autorevert
    company
    company-shell
    yasnippet
    buffer-move
    desktop
    (prettify-symbols-mode :location built-in)
    (icicles :location local)
    (eshell :location built-in)
    (shell :location built-in)
    (term :location built-in)
    xterm-color
    (tramp :location built-in)
    ))


;; company
(defun cats-core/post-init-company ()
  ;; Enable auto-completion everywhere!
  (spacemacs|add-company-backends
   :backends company-capf
   :modes shell-mode))

(defun cats-core/post-init-company-shell ()
  ;; Enable auto-completion everywhere!
  (spacemacs|add-company-backends
   :backends (company-shell company-shell-env)
   :modes shell-mode))


;; yasnippet
(defun cats-core/post-init-yasnippet ()
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(eshell-mode-hook))
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(shell-mode-hook)))


;; icicles
(defun cats-core/init-icicles ()
  (use-package icicles
    :commands (icicle-complete-keys icy-mode icicle-auto-complete-keys-mode)
    :defer t
    :init
    (progn
      (spacemacs|add-toggle icy-mode
        :status icy-mode
        :on (progn
              (when (bound-and-true-p icy-mode)
                (icy-mode -1))
              (icy-mode))
        :off (icy-mode -1)
        :documentation "Icicles"
        :evil-leader "toI")

      (spacemacs|add-toggle icicle-auto-complete-keys-mode
        :status icicle-auto-complete-keys-mode
        :on (progn
              (when (bound-and-true-p icicle-auto-complete-keys-mode)
                (icicle-auto-complete-keys-mode -1))
              (icicle-auto-complete-keys-mode))
        :off (icicle-auto-complete-keys-mode -1)
        :documentation "Describe keys"
        :evil-leader "tod"))
    :config
    (progn
      (spacemacs|hide-lighter icicle-auto-complete-keys-mode)
      (spacemacs|hide-lighter icy-mode))))


;; autorevert
(defun cats-core/init-autorevert ()
  "Auto refresh, auto-revert buffers of changed files."
  (use-package autorevert
    :init
    (progn
      (setq auto-revert-check-vc-info nil
            auto-revert-mode-text " ♻"
            auto-revert-tail-mode-text " ♻~")

      (defadvice auto-revert-mode (around auto-revert-turn-on-maybe)
        (unless
            (or
             buffer-read-only
             (hardhat-buffer-included-p (current-buffer))
             (cats//current-buffer-remote-p))
          ad-do-it))
      (ad-activate 'auto-revert-mode))))


;; buffer-move
(defun cats-core/init-buffer-move ()
  "Init buffer move."
  (use-package buffer-move
    :defer t))


;; projectile
(defun cats-core/pre-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :pre-config
    (progn
      (projectile-register-project-type 'npm '("package.json")
        :compile "npm build"
        :test "npm test"
        :run "npm start"
        :test-suffix ".spec"))
    :post-init
    (progn
      (when cats/projectile-enable-caching
        (setq projectile-enable-caching t))

      (when cats/projectile-require-project-root
        (setq projectile-require-project-root t))

      (defun cats//dabbrev-from-projectile (&optional dir)
        "Use ."
        (when cats/verbose
          (message "!!! Running cats//dabbrev-from-projectile: %s" (file-name-nondirectory (directory-file-name (or dir (projectile-project-root))))))
        (add-to-list 'directory-abbrev-alist
                     (cons
                      (concat "^" (directory-file-name (or dir (projectile-project-root))))
                      (file-name-nondirectory (directory-file-name (or dir (projectile-project-root)))))))
      ;; (add-hook 'cats/project-hook 'cats//dabbrev-from-projectile)

      (defun cats/find-file-hook-to-project ()
        "Use ."
        (when cats/verbose
          (message "!!! Running cats//find-file-hook-to-project"))
        (defun cats/do-nothing ())

        (condition-case err
            (let ((projectile-require-project-root t))
              (projectile-project-root)
              (let ((project-root (projectile-project-root))
                    (proj-dir-root (directory-file-name (projectile-project-root)))
                    (proj-dir-base (file-name-nondirectory (directory-file-name (projectile-project-root)))))
                (when (and project-root
                           (not (string= project-root cats//projectile-curr)))
                  (let ((projectile-switch-project-action 'cats/do-nothing))
                    (when cats/verbose
                      (message "Project Root %s" project-root)
                      (message "Project Base %s" proj-dir-base)
                      (message "Project Dir %s" proj-dir-root)
                      (message "Buffer Name %s" buffer-file-name))
                    (projectile-switch-project-by-name project-root)
                    (setq cats/projectile-dir-root proj-dir-root)
                    (setq cats/projectile-dir-base proj-dir-base)
                    (cats/run-project-hook project-root)))))
          (error
           (progn
             (setq cats/projectile-dir-root nil)
             (setq cats/projectile-dir-base nil)
             (cats/run-project-hook nil))
           nil))
        )

      ;; does not attach to any c-level calls but is best we got
      (defadvice switch-to-buffer (after cats/projectile-switch activate)
        (when buffer-file-name
          (when cats/verbose
            (message "!!! Switch to buffer: %s" buffer-file-name))
          (cats/find-file-hook-to-project)))

      (defadvice switch-to-prev-buffer (after cats/projectile-switch-prev activate)
        (when buffer-file-name
          (when cats/verbose
            (message "!!! Switch prev buffer: %s" buffer-file-name))
          (cats/find-file-hook-to-project)))

      (defadvice switch-to-next-buffer (after cats/projectile-switch-next activate)
        (when buffer-file-name
          (when cats/verbose
            (message "!!! Switch next buffer: %s" buffer-file-name))
          (cats/find-file-hook-to-project)))

      (add-hook 'projectile-mode-hook 'cats/projectile-mode-hook)
      (defun cats/projectile-mode-hook ()
        (cond
         (projectile-mode
          (add-hook 'find-file-hook #'cats/find-file-hook-to-project t t))
         (t
          (remove-hook 'find-file-hook #'cats/find-file-hook-to-project t))))

      (defadvice projectile-mode (before caching-turn-off-maybe)
        (when (cats//current-buffer-remote-p)
          (setq-local projectile-enable-caching nil)))
      (ad-activate 'projectile-mode))))


;; desktop
(defun cats-core/pre-init-desktop ()
  (spacemacs|use-package-add-hook desktop
    :pre-config
    (progn
      (desktop-save-mode 0)

      (setq desktop-dirname desktop/desktop-dirname
            desktop-base-file-name desktop/desktop-base-file-name
            desktop-base-lock-name desktop/desktop-base-lock-name
            desktop-path (list desktop/desktop-dirname)
            desktop-load-locked-desktop nil
            ;; Fix the frameset warning at startup
            desktop-restore-frames nil
            ;; Save desktops a minute after Emacs was idle.
            desktop-auto-save-timeout 60)

      ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
      ;; Save a bunch of variables to the desktop file.
      ;; For lists, specify the length of the maximal saved data too.
      (setq desktop-globals-to-save
            (append '((comint-input-ring . 50)
                      desktop-missing-file-warning
                      (dired-regexp-history . 20)
                      (extended-command-history . 30)
                      (face-name-history . 20)
                      (file-name-history . 100)
                      (ido-buffer-history . 100)
                      (ido-last-directory-list . 100)
                      (ido-work-directory-list . 100)
                      (ido-work-file-list . 100)
                      (magit-read-rev-history . 50)
                      (minibuffer-history . 50)
                      (org-refile-history . 50)
                      (org-tags-history . 50)
                      (query-replace-history . 60)
                      (read-expression-history . 60)
                      (regexp-history . 60)
                      (regexp-search-ring . 20)
                      register-alist
                      (search-ring . 20)
                      (shell-command-history . 50)
                      tags-file-name
                      tags-table-list)))

      ;; Don't save .gpg files. Restoring those files in emacsclients causes
      ;; a problem as the password prompt appears before the frame is loaded.
      (setq desktop-files-not-to-save
            (concat "\\(^/[^/:]*:\\|(ftp)$\\)" ; original value
                    "\\|\\(\\.gpg$\\)"
                    "\\|\\(\\.plstore$\\)"
                    "\\|\\(\\.desktop$\\)"
                    "COMMIT_EDITMSG\\'"
                    ;; If backup files with names like "file.sv.20150619_1641.bkp"
                    ;; are saved to the desktop file, emacsclient crashes at launch
                    ;; Need to debug why that's the case. But for now, simply not
                    ;; saving the .bkp files to the desktop file is a workable
                    ;; solution -- Fri Jun 19 16:45:50 EDT 2015 - kmodi
                    "\\|\\(\\.bkp$\\)"
                    "\\|\\(\\TAGS$\\)"))



      ;; Don't save the eww buffers
      (setq desktop-buffers-not-to-save
            (concat desktop-buffers-not-to-save
                    "\\|\\(^eww\\(<[0-9]+>\\)*$\\)"))

      (dolist (mode '(magit-mode magit-log-mode))
        (add-to-list 'desktop-modes-not-to-save mode))

      ;; remove desktop after it's been read
      (add-hook 'desktop-after-read-hook 'cats//desktop-after-read))
    :pre-init
    (spacemacs/defer-until-after-user-config
     '(lambda ()
        ;; ask user whether to restore desktop at start-up
        ;; (add-hook 'emacs-startup-hook 'cats//desktop-after-init)
        (cats//desktop-after-init)))))


;; prettyify symbols
(defun cats-core/init-prettify-symbols-mode ()
  "Add prettify symbols."
  (use-package prettify-symbols-mode
    :commands prettify-symbols-mode
    :defer t
    :init
    (progn
      ;; (setq prettify-symbol-categories '(lambda relational logical))
      (setq prettify-symbols-unprettify-at-point 'right)
      (spacemacs|add-toggle prettify-symbols-mode
        :status prettify-symbols-mode
        :on (progn
              (when (bound-and-true-p prettify-symbols-mode)
                (prettify-symbols-mode -1))
              (prettify-symbols-mode))
        :off (prettify-symbols-mode -1)
        :documentation "Prettify symbols."
        :evil-leader "top")
      (spacemacs|add-toggle global-prettify-symbols-mode
        :status global-prettify-symbols-mode
        :on (progn
              (when (bound-and-true-p global-prettify-symbols-mode)
                (global-prettify-symbols-mode -1))
              (global-prettify-symbols-mode))
        :off (global-prettify-symbols-mode -1)
        :documentation "Prettify symbols globally."
        :evil-leader "to C-p"))
    :config (spacemacs|hide-lighter prettify-symbols-mode)))


;; eshell
(defun cats-core/pre-init-eshell ()
  (spacemacs|use-package-add-hook eshell
    :post-init
    (progn
      (setq compilation-environment '("TERM=xterm-256color"))
      (add-hook 'eshell-mode-hook
         (lambda ()
           ;; The 'ls' executable requires the Gnu version on the Mac
           (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                         "/usr/local/bin/gls"
                       "/bin/ls")))
             (add-to-list 'eshell-command-aliases-list (list "ll" (concat ls " -AlohG --color=always") ))))))
    :post-config
    (progn
      (require 'em-alias)
      )
    ))


;; term
(defun cats-core/pre-init-term ()
  ;; kill buffer after terminal is exited
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
        (let ((buffer (process-buffer proc)))
          ad-do-it
          (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)

  (defadvice ansi-term (before force-bash)
    (interactive (list shell-default-term-shell)))
  (ad-activate 'ansi-term)

  (with-eval-after-load 'exec-path-from-shell
    (let ((shell-term-shell (getenv "SHELL")))
      (unless (empty-string-p shell-term-shell)
        (setq shell-term-shell (chomp shell-term-shell))
        (setq shell-default-term-shell shell-term-shell)
        (setq multi-term-program shell-term-shell))))

  ;; display certain characters correctly
  (defun my-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)

  ;; clickable urls
  (defun my-term-hook ()
    (goto-address-mode))
  (add-hook 'term-mode-hook 'my-term-hook))


;; shell
(defun cats-core/pre-init-shell ())


;; xterm-color
(defun cats-core/pre-init-xterm-color ()
  (spacemacs|use-package-add-hook xterm-color
    :post-init
    (add-hook 'compilation-start-hook
       (lambda (proc)
         ;; We need to differentiate between compilation-mode buffers
         ;; and running as part of comint (which at this point we assume
         ;; has been configured separately for xterm-color)
         (when (eq (process-filter proc) 'compilation-filter)
           ;; This is a process associated with a compilation-mode buffer.
           ;; We may call `xterm-color-filter' before its own filter function.
           (set-process-filter
            proc
            (lambda (proc string)
              (funcall 'compilation-filter proc
                       (xterm-color-filter string)))))))))


;; tramp
(defun cats-core/pre-init-tramp ()
  (use-package tramp
    :defer t
    :config
    (progn
      ;; Use my ~/.ssh/config control master settings according to
      ;; https://puppet.com/blog/speed-up-ssh-by-reusing-connections
      ;; (setq tramp-ssh-controlmaster-options "")

      ;; From https://emacs.stackexchange.com/questions/22306
      (setq backup-enable-predicate
            (lambda (name)
              (and (normal-backup-enable-predicate name)
                   ;; Disable all tramp backups
                   (and cats/disable-tramp-backups
                        (member 'all cats/disable-tramp-backups)
                        (not (file-remote-p name 'method)))
                   (not ;; disable backup for tramp with the listed methods
                    (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method cats/disable-tramp-backups)))))))

      (defun tramp-set-auto-save--check (original)
        (if (funcall backup-enable-predicate (buffer-file-name))
            (funcall original)
          (auto-save-mode -1)))
      (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check))))

;;; packages.el ends here
