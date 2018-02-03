;;; packages.el --- cats: Core

;;; Commentary:

;;; Code:

(require 'subr-x)

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-core-packages
  '(projectile
    autorevert
    buffer-move
    desktop
    (prettify-symbols-mode :location built-in)
    ))


;; autorevert
(defun cats-core/init-autorevert ()
  "Auto refresh, auto-revert buffers of changed files."
  (use-package autorevert
    :init
    (progn
      (setq auto-revert-check-vc-info nil
            auto-revert-mode-text " ♻"
            auto-revert-tail-mode-text " ♻~")
      (add-hook 'find-file-hook 'cats//auto-revert-turn-on-maybe))))


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
      (setq projectile-enable-caching t)

      (defun cats//locate-jshint-from-projectile (&optional dir)
        "Use local jshint from `./node_modules` if available."
        (when (empty-string-p dir)
          (setq dir default-directory))

        (let ((default-directory dir))
          (async-start
           `(lambda ()
              (executable-find "jshint"))
           (lambda (result)
             (when result
               (cats/set-executable-jshint result))))))
      (add-hook 'cats/project-hook 'cats//locate-jshint-from-projectile)

      (defun cats//locate-jscs-from-projectile (&optional dir)
        "Use local jscs from `./node_modules` if available."
        (when (empty-string-p dir)
          (setq dir default-directory))

        (let ((default-directory dir))
          (async-start
           `(lambda ()
              (executable-find "jscs"))
           (lambda (result)
             (when result
               (cats/set-executable-jscs result))))))
      (add-hook 'cats/project-hook 'cats//locate-jscs-from-projectile)

      (defun cats//locate-eslint-from-projectile (&optional dir)
        "Use local eslint from `./node_modules` if available."
        (when (empty-string-p dir)
          (setq dir default-directory))

        (let ((default-directory dir))
          (async-start
           `(lambda ()
              (executable-find "eslint"))
           (lambda (result)
             (when result
               (cats/set-executable-eslint result))))))
      (add-hook 'cats/project-hook 'cats//locate-eslint-from-projectile)

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
      )))


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

;;; packages.el ends here
