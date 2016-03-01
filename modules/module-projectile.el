;;; module-projectile.el --- Projectile Module
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
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'module-vars)
(require 'module-common)
(require 'module-core)
(require 'module-utils)

(dotemacs-use-package-add-hook projectile
  :pre-config
  (progn
    (projectile-register-project-type 'npm '("package.json") "npm" "npm test"))
  :post-init
  (progn
    (defun dotemacs//locate-mocha-from-projectile (&optional dir)
      "Use local mocha from `./node_modules` if available."
      (let ((proj-root (or dir (projectile-project-root)))
            (proj-type (projectile-detect-project-type)))
        (when (string= proj-type "npm")
          (async-start
           `(lambda ()
              (set  'proj-root ,proj-root)
              (let ((default-directory proj-root))
                (executable-find "mocha")))
           (lambda (result)
             (dotemacs/set-executable-mocha result))))))
    (add-hook 'dotemacs/project-hook 'dotemacs//locate-mocha-from-projectile)

    (defun dotemacs//locate-jshint-from-projectile (&optional dir)
      "Use local jshint from `./node_modules` if available."
      (let ((proj-root (or dir (projectile-project-root)))
            (proj-type (projectile-detect-project-type)))
        (when (string= proj-type "npm")
          (async-start
           `(lambda ()
              (set  'proj-root ,proj-root)
              (let ((default-directory proj-root))
                (executable-find "jshint")))
           (lambda (result)
             (dotemacs/set-executable-jshint result))))))
    (add-hook 'dotemacs/project-hook 'dotemacs//locate-jshint-from-projectile)

    (defun dotemacs//locate-jscs-from-projectile (&optional dir)
      "Use local jscs from `./node_modules` if available."
      (let ((proj-root (or dir (projectile-project-root)))
            (proj-type (projectile-detect-project-type)))
        (when (string= proj-type "npm")
          (async-start
           `(lambda ()
              (set  'proj-root ,proj-root)
              (let ((default-directory proj-root))
                (executable-find "jscs")))
           (lambda (result)
             (dotemacs/set-executable-jscs result))))))
    (add-hook 'dotemacs/project-hook 'dotemacs//locate-jscs-from-projectile)

    (defun dotemacs//locate-eslint-from-projectile (&optional dir)
      "Use local eslint from `./node_modules` if available."
      (let ((proj-root (or dir (projectile-project-root)))
            (proj-type (projectile-detect-project-type)))
        (when (string= proj-type "npm")
          (async-start
           `(lambda ()
              (set  'proj-root ,proj-root)
              (let ((default-directory proj-root))
                (executable-find "eslint")))
           (lambda (result)
             (dotemacs/set-executable-eslint result))))))
    (add-hook 'dotemacs/project-hook 'dotemacs//locate-eslint-from-projectile)

    ;; make-variable-frame-local is obsolete according to the docs, but I don't
    ;; want to have to manually munge frame-parameters all the time so I'm using
    ;; it anyway.
    (make-variable-frame-local
     (defvar dotemacs/projectile-curr nil
       "The current projectile project."))

    (defun dotemacs/find-file-hook-to-project ()
      "Use ."
      (when dotemacs/verbose
        (message "!!! Running dotemacs//find-file-hook-to-project"))
      (defun dotemacs/do-nothing ())
      (let ((projectile-require-project-root t))
        (when (projectile-project-p)
          (let ((project-root (projectile-project-root)))
            (when (and project-root
                       (not (string= project-root dotemacs/projectile-curr)))
              (let ((projectile-switch-project-action 'dotemacs/do-nothing))
                (when dotemacs/verbose
                  (message "!!! Project Root %s" project-root)
                  (message "!!! Buffer Name %s" buffer-file-name))
                (projectile-switch-project-by-name project-root)
                (setq dotemacs/projectile-curr project-root)
                (dotemacs/run-project-hook project-root)))))))

    ;; does not attach to any c-level calls but is best we got
    (defadvice switch-to-buffer (after dotemacs/projectile-switch activate)
      (when buffer-file-name
        (when dotemacs/verbose
          (message "!!! Switch to buffer: %s" buffer-file-name))
        (dotemacs/find-file-hook-to-project)))

    (defadvice switch-to-prev-buffer (after dotemacs/projectile-switch-prev activate)
      (when buffer-file-name
        (when dotemacs/verbose
          (message "!!! Switch prev buffer: %s" buffer-file-name))
        (dotemacs/find-file-hook-to-project)))

    (defadvice switch-to-next-buffer (after dotemacs/projectile-switch-next activate)
      (when buffer-file-name
        (when dotemacs/verbose
          (message "!!! Switch next buffer: %s" buffer-file-name))
        (dotemacs/find-file-hook-to-project)))

    (add-hook 'projectile-mode-hook 'dotemacs/projectile-mode-hook)
    (defun dotemacs/projectile-mode-hook ()
      (cond
       (projectile-mode
        (add-hook 'find-file-hook #'dotemacs/find-file-hook-to-project t t))
       (t
        (remove-hook 'find-file-hook #'dotemacs/find-file-hook-to-project t))))))

(use-package projectile
  :ensure t
  :defer 1.4
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (progn
    (defun projectile-find-file-ignored ()
      "Projectile find file without ignore."
      (interactive)
      (let ((projectile-git-command "git ls-files -zco"))
        (call-interactively 'projectile-find-file)))

    (defun projectile-add-magit-repo-dirs-to-known-projects ()
      "Add `magit-repo-dirs' to `projectile-known-projects'."
      (interactive)
      (--each (mapcar 'cdr (magit-list-repos magit-repo-dirs))
        (projectile-add-known-project (file-name-as-directory
                                       (file-truename it)))))

    ;; note for Windows: GNU find or Cygwin find must be in path to enable
    ;; fast indexing
    (when (and (dotemacs/system-is-mswindows) (executable-find "find"))
      (setq projectile-indexing-method 'alien
            projectile-generic-command "find . -type f"))
    (when-let (gnu-find (and (dotemacs/system-is-mac)
                             (executable-find "gfind")))
      (setq projectile-generic-command "gfind . -type f print0"))

    (setq projectile-sort-order 'recentf
          projectile-use-git-grep t
          projectile-switch-project-action 'projectile-dired
          projectile-cache-file (concat dotemacs-cache-directory
                                        "projectile.cache")
          projectile-known-projects-file (concat dotemacs-cache-directory
                                                 "projectile-bookmarks.eld")
          projectile-find-dir-includes-top-level t
          projectile-require-project-root t
          projectile-verbose nil)

    (setq projectile-enable-caching nil)
    (defadvice projectile-mode (before maybe-use-cache activate)
      (when
        (--any? (and it (file-remote-p it))
                (list
                  (buffer-file-name)
                   list-buffers-directory
                   default-directory))
        (setq-local projectile-enable-caching t)))

    (unless (boundp 'dotemacs-use-helm-projectile)
      (dotemacs-set-leader-keys
        "pb" 'projectile-switch-to-buffer
        "pd" 'projectile-find-dir
        "pf" 'projectile-find-file
        "pF" 'projectile-find-file-dwim
        "ph" 'helm-projectile
        "pr" 'projectile-recentf
        "ps" 'projectile-switch-project))
    (dotemacs-set-leader-keys
      "p!" 'projectile-run-shell-command-in-root
      "p&" 'projectile-run-async-shell-command-in-root
      "pa" 'projectile-toggle-between-implementation-and-test
      "pc" 'projectile-compile-project
      "pD" 'projectile-dired
      "pG" 'projectile-regenerate-tags
      "pI" 'projectile-invalidate-cache
      "pk" 'projectile-kill-buffers
      "po" 'projectile-multi-occur
      "pR" 'projectile-replace
      "pT" 'projectile-find-test-file
      "py" 'projectile-find-tag))
  :config
  (progn
    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil 'projectile-cleanup-known-projects)
    (projectile-global-mode)
    (dotemacs-hide-lighter projectile-mode)))

(use-package help-fns+
  :commands (describe-keymap)
  :ensure t
  :init
  (dotemacs-set-leader-keys "hdK" 'describe-keymap))

(use-package helm-projectile
  :ensure t
  :defer t
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project)
  :init
  (progn
    (setq projectile-switch-project-action 'helm-projectile)

    (defconst dotemacs-use-helm-projectile t
      "This variable is only defined if helm-projectile is used.")

    ;; needed for smart search if user's default tool is grep
    (defalias 'dotemacs-helm-project-do-grep 'helm-projectile-grep)
    (defalias 'dotemacs-helm-project-do-grep-region-or-symbol 'helm-projectile-grep)

    (dotemacs-set-leader-keys
      "pb"  'helm-projectile-switch-to-buffer
      "pd"  'helm-projectile-find-dir
      "pf"  'helm-projectile-find-file
      "ph"  'helm-projectile
      "pp"  'helm-projectile-switch-project
      "pr"  'helm-projectile-recentf
      "pv"  'projectile-vc
      "sgp" 'helm-projectile-grep))
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'helm-projectile-sources-list 'helm-source-projectile-recentf-list)))

(provide 'module-projectile)
;;; module-projectile.el ends here
