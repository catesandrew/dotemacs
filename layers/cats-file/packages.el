;;; packages.el --- cats: File

;;; Commentary:

;;; Code:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-file-packages
  '(desktop
     (files :location built-in)
     helm
     helm-projectile
     ignoramus
     neotree
     projectile
     (recentf :location built-in)
     (recentf-ext :location local)
     (simple :location built-in)
     ;; zel
     ))

(defun cats-file/init-simple ()
  "Auto refresh, auto-revert buffers of changed files."
  (use-package simple
    :config
    (progn
      ;; ensures we can quickly pop the mark several times by typing
      ;; `C-u C-SPC C-SPC`, instead of having to type `C-u C-SPC C-u C-SPC`.
      (setq set-mark-command-repeat-pop t)

      ;; When popping the mark, continue popping until the cursor actually moves
      ;; Also, if the last command was a copy - skip past all the expand-region
      ;; cruft. Testing the new `advicd-add` interface.
      (defun cats/multi-pop-to-mark (orig-fun &rest args)
        "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
        (let ((p (point)))
          (when (eq last-command 'save-region-or-current-line)
            (apply orig-fun args)
            (apply orig-fun args)
            (apply orig-fun args))
          (dotimes (i 10)
            (when (= p (point)) (apply orig-fun args)))))
      (advice-add 'pop-to-mark-command :around 'cats/multi-pop-to-mark))))

(defun cats-file/init-files ()
  "Auto refresh, auto-revert buffers of changed files."
  (use-package files
    :init
    (progn
      (setq view-read-only t) ; View read-only files
      (add-to-list 'find-file-not-found-functions 'cats//create-non-existent-directory))))

(defun cats-file/init-ignoramus ()
  "Auto refresh, auto-revert buffers of changed files."
  (use-package ignoramus
  :ensure t
  :init
  (progn
    ;; List of file beginnings to ignore. These are not regular expressions, but
    ;; literal strings which occur at the beginnings of file or directory names
    ;; to ignore. The string to match comprises only the last element of a
    ;; fully-qualified pathname."
    (setq ignoramus-file-basename-beginnings
          ignoramus/file-basename-beginnings)

    ;; List of file endings to ignore. These are not regular expressions, but
    ;; literal strings which occur at the ends of file names to ignore."
    (setq ignoramus-file-basename-endings
          ignoramus/file-basename-endings)

    ;; List of exact filenames to ignore. These are not regular expressions, but
    ;; literal strings which exactly match a file or directory name to ignore.
    ;; The string to match comprises only the last element of a fully-qualified
    ;; pathname."
    (setq ignoramus-file-basename-exact-names
          (append ignoramus/file-basename-exact-names
                  ignoramus/directory-basename-exact-names))

    ;; List of regexps matching filenames to ignore. The string to match
    ;; comprises only the last element of a fully-qualified pathname."
    (setq ignoramus-file-basename-regexps
          ignoramus/file-basename-regexps))
  :config
  (progn
    (ignoramus-setup '(comint
                       completions
                       dired
                       eshell
                       grep
                       ido
                       pcomplete
                       shell)))))

(defun cats-file/pre-init-recentf ()
  (spacemacs|use-package-add-hook recentf
    :post-init
    (progn
      (setq recentf-max-saved-items 5000)
      (setq recentf-max-menu-items 1000)
      (advice-add 'recentf-cleanup :around 'cats//shut-up-around)

      ;; Cleanup recent files only when Emacs is idle, but not when the mode is
      ;; enabled, because that unnecessarily slows down Emacs. My Emacs idles
      ;; often enough to have the recent files list clean up regularly
      (setq recentf-auto-cleanup 300)))

  (when (configuration-layer/package-usedp 'ignoramus)
    (spacemacs|use-package-add-hook recentf
      :pre-config
      (with-eval-after-load 'ignoramus
        (setq recentf-exclude
              (append recentf-exclude
                      (list ignoramus-boring-file-regexp)))))))


;; recentf
(defun cats-file/init-recentf-ext ()
  "Switching to file buffer considers it as most recent file."
  (use-package recentf-ext
    :after recentf))


;; projectile
(defun cats-file/pre-init-projectile ()
  (when (configuration-layer/package-usedp 'ignoramus)
    (spacemacs|use-package-add-hook projectile
      :post-init
      (progn
        (add-hook 'cats/find-executable-hook
           'cats//projectile-set-find-executable)

        ;; (add-hook 'emacs-startup-hook 'cats//locate-find)
        ;; (run-hooks 'emacs-startup-hook)
        ;; (cats//locate-find)

        (setq projectile-use-git-grep t)
        (setq projectile-find-dir-includes-top-level t)
        (setq projectile-verbose nil))
      :pre-config
      (progn
        (with-eval-after-load 'ignoramus
          (advice-add 'projectile-ignored-directory-p
                      :around 'cats//projectile-ignored-directory-p)
          (advice-add 'projectile-ignored-file-p
                      :around 'cats//projectile-ignored-file-p)
          (setq projectile-globally-ignored-file-suffixes
                (mapcar #'(lambda (ext)
                            (replace-regexp-in-string "\\`\\." "" ext))
                        ignoramus/file-basename-endings))
          (setq projectile-globally-ignored-files
                ignoramus/file-basename-exact-names)
          (setq projectile-globally-ignored-directories
                (append ignoramus/directory-basename-exact-names
                        (--map (s-append "/" it)
                               (--map (s-prepend "-" it)
                                      ignoramus/directory-basename-exact-names)))))))))

(defun cats-file/post-init-projectile ()
  "Remove dead projects when Emacs is idle."
  (run-with-idle-timer 120 nil #'projectile-cleanup-known-projects))

(defun cats-file/pre-init-helm-projectile ()
  (spacemacs|use-package-add-hook helm-projectile
    :post-config
    (progn
      (with-eval-after-load 'projectile
        (add-to-list 'helm-projectile-sources-list 'helm-source-projectile-recentf-list)))))

(defun cats-file/pre-init-neotree ()
  (when (configuration-layer/package-usedp 'ignoramus)
    (spacemacs|use-package-add-hook neotree
      :pre-config
      (progn
        (with-eval-after-load 'ignoramus
          (setq neo-hidden-regexp-list
                (list ignoramus-boring-file-regexp))))))

  (when (configuration-layer/package-usedp 'projectile)
    (spacemacs|use-package-add-hook neotree
      :post-init
      (progn
        (add-hook 'cats/project-hook
           'cats//neotree-dir-from-projectile-root t nil))))

  (spacemacs|use-package-add-hook neotree
    :post-init
    (progn
      ;; (setq neo-mode-line-type 'none)
      (setq neo-window-width 27)
      (setq neo-theme 'nerd)
      ;; if t, every time when the neotree window is opened, it will try to find
      ;; current file and jump to node.
      (setq neo-smart-open nil)
      ;; Don't allow neotree to be the only open window
      (setq neo-show-hidden-files nil)

      (defun cats/neotree-find ()
        (interactive)
        (neotree-find)
        (recenter))

      (spacemacs/declare-prefix "f/" "neotree find")
      (spacemacs/set-leader-keys
        "f/" 'cats/neotree-find
        "fn" 'neotree-show
        "fN" 'neotree-hide))

    :post-config
    (when neo-persist-show
      (add-hook 'popwin:before-popup-hook
                (lambda () (setq neo-persist-show nil)))
      (add-hook 'popwin:after-popup-hook
                (lambda () (setq neo-persist-show t))))
    )
  )

(defun cats-file/pre-init-desktop ()
  (when (configuration-layer/package-usedp 'ignoramus)
    (spacemacs|use-package-add-hook desktop
      :pre-config
      (progn
        (with-eval-after-load 'ignoramus
          (setq desktop-files-not-to-save
                (concat desktop-files-not-to-save
                        ignoramus-boring-file-regexp)))))))

(defun cats-file/pre-init-helm ()
  (when (configuration-layer/package-usedp 'ignoramus)
    (spacemacs|use-package-add-hook helm
      :pre-config
      (progn
        (with-eval-after-load 'ignoramus
          (setq helm-grep-ignored-files
                (cons ".#*"
                      (delq nil
                            (mapcar
                             #'(lambda (pat)
                                 (concat "*" pat)) ignoramus/file-basename-endings))))
          (setq helm-grep-ignored-directories
                ignoramus/directory-basename-exact-names))))))


;; zel
(defun cats-file/init-zel ()
  "zel tracks the most used files, based on frecency.

Zel is basically a port of z in Emacs Lisp."
  (use-package zel
    :demand t
    :bind (("C-c C-r" . zel-find-file-frecent))
    :init
    (progn
      (push "zel-history" spacemacs-useless-buffers-regexp)
      (setq zel-history-file (concat spacemacs-cache-directory "zel-history"))
      (setq zel--aging-threshold 90000))
    :config
    (progn
      (add-to-list 'zel-exclude-patterns
        (file-truename spacemacs-cache-directory))
      (add-to-list 'zel-exclude-patterns (file-truename package-user-dir))
      (add-to-list 'zel-exclude-patterns "COMMIT_EDITMSG\\'")
      (with-eval-after-load 'ignoramus
        (add-to-list 'zel-exclude-patterns ignoramus-boring-file-regexp)
        ;; (setq zel-exclude-patterns
        ;;   (append zel-exclude-patterns
        ;;     (list ignoramus-boring-file-regexp)))
        )
      (zel-install)
      ;; (run-at-time nil (* 5 60) 'recentf-save-list)
      (run-with-idle-timer (* 1 60) t 'zel-write-history)))

  (spacemacs|use-package-add-hook projectile
    "List frecent file paths in descending order by their rank."
    :post-config
    (progn
      (spacemacs/set-leader-keys
        "ps" 'projectile-run-project
        "pz" 'projectile-zel-frecent))))

;;; packages.el ends here
