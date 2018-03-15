(defun cats/executable-find (command directory)
  "Search for COMMAND in DIRECTORY and return the absolute file name.
Return nil if COMMAND is not found anywhere in DIRECTORY."
  (let ((default-directory directory))
    (executable-find command)))

(defun cats/set-executable-eslint (eslint)
  "Set cats//executable-eslint to ESLINT."
  (unless (string= eslint cats//executable-eslint)
    (setq cats//executable-eslint eslint)
    (ignore-errors (run-hook-with-args 'cats/eslint-executable-hook eslint))))

(defun cats/set-executable-jscs (jscs)
  "Set cats//executable-jscs to JSCS."
  (unless (string= jscs cats//executable-jscs)
    (setq cats//executable-jscs jscs)
    (ignore-errors (run-hook-with-args 'cats/jscs-executable-hook jscs))))

(defun cats/set-executable-jshint (jshint)
  "Set cats//executable-jshint to JSHINT."
  (unless (string= jshint cats//executable-jshint)
    (setq cats//executable-jshint jshint)
    (ignore-errors (run-hook-with-args 'cats/jshint-executable-hook jshint))))

(defun cats/set-executable-tidy (tidy)
  "Set cats//executable-tidy to TIDY."
  (unless (string= tidy cats//executable-tidy)
    (setq cats//executable-tidy tidy)
    (ignore-errors (run-hook-with-args 'cats/tidy-executable-hook tidy))))

(defun cats/set-executable-mocha (mocha)
  "Set cats//executable-mocha to MOCHA."
  (unless (string= mocha cats//executable-mocha)
    (setq cats//executable-mocha mocha)
    (ignore-errors (run-hook-with-args 'cats/mocha-executable-hook mocha))))

(defun cats/set-executable-babel-node (babel-node)
  "Set cats//executable-babel-node to BABEL-NODE."
  (unless (string= babel-node cats//executable-babel-node)
    (setq cats//executable-babel-node babel-node)
    (ignore-errors
      (run-hook-with-args 'cats/babel-node-executable-hook babel-node))))

(defun cats/set-executable-coffeelint (coffeelint)
  "Set cats//executable-coffeelint to COFFEELINT."
  (unless (string= coffeelint cats//executable-coffeelint)
    (setq cats//executable-coffeelint coffeelint)
    (ignore-errors
      (run-hook-with-args 'cats/coffeelint-executable-hook coffeelint))))

(defun cats/set-executable-node (node)
  "Set cats//executable-node to NODE."
  (unless (string= node cats//executable-node)
    (setq cats//executable-node node)
    (ignore-errors
      (run-hook-with-args 'cats/node-executable-hook node))))

(defun cats/set-executable-phantomjs (phantomjs)
  "Set cats//executable-phantomjs to PHANTOMJS."
  (unless (string= phantomjs cats//executable-phantomjs)
    (setq cats//executable-phantomjs phantomjs)
    (ignore-errors
      (run-hook-with-args 'cats/phantomjs-executable-hook phantomjs))))

(defun cats/set-executable-handlebars (handlebars)
  "Set cats//executable-handlebars to HANDLEBARS."
  (unless (string= handlebars cats//executable-handlebars)
    (setq cats//executable-handlebars handlebars)
    (ignore-errors
      (run-hook-with-args 'cats/handlebars-executable-hook handlebars))))

(defun cats/set-executable-find (find)
  "Set cats//executable-find to FIND."
  (unless (string= find cats//executable-find)
    (setq cats//executable-find find)
    (ignore-errors
      (run-hook-with-args 'cats/find-executable-hook find))))

(defun cats/set-executable-tsserver (tsserver)
  "Set cats//executable-tsserver to TSSERVER."
  (unless (string= tsserver cats//executable-tsserver)
    (setq cats//executable-tsserver tsserver)
    (ignore-errors (run-hook-with-args 'cats/tsserver-executable-hook tsserver))))


;; frame
(defun cats//initialize-frame-transparency ()
  "Transparent frame."
  ;; https://github.com/d12frosted/homebrew-emacs-plus/blob/master/Formula/emacs-plus.rb#L98
  ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Properties-in-Mode.html#Properties-in-Mode
  (when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (spacemacs/toggle-transparent-frame)))
(add-hook 'after-make-frame-functions 'cats//initialize-frame-transparency)

(defun cats//initialize-frame-uuid (frame)
  (set-frame-parameter frame 'cats//frame-name (uuidgen-4)))
(add-hook 'after-make-frame-functions 'cats//initialize-frame-uuid)


;; projectile
(defun cats/run-project-hook (dir frame-name)
  "Set `cats//projectile-curr' with `DIR' and `FRAME-NAME'."
  ;; (princ (format "cats/run-project-hook dir: `%s'\n" dir))
  ;; (princ (format "cats/run-project-hook cats//projectile-curr: `%s'\n" cats//projectile-curr))
  ;; (princ (format "cats/run-project-hook frame-name: `%s''\n" frame-name))
  ;; (princ (format "cats/run-project-hook dir: `%s''\n" dir))
  (when dir
    (unless (string= dir (frame-parameter nil 'cats//projectile-curr))
      (set-frame-parameter nil 'cats//projectile-curr dir)
      (ignore-errors (run-hook-with-args 'cats/project-hook dir frame-name)))))

(defun cats//get-frame-name (frame)
  "Helper function to extract the name of a `FRAME'."
  (frame-parameter frame 'cats//frame-name))

(defun cats//dabbrev-from-projectile (&optional dir)
  "Use ."
  (add-to-list 'directory-abbrev-alist
               (cons
                (concat "^" (directory-file-name (or dir (projectile-project-root))))
                (file-name-nondirectory (directory-file-name (or dir (projectile-project-root)))))))

(defun cats//do-nothing ()
  "A function that does nothing.")

(defun cats/find-file-hook-to-project ()
  "Use ."
  ;; (unless (cats//current-buffer-remote-p))
  (condition-case err
      (let ((projectile-require-project-root t))
        (projectile-project-root)
        (let ((project-root (projectile-project-root))
              (proj-dir-root (directory-file-name (projectile-project-root)))
              (proj-dir-base (file-name-nondirectory (directory-file-name (projectile-project-root)))))
          ;; (message "project-root:%s" project-root)
          ;; (message "cats//projectile-curr: %s" cats//projectile-curr)
          (when (and project-root
                  (not (string= project-root (frame-parameter nil 'cats//projectile-curr))))
            (let ((projectile-switch-project-action 'cats//do-nothing)
                  (projectile-before-switch-project-hook 'cats//do-nothing)
                  (projectile-after-switch-project-hook 'cats//do-nothing)
                  (frame-name (cats//get-frame-name nil)))
              ;; (message "Project Base %s" proj-dir-base)
              ;; (message "Project Dir %s" proj-dir-root)
              ;; (message "Buffer Name %s" buffer-file-name)
              ;; (princ (format "frame-name: `%s''\n" frame-name))
              (set-frame-parameter nil 'cats//projectile-switching-project-by-name t)
              (projectile-switch-project-by-name project-root)
              (set-frame-parameter nil 'cats/projectile-dir-root proj-dir-root)
              (set-frame-parameter nil 'cats/projectile-dir-base proj-dir-base)
              (cats/run-project-hook project-root frame-name)))))
    (error
     (progn
       (set-frame-parameter nil 'cats/projectile-dir-root nil)
       (set-frame-parameter nil 'cats/projectile-dir-base nil))
     nil)))


;; other funcs

(defun cats/abbreviate-file-name (filename)
  "Return a version of FILENAME shortened."
  ;; Get rid of the prefixes added by the automounter.
  (save-match-data
    (if (string-match (concat "^" (frame-parameter nil 'cats/projectile-dir-root)) filename)
        (setq filename
              (concat "★" (substring filename (match-end 0))))
      (setq filename (file-name-nondirectory filename))
      ;; (setq filename (concat (frame-parameter nil 'cats/projectile-dir-base)
      ;;                    (substring filename (match-end 0))))
      )))

(defun cats//set-frame-size (frame)
  "For the height, subtract 52 pixels from the screen height (for panels,
menubars and what not), then divide by the height of a char to get the
height we want. Use 140 char wide window for largeish displays and
smaller 100 column windows for smaller displays."
  (when (display-graphic-p)
    (let* ((fwp (if (> (x-display-pixel-width) 1680) 140 100))
            (fhp (/ (- (x-display-pixel-height) 52)
                   (frame-char-height))))
      (setq cats//frame-width fwp)
      (setq cats//frame-height fhp)
      (add-to-list 'initial-frame-alist `(width . ,cats//frame-width))
      (add-to-list 'initial-frame-alist `(height . ,cats//frame-height))
      (add-to-list 'default-frame-alist `(height . ,cats//frame-height))
      (add-to-list 'default-frame-alist `(width  . ,cats//frame-width))
      (set-frame-height frame fhp)
      (set-frame-width frame fwp))))
(add-hook 'after-make-frame-functions 'cats//set-frame-size)

(defun cats//do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) buffer/do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun cats//force-save-some-buffers ()
  "Save all modified buffers, without prompts."
  (save-some-buffers 'dont-ask))

(defun cats/desktop-session-restore-and-enable ()
  "Load the desktop and enable autosaving."
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (if (cats//saved-session)
        (desktop-read)
      (message "No desktop found."))
    (desktop-save-mode 1)))

;; use session-save to save the desktop manually
(defun cats/session-save ()
  "Save an Emacs session."
  (interactive)
  (if (cats//saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; use session-restore to restore the desktop manually
(defun cats/session-restore ()
  "Restore a saved Emacs session."
  (interactive)
  (if (cats//saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun cats//desktop-after-read ()
  "Load the desktop and enable autosaving."
  ;; desktop-remove clears desktop-dirname
  (setq desktop-dirname-tmp desktop/desktop-dirname)
  (desktop-remove)
  (setq desktop-dirname desktop-dirname-tmp))

(defun cats//saved-session ()
  "Save session."
  (file-exists-p (
    concat desktop/desktop-dirname "/" desktop/desktop-base-file-name)))

(defun cats//desktop-after-init ()
  "Save an emacs session."
  (if (cats//saved-session)
      (if (y-or-n-p "Restore desktop? ")
          (cats/session-restore))))


;; find
(defun cats//locate-find ()
  "Use find or gfind."
  (if (eq system-type 'darwin)
      (setq app "gfind")
    (setq app "find"))

  (async-start
   `(lambda ()
      (set  'app ,app)
      (executable-find app))
   (lambda (result)
     (when result
       (cats/set-executable-find result)))))


;; tidy
(defun cats//locate-tidy ()
  "Use tidy5 or tidy."
  (async-start
   `(lambda ()
      (executable-find "tidy"))
   (lambda (result)
     (when result
       (cats/set-executable-tidy result)))))


;; tsserver

(defvar cats//executable-tsserver nil)
(defvar cats/tsserver-executable-hook nil
  "Hooks run when cats//executable-tsserver is changed.")


;; name and email

;; get vars from system or env vars, below are two ways with osx:
;; (shell-command-to-string "finger `whoami` | awk -F: '{ print $3 }' | head -n1 | sed 's/^ //'")
;; (shell-command-to-string "dscl . read /Users/`whoami` RealName | grep -v RealName | cut -c 2-")
(defun cats//locate-name ()
  ;; (setq cats//name (chomp (getenv "NAME")))
  (setq cats//name (getenv "NAME"))

  (unless (empty-string-p cats//name)
    (setq cats//name (chomp cats//name)))

  (when (empty-string-p cats//name)
    (async-start
     `(lambda ()
        (when (eq system-type 'darwin)
          (shell-command-to-string "finger `whoami` | awk -F: '{ print $3 }' | head -n1 | sed 's/^ //'")))
     (lambda (result)
       (when result
         (setq cats//name result)))))

  (unless (empty-string-p cats//name)
    (setq user-full-name (chomp cats//name))))

(defun cats//locate-email ()
  ;; (setq email (chomp (getenv "EMAIL")))
  (setq cats//email (getenv "EMAIL"))

  (unless (empty-string-p cats//email)
    (setq cats//email (chomp cats//email)))

  (unless (empty-string-p cats//email)
    (setq user-mail-address cats//email)
    (setq epa-file-encrypt-to cats//email)))


;; prettyify symbols
(defun cats/pretty-symbols (new-pretty-symbols)
  (mapcar
   (lambda (item)
     (push item prettify-symbols-alist))
   new-pretty-symbols))

;;; funcs.el ends here
