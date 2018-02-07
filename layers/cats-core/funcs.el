(defun cats/run-project-hook (&optional dir)
  "Sets cats//projectile-curr."
  (unless (string= dir cats//projectile-curr)
    (when cats/verbose
      (message "projectile-curr updated: %s" dir))
    (setq cats//projectile-curr dir)
    (ignore-errors (run-hook-with-args 'cats/project-hook dir))))

(defun cats/executable-find (command directory)
  "Search for COMMAND in DIRECTORY and return the absolute file name.
Return nil if COMMAND is not found anywhere in DIRECTORY."
  (let ((default-directory directory))
    (executable-find command)))

(defun cats/set-executable-eslint (eslint)
  "Set cats//executable-eslint to ESLINT."
  (unless (string= eslint cats//executable-eslint)
    (when cats/verbose
      (message "eslint %s updated." eslint))
    (setq cats//executable-eslint eslint)
    (ignore-errors (run-hook-with-args 'cats/eslint-executable-hook eslint))))

(defun cats/set-executable-jscs (jscs)
  "Set cats//executable-jscs to JSCS."
  (unless (string= jscs cats//executable-jscs)
    (when cats/verbose
      (message "jscs %s updated." jscs))
    (setq cats//executable-jscs jscs)
    (ignore-errors (run-hook-with-args 'cats/jscs-executable-hook jscs))))

(defun cats/set-executable-jshint (jshint)
  "Set cats//executable-jshint to JSHINT."
  (unless (string= jshint cats//executable-jshint)
    (when cats/verbose
      (message "jshint %s updated." jshint))
    (setq cats//executable-jshint jshint)
    (ignore-errors (run-hook-with-args 'cats/jshint-executable-hook jshint))))

(defun cats/set-executable-tidy (tidy)
  "Set cats//executable-tidy to TIDY."
  (unless (string= tidy cats//executable-tidy)
    (when cats/verbose
      (message "tidy %s updated." tidy))
    (setq cats//executable-tidy tidy)
    (ignore-errors (run-hook-with-args 'cats/tidy-executable-hook tidy))))

(defun cats/set-executable-mocha (mocha)
  "Set cats//executable-mocha to MOCHA."
  (unless (string= mocha cats//executable-mocha)
    (when cats/verbose
      (message "mocha %s updated." mocha))
    (setq cats//executable-mocha mocha)
    (ignore-errors (run-hook-with-args 'cats/mocha-executable-hook mocha))))

(defun cats/set-executable-babel-node (babel-node)
  "Set cats//executable-babel-node to BABEL-NODE."
  (unless (string= babel-node cats//executable-babel-node)
    (when cats/verbose
      (message "babel-node %s updated." babel-node))
    (setq cats//executable-babel-node babel-node)
    (ignore-errors
      (run-hook-with-args 'cats/babel-node-executable-hook babel-node))))

(defun cats/set-executable-coffeelint (coffeelint)
  "Set cats//executable-coffeelint to COFFEELINT."
  (unless (string= coffeelint cats//executable-coffeelint)
    (when cats/verbose
      (message "coffeelint %s updated." coffeelint))
    (setq cats//executable-coffeelint coffeelint)
    (ignore-errors
      (run-hook-with-args 'cats/coffeelint-executable-hook coffeelint))))

(defun cats/set-executable-node (node)
  "Set cats//executable-node to NODE."
  (unless (string= node cats//executable-node)
    (when cats/verbose
      (message "node %s updated." node))
    (setq cats//executable-node node)
    (ignore-errors
      (run-hook-with-args 'cats/node-executable-hook node))))

(defun cats/set-executable-phantomjs (phantomjs)
  "Set cats//executable-phantomjs to PHANTOMJS."
  (unless (string= phantomjs cats//executable-phantomjs)
    (when cats/verbose
      (message "phantomjs %s updated." phantomjs))
    (setq cats//executable-phantomjs phantomjs)
    (ignore-errors
      (run-hook-with-args 'cats/phantomjs-executable-hook phantomjs))))

(defun cats/set-executable-handlebars (handlebars)
  "Set cats//executable-handlebars to HANDLEBARS."
  (unless (string= handlebars cats//executable-handlebars)
    (when cats/verbose
      (message "handlebars %s updated." handlebars))
    (setq cats//executable-handlebars handlebars)
    (ignore-errors
      (run-hook-with-args 'cats/handlebars-executable-hook handlebars))))

(defun cats/set-executable-find (find)
  "Set cats//executable-find to FIND."
  (unless (string= find cats//executable-find)
    (when cats/verbose
      (message "find %s updated." find))
    (setq cats//executable-find find)
    (ignore-errors
      (run-hook-with-args 'cats/find-executable-hook find))))


;; other funcs

(defun cats/abbreviate-file-name (filename)
  "Return a version of FILENAME shortened."
  ;; Get rid of the prefixes added by the automounter.
  (save-match-data
    (if (string-match (concat "^" cats/projectile-dir-root) filename)
        (setq filename
              (concat "★" (substring filename (match-end 0))))
      (setq filename (file-name-nondirectory filename))
        ;; (setq filename
        ;;       (concat cats/projectile-dir-base
        ;;               (substring filename (match-end 0))))
      )))

(defun cats//set-frame-size ()
  "For the height, subtract 52 pixels from the screen height (for panels,
menubars and what not), then divide by the height of a char to get the
height we want. Use 140 char wide window for largeish displays and
smaller 100 column windows for smaller displays."
  (let* ((fwp (if (> (x-display-pixel-width) 1680) 140 100))
         (fhp (/ (- (x-display-pixel-height) 52)
                 (frame-char-height))))
    (setq cats//frame-width fwp)
    (setq cats//frame-height fhp)
    (add-to-list 'initial-frame-alist `(width . ,cats//frame-width))
    (add-to-list 'initial-frame-alist `(height . ,cats//frame-height))
    (add-to-list 'default-frame-alist `(height . ,cats//frame-height))
    (add-to-list 'default-frame-alist `(width  . ,cats//frame-width))
    (set-frame-height (selected-frame) fhp)
    (set-frame-width (selected-frame) fwp)))

(defun cats//auto-revert-turn-on-maybe ()
  (unless (file-remote-p default-directory)
    (auto-revert-mode)))

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

(defun cats//desktop-session-restore-and-enable ()
  "Load the desktop and enable autosaving."
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (if (cats//saved-session)
        (desktop-read)
      (message "No desktop found."))
    (desktop-save-mode 1)))

;; use session-save to save the desktop manually
(defun cats//session-save ()
  "Save an Emacs session."
  (interactive)
  (if (cats//saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; use session-restore to restore the desktop manually
(defun cats//session-restore ()
  "Restore a saved Emacs session."
  (interactive)
  (if (cats//saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun cats//desktop-after-read ()
  "Load the desktop and enable autosaving."
  (interactive)
  ;; desktop-remove clears desktop-dirname
  (setq desktop-dirname-tmp desktop/desktop-dirname)
  (desktop-remove)
  (setq desktop-dirname desktop-dirname-tmp))

(defun cats//saved-session ()
  "Save session."
  (interactive)
  (file-exists-p (
    concat desktop/desktop-dirname "/" desktop/desktop-base-file-name)))

(defun cats//desktop-after-init ()
  "Save an emacs session."
  (interactive)
  (if (cats//saved-session)
      (if (y-or-n-p "Restore desktop? ")
          (cats//session-restore))))


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

  ;; (when (empty-string-p cats//email)
  ;;   (async-start
  ;;    `(lambda ()
  ;;       (when (eq system-type 'darwin)
  ;;         (shell-command-to-string "finger")))
  ;;    (lambda (result)
  ;;      (when result
  ;;        (setq cats//email result)))))

  (unless (empty-string-p cats//email)
    (setq user-mail-address (chomp cats//email))))


;; prettyify symbols
(defun cats/pretty-symbols (new-pretty-symbols)
  (mapcar
   (lambda (item)
     (push item prettify-symbols-alist))
   new-pretty-symbols))

;;; funcs.el ends here
