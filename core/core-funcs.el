;;; core-funcs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar dotemacs-repl-list '()
  "List of all registered REPLs.")
(defvar dotemacs-post-user-config-hook nil
  "Hook run after dotemacs/user-config")
(defvar dotemacs-post-user-config-hook-run nil
  "Whether `dotemacs-post-user-config-hook' has been run")

(defun dotemacs-mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun dotemacs-mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.

If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun dotemacs-dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dotemacs-dump varlist buf)
      (make-directory (file-name-directory filename) t)
      (save-buffer)
      (kill-buffer))))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun dotemacs-dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (cl-loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(defvar dotemacs--init-redisplay-count 0
  "The number of calls to `redisplay'")
(defun dotemacs-redisplay ()
  "`redisplay' wrapper."
  (setq dotemacs--init-redisplay-count (1+ dotemacs--init-redisplay-count))
  (redisplay))

(defun dotemacs//create-key-binding-form (props func)
  "Helper which returns a from to bind FUNC to a key according to PROPS.

Supported properties:

`:evil-leader STRING'
    One or several key sequence strings to be set with `dotemacs-set-leader-keys .

`:evil-leader-for-mode CONS CELL'
    One or several cons cells (MODE . KEY) where MODE is a major-mode symbol
    and KEY is a key sequence string to be set with
    `dotemacs-set-leader-keys-for-major-mode'.

`:global-key STRING'
    One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'. "
  (let ((evil-leader (dotemacs-mplist-get props :evil-leader))
        (evil-leader-for-mode (dotemacs-mplist-get props :evil-leader-for-mode))
        (global-key (dotemacs-mplist-get props :global-key))
        (def-key (dotemacs-mplist-get props :define-key)))
    (append
     (when evil-leader
       `((dolist (key ',evil-leader)
           (dotemacs-set-leader-keys key ',func))))

     (when evil-leader-for-mode
       `((dolist (val ',evil-leader-for-mode)
           (dotemacs-set-leader-keys-for-major-mode
            (car val) (cdr val) ',func))))

     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))

     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defun dotemacs-view-org-file (file &optional anchor-text expand-scope)
  "Open the change log for the current version."
  (interactive)
  (find-file file)
  (org-indent-mode)
  (view-mode)
  (goto-char (point-min))

  (when anchor-text
    (re-search-forward anchor-text))
  (beginning-of-line)

  (cond
   ((eq expand-scope 'subtree)
    (show-subtree))
   ((eq expand-scope 'all)
    (show-all))
   (t nil))

  ;; Make ~SPC ,~ work, reference:
  ;; http://stackoverflow.com/questions/24169333/how-can-i-emphasize-or-verbatim-quote-a-comma-in-org-mode
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (setq-local org-emphasis-alist '(("*" bold)
                                   ("/" italic)
                                   ("_" underline)
                                   ("=" org-verbatim verbatim)
                                   ("~" org-kbd)
                                   ("+"
                                    (:strike-through t))))

  (setq-local org-hide-emphasis-markers t))

(defun dotemacs-get-package-directory (pkg)
  "Return the directory of PKG. Return nil if not found."
  (let ((elpa-dir (concat user-emacs-directory "elpa/")))
    (when (file-exists-p elpa-dir)
      (let ((dir (reduce (lambda (x y) (if x x y))
                         (mapcar (lambda (x)
                                   (when (string-match
                                          (concat "/"
                                                  (symbol-name pkg)
                                                  "-[0-9]+") x) x))
                                 (directory-files elpa-dir 'full))
                         :initial-value nil)))
        (when dir (file-name-as-directory dir))))))

(defun dotemacs-load-or-install-package (pkg &optional log file-to-load)
  "Load PKG package. PKG will be installed if it is not already installed.
Whenever the initial require fails the absolute path to the package
directory is returned.
If LOG is non-nil a message is displayed in spacemacs-mode buffer.
FILE-TO-LOAD is an explicit file to load after the installation."
  (let ((warning-minimum-level :error))
    (condition-case nil
        (require pkg)
      (error
       ;; not installed, we try to initialize package.el only if required to
       ;; precious seconds during boot time
       (require 'cl)
       (let ((pkg-elpa-dir (dotemacs-get-package-directory pkg)))
         (if pkg-elpa-dir
             (add-to-list 'load-path pkg-elpa-dir)
           ;; install the package
           (package-refresh-contents)
           (package-install pkg)
           (setq pkg-elpa-dir (dotemacs-get-package-directory pkg)))
         (require pkg nil 'noerror)
         (when file-to-load
           (load-file (concat pkg-elpa-dir file-to-load)))
         pkg-elpa-dir)))))

;; hide mode line
;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(defun dotemacs-recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch
Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

(defun dotemacs-recompile-user-home ()
  "Recompile packages in elpa user dir."
  (interactive)
  (byte-recompile-directory dotemacs-core-directory 0 t)
  (byte-recompile-directory dotemacs-evil-dir 0 t)
  (byte-recompile-directory dotemacs-modules-dir 0 t)
  (byte-recompile-directory dotemacs-user-settings-dir 0 t))

(defun dotemacs-register-repl (feature repl-func &optional tag)
  "Register REPL-FUNC to the global list of REPLs DOTEMACS-REPL-LIST.
FEATURE will be loaded before running the REPL, in case it is not already
loaded. If TAG is non-nil, it will be used as the string to show in the helm
buffer."
  (push `(,(or tag (symbol-name repl-func))
          . (,feature . ,repl-func))
        dotemacs-repl-list))

(defun dotemacs/defer-until-after-user-config (func)
  "Call FUNC if dotemacs/user-config has been called. Otherwise,
defer call using `dotemacs-post-user-config-hook'."
  (if dotemacs-post-user-config-hook-run
      (funcall func)
    (add-hook 'dotemacs-post-user-config-hook func)))

(defun dotemacs/setup-startup-hook ()
  "Add post init processing."
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (run-hooks 'dotemacs-post-user-config-hook)
     (setq dotemacs-post-user-config-hook-run t)
     (when (fboundp dotemacs-scratch-mode)
       (with-current-buffer "*scratch*"
         (funcall dotemacs-scratch-mode)))
     ;; from jwiegley
     ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
     (let ((elapsed (float-time
                     (time-subtract (current-time) emacs-start-time))))
       (dotemacs-buffer/append
        (format "\n[Emacs packages loaded in %.3fs]\n"
                elapsed))))))

(provide 'core-funcs)
;;; core-funcs.el ends here
