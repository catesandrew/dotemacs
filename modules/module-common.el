;;; module-common.el --- Common functions and utilities

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:
;;
;; This can never require `module-utils` or `module-core`

(require 'module-vars)

;;; Code:

(defun current//buffer-remote-p ()
  (--any? (and it (file-remote-p it))
          (list
           (buffer-file-name)
           list-buffers-directory
           default-directory)))

(defun flycheck//turn-on-maybe ()
  (unless (or buffer-read-only
              (hardhat-buffer-included-p (current-buffer))
              (current//buffer-remote-p))
    (flycheck-mode)))

(defun dotemacs/add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun dotemacs/add-all-to-hook (hook &rest funs)
  "Add functions to hook."
  (dotemacs/add-to-hook hook funs))

(defun dotemacs/add-to-hook (hook funs)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun dotemacs-eslint-set-local-eslint-from-projectile ()
  "Use local eslint CLI from `./node_modules` if available."
  (when-let (eslint (executable-find "eslint"))
            (setq flycheck-javascript-eslint-executable eslint)
            (setq dotemacs//flycheck-executable-eslint eslint)))

(defun dotemacs-flycheck-executables-search ()
  "Lazy locate javascript executables."
  (unless dotemacs//flycheck-executables-searched
    (when-let (eslint (executable-find "eslint"))
              (setq flycheck-javascript-eslint-executable eslint)
              (setq dotemacs//flycheck-executable-eslint eslint))
    (when-let (jscs (executable-find "jscs"))
              (setq flycheck-javascript-jscs-executable jscs)
              (setq dotemacs//flycheck-executable-jscs jscs))
    (when-let (jshint (executable-find "jshint"))
              (setq flycheck-javascript-jshint-executable jshint)
              (setq dotemacs//flycheck-executable-jshint jshint))
    (when-let (tidy5 (executable-find "tidy5"))
              (setq flycheck-html-tidy-executable tidy5)
              (setq dotemacs//flycheck-executable-tidy5 tidy5))
    (setq dotemacs//flycheck-executables-searched t)))

(defun dotemacs//flycheck-disable (checker)
  (interactive)
  (add-to-list 'flycheck-disabled-checkers checker))

(defun dotemacs//flycheck-enable (checker)
  (interactive)
  (setq flycheck-disabled-checkers (remove checker flycheck-disabled-checkers)))

(defun dotemacs-flycheck-executables-updated ()
  (when (bound-and-true-p dotemacs//flycheck-executables-searched)
    (when dotemacs//flycheck-executable-eslint
      (dotemacs-set-leader-keys
        "tee" 'flycheck-eslint-enable
        "teE" 'flycheck-eslint-disable))

    (when dotemacs//flycheck-executable-jscs
      (dotemacs-set-leader-keys
        "tec" 'flycheck-jscs-enable
        "teC" 'flycheck-jscs-disable))

    (when dotemacs//flycheck-executable-jshint
      (dotemacs-set-leader-keys
        "teh" 'flycheck-jshint-enable
        "teH" 'flycheck-jshint-disable))

    ;; (when (equal major-mode 'js2-mode)
    ;;   (dotemacs-flycheck-init-javascript))
    ))

(defun dotemacs-flycheck-init-react ()
  "Init flycheck settings for react-mode."
  (when (bound-and-true-p dotemacs//flycheck-executables-searched)
    (if dotemacs//flycheck-executable-eslint
        (progn
          (dotemacs//flycheck-enable 'javascript-eslint)
          ;; disable jshint since we prefer eslint checking
          (when dotemacs//flycheck-executable-jshint
            (dotemacs//flycheck-disable 'javascript-jshint)))
      (progn
        ;; otherwise enable jshint if eslint is not found
        (when dotemacs//flycheck-executable-jshint
          (dotemacs//flycheck-enable 'javascript-jshint))))

    ;; disable html-tidy
    (when dotemacs//flycheck-executable-tidy5
      (dotemacs//flycheck-disable 'html-tidy))

    ;; disable jscs
    (when dotemacs//flycheck-executable-jscs
      (dotemacs//flycheck-disable 'javascript-jscs))

    ;; disable json-jsonlist checking for json files
    (dotemacs//flycheck-disable 'json-jsonlist)))

(defun dotemacs-flycheck-init-javascript ()
  "Use flycheck settings for a js2-mode."
  (if dotemacs//flycheck-executable-eslint
      (progn
        (dotemacs//flycheck-enable 'javascript-eslint)
        ;; disable jshint since we prefer eslint checking
        (when dotemacs//flycheck-executable-jshint
          (dotemacs//flycheck-disable 'javascript-jshint)))
    (progn
      ;; otherwise enable jshint if eslint is not found
      (when dotemacs//flycheck-executable-jshint
        (dotemacs//flycheck-enable 'javascript-jshint))))

  ;; disable jscs
  (when dotemacs//flycheck-executable-jscs
    (dotemacs//flycheck-disable 'javascript-jscs)))

(defun dotemacs/user-full-name ()
  "Guess the user's full name. Returns nil if no likely name could be found."
  (or (replace-regexp-in-string
       "\n$" "" (shell-command-to-string "git config --get user.name"))
      (user-full-name)
      (getenv "USER")))

(defun dotemacs/user-email ()
  "Guess the user's email address. Returns nil if none could be found."
  (or (replace-regexp-in-string
       "\n$" "" (shell-command-to-string "git config --get user.email"))
      user-mail-address
      (getenv "EMAIL")))
(provide 'module-common)
;;; module-common.el ends here
