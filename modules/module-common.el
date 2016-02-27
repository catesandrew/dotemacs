;;; module-common.el --- Common functions and utilities

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:
;;
;; This can never require `module-utils` or `module-core`
;;
;;; Code:
(require 'module-vars)


;; funcs

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


;; eslint
(defvar dotemacs//executable-eslint nil)
(defvar dotemacs/eslint-executable-hook nil
  "Hooks run when dotemacs//executable-eslint is changed.")

(defun dotemacs/set-executable-eslint (eslint)
  "Set dotemacs//executable-eslint to ESLINT."
  (unless (string= eslint dotemacs//executable-eslint)
    (when dotemacs/verbose
      (message "eslint %s updated." eslint))
    (setq dotemacs//executable-eslint eslint)
    (ignore-errors (run-hooks 'dotemacs/eslint-executable-hook))))


;; jscs
(defvar dotemacs//executable-jscs nil)
(defvar dotemacs/jscs-executable-hook nil
  "Hooks run when dotemacs//executable-jscs is changed.")

(defun dotemacs/set-executable-jscs (jscs)
  "Set dotemacs//executable-jscs to JSCS."
  (unless (string= jscs dotemacs//executable-jscs)
    (when dotemacs/verbose
      (message "jscs %s updated." jscs))
    (setq dotemacs//executable-jscs jscs)
    (ignore-errors (run-hooks 'dotemacs/jscs-executable-hook))))


;; jshint
(defvar dotemacs//executable-jshint nil)
(defvar dotemacs/jshint-executable-hook nil
  "Hooks run when dotemacs//executable-jshint is changed.")

(defun dotemacs/set-executable-jshint (jshint)
  "Set dotemacs//executable-jshint to JSHINT."
  (unless (string= jshint dotemacs//executable-jshint)
    (when dotemacs/verbose
      (message "jshint %s updated." jshint))
    (setq dotemacs//executable-jshint jshint)
    (ignore-errors (run-hooks 'dotemacs/jshint-executable-hook))))


;; tidy5
(defvar dotemacs//executable-tidy nil)
(defvar dotemacs/tidy-executable-hook nil
  "Hooks run when dotemacs//executable-tidy is changed.")

(defun dotemacs/set-executable-tidy (tidy)
  "Set dotemacs//executable-tidy to TIDY."
  (unless (string= tidy dotemacs//executable-tidy)
    (when dotemacs/verbose
      (message "tidy %s updated." tidy))
    (setq dotemacs//executable-tidy tidy)
    (ignore-errors (run-hooks 'dotemacs/tidy-executable-hook))))


;; mocha
(defvar dotemacs//executable-mocha nil)
(defvar dotemacs/mocha-executable-hook nil
  "Hooks run when dotemacs//executable-mocha is changed.")

(defun dotemacs/set-executable-mocha (mocha)
  "Set dotemacs//executable-mocha to MOCHA."
  (unless (string= mocha dotemacs//executable-mocha)
    (when dotemacs/verbose
      (message "mocha %s updated." mocha))
    (setq dotemacs//executable-mocha mocha)
    (ignore-errors (run-hooks 'dotemacs/mocha-executable-hook))))


;; csslint


;; stylus


;; handlebars


;; after emacs startup

(dotemacs/defer-until-after-user-config
 '(lambda ()
    (setq user-full-name    "Andrew Cates"
          user-mail-address "catesandrew@gmail.com")

    (when-let (tidy5 (executable-find "tidy5"))
      (dotemacs/set-executable-tidy tidy5))

    ;; (dotemacs-define-custom-layout "NixOS Configuration"
    ;;   :binding "N"
    ;;   :body
    ;;   (dired "~/Projects/nixpkgs/pkgs/")
    ;;   (split-window-right)
    ;;   (find-file "~/nixos-config/common/desktop.nix"))

    ;; (dotemacs-define-custom-layout "Blog"
    ;;   :binding "b"
    ;;   :body
    ;;   (when (y-or-n-p "Hi, do you want to create a new post?")
    ;;     (call-interactively 'op/new-post)))

    ;; (dotemacs-define-custom-layout "@bspwm"
    ;;   :binding "B"
    ;;   :body
    ;;   (find-file "~/dotbspwm/.config/sxhkd/sxhkdrc")
    ;;   (split-window-right-and-focus)
    ;;   (find-file "~/dotbspwm/.config/bspwm/bspwmrc")
    ;;   (split-window-below-and-focus)
    ;;   (find-file "~/dotbspwm/.config/bspwm/autostart"))
    ))

(provide 'module-common)
;;; module-common.el ends here
