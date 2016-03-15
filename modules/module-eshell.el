;;; module-eshell.el --- EShell Module
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
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-use-package-ext)
(require 'core-auto-completion)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'comint)


;; variables
(defvar shell-enable-smart-eshell nil
  "If non-nil then `em-smart' is enabled. `em-smart' allows to quickly review
commands, modify old commands or enter a new one.")

(defvar shell-protect-eshell-prompt t
  "If non-nil then eshell's prompt is protected. This means that
movement to the prompt is inhibited like for `comint-mode'
prompts and the prompt is made read-only")

;; Terminal emulation and shells
(dotemacs-defvar-company-backends eshell-mode)


;; packages
(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook eshell
    :post-init
    (progn
      (push 'company-capf company-backends-eshell-mode)
      (dotemacs-add-company-hook eshell-mode))
    :post-config
    (progn
      (defun dotemacs//toggle-shell-auto-completion-based-on-path ()
        "Deactivates automatic completion on remote paths.
Retrieving completions for Eshell blocks Emacs. Over remote
connections the delay is often annoying, so it's better to let
the user activate the completion manually."
        (if (file-remote-p default-directory)
            (setq-local company-idle-delay nil)
          (setq-local company-idle-delay 0.2)))
      (add-hook 'eshell-directory-change-hook
                'dotemacs//toggle-shell-auto-completion-based-on-path)

      ;; The default frontend screws everything up in short windows like
      ;; terminal often are
      (defun dotemacs//eshell-switch-company-frontend ()
        "Sets the company frontend to `company-preview-frontend' in e-shell mode."
        (setq-local company-frontends '(company-preview-frontend)))
      (add-hook 'eshell-mode-hook
         'dotemacs//eshell-switch-company-frontend))))

(use-package esh-help
  :defer t
  :ensure t
  :init (add-hook 'eshell-mode-hook 'eldoc-mode)
  :config (setup-esh-help-eldoc))

(use-package eshell
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-register-repl 'eshell 'eshell)
    (setq eshell-cmpl-cycle-completions nil
          ;; auto truncate after 20k lines
          eshell-buffer-maximum-lines 20000
          ;; history size
          eshell-history-size 350
          ;; no duplicates in history
          eshell-hist-ignoredups t
          ;; buffer shorthand -> echo foo > #'buffer
          eshell-buffer-shorthand t
          ;; my prompt is easy enough to see
          eshell-highlight-prompt nil
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t
          ;; cache directory
          eshell-directory-name (concat dotemacs-cache-directory "eshell/"))

    (defun dotemacs//eshell-auto-end ()
      "Move point to end of current prompt when switching to insert state."
      (when (and (eq major-mode 'eshell-mode)
                 ;; Not on last line, we might want to edit within it.
                 (not (eq (line-end-position) (point-max))))
        (end-of-buffer)))

    (when shell-protect-eshell-prompt
      (defun dotemacs//protect-eshell-prompt ()
        "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
        (let ((inhibit-field-text-motion t))
          (add-text-properties
           (point-at-bol)
           (point)
           '(rear-nonsticky t
             inhibit-line-move-field-capture t
             field output
             read-only t
             front-sticky (field inhibit-line-move-field-capture)))))
      (add-hook 'eshell-after-prompt-hook 'dotemacs//protect-eshell-prompt))

    (defun dotemacs//init-eshell ()
      "Stuff to do when enabling eshell."
      (setq pcomplete-cycle-completions nil)
      ;; Disable case insensitivity for filename autocompletion in shell-mode
      (setq pcomplete-ignore-case t) ;; Controls case sensitivity for pcomplete
      (if (bound-and-true-p linum-mode) (linum-mode -1))
      (unless shell-enable-smart-eshell
        ;; we don't want auto-jump to prompt when smart eshell is enabled.
        ;; Idea: maybe we could make auto-jump smarter and jump only if the
        ;; point is not on a prompt line
        (add-hook 'evil-insert-state-entry-hook
                  'dotemacs//eshell-auto-end nil t))

      (semantic-mode -1)

      ;; Caution! this will erase buffer's content at C-l
      (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
      (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof))

    (autoload 'eshell-delchar-or-maybe-eof "em-rebind")

    ;; Defining a function like this makes it possible to type 'clear' in eshell and
    ;; have it work
    (defun eshell/clear ()
      "Clear contents in eshell."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (eshell-send-input))

    (add-hook 'eshell-mode-hook 'dotemacs//init-eshell)
    (add-hook 'eshell-mode-hook 'dotemacs/disable-hl-line-mode))
  :config
  (progn
    (require 'esh-opt)

    ;; quick commands
    (defalias 'eshell/e 'find-file-other-window)
    (defalias 'eshell/d 'dired)
    (setenv "PAGER" "cat")

    ;; support `em-smart'
    (when shell-enable-smart-eshell
      (require 'em-smart)
      (setq eshell-where-to-jump 'begin
            eshell-review-quick-commands nil
            eshell-smart-space-goes-to-end t)
      (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

    ;; Visual commands
    (require 'em-term)
    (mapc (lambda (x) (push x eshell-visual-commands))
          '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

    ;; automatically truncate buffer after output
    (when (boundp 'eshell-output-filter-functions)
      (push 'eshell-truncate-buffer eshell-output-filter-functions))

    ;; These don't work well in normal state due to evil/emacs cursor
    ;; incompatibility
    (evil-define-key 'insert eshell-mode-map
      (kbd "C-k") 'eshell-previous-matching-input-from-input
      (kbd "C-j") 'eshell-next-matching-input-from-input)))

(use-package eshell-prompt-extras
  :ensure t
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-z
  :defer t
  :ensure t
  :init
  (with-eval-after-load 'eshell
    (require 'eshell-z)))

(dotemacs-use-package-add-hook helm
  :post-init
  (progn
    ;; eshell
    (defun dotemacs/helm-eshell-history ()
      "Correctly revert to insert state after selection."
      (interactive)
      (helm-eshell-history)
      (dotemacs/normal-to-insert-state))
    (defun dotemacs/helm-shell-history ()
      "Correctly revert to insert state after selection."
      (interactive)
      (helm-comint-input-ring)
      (dotemacs/normal-to-insert-state))
    (defun dotemacs/init-helm-eshell ()
      "Initialize helm-eshell."
      ;; this is buggy for now
      ;; (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete)
      (dotemacs-set-leader-keys-for-major-mode 'eshell-mode
        "H" 'dotemacs/helm-eshell-history)
      (define-key eshell-mode-map
        (kbd "M-l") 'dotemacs/helm-eshell-history))
    (add-hook 'eshell-mode-hook 'dotemacs/init-helm-eshell)
    ;;shell
    (dotemacs-set-leader-keys-for-major-mode 'shell-mode
      "H" 'dotemacs/helm-shell-history)))

(provide 'module-eshell)
;;; module-eshell.el ends here
