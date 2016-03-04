;;; module-shell-script.el --- Shell Script Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'core-use-package-ext)
(require 'core-fonts-support)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; variables
(dotemacs-defvar-company-backends sh-mode)
(dotemacs-defvar-company-backends fish-mode)

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'sh-mode))

(use-package sh-script                  ; Shell scripts
  :defer t
  :init
  (progn
    ;; Use two spaces in shell scripts.
    (setq sh-indentation 2
          sh-basic-offset 2)

    (dotemacs-set-leader-keys-for-major-mode 'sh-mode
      "\\" 'sh-backslash-region)

    ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
    (dolist (pattern '("\\.zsh\\'"
                       "zlogin\\'"
                       "zlogout\\'"
                       "zpreztorc\\'"
                       "zprofile\\'"
                       "zshenv\\'"
                       "zshrc\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

    (defun dotemacs//setup-shell ()
      (when (and buffer-file-name
                 (string-match-p "\\.zsh\\'" buffer-file-name))
        (sh-set-shell "zsh")))
    (add-hook 'sh-mode-hook 'dotemacs//setup-shell)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook sh-mode)
      (dotemacs-add-company-hook fish-mode)))

  (use-package company-shell
    :ensure t
    :defer t
    :init
    (progn
      (push 'company-tern company-backends-sh-mode)
      (push 'company-tern company-backends-fish-mode))))

(provide 'module-shell-script)
;;; module-shell-script.el ends here
