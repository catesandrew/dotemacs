;;; module-rust.el --- Rust Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'core-use-package-ext)
(require 'core-auto-completion)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; config

;; Define the buffer local company backend variable
(dotemacs-defvar-company-backends rust-mode)

;; funcs

;; http://doc.crates.io/guide.html
(defun dotemacs/rust-cargo-build ()
  (interactive)
  (compile "cargo build"))

(defun dotemacs/rust-cargo-run ()
  (interactive)
  (compile "cargo run"))

(defun dotemacs/rust-cargo-test ()
  (interactive)
  (compile "cargo test"))

(defun dotemacs/rust-cargo-doc ()
  (interactive)
  (compile "cargo doc"))

(defun dotemacs/rust-cargo-clean ()
  (interactive)
  (compile "cargo clean"))

;; packages

(dotemacs-use-package-add-hook flycheck
  :post-init
  (dotemacs/add-flycheck-hook 'rust-mode))

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode                  ; Rust
  :ensure t
  :defer t
  :config
  (progn
    (dotemacs-declare-prefix-for-mode 'rust-mode "mc" "cargo")
    (dotemacs-set-leader-keys-for-major-mode 'rust-mode
      "="  'rust-format-buffer
      "cc" 'dotemacs/rust-cargo-build
      "ct" 'dotemacs/rust-cargo-test
      "cd" 'dotemacs/rust-cargo-doc
      "cx" 'dotemacs/rust-cargo-run
      "cC" 'dotemacs/rust-cargo-clean)))

(use-package toml-mode
  :ensure t
  :defer t)

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (push 'company-capf company-backends-rust-mode)
      (dotemacs-add-company-hook rust-mode)
      (add-hook 'rust-mode-hook
                (lambda ()
                  (setq-local company-tooltip-align-annotations t))))))

(dotemacs-use-package-add-hook smartparens
  :post-init
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(use-package racer
  :ensure t
  :defer t
  :init
  (progn
    (dotemacs/add-to-hook 'rust-mode-hook '(racer-mode eldoc-mode))
    (dotemacs-declare-prefix-for-mode 'rust-mode "mg" "goto")
    (dotemacs-set-leader-keys-for-major-mode 'rust-mode
      "gg" 'racer-find-definition)))

(provide 'module-rust)
;;; module-rust.el ends here
