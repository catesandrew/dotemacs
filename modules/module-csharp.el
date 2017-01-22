;;; module-java.el --- Java Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
(require 'core-use-package-ext)
(require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'core-auto-completion)
;; (require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

;; config

(dotemacs-defvar-company-backends csharp-mode)

;; Command prefixes
(setq csharp/key-binding-prefixes '(("mc" . "csharp/compile")
                              ("mf" . "csharp/file")
                              ("mg" . "csharp/navigation")
                              ("mh" . "csharp/documentation")
                              ("mr" . "csharp/refactoring")
                              ("ms" . "csharp/server")
                              ("mt" . "csharp/tests")))

;; Load omnisharp-mode with csharp-mode, this should start the omnisharp server automatically
(add-hook 'csharp-mode-hook 'omnisharp-mode)

(use-package omnisharp
  :defer t
  :ensure omnisharp
  ;; Load omnisharp-mode with csharp-mode, this should start the omnisharp server automatically
  :init
  (progn
    (setq omnisharp-server-executable-path "/usr/local/src/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
    (when (eq dotemacs-completion-engine 'company)
      ;; needed to avoid an error when fetching doc using company
      ;; Note: if you are using a roslyn based omnisharp server you can
      ;; set back this variable to t.
      (setq omnisharp-auto-complete-want-documentation nil))
    (push 'company-omnisharp company-backends-csharp-mode))
  :config
  (progn
    (mapc (lambda(x) (dotemacs-declare-prefix-for-mode
                       'csharp-mode (car x) (cdr x)))
          csharp/key-binding-prefixes)

    (dotemacs-set-leader-keys-for-major-mode 'csharp-mode
      ;; Compile
      "cc" 'omnisharp-build-in-emacs ;; Only one compile command so use top-level
      ;; Solution/project manipulation
      "fa" 'omnisharp-add-to-solution-current-file
      "fA" 'omnisharp-add-to-solution-dired-selected-files
      "fr" 'omnisharp-remove-from-project-current-file
      "fR" 'omnisharp-remove-from-project-dired-selected-files
      "pl" 'omnisharp-add-reference
      ;; Navigation
      "gg"   'omnisharp-go-to-definition
      "gG"   'omnisharp-go-to-definition-other-window
      "gu"   'omnisharp-helm-find-usages
      "gU"   'omnisharp-find-usages-with-ido
      "gs"   'omnisharp-helm-find-symbols
      "gi"   'omnisharp-find-implementations
      "gI"   'omnisharp-find-implementations-with-ido
      "gr"   'omnisharp-navigate-to-region
      "gm"   'omnisharp-navigate-to-solution-member
      "gM"   'omnisharp-navigate-to-solution-member-other-window
      "gf"   'omnisharp-navigate-to-solution-file
      "gF"   'omnisharp-navigate-to-solution-file-then-file-member
      "gc"   'omnisharp-navigate-to-current-file-member
      ;; Help, documentation, info
      "ht" 'omnisharp-current-type-information
      "hT" 'omnisharp-current-type-information-to-kill-ring
      ;; Refactoring
      "rm" 'omnisharp-rename
      "rM" 'omnisharp-rename-interactively
      "rr" 'omnisharp-run-code-action-refactoring
      ;; Server manipulation, inspired spacemacs REPL bindings since C# does not provice a REPL
      "ss" 'omnisharp-start-omnisharp-server
      "sS" 'omnisharp-stop-server
      "sr" 'omnisharp-reload-solution
      ;; Tests
      "ta" 'omnisharp-unit-test-all
      "tb" 'omnisharp-unit-test-fixture
      "tt" 'omnisharp-unit-test-single
      ;; Code manipulation
      "u" 'omnisharp-auto-complete-overrides
      "i" 'omnisharp-fix-usings
      "=" 'omnisharp-code-format)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook csharp-mode))))

(use-package csharp-mode
  :ensure t
  :defer t
  :init
  (progn
    (setq sh-indentation 2
          sh-basic-offset 2)))

(provide 'module-csharp)
;;; module-csharp.el ends here
