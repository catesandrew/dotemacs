;;; module-finance.el --- Finance Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(dotemacs-defvar-company-backends ledger-mode)

(use-package flycheck-ledger
  :ensure t
  :defer t)
; (with-eval-after-load 'flycheck
;   (require 'flycheck-ledger))

(use-package ledger-mode
  :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
  :ensure t
  :defer t
  :init
  (progn
    (setq ledger-post-amount-alignment-column 62)
    (push 'company-capf company-backends-ledger-mode)
    (dotemacs-set-leader-keys-for-major-mode 'ledger-mode
      "hd"   'ledger-delete-current-transaction
      "a"    'ledger-add-transaction
      "b"    'ledger-post-edit-amount
      "c"    'ledger-toggle-current
      "C"    'ledger-mode-clean-buffer
      "l"    'ledger-display-ledger-stats
      "p"    'ledger-display-balance-at-point
      "q"    'ledger-post-align-xact
      "r"    'ledger-reconcile
      "R"    'ledger-report
      "t"    'ledger-insert-effective-date
      "y"    'ledger-set-year
      "RET" 'ledger-set-month)
    (evilified-state-evilify ledger-report-mode ledger-report-mode-map)))

(when (eq dotemacs-completion-engine 'company)
  (dotemacs-use-package-add-hook company
    :post-init
    (progn
      (dotemacs-add-company-hook ledger-mode))))

(provide 'module-finance)
;;; module-finance.el ends here
