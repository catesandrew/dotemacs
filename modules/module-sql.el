;;; module-sql.el --- SQL Module
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

(use-package sql
  :defer t
  :ensure t
  :init (dotemacs-register-repl 'sql 'dotemacs/sql-start "sql")
  :config
  (progn
    (dotemacs-load-private-file "sql-connections" 'noerror)
    (add-to-list 'display-buffer-alist
               `(,(rx bos "*SQL")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window
                  (side            . bottom)
                  (reusable-frames . visible)
                  (window-height   . 0.4))))

    (setq dotemacs-sql-highlightable sql-product-alist
          dotemacs-sql-startable (remove-if-not
                              (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                              sql-product-alist)

          ;; should not set this to anything else than nil
          ;; the focus of SQLi is handled by dotemacs conventions
          sql-pop-to-buffer-after-send-region nil)

    (defun dotemacs//sql-source (products)
      "return a source for helm selection"
      `((name . "SQL Products")
        (candidates . ,(mapcar (lambda (product)
                                 (cons (sql-get-product-feature (car product) :name)
                                       (car product)))
                               products))
        (action . (lambda (candidate) (helm-marked-candidates)))))

    (defun dotemacs/sql-highlight ()
      "set SQL dialect-specific highlighting"
      (interactive)
      (let ((product (car (helm
                           :sources (list (dotemacs//sql-source dotemacs-sql-highlightable))))))
        (sql-set-product product)))

    (defun dotemacs/sql-start ()
      "set SQL dialect-specific highlighting and start inferior SQLi process"
      (interactive)
      (let ((product (car (helm
                           :sources (list (dotemacs//sql-source dotemacs-sql-startable))))))
        (sql-set-product product)
        (sql-product-interactive product)))

    (defun dotemacs/sql-send-string-and-focus ()
      "Send a string to SQLi and switch to SQLi in `insert state'."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region t))
        (call-interactively 'sql-send-string)
        (dotemacs/normal-to-insert-state)))

    (defun dotemacs/sql-send-buffer-and-focus ()
      "Send the buffer to SQLi and switch to SQLi in `insert state'."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region t))
        (sql-send-buffer)
        (dotemacs/normal-to-insert-state)))

    (defun dotemacs/sql-send-paragraph-and-focus ()
      "Send the paragraph to SQLi and switch to SQLi in `insert state'."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region t))
        (sql-send-paragraph)
        (dotemacs/normal-to-insert-state)))

    (defun dotemacs/sql-send-region-and-focus (start end)
      "Send region to SQLi and switch to SQLi in `insert state'."
      (interactive "r")
      (let ((sql-pop-to-buffer-after-send-region t))
        (sql-send-region start end)
        (dotemacs/normal-to-insert-state)))

    (dotemacs-set-leader-keys-for-major-mode 'sql-mode
      "'" 'dotemacs/sql-start

      ;; sqli buffer
      "bb" 'sql-show-sqli-buffer
      "bs" 'sql-set-sqli-buffer

      ;; dialects
      "hk" 'dotemacs/sql-highlight

      ;; interactivity
      "sb" 'sql-send-buffer
      "sB" 'dotemacs/sql-send-buffer-and-focus
      "si" 'dotemacs/sql-start
      ;; paragraph gets "f" here because they can be assimilated to functions.
      ;; If you separate your commands in a SQL file, this key will send the
      ;; command under the point, which is what you probably want.
      "sf" 'sql-send-paragraph
      "sF" 'dotemacs/sql-send-paragraph-and-focus
      "sq" 'sql-send-string
      "sQ" 'dotemacs/sql-send-string-and-focus
      "sr" 'sql-send-region
      "sR" 'dotemacs/sql-send-region-and-focus

      ;; listing
      "la" 'sql-list-all
      "lt" 'sql-list-table)

    (dotemacs-set-leader-keys-for-major-mode 'sql-interactive-mode
      ;; sqli buffer
      "br" 'sql-rename-buffer
      "bS" 'sql-save-connection)

    (add-hook 'sql-interactive-mode-hook
              (lambda () (toggle-truncate-lines t)))))

(use-package sql-indent
  :ensure t
  :defer t)

(provide 'module-sql)
;;; module-sql.el ends here
