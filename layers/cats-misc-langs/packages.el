;;; packages.el --- cats-misc-langs: Layer packages

;;; Commentary:

;; Miscellaneous languages.

;;; Code:

(defconst cats-misc-langs-packages
  '(
    thrift
    ))

(defun cats-misc-langs/init-thrift ()
  (use-package thrift
    :defer t
    :init (put 'thrift-indent-level 'safe-local-variable #'integerp)
    ;; Fake inheritance from prog mode
    :config (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook)))))

;;; packages.el ends here
