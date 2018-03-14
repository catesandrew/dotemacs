;;; packages.el --- cats-misc-langs: Layer packages

;;; Commentary:

;; Miscellaneous languages.

;;; Code:

(defconst cats-misc-langs-packages
  '(
    thrift
     exec-path-from-shell
    ))


;; exec-path-from-shell
(defun cats-misc-langs/pre-init-exec-path-from-shell ()
  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-config
    (dolist (var '(
                    "JAVA_OPTS"
                    "PYENV_HOME"
                    "PYENV_ROOT"
                    "PYENV_SHELL"
                    "PYTHONPATH"
                    "RBENV_HOME"
                    "RBENV_ROOT"
                    "RBENV_SHELL"
                    ) exec-path-from-shell-variables)
      (unless (or (member var exec-path-from-shell-variables) (getenv var))
        (push var exec-path-from-shell-variables)))))


;; thrift

(defun cats-misc-langs/init-thrift ()
  (use-package thrift
    :defer t
    :init (put 'thrift-indent-level 'safe-local-variable #'integerp)
    ;; Fake inheritance from prog mode
    :config (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook)))))

;;; packages.el ends here
