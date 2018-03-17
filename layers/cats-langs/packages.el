;;; packages.el --- cats-misc-langs: Layer packages

;;; Commentary:

;; Miscellaneous languages.

;;; Code:

(defconst cats-langs-packages
  '(
     exec-path-from-shell
     haskell-mode
     org
     thrift
     ))


;; org
(defun cats-langs/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (add-to-list 'org-babel-load-languages '(haskell . t))))


;; exec-path-from-shell
(defun cats-langs/pre-init-exec-path-from-shell ()
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
(defun cats-langs/init-thrift ()
  (use-package thrift
    :defer t
    :init (put 'thrift-indent-level 'safe-local-variable #'integerp)
    ;; Fake inheritance from prog mode
    :config (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook)))))


;; haskell-mode
(defun cats//init-haskell ()
  "Initialize the haskell mode."
  (setq-local rebox-style-loop '(725)))

(defun cats-langs/pre-init-haskell-mode ()
  (spacemacs|use-package-add-hook haskell-mode
    :pre-init
    (progn
      ;; (with-eval-after-load 'rebox2
      ;;   (setq-local rrebox-language-character-alist
      ;;     (append rebox-language-character-alist
      ;;       '(7 . "-"))))
      ;; (add-hook 'haskell-mode-hook 'cats//init-haskell)
      )
    :post-config
    (rebox-register-all-templates)))

;;; packages.el ends here
