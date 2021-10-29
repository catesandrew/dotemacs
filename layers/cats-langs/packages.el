;;; packages.el --- cats-misc-langs: Layer packages

;;; Commentary:

;; Miscellaneous languages.

;;; Code:

(defconst cats-langs-packages
  '(
     exec-path-from-shell
     haskell-mode
     org
     feature-mode
     ;; thrift
     ))


(defun cats-langs/init-feature-mode ()
  (use-package feature-mode
    :defer t
    :mode (("\\.feature\\'" . feature-mode))
    :init (progn (setq feature-step-search-path "**/*steps.js"))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'feature-mode
        "s" 'feature-verify-scenario-at-pos
        "v" 'feature-verify-all-scenarios-in-buffer
        "f" 'feature-verify-all-scenarios-in-project
        "g" 'feature-goto-step-definition
        "." 'feature-goto-step-definition))))


;; org
(defun cats-langs/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (add-to-list 'org-babel-load-languages '(haskell . t))))


;; exec-path-from-shell
(defun cats-langs/post-init-exec-path-from-shell ()
  (dolist (var '(
                  "JAVA_OPTS"
                  "JAVA_HOME"
                  "JDK_HOME"
                  "GROOVY_HOME"
                  "PYENV_HOME"
                  "PYENV_ROOT"
                  "PYENV_SHELL"
                  "PYTHONPATH"
                  "RBENV_HOME"
                  "RBENV_ROOT"
                  "RBENV_SHELL"
                  ) exec-path-from-shell-variables)
    (unless (member var exec-path-from-shell-variables)
      (push var exec-path-from-shell-variables))))


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
