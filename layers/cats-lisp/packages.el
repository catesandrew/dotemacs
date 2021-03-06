;;; packages.el --- cats-lisp: Packages

;;; Commentary:

;; Personal Emacs Lisp layer.

;;; Code:

(defconst cats-lisp-packages
  '(
     buttercup
     flycheck
     flycheck-cask
     ;; flycheck-package
     org
     ))


;; org
(defun cats-lisp/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (add-to-list 'org-babel-load-languages '(lisp . t))))


;; flycheck
(defun cats-lisp/post-init-flycheck ()
  (setq flycheck-emacs-lisp-load-path nil))

(defun cats-lisp/init-flycheck-cask ()
  (use-package flycheck-cask
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(defun cats-lisp/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :after flycheck
    :init (flycheck-package-setup)))

(defun cats-lisp/init-buttercup ()
  (use-package buttercup
    :defer t
    :init
    (defun cats-lisp/is-buttercup-buffer ()
      (and (buffer-file-name)
           (string-match-p (rx "/test-" (1+ (not (any "/"))) ".el" eos)
                           (buffer-file-name))))

    ;; Load buttercup automatically for proper indentation in specs
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (when (cats-lisp/is-buttercup-buffer)
                  (require 'buttercup))))))

;;; packages.el ends here
