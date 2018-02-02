;;; packages.el --- cats-emacs-lisp: Packages

;;; Commentary:

;; Personal Emacs Lisp layer.

;;; Code:

(defconst cats-emacs-lisp-packages
  '(
    flycheck
    flycheck-cask
    flycheck-package
    buttercup
    ))

(defun cats-emacs-lisp/post-init-flycheck ()
  (setq flycheck-emacs-lisp-load-path nil))

(defun cats-emacs-lisp/init-flycheck-cask ()
  (use-package flycheck-cask
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(defun cats-emacs-lisp/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :after flycheck
    :init (flycheck-package-setup)))

(defun cats-emacs-lisp/init-buttercup ()
  (use-package buttercup
    :defer t
    :init
    (defun cats-emacs-lisp/is-buttercup-buffer ()
      (and (buffer-file-name)
           (string-match-p (rx "/test-" (1+ (not (any "/"))) ".el" eos)
                           (buffer-file-name))))

    ;; Load buttercup automatically for proper indentation in specs
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (when (cats-emacs-lisp/is-buttercup-buffer)
                  (require 'buttercup))))))

;;; packages.el ends here
