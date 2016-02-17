;;; module-cscope.el --- CScope Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'core-vars)
(require 'core-funcs)
(require 'core-use-package-ext)
(require 'core-keybindings)
(require 'core-auto-completion)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

(dotemacs-use-package-add-hook evil-jump
  :post-init
  )

(use-package xcscope
  :ensure t
  :commands (cscope-index-files cscope/run-pycscope)
  :init
  (progn
    ;; for python projects, we don't want xcscope to rebuild the databse,
    ;; because it uses cscope instead of pycscope
    (setq cscope-option-do-not-update-database t
          cscope-display-cscope-buffer nil)

    (defun cscope//safe-project-root ()
      "Return project's root, or nil if not in a project."
      (and (fboundp 'projectile-project-root)
           (projectile-project-p)
           (projectile-project-root)))

    (defun cscope/run-pycscope (directory)
      (interactive (list (file-name-as-directory
                          (read-directory-name "Run pycscope in directory: "
                                               (cscope//safe-project-root)))))
      (let ((default-directory directory))
        (shell-command
         (format "pycscope -R -f '%s'"
                 (expand-file-name "cscope.out" directory)))))))

(use-package helm-cscope
  :defer t
  :ensure t
  :init
  (defun dotemacs/setup-helm-cscope (mode)
    "Setup `helm-cscope' for MODE"
    (dotemacs-set-leader-keys-for-major-mode mode
      "gc" 'helm-cscope-find-called-function
      "gC" 'helm-cscope-find-calling-this-funtcion
      "gd" 'helm-cscope-find-global-definition
      "ge" 'helm-cscope-find-egrep-pattern
      "gf" 'helm-cscope-find-this-file
      "gF" 'helm-cscope-find-files-including-file
      "gr" 'helm-cscope-find-this-symbol
      "gx" 'helm-cscope-find-this-text-string)
    (defadvice helm-cscope-find-this-symbol (before cscope/goto activate)
      (evil-jumper--push))))

(provide 'module-cscope)
;;; module-cscope ends here
