;;; packages.el --- cats: Programming

;;; Commentary:

;;; Code:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-programming-packages
  '((auto-fill-comments-mode :location local)
     clean-aindent-mode
     (compile :location built-in)
     evil-string-inflection
     ;; (hs-minor-mode :location built-in)
     (prog-mode :location built-in)
     string-inflection
     ycmd
    ))

(defun cats-programming/pre-init-ycmd ()
  (spacemacs|use-package-add-hook ycmd
    :post-init
    (setq ycmd-server-command cats/ycmd-server-command)))

(defun cats-programming/post-init-compile ()
  "Colorize output of compilation mode."
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t))

(defun cats-programming/init-hs-minor-mode ()
  "Visit http://stackoverflow.com/questions/1085170 for more info."
  (use-package hs-minor-mode
    :init
    (progn
      ;; Space to toggle folds.
      ;; (define-key evil-motion-state-map (kbd "SPC") 'evil-toggle-fold)

      ;; required for evil folding
      (setq hs-set-up-overlay 'cats/fold-overlay))))

(defun cats-programming/init-auto-fill-comments-mode ()
  "Break line beyond `current-fill-column` in comments only, while editing."
  (use-package auto-fill-comments-mode
    :commands auto-fill-comments-mode
    :defer t
    :init
    (progn
      (spacemacs|add-toggle auto-fill-comments-mode
        :status auto-fill-comments-mode
        :on (progn
              (when (bound-and-true-p auto-fill-comments-mode)
                (auto-fill-comments-mode -1))
              (auto-fill-comments-mode))
        :off (auto-fill-comments-mode -1)
        :documentation "Reflow long comments."
        :evil-leader "toc"))
    :config (spacemacs|hide-lighter auto-fill-comments-mode)))

(defun cats-programming/init-prog-mode ()
  "Add programming mode hooks."
  (use-package prog-mode
    :init
    (progn
      ;; bootstrap with our defaults
      (add-hook 'cats/prog-mode-hook 'cats/prog-mode-defaults)
      ;; run our cats/prog-mod-hooks with prog-mode
      (add-hook 'prog-mode-hook (lambda () (run-hooks #'cats/prog-mode-hook))))))

(defun cats-programming/pre-init-clean-aindent-mode ()
  "Keep track of the last auto-indent operation and trims down white space."
  (spacemacs|use-package-add-hook clean-aindent-mode
    :post-init
    (add-hook 'prog-mode-hook 'clean-aindent-mode)))

(defun cats-programming/pre-init-string-inflection ()
  "String inflections for underscore -> UPCASE -> CamelCase conversion of names."
  (spacemacs|use-package-add-hook string-inflection
    :post-init
    (progn
      (spacemacs/declare-prefix "xia" "auto")

      (spacemacs/declare-prefix "xi_" "emacs_lisp")
      (spacemacs/declare-prefix "xiu" "emacs_lisp")
      (spacemacs/declare-prefix "xiC" "EmacsLisp")
      (spacemacs/declare-prefix "xic" "emacsLisp")
      (spacemacs/declare-prefix "xiU" "EMACS_LISP")
      (spacemacs/declare-prefix "xik" "emacs-lisp")
      (spacemacs/declare-prefix "xi-" "emacs-lisp"))))

(defun cats-programming/init-evil-string-inflection ()
  "String inflections for underscore -> UPCASE -> CamelCase conversion of names."
  (use-package evil-string-inflection
    :defer t
    :ensure t
    :init
    (progn
      (define-key evil-normal-state-map "gR" 'evil-operator-string-inflection))))

;;; packages.el ends here
