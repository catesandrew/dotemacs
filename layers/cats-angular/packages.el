;;; packages.el --- cats-angular: Layer packages

;;; Commentary:

;;; Code:

(defconst cats-angular-packages
  '(
     (ng2-mode :location local)
     add-node-modules-path
     company
     emmet-mode
     evil-matchit
     flycheck
     import-js
     js-doc
     prettier-js
     smartparens
     tern
     web-beautify
     yasnippet
     (treesit-fold :location
       (recipe :fetcher github
         :repo "abougouffa/treesit-fold"))
     ))


;; ng2-mode
(defun cats-angular/init-ng2-mode ()
  (use-package ng2-mode
    :defer t
    :commands (ng2-ts-mode ng2-html-mode)
    :init
    (add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-html-mode))
    (add-to-list 'magic-mode-alist (cons #'cats//typescript-ng2-file-p 'ng2-ts-mode))

    (when html-enable-lsp
      (add-hook 'ng2-html-mode-hook #'spacemacs//setup-lsp-for-html-buffer t))

    (with-eval-after-load 'lsp-mode
      (setq lsp-completion-provider :capf)
      (setq lsp-completion-show-detail t)
      (setq lsp-completion-show-kind t)
      ;; (setq lsp-enable-snippet t)
      ;; (setq lsp-enable-symbol-highlighting t)
      ;; (setq lsp-ui-sideline-enable t)
      )

    (with-eval-after-load 'color-identifiers-mode
      (dolist (maj-mode '(ng2-ts-mode))
        (add-to-list
          'color-identifiers:modes-alist
          `(,maj-mode . (""
                          "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                          (nil font-lock-variable-name-face tree-sitter-hl-face:variable))))))

    ;; setup angular backend
    (add-hook 'ng2-ts-mode-local-vars-hook #'cats//angular-setup-backend)
    ;; setup fmt on save
    (when typescript-fmt-on-save
      (add-hook 'ng2-ts-mode-local-vars-hook #'cats//angular-fmt-before-save-hook))

    ;; set the jtsx-typescript layers keymap as parent to the angular layers keymap
    (set-keymap-parent spacemacs-ng2-ts-mode-map spacemacs-jtsx-typescript-mode-map)

    (spacemacs/set-leader-keys-for-major-mode 'ng2-ts-mode
      "c" 'ng2-open-counterpart)
    (spacemacs/set-leader-keys-for-major-mode 'ng2-html-mode
      "c" 'ng2-open-counterpart)

    :config
    (with-eval-after-load 'ng2-ts-mode
      (define-key ng2-ts-mode-map (kbd "C-d") nil))))


;; etc

(defun cats-angular/pre-init-treesit-fold ()
  (spacemacs|use-package-add-hook treesit-fold
    :post-config
    (add-to-list 'treesit-fold-range-alist
      `(ng2-ts-mode . ,(treesit-fold-parsers-typescript)))
    (add-to-list 'treesit-fold-summary-parsers-alist
      '(ng2-ts-mode . treesit-fold-summary-javadoc))))

(defun cats-angular/post-init-add-node-modules-path ()
  (add-hook 'ng2-html-mode-hook #'add-node-modules-path)
  (add-hook 'ng2-ts-mode-hook #'add-node-modules-path))

(defun cats-angular/post-init-company ()
  (add-hook 'ng2-ts-mode-local-vars-hook #'cats//angular-setup-company))

(defun cats-angular/post-init-emmet-mode ()
  (spacemacs/add-to-hooks 'emmet-mode '(ng2-html-mode-hook))
  (add-hook 'ng2-ts-mode-hook 'cats/angular-emmet-mode))

(defun cats-angular/post-init-evil-matchit ()
  (evilmi-load-plugin-rules '(ng2-html-mode) '(simple template html))
  (add-hook 'ng2-html-mode-hook 'turn-on-evil-matchit-mode)
  (add-hook 'ng2-ts-mode-hook 'turn-on-evil-matchit-mode))

(defun cats-angular/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'ng2-ts-mode)))
  (dolist (mode '(ng2-ts-mode
                  ng2-html-mode))
    (spacemacs/enable-flycheck mode))
  (add-hook 'ng2-ts-mode-hook #'spacemacs//typescript-setup-checkers 'append))

(defun cats-angular/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'spacemacs--import-js-modes (cons 'ng2-ts-mode 'ng2-ts-mode-hook))))

(defun cats-angular/post-init-js-doc ()
  (dolist (hook '(ng2-ts-mode))
    (add-hook hook 'spacemacs/js-doc-require))

  (dolist (mode '(ng2-ts-mode))
    (spacemacs/declare-prefix-for-mode mode "mrd" "jsdoc")
    (spacemacs/js-doc-set-key-bindings mode)))

(defun cats-angular/pre-init-prettier-js ()
  (when (eq web-fmt-tool 'prettier)
    (dolist (mode '(ng2-html-mode))
      (add-to-list 'spacemacs--prettier-modes mode)))
  (when (eq typescript-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'ng2-ts-mode)))

(defun cats-angular/post-init-smartparens ()
  (add-hook 'ng2-html-mode-hook #'spacemacs//deactivate-smartparens)
  (add-hook 'ng2-ts-mode-hook #'spacemacs//activate-smartparens))

(defun cats-angular/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'ng2-ts-mode))

(defun cats-angular/pre-init-web-beautify ()
  (when (eq web-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes (cons 'ng2-html-mode 'web-beautify-html)))

  (when (eq typescript-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'ng2-ts-mode 'web-beautify-js))))

(defun cats-angular/post-init-yasnippet ()
  (add-hook 'ng2-ts-mode-hook #'spacemacs//react-setup-yasnippet))
