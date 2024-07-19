;;; packages.el --- cats-angular: Layer packages

;;; Commentary:

;;; Code:

(defconst cats-angular-packages
  '(
     (ng2-mode :location local)
     add-node-modules-path
     company
     eldoc
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
    (cats/angular-safe-local-variables '(lsp tide))
    (cats/angular-mode-init 'ng2-ts-mode-local-vars-hook)

    ;; Check if .html is in auto-mode-alist and remove it if it is
    (setq auto-mode-alist
      (remove (assoc "\\.html\\'" auto-mode-alist) auto-mode-alist))
    ;; Re-add the general .html pattern to auto-mode-alist for web-mode
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    ;; Add *.component.html to auto-mode-alist
    (add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . ng2-html-mode))

    (add-to-list 'magic-mode-alist (cons #'cats//typescript-ng2-file-p 'ng2-ts-mode))

    ;; todo configure this better later
    ;; (when angular-html-enable-lsp
    ;;   (add-hook 'ng2-html-mode-hook #'spacemacs//setup-lsp-for-html-buffer t))

    (with-eval-after-load 'lsp-mode
      (setq lsp-completion-provider :capf)
      (setq lsp-completion-show-detail t)
      (setq lsp-completion-show-kind t)
      ;; (setq lsp-enable-snippet t)
      (setq lsp-enable-symbol-highlighting t)
      (setq lsp-modeline-code-actions-enable t)

      ;; lsp-ui-sideline:
      (setq
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics nil ;; show diagnostics messages in sideline
        lsp-ui-sideline-show-hover nil ;; show hover messages in sideline
        lsp-ui-sideline-show-code-actions nil ;; show code actions in sideline
        )

      ;; lsp-ui-peek
      (setq
        lsp-ui-peek-enable t ;; enable ‘lsp-ui-peek’
        lsp-ui-peek-show-directory t ;; show the directory of files
        )

      ;; lsp-ui-doc
      (setq
        lsp-ui-doc-enable t ;; Enable lsp-ui-doc
        ;; lsp-ui-doc-position Where to display the doc (top, bottom or at-point)
        ;; lsp-ui-doc-side Where to display the doc (left or right)
        ;; lsp-ui-doc-delay Number of seconds before showing the doc
        lsp-ui-doc-show-with-cursor nil ;; When non-nil, move the cursor over a symbol to show the doc
        lsp-ui-doc-show-with-mouse t ;; When non-nil, move the mouse pointer over a symbol to show the doc
        )

      ;; lsp-ui-imenu
      (setq
        lsp-ui-imenu-enable t
        ;; lsp-ui-imenu-kind-position place to show entries kind
        ;; lsp-ui-imenu-buffer-position place to show the buffer window
        ;; lsp-ui-imenu-window-width set window width
        ;; lsp-ui-imenu-window-fix-width when non-nil, the window will not be resizable (eg. unaffected by balance-windows)
        ;; lsp-ui-imenu--custom-mode-line-format mode line format
        ;; lsp-ui-imenu-auto-refresh auto refresh when necessary
        ;; lsp-ui-imenu-refresh-delay delay to refresh imenu
        )
      )

    (with-eval-after-load 'color-identifiers-mode
      (dolist (maj-mode '(ng2-ts-mode))
        (add-to-list
          'color-identifiers:modes-alist
          `(,maj-mode . (""
                          "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                          (nil font-lock-variable-name-face tree-sitter-hl-face:variable))))))

    ;; set the typescript layers keymap as parent to the ng2-ts layers keymap
    (set-keymap-parent spacemacs-ng2-ts-mode-map spacemacs-typescript-mode-map)

    (spacemacs/set-leader-keys-for-major-mode 'ng2-ts-mode
      "c" 'ng2-open-counterpart)
    (spacemacs/set-leader-keys-for-major-mode 'ng2-html-mode
      "c" 'ng2-open-counterpart)

    :config
    (cats/angular-mode-config 'ng2-ts-mode)))


;; etc
(defun cats-angular/pre-init-treesit-fold ()
  (spacemacs|use-package-add-hook treesit-fold
    :post-config
    (add-to-list 'treesit-fold-range-alist
      `(ng2-ts-mode . ,(treesit-fold-parsers-typescript)))
    (add-to-list 'treesit-fold-summary-parsers-alist
      '(ng2-ts-mode . treesit-fold-summary-javadoc))))

(defun cats-angular/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(ng2-ts-mode-hook
                                                    ng2-html-mode-hook)))

(defun cats-angular/post-init-company ()
  (spacemacs/add-to-hooks #'cats//angular-setup-company
    '(ng2-ts-mode-local-vars-hook)))

(defun cats-angular/post-init-eldoc ()
  (spacemacs/add-to-hooks #'cats//angular-setup-eldoc
                          '(ng2-ts-mode-local-vars-hook) t))

(defun cats-angular/post-init-emmet-mode ()
  (add-hook 'ng2-ts-mode-hook #'cats/angular-emmet-mode))

(defun cats-angular/post-init-evil-matchit ()
  (evilmi-load-plugin-rules '(ng2-html-mode) '(simple template html))
  (evilmi-load-plugin-rules '(ng2-ts-mode) '(simple javascript html))

  (add-hook 'ng2-html-mode-hook 'turn-on-evil-matchit-mode)
  (add-hook 'ng2-ts-mode-hook 'turn-on-evil-matchit-mode))

(defun cats-angular/set-tide-linter ()
  (pcase angular-linter
    ('tslint (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)
             (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode))
    ('eslint (flycheck-add-mode 'javascript-eslint 'ng2-ts-mode)
             (add-to-list 'flycheck-disabled-checkers 'typescript-tslint)
             (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append))
    (_ (message
        "Invalid typescript-layer configuration, no such linter: %s" angular-linter))))

(defun cats-angular/set-lsp-linter ()
  (pcase angular-linter
    ('tslint (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode))
    ;; This sets tslint unconditionally for all lsp clients which is wrong
    ;; Must be set for respective modes only, see go layer for examples.
    ('eslint (flycheck-add-mode 'javascript-eslint 'ng2-ts-mode))
    (_ (message
        "Invalid angular-layer configuration, no such linter: %s" angular-linter))))

(defun cats-angular/set-linter ()
  (pcase angular-backend
    ('tide (cats-angular/set-tide-linter))
    ('lsp (cats-angular/set-lsp-linter))))

(defun cats-angular/post-init-flycheck ()
  (add-hook 'cats/project-hook 'cats//locate-node-from-projectile)
  (add-hook 'cats/project-hook 'cats//locate-jshint-from-projectile)
  (add-hook 'cats/project-hook 'cats//locate-jscs-from-projectile)
  (add-hook 'cats/eslint-executable-hook 'cats//esilnt-set-eslint-executable)
  (add-hook 'cats/project-hook 'cats//locate-eslint-from-projectile)

  (spacemacs/enable-flycheck 'ng2-ts-mode)
  (spacemacs/enable-flycheck 'ng2-html-mode)
  (spacemacs/add-to-hooks #'cats//angular-setup-checkers
    '(ng2-ts-mode-hook)
    t)
  (spacemacs/add-to-hooks #'cats-angular/set-linter
    '(ng2-ts-mode-local-vars-hook)
    t)
  )

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
  (when (eq angular-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'ng2-ts-mode))
  (when (eq angular-html-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'ng2-html-mode)))

(defun cats-angular/post-init-smartparens ()
  (spacemacs/add-to-hooks #'spacemacs//activate-smartparens '(ng2-ts-mode-hook
                                                              ng2-html-mode-hook)))

(defun cats-angular/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'ng2-ts-mode))

(defun cats-angular/pre-init-web-beautify ()
  (when (eq angular-html-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes (cons 'ng2-html-mode 'web-beautify-html))))

(defun cats-angular/post-init-yasnippet ()
  (spacemacs/add-to-hooks #'cats/angular-yasnippet-setup '(ng2-ts-mode-hook
                                                           nt2-html-mode-hook)))
