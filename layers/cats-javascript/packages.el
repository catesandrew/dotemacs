;;; packages.el --- cats-javascript: Layer packages

;;; Commentary:

;; A personal javascript layer.

;;; Code:

(defconst cats-javascript-packages
  '(
    babel-repl
    coffee-mode
    company
    company-tern
    ;; company-ycmd
    flycheck
    js-doc
    js2-mode
    (js2-menu-extras :location built-in)
    js2-refactor
    json-mode
    json-reformat
    livid-mode
    mocha
    nodejs-repl
    popwin
    skewer-mode
    smartparens
    tern
    web-beautify
    xref-js2
    ;; ycmd
    rjsx-mode
    emmet-mode
    ggtags
    helm-gtags
    evil-matchit
    ))



;; rjsx
(defun cats-javascript/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
      (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
      (add-to-list 'magic-mode-alist '("^import React" . rjsx-mode)))
    :config
    (progn
      ;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
      ;; Workaround sgml-mode and align closing bracket with opening bracket
      (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
        "Workaround sgml-mode and follow airbnb component style."
        (let* ((cur-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
          (if (string-match "^\\( +\\)\/?> *$" cur-line)
              (let* ((empty-spaces (match-string 1 cur-line)))
                (replace-regexp empty-spaces
                                (make-string (- (length empty-spaces) sgml-basic-offset) 32)
                                nil
                                (line-beginning-position) (line-end-position))))))

      (evil-define-key 'insert rjsx-mode-map (kbd "C-d")
        'cats//rjsx-delete-creates-full-tag-with-insert)

      (with-eval-after-load 'flycheck
        (add-hook 'rjsx-mode-hook 'cats/disable-js2-checks-if-flycheck-active))
      )))



;; emmet-mode
(defun cats-javascript/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  (add-hook 'rjsx-mode-hook 'cats//setup-emmet-mode-for-react))

(defun cats-javascript/post-init-ggtags ()
  (add-hook 'rjsx-mode-hook #'spacemacs/ggtags-mode-enable))

(defun cats-javascript/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'rjsx-mode))

(defun cats-javascript/post-init-evil-matchit ()
  (add-hook `rjsx-mode `turn-on-evil-matchit-mode))


;; babel

(defun cats-javascript/init-babel-repl ()
  (use-package babel-repl
    :ensure t
    :commands (babel-repl babel-shell-mode)
    :defer t
    :init
    (progn
      ;; Set args with `.dir-locals.el' file.
      ;;
      ;; ((nil . (
      ;;   (eval . (setq-default babel-repl-cli-arguments '("--presets" "es2015"))))))
      (setq babel-repl-cli-arguments '())

      (add-hook 'cats/babel-node-executable-hook
         'cats//set-babel-node-executable)

      (add-hook 'cats/project-hook
         'cats//locate-babel-node-from-projectile)

      (spacemacs/register-repl 'babel-repl 'babel-repl "babel")
      (push "\\*babel-shell\\*" spacemacs-useful-buffers-regexp)
      (spacemacs|hide-lighter babel-shell-mode)
      (dolist (mode '(js2-mode js2-jsx-mode web-mode))
        (spacemacs/declare-prefix-for-mode mode "mb" "babel")
        (spacemacs/set-leader-keys-for-major-mode mode
          "b'" 'babel-start-repl
          "ba" 'babel-send-region-or-buffer
          "br" 'babel-send-region
          "bb" 'babel-send-buffer
          "bp" 'babel-send-paragraph
          "bd" 'babel-send-dwim)))
    :config
    (progn
      (evilified-state-evilify babel-shell-mode babel-shell-mode-map
        "q" 'quit-window)

      (dolist (mode (list babel-shell-mode-map))
        (evil-define-key 'normal mode
          (kbd "q") 'quit-window)))))


;; coffee
(defun cats-javascript/pre-init-coffee-mode ()
  (spacemacs|use-package-add-hook coffee-mode
    :post-init
    (progn
      (add-hook 'cats/project-hook
         'cats//locate-coffeelint-from-projectile)

      (with-eval-after-load 'flycheck
        (add-hook 'cats/coffeelint-executable-hook
           'cats//set-coffeelint-executable)))))


;; company
(defun cats-javascript/post-init-company ()
  (spacemacs|add-company-hook rjsx-mode)
  (spacemacs|add-company-hook js2-jsx-mode))


;; company-tern
(defun cats-javascript/post-init-company-tern ()
  (push 'company-tern company-backends-rjsx-mode)
  (push 'company-tern company-backends-js2-jsx-mode))


;; company-ycmd
(defun cats-javascript/post-init-company-ycmd ()
  (push 'company-ycmd company-backends-rjsx-mode)
  (push 'company-ycmd company-backends-js2-mode)
  (push 'company-ycmd company-backends-js2-jsx-mode))


;; flycheck
(defun cats-javascript/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'rjsx-mode)
  (spacemacs/add-flycheck-hook 'js2-jsx-mode))


;; js-doc
(defun cats-javascript/pre-init-js-doc ()
  (spacemacs|use-package-add-hook js-doc
    :post-init
    (progn
      (setq js-doc-mail-address user-mail-address)
      (setq js-doc-author cats/js-doc-author)
      (setq js-doc-url cats/js-doc-url)
      (setq js-doc-license cats/js-doc-license)
      (setq js-doc-parameter-line cats/js-doc-parameter-line)
      (setq js-doc-all-tag-alist cats/js-doc-tags)

      (setq js-doc-file-doc-lines
            '(js-doc-top-line
              " * @module %F\n"
              " * @author %a\n"
              " * @license %l\n"
              js-doc-bottom-line))

      (dolist (hook '(rjsx-mode-hook
                      js2-jsx-mode-hook
                      react-mode-hook))
        (add-hook hook 'spacemacs/js-doc-require))

      (dolist (mode '(js2-mode js2-jsx-mode react-mode rjsx-mode))
        (spacemacs/declare-prefix-for-mode mode "mrd" "jsdoc")
        (spacemacs/js-doc-set-key-bindings mode)))))


;; js2-mode
(defun cats-javascript/pre-init-js2-mode ()
  (spacemacs|use-package-add-hook js2-mode
    :post-init
    (progn
      (add-to-list 'auto-mode-alist (cons (rx ".jsx" eos) 'js2-jsx-mode))
      (add-to-list 'auto-mode-alist '("\\.jshintrc$" . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.eslintrc$" . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
      (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

      (add-hook 'cats/project-hook
         'cats//locate-node-from-projectile)

      (spacemacs|add-toggle js2-highlight-unused-variables-mode
        :status js2-highlight-unused-variables-mode
        :on (progn
              (when (bound-and-true-p js2-highlight-unused-variables-mode)
                (js2-highlight-unused-variables-mode -1))
              (js2-highlight-unused-variables-mode))
        :off (js2-highlight-unused-variables-mode -1)
        :documentation "Highlight unused js vars."
        :evil-leader "toj"))

    :post-config
    (progn
      (setq-default javascript-indent-lever 2)
      (setq-default js-switch-indent-offset 2)
      (setq-default js2-jslint-globals t)
      (setq-default js2-basic-offset 2)
      (setq-default js-indent-level 2)
      (setq-default js2-use-font-lock-faces t)
      (setq-default js2-include-browser-externs t)
      (setq-default js2-include-node-externs t)
      (setq-default js2-missing-semi-one-line-override t)
      (setq-default js2-strict-cond-assign-warning nil)
      (setq-default js2-strict-var-hides-function-arg-warning nil)
      (setq-default js2-strict-var-redeclaration-warning nil)
      (setq-default js2-warn-about-unused-function-arguments nil)
      (setq-default js2-allow-rhino-new-expr-initializer nil)
      (setq-default js2-auto-indent-p nil)
      (setq-default js2-enter-indents-newline nil)
      (setq-default js2-mirror-mode nil)
      (setq-default js2-strict-inconsistent-return-warning nil)
      (setq-default js2-include-rhino-externs nil)
      (setq-default js2-include-gears-externs nil)
      (setq-default js2-concat-multiline-strings 'eol)
      (setq-default js2-rebind-eol-bol-keys nil)
      (setq-default js2-bounce-indent nil)
      (setq-default js2-indent-switch-body 2)

      (setq-default js2-global-externs cats/js2-global-externs)

      ;; Let flycheck handle parse errors
      (setq-default js2-mode-show-parse-errors nil)
      (setq-default js2-mode-show-strict-warnings nil)
      (setq-default js2-strict-missing-semi-warning nil)
      (setq-default js2-highlight-external-variables nil)
      (setq-default js2-strict-trailing-comma-warning nil)

      (with-eval-after-load 'flycheck
        (add-hook 'js2-mode-hook 'cats/disable-js2-checks-if-flycheck-active)
        (add-hook 'js2-jsx-mode-hook 'cats/disable-js2-checks-if-flycheck-active)))))


;; js2-menu-extras
(defun cats-javascript/init-js2-menu-extras ()
  (use-package js2-imenu-extras
    :ensure js2-mode
    :commands (js2-imenu-extras-mode js2-imenu-make-index)
    :defer t
    :init
    (progn
      (set-default 'imenu-auto-rescan t)
      ;; required to make `<leader> sj' or `<leader> ij` to work correctly
      ;; it is tied to `dotemacs/jump-in-buffer`
      (dolist (hook '(rjsx-mode-hook
                      js2-jsx-mode-hook
                      react-mode-hook
                      js2-mode-hook))
        (add-hook hook 'js2-imenu-extras-mode)
        (add-hook hook
           (lambda ()
             (setq imenu-create-index-function 'js2-imenu-make-index)))))))


;; js2-refactor
(defun cats-javascript/post-init-js2-refactor ()
  (dolist (hook '(rjsx-mode-hook
                  js2-jsx-mode-hook))
    (add-hook hook 'spacemacs/js2-refactor-require)))


;; json-mode
(defun cats-javascript/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-config\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.jscsrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode)))


;; json-reformat
(defun cats-javascript/init-json-reformat ()
  "Reformat JSON."
  (use-package json-reformat
    :ensure t
    :defer t
    :init
    (progn
      (setq json-reformat:indent-width 2)
      (setq json-reformat:pretty-string? t)

      (dolist (mode '(json-mode))
        (spacemacs/declare-prefix-for-mode mode "mr" "reformat")
        (spacemacs/set-leader-keys-for-major-mode mode
          "rr" 'json-reformat-region)))
    ))


;; livid-mode
(defun cats-javascript/pre-init-livid-mode ()
  (spacemacs|use-package-add-hook livid-mode
    :post-init
    (progn
      (defalias 'js-live-eval 'livid-mode
        "Minor mode for automatic evaluation of a JavaScript buffer on every change")

      (dolist (mode '(rjsx-mode js2-mode js2-jsx-mode))
        (spacemacs/declare-prefix-for-mode mode "ml" "livid")
        (spacemacs/set-leader-keys-for-major-mode mode
          "le" 'js-live-eval)))))


;; mocha
(defun cats-javascript/init-mocha ()
  (use-package mocha
    :ensure t
    ;; :commands (mocha-run mocha-debug)
    :defer t
    :init
    (progn
      (dolist (mode '(rjsx-mode js2-mode js2-jsx-mode web-mode))
        (spacemacs/declare-prefix-for-mode mode "mm" "mocha")
        (spacemacs/set-leader-keys-for-major-mode mode
          "mp" 'mocha-test-project
          "mP" 'mocha-debug-project
          "mf" 'mocha-test-file
          "mF" 'mocha-debug-file
          "mp" 'mocha-test-at-point))

      (add-hook 'cats/project-hook
         'cats//locate-mocha-from-projectile)
      ;; (cats//set-mocha-executable cats//executable-mocha)

      (add-hook 'cats/node-executable-hook
         'cats//set-node-executable)

      (add-hook 'cats/mocha-executable-hook
         'cats//set-mocha-executable)

      (push "\\*mocha tests\\*" spacemacs-useful-buffers-regexp)
      (setq mocha-environment-variables "NODE_ENV=test")
      (setq mocha-options "--recursive --reporter dot -t 5000")
      (setq mocha-reporter "spec")
      (setq mocha-project-test-directory "test"))))


;; nodejs-repl

(defun cats-javascript/init-nodejs-repl ()
  (use-package nodejs-repl
    :ensure t
    :commands (nodejs-repl--get-or-create-process nodejs-repl)
    :defer t
    :init
    (progn
      (setq nodejs-repl-arguments '())

      (add-hook 'cats/node-executable-hook
         'cats//nodejs-set-node-executable)

      (spacemacs/register-repl 'nodejs-repl 'nodejs-repl "nodejs")
      (push "\\*nodejs\\*" spacemacs-useful-buffers-regexp)
      (spacemacs|hide-lighter nodejs-repl-mode)
      (dolist (mode '(rjsx-mode js2-mode js2-jsx-mode web-mode))
        (spacemacs/declare-prefix-for-mode mode "mn" "nodejs")
        (spacemacs/set-leader-keys-for-major-mode mode
          "n'" 'nodejs-start-repl
          "nf" 'nodejs-load-file
          "nl" 'nodejs-send-line
          "nb" 'nodejs-send-buffer
          "nr" 'nodejs-send-region)))

    :config
    (progn
      ;; overwrite TAB with update completion-at-point, it
      ;; should be in a company backend and is a quick hack.
      (evilified-state-evilify nodejs-repl-mode nodejs-repl-mode-map
        (kbd "C-g") 'nodejs-repl-quit-or-cancel
        (kbd "C-u") 'nodejs-repl-clear-line
        (kbd "TAB") 'completion-at-point
        (kbd "C-c C-c") 'nodejs-repl-quit-or-cancel
        "q" 'quit-window)

      (dolist (mode (list nodejs-repl-mode-map))
        (evil-define-key 'normal mode
          (kbd "C-g") 'nodejs-repl-quit-or-cancel
          (kbd "C-u") 'nodejs-repl-clear-line
          (kbd "TAB") 'completion-at-point
          (kbd "C-c C-c") 'nodejs-repl-quit-or-cancel
          (kbd "q") 'quit-window)))))


;; popwin
(defun cats-javascript/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*xref*" :dedicated t :position bottom)
          popwin:special-display-config)
    (push '("*nodejs*" :dedicated t :position bottom)
          popwin:special-display-config)
    (push '("*mocha tests*"                :dedicated t :position bottom)
          popwin:special-display-config)
    (push '("*babel-shell*"                :dedicated t :position bottom
            :stick t        :noselect nil)
          popwin:special-display-config)
    (push '("* Mocha Test Output *"        :dedicated t :position bottom
            :stick tc-state :noselect nil :height 0.4)
          popwin:special-display-config)
    ))


;; skewer-mode
(defun cats-javascript/pre-init-skewer-mode ()
  (spacemacs|use-package-add-hook skewer-mode
    :post-init
    (progn
      (dolist (hook '(rjsx-mode-hook
                      js2-jsx-mode-hook
                      react-mode-hook))
        (add-hook hook 'skewer-mode))
      (add-hook 'cats/phantomjs-executable-hook
         'cats//skewer-set-phantomjs-executable)
      (add-hook 'cats/project-hook
         'cats//locate-phantomjs-from-projectile)
      (add-hook 'css-mode-hook 'skewer-css-mode)
      (add-hook 'html-mode-hook 'skewer-html-mode))
    :post-config
    (progn
      (dolist (mode '(rjsx-mode js2-jsx-mode web-mode react-mode css-mode html-mode))
        (spacemacs/declare-prefix-for-mode mode "ms" "skewer")
        (spacemacs/declare-prefix-for-mode mode "me" "eval")
        (spacemacs/set-leader-keys-for-major-mode mode
          "'" 'spacemacs/skewer-start-repl
          "ee" 'skewer-eval-last-expression
          "eE" 'skewer-eval-print-last-expression
          "sb" 'skewer-load-buffer
          "sB" 'spacemacs/skewer-load-buffer-and-focus
          "si" 'spacemacs/skewer-start-repl
          "sf" 'skewer-eval-defun
          "sF" 'spacemacs/skewer-eval-defun-and-focus
          "sr" 'spacemacs/skewer-eval-region
          "sR" 'spacemacs/skewer-eval-region-and-focus
          "ss" 'skewer-repl)
        ))))


;; smartparens
(defun cats-javascript/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (sp-with-modes 'rjsx-mode
      (sp-local-pair "<" ">"))
    (sp-with-modes 'js2-jsx-mode
      (sp-local-pair "<" ">"))))


;; tern
(defun cats-javascript/pre-init-tern ()
  (spacemacs|use-package-add-hook tern
    :post-init
    (progn
      (dolist (hook '(rjsx-mode-hook
                      js2-jsx-mode-hook
                      react-mode-hook))
        (add-hook hook 'tern-mode)))
    :post-config
    (progn
      (dolist (mode '(rjsx-mode js2-jsx-mode react-mode))
        (when javascript-disable-tern-port-files
          (add-to-list 'tern-command "--no-port-file" 'append))
        (spacemacs//set-tern-key-bindings mode)))))


;; web-beautify
(defun cats-javascript/post-init-web-beautify ()
  (dolist (mode '(rjsx-mode
                  js2-jsx-mode))
    (spacemacs/set-leader-keys-for-major-mode mode  "=" 'web-beautify-js)))


;; xref-js2
(defun cats-javascript/init-xref-js2 ()
  (use-package xref-js2
    :ensure t
    :defer t
    :init
    (progn
      ;; (evilified-state-evilify xref-js2-mode xref-js2-mode-map
      ;;   (kbd "q") 'quit-window)

      (dolist (mode '(rjsx-mode js2-mode js2-jsx-mode react-mode web-mode))
        (spacemacs/declare-prefix-for-mode mode "mj" "jump/join/split")
        (spacemacs/set-leader-keys-for-major-mode mode
          "jg" 'xref-find-definitions
          "jr" 'xref-find-references
          "jl" 'xref-resume-last-search
          "jb" 'xref-pop-marker-stack))

      (dolist (hook '(rjsx-mode-hook
                      js2-jsx-mode-hook
                      react-mode-hook
                      js2-mode-hook))
        (add-hook hook
           (lambda ()
             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))))


;; ycmd
(defun cats-javascript/post-init-ycmd ()
  (dolist (hook '(rjsx-mode-hook
                  js2-jsx-mode-hook
                  react-mode-hook
                  js2-mode-hook))
    (add-hook hook 'ycmd-mode))
  (add-to-list 'spacemacs-jump-handlers-rjsx-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-js2-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-js2-jsx-mode '(ycmd-goto :async t))
  (dolist (mode '(react-mode rjsx-mode js2-mode js2-jsx-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "jy" 'ycmd-goto
      "jY" 'ycmd-goto-imprecise)))


;;; packages.el ends here
