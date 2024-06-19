;;; packages.el --- cats-javascript: Layer packages

;;; Commentary:

;; npm i -g js-beautify
;; npm i -g tern

;; A personal javascript layer.

;;; Code:

(defconst cats-javascript-packages
  '(
     babel-repl
     coffee-mode
     company
     dap-mode
     eldoc
     (eslint-fix :location local)
     exec-path-from-shell
     flycheck
     import-js
     indium
     js-doc
     js2-mode
     js2-refactor
     json-mode ;; todo move into new json layer
     json-reformat ;; todo move into new json layer
     karma
     livid-mode
     mocha
     ;; nodejs-repl
     org
     popwin
     rebox2
     prettier-js
     ;; rjsx-mode
     skewer-mode
     tern
     web-beautify
     ;; tide
     (tj-mode :location (recipe :fetcher github
                          :repo "purcell/tj-mode"))
     xref-js2
     jest
     ;; npm-mode
     ;; (tsi :location (recipe :fetcher github
     ;;                       :repo "orzechowskid/tsi.el"))
     (css-in-js-mode :location (recipe :fetcher github
                                 :branch "main"
                                 :repo "orzechowskid/tree-sitter-css-in-js"))
     ;; (tsx-mode :location (recipe :fetcher github
     ;;                       :branch "main"
     ;;                       :repo "orzechowskid/tsx-mode.el"))
     (flymake-stylelint :location (recipe :fetcher github
                                    :repo "orzechowskid/flymake-stylelint"))
     add-node-modules-path
     emmet-mode
     smartparens
     yasnippet
     typescript-mode
     (skerrick :location (recipe :fetcher github
                           :repo "anonimitoraf/skerrick"))
     (atomic-chrome :location (recipe :fetcher github
                                :repo "karimaziev/atomic-chrome"))

     jtsx ;; Extends Emacs JSX/TSX built-in support
     (treesit-fold :location
       (recipe :fetcher github
         :repo "abougouffa/treesit-fold"))
     ))


;; here to get tsx-mode working
(add-hook 'configuration-layer-post-load-hook
  (defun cats//layer-hook ()
    (setq auto-mode-alist (delete '("\\.tsx\\'" . typescript-tsx-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist))))


;; jtsx
;; https://github.com/llemaitre19/jtsx

;; - `jtsx-jsx-mode` is fully compatible with pure JS files, ithas some rare conflicts with TS files.
;; It is thus recommanded to use
;; - `jtsx-typescript-mode` (based on typescript-ts-mode) for plain TS files.

;; `jtsx-jsx-mode` to JSX and JS files
;; `jtsx-tsx-mode` to TSX files
;; `jtsx-typescript-mode` to TS files

;; bind jtsx functions to the same shortcuts for jtsx-jsx-mode and jtsx-tsx-mode
;; set indention offsets for JSX/JS and TSX/TS modes (use base mode variables)
;; customize jtsx behaviour through provided variables
;; enable hideshow minor mode for code folding
(defun cats-javascript/init-jtsx ()
  (use-package jtsx
    :ensure t
    :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
            ("\\.tsx\\'" . jtsx-tsx-mode)
            ("\\.ts\\'" . jtsx-typescript-mode))
    :hook ((jtsx-jsx-mode . hs-minor-mode)
            (jtsx-tsx-mode . hs-minor-mode)
            (jtsx-typescript-mode . hs-minor-mode))
    :init
    (add-to-list 'auto-mode-alist '("\\.jshintrc$\\'" . jtsx-jsx-mode))
    (add-to-list 'auto-mode-alist '("\\.eslintrc$\\'" . jtsx-jsx-mode))
    (add-to-list 'auto-mode-alist '("\\.mjs\\'" . jtsx-jsx-mode))
    (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . jtsx-jsx-mode))

    (dolist (hook '(jtsx-jsx-mode))
      (add-hook hook (lambda () (run-hooks #'cats/javascript-mode-hook))))

    (dolist (hook '(jtsx-tsx-mode jtsx-typescript-mode))
      (add-hook hook (lambda () (run-hooks #'cats/typescript-mode-hook))))

    (add-hook 'jtsx-jsx-mode-local-vars-hook #'spacemacs//javascript-setup-backend)
    (add-hook 'jtsx-jsx-mode-local-vars-hook #'spacemacs//javascript-setup-next-error-fn)
    ;; safe values for backend to be used in directory file variables
    ;; (dolist (value '(lsp tern tide))
    ;;   (add-to-list 'safe-local-variable-values
    ;;                (cons 'javascript-backend value)))

    ;; Optional customizations
    (setq typescript-ts-mode-indent-offset 2)
    (setq jtsx-switch-indent-offset 0)
    (setq jtsx-indent-statement-block-regarding-standalone-parent nil)
    (setq jtsx-jsx-element-move-allow-step-out t)
    (setq jtsx-enable-jsx-electric-closing-element t)
    (setq jtsx-enable-electric-open-newline-between-jsx-element-tags t)
    (setq jtsx-enable-jsx-element-tags-auto-sync nil)
    (setq jtsx-enable-all-syntax-highlighting-features t)
    (with-eval-after-load 'color-identifiers-mode
      (add-to-list
        'color-identifiers:modes-alist
        `(jtsx-jsx-mode . (,color-identifiers:re-not-inside-class-access
                            "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                            (nil font-lock-variable-name-face js2-function-param))))
      (dolist (maj-mode '(jtsx-tsx-mode jtsx-typescript-mode tsx-mode))
        (add-to-list
          'color-identifiers:modes-alist
          `(,maj-mode . (""
                          "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                          (nil font-lock-variable-name-face tree-sitter-hl-face:variable))))))
    :config
    (when javascript-fmt-on-save
      (add-hook 'jtsx-jsx-mode-local-vars-hook 'spacemacs/javascript-fmt-before-save-hook))

    (when typescript-fmt-on-save
      (add-hook 'jtsx-jsx-mode-local-vars-hook #'spacemacs/typescript-fmt-before-save-hook)
      (add-hook 'jtsx-typescript-mode-local-vars-hook #'spacemacs/typescript-fmt-before-save-hook))

    ;; todo bind to evil keys spacemacs
    ;; (defun jtsx-bind-keys-to-mode-map (mode-map)
    ;;   "Bind keys to MODE-MAP."
    ;;   (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    ;;   (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    ;;   (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    ;;   (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    ;;   (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    ;;   (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    ;;   (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    ;;   (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    ;;   (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    ;;   (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    ;;   (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    ;;   (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    ;;   (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node))

    ;; (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    ;;   (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

    ;; (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    ;;   (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

    ;; (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
    ;; (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
    )
  )

(defun cats-javascript/pre-init-treesit-fold ()
  (spacemacs|use-package-add-hook treesit-fold
    :post-config
    (add-to-list 'treesit-fold-range-alist
      `(jtsx-jsx-mode . ,(treesit-fold-parsers-javascript)))
    (add-to-list 'treesit-fold-range-alist
      `(jtsx-tsx-mode . ,(treesit-fold-parsers-typescript)))
    (add-to-list 'treesit-fold-range-alist
      `(jtsx-typescript-mode . ,(treesit-fold-parsers-typescript)))

    (add-to-list 'treesit-fold-summary-parsers-alist
      '(tsx-mode . treesit-fold-summary-javadoc))
    (add-to-list 'treesit-fold-summary-parsers-alist
      '(jtsx-jsx-mode . treesit-fold-summary-javadoc))
    (add-to-list 'treesit-fold-summary-parsers-alist
      '(jtsx-tsx-mode . treesit-fold-summary-javadoc))
    (add-to-list 'treesit-fold-summary-parsers-alist
      '(jtsx-typescript-mode . treesit-fold-summary-javadoc))))


;; atomic-chrome

;; codepen.io
;; stackblitz.com
;; jsfiddle.net
;; leetcode.com
;; hackerrank.com
;; repl.it
;; glitch.com
;; plnkr.co
;; codesandbox.io
(defun cats-javascript/init-atomic-chrome ()
  (use-package atomic-chrome
    :commands (atomic-chrome-start-server)
    :defines atomic-chrome-create-file-strategy
    :config
    ;; (setq-default atomic-chrome-default-major-mode 'markdown-mode)
    (setq atomic-chrome-create-file-strategy '((temp-directory)))
    (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
    (setq-default atomic-chrome-buffer-open-style 'frame)
    (setq-default atomic-chrome-auto-remove-file t)
    (setq-default atomic-chrome-url-major-mode-alist
      '(("ramdajs.com" . js-ts-mode)
         ("github.com" . gfm-mode)
         ("gitlab.com" . gfm-mode)
         ("leetcode.com" . typescript-ts-mode)
         ("codesandbox.io" . js-ts-mode)
         ("typescriptlang.org" . typescript-ts-mode)
         ("jsfiddle.net" . js-ts-mode)
         ("w3schools.com" . js-ts-mode)))
    (setq atomic-chrome-create-file-strategy
      '(("~/code/leetcode" :url ("leetcode.com"))
         ("~/code/leetcode/js/" :url ("leetcode.com") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/codepen" :url ("codepen.io"))
         ("~/code/codepen/js/" :url ("codepen.io") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/stackblitz" :url ("stackblitz.com"))
         ("~/code/stackblitz/js/" :url ("stackblitz.com") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/jsfiddle" :url ("jsfiddle.net"))
         ("~/code/jsfiddle/js/" :url ("jsfiddle.net") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/hackerrank" :url ("hackerrank.com"))
         ("~/code/hackerrank/js/" :url ("hackerrank.com") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/repl" :url ("repl.it"))
         ("~/code/repl/js/" :url ("repl.it") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/glitch" :url ("glitch.com"))
         ("~/code/glitch/js/" :url ("glitch.com") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/plnkr" :url ("plnkr.co"))
         ("~/code/plnkr/js/" :url ("plnkr.co") :extension ("js" "ts" "tsx" "jsx"))
         ("~/code/medium" :url ("medium.com"))))

    ;; (add-to-list 'atomic-chrome-create-file-strategy
    ;;   '("~/repos/ts-scratch/src/" :extension
    ;;      ("js" "ts" "tsx" "jsx" "cjs" "mjs")))
    )
  )


;; skerrick-mode
(defun cats-javascript/init-skerrick ()
  (use-package skerrick
    :defer t
    :commands (skerrick-start-server)
    :config
    ;; Needs to be run on the very first install of skerrick. Or when you want to upgrade.
    ;; npm install -g skerrick
    ;; (unless (equal (shell-command-to-string "type skerrick") "skerrick not found\n")
    ;;   (skerrick-install-or-upgrade-server-binary))
    (defun cats//skerrick-eval ()
      (interactive)
      (if (use-region-p)
        (skerrick-eval-region)
        (beginning-of-line)
        (set-mark-command nil)
        (end-of-line)
        (skerrick-eval-region)
        (pop-mark)))

    (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (bind-key "C-x C-e" 'cats//skerrick-eval 'spacemacs-jtsx-jsx-mode-map))

    (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (bind-key "C-x C-e" 'cats//skerrick-eval 'spacemacs-jtsx-tsx-mode-map))

    (defun jtsx-bind-keys-to-jtsx-typescript-mode-map ()
      (bind-key "C-x C-e" 'cats//skerrick-eval 'spacemacs-jtsx-typescript-mode-map))

    (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
    (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
    (add-hook 'jtsx-typescript-mode-hook 'jtsx-bind-keys-to-jtsx-typescript-mode-map)
    ))


;; typescript-mode
(defun cats-javascript/pre-init-typescript-mode ()
  (spacemacs|use-package-add-hook typescript-mode
    :post-init
    (progn
      (setq typescript-indent-level 2))))


;; flymake-stylelint
(defun cats-javascript/init-flymake-stylelint ()
  (use-package flymake-stylelint
    :ensure t))


;; tsi
;; (defun cats-javascript/init-tsi ()
;;   (use-package tsi
;;     :ensure t
;;     :init
;;     (progn
;;       (setq tsi-css-indent-offset 2)
;;       (setq tsi-typescript-indent-offset 2))))


;; css-in-js-mode
(defun cats-javascript/init-css-in-js-mode ()
  (use-package css-in-js-mode
    :ensure t))


;; tsx-mode
;; (defun cats-javascript/init-tsx-mode ()
;;   (use-package tsx-mode
;;     :defer t
;;     :init
;;     (progn
;;       (setq tsi-css-indent-offset 2)
;;       (setq tsi-typescript-indent-offset 2)
;;       (setq typescript-ts-mode-indent-offset 2))
;;     :config
;;     (progn
;;       ;; (add-to-list 'lsp--formatting-indent-alist '(tsx-mode . typescript-ts-mode-indent-offset))
;;       )
;;     ))


;; react-redux-yasnippets
(defun cats-javascript/init-js-react-redux-yasnippets ()
  (use-package js-react-redux-yasnippets
    :ensure t
    :init
    (progn)
    :config
    (progn)))


;;
(defun cats-javascript/init-npm-mode ()
  (use-package npm-mode
    :ensure t
    :defer t))


;;
(defun cats-javascript/init-jest ()
  (use-package jest
    :ensure t
    :defer t
    :init
    (progn
      (add-hook 'jest-mode-hook #'compilation-minor-mode)
      (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode rjsx-mode js2-mode js2-jsx-mode))
        (spacemacs/declare-prefix-for-mode mode "mt" "jest")
        (spacemacs/set-leader-keys-for-major-mode mode
          "tj" 'jest
          "tp" 'jest-last-failed
          "tl" 'jest-function
          "tL" 'jest-function-dwim
          "tf" 'jest-file
          "tF" 'jest-file-dwim
          "tr" 'jest-repeat)))
    :config
    (progn
      ;; (add-hook 'cats/project-hook
      ;;   'cats//locate-jest-from-projectile)

      ;; (add-hook 'cats/node-executable-hook
      ;;   'cats//jest-set-node-executable)

      ;; (add-hook 'cats/jest-executable-hook
      ;;   'cats//set-jest-executable)

      (push "\\*jest\\*" spacemacs-useful-buffers-regexp)
      ;; (setq mocha-environment-variables "NODE_ENV=test")
      ;; (setq mocha-options "--recursive --reporter dot -t 5000")
      ;; (setq mocha-reporter "spec")
      ;; (setq mocha-project-test-directory "test")

      ;; overwrite TAB with update completion-at-point, it
      ;; should be in a company backend and is a quick hack.
      (evilified-state-evilify jest-mode jest-mode-map
        ;; (kbd "C-g") 'nodejs-repl-quit-or-cancel
        ;; (kbd "C-u") 'nodejs-repl-clear-line
        (kbd "TAB") 'completion-at-point
        ;; (kbd "C-c C-c") 'nodejs-repl-quit-or-cancel
        "q" 'quit-window)

      (dolist (mode (list jest-mode-map))
        (evil-define-key 'normal mode
          ;; (kbd "C-g") 'nodejs-repl-quit-or-cancel
          ;; (kbd "C-u") 'nodejs-repl-clear-line
          (kbd "TAB") 'completion-at-point
          ;; (kbd "C-c C-c") 'nodejs-repl-quit-or-cancel
          (kbd "q") 'quit-window))
      )))


;; karma
(defun cats-javascript/init-karma ()
  "Use the karma test runner."
  (use-package karma
    :commands (karma-start karma-run)))


;; org
(defun cats-javascript/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(js . t))))


;; rebox2
(defun cats-javascript/pre-init-rebox2 ()
  (spacemacs|use-package-add-hook rebox2
    :post-config
    (progn
      ;; Box templates. First number is style, second is recognition weight.
      ;; Adding 300 replaces `?' by `/', for C++ style comments.
      ;; Adding 400 replaces `?' by `#', for scripting languages.
      ;; Adding 500 replaces `?' by ';', for LISP and assembler.
      ;; Adding 600 replaces `?' by `%', for TeX and PostScript.
      ;; js-doc style
      (rebox-register-template 247 248 '("/**"
                                         " * box123456"
                                         " */"))
      (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode tsx-mode rjsx-mode js2-mode js2-jsx-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "rdq" 'cats/js-doc-reflow)))))


;; exec-path-from-shell
(defun cats-javascript/post-init-exec-path-from-shell ()
  (dolist (var '(
                  "NODE_REPL_HISTORY_FILE"
                  "NODE_REPL_MODE"
                  "NVM_BIN"
                  "NVM_DIR"
                  "NVM_PATH"
                  "NVM_TARGET"
                  ) exec-path-from-shell-variables)
    (unless (member var exec-path-from-shell-variables)
      (push var exec-path-from-shell-variables))))


;; indium
(defun cats-javascript/init-indium ()
  "Use indium."
  (use-package indium
    :commands (indium-interaction-mode
               indium-repl-mode
               indium-run-node
               indium-repl-get-buffer
               indium-inspector-mode)
    :init
    (progn
      (spacemacs/register-repl
       'indium-repl-mode
       'cats/indium-start-repl
       "indium")

      (spacemacs|add-company-backends
       :backends company-indium-repl
       :modes indium-repl-mode)

      (when (string-equal system-type "darwin")
        (setq indium-chrome-executable "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"))
      (setq indium-v8-cache-disabled t)

      (add-hook 'indium-repl-mode-hook 'spacemacs/disable-hl-line-mode)

      (spacemacs|add-toggle indium-interaction-mode
        :status indium-interaction-mode
        :on (progn
              (when (bound-and-true-p indium-interaction-mode)
                (indium-interaction-mode -1))
              (indium-interaction-mode))
        :off (indium-interaction-mode -1)
        :documentation "Indium interactive mode."
        :evil-leader-for-mode
        (jtsx-jsx-mode . "Ti")
        (jtsx-tsx-mode . "Ti")
        (jtsx-typescript-mode . "Ti")
        (js2-mode . "Ti")
        (rjsx-mode . "Ti")
        (js2-jsx-mode . "Ti")
        (indium-repl-mode . "Ti"))

      (push "\\*JS REPL\\*" spacemacs-useful-buffers-regexp)
      (push "\\*node process\\*" spacemacs-useless-buffers-regexp)
      (spacemacs|hide-lighter indium-repl-mode)
      (dolist (mode '(indium-repl-mode jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode rjsx-mode js2-mode js2-jsx-mode))
        (spacemacs/declare-prefix-for-mode mode "mu" "indium")
        (spacemacs/set-leader-keys-for-major-mode mode
          "u'" 'cats/indium-start-node-repl
          "ub" 'indium-eval-buffer
          "uB" 'cats/indium-eval-buffer-and-focus
          "uc" 'cats/indium-start-chrome-repl
          "uC" 'indium-connect-to-chrome
          "ue" 'indium-eval
          "uf" 'indium-eval-defun
          "uF" 'cats/indium-eval-defun-and-focus
          "ul" 'indium-eval-last-node
          "un" 'cats/indium-start-node-repl
          "us" 'indium-switch-to-repl-buffer
          "ur" 'indium-eval-region
          "uR" 'cats/indium-eval-region-and-focus
          "ui" 'indium-restart-node))

      (spacemacs/set-leader-keys-for-major-mode 'indium-repl-mode
        "q" 'quit-window
        "Q" 'indium-quit
        "i" 'indium-repl-inspect
        "o" 'indium-repl-clear-output
        "z" 'indium-repl-pop-buffer
        "p" 'indium-repl-previous-input
        "k" 'indium-repl-previous-input
        "n" 'indium-repl-next-input
        "j" 'indium-repl-next-input)

      (spacemacs/set-leader-keys-for-major-mode 'indium-inspector-mode
        "q" 'quit-window
        "RET" 'indium-follow-link
        "gg" 'indium-inspector-refresh
        "i" 'indium-repl-inspect
        "C-n" 'indium-inspector-next-reference
        "C-p" 'indium-inspector-previous-reference
        "h" 'indium-inspector-pop
        "j" 'indium-inspector-next-reference
        "k" 'indium-inspector-previous-reference
        "l" 'indium-follow-link
        "TAB" 'indium-inspector-next-reference
        "S-TAB" 'indium-inspector-previous-reference))
    :config
    (progn
      (evilified-state-evilify JS-REPL-mode JS-REPL-mode-map)
      (evilified-state-evilify Inspector-mode Inspector-mode-map)

      (when (eq dotspacemacs-editing-style 'vim)
        (evil-define-key 'insert indium-inspector-mode-map
          (kbd "C-k") 'indium-inspector-previous-reference
          (kbd "C-j") 'indium-inspector-next-reference)
        (evil-define-key 'insert indium-repl-mode-map
          (kbd "C-k") 'indium-repl-previous-input
          (kbd "C-j") 'indium-repl-next-input))

      (evil-define-key 'normal indium-repl-mode-map
        (kbd "C-k") 'indium-repl-previous-input
        (kbd "C-j") 'indium-repl-next-input
        "q" 'quit-window
        "gk" 'indium-repl-previous-input
        "gj" 'indium-repl-next-input)

      (evil-define-key 'normal indium-inspector-mode-map
        (kbd "C-k") 'indium-inspector-previous-reference
        (kbd "C-j") 'indium-inspector-next-reference
        (kbd "C-h") 'indium-inspector-pop
        (kbd "C-l") 'indium-follow-link
        "q" 'quit-window
        "gh" 'indium-inspector-pop
        "gk" 'indium-inspector-previous-reference
        "gj" 'indium-inspector-next-reference
        "gl" 'indium-follow-link))))


;; tide
(defun cats-javascript/init-tide ()
  (use-package tide
    :commands (tide-mode tide-setup)
    :init
    (progn
      (add-hook 'cats/node-executable-hook
         'cats//tide-set-node-executable)

      (add-hook 'cats/project-hook
         'cats//locate-tsserver-from-projectile)

      (add-hook 'cats/tsserver-executable-hook
         'cats//tide-set-tsserver-executable)

      (spacemacs|add-toggle tide-mode
        :status tide-mode
        :on (progn
              (when (bound-and-true-p tide-mode)
                (tide-mode -1))
              (tide-mode))
        :off (tide-mode -1)
        :documentation "Tide mode."
        :evil-leader-for-mode
        (jtsx-jsx-mode . "Ti")
        (jtsx-tsx-mode . "Ti")
        (jtsx-typescript-mode . "Ti")
        (js2-mode . "Tt")
        (rjsx-mode . "Tt")
        (js2-jsx-mode . "Tt"))

      (spacemacs|add-toggle tide-hl-identifier-mode
        :status tide-hl-identifier-mode
        :on (progn
              (when (bound-and-true-p tide-hl-identifier-mode)
                (tide-hl-identifier-mode -1))
              (tide-hl-identifier-mode))
        :off (tide-hl-identifier-mode -1)
        :documentation "Tide identifier mode."
        :evil-leader-for-mode
        (jtsx-jsx-mode . "Ti")
        (jtsx-tsx-mode . "Ti")
        (jtsx-typescript-mode . "Ti")
        (js2-mode . "Th")
        (rjsx-mode . "Th")
        (js2-jsx-mode . "Th"))

      (defadvice tide-mode (after check-flycheck-tide-eslint-checkers activate)
        (if (bound-and-true-p tide-mode)
            (tide-flycheck-setup)
          (tide-flycheck-teardown))))))


;; rjsx
;; (defun cats-javascript/init-rjsx-mode ()
;;   (use-package rjsx-mode
;;     :defer t
;;     :init
;;     (progn
;;       (advice-add #'js-jsx-indent-line
;;                   :after
;;                   #'cats//js-jsx-indent-line-align-closing-bracket)

;;       (add-hook 'js2-mode-hook (lambda () (run-hooks #'cats/javascript-mode-hook)))

;;       (add-to-list 'magic-mode-alist '("\\(import.*from \'react\';\\|\/\/ @flow\nimport.*from \'react\';\\)" . rjsx-mode))

;;       ;; setup rjsx backend
;;       (spacemacs/add-to-hooks #'cats//rjsx-setup-backend '(rjsx-mode-local-vars-hook))
;;       ;; safe values for backend to be used in directory file variables
;;       (dolist (value '(lsp tide tern))
;;         (add-to-list 'safe-local-variable-values
;;           (cons 'rjsx-mode-backend value))))
;;     :config
;;     (progn
;;       ;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
;;       ;; Workaround sgml-mode and align closing bracket with opening bracket
;;       ;; (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
;;       ;;   "Workaround sgml-mode and follow airbnb component style."
;;       ;;   (let* ((cur-line (buffer-substring-no-properties
;;       ;;                     (line-beginning-position)
;;       ;;                     (line-end-position))))
;;       ;;     (if (string-match "^\\( +\\)\/?> *$" cur-line)
;;       ;;         (let* ((empty-spaces (match-string 1 cur-line)))
;;       ;;           (replace-regexp empty-spaces
;;       ;;                           (make-string (- (length empty-spaces) sgml-basic-offset) 32)
;;       ;;                           nil
;;       ;;                           (line-beginning-position) (line-end-position))))))

;;       (evil-define-key 'insert rjsx-mode-map (kbd "C-d")
;;         'cats//rjsx-delete-creates-full-tag-with-insert)

;;       (with-eval-after-load 'flycheck
;;         (add-hook 'rjsx-mode-hook 'cats/disable-js2-checks-if-flycheck-active))
;;       )))


;; eldoc
(defun cats-javascript/post-init-eldoc ()
  (spacemacs/add-to-hooks #'spacemacs//typescript-setup-eldoc
    '(jtsx-tsx-mode-local-vars-hook
      jtsx-typescript-mode-local-vars-hook) t)

  (spacemacs/add-to-hooks #'spacemacs//javascript-setup-eldoc
    '(jtsx-jsx-mode-local-vars-hook) t)
  )


;; import-js
(defun cats-javascript/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'spacemacs--import-js-modes (cons 'jtsx-tsx-mode 'jtsx-tsx-mode-hook))
    (add-to-list 'spacemacs--import-js-modes (cons 'jtsx-jsx-mode 'jtsx-jsx-mode-hook))
    (add-to-list 'spacemacs--import-js-modes (cons 'jtsx-typescript-mode 'jtsx-typescript-mode-hook))
    ))

(defun cats-javascript/post-init-import-js ()
  (spacemacs|use-package-add-hook import-js
    :post-init
    (progn
      (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode js2-mode js2-jsx-mode rjsx-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "I" 'import-js-import)

        (spacemacs/declare-prefix-for-mode mode "mo" "js-import")
        (spacemacs/set-leader-keys-for-major-mode mode
          "oi" 'import-js-import
          "of" 'import-js-fix
          "og" 'import-js-goto
          ;;"gG" 'import-js-goto-other-window
          ;; (kbd "C-g") 'import-js-pop-goto-definition
          "os" 'cats/run-import-js
          "ok" 'cats/kill-import-js)))

    :post-config
    (progn
      (add-hook 'cats/project-hook
        'cats//locate-importjs-from-projectile)

      ;; initialize the vars
      (let* ((frame (selected-frame))
              (name (cats//frame-name frame))
              (dir (cats//projectile-curr frame)))
        (when dir
          (cats//locate-importjs-from-projectile dir name)
          (setq import-js-current-project-root dir))))))


;; eslint-fix
(defun cats-javascript/init-eslint-fix ()
  (use-package eslint-fix
    :disabled t
    :commands (eslint-fix)
    :init
    (progn
      (add-hook 'cats/eslint-executable-hook
        'cats//esilnt-set-eslint-fix-executable)

      (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode js2-mode js2-jsx-mode rjsx-mode))
        (add-hook mode 'cats//eslint-fix-hook)))))


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
      (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode js2-mode js2-jsx-mode rjsx-mode))
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
  (spacemacs/add-to-hooks #'spacemacs//typescript-setup-company
    '(jtsx-tsx-mode-local-vars-hook
      jtsx-typescript-mode-local-vars-hook))

  (spacemacs/add-to-hooks #'spacemacs//javascript-setup-company
    '(jtsx-jsx-mode-local-vars-hook))

  (spacemacs|add-company-backends
    :backends company-capf
    :modes js2-mode)

  (spacemacs|add-company-backends
    :backends company-capf
    :modes jtsx-jsx-mode)

  (spacemacs|add-company-backends
    :backends company-capf
    :modes jtsx-tsx-mode)

  (spacemacs|add-company-backends
    :backends company-capf
    :modes jtsx-typescript-mode)

  (spacemacs|add-company-backends
    :backends company-capf
    :modes
    indium-mode
    indium-repl-mode))


;; dap-mode
(defun cats-javascript/pre-init-dap-mode ()
  (when (eq javascript-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'jtsx-jsx-mode))
  (add-hook 'jtsx-jsx-mode-local-vars-hook #'spacemacs//javascript-setup-dap))


;; add-node-modules-path
(defun cats-javascript/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(jtsx-jsx-mode-hook
                                                    jtsx-tsx-mode-hook
                                                    jtsx-typescript-mode-hook)))


;; emmet-mode
(defun cats-javascript/post-init-emmet-mode ()
  (add-hook 'jtsx-jsx-mode-hook #'spacemacs/javascript-emmet-mode)
  (add-hook 'jtsx-tsx-mode-hook #'spacemacs/typescript-emmet-mode)
  (add-hook 'jtsx-typescript-mode-hook #'spacemacs/typescript-emmet-mode))


;; smartparens
(defun cats-javascript/post-init-smartparens ()
  (spacemacs/add-to-hooks #'spacemacs//activate-smartparens '(jtsx-jsx-mode-hook
                                                              jtsx-tsx-mode-hook
                                                              jtsx-typescript-mode-hook)))


;; yasnippet
(defun cats-javascript/post-init-yasnippet ()
  (spacemacs/add-to-hooks #'spacemacs/typescript-yasnippet-setup '(jtsx-jsx-mode-hook
                                                                   jtsx-typescript-mode-hook)))


;; flycheck
(defun cats-javascript/setup-tsx-mode ()
  (with-eval-after-load 'flycheck
    ;; try some CSS-in-JS linting magic
    (flycheck-add-mode 'css-stylelint 'jtsx-jsx-mode)
    (flycheck-add-mode 'css-stylelint 'jtsx-tsx-mode)))

(defun cats-javascript/post-init-flycheck ()
  (add-hook 'cats/project-hook 'cats//locate-node-from-projectile)
  (add-hook 'cats/project-hook 'cats//locate-jshint-from-projectile)
  (add-hook 'cats/project-hook 'cats//locate-jscs-from-projectile)
  (add-hook 'cats/eslint-executable-hook
    'cats//esilnt-set-eslint-executable)
  (add-hook 'cats/project-hook 'cats//locate-eslint-from-projectile)

  (spacemacs/enable-flycheck 'jtsx-jsx-mode)
  (spacemacs/enable-flycheck 'jtsx-tsx-mode)
  (spacemacs/enable-flycheck 'jtsx-typescript-mode)

  (with-eval-after-load 'flycheck
    ;; try some CSS-in-JS linting magic
    (flycheck-add-mode 'css-stylelint 'jtsx-jsx-mode)
    (flycheck-add-mode 'css-stylelint 'jtsx-tsx-mode))

  (spacemacs/add-to-hooks #'spacemacs//javascript-setup-checkers
    '(jtsx-jsx-mode-hook)
    t)

  (spacemacs/add-to-hooks #'spacemacs//typescript-setup-checkers
    '(jtsx-tsx-mode-hook
      jtsx-typescript-mode-hook)
    t)

  (spacemacs/add-to-hooks #'cats//typescript-setup-backend
    '(jtsx-typescript-mode-local-vars-hook
      jtsx-tsx-mode-local-vars-hook)
    t)
  )


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

      (dolist (hook '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode rjsx-mode-hook js2-jsx-mode-hook))
        (add-hook hook 'spacemacs/js-doc-require))

      (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode js2-mode js2-jsx-mode rjsx-mode))
        (spacemacs/declare-prefix-for-mode mode "mrd" "jsdoc")
        (spacemacs/js-doc-set-key-bindings mode)))))


;; js2-mode
(defun cats-javascript/pre-init-js2-mode ()
  (spacemacs|use-package-add-hook js2-mode
    :post-init
    (progn
      (dolist (hook '(js2-jsx-mode-hook
                      js2-mode-hook))
        (add-hook hook (lambda () (run-hooks #'cats/javascript-mode-hook))))

      (spacemacs|add-toggle js2-highlight-unused-variables-mode
        :status js2-highlight-unused-variables-mode
        :on (progn
              (when (bound-and-true-p js2-highlight-unused-variables-mode)
                (js2-highlight-unused-variables-mode -1))
              (js2-highlight-unused-variables-mode))
        :off (js2-highlight-unused-variables-mode -1)
        :documentation "Highlight unused js vars."
        :evil-leader-for-mode
        (js2-mode . "Tj")
        (k-mode . "Tj")
        (c-mode . "Tj")
        (c++-mode . "Tj")))
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


;; js2-refactor
(defun cats-javascript/post-init-js2-refactor ()
  (dolist (hook '(;; rjsx-mode-hook
                  js2-jsx-mode-hook))
    (add-hook hook 'spacemacs/js2-refactor-require)))


;; json-mode
(defun cats-javascript/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-config\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jscsrc$\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-ts-mode)))


;; json-reformat
(defun cats-javascript/pre-init-json-reformat ()
  "Reformat JSON."
  (spacemacs|use-package-add-hook json-reformat
    :post-init
    (progn
      (setq json-reformat:indent-width 2)
      (setq json-reformat:pretty-string? t)

      (dolist (mode '(json-ts-mode json-mode))
        (spacemacs/declare-prefix-for-mode mode "mr" "reformat")
        (spacemacs/set-leader-keys-for-major-mode mode
          "rr" 'json-reformat-region)))))


;; livid-mode
(defun cats-javascript/pre-init-livid-mode ()
  (when (eq javascript-repl 'skewer)
    (spacemacs|use-package-add-hook livid-mode
      :post-init
      (progn
        (defalias 'js-live-eval 'livid-mode
          "Minor mode for automatic evaluation of a JavaScript buffer on every change")
        ))))


;; mocha
(defun cats-javascript/init-mocha ()
  (use-package mocha
    :ensure t
    ;; :commands (mocha-run mocha-debug)
    :defer t
    :init
    (progn
      (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode rjsx-mode js2-mode js2-jsx-mode))
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
         'cats//mocha-set-node-executable)

      (add-hook 'cats/mocha-executable-hook
         'cats//set-mocha-executable)

      (push "\\*mocha tests\\*" spacemacs-useful-buffers-regexp)
      (setq mocha-environment-variables "NODE_ENV=test")
      (setq mocha-options "--recursive --reporter dot -t 5000")
      (setq mocha-reporter "spec")
      (setq mocha-project-test-directory "test"))))


;; nodejs-repl
(defun cats-javascript/post-init-nodejs-repl ()
  (when (eq javascript-repl 'nodejs)
    (spacemacs|use-package-add-hook nodejs-repl
      :post-init
      (progn
        (setq nodejs-repl-arguments '())
        (add-hook 'cats/node-executable-hook
          'cats//nodejs-set-node-executable)

        (push "\\*nodejs\\*" spacemacs-useful-buffers-regexp))
      :post-config
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
            (kbd "q") 'quit-window))))))


;; popwin
(defun cats-javascript/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*xref*" :dedicated t :position bottom)
          popwin:special-display-config)
    (push '("*nodejs*" :dedicated t :position bottom)
          popwin:special-display-config)
    ;; indium
    (push '("*node process*" :dedicated t :position bottom :noselect nil)
          popwin:special-display-config)
    ;; (push '("^\\*JS \\(REPL\\|Inspector\\)\\*$"
    ;;         :regexp t
    ;;         :dedicated t
    ;;         :position bottom
    ;;         :stick t)
    ;;       popwin:special-display-config)
    ;; tide
    (push '("*tide-server*"  :position bottom :noselect nil)
          popwin:special-display-config)
    ;; mocha
    (push '("*mocha tests*"                :dedicated t :position bottom)
          popwin:special-display-config)
    (push '("* Mocha Test Output *"        :dedicated t :position bottom
            :stick tc-state :noselect nil :height 0.4)
          popwin:special-display-config)
    ;; babel
    (push '("*babel-shell*"                :dedicated t :position bottom
            :stick t        :noselect nil)
          popwin:special-display-config)
    ))


;; skewer-mode
(defun cats-javascript/pre-init-skewer-mode ()
  (when (eq javascript-repl 'skewer)
    (spacemacs|use-package-add-hook skewer-mode
      :post-init
      (progn
        (dolist (hook '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode rjsx-mode-hook js2-jsx-mode-hook))
          (add-hook hook 'skewer-mode))
        (add-hook 'cats/phantomjs-executable-hook
          'cats//skewer-set-phantomjs-executable)
        (add-hook 'cats/project-hook
          'cats//locate-phantomjs-from-projectile)
        (add-hook 'css-mode-hook 'skewer-css-mode)
        (add-hook 'html-mode-hook 'skewer-html-mode)))))


;; tern
(defun cats-javascript/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'jtsx-jsx-mode))

;; (defun cats-javascript/pre-init-tern ()
;;   "Note this has moved into its own layer now."
;;   (spacemacs|use-package-add-hook tern
;;     :post-config
;;     (spacemacs//set-tern-key-bindings 'rjsx-mode)
;;     :post-init
;;     (cats//locate-tern)))


;; tj-mode
(defun cats-javascript/init-tj-mode ()
  (use-package tj-mode
    :commands tj-mode
    :disabled t
    :init
    (progn
      (dolist (hook '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode rjsx-mode-hook js2-mode-hook js2-jsx-mode-hook))
        (add-hook hook 'tj-mode)))))


;; xref-js2
(defun cats-javascript/init-xref-js2 ()
  (use-package xref-js2
    :ensure t
    :defer t
    :init
    (progn
      ;; (evilified-state-evilify xref-js2-mode xref-js2-mode-map
      ;;   (kbd "q") 'quit-window)

      (dolist (mode '(rjsx-mode js2-mode js2-jsx-mode))
        (spacemacs/declare-prefix-for-mode mode "mj" "jump/join/split")
        (spacemacs/set-leader-keys-for-major-mode mode
          "jg" 'xref-find-definitions
          "jr" 'xref-find-references
          "jl" 'xref-resume-last-search
          "jb" 'xref-pop-marker-stack))

      (dolist (hook '(jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode rjsx-mode-hook js2-jsx-mode-hook js2-mode-hook))
        (add-hook hook
          (lambda ()
            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))))


;; prettier-js
(defun cats-javascript/pre-init-prettier-js ()
  (when (eq javascript-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'jtsx-jsx-mode)
    (add-to-list 'spacemacs--prettier-modes 'jtsx-tsx-mode)
    (add-to-list 'spacemacs--prettier-modes 'jtsx-typescript-mode)))


;; web-beautify
(defun cats-javascript/pre-init-web-beautify ()
  (when (eq javascript-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'jtsx-jsx-mode 'web-beautify-js))))

;;; packages.el ends here
