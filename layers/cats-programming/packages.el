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
     ;; helm-fontawesome
     logview
     (prog-mode :location built-in)
     ;; realgud
     ;; (realgud-node-inspect :location
     ;;   (recipe
     ;;     :fetcher github
     ;;     :repo "realgud/realgud-node-inspect"))
     shut-up
     string-inflection
     polymode
     hcl-mode
     ;; treesit-auto
     ;; code-review
     lsp-mode
     ;; (combobulate :location
     ;;   (recipe :fetcher github
     ;;     :repo "mickeynp/combobulate"))
    ))


;; treesit
(defun cats-programming/init-treesit-auto ()
  (use-package treesit-auto
    :ensure t
    :config
    (global-treesit-auto-mode)))


;; lsp-mode
(defun cats-programming/pre-init-lsp-mode ()
  (spacemacs|use-package-add-hook lsp-mode
    :post-config
    (progn
      ;; enable log only for debug
      (setq lsp-log-io nil)

      ;; use `evil-matchit' instead
      (setq lsp-enable-folding nil)

      ;; https://github.com/emacs-lsp/lsp-mode/issues/3173
      (setq lsp-completion-provider :none)
      (setq lsp-completion-show-detail nil)
      (setq lsp-completion-show-kind nil)

      ;; no real time syntax check
      (setq lsp-diagnostic-package :none)

      (setq lsp-keep-workspace-alive nil)

      ;; handle yasnippet by myself
      (setq lsp-enable-snippet nil)

      ;; ;; use `company-ctags' only.
      ;; ;; Please note `company-lsp' is automatically enabled if installed
      ;; (setq lsp-enable-completion-at-point nil)

      ;; turn off for better performance
      (setq lsp-enable-symbol-highlighting nil)

      ;; use ffip instead
      (setq lsp-enable-links nil)

      ;; auto restart lsp
      (setq lsp-restart 'auto-restart)

      ;; don't ping LSP lanaguage server too frequently
      (defvar lsp-on-touch-time 0)

      (setq lsp-ui-sideline-enable nil)
    )
    :post-init
    (progn
      ;; @see https://github.com/emacs-lsp/lsp-mode/pull/1498 and code related to auto configure.
      ;; Require clients could be slow, I only load ones which I'm interested
      (setq lsp-client-packages '(lsp-angular lsp-ansible lsp-bash lsp-cmake lsp-css lsp-d lsp-dart lsp-docker lsp-dockerfile lsp-emmet lsp-eslint lsp-go lsp-graphql lsp-grammarly lsp-java lsp-javascript lsp-json lsp-kotlin lsp-latex lsp-ltex lsp-lua lsp-markdown lsp-marksman lsp-nginx lsp-nix lsp-magik lsp-ocaml lsp-perl lsp-php lsp-pyls lsp-pylsp lsp-pyright lsp-python-ms lsp-purescript lsp-r lsp-racket lsp-remark lsp-rust lsp-solargraph lsp-tailwindcss lsp-tex lsp-terraform lsp-toml lsp-vhdl lsp-vimscript lsp-xml lsp-yaml lsp-ruby-syntax-tree lsp-sqls lsp-svelte lsp-steep))

      ;; https://github.com/emacs-lsp/lsp-mode/issues/3173
      (setq lsp-completion-provider :none)
      (setq lsp-completion-show-detail nil)
      (setq lsp-completion-show-kind nil)

      ;; (dolist (hook '(
      ;;                  c++-mode-hook
      ;;                  c-mode-hook
      ;;                  cc-mode-hook
      ;;                  css-mode-hook
      ;;                  go-mode-hook
      ;;                  go-ts-mode-hook
      ;;                  html-mode-hook
      ;;                  inf-ruby-mode-hook
      ;;                  java-mode-hook
      ;;                  javascript-mode-hook
      ;;                  jquery-mode-hook
      ;;                  js-mode-hook
      ;;                  js2-jsx-mode-hook
      ;;                  js2-mode-hook
      ;;                  js3-mode-hook
      ;;                  jsp-mode-hook
      ;;                  jsx-mode-hook
      ;;                  latex-mode-hook
      ;;                  less-css-mode-hook
      ;;                  markdown-mode-hook
      ;;                  perl-mode-hook
      ;;                  php-mode-hook
      ;;                  python-mode-hook
      ;;                  react-mode-hook
      ;;                  rjsx-mode-hook
      ;;                  ruby-mode-hook
      ;;                  rust-mode-hook
      ;;                  sass-mode-hook
      ;;                  scss-mode-hook
      ;;                  shell-script-mode-hook
      ;;                  swift-mode-hook
      ;;                  shell-script-mode-hook
      ;;                  tsx-mode-hook
      ;;                  typescript-mode-hook
      ;;                  typescript-tsx-mode-hook
      ;;                  web-mode-hook
      ;;                  xah-css-mode-hook
      ;;                  yaml-mode-hook
      ;;                  ))
      ;;   (add-hook hook #'lsp-deferred)
      ;; )
    )
  )
)



;; combobulate
(defun cats-programming/init-combobulate ()
  (use-package combobulate
    :ensure t
    :hook ((python-ts-mode . combobulate-mode)
            (js-ts-mode . combobulate-mode)
            (css-ts-mode . combobulate-mode)
            (yaml-ts-mode . combobulate-mode)
            (typescript-ts-mode . combobulate-mode)
            (tsx-mode . combobulate-mode)
            (js2-mode . combobulate-mode)
            (tsx-ts-mode . combobulate-mode))
    ;;(prog-mode . combobulate-mode)
    :init
    (progn
      (setq combobulate-js-ts-enable-auto-close-tag nil)
      )
    ))


;; shut-up
(defun cats//shut-up-around (function &rest args)
  (shut-up (apply function args)))

(defun cats-programming/init-shut-up ()
  (use-package shut-up
    :commands (shut-up)))

(defun cats-programming/init-hcl-mode ()
  (use-package hcl-mode
    :mode "\\.nomad\\'"))


;; code-review
;; https://github.com/wandersoncferreira/code-review/issues/245
;; (defun cats-programming/init-code-review ()
;;   (use-package code-review
;;     :after (magit forge)
;;     :init
;;     (with-eval-after-load 'evil-collection-magit
;;       ;; From Doom Emacs
;;       (dolist (binding evil-collection-magit-mode-map-bindings)
;;         (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
;;           (dolist (state states)
;;             (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
;;       (evil-set-initial-state 'code-review-mode evil-default-state))
;;     :config
;;     (progn
;;       (evil-make-overriding-map code-review-mode-map evil-default-state)
;;       (setq code-review-auth-login-marker 'forge)
;;       (add-hook 'code-review-mode-hook
;;         (lambda ()
;;           ;; include *Code-Review* buffer into current workspace
;;           (persp-add-buffer (current-buffer))))
;;       ;; From Doom Emacs
;;       (defun magit/start-code-review (arg)
;;         (interactive "P")
;;         (call-interactively
;;           (if (or arg (not (featurep 'forge)))
;;             #'code-review-start
;;             #'code-review-forge-pr-at-point)))

;;       (transient-append-suffix 'magit-merge "i"
;;         '("y" "Review pull request" magit/start-code-review))
;;       (with-eval-after-load 'forge
;;         (transient-append-suffix 'forge-dispatch "c u"
;;           '("c r" "Review pull request" magit/start-code-review))))))


;; polymode
(defun cats-programming/init-polymode ()
  (use-package polymode
    :commands (poly-js2-mode poly-rjsx-mode)
    :init
    (progn
      (spacemacs|add-toggle poly-rjsx-mode
        :status poly-rjsx-mode
        :on (progn
              (when (bound-and-true-p poly-rjsx-mode)
                (poly-rjsx-mode -1))
              (poly-rjsx-mode))
        :off (poly-rjsx-mode -1)
        :documentation "Poly RJSX mode."
        :evil-leader-for-mode
        (rjsx-mode . "Tp"))

      (spacemacs|add-toggle poly-js2-mode
        :status poly-js2-mode
        :on (progn
              (when (bound-and-true-p poly-js2-mode)
                (poly-js2-mode -1))
              (poly-js2-mode))
        :off (poly-js2-mode -1)
        :documentation "Poly JS2 mode."
        :evil-leader-for-mode
        (js2-mode . "Tp"))
      )
    :config
    (progn
      (define-innermode poly-cats-root-innermode
        :mode nil
        :fallback-mode 'host
        :head-mode 'host
        :tail-mode 'host)

      (define-innermode poly-cats-css-innermode poly-cats-root-innermode
        :mode 'css-mode
        :head-matcher "css\`\\|styled\.[[:alnum:]]+\`"
        :tail-matcher "\`"
        :allow-nested nil)

      (define-innermode poly-cats-graphql-innermode poly-cats-root-innermode
        :mode 'graphql-mode
        :head-matcher "gql\`"
        :tail-matcher "\`"
        :allow-nested nil)

      (define-hostmode poly-rjsx-hostmode nil
        "RJSX hostmode."
        :mode 'rjsx-mode)

      (define-polymode poly-rjsx-mode
        :hostmode 'poly-rjsx-hostmode
        :innermodes '(poly-cats-graphql-innermode poly-cats-css-innermode))

      (define-hostmode poly-js2-hostmode poly-rjsx-hostmode
        "JS2 hostmode"
        :mode 'js2-mode)

      (define-polymode poly-js2-mode
        :hostmode 'poly-js2-hostmode
        :innermodes '(poly-cats-graphql-innermode poly-cats-css-innermode))
      )
    )
  )


;; helm-fontawesome
(defun cats-programming/init-helm-fontawesome ()
  (use-package fontawesome
    :disabled t
    :ensure t
    :commands (helm-fontawesome)))


;; realgud-node-inspect
(defun cats-programming/init-realgud-node-inspect ()
  (use-package realgud-node-inspect
    :disabled t
    :after (realgud)
    :init
    (progn
      (let ((default-directory (configuration-layer/get-elpa-package-install-directory 'realgud-node-inspect)))
        (compile (format "EMACSLOADPATH=:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.el"))))))
    :config
    (progn)))


;; realgud
(defun cats-programming/init-realgud()
  (use-package realgud
    :disabled t
    :commands (realgud:node-inspect realgud:gdb realgud:nodejs-set-breakpoint)
    :init
    (progn
      ;; This one is to represent `realgud-populate-src-buffer-map-plain'.
      (evilified-state-evilify-map realgud:shortkey-mode-map
        :eval-after-load realgud
        :mode realgud-short-key-mode
        :bindings
        "b" 'realgud:cmd-break
        "u" 'realgud:cmd-delete
        "X" 'realgud:cmd-clear
        "-" 'realgud:cmd-disable
        "+" 'realgud:cmd-enable
        "T" 'realgud:cmd-backtrace
        "f" 'realgud:cmd-finish
        "n" 'realgud:cmd-next
        "q" 'realgud:cmd-quit
        "Q" 'realgud:cmd-kill
        "r" 'realgud:cmd-restart
        "R" 'realgud:cmd-restart
        "s" 'realgud:cmd-step
        "i" 'realgud:cmd-step
        "!" 'realgud:cmd-shell))
    :config
    (progn
      )))


;; logview
(defun cats-programming/init-logview ()
  (use-package logview
    :commands log-view
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'log-view-mode
        "q" 'quit-window
        (kbd "RET") 'log-view-toggle-entry-display
        "m" 'log-view-toggle-mark-entry
        "c" 'log-view-modify-change-comment
        "d" 'log-view-diff
        "=" 'log-view-diff
        "D" 'log-view-diff-changeset
        "a" 'log-view-annotate-version
        "F" 'log-view-find-revision
        "gj" 'log-view-msg-next
        "gk" 'log-view-msg-prev
        "]" 'log-view-msg-next
        "[" 'log-view-msg-prev
        (kbd "C-j") 'log-view-file-next
        (kbd "C-k") 'log-view-file-prev)
      )
    :config
    (progn
      ;; (evil-set-initial-state 'log-view-mode 'normal)
      (evil-define-key 'normal log-view-mode-map
        "q" 'quit-window
        (kbd "RET") 'log-view-toggle-entry-display
        "m" 'log-view-toggle-mark-entry
        "c" 'log-view-modify-change-comment
        "d" 'log-view-diff
        "=" 'log-view-diff
        "D" 'log-view-diff-changeset
        "a" 'log-view-annotate-version
        "F" 'log-view-find-revision
        "gj" 'log-view-msg-next
        "gk" 'log-view-msg-prev
        "]" 'log-view-msg-next
        "[" 'log-view-msg-prev
        (kbd "C-j") 'log-view-file-next
        (kbd "C-k") 'log-view-file-prev))))


;; compile
(defun cats-programming/post-init-compile ()
  ;; if you want the compilation buffer to stop scrolling so that the first error
  ;; is visible, set compilation-scroll-output to 'first-error
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  ;; auto scroll compilation buffer
  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (setq compilation-environment '("TERM=xterm-256color"))

  (spacemacs/set-leader-keys-for-major-mode 'compilation-mode
    "?" 'describe-mode
    ;; "?" evil-collection-evil-search-backward
    ;; "gg" 'evil-goto-first-line
    ;; "0" 'evil-digit-argument-or-evil-beginning-of-line
    "RET" 'compile-goto-error
    "S-RET" 'compilation-display-error
    "C-o" 'compilation-display-error
    "C-j" 'compilation-next-error
    "C-J" 'cats/next-error
    "C-k" 'compilation-previous-error
    "C-K" 'cats/previous-error
    "[" 'compilation-previous-file
    "]" 'compilation-next-file
    "r" 'recompile))

(defvar java-stack-trace-dir "src/")
(defun java-stack-trace-regexp-to-filename ()
  "Generates a relative filename from java-stack-trace regexp match data."
  (concat java-stack-trace-dir
    (replace-regexp-in-string "\\." "/" (match-string 1))
    (match-string 2)))

(defun cats-programming/pre-init-compile ()
  (spacemacs|use-package-add-hook compile
    :post-config
    (progn
      ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
      ;; TODO output json lines if possible

      ;; ;; ansi-color version
      ;; (add-hook 'compilation-filter-hook
      ;;    (lambda () (ansi-color-process-output nil)))

      ;; ;; xterm-color version
      ;; (add-hook 'compilation-start-hook
      ;;   (lambda (proc)
      ;;     ;; We need to differentiate between compilation-mode buffers
      ;;     ;; and running as part of comint (which at this point we assume
      ;;     ;; has been configured separately for xterm-color)
      ;;     (when (eq (process-filter proc) 'compilation-filter)
      ;;       ;; This is a process associated with a compilation-mode buffer.
      ;;       ;; We may call `xterm-color-filter' before its own filter function.
      ;;       (set-process-filter
      ;;         proc
      ;;         (lambda (proc string)
      ;;           (funcall 'compilation-filter proc
      ;;             (xterm-color-filter string)))))))

      ;; Writing that regexp would have been hard, but of course Emacs has a
      ;; solution. First I found the current value of
      ;; compilation-error-regexp-alist-alist using describe-variable in order
      ;; to get an example regexp to start with. Then, in the compilation buffer
      ;; where my stack trace was, I ran re-builder, which pops up a little mini
      ;; window where I could tweak my regexp and see immediately whether or not
      ;; it was matching correctly. Then I just added this to my init.el,
      ;; evaluated it with eval-region, and my next compile had the errors
      ;; highlighted correctly, and I could immediately go to the offending code
      ;; with next-error or by putting the point on the stack trace line in the
      ;; compilation buffer and pressing RET.

      ;; Add NodeJS error format
      (add-to-list 'compilation-error-regexp-alist 'node)
      (add-to-list 'compilation-error-regexp-alist-alist
        ;; also try '(node "^File \"\\(.*\\)\", line \\([[:digit:]]+\\):$" 1 2)
        '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                 1 ;; file
                 2 ;; line
                 3 ;; column
                 ))

      ;; Add mocha
      (add-to-list 'compilation-error-regexp-alist 'mocha)
      (add-to-list 'compilation-error-regexp-alist-alist
        '(mocha "at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3))

      ;; Add Java/Node stack Trace
      (add-to-list 'compilation-error-regexp-alist 'java-stack-trace)
      (add-to-list 'compilation-error-regexp-alist-alist
        '(java-stack-trace .
           ("^[[:space:]]*at \\(\\(?:[[:lower:]]+\\.\\)+\\)[^(]+(\\([[:alnum:]]+\\.java\\):\\([[:digit:]]+\\))"
             java-stack-trace-regexp-to-filename 3)))

      ;; ESLint needs to be ran with `eslint . --format unix`
      (add-to-list 'compilation-error-regexp-alist-alist
        '(eslint "^\\(\w+\\):\\([0-9]+\\):\\([0-9]+\\):.*$" 1 2 3))
      (add-to-list 'compilation-error-regexp-alist 'eslint)

      ;; JSHint
      (add-to-list 'compilation-error-regexp-alist-alist
        '(jshint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), " 1 2 3))
      (add-to-list 'compilation-error-regexp-alist 'jshint)

      (evil-set-initial-state 'compilation-mode 'normal)
      (evil-define-key 'normal compilation-mode-map
        "g?" 'describe-mode
        (kbd "<return>") 'compile-goto-error
        "go" 'compilation-display-error
        (kbd "C-o") 'compilation-display-error
        (kbd "S-<return>") 'compilation-display-error
        "gj" 'compilation-next-error
        "gk" 'compilation-previous-error
        (kbd "C-j") 'compilation-next-error
        (kbd "C-J") 'cats/next-error
        (kbd "C-k") 'compilation-previous-error
        (kbd "C-K") 'cats/previous-error
        "[" 'compilation-previous-file
        "]" 'compilation-next-file
        "gr" 'recompile)
      )))

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
      (add-hook 'prog-mode-hook (lambda () (run-hooks 'cats/prog-mode-hook))))))

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
