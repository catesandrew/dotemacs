;;; Clojure Layer packages File for Spacemacs

(setq cats-clojure-packages
  '(
     ))

;; Safe structural editing for all major modes
;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure configurations
;;
;;
;; CIDER 0.23 Lima release options
;; Configure the position of evaluation result
;; By default the result displays at the end of the current line
;; Set cider-result-overlay-position to `at-point' to display results right after the expression evaluated
;; Useful for evaluating nexsted expressions with `, e e'
;; (setq cider-result-overlay-position 'at-point)
;;
;;
;; Pretty print in Clojure to use the Fast Idiomatic Pretty-Printer. This is approximately 5-10x faster than clojure.core/pprint
;; (setq cider-pprint-fn 'fipp)
;;
;;
;; Indentation of function forms
;; https://github.com/clojure-emacs/clojure-mode#indentation-of-function-forms
;; (setq clojure-indent-style 'align-arguments)
;;
;; Vertically align s-expressions
;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
;; (setq clojure-align-forms-automatically t)
;;
;; anakondo - static analysis using clj-kondo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/didibus/anakondo
;; Provides auto-completion without the need for a REPL
;; Add anakondo to `dotspacemacs-additional-packages` list
;;
;; `SPC SPC anakondo-minor-mode' to run manually for the current project.
;;
;; Commented until static analysis is an optional or background process
;; https://github.com/didibus/anakondo/issues/1
;;
;; Lazy load of anakondo until Clojure buffer is used
;; (autoload 'anakondo-minor-mode "anakondo")
;;
;; Enable anakondo-minor-mode in all Clojure buffers
;; (add-hook 'clojure-mode-hook #'anakondo-minor-mode)
;; Enable anakondo-minor-mode in all ClojureScript buffers
;; (add-hook 'clojurescript-mode-hook #'anakondo-minor-mode)
;; Enable anakondo-minor-mode in all cljc buffers
;; (add-hook 'clojurec-mode-hook #'anakondo-minor-mode)
;;
;; LSP server for Clojure with clj-kondo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An alternative approach to the Clojure layer variable clojure-enable-linters 'clj-kondo
;; for those environments where the clj-kondo binary does not run (eg. graal).
;; Uses a custom script to run the clj-kondo-lsp-server.jar which should be added
;; to the operating system path and include:
;; java -jar ~/path/to/clj-kondo-lsp-server-standalone.jar
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((clojure-mode . lsp))
;;   :commands lsp
;;   :custom ((lsp-clojure-server-command '("clojure-lsp-server-clj-kondo")))
;;   :config (dolist  (m '(clojure-mode clojurescript-mode))
;;             (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))
;;
;; Turn on command-log-mode when opening a source code or text file
;; (add-hook 'clojure-mode-hook 'command-log-mode)
;; (add-hook 'prog-mode-hook 'command-log-mode)
;; (add-hook 'text-mode-hook 'command-log-mode)
;;
;;
;; end of clojure configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
