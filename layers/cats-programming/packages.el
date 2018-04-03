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
     log-view
     (prog-mode :location built-in)
     ;; realgud
     ;; (realgud-node-inspect :location
     ;;   (recipe
     ;;     :fetcher github
     ;;     :repo "realgud/realgud-node-inspect"))
     shut-up
     string-inflection
     ycmd
    ))


;; shut-up
(defun cats//shut-up-around (function &rest args)
  (shut-up (apply function args)))

(defun cats-programming/init-shut-up ()
  (use-package shut-up
    :commands (shut-up)))


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


;; ycmd
(defun cats-programming/pre-init-ycmd ()
  (spacemacs|use-package-add-hook ycmd
    :post-init
    (setq ycmd-server-command cats/ycmd-server-command)))


;; logview
(defun cats-programming/init-log-view ()
  (use-package log-view
    :commands log-view
    :disabled t
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
        (kbd "C-k") 'log-view-file-prev)
      )))


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
