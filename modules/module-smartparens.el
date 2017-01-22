;;; module-smartparens.el --- Smart Parens Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;; Personal smartparens configuration.
;;
;; Extends the default `smartparens-config' with key-bindings and
;; additional pairs.
;;
(require 'use-package)
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-fonts-support)
(require 'core-keybindings)
(require 'core-toggle)
;; (require 'core-display-init)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defun dotemacs-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(defun dotemacs-sp-kill-surrounding-sexp ()
  "Kill from beginning to end of sexp."
  (interactive)
  (kill-region (progn (sp-beginning-of-sexp)
                 (1- (point)))
    (progn (sp-end-of-sexp)
      (1+  (point)))))

(defun dotemacs-gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

(defun dotemacs-org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

(defun dotemacs-after-symbol-p (_id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_\\|\\s'"))))

(defun dotemacs-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args))))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))

(define-minor-mode evil-sp-mode
  "Minor mode for combining evil and smartparens."
  :init-value nil
  :keymap (make-sparse-keymap) ; defines evil-sp-mode-map
  :lighter " ESP")
(add-hook 'smartparens-enabled-hook 'evil-sp-mode) ;; only load with smartparens

(dotemacs-use-package-add-hook smartparens
  :post-config
  (progn
    ;; This only makes current evil commands work better with smartparens.
    (defun dotemacs/evil-sp-evilize-name (name)
      "Return an interned symbol NAME prefixed with 'evil-'."
      (intern (format "evil-%s" name)))

    (defmacro dotemacs/evil-sp-make-evil-smartparens-movement (name)
      "Create Evil movement command from NAME function."
      `(let ((evil-name (dotemacs/evil-sp-evilize-name ,name))
             (doc (documentation ,name)))
         (evil-define-motion evil-name (count)
           doc
           :type inclusive
           :jump t
           (,name count))))

    (defun dotemacs/evil-sp-beginning-of-previous-sexp (&optional arg)
      "Goto beginning of previous sexp.
If ARG is non-nil, go back ARG sexps."
      (interactive "P")
      (setq arg (or arg 1))
      (sp-previous-sexp arg)
      (sp-backward-sexp))

    (defun dotemacs/evil-sp-smartparens-config ()
      "Bind smarparens commands."
      (interactive)
      (let ((sexp-motions
             '(("sh" . sp-backward-sexp)
               ("sj" . sp-down-sexp)
               ("sk" . sp-backward-up-sexp)
               ("sl" . sp-forward-sexp)

               ("sn" . sp-next-sexp)
               ("sp" . dotemacs/evil-sp-beginning-of-previous-sexp)
               ("sa" . sp-beginning-of-sexp)
               ("se" . sp-end-of-sexp)

               ("sH" . sp-beginning-of-previous-sexp)
               ("sJ" . sp-up-sexp)
               ("sK" . sp-backward-down-sexp)
               ("sL" . sp-beginning-of-next-sexp)

               ;; sp-skip-forward-to-symbol (&optional stop-at-string stop-after-string)
               ;; sp-skip-backward-to-symbol (&optional stop-at-string stop-after-string)

               ("s\C-l" . sp-forward-symbol)
               ("s\C-L" . sp-skip-forward-to-symbol)
               ("s\C-h" . sp-backward-symbol)
               ("s\C-H" . sp-skip-backward-to-symbol)))

            (sexp-modifications
             '(("st" . sp-transpose-sexp) ;; or `sp-transpose-hybrid-sexp`
               ("sd" . sp-kill-hybrid-sexp)
               ("su" . sp-unwrap-sexp)
               ("sb" . sp-backward-unwrap-sexp)

               ;; prefix: sf
               ("sfh" . sp-backward-slurp-sexp)
               ("sfH" . sp-backward-barf-sexp)

               ("sfl" . sp-forward-slurp-sexp) ;; or `sp-slurp-hybrid-sexp`
               ("sfL" . sp-forward-barf-sexp)

               ;; prefix: sg
               ("sgL" . sp-add-to-previous-sexp)
               ("sgH" . sp-add-to-next-sexp)

               ;; prefix: ss
               ("ssw" . sp-swap-enclosing-sexp)
               ("sss" . sp-splice-sexp)
               ("ssdl" . sp-splice-sexp-killing-forward)
               ("ssdh" . sp-splice-sexp-killing-backward)
               ("ssda" . sp-splice-sexp-killing-around)

               ("ssc" . sp-convolute-sexp)

               ("ssh" . sp-absorb-sexp)
               ("ssl" . sp-emit-sexp)

               ("ssH" . sp-extract-before-sexp)
               ("ssL" . sp-extract-after-sexp)

               ("ssy" . sp-split-sexp)
               ("ssY" . sp-join-sexp))))

        (loop for (key . func) in sexp-motions
              do
              ;; Define the motion command
              (dotemacs/evil-sp-make-evil-smartparens-movement func)

              (mapc (lambda (state)
                      (evil-define-key state evil-sp-mode-map key func))
                    '(normal visual motion)))

        (loop for (key . func) in sexp-modifications
              do
              (evil-define-key 'normal evil-sp-mode-map
                key func))
        ))
    (dotemacs/evil-sp-smartparens-config)))

;; Use SmartParens instead of Paredit and Electric Pair
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :defer t
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :init
  (progn
    (setq sp-base-key-bindings 'sp)
    ;; TODO move these hooks into their layers
    ;; (dolist (hook '(LaTeX-mode-hook web-moode-hook inferior-python-mode-hook))
    ;;   (add-hook hook #'smartparens-mode))

    (dotemacs/add-to-hooks (if dotemacs-smartparens-strict-mode
                                'smartparens-strict-mode
                              'smartparens-mode)
                           '(prog-mode-hook comint-mode-hook))

    ;; enable smartparens-mode in `eval-expression'
    (defun conditionally-enable-smartparens-mode ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
      (if (eq this-command 'eval-expression)
          (smartparens-mode)))
    (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

    (dotemacs-add-toggle smartparens
      :status smartparens-mode
      :on (smartparens-mode)
      :off (smartparens-mode -1)
      :documentation "Enable smartparens."
      :evil-leader "tp")

    (dotemacs-add-toggle smartparens-globally
      :status smartparens-mode
      :on (smartparens-global-mode)
      :off (smartparens-global-mode -1)
      :documentation "Enable smartparens globally."
      :evil-leader "t C-p")

    (setq sp-show-pair-delay 0.2
          sp-autoskip-closing-pair 'always ; https://github.com/Fuco1/smartparens/issues/142
          ; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil)

    (dotemacs-set-leader-keys
     "js" 'sp-split-sexp
     "jn" 'sp-newline))
  :config
  (progn
    (require 'smartparens-config)
    (dotemacs-diminish smartparens-mode " ⓟ" " p")

    (show-smartparens-global-mode +1)

    (defun dotemacs/smartparens-pair-newline (id action context)
      (save-excursion
        (newline)
        (indent-according-to-mode)))

    (defun dotemacs/smartparens-pair-newline-and-indent (id action context)
      (dotemacs/smartparens-pair-newline id action context)
      (indent-according-to-mode))

    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (sp-pair "{" nil :post-handlers
             '(:add (dotemacs/smartparens-pair-newline-and-indent "RET")))
    (sp-pair "[" nil :post-handlers
             '(:add (dotemacs/smartparens-pair-newline-and-indent "RET")))

    (defun dotemacs/smart-closing-parenthesis ()
      (interactive)
      (let* ((sp-navigate-close-if-unbalanced t)
             (current-pos (point))
             (current-line (line-number-at-pos current-pos))
             (next-pos (save-excursion
                         (sp-up-sexp)
                         (point)))
             (next-line (line-number-at-pos next-pos)))
        (cond
         ((and (= current-line next-line)
               (not (= current-pos next-pos)))
          (sp-up-sexp))
         (t
          (insert-char ?\))))))
    (when dotemacs-smart-closing-parenthesis
      (define-key evil-insert-state-map ")" 'dotemacs/smart-closing-parenthesis))

    ;;; TODO: Move these additional pairs for various modes into their modules
    (sp-with-modes '(php-mode)
      (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                                 (dotemacs-php-handle-docstring "RET")))
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

    (sp-with-modes '(scala-mode)
      (sp-local-pair "'" nil :actions nil))

    (sp-with-modes '(text-mode)
      (sp-local-pair "`" "'" :actions '(insert wrap)))

    (sp-with-modes '(racket-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" nil :actions nil))

    ;; TODO research sp-local-tag or evil-surround with modes
    (sp-with-modes '(tex-mode
                     plain-tex-mode
                     latex-mode)
      ; (sp-local-tag "i" "\"<" "\">")
      (sp-local-pair "$" " $")
      (sp-local-pair "\\[" " \\]")
      (sp-local-pair "\\(" " \\)")
      (sp-local-pair "\\{" " \\}")
      (sp-local-pair "\\left(" " \\right)")
      (sp-local-pair "\\left\\{" " \\right\\}"))

    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'dotemacs-org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

    (sp-with-modes '(markdown-mode
                     gfm-mode
                     rst-mode)
      ; (sp-local-tag "2" "**" "**")
      ; (sp-local-tag "s" "```scheme" "```")
      ; (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags)
      (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'dotemacs-gfm-skip-asterisk)
      (sp-local-pair "_" "_" :wrap "C-_"))

    (sp-with-modes '(rust-mode)
      ;; Don't pair lifetime specifiers
      (sp-local-pair "'" nil :actions nil))

    (sp-with-modes '(malabar-mode c++-mode)
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

    (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                        ("* ||\n[i]" "RET")))
    (sp-with-modes '(haskell-mode)
      (sp-local-pair "'" nil :unless '(dotemacs-after-symbol-p))
      (sp-local-pair "\\(" nil :actions nil))

    ;; Emacs Lisp
    (sp-with-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     lisp-interaction-mode
                     lisp-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "(" nil :bind "M-(")
      (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))))

(provide 'module-smartparens)
;;; module-smartparens.el ends here
