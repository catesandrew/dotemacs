;;; funcs.el --- cats: Programming

;;; Commentary:

;; Personal functions

;;; Code:


;; funcs
(defun cats/newline-for-code ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun cats/disable-electric-indent-mode ()
  (if (fboundp 'electric-indent-local-mode)
      ;; for 24.4
      (electric-indent-local-mode -1)
    ;; for 24.3
    (add-hook 'electric-indent-functions
              (lambda () 'no-indent) nil 'local)))


;; folding, hs-minor-mode

(defun cats/fold-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((col
           (save-excursion
             (move-end-of-line 0)
             (current-column)))
          (count (count-lines (overlay-start ov) (overlay-end ov))))
      (overlay-put
       ov 'after-string
       (format "%s [ %d ] … "
               (make-string
                (- (window-width) col 32)
                (string-to-char "."))
               count)))))


;; prettify, font-lock

(defun cats/highlight-TODO-words ()
  "Highlight keywords in comments."
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|BUG\\|CHECK\\|DONE\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\\>:?\\)"
          1 font-lock-warning-face t))))

(defun cats/prettify-symbols-auto ()
  "Prettify symbol list by `major-mode'."
  (interactive)
  (cond
   ;; lisp
   ((member major-mode
            '(emacs-lisp-mode
             lisp-mode))
    (cats/pretty-symbols pretty-symbols/elisp))
   ;; javascript
   ((member major-mode
            '(js2-mode
              js-mode
              js-jsx-mode
              js2-jsx-mode
              rjsx-mode
              javascript-mode))
    (cats/pretty-symbols pretty-symbols/js2-width))
   (t
    ;; default
    (cats/pretty-symbols pretty-symbols/prog))))


;; personal prog-mode defaults
(defun add-pragmatapro-prettify-symbols-alist ()
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

;; enable prettified symbols on comments
(defun setup-compose-predicate ()
  (setq prettify-symbols-compose-predicate
    (defun my-prettify-symbols-default-compose-p (start end _match)
      "Same as `prettify-symbols-default-compose-p', except compose symbols in comments as well."
      (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                             '(?w ?_) '(?. ?\\)))
              (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                              '(?w ?_) '(?. ?\\))))
        (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
               (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
               (nth 3 (syntax-ppss))))))))

(defun pragmatapro-prettify ()
  (add-pragmatapro-prettify-symbols-alist)
  (setup-compose-predicate))

(defun cats/prog-mode-defaults ()
  (company-mode)

  "Default coding hook, useful with any programming language."
  (when cats/prog-mode-spell-checking
    (spacemacs/toggle-spelling-checking-off)
    (spacemacs/toggle-spelling-checking-on))

  (spacemacs/toggle-encourage-mode-off)
  (spacemacs/toggle-encourage-mode-on)

  (unless (bound-and-true-p my-pmh-ran)
    ;; add buffer-local indicator for whether prog-mode-hook has run.
    (set (make-local-variable 'my-pmh-ran) t)

    ;; Jump to bug references from code
    (bug-reference-prog-mode)

    ;; cleanup whitespace
    (setq-default dotspacemacs-whitespace-cleanup 'trailing)
    (spacemacs/toggle-whitespace-cleanup-on)
    (spacemacs/toggle-hungry-delete-on)

    ;; disable line wrap
    (spacemacs/toggle-truncate-lines-on)

    (spacemacs/toggle-visual-line-navigation-off)

    ;; TODO: Check and disable visual-fill-column-mode
    ;; (when (bound-and-true-p visual-fill-column-mode)
    ;;   (visual-fill-column-mode--disable))

    ;; (set-syntax-table cats/prog-syntax-table)
    (defadvice evil-inner-word (around bars-as-word activate)
      (let ((table cats/prog-syntax-table))
        (with-syntax-table table
          ad-do-it)))

    (spacemacs/toggle-rainbow-identifier-off)
    (spacemacs/toggle-color-identifiers-mode-on)
    (spacemacs/toggle-color-identifiers-mode-off)
    ;; crashes when opening package.json
    ;; (unless (bound-and-true-p rainbow-mode)
    ;;   (rainbow-mode))

    (spacemacs/toggle-auto-fill-comments-mode-on)
    ;; (cats/highlight-TODO-words)

    ;; Since paredit and other modes automatically insert final characters like
    ;; semi-colons and parenthesis, what I really want is to hit return from the
    ;; end of the line. Pretty simple function. And we can bind that to the
    ;; free, Meta-Return:
    ;; (global-set-key (kbd "M-RET") 'cats/newline-for-code)

    ;; prettify and enable locally
    (pragmatapro-prettify)
    (cats/prettify-symbols-auto)
    (spacemacs/toggle-prettify-symbols-mode-on)
    ))


;; string inflection

(defun cats/string-inflection-cycle-auto ()
  "Switch string inflectin by `major-mode'."
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))


;; compile
(defun cats/next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)))

(defun cats/previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)))


;; lsp

(defun cats/disable-lsp-temporarily ()
  "Disable lsp in current buffer if it was enabled.  Also set a buffer local
variable for later use by `cats/reenable-lsp' to re-enable lsp."
  (when lsp-mode
    (lsp-disconnect) ; disable lsp
    (setq-local cats//lsp-disabled t)))

(defun cats/reenable-lsp ()
  "If current buffer was disabled by `cats/disable-lsp-temporarily' as indicated
by `cats//lsp-disabled' buffer local variable, then enable lsp in current buffer."
    (when (and (bound-and-true-p cats//lsp-disabled) (not lsp-mode))
      (lsp)
      (setq-local cats//lsp-disabled nil)))

(defun cats/turn-off-lsp ()
  "Disable lsp on all existing buffers as well new buffers to be created.  Thus
lsp will be disabled for all buffers until `cats/turn-on-lsp' is called later on."
  (interactive)

  ;; prevent spacemacs from enabling lsp by calling lsp-deferred
  (setq cats/saved-javascript-backend javascript-backend)
  (message "javascript-backend was %s" javascript-backend)
  (setq javascript-backend nil)

  ;; Remove advice added by lsp
  (advice-remove 'set-visited-file-name #'lsp--on-set-visited-file-name)

  ;; Disable lsp in all buffers that have it enabled.  Also note which buffers
  ;; are disabled so that we can re-enable them later on.
  (mode-local-map-file-buffers #'cats/disable-lsp-temporarily)

  ;; Don't know the exact syntax of this variable.  It must be a list.
  ;; So I tried adding "/tmp" which seem to get the job done.
  (setq lsp-enabled-clients '("/tmp")))

(defun cats/turn-on-lsp ()
  "Re-enable lsp which was disabled by prior call of `cats/turn-off-lsp'.
Lsp is re-enabled on all existing buffers if it was disabled earlier."
  (interactive)
  (setq javascript-backend cats/saved-javascript-backend)
  (advice-add 'set-visited-file-name :around #'lsp--on-set-visited-file-name)
  (setq lsp-enabled-clients nil)
  (mode-local-map-file-buffers #'cats/reenable-lsp))

;;; funcs.el ends here
