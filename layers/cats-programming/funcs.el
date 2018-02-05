;;; funcs.el --- cats: Programming

;;; Commentary:

;; Personal functions

;;; Code:


;; funcs

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
              react-mode
              javascript-mode))
    (cats/pretty-symbols pretty-symbols/js2))
   (t
    ;; default
    (cats/pretty-symbols pretty-symbols/prog))))



;; personal prog-mode defaults

(defun cats/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when cats/prog-mode-spell-checking
    (flyspell-prog-mode))

  (unless (bound-and-true-p my-pmh-ran)
    ;; add buffer-local indicator for whether prog-mode-hook has run.
    (set (make-local-variable 'my-pmh-ran) t)

    ;; disable line wrap
    (unless (bound-and-true-p truncate-lines)
      ;; (set (make-local-variable 'truncate-lines) t)
      (setq truncate-lines t))

    ;; enable line number-mode
    (when cats/line-numbers
      (unless (bound-and-true-p linum-mode)
        (spacemacs/toggle-line-numbers-on)))

    (when (bound-and-true-p visual-line-mode)
      (setq visual-line-mode nil))

    ;; TODO: Check and disable visual-fill-column-mode
    ;; (when (bound-and-true-p visual-fill-column-mode)
    ;;   (visual-fill-column-mode--disable))

    ;; (smartparens-mode +1)

    ;; (set-syntax-table cats/prog-syntax-table)
    (defadvice evil-inner-word (around bars-as-word activate)
      (let ((table cats/prog-syntax-table))
        (with-syntax-table table
          ad-do-it)))

    (spacemacs/toggle-auto-fill-comments-mode-on)
    ;; (subword-mode +1) ;; camelCase
    (cats/highlight-TODO-words)

    ;; prettify and enable locally
    (cats/prettify-symbols-auto)
    (spacemacs/toggle-prettify-symbols-mode-on)))


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

;;; funcs.el ends here
