;;; module-programming.el --- Programming Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:
(require 'use-package)
;; (require 'core-vars)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'core-display-init)
(require 'core-fonts-support)
(require 'core-toggle)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;; variables

(defvar dotemacs/prog-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    ;; dash "-" is now a word character in programming mode
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    table))

(define-minor-mode auto-fill-comments-mode
  "Toggle `auto-fill-mode` to auto-fill comments."
  :lighter nil
  :keymap nil
  (cond
   (auto-fill-comments-mode
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))
   (:else
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode -1))))

(dotemacs-add-toggle auto-fill-comments-mode
  :status auto-fill-function
  :on (auto-fill-comments-mode)
  :off (auto-fill-comments-mode -1)
  :documentation "Break line beyond `current-fill-column` in comments only, while editing."
  :evil-leader "tg")

;; funcs

(defun dotemacs/pretty-symbols (new-pretty-symbols)
  (mapcar (lambda (item)
            (push item prettify-symbols-alist))
          new-pretty-symbols))

(defun dotemacs-disable-electric-indent-mode ()
  (if (fboundp 'electric-indent-local-mode)
      ;; for 24.4
      (electric-indent-local-mode -1)
    ;; for 24.3
    (add-hook 'electric-indent-functions
              (lambda () 'no-indent) nil 'local)))

(defun dotemacs-fold-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((col (save-excursion
                 (move-end-of-line 0)
                 (current-column)))
          (count (count-lines (overlay-start ov) (overlay-end ov))))
      (overlay-put ov 'after-string
                   (format "%s [ %d ] … "
                           (make-string (- (window-width) col 32) (string-to-char "."))
                           count)))))

;; required for evil folding
(defun dotemacs-enable-hs-minor-mode ()
  "Enable hs-minor-mode for code folding."
  (ignore-errors
    (hs-minor-mode)
    (dotemacs-hide-lighter hs-minor-mode)))

(defun dotemacs-highlight-TODO-words ()
  "Highlight keywords in comments."
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|BUG\\|CHECK\\|DONE\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\\>:?\\)"
          1 font-lock-warning-face t))))

;; To augment and/or counteract these defaults your own function
;; to dotemacs-prog-mode-hook, using:
;;
;; (add-hook 'dotemacs-prog-mode-hook 'dotemacs-prog-mode-defaults t)
;;
;; (the final optional t sets the *append* argument)

(defun dotemacs-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."

  (when spell-checking-enable-by-default
    (flyspell-prog-mode))

  (unless (bound-and-true-p my-pmh-ran)
    ;; add buffer-local indicator for whether prog-mode-hook has run.
    (set (make-local-variable 'my-pmh-ran) t)

    ;; disable line wrap
    (unless (bound-and-true-p truncate-lines)
      ; (set (make-local-variable 'truncate-lines) t)
      (setq truncate-lines t))

    ;; enable line number-mode
    (when dotemacs-line-numbers
      (unless (bound-and-true-p linum-mode)
        (linum-mode)))

    (when (bound-and-true-p visual-line-mode)
      (setq visual-line-mode nil))

    ; TODO: Check and disable visual-fill-column-mode
    ; (when (bound-and-true-p visual-fill-column-mode)
    ;   (visual-fill-column-mode--disable))

    ; (smartparens-mode +1)

    ; how-to-check-whether-a-minor-mode-e-g-flymake-mode-is-on
    ; http://stackoverflow.com/questions/10088168/
    ; however, hs-minor-mode already set in `init.el`
    ; (unless (bound-and-true-p hs-minor-mode)
    ;   (hs-minor-mode t))
    (dotemacs-enable-hs-minor-mode)
    ;; (set-syntax-table dotemacs/prog-syntax-table)
    ;; (defadvice evil-inner-word (around bars-as-word activate)
    ;;   (let ((table dotemacs/prog-syntax-table))
    ;;     (with-syntax-table table
    ;;       ad-do-it)))

    (auto-fill-comments-mode)
    (subword-mode +1) ;; camelCase
    (dotemacs-highlight-TODO-words)))

;; (use-package outline                    ; Navigate outlines in buffers
;;   :defer t
;;   :init (dolist (hook '(text-mode-hook prog-mode-hook))
;;           (add-hook hook #'outline-minor-mode))
;;   :diminish (outline-minor-mode . "📑"))

(use-package aggressive-indent
  :defer t
  :ensure t
  :init
  (progn
    (dotemacs-add-toggle aggressive-indent
      :status aggressive-indent-mode
      :on (aggressive-indent-mode)
      :off (aggressive-indent-mode -1)
      :documentation "Always keep code indented."
      :evil-leader "tI")
    (dotemacs-add-toggle aggressive-indent-globally
      :status aggressive-indent-mode
      :on (global-aggressive-indent-mode)
      :off (global-aggressive-indent-mode -1)
      :documentation "Always keep code indented globally."
      :evil-leader "t C-I"))
  :config
  (progn
    (add-hook 'diff-auto-refine-mode-hook 'dotemacs-toggle-aggressive-indent-off)
    (dotemacs-diminish aggressive-indent-mode " Ⓘ" " I")))

; TODO if this is useful, add a toggle
(use-package elide-head                 ; Elide lengthy GPL headers
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package prog-mode                  ; Prog Mode
  :defer
  :init
  (dotemacs/add-to-hooks
   (lambda ()
     (dotemacs/pretty-symbols
      '(("add-hook" . ?)
        ("lambda" . 955)
        ("=>" . 8658)
        )))
   '(emacs-lisp-mode-hook))

  (defun dotemacs/js2--setup-pretty-symbols ()
    (dotemacs/pretty-symbols
     '(("not" . 172)
       ("!" . 172)
       ("forall" . 8704)
       ("::" . 8759)
       ;; ("." . 8728)
       ("~>" . 8669)
       ;; ("()" . #X2205)
       ("==" . #X225F)
       ("!=" . #X2260)
       ("===" . #X2261)
       ("!==" . #X2262)
       (">=" . #X2265)
       ("<=" . #X2264)
       ("!!" . #X203C)
       ("&&" . #X2227)
       ("||" . #X2228)
       ;; ("null" . 00D8)
       ("sqrt" . #X221A)
       ("undefined" . #X22A5)
       ("pi" . #X3C0)
       ("function" . 955)
       ("->" . 8594)
       ("-<" . 8610)
       ("<-" . 8592)
       ("=>" . 8658)
       ;; ("map" . 8614)
       ("return" . 8592))))
  (add-hook 'js2-mode-hook 'dotemacs/js2--setup-pretty-symbols))

; http://stackoverflow.com/questions/1085170
(use-package hs-minor-mode
  :defer t
  :init
  (progn
    ;; Space to toggle folds.
    (define-key evil-motion-state-map (kbd "SPC") 'evil-toggle-fold)
    ;; TODO: Figure out how to only map " " when in hs-minor-mode
    ;; (evil-define-key 'motion hs-minor-mode-map " " 'evil-toggle-fold)

    ;; required for evil folding
    (setq hs-set-up-overlay 'dotemacs-fold-overlay)))

; This works and sets the mode correctly but the symbols do not show up
(use-package prettify-symbols-mode
  :defer t
  :init
  (progn
    ; (dolist (mode '(emacs-lisp js2 java python ruby))
    ;   (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
    ;             (lambda ()
    ;               (prettify-symbols-mode))))
    (setq prettify-symbols-unprettify-at-point 'right)
    (setq prettify-symbol-categories '(lambda relational logical))))

; Instead set the mode globally
(global-prettify-symbols-mode)

(provide 'module-programming)
;;; module-programming.el ends here
