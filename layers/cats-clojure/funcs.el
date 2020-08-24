;;; funcs.el --- Clojure Layer functions File for Spacemacs

;;
;; Evaluate code when it is contained in a (comment (,,,))
;; 24th sept - didnt work, even after updating spacemacs and packages
;; (setq cider-eval-toplevel-inside-comment-form t)
;;
;; (add-hook 'clojure-mode-hook
;;           '(setq cider-eval-toplevel-inside-comment-form t))
;;
;;
;; Toggle view of a clojure `(comment ,,,) block'
;;
(defun clojure-hack/toggle-comment-block (arg)
  "Close all top level (comment) forms. With universal arg, open all."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^(comment\\>" nil 'noerror)
      (call-interactively
        (if arg 'evil-open-fold
          'evil-close-fold)))))

;; (evil-define-key 'normal clojure-mode-map
;;   "zC" 'clojure-hack/toggle-comment-block
;;   "zO" (lambda () (interactive) (clojure-hack/toggle-comment-block 'open)))

;; toggle reader macro sexp comment
;; toggles the #_ characters at the start of an expression
(defun clojure-toggle-reader-comment-sexp ()
  (interactive)
  (let* ((point-pos1 (point)))
    (evil-insert-line 0)
    (let* ((point-pos2 (point))
            (cmtstr "#_")
            (cmtstr-len (length cmtstr))
            (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len)))
            (point-movement (if (string= cmtstr line-start) -2 2))
            (ending-point-pos (+ point-pos1 point-movement 1)))
      (if (string= cmtstr line-start)
        (delete-char cmtstr-len)
        (insert cmtstr))
      (goto-char ending-point-pos)))
  (evil-normal-state))
;;
;; Assign keybinding to the toggle-reader-comment-sexp function
;; (define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)

;; Experiment: Start Clojure REPL with a specific profile
;; https://stackoverflow.com/questions/18304271/how-do-i-choose-switch-leiningen-profiles-with-emacs-nrepl
;;
;; (defun start-cider-repl-with-profile ()
;;   (interactive)
;;   (letrec ((profile (read-string "Enter profile name: "))
;;            (lein-params (concat "with-profile +" profile " repl :headless")))
;;     (message "lein-params set to: %s" lein-params)
;;     (set-variable 'cider-lein-parameters lein-params)
;;     (cider-jack-in)))
;;
;; My altered more idiomatic version, hopefully
;; - seems to be a bug...
;; (defun start-cider-repl-with-profile (profile)
;;   (interactive "sEnter profile name: ")
;;   (letrec ((lein-params (concat "with-profile +" profile " repl :headless")))
;;     (message "lein-params set to: %s" lein-params)
;;     (set-variable 'cider-lein-parameters lein-params)
;;     (cider-jack-in)))

;;; funcs.el ends here
