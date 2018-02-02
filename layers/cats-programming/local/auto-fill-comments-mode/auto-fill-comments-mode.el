;;; auto-fill-comments-mode.el --- Auto fill comments

;;; Commentary:

;; Personal functions

;;; Code:

;;;###autoload
(define-minor-mode auto-fill-comments-mode
  "Toggle `auto-fill-mode` to only auto-fill comments."
  :lighter nil
  :keymap nil
  (cond
   (auto-fill-comments-mode
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))
   (:else
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode -1))))

(provide 'auto-fill-comments-mode)

;;; auto-fill-comments-mode.el ends here
