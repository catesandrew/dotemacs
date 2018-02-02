;;; funcs.el --- cats-web: Funcs

;;; Commentary:

;; Personal functions

;;; Code:

(defun css-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

;;; funcs.el ends here
