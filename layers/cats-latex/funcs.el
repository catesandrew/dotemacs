;;; funcs.el --- cats-latex

;;; Commentary:

;;; Code:

(defun TeX-active-master (&optional extension nondirectory)
  "The master file currently being compiled.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value t means use `TeX-default-extension'.

If optional second argument NONDIRECTORY is non-nil, do not include
the directory."
  (if TeX-current-process-region-p
      (concat TeX-build-directory (TeX-region-file extension nondirectory))
    (concat TeX-build-directory (TeX-master-file extension nondirectory))))

(defun cats/tex-mode-defaults ()
  "Default latex hook."
  (unless (bound-and-true-p my-lmh-ran)
    ;; add buffer-local indicator for whether prog-mode-hook has run.
    (set (make-local-variable 'my-lmh-ran) t)

    (latex-unicode-simplified)
    (spacemacs/toggle-prettify-symbols-mode-on)))

;; bootstrap with our defaults
(add-hook 'cats/tex-mode-hook 'cats/tex-mode-defaults)
;; run our cats/tex-mod-hooks with LaTeX-mode
(add-hook 'LaTeX-mode-hook (lambda () (run-hooks #'cats/tex-mode-hook)))
;; run our cats/tex-mod-hooks with latex-mode
(add-hook 'latex-mode-hook (lambda () (run-hooks #'cats/tex-mode-hook)))

;;; funcs.el ends here
