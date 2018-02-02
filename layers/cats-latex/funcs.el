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


;;; funcs.el ends here
