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


;; tex-mode
(defun cats/tex-mode-local ()
  (add-hook 'hack-local-variables-hook 'cats/tex-mode-defaults nil 'local))

(defun cats/tex-mode-defaults ()
  ;; add buffer-local indicator for whether prog-mode-hook has run.
  (latex-unicode-simplified)
  (spacemacs/toggle-prettify-symbols-mode-on))

(defun cats/pdf-file-name (&optional extension nondirectory ask)
  "Return the name of pdf file for the current document.

If optional argument EXTENSION is non-nil, add that file extension to
the name. Special value t means use `TeX-default-extension'.

If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

If optional third argument ASK is non-nil, ask the user for the
name of master file if it cannot be determined otherwise."
  (if (eq extension t)
    (setq extension TeX-default-extension))

  (let ((output-file (concat
                       (car (split-string
                              (buffer-file-name)
                              (concat (file-name-base (buffer-file-name)) "\\.tex")))
                       "build/"
                       (file-name-base (buffer-file-name)))))
    (if extension
	    (concat output-file "." extension)
	    output-file)))

;; The modes provided by AUCTeX are listed at
;; <https://www.gnu.org/software/auctex/manual/auctex.html#Modes-and-Hooks> and
;; they are:
;;
;; - `plain-TeX-mode`
;; - `LaTeX-mode`
;; - `ams-TeX-mode`
;; - `ConTeXt-mode`
;; - `Texinfo-mode`
;; - `docTeX-mode`
;;
;; Instead of:
;;
;; - `tex-mode`
;; - `plain-tex-mode`
;; - `latex-mode`
;; - `slitex-mode`
;; - `doctex-mode`
;;
;; (note the different capitalization) are the major modes provided by the [TeX
;; mode](<https://www.gnu.org/software/emacs/manual/html_node/emacs/TeX-Mode.html>)
;; package shipped with Emacs.

;;; funcs.el ends here
