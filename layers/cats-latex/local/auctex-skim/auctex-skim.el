;;; auctex-skim.el --- Add Skim as viewer to AUCTeX

;;; Commentary:

;; Setup Skim as viewer for AUCTeX with `auctex-skim-setup', or select it as
;; default viewer with `auctex-skim-select'.

;;; Code:

(require 'tex)

(defcustom auctex-skim-bundle "/Applications/Skim.app"
  "The path to the Skim.app bundle."
  :group 'TeX-View
  :type 'directory)

(defcustom auctex-skim-displayline-path "Contents/SharedSupport/displayline"
  "The path to the `displayline' executable.

A relative path is relative to `auctex-skim-bundle'."
  :group 'TeX-View
  :type 'string)

(defun auctex-skim-displayline-path ()
  "Get the path to the displayline tool of Skim.

Return nil, if the tool was not found."
  (let ((bundle (file-name-as-directory auctex-skim-bundle)))
    (when (file-directory-p bundle)
      (executable-find (expand-file-name auctex-skim-displayline-path
                                         bundle)))))

;;;###autoload
(defun auctex-skim-setup ()
  "Setup Skim as viewer for AUCTeX."
  (add-to-list 'TeX-view-program-list '("Default application" "open %o"))
  (let ((displayline (auctex-skim-displayline-path)))
    (when (and displayline (file-executable-p displayline))
      (add-to-list 'TeX-view-program-list
                   (list "Skim" (list displayline " -b -r %n %o %b"))))))

;;;###autoload
(defun auctex-skim-select ()
  "Select Skim as default viewer for AUCTeX.

Warn if Skim is not available."
  (interactive)
  (auctex-skim-setup)
  (unless (assoc "Skim" TeX-view-program-list)
    (lwarn 'auctex :warning "Skim not available, falling back to default application"))
  (setq TeX-view-program-selection
        `((output-dvi "Default application")
          (output-html "Default application")
          ;; Use Skim if installed for SyncTex support.
          (output-pdf ,(if (assoc "Skim" TeX-view-program-list)
                           "Skim" "Default application")))))

(provide 'auctex-skim)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; auctex-skim.el ends here
