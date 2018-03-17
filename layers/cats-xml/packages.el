;;; packages.el --- cats-xml: Packages

;;; Commentary:

;; Personal XML layer.

;;; Code:

(defconst cats-xml-packages
  '(
    nxml
    org
    smartparens
    ))


;; org
(defun cats-xml/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(xml . t))))

(defun cats-xml/init-nxml ()
  (use-package nxml-mode
    :defer t
    ;; Complete closing tags, and insert XML declarations into empty files
    :config (setq nxml-slash-auto-complete-flag t
                  nxml-auto-insert-xml-declaration-flag t)))

(defun cats-xml/post-init-smartparens ()
  (add-hook 'nxml-mode-hook 'smartparens-mode))

;;; packages.el ends here
