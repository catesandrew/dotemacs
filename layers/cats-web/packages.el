;;; packages.el --- cats-web: Packages

;;; Commentary:

;;; Code:

(defconst cats-web-packages
  '(
    css-eldoc
    css-mode
    emmet-mode
    smartparens
    web-mode
    ))


;; css-eldoc
(defun cats-web/init-css-eldoc ()
  "Basic Eldoc for CSS."
  (use-package css-eldoc
    :ensure t
    :commands (turn-on-css-eldoc)
    :init (add-hook 'css-mode-hook 'turn-on-css-eldoc)))


;; css-mode
(defun cats-web/pre-init-css-mode ()
  (spacemacs|use-package-add-hook css-mode
    :post-init
    (progn
      (setq css-indent-offset 2)
      (add-hook 'css-mode-hook (lambda ()
           (setq imenu-create-index-function 'css-imenu-make-index))))))


;; emmet-mode
(defun cats-web/pre-init-emmet-mode ()
  (spacemacs|use-package-add-hook emmet-mode
    :post-init
    (progn
      (setq emmet-indentation 2
            emmet-move-cursor-between-quotes t))))


;; smartparens
(defun cats-web/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (sp-with-modes '(web-mode)
      (sp-local-pair "<% " " %>")
      (sp-local-pair "{ " " }")
      (sp-local-pair "<%= "  " %>")
      (sp-local-pair "<%# "  " %>")
      (sp-local-pair "<%$ "  " %>")
      (sp-local-pair "<%@ "  " %>")
      (sp-local-pair "<%: "  " %>")
      (sp-local-pair "{{ "  " }}")
      (sp-local-pair "{% "  " %}")
      (sp-local-pair "{%- "  " %}")
      (sp-local-pair "{# "  " #}"))))


;; web-mode
(defun cats-web/pre-init-web-mode ()
  (spacemacs|use-package-add-hook web-mode
    :post-config
    (progn
      (setq web-mode-enable-auto-pairing nil
            web-mode-markup-indent-offset 2
            web-mode-code-indent-offset 2
            web-mode-sql-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-attr-indent-offset 2
            web-mode-style-padding 2
            web-mode-script-padding 2))))

;;; packages.el ends here
