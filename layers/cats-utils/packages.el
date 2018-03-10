;;; packages.el --- cats-utils

;;; Commentary:

;;; Code:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst cats-utils-packages
  '(
     plantuml-mode
     flycheck
     (flycheck-plantuml :requires flycheck)
     ))


;; plantuml
(defun cats-utils/post-init-plantuml-mode ()
  (let ((dir (configuration-layer/get-layer-local-dir 'cats-utils)))
    (setq plantuml-jar-path (concat dir "plantuml/plantuml.jar"))))

(defun cats-utils/pre-init-plantuml-mode ()
  (spacemacs|use-package-add-hook plantuml-mode
    :pre-config
    (progn)))


;; flycheck
(defun cats-utils/post-init-flycheck ()
  (add-hook 'plantuml-mode-hook 'flycheck-mode))

(defun cats-utils/init-flycheck-plantuml ()
  "Initialize flycheck-plantuml"
  (use-package flycheck-plantuml
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-plantuml-setup t)))

;;; packages.el ends here
