;;; packages.el --- cats-web: Packages

;;; Commentary:

;;; Code:

(defconst cats-web-packages
  '(
    css-eldoc
    css-mode
     (css-sort-buffer :location (recipe
                                  :fetcher wiki
                                  css-sort-buffer))
     (css-sort :location (recipe
                           :fetcher github
                           :repo "diiq/css-sort.el"))
    com-css-sort
    emmet-mode
    flycheck
    helm-css-scss
    org
    smartparens
    web-mode
    ;; xah-css-mode
    yasnippet
    ))


;; com-css-sort
(defun cats-web/init-com-css-sort ()
  (use-package com-css-sort
    :commands (com-css-sort-attributes-block com-css-sort-attributes-document)
    :init
    (progn
      (setq com-css-sort-sort-type 'type-sort))
    ))


;; css-sort-buffer
(defun cats-web/init-css-sort-buffer ()
  (use-package css-sort-buffer
    :commands (css-sort-buffer)))


;; css-sort-buffer
(defun cats-web/init-css-sort ()
  (use-package css-sort
    :commands (css-sort-attributes)))


;; org
(defun cats-web/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(css . t))))


;; xah-css-mode
(defun cats-web/init-xah-css-mode ()
  (use-package xah-css-mode
    :defer t
    :mode ("\\.css\\'" . xah-css-mode)
    :init
    (progn
      (spacemacs|add-company-backends
       :backends company-css
       :modes xah-css-mode)

      ;; Mark `css-indent-offset' as safe-local variable
      (put 'css-indent-offset 'safe-local-variable #'integerp)

      ;; Explicitly run prog-mode hooks since css-mode does not derive from
      ;; prog-mode major-mode in Emacs 24 and below.
      (when (version< emacs-version "25")
        (add-hook 'xah-css-mode-hook 'spacemacs/run-prog-mode-hooks))

      (defun css-expand-statement ()
        "Expand CSS block"
        (interactive)
        (save-excursion
          (end-of-line)
          (search-backward "{")
          (forward-char 1)
          (while (or (eobp) (not (looking-at "}")))
          (let ((beg (point)))
            (newline)
            (search-forward ";")
            (indent-region beg (point))
            ))
          (newline)))

      (defun css-contract-statement ()
        "Contract CSS block"
        (interactive)
        (end-of-line)
        (search-backward "{")
        (while (not (looking-at "}"))
          (join-line -1)))

      (spacemacs/set-leader-keys-for-major-mode 'xah-css-mode
        "zc" 'css-contract-statement
        "zo" 'css-expand-statement))))


;; flycheck
(defun cats-web/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(css-csslint css-stylelint))
      (flycheck-add-mode checker 'xah-css-mode)))
  (dolist (mode '(xah-css-mode))
    (spacemacs/enable-flycheck mode)))


;; helm
(defun cats-web/pre-init-helm-css-scss ()
  (spacemacs|use-package-add-hook helm-css-scss
    :post-init
    (dolist (mode '(xah-css-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "gh" 'helm-css-scss))))


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
      (spacemacs/add-to-hooks 'emmet-mode '(xah-css-mode-hook))
      (setq emmet-indentation 2
            emmet-move-cursor-between-quotes t))))


;; smartparens
(defun cats-web/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (sp-with-modes '(web-mode)
      (sp-local-pair "%" "%"
        :unless '(sp-in-string-p)
        :post-handlers '(((lambda (&rest _ignored)
                            (just-one-space)
                            (save-excursion (insert " ")))
                           "SPC" "=" "-")))
      (sp-local-tag "%" "<% "  " %>")
      (sp-local-tag "=" "<%= " " %>")
      (sp-local-tag "-" "<%- " " %>")
      ;; (sp-local-pair "<% " " %>")
      ;; (sp-local-pair "{ " " }")
      ;; (sp-local-pair "<%= "  " %>")
      ;; (sp-local-pair "<%- "  " %>")
      ;; (sp-local-pair "<%# "  " %>")
      ;; (sp-local-pair "<%$ "  " %>")
      ;; (sp-local-pair "<%@ "  " %>")
      ;; (sp-local-pair "<%: "  " %>")
      ;; (sp-local-pair "{{ "  " }}")
      ;; (sp-local-pair "{% "  " %}")
      ;; (sp-local-pair "{%- "  " %}")
      ;; (sp-local-pair "{# "  " #}")
      )
    )
  )

(defun cats-web/post-init-smartparens ()
  (spacemacs/add-to-hooks
   (if dotspacemacs-smartparens-strict-mode
       'smartparens-strict-mode
     'smartparens-mode)
   '(xah-css-mode-hook)))


;; web-mode
(defun cats-web/pre-init-web-mode ()
  (spacemacs|use-package-add-hook web-mode
    :post-init
    (progn
      (add-to-list 'auto-mode-alist '("\\.mjml$" . web-mode)))
    :post-config
    (progn
      ;; use 2 space indent also for element’s attributes,
      ;; concatenations and contiguous function calls:
      (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
      (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
      (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

      ;; (setq web-mode-enable-auto-pairing nil)
      (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-sql-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2)


      ;; <%_ secrets.forEach(({name, description, required}) => { _%>
      ;; <%- name %>:
      ;;   description: '<%- description %>'
      ;;   required: <%- required %>
      ;; <%_ }) _%>

      (setq web-mode-extra-snippets
        '(("erb" . (("if" . "<%_ if (|) { _%>\n\n<%_ } _%>")))
           ("yaml" . (("if" . "<%_ if (|) { _%>\n\n<%_ } _%>")))
           ("ejs" . (("fe" . "<%_ [].forEach((|) => { _%>\n\n<%_ }) _%>")))
           ("ejs" . (("if" . "<%_ if (|) { _%>\n\n<%_ } _%>")))))

      (setq web-mode-extra-auto-pairs
        '(("erb"  . (("beg" "end")))
           ("ejs"  . (("<%-" "%>")))
           ("ejs"  . (("<%=" "%>")))
           ("ejs"  . (("<%" "%>")))
           ("md"  . (("<%-" "%>")))
           ("md"  . (("<%=" "%>")))
           ("md"  . (("<%" "%>")))))

      (setq web-mode-engines-alist
        '(("ejs"  . "\\.ejs\\.js[x]?\\'")
           ("ejs" . "\\.ejs\\.json\\'")
           ("ejs"  . "\\.ejs\\.md\\'")
           ("ejs"  . "\\.ejs\\.yml\\'")
           ))

      (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
           ("jsx"  . "\\.ejs.\\.jsx\\'")
           ("javascript"  . "\\.ejs\\.js\\'")
           ("json"  . "\\.ejs\\.json\\'")
           ("markdown"  . "\\.ejs\\.md\\'")
           ("yaml"  . "\\.ejs\\.yml\\'")))

      )))


;; yasnippet
(defun cats-web/post-init-yasnippet ()
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(xah-css-mode-hook)))

;;; packages.el ends here
