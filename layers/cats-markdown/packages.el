;;; packages.el --- cats-markdown: Packages

;;; Commentary:

;; Personal markdown layer.

;;; Code:

(defconst cats-markdown-packages
  '(
    markdown-mode
    rst
    mustache-mode
    handlebars-mode
    jira-markup-mode
    smartparens
    ))

(defun cats-markdown/pre-init-markdown-mode ()
  (spacemacs|use-package-add-hook markdown-mode
    :post-mode (("\\.markdown$" . markdown-mode)
                ("\\.apib$" . markdown-mode))
    :post-config
    (progn
      ;; http://www.tychoish.com/posts/imenu-for-markdown-and-writing/
      (setq markdown-imenu-generic-expression
            '(("title"  "^\\(.*\\)[\n]=+$" 1)
              ("h2-"    "^\\(.*\\)[\n]-+$" 1)
              ("h1"   "^# \\(.*\\)$" 1)
              ("h2"   "^## \\(.*\\)$" 1)
              ("h3"   "^### \\(.*\\)$" 1)
              ("h4"   "^#### \\(.*\\)$" 1)
              ("h5"   "^##### \\(.*\\)$" 1)
              ("h6"   "^###### \\(.*\\)$" 1)
              ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

      ;; ;; GFM: No filling because line breaks are significant.
      ;; (add-hook 'gfm-mode-hook 'turn-off-auto-fill)
      ;; ;; Use visual lines instead
      ;; (add-hook 'gfm-mode-hook 'visual-line-mode)
      ;; (add-hook 'gfm-mode-hook 'dotemacs-whitespace-style-no-long-lines)

      ;; disable auto indent
      (add-hook 'markdown-mode-hook
         (lambda ()
           (electric-indent-local-mode -1)
           (setq imenu-generic-expression markdown-imenu-generic-expression)))

      (when (eq system-type 'darwin)
        (setq markdown-open-command "mark"))

      (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
        "oh" 'cats-markdown/post-header
        "op" 'cats-markdown/publish-jekyll-draft)

      (let* ((layer-dir (configuration-layer/get-layer-local-dir 'cats-markdown))
             (stylesheet (expand-file-name "pandoc.css" layer-dir)))
        (setq markdown-command
              (mapconcat #'shell-quote-argument
                         `("pandoc" "--toc" "--section-divs"
                           "--css" ,(concat "file://" stylesheet)
                           "--standalone" "-f" "markdown" "-t" "html5")
                         " "))))
    ))

(defun cats-markdown/init-rst ()
  (use-package rst
    :defer t
    :init
    (progn
      ;; Indent with 3 spaces after all kinds of literal blocks
      (setq rst-indent-literal-minimized 2
            rst-indent-literal-normal 2))
    :config
    (add-hook 'rst-mode-hook #'auto-fill-mode)

    (dolist (prefix '(("ma" . "rst/adornments")
                      ("ml" . "rst/lists")
                      ("mi" . "rst/insert")))
      (spacemacs/declare-prefix-for-mode 'rst-mode (car prefix) (cdr prefix)))

    (defun cats-restructuredtext/insert-keybinding (key)
      "Insert KEY as ReST markup."
      (interactive "kType key sequence: ")
      (format ":kbd:`%s`" (help-key-description key nil)))

    (spacemacs/set-leader-keys-for-major-mode 'rst-mode
      "=" 'rst-adjust
      ;; Adornments
      "as" 'rst-straighten-adornments
      "ad" 'rst-display-adornments-hierarchy
      ;; Lists
      "li" 'rst-insert-list
      "lc" 'rst-convert-bullets-to-enumeration
      "lb" 'rst-bullet-list-region
      "le" 'rst-enumerate-region:w
      "ls" 'rst-straighten-bullets-region
      ;; Inserts
      "ik" 'cats-restructuredtext/insert-keybinding
      ;; ToC
      "t" 'rst-toc)

    ;; Section navigation in normal state movements
    (evil-define-key 'normal rst-mode-map
      "gj" 'rst-forward-section
      "gk" 'rst-backward-section)))

(defun cats-markdown/post-init-smartparens ()
  (add-hook 'rst-mode-hook 'smartparens-mode))

(defun cats-markdown/init-mustache-mode ()
  "Mustache mode."
  (use-package mustache-mode
    :ensure t
    :defer t
    :mode (("\\.mustache$" . mustache-mode))))


;; handlebars
(defun cats-markdown/init-handlebars-mode ()
  (use-package handlebars-mode
    :ensure t
    :defer t
    :init
    (progn
      (with-eval-after-load 'projectile
        (add-hook 'cats/project-hook
           'cats//locate-handlebars-from-projectile)
        (run-hooks 'cats/project-hook))

      (with-eval-after-load 'flycheck
        (add-hook 'cats/handlebars-executable-hook
           'cats//hbs-set-handlebars-executable)))
    :mode (("\\.hbs$" . handlebars-mode)
           ("\\.handlebars$" . handlebars-mode))))


;; jira-markup
(defun cats-markdown/init-jira-markup-mode ()
  (use-package jira-markup-mode
    :ensure t
    :defer t))


;; smartparens
(defun cats-markdown/post-init-smartparens ()
  (add-hook 'rst-mode-hook 'smartparens-mode))

;;; packages.el ends here
