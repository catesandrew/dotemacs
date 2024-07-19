;;; funcs.el --- angular layer funcs

;;; Commentary:

;; Personal functions

;;; Code:


;; Backend
(defun cats//angular-setup-backend ()
  "Conditionally setup angular backend."
  (pcase angular-backend
    ('tide (spacemacs//tide-setup))
    ('lsp (cats//angular-setup-lsp))))

(defun cats//angular-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq angular-backend 'tide)
    (spacemacs//tide-setup-company 'ng2-ts-mode)))

(defun cats//angular-setup-eldoc ()
  "Conditionally setup eldoc based on backend."
  (pcase angular-backend
    ('tide (spacemacs//tide-setup-eldoc))
    ('lsp (cats//angular-setup-lsp-eldoc))))


;; LSP
(defun cats//angular-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (unless angular-lsp-linter
          (setq-local lsp-diagnostics-provider :none))
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun cats//angular-setup-lsp-eldoc ()
  "Setup eldoc for LSP."
  (eldoc-mode))


;; Emmet
(defun cats/angular-emmet-mode ()
  "Activate `emmet-mode' and configure it for local buffer."
  (emmet-mode))


;; Others
(defun cats/last-two-extensions (filename)
  "Return the last two extensions of FILENAME."
  (let* ((extensions (split-string filename "\\."))  ; Split filename into extensions
          (num-extensions (length extensions)))       ; Get the number of extensions
    (if (>= num-extensions 3)                       ; If there are at least 3 extensions
      (concat (nth (- num-extensions 2) extensions) ; Concatenate the last two extensions
        "." (car (last extensions)))        ; with a dot in between
      filename)))                                   ; Otherwise, return the original filename

(defun cats//typescript-ng2-file-p ()
  "Enable ng2-ts mode by using magic-mode-alist."
  (when buffer-file-name
    (and (member (cats/last-two-extensions buffer-file-name) '("config.ts" "component.ts" "service.ts" "pipe.ts" "directive.ts" "guard.ts" "module.ts" "routes.ts" "const.ts"))
         (re-search-forward "\\(\\( from \\|require(\\)[\"']@angular\\)"
                            magic-mode-regexp-match-limit t)
         (save-excursion
           (goto-char (match-beginning 1))
           (let ((sexp (syntax-ppss)))
             ;; not inside string or comment
             (not (or (nth 3 sexp)
                      (nth 4 sexp))))))))


(defun cats/angular-yasnippet-setup ()
  (yas-activate-extra-mode 'js-mode))

;; Format
(defun cats/angular-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'cats/angular-format t t))

(defun cats/angular-format ()
  "Call formatting tool specified in `angular-fmt-tool'."
  (interactive)
  (call-interactively
    (pcase angular-fmt-tool
      ('typescript-formatter 'spacemacs/typescript-tsfmt-format-buffer)
      ('tide 'tide-format)
      ('prettier 'prettier-js)
      (_ (user-error
           "%s isn't a valid typescript formatter. Possible values are 'tide, 'typescript-formatter or 'prettier"
           angular-fmt-tool)))))

(defun cats//angular-setup-checkers ()
  (when-let* ((found (executable-find "eslint_d")))
    (setq-local flycheck-javascript-eslint-executable found)))

(defun cats/angular-mode-init (hook)
  (add-hook hook 'cats//angular-setup-backend)
  (when angular-fmt-on-save
    (add-hook hook 'cats/angular-fmt-before-save-hook)))

(defun cats/angular-safe-local-variables (values)
  ;; safe values for backend to be used in directory file variables
  (dolist (value values)
    (add-to-list 'safe-local-variable-values
                 (cons 'angular-backend value))))

(defun cats/angular-mode-config (mode)
  (pcase angular-backend
    ('lsp (spacemacs/set-leader-keys-for-major-mode mode
            "==" 'cats/angular-format))
    ('tide (spacemacs/set-leader-keys-for-major-mode mode
             "=" 'cats/angular-format))))
