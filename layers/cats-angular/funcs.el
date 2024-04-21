;;; funcs.el --- angular layer funcs

;;; Commentary:

;; Personal functions

;;; Code:


;; Backend
(defun cats//angular-setup-backend ()
  "Conditionally setup angular backend."
  (pcase typescript-backend
    ('tide (spacemacs//tide-setup))
    ('lsp (cats//angular-setup-lsp))))

(defun cats//angular-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq typescript-backend 'tide)
    (spacemacs//tide-setup-company 'ng2-ts-mode)))


;; LSP
(defun cats//angular-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (unless typescript-lsp-linter
          (setq-local lsp-diagnostics-provider :none))
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Emmet
(defun cats/angular-emmet-mode ()
  "Activate `emmet-mode' and configure it for local buffer."
  (emmet-mode)
  (setq-local emmet-expand-jsx-className? t))


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
    (and (member (cats/last-two-extensions buffer-file-name) '("config.ts" "component.ts" "service.ts" "pipe.ts" "directive.ts" "guard.ts" "module.ts"))
         (re-search-forward "\\(\\( from \\|require(\\)[\"']@angular\\)"
                            magic-mode-regexp-match-limit t)
         (save-excursion
           (goto-char (match-beginning 1))
           (let ((sexp (syntax-ppss)))
             ;; not inside string or comment
             (not (or (nth 3 sexp)
                      (nth 4 sexp))))))))


(defun cats//angular-setup-yasnippet ()
  (yas-activate-extra-mode 'ng2-ts-mode))

;; Format
(defun cats//angular-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'spacemacs/typescript-format t t))
