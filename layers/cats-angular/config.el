;;; config.el --- angular layer config

(defvar angular-fmt-on-save nil
  "Run formatter on buffer save.")

(defvar angular-fmt-tool 'tide
  "The name of the tool to be used for TypeScript source code formatting.
Possible values are 'tide (default), 'typescript-formatter and 'prettier.")

(defvar angular-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'tide)
  "The backend to use for IDE features.
Possible values are `tide' and `lsp'.
If `nil' then `tide' is the default backend unless `lsp' layer is used.")

(defvar angular-linter 'eslint
  "The linter to use for typescript. Possible values are `eslint' `tslint'")

(defvar angular-lsp-linter t
  "If the backend is `lsp', and this variable is non-nil, then
use lsp as the linter, otherwise let flycheck choose the best
linter that's available.")

(defvar angular-html-fmt-tool 'web-beautify
  "The formatter to format a CSS/SCSS/Less file. Possible values are `web-beautify' and `prettier'.")

(spacemacs|define-jump-handlers ng2-ts-mode)
(spacemacs|define-jump-handlers ng2-html-mode)
