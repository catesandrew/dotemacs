;;; config.el --- Grammar Layer configuration File for Spacemacs

(defvar cats/text-mode-hook nil
  "Hooks run when `text-mode-hook' is fired.")

(defvar grammar-checking-enable-by-default t
  "Enable grammar checking by default.")

(defvar writegood-cliches-font-lock-keywords-regexp
  (concat "\\b" (regexp-opt writegood-cliches) "\\b")
  "Matches cliche-phrases.")

(defvar writegood-cliches-font-lock-keywords
  (list (list writegood-cliches-font-lock-keywords-regexp
              0 (quote 'writegood-cliches-face) 'prepend)))

(defface writegood-cliches-face
  '((((class color) (background light))
     (:inherit font-lock-warning-face :background "Black"))
    (((class color) (background dark))
     (:inherit font-lock-warning-face :background "DarkRed")))
  "Writegood face for cliches"
  :group 'writegood)
