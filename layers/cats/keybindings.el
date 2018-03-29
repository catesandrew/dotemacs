;;; keybindings.el --- cats: keybindings

;;; Commentary:

;; My personal key bindings.

;;; Code:

(spacemacs/declare-prefix "mT" "toggles")
(spacemacs/declare-prefix "to" "personal")
(spacemacs/declare-prefix "o" "personal")
(spacemacs/declare-prefix "oe" "eval")
(spacemacs/declare-prefix "oi" "insert")
(spacemacs/declare-prefix "ox" "text")
(spacemacs/declare-prefix "oh" "help")

;; File bindings
(spacemacs/set-leader-keys
  "oee" 'eval-expression
  "oid" 'cats/insert-current-date
  "oip" 'cats/insert-page-break-line
  "toi" 'toggle-input-method
  "oxi" 'set-input-method)

;;; keybindings.el ends here
