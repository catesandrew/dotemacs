;;; keybindings.el --- cats-org: keybindings

;;; Commentary:

;; My personal key bindings.

;;; Code:

(spacemacs/declare-prefix "oo" "org")

;; File bindings
(spacemacs/set-leader-keys
  "oob" 'cats/org-find-bba
  "ooe" 'cats/org-find-emacs
  "oon" 'cats/org-find-notes
  "oop" 'cats/org-find-private
  "oow" 'cats/org-find-work
  "ooW" 'cats/org-find-workhours)

;;; keybindings.el ends here
