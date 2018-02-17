;; To augment and/or counteract these defaults your own function
;; to cats/tex-mode-hook, using:
;;
;; (add-hook 'cats/tex-mode-hook 'cats/tex-mode-defaults t)
;;
;; The final optional t sets the *append* argument

(defvar cats/tex-mode-hook nil
  "Hooks run when `tex-mode-hook' is fired.")
