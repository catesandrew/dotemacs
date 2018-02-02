;;; packages.el --- Grammar Layer packages File for Spacemacs

(setq cats-grammar-packages
  '(
    writegood-mode
    langtool
    flyspell
    (text-mode :location built-in)
))


;; text-mode
(defun cats-grammar/init-text-mode ()
  "Add text mode hooks."
  (use-package text-mode
    :init
    (progn
      ;; bootstrap with our defaults
      (add-hook 'cats/text-mode-hook 'cats/text-mode-defaults)
      ;; run our cats/prog-mod-hooks with text-mode
      (add-hook 'text-mode-hook (lambda () (run-hooks #'cats/text-mode-hook))))))


;; writegood
(defun cats-grammar/init-writegood-mode ()
  (use-package writegood-mode
    :defer t
    :ensure t
    :init
    (progn
      (cats-grammar/add-writegood-hook 'text-mode-hook)

      (spacemacs|add-toggle writegood-mode
        :status writegood-mode
        :on (writegood-mode)
        :off (writegood-mode -1)
        :documentation "Highlight problems with your writing (in english)."
        :evil-leader "tog")

      (spacemacs/set-leader-keys
       "ogb" 'writegood-grade-level
       "ogd" 'writegood-reading-ease)

      (defun writegood-cliches-turn-on (orig-fun)
        "Turn on warnings for cliches."
        (apply orig-fun)
        (font-lock-add-keywords nil writegood-cliches-font-lock-keywords))

      (defun writegood-cliches-turn-off (orig-fun)
        "Turn on syntax highlighting for cliches."
        (apply orig-fun)
        (font-lock-remove-keywords nil writegood-cliches-font-lock-keywords))

      (advice-add 'writegood-cliches-turn-on :around 'writegood-turn-on)
      (advice-add 'writegood-cliches-turn-off :around 'writegood-turn-off))))


;; langtool
(defun cats-grammar/init-langtool ()
  (use-package langtool
    :defer t
    :ensure t
    :init
    (progn
      (setq langtool-language-tool-jar "/usr/local/opt/languagetool/libexec/languagetool-commandline.jar")
      (setq langtool-default-language "en-US")
      (setq langtool-mother-tongue "en")

      (defun langtool-autoshow-detail-popup (overlays)
        (when (require 'popup nil t)
          ;; Do not interrupt current popup
          (unless (or popup-instances
                      ;; suppress popup after type `C-g` .
                      (memq last-command '(keyboard-quit)))
            (let ((msg (langtool-details-error-message overlays)))
              (popup-tip msg)))))

      (setq langtool-autoshow-message-function
            'langtool-autoshow-detail-popup)

      (spacemacs/set-leader-keys
       "ogw" 'langtool-check
       "ogW" 'langtool-check-done
       "ogp" 'langtool-show-message-at-point
       "ogb" 'langtool-correct-buffer))))


;; flyspell
(defun cats-grammar/pre-init-flyspell ()
  (spacemacs|use-package-add-hook flyspell
    :post-init
    (progn
      (setq flyspell-use-meta-tab nil
            flyspell-issue-welcome-flag nil  ;; Make Flyspell less chatty
            flyspell-issue-message-flag nil)

      (spacemacs/set-leader-keys
        "Si" 'ispell-word-then-abbrev
        "Sp" 'flyspell-goto-previous-error)

      (spacemacs/set-leader-keys
        "oSi" 'ispell-word-then-abbrev
        "oSp" 'flyspell-goto-previous-error))
    :post-config
    (progn
      ;; Undefine mouse buttons which get in the way
      (define-key flyspell-mouse-map [down-mouse-2] nil)
      (define-key flyspell-mouse-map [mouse-2] nil))))
