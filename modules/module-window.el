;;; module-window.el --- Window Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'use-package)
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defvar dotemacs/winner-boring-buffers nil
  "List of boring buffers for winner mode to ignore.")

(use-package windmove                   ; Move between windows with Shift+Arrow
  :init (windmove-default-keybindings))

(use-package winner                     ; Undo and redo window configurations
  :ensure t
  :init
  (progn
    (winner-mode t)
    (setq dotemacs/winner-boring-buffers '("*Compile-Log*"
                                            "*inferior-lisp*"
                                            "*Fuzzy Completions*"
                                            "*Apropos*"
                                            "*Help*"
                                            "*cvs*"
                                            "*Buffer List*"
                                            "*Ibuffer*"
                                            "*esh command on file*"
                                            ))
    (setq winner-boring-buffers
          (append winner-boring-buffers dotemacs/winner-boring-buffers))
    (winner-mode t)))

(use-package window-numbering
  :ensure t
  :config
  (progn
    (defun window-numbering-install-mode-line (&optional position)
      "Do nothing, the display is handled by the powerline.")
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
    (dotemacs-set-leader-keys
      "0" 'select-window-0
      "1" 'select-window-1
      "2" 'select-window-2
      "3" 'select-window-3
      "4" 'select-window-4
      "5" 'select-window-5
      "6" 'select-window-6
      "7" 'select-window-7
      "8" 'select-window-8
      "9" 'select-window-9)
    (window-numbering-mode 1))

  ;; make sure neotree is always 0
  (defun dotemacs//window-numbering-assign ()
    "Custom number assignment for neotree."
    (when (and (boundp 'neo-buffer-name)
               (string= (buffer-name) neo-buffer-name))
      0))
  ;; using lambda to work-around a bug in window-numbering, see
  ;; https://github.com/nschum/window-numbering.el/issues/10
  (setq window-numbering-assign-func
        (lambda () (dotemacs//window-numbering-assign))))

(provide 'module-window)
;;; module-window.el ends here
