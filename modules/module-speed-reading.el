;;; module-speed-reading.el --- Speed Reading Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'evil-evilified-state)
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

(use-package spray
  :commands spray-mode
  :ensure t
  :init
  (progn
    (defun speed-reading/start-spray ()
      "Start spray speed reading on current buffer at current point."
      (interactive)
      (dotemacs/normal-to-insert-state)
      (spray-mode t)
      (internal-show-cursor (selected-window) nil))
    (dotemacs-set-leader-keys "aR" 'speed-reading/start-spray)

    (defadvice spray-quit (after speed-reading//quit-spray activate)
      "Correctly quit spray."
      (internal-show-cursor (selected-window) t)
      (evil-normal-state)))
  :config
  (progn
    (define-key spray-mode-map (kbd "h") 'spray-backward-word)
    (define-key spray-mode-map (kbd "l") 'spray-forward-word)
    (define-key spray-mode-map (kbd "q") 'spray-quit)))

(dotemacs-use-package-add-hook which-key
  :post-init
  (push '("\\`speed-reading/\\(.+\\)\\'" . "\\1")
        which-key-description-replacement-alist))

(provide 'module-speed-reading)
;;; module-speed-reading.el ends here
