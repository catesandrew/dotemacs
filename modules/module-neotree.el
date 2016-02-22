;;; module-neotree.el --- Neotree Module
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
;; (require 'core-vars)
;; (require 'core-funcs)
(require 'core-keybindings)
;; (require 'core-display-init)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;; funcs

;; packages

(use-package neotree
  :ensure t
  :defer t
  :commands neo-global--window-exists-p
  :init
  (progn
    (add-to-list 'evil-motion-state-modes 'neotree-mode)
    (setq neo-window-width 27
          neo-theme 'nerd
          neo-create-file-auto-open t
          neo-banner-message nil
          neo-show-updir-line nil
          neo-mode-line-type 'neotree
          neo-smart-open nil       ; if t, every time when the neotree window is
                                   ; opened, it will try to find current file
                                   ; and jump to node.

          neo-dont-be-alone t      ; Don't allow neotree to be the only open
                                   ; window
          neo-persist-show nil
          neo-show-hidden-files nil
          neo-auto-indent-point t
          neo-modern-sidebar t
          neo-vc-integration nil)

    (defun dotemacs-neotree-expand-or-open ()
      "Collapse a neotree node."
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (when node
          (if (file-directory-p node)
              (progn
                (neo-buffer--set-expand node t)
                (neo-buffer--refresh t)
                (when neo-auto-indent-point
                  (next-line)
                  (neo-point-auto-indent)))
            (call-interactively 'neotree-enter)))))

    (defun dotemacs-neotree-collapse ()
      "Collapse a neotree node."
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (when node
          (when (file-directory-p node)
            (neo-buffer--set-expand node nil)
            (neo-buffer--refresh t))
          (when neo-auto-indent-point
            (neo-point-auto-indent)))))

    (defun dotemacs-neotree-collapse-or-up ()
      "Collapse an expanded directory node or go to the parent node."
      (interactive)
      (let ((node (neo-buffer--get-filename-current-line)))
        (when node
          (if (file-directory-p node)
              (if (neo-buffer--expanded-node-p node)
                  (dotemacs-neotree-collapse)
                (neotree-select-up-node))
            (neotree-select-up-node)))))

    (defun neotree-find-project-root ()
      (interactive)
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (let ((origin-buffer-file-name (buffer-file-name)))
          (neotree-find (projectile-project-root))
          (neotree-find origin-buffer-file-name))))

    (defun dotemacs-neotree-key-bindings ()
      "Set the key bindings for a neotree buffer."
      (evilified-state-evilify-map neotree-mode-map
        :mode neotree-mode
        :bindings
        (kbd "TAB")  'neotree-stretch-toggle
        (kbd "RET") 'neotree-enter
        (kbd "|") 'neotree-enter-vertical-split
        (kbd "-") 'neotree-enter-horizontal-split
        (kbd "?") 'evil-search-backward
        (kbd "c") 'neotree-create-node
        (kbd "d") 'neotree-delete-node
        (kbd "gr") 'neotree-refresh
        (kbd "h") 'dotemacs-neotree-collapse-or-up
        (kbd "H") 'neotree-select-previous-sibling-node
        (kbd "J") 'neotree-select-down-node
        (kbd "K") 'neotree-select-up-node
        (kbd "l") 'dotemacs-neotree-expand-or-open
        (kbd "L") 'neotree-select-next-sibling-node
        (kbd "q") 'neotree-hide
        (kbd "r") 'neotree-rename-node
        (kbd "R") 'neotree-change-root
        (kbd "s") 'neotree-hidden-file-toggle))

    ;; neo-global--select-window
    (dotemacs-set-leader-keys
      "fn" 'neotree-show
      "fN" 'neotree-hide
      "ft" 'neotree-toggle
      "pt" 'neotree-find-project-root))

  :config
  (progn
    (dotemacs-neotree-key-bindings)
    (when neo-persist-show
      (add-hook 'popwin:before-popup-hook
                (lambda () (setq neo-persist-show nil)))
      (add-hook 'popwin:after-popup-hook
                (lambda () (setq neo-persist-show t))))))

(provide 'module-neotree)
;;; module-neotree.el ends here
