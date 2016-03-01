;;; module-spaceline.el --- SpaceLine Module
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
;; (require 'core-keybindings)
(require 'core-fonts-support)
(require 'core-display-init)
(require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)

;;; Code:

(defmacro dotemacs-symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a dotemacs variable.
If SYMBOL value is `display-graphic-p' then return the result of
 `(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq 'display-graphic-p ,symbol) (display-graphic-p) ,symbol))

(dotemacs-use-package-add-hook spaceline-config
  :pre-config
  (progn
    ;; customize-vc-mode-appearance-in-mode-line
    ;; http://emacs.stackexchange.com/questions/10955/
    (defadvice vc-mode-line (after strip-backend () activate)
      (when (stringp vc-mode)
        (let ((noback (replace-regexp-in-string
                       (format "^ %s[-:]" (vc-backend buffer-file-name))
                       "" vc-mode)))
          (setq vc-mode noback))))

    (spaceline-define-segment version-control
      "Version control information."
      (powerline-raw
       (when (buffer-file-name)
         (let ((backend (vc-backend buffer-file-name))
               (state (vc-state (buffer-file-name))))
           (s-trim (concat
                    (pcase backend
                      (`Git " ")
                      (`Svn " ")
                      (`Hg " "))
                    (format "(%s)" vc-mode)
                    (pcase state
                      (`up-to-date " ")
                      (`edited " ")
                      (`added " ")
                      (`unregistered " ")
                      (`removed " ")
                      (`needs-merge " ")
                      (`needs-update " ")
                      (`ignored " Ign")
                      (_ " ")))))))
      :when vc-mode)

    (defun spaceline--dotemacs-theme (left second-left &rest additional-segments)
      "Convenience function for the spacemacs and emacs themes."
      (spaceline-install

       `(,left
         anzu
         auto-compile
         ,second-left
         major-mode
         (process :when active)
         ((flycheck-error flycheck-warning flycheck-info)
          :when active)
         ((minor-modes :separator spaceline-minor-modes-separator)
          :when active)
         (mu4e-alert-segment :when active)
         (erc-track :when active)
         (version-control :when active)
         (org-pomodoro :when active)
         (org-clock :when active)
         nyan-cat)

       `(which-function
         ;; (python-pyvenv :fallback python-pyenv)
         ;; (battery :when active)
         selection-info
         input-method
         ((;; buffer-encoding-abbrev
           point-position
           line-column)
          :separator "")
         (global :when active)
         ,@additional-segments
         buffer-position
         hud)))

    (defun spaceline-dotemacs-theme (&rest additional-segments)
      "Install the modeline used by dotemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
      (apply 'spaceline--dotemacs-theme
             '((persp-name
                workspace-number
                window-number)
               :fallback evil-state
               :separator "|"
               :face highlight-face)
             '(buffer-modified buffer-size buffer-id remote-host)
             additional-segments))))

(use-package spaceline-config
  :ensure spaceline
  :init
  (progn
    (dotemacs|do-after-display-system-init
     (setq-default powerline-default-separator
                   (if (display-graphic-p) 'wave 'utf-8)))

    (defun dotemacs-set-powerline-for-startup-buffers ()
      "Set the powerline for buffers created when Emacs starts."
      (dolist (buffer '("*Messages*" "*dotemacs*" "*scratch" "*Compile-Log*"))
        (when (get-buffer buffer)
          (dotemacs-restore-powerline buffer))))
    (add-hook 'emacs-startup-hook
              'dotemacs-set-powerline-for-startup-buffers))
  :config
  (progn
    (spaceline-toggle-battery-off)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-major-mode-off)
    (spaceline-toggle-org-clock-off)

    (defun dotemacs/customize-powerline-faces ()
      "Alter powerline face to make them work with more themes."
      (set-face-attribute 'powerline-inactive2 nil
                          :inherit 'font-lock-comment-face))
    (dotemacs/customize-powerline-faces)

    (dolist (spec '((minor-modes "tmm")
                    (major-mode "tmM")
                    (version-control "tmv")
                    (point-position "tmp")
                    (org-clock "tmc")))
      (let* ((segment (car spec))
             (status-var (intern (format "spaceline-%S-p" segment))))
        (eval `(dotemacs-add-toggle ,(intern (format "mode-line-%S" segment))
                 :status ,status-var
                 :on (setq ,status-var t)
                 :off (setq ,status-var nil)
                 :documentation ,(format "Show %s in the mode-line."
                                         (replace-regexp-in-string
                                          "-" " " (format "%S" segment)))
                 :evil-leader ,(cadr spec)))))
    (setq spaceline-org-clock-p nil)

    (defun dotemacs//evil-state-face ()
      (when (bound-and-true-p evil-state)
          (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
            (intern (format "dotemacs-%S-face" state)))))
    (setq spaceline-highlight-face-func 'dotemacs//evil-state-face)

    (let ((unicodep (dotemacs-symbol-value
                     dotemacs-mode-line-unicode-symbols)))
      (setq spaceline-window-numbers-unicode unicodep)
      (setq spaceline-workspace-numbers-unicode unicodep))

    (spaceline-dotemacs-theme)
    (spaceline-helm-mode t)
    (spaceline-info-mode t)

    (defun dotemacs-restore-powerline (buffer)
      "Restore the powerline in buffer"
      (with-current-buffer buffer
        (setq-local mode-line-format (default-value 'mode-line-format))
        (powerline-set-selected-window)
        (powerline-reset)))

    (defun dotemacs//prepare-diminish ()
      (when spaceline-minor-modes-p
        (let ((unicodep (dotemacs-symbol-value
                         dotemacs-mode-line-unicode-symbols)))
          (dotemacs|do-after-display-system-init
           (setq spaceline-minor-modes-separator
                 (if unicodep (if (display-graphic-p) "" " ") "|")))
          (dolist (mm dotemacs--diminished-minor-modes)
            (let ((mode (car mm)))
              (when (and (boundp mode) (symbol-value mode))
                (let* ((unicode (cadr mm))
                       (ascii (caddr mm))
                       (dim (if unicodep
                                unicode
                              (if ascii ascii unicode))))
                  (diminish mode dim))))))))
    (add-hook 'spaceline-pre-hook 'dotemacs//prepare-diminish)))

(provide 'module-spaceline)
;;; module-spaceline.el ends here
