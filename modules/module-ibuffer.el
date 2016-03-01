;;; module-ibuffer.el --- iBuffer Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
(require 'core-vars)
(require 'core-funcs)
(require 'core-keybindings)
(require 'evil-evilified-state)
(require 'module-vars)
(require 'module-common)
;; (require 'module-core)
;; (require 'module-utils)
(require 'ibuffer)
(require 'evil-ex)
(require 'cl)

;;; Code:

(defvar ibuffer-group-buffers-by 'modes
  "If non nil ibuffer will group the buffers according to the passed symbol.
The supported values are `modes' to group by major-modes and `projects' to
group by projectile projects.")

(defun dotemacs//ibuffer-get-major-modes-ibuff-rules-list (mm-list result-list)
  (if mm-list
      (let* ((cur-mm (car mm-list))
             (next-res-list-el `(,(symbol-name cur-mm) (mode . ,cur-mm))))
        (dotemacs//ibuffer-get-major-modes-ibuff-rules-list
         (cdr mm-list) (cons next-res-list-el result-list)))
    result-list))

(defun dotemacs//ibuffer-get-major-modes-list ()
  (mapcar
   (function (lambda (buffer)
               (buffer-local-value 'major-mode (get-buffer buffer))))
   (buffer-list (selected-frame))))

(defun dotemacs//ibuffer-create-buffs-group ()
  (interactive)
  (let* ((ignore-modes '(Buffer-menu-mode
                         compilation-mode
                         minibuffer-inactive-mode
                         ibuffer-mode
                         magit-process-mode
                         messages-buffer-mode
                         fundamental-mode
                         completion-list-mode
                         help-mode
                         Info-mode))
         (cur-bufs
          (list (cons "Home"
                      (dotemacs//ibuffer-get-major-modes-ibuff-rules-list
                       (cl-set-difference
                        (remove-duplicates
                         (dotemacs//ibuffer-get-major-modes-list))
                        ignore-modes) '())))))
    (setq ibuffer-saved-filter-groups cur-bufs)
    (ibuffer-switch-to-saved-filter-groups "Home")))

(use-package ibuffer                    ; Better buffer list
  :init
  (progn
    (dotemacs-set-leader-keys "bB" 'ibuffer)
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (defun dotemacs//ibuffer-group-by-modes ()
      "Group buffers by modes."
      (when (eq 'modes ibuffer-group-buffers-by)
        (dotemacs//ibuffer-create-buffs-group)))
    (add-hook 'ibuffer-hook 'dotemacs//ibuffer-group-by-modes)

    (setq ibuffer-expert t
          ibuffer-show-empty-filter-groups nil)

    ;; Use ibuffer to provide :ls
    (evil-ex-define-cmd "buffers" 'ibuffer))
  :config
  (progn
    ;; Since we could override `,` with <leader>, let's make `;` do that
    ;; functionality
    (when (equal dotemacs-leader-key ",")
      (define-key ibuffer-mode-map
        (kbd ";") 'ibuffer-toggle-sorting-mode)
      (define-key ibuffer-mode-map
        (kbd ",") nil))
    (evilified-state-evilify-map ibuffer-mode-map
      :mode ibuffer-mode)))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :init
  (progn
    (defun dotemacs//ibuffer-group-by-projects ()
      "Group buffers by projects."
      (when (eq 'projects ibuffer-group-buffers-by)
        (ibuffer-projectile-set-filter-groups)
        (unless (eq ibuffer-sorting-mode 'alphabetic)
          (ibuffer-do-sort-by-alphabetic))))

    (add-hook 'ibuffer-hook 'dotemacs//ibuffer-group-by-projects)))

(provide 'module-ibuffer)
;;; module-ibuffer.el ends here
