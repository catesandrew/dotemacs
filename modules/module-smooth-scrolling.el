;;; module-smooth-scrolling.el --- Smooth Scrolling Module

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

(require 'use-package)
;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'module-vars)
(require 'module-common)
;; (require 'module-core)
(require 'module-utils)

;;; Code:

(defvar dotemacs-smooth-scrolling t
  "If non-nil, smooth scrolling (native-scrolling) is enabled.
Smooth scrolling overrides the default behavior of Emacs which
recenters the point when it reaches the top or bottom of the
screen.")

(dotemacs-use-package-add-hook smooth-scrolling
  :pre-init
  (setq scroll-conservatively 1000        ; Never recenter the screen while scrolling
        scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
        ;; scroll-preserve-screen-position t
        ;; These settings make trackpad scrolling on OS X much more predictable
        ;; and smooth
        mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(1)))

(use-package smooth-scrolling
  :ensure t
  :init
  (progn
    (setq smooth-scroll-margin 5)
    (dotemacs-add-toggle smooth-scrolling
      :status smooth-scrolling-mode
      :on (progn
            (smooth-scrolling-mode)
            (enable-smooth-scroll-for-function previous-line)
            (enable-smooth-scroll-for-function next-line)
            (enable-smooth-scroll-for-function isearch-repeat))
      :off (progn
             (smooth-scrolling-mode -1)
             (disable-smooth-scroll-for-function previous-line)
             (disable-smooth-scroll-for-function next-line)
             (disable-smooth-scroll-for-function isearch-repeat))
      :documentation "Smooth scrolling."
      :evil-leader "tv")
    (when dotemacs-smooth-scrolling
      (dotemacs-toggle-smooth-scrolling-on))
    ;; add hooks here only for emacs built-in packages that are not owned
    ;; by a layer.
    (defun dotemacs//unset-scroll-margin ()
      "Set scroll-margin to zero."
      (setq-local scroll-margin 0))
    (dotemacs/add-to-hooks 'dotemacs//unset-scroll-margin
                           '(messages-buffer-mode-hook)))
  :config
  (progn))

(provide 'module-smooth-scrolling)
;;; module-smooth-scrolling.el ends here
