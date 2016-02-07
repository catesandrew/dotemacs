;;; module-smooth-scrolling.el --- Smooth Scrolling Module

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

;; (require 'core-funcs)
;; (require 'core-keybindings)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
(require 'module-utils)

(declare-function dotemacs/add-to-hooks "module-utils"
                  (fun hooks))
(declare-function dotemacs//unset-scroll-margin "module-utils" ())

;;; Code:

(defvar dotemacs-smooth-scrolling t
  "If non-nil, smooth scrolling (native-scrolling) is enabled.  Smooth scrolling
overrides the default behavior of Emacs which recenters the point when
it reaches the top or bottom of the screen.")

;; Defaults
(setq scroll-margin 5                   ; 0 to drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; scroll-preserve-screen-position t
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package smooth-scrolling
  :ensure t
  :defer t
  :if dotemacs-smooth-scrolling
  :init (setq smooth-scroll-margin 5
              scroll-conservatively 101
              scroll-preserve-screen-position t
              auto-window-vscroll nil)
  :config
  (progn
    ;; add hooks here only for emacs built-in packages
    (dotemacs/add-to-hooks 'dotemacs//unset-scroll-margin
                           '(messages-buffer-mode-hook))))

(unless dotemacs-smooth-scrolling
  ;; deactivate smooth-scrolling advices
  (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
  (ad-activate 'previous-line)
  (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
  (ad-activate 'next-line)
  (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
  (ad-activate 'isearch-repeat))

(provide 'module-smooth-scrolling)
;;; module-smooth-scrolling.el ends here