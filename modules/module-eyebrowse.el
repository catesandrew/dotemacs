;;; module-eyebrowse.el --- Eyebrowse Module
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;;; Commentary:
;;
;;; Code:
;; (require 'core-funcs)
(require 'core-keybindings)
(require 'core-transient-state)
;; (require 'module-vars)
;; (require 'module-common)
;; (require 'module-core)
(require 'module-utils)
(require 'dash)


;; funcs

(defvar eyebrowse-display-help t
  "If non-nil additional help is displayed when selecting a workspace.")

(dotemacs-declare-prefix "W" "workspaces")

;; Eyebrowse uses window-state objects (as returned by `window-state-get') to
;; store window configurations, so here are some utility functions to help us
;; analyse window-states. it might make more sense to move these functions to a
;; more general place

(defun dotemacs/window-state-window-p (object)
  "Return t if OBJECT is a window, as represented in window-state objects.
Note: this function doesn't test for real window objects, but for
representations of a window in a window-state object as returned by
`window-state-get'."
  (and (listp object)
       (memq (car object) '(leaf vc hc))))

(defun dotemacs/window-state-get-buffer (window)
  "Get WINDOW's buffer.
WINDOW is the representation of a window in a window-state
object. The returned value is the representation of a buffer in a
window-state object."
  (cdr (assq 'buffer window)))

(defun dotemacs/window-state-get-buffer-name (window)
  "Get WINDOW's buffer's name.
WINDOW is the representation of a window in a window-state object."
  (car (dotemacs/window-state-get-buffer window)))

(defun dotemacs/window-state-walk-windows-1 (window fn)
  "Helper function for `dotemacs/window-state-walk-windows'."
  ;; WINDOW is a misleading name. WINDOW is a list that can represent a window,
  ;; or a concatenation of several windows. window-state objects are weird.
  (let ((child-windows
         (-filter #'dotemacs/window-state-window-p window))
        (bare-window
         ;; if WINDOW contains more than one window, take only the first window
         (--take-while (not (dotemacs/window-state-window-p it))
                       window)))
    (--each child-windows
      (dotemacs/window-state-walk-windows-1 it fn))
    (push (funcall fn bare-window) result)))

(defun dotemacs/window-state-walk-windows (state fn)
  "Execute FN once for each window in STATE and make a list of the results.
FN is a function to execute. STATE is a window-state object."
  (let (result)
    (dotemacs/window-state-walk-windows-1 (cdr state) fn)
    result))

(defun dotemacs/window-state-all-windows (state)
  "Get all windows contained in STATE.
STATE is a window-state object. The returned windows are not
actual window objects. They are windows as represented in
window-state objects."
  (dotemacs/window-state-walk-windows state #'identity))

(defun dotemacs/window-state-get-buffer-names (state)
  "Get names of all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  (delq nil (dotemacs/window-state-walk-windows state #'dotemacs/window-state-get-buffer-name)))

(defun dotemacs/window-state-get-buffers (state)
  "Get all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  ;; delq nil - removes buffers stored in STATE that don't exist anymore
  (delq nil (mapcar #'get-buffer (dotemacs/window-state-get-buffer-names state))))

;; packages

;; (dotemacs-use-package-add-hook eyebrowse
;;   :post-config
;;   (progn
;;     (defun dotemacs//eyebrowse-dir-switch (&optional str)
;;       "Hook eyebrowse to projectile and neotree."
;;       (when dotemacs/verbose
;;         (message "!!! Running dotemacs//eyebrowse-dir-from-projectile-root")))
;;     (add-hook 'dotemacs/project-hook 'dotemacs//eyebrowse-dir-switch)
;;     (add-hook 'eyebrowse-post-window-switch-hook 'dotemacs/run-project-hook)))

(use-package eyebrowse
  :ensure t
  :defer t
  :diminish eyebrowse-mode
  :init
  (progn
    (setq eyebrowse-new-workspace #'dotemacs-home-delete-other-windows
          eyebrowse-wrap-around t)
    (eyebrowse-mode)

    ;; vim-style tab switching
    (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
    (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

    (dotemacs-set-leader-keys "bW" 'dotemacs/goto-buffer-workspace)

    (defun dotemacs/find-workspace (buffer)
      "Find Eyebrowse workspace containing BUFFER.
If several workspaces contain BUFFER, return the first one.
Workspaces are ordered by slot number. If no workspace contains
BUFFER, return nil."
      ;; the second element of a workspace is its window-state object
      (--find (memq buffer (dotemacs/window-state-get-buffers (cadr it)))
              (eyebrowse--get 'window-configs)))

    (defun dotemacs/display-in-workspace (buffer alist)
      "Display BUFFER's workspace.
Return BUFFER's window, if exists, otherwise nil. If BUFFER is
already visible in current workspace, just return its window
without switching workspaces."
      (or (get-buffer-window buffer)
          (-when-let (workspace (dotemacs/find-workspace buffer))
            (eyebrowse-switch-to-window-config (car workspace))
            (get-buffer-window buffer))))

    (defun dotemacs/goto-buffer-workspace (buffer)
      "Switch to BUFFER's window in BUFFER's workspace.
If BUFFER isn't displayed in any workspace, display it in the
current workspace, preferably in the current window."
      (interactive "B")
      (pop-to-buffer buffer '((;; reuse buffer window from some workspace
                               dotemacs/display-in-workspace
                               ;; fallback to display in current window
                               display-buffer-same-window)
                              (inhibit-same-window . nil))))

    (defun dotemacs/workspaces-ms-rename ()
      "Rename a workspace and get back to transient-state."
      (interactive)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
      (dotemacs/workspaces-transient-state/body))

    (defun dotemacs//workspaces-ms-get-slot-name (window-config)
      "Return the name for the given window-config"
      (let ((slot (car window-config))
            (caption (eyebrowse-format-slot window-config)))
        (if (= slot current-slot)
            (format "[%s]" caption)
          caption)))

    (defun dotemacs//workspaces-ms-get-window-configs ()
      "Return the list of window configs."
      (--sort (if (eq (car other) 0)
                  t
                (< (car it) (car other)))
              (eyebrowse--get 'window-configs)))

    (dotemacs-define-transient-state workspaces
        :title "Workspaces Transient State"
        :additional-docs
        (dotemacs--workspaces-ms-documentation .
         "\n\n[_0_.._9_] switch to workspace  [_n_/_p_] next/prev  [_<tab>_] last  [_c_] close  [_r_] rename")
        :bindings
        ("0" eyebrowse-switch-to-window-config-0)
        ("1" eyebrowse-switch-to-window-config-1)
        ("2" eyebrowse-switch-to-window-config-2)
        ("3" eyebrowse-switch-to-window-config-3)
        ("4" eyebrowse-switch-to-window-config-4)
        ("5" eyebrowse-switch-to-window-config-5)
        ("6" eyebrowse-switch-to-window-config-6)
        ("7" eyebrowse-switch-to-window-config-7)
        ("8" eyebrowse-switch-to-window-config-8)
        ("9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("C-i" eyebrowse-last-window-config)
        ("c" eyebrowse-close-window-config :exit t)
        ("h" eyebrowse-prev-window-config)
        ("l" eyebrowse-next-window-config)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("r" dotemacs/workspaces-ms-rename :exit t)
        ("w" eyebrowse-switch-to-window-config :exit t))

    (defun dotemacs//workspace-format-name (workspace)
      (let ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
            (name (nth 2 workspace))
            (number (car workspace)))
        (concat
         (if current "[" "")
         (if (< 0 (length name)) name (int-to-string number))
         (if current "]" ""))))

    (defun dotemacs//workspaces-ms-list ()
      "Return the list of workspaces for the workspacae transient state."
      (mapconcat 'dotemacs//workspace-format-name (eyebrowse--get 'window-configs) " | "))

    (add-hook 'dotemacs-post-user-config-hook
       (lambda ()
         (setq dotemacs/workspaces-transient-state/hint
               `(concat
                 ,(when dotemacs-show-transient-state-title
                    (concat
                     (propertize "Workspaces Transient State"
                                 'face 'dotemacs-transient-state-title-face)
                     "\n"))
                 (dotemacs//workspaces-ms-list)
                 dotemacs--workspaces-ms-documentation)))
       t))
  :config
  (progn))

(provide 'module-eyebrowse)
;;; module-eyebrowse.el ends here
