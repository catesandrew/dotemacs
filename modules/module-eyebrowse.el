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

    (defun dotemacs//workspaces-ts-toggle-hint ()
      "Toggle the full hint docstring for the workspaces transient-state."
      (interactive)
      (setq dotemacs--workspaces-ts-full-hint-toggle
            (logxor dotemacs--workspaces-ts-full-hint-toggle 1)))


    (defun dotemacs/workspaces-ts-rename ()
      "Rename a workspace and get back to transient-state."
      (interactive)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
      (dotemacs/workspaces-transient-state/body))

    (dotemacs|transient-state-format-hint workspaces
      dotemacs--workspaces-ts-full-hint
      "\n\n
 Go to^^^^^^                         Remove/Rename...^^
--^-^--^^^^-----------------------  --^-^---------------------------
 [_0_,_9_]^^     nth/new workspace   [_d_] close current workspace
 [_C-0_,_C-9_]^^ nth/new workspace   [_R_] rename current workspace
 [_n_/_C-l_]^^   next workspace
 [_N_/_p_/_C-h_] prev workspace
 [_<tab>_]^^^^   last workspace\n")

    (dotemacs-define-transient-state workspaces
        :title "Workspaces Transient State"
        :hint-is-doc t
        :dynamic-hint (dotemacs//workspaces-ts-hint)
        :bindings
        ("?" dotemacs//workspaces-ts-toggle-hint)
        ("0" eyebrowse-switch-to-window-config-0 :exit t)
        ("1" eyebrowse-switch-to-window-config-1 :exit t)
        ("2" eyebrowse-switch-to-window-config-2 :exit t)
        ("3" eyebrowse-switch-to-window-config-3 :exit t)
        ("4" eyebrowse-switch-to-window-config-4 :exit t)
        ("5" eyebrowse-switch-to-window-config-5 :exit t)
        ("6" eyebrowse-switch-to-window-config-6 :exit t)
        ("7" eyebrowse-switch-to-window-config-7 :exit t)
        ("8" eyebrowse-switch-to-window-config-8 :exit t)
        ("9" eyebrowse-switch-to-window-config-9 :exit t)
        ("C-0" eyebrowse-switch-to-window-config-0)
        ("C-1" eyebrowse-switch-to-window-config-1)
        ("C-2" eyebrowse-switch-to-window-config-2)
        ("C-3" eyebrowse-switch-to-window-config-3)
        ("C-4" eyebrowse-switch-to-window-config-4)
        ("C-5" eyebrowse-switch-to-window-config-5)
        ("C-6" eyebrowse-switch-to-window-config-6)
        ("C-7" eyebrowse-switch-to-window-config-7)
        ("C-8" eyebrowse-switch-to-window-config-8)
        ("C-9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("C-h" eyebrowse-prev-window-config)
        ("C-i" eyebrowse-last-window-config)
        ("C-l" eyebrowse-next-window-config)
        ("d" eyebrowse-close-window-config)
        ("h" eyebrowse-prev-window-config)
        ("l" eyebrowse-next-window-config)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("R" dotemacs/workspaces-ts-rename :exit t)
        ("w" eyebrowse-switch-to-window-config :exit t))
    (dotemacs-set-leader-keys
     "bW" 'dotemacs/goto-buffer-workspace
     "lw" 'dotemacs/workspaces-transient-state/body)

    (defun dotemacs//workspace-format-name (workspace)
      "Return a porpertized string given a WORKSPACE name."
      (let* ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
             (name (nth 2 workspace))
             (number (car workspace))
             (caption (if (< 0 (length name))
                          (concat (int-to-string number) ":" name)
                        (int-to-string number))))
        (if current
            (propertize (concat "[" caption "]") 'face 'warning)
          caption)))

    (defun dotemacs//workspaces-ts-hint ()
      "Return a one liner string containing all the workspace names."
      (concat
       " "
       (mapconcat 'dotemacs//workspace-format-name
                  (eyebrowse--get 'window-configs) " | ")
       (when eyebrowse-display-help dotemacs--workspaces-ts-full-hint))))
  :config
  (progn))

(provide 'module-eyebrowse)
;;; module-eyebrowse.el ends here
