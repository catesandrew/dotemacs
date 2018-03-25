;;; eslint-fix.el --- eslint fix                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Andrew Cates

;; Author: Andrew Cates <andrew@cates.io>
;; Keywords: eslint

;;; Code:
(require 'dash)

(defgroup eslint-fix nil
  "Fix javascript code with eslint"
  :group 'tools)

(defcustom eslint-fix-executable "eslint"
  "The eslint executable used by `eslint-fix'."
  :group 'eslint-fix
  :type 'string)

(defcustom eslint-fix-preprocess-command nil
  "The shell command to pipe the buffer into before piping to eslint.
This is useful for integrating `prettier', for example. It is ignored if nil."
  :group 'eslint-fix
  :type 'string)

(defun eslint-fix--goto-line (line)
  "Move point to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun eslint-fix--delete-whole-line (&optional arg)
    "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
    (setq arg (or arg 1))
    (if (and (> arg 0)
             (eobp)
             (save-excursion (forward-visible-line 0) (eobp)))
        (signal 'end-of-buffer nil))
    (if (and (< arg 0)
             (bobp)
             (save-excursion (end-of-visible-line) (bobp)))
        (signal 'beginning-of-buffer nil))
    (cond ((zerop arg)
           (delete-region (progn (forward-visible-line 0) (point))
                          (progn (end-of-visible-line) (point))))
          ((< arg 0)
           (delete-region (progn (end-of-visible-line) (point))
                          (progn (forward-visible-line (1+ arg))
                                 (unless (bobp)
                                   (backward-char))
                                 (point))))
          (t
           (delete-region (progn (forward-visible-line 0) (point))
                                                  (progn (forward-visible-line arg) (point))))))

(defun eslint-fix--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in eslint-fix--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (eslint-fix--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (eslint-fix--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in eslint-fix--apply-rcs-patch")))))))))

(defun eslint-fix--replace-buffer-contents-via-patch (buffer file)
  "Replace BUFFER contents with contents of FILE.

Maintain point position as best as possible and minimize undo
size by applying the changes as a diff patch."
  (with-temp-buffer
    (let ((patch-buffer (current-buffer)))
      (with-current-buffer buffer
        (when (not (zerop
                    (call-process-region
                     (point-min) (point-max) "diff"
                     nil patch-buffer nil "-n" "-" file)))
          (eslint-fix--apply-rcs-patch patch-buffer))))))

(defun eslint-fix--buffer-contains-exit-codep ()
  "Return t if buffer ends with an eslint exit code."
  (goto-char (point-max))
  (beginning-of-line)
  (looking-at "# exit [[:digit:]]+"))

;; (defun eslint-fix--connection-filter (connection output)
;;   "Copy OUTPUT from CONNECTION to output buffer."
;;   (-when-let* ((output-buffer (process-get connection 'eslint-fix-output-buffer)))
;;     (with-current-buffer output-buffer
;;       (insert output))))

(defun eslint-fix ()
  "Use eslint to \"fix\ the current buffer."
  (interactive)
  (-when-let* ((buffer (current-buffer))
               (output-file (make-temp-file "eslint-fix-")))
    (unwind-protect
        (save-restriction
          (widen)
          (with-temp-buffer
            ;; (process-put connection 'eslint-fix-output-buffer (current-buffer))
            ;; (set-process-filter connection 'eslint-fix--connection-filter)
            ;; (with-current-buffer buffer
            ;;   (process-send-string connection
            ;;                        (concat
            ;;                         (combine-and-quote-strings
            ;;                          (list token
            ;;                                default-directory
            ;;                                "--fix-to-stdout"
            ;;                                "--stdin-filename" buffer-file-name
            ;;                                "--stdin"))
            ;;                         "\n"))
            ;;   (process-send-region connection (point-min) (point-max))
            ;;   (process-send-eof connection))

            ;; Wait for connection to close
            (when (eslint-fix--wait-for-connection-to-close connection)
              ;; Do not replace contents if there was an error or buffer is empty
              (unless (or (zerop (buffer-size))
                          (eslint-fix--buffer-contains-exit-codep))
                ;; Use write-region instead of write-file to avoid saving to
                ;; recentf and any other hooks.
                (let ((inhibit-message t))
                  (write-region (point-min) (point-max) output-file))
                (eslint-fix--replace-buffer-contents-via-patch buffer output-file))
              )))
      (delete-file output-file))
    ))

;;;###autoload
(define-minor-mode eslint-fix-mode
  "Use eslint_d to automatically fix javascript before saving."
  :lighter " fix"
  (if eslint-fix-mode
      (add-hook 'before-save-hook #'eslint-fix nil t)
    (remove-hook 'before-save-hook #'eslint-fix t)))

;; (defun eslint-fix ()
;;   "Format the current file with ESLint."
;;   (interactive)
;;   (if (executable-find "eslint")
;;     (progn (call-process "eslint" nil "*ESLint Errors*" nil "--fix" buffer-file-name)
;;       (revert-buffer t t t))
;;     (message "ESLint not found.")))

;; (defun cats/eslint-fix ()
;;   "Format the current file with ESLint."
;;   (interactive)
;;   (if cats//executable-eslint
;;     (progn (call-process cats//executable-eslint nil "*ESLint Errors*" nil "--fix" buffer-file-name)
;;       (revert-buffer t t t))
;;     (message "ESLint not found.")))
;; ;; (add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))

(provide 'eslint-fix)

;;; eslint-fix.el ends here
