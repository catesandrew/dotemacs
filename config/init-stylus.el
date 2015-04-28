(require 'init-programming)

(add-to-list 'auto-mode-alist
             '("\\.styl$" . (lambda ()
                              (require 'stylus-mode)
                              (stylus-mode))))

(after 'stylus-mode

  (defun my-stylus-mode-defaults ()
    ;; disable electric re-indenting...
    (electric-indent-local-mode -1)
  )

  (setq my-stylus-mode-hook 'my-stylus-mode-defaults)
  (add-hook 'stylus-mode-hook (lambda () (run-hooks 'my-stylus-mode-hook)))

  (defvar my-stylus-command-args nil
    "Additional list of arguments to pass into the stylus command.")

  (defvar my-stylus-display-buffer-name "*Stylus Output*"
    "The name of the Stylus buffer with CSS output.")

  (defvar my-stylus-last-command-mode nil)

  (defun my-stylus-process-sentinel (process event)
    (when (equal event "finished\n")
      (if my-stylus-last-command-mode
          (display-buffer my-stylus-display-buffer-name)
        (with-current-buffer my-stylus-display-buffer-name
          (skewer-css-eval-buffer)))))

  (defun my-stylus-setup-output-buffer (show)
    (let ((buffer (get-buffer-create my-stylus-display-buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)
        (when show
          (display-buffer buffer))
        (css-mode))))

  (defun my-stylus-async (begin end show)
    (require 'skewer-css)
    (my-stylus-setup-output-buffer show)
    (setq my-stylus-last-command-mode show)
    (let ((process (apply 'start-process
                          "stylus" my-stylus-display-buffer-name "stylus" my-stylus-command-args)))
      (set-process-sentinel process 'my-stylus-process-sentinel)
      (process-send-region process begin end)
      (process-send-eof process)))

  (defun my-stylus-compile-and-show-region (start end)
    (interactive "r")
    (my-stylus-async start end t))

  (defun my-stylus-compile-and-show-buffer ()
    (interactive)
    (my-stylus-async (point-min) (point-max) t))

  (defun my-stylus-compile-and-eval-buffer ()
    (interactive)
    (my-stylus-async (point-min) (point-max) nil))

  (defun my-stylus-mode-defaults ()
    (unless (process-status "httpd")
      (httpd-start))
    (run-hooks 'my-prog-mode-hook))

  (setq my-stylus-mode-hook 'my-stylus-mode-defaults)

  (add-hook 'stylus-mode-hook (lambda ()
                             (run-hooks 'my-stylus-mode-hook))))

(provide 'init-stylus)
