;;; funcs.el --- cats-scala: Functions

;;; Commentary:

;; Personal functions for scala programming

;;; Code:

;; (require 'dash)

;; configuration-layer/package-usedp
(when (configuration-layer/package-usedp 'dash)
  (defun cats-scala/find-config-file-in-sbt-project (file &optional _checker)
    "Find a config FILE in sbt project/ directories."
    (-when-let* ((file-name (buffer-file-name))
                 (root-dir (locate-dominating-file file-name "build.sbt"))
                 (project-dir (expand-file-name "project/" root-dir))
                 (config-file (expand-file-name file project-dir)))
      (when (file-exists-p config-file)
        config-file))))

(defun cats-scala/pop-to-sbt-frame ()
  "Open SBT REPL for this project in a new frame."
  (interactive)
  ;; Start SBT when no running, taken from `sbt:command'
  (when (not (comint-check-proc (sbt:buffer-name)))
    (sbt:run-sbt))

  (let ((display-buffer-overriding-action '(display-buffer-pop-up-frame)))
    (pop-to-buffer (sbt:buffer-name))
    ;; Dedicate the new window to the SBT REPL
    (set-window-dedicated-p (selected-window) t)))

;;; funcs.el ends here
