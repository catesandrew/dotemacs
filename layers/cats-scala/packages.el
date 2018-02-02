;;; packages.el --- cats-scala: Layer packages

;;; Commentary:

;; A personal scala layer.

;;; Code:

(defconst cats-scala-packages
  '(
    play-routes-mode
    sbt-mode
    ensime
    flycheck
    dash
    ))

(defun cats-scala/pre-init-dash ())

(defun cats-scala/init-play-routes-mode ()
  (use-package play-routes-mode
    :defer t))

(defun cats-scala/post-init-sbt-mode ()
  (with-eval-after-load 'scala-mode2
    (spacemacs/set-leader-keys-for-major-mode 'scala-mode
      "oi" #'cats-scala/pop-to-sbt-frame)))

(defun cats-scala/post-init-ensime ()
  (with-eval-after-load 'ensime-sbt
    ;; Unfortunately that's not customisable
    (advice-add 'ensime-sbt-do-compile :override
                (lambda ()
                  "Compile code and tests of this project."
                  (interactive)
                  (sbt-command "test:compile"))
                '((name . cats-sbt-compile-tests)))))

(defun cats-scala/post-init-flycheck ()
  (when (configuration-layer/package-usedp 'dash)
    (add-hook 'flycheck-locate-config-file-functions
              #'cats-scala/find-config-file-in-sbt-project)))

;;; packages.el ends here
