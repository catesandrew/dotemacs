;;; packages.el --- kubernetes Layer packages File for Spacemacs
;;; License: GPLv3

(defconst cats-vm-packages
  '(
     docker-tramp
     exec-path-from-shell
     kubernetes
     ;; kubernetes-evil
     kubernetes-tramp
     popwin
     timonier
     ))


;; docker-tramp
(defun cats-vm/pre-init-docker-tramp ()
  (setq docker-tramp-use-names t))


;; exec-path-from-shell
(defun cats-vm/pre-init-exec-path-from-shell ()
  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-config
    (dolist (var '(
                    "DOCKER_CERT_PATH"
                    "DOCKER_COMPLETION_TLS"
                    "DOCKER_HOST"
                    "DOCKER_MACHINE_NAME"
                    "DOCKER_NAMESPACE"
                    "DOCKER_PREFIX"
                    "DOCKER_REGISTRY"
                    "DOCKER_TLS_VERIFY"
                    "KUBECONFIG"
                    "MINIKUBE_HOME"
                    "VAGRANT_CHECKPOINT_DISABLE"
                    "VAGRANT_DOTFILE_PATH"
                    "VAGRANT_HOME"
                    "VAGRANT_VMWARE_CLONE_DIRECTORY"
                    "PACKER_CACHE_DIR"
                    ) exec-path-from-shell-variables)
      (unless (or (member var exec-path-from-shell-variables) (getenv var))
        (push var exec-path-from-shell-variables)))))


;; kubernetes
(defun cats-vm/init-kubernetes ()
  (use-package kubernetes
    :commands (kubernetes-overview)
    :defer t
    :config
    (progn
      (evil-set-initial-state 'kubernetes-mode 'motion)
      (evil-set-initial-state 'kubernetes-log-line-mode 'motion)
      (evil-set-initial-state 'kubernetes-logs-mode 'motion)
      (evil-set-initial-state 'kubernetes-overview-mode 'motion)
      (evil-set-initial-state 'kubernetes-display-thing-mode 'motion)

      (evilified-state-evilify-map kubernetes-mode-map
        :mode kubernetes-mode
        :bindings
        (kbd "p")   #'magit-section-backward
        (kbd "n")   #'magit-section-forward
        (kbd "M-p") #'magit-section-backward-sibling
        (kbd "M-n") #'magit-section-forward-sibling
        (kbd "C-i") #'magit-section-toggle
        (kbd "^")   #'magit-section-up
        [tab]       #'magit-section-toggle
        [C-tab]     #'magit-section-cycle
        [M-tab]     #'magit-section-cycle-diffs
        [S-tab]     #'magit-section-cycle-global

        [remap evil-next-line] #'next-line
        [remap evil-previous-line] #'previous-line
        [remap evil-next-visual-line] #'next-line
        [remap evil-previous-visual-line] #'previous-line

        (kbd "q") #'quit-window
        (kbd "RET") #'kubernetes-navigate
        (kbd "M-w") #'kubernetes-copy-thing-at-point

        (kbd "?") #'kubernetes-overview-popup
        (kbd "c") #'kubernetes-config-popup
        (kbd "g r") #'kubernetes-refresh
        (kbd "h") #'describe-mode
        (kbd "d") #'kubernetes-describe-popup
        (kbd "D") #'kubernetes-mark-for-delete
        (kbd "e") #'kubernetes-exec-popup
        (kbd "u") #'kubernetes-unmark
        (kbd "U") #'kubernetes-unmark-all
        (kbd "x") #'kubernetes-execute-marks
        (kbd "l") #'kubernetes-logs-popup
        (kbd "L") #'kubernetes-labels-popup))
    :init
    (progn
      (evil-leader/set-key
        "Ko" 'kubernetes-overview)))

  (with-eval-after-load 'kubernetes-overview
    (evilified-state-evilify-map kubernetes-overview-mode-map
      :mode kubernetes-overview-mode
      :bindings
      "q" 'quit-window
      "v" 'kubernetes-overview-set-sections))

  (with-eval-after-load 'kubernetes-display-thing
    (evilified-state-evilify-map kubernetes-display-thing-mode-map
      :mode kubernetes-display-thing-mode
      :bindings
      "q" 'quit-window))

  (with-eval-after-load 'kubernetes-log-line
    (evilified-state-evilify-map kubernetes-log-line-mode-map
      :mode kubernetes-log-line-mode
      :bindings
      "q" 'quit-window
      "n" 'kubernetes-logs-forward-line
      "p" 'kubernetes-logs-previous-line))

  (with-eval-after-load 'kubernetes-logs
    (evilified-state-evilify-map kubernetes-logs-mode-map
      :mode kubernetes-logs-mode
      :bindings
      "q" 'quit-window
      "n" 'kubernetes-logs-forward-line
      "p" 'kubernetes-logs-previous-line
      "RET" 'kubernetes-logs-inspect-line))

  (with-eval-after-load 'kubernetes-overview
    )
      )



;; kubernetes-tramp
(defun cats-vm/init-kubernetes-tramp ()
  (use-package kubernetes-tramp
    :defer t
    :init
    (progn
      (defvar spacemacs--kubernetes-tramp-loaded nil)
      (defadvice kubernetes-tramp-term (before spacemacs//load-kubernetes activate)
        "Lazy load kubernetes-tramp."
        (unless spacemacs--kubernetes-tramp-loaded
          (kubernetes-tramp-add-method)
          (setq spacemacs--kubernetes-tramp-loaded t)))
      (spacemacs/set-leader-keys "Kt" 'kubernetes-tramp-term))))


;; kubernetes-evil
;; (defun cats-vm/init-kubernetes-evil ()
;;   (use-package kubernetes-evil
;;     :defer t
;;     :after kubernetes))


;; timonier

(defun cats-vm/init-timonier ()
  (use-package timonier
    :defer t))

;; (defun cats-vm/init-dockerfile-mode ()
;;   (use-package docker-mode
;;     :defer t
;;     :config (evil-leader/set-key-for-mode 'dockerfile-mode
;;               "mcb" 'dockerfile-build-buffer)))


(defun cats-vm/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*kubernetes overview*" :dedicated t :position bottom)
          popwin:special-display-config)
    ))
