(spacemacs/declare-prefix "of" "files")

;; File bindings
(spacemacs/set-leader-keys
  ;; Bindings in private key spacpacemacs
  "oR"  'cats/recompile-packages
  "ofd" 'cats/delete-file-and-buffer
  "ofD" 'cats/launch-dwim
  "ofr" 'cats/rename-file-and-buffer
  "ofu" 'cats/find-user-init-file-other-window
  "ofg" 'cats/browse-feature-url
  "ofi" 'cats/open-in-intellij
  "ofy" 'cats/copy-filename-as-kill)

(spacemacs/set-leader-keys
  "jp" 'pop-to-mark-command
  "jP" 'unpop-to-mark-command)

;;; keybindings.el ends here
