;;; cats-combobulate-mode.el --- a minor mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andrew Cates

;; Author: Andrew Cates <acates@Principal-Software-Engineer>
;; Keywords: abbrev, abbrev,


(defvar cats-combobulate-mode-map (make-sparse-keymap)
  "Keymap for `cats-combobulate-mode'.")

;;;###autoload
(define-minor-mode cats-combobulate-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-mpereira-combobulate-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value nil
  :lighter " cats-combobulate-mode"
  :keymap cats-combobulate-mode-map)

;;;###autoload
(define-globalized-minor-mode global-cats-combobulate-mode
  cats-combobulate-mode
  cats-combobulate-mode)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((cats-combobulate-mode . ,cats-combobulate-mode-map)))

(defun cats/turn-off-cats-combobulate-mode ()
  "Turn off `cats-combobulate-mode'."
  (cats-combobulate-mode -1))

(add-hook 'minibuffer-setup-hook #'cats/turn-off-cats-combobulate-mode)

(dolist (hook '(bash-ts-mode-hook
                 c++-ts-mode-hook
                 c-ts-mode-hook
                 cmake-ts-mode-hook
                 csharp-ts-mode-hook
                 css-ts-mode-hook
                 dockerfile-ts-mode-hook
                 go-mod-ts-mode-hook
                 go-ts-mode-hook
                 java-ts-mode-hook
                 js-ts-mode-hook
                 json-ts-mode-hook
                 python-ts-mode-hook
                 ruby-ts-mode-hook
                 rust-ts-mode-hook
                 toml-ts-mode-hook
                 tsx-ts-mode-hook
                 typescript-ts-mode-hook
                 typescript-ts-mode-hook
                 yaml-ts-mode-hook
                 yaml-ts-mode-hook))
  (add-hook hook #'cats-combobulate-mode))

(provide 'cats-combobulate-mode)
