;;; core-evilified-state.el --- A minimalistic evil state

;; Copyright (C) 2014, 2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil spacemacs
;; Created: 22 Mar 2015
;; Version: 1.0
;; Package-Requires: ((evil "1.0.9") (evil-leader "0.4.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Define a `evilified' evil state inheriting from `emacs' state and
;; setting a minimalist list of Vim key bindings (like navigation, search, ...)

;;; Code:

(require 'evil)

(defvar dotemacs-core-evilified-state--modes nil
  "List of all evilified modes.")

(defvar dotemacs-core-evilified-state--visual-state-map evil-visual-state-map
  "Evil visual state map backup.")

(defvar dotemacs-core-evilified-state--evil-surround nil
  "Evil surround mode variable backup.")
(make-variable-buffer-local 'dotemacs-core-evilified-state--evil-surround)

(evil-define-state evilified
  "Evilified state.
 Hybrid `emacs state' with carrefully selected Vim key bindings."
  :tag " <Ev> "
  :enable (emacs)
  :message "-- EVILIFIED BUFFER --"
  :cursor box)

(add-hook 'evil-evilified-state-entry-hook 'dotemacs-evilified-state-on-entry)
(add-hook 'evil-evilified-state-exit-hook 'dotemacs-evilified-state-on-exit)

(defun dotemacs-evilified-state-on-entry ()
  "Setup evilified state."
  (setq-local dotemacs-core-evilified-state--evil-surround
              (bound-and-true-p evil-surround-mode))
  (when dotemacs-core-evilified-state--evil-surround
    (evil-surround-mode -1))
  (setq-local evil-visual-state-map (cons 'keymap nil))
  (add-hook 'evil-visual-state-entry-hook
            'dotemacs-evilified-state--visual-state-set-key nil 'local))

(defun dotemacs-evilified-state-on-exit ()
  "Cleanup evilified state."
  (when dotemacs-core-evilified-state--evil-surround
    (evil-surround-mode))
  (setq-local evil-visual-state-map
              dotemacs-core-evilified-state--visual-state-map)
  (remove-hook 'evil-visual-state-entry-hook
               'dotemacs-evilified-state--visual-state-set-key 'local))

(defun dotemacs-evilified-state--visual-state-set-key ()
  "Define key for visual state."
  (local-set-key "y" 'evil-yank))

;; default key bindings for all evilified buffers
(define-key evil-evilified-state-map (kbd dotemacs-leader-key)
  evil-leader--default-map)
(define-key evil-evilified-state-map "/" 'evil-search-forward)
(define-key evil-evilified-state-map ":" 'evil-ex)
(define-key evil-evilified-state-map "h" 'evil-backward-char)
(define-key evil-evilified-state-map "j" 'evil-next-visual-line)
(define-key evil-evilified-state-map "k" 'evil-previous-visual-line)
(define-key evil-evilified-state-map "l" 'evil-forward-char)
(define-key evil-evilified-state-map "n" 'evil-search-next)
(define-key evil-evilified-state-map "N" 'evil-search-previous)
(define-key evil-evilified-state-map "v" 'evil-visual-char)
(define-key evil-evilified-state-map "V" 'evil-visual-line)
; Add basic scroll commands to evilified commands
;
; C-f and C-b's original emacs functions are shadowed by h and l,
; C-j by x, and C-k by <leader>-u. G and gg are convenience commands.
(define-key evil-evilified-state-map "gg" 'evil-goto-first-line)
(define-key evil-evilified-state-map "G" 'evil-goto-line)
(define-key evil-evilified-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-evilified-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-evilified-state-map (kbd "C-j") 'evil-scroll-down)
(define-key evil-evilified-state-map (kbd "C-k") 'evil-scroll-up)

;; old macro
(defmacro evilify (mode map &rest body)
  "Set `evilified state' as default for MODE.

BODY is a list of additional key bindings to apply for the given MAP in
`evilified state'."
  (let ((defkey (when body `(evil-define-key 'evilified ,map ,@body))))
    `(progn (unless ,(null mode)
              (unless (memq ',mode dotemacs-core-evilified-state--modes)
                (push ',mode dotemacs-core-evilified-state--modes))
              (unless (memq ',mode evil-evilified-state-modes)
                (delq ',mode evil-emacs-state-modes)
                (push ',mode evil-evilified-state-modes)))
            (unless ,(null defkey) (,@defkey)))))


(provide 'core-evilified-state)

;;; core-evilified-state.el ends here