;;; core-evilify-keymap.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defmacro dotemacs-evilify-map (map &rest props)
  "Evilify MAP."
  (declare (indent 1))
  (let* ((mode (plist-get props :mode))
         (evilified-map (plist-get props :evilified-map))
         (bindings (dotemacs-mplist-get props :bindings))
         (defkey (when bindings `(evil-define-key 'evilified ,map ,@bindings))))
    `(progn
       (let ((sorted-map (dotemacs-evilify-sort-keymap
                          (or ,evilified-map evil-evilified-state-map)))
             processed)
         (mapc (lambda (map-entry)
                 (unless (or (member (car map-entry) processed)
                             ;; don't care about evil-escape starter key
                             (and (boundp 'evil-escape-key-sequence)
                                  (equal (car map-entry)
                                         (elt evil-escape-key-sequence 0))))
                   (setq processed (dotemacs-evilify-event
                                    ,map ',map
                                    (car map-entry) (cdr map-entry)))))
               sorted-map))
       (unless ,(null defkey)
         (,@defkey))
       (unless ,(null mode)
         (dotemacs-evilify-configure-default-state ',mode)))))

(defun dotemacs-evilify-configure-default-state (mode)
  "Configure default state for the passed mode."
  (add-to-list 'evil-evilified-state--modes mode)
  (unless (bound-and-true-p holy-mode)
    (delq mode evil-emacs-state-modes)
    (add-to-list 'evil-evilified-state-modes mode)))

(defun dotemacs-evilify-event (map map-symbol event evil-value
                                     &optional processed pending-funcs)
  "Evilify EVENT in MAP and return a list of PROCESSED events."
  (if (and event (or evil-value pending-funcs))
      (let* ((kbd-event (kbd (single-key-description event)))
             (map-value (lookup-key map kbd-event)))
        (unless (and (symbolp map-value)
                     (string-match-p "--evilified" (symbol-name map-value)))
          (unless evil-value
            (setq evil-value (lookup-key evil-evilified-state-map kbd-event)))
          (when (or map-value pending-funcs)
            (let* ((pending-func (unless evil-value (pop pending-funcs)))
                   (evil-event (unless evil-value (cdr pending-func)))
                   (evil-value (or evil-value (car pending-func)))
                   (wrapper (dotemacs-evilify-wrapper map map-symbol
                                                        map-value event
                                                        evil-value evil-event)))
              (define-key map kbd-event wrapper)))
          (when map-value
            (add-to-list 'pending-funcs (cons map-value event) 'append))
          (push event processed)
          (setq processed (dotemacs-evilify-event
                           map map-symbol
                           (dotemacs-evilify-find-new-event event) nil
                           processed pending-funcs))))
    (when pending-funcs
      (warn
       (concat (format (concat "Auto-evilication could not remap these "
                               "functions in map `%s':\n")
                       map-symbol)
               (mapconcat (lambda (x)
                            (format "   - `%s' originally mapped on `%s''"
                                    (car x) (single-key-description (cdr x))))
                          pending-funcs "\n")))))
  processed)

(defun dotemacs-evilify-wrapper-name (map-symbol map-value event
                                                   evil-value evil-event)
  "Return a name for the wrapper function."
  (intern (if map-value
              (format "%s-or-%s--evilified-%s-%s"
                      (dotemacs-evilify-wrapper-value-symbol map-value event)
                      (dotemacs-evilify-wrapper-value-symbol evil-value
                                                               evil-event)
                      map-symbol (single-key-description event))
            (format "%s--evilified-%s-%s"
                    (dotemacs-evilify-wrapper-value-symbol evil-value
                                                             evil-event)
                    map-symbol (single-key-description event)))))

(defun dotemacs-evilify-wrapper-documentation (map-value
                                                 event evil-value evil-event)
  "Return a docstring for the wrapper function."
  (let ((map-string (dotemacs-evilify-wrapper-value-string map-value event))
        (evil-string (dotemacs-evilify-wrapper-value-string evil-value
                                                              evil-event)))
    (if map-value
        (format (concat "Wrap %s and %s.\n"
                        "In evilified state %s is executed. Whereas "
                        "in other states (i.e. emacs state) the stock "
                        "%s is executed.")
                map-string evil-string
                evil-string map-string)
      (format (concat "Wrap %s.\n"
                      "This function is a dummy wrapper which only "
                      "executes the %s.")
              evil-string evil-string))))

(defun dotemacs-evilify-wrapper-value-symbol (value event)
  "Return a symbol string given VALUE type and EVENT."
  (if (keymapp value)
      (format "keymap-%s" (single-key-description event))
    value))

(defun dotemacs-evilify-wrapper-value-string (value event)
  "Return a string given VALUE type and EVENT."
  (if (keymapp value)
      (format "keymap on key `%s'" (single-key-description event))
    (format "function `%s'" value)))

(defun dotemacs-evilify-call (value event)
  "Call VALUE depending on its type (symbol or keymap)."
  `(if ,(keymapp value)
       (progn
         (message "%s-" ,(single-key-description event))
         (,(if (version< emacs-version "24.4")
               'set-temporary-overlay-map
             'set-transient-map)
          ',value))
     (unless ,(null value)
       (call-interactively ',value))))

(defun dotemacs-evilify-wrapper (map map-symbol map-value event
                                       evil-value evil-event)
  "Define a wrapper for the passed event."
  (eval `(defun ,(dotemacs-evilify-wrapper-name
                  map-symbol map-value event evil-value evil-event) ()
           ,(dotemacs-evilify-wrapper-documentation
             map-value event evil-value evil-event)
           (interactive)
           (if (eq 'evilified evil-state)
               ;; evilified state
               ,(if evil-value
                   (dotemacs-evilify-call evil-value event)
                  (dotemacs-evilify-call map-value event))
             ;; other states (i.e. emacs)
             ,(dotemacs-evilify-call map-value event)))))

(defun dotemacs-evilify-find-new-event (event)
  "Return a new event for the evilified EVENT."
  (when event
    (cond
     ;; space
     ((equal event 32) nil)
     ((equal event ?/) nil)
     ((equal event ?:) nil)
     ((and (<= ?a event) (<= event ?z)) (- event 32))
     ((and (<= ?A event) (<= event ?Z)) (- event 64))
     ((and (<= 1 event) (<= event 26)) (+ (expt 2 25) event)))))

(defun dotemacs-evilify-sort-keymap (map)
  "Sort MAP following the order: `s' > `S' > `C-s' > `C-S-s'"
  (let (list)
    (map-keymap (lambda (a b) (push (cons a b) list)) map)
    (sort list
          (lambda (a b)
            (setq a (car a) b (car b))
            (if (integerp a)
                (if (integerp b)
                    (if (and (< a 256) (< b 256))
                        (> a b)
                      (< a b))
                  t)
              (if (integerp b) nil
                (string< a b)))))))

(provide 'core-evilify-keymap)