;;; config.el --- cats: Programming

;;; Commentary:

;; Personal functions

;;; Code:


;; To augment and/or counteract these defaults your own function
;; to cats/prog-mode-hook, using:
;;
;; (add-hook 'cats/prog-mode-hook 'cats/prog-mode-defaults t)
;;
;; The final optional t sets the *append* argument

(defvar cats/prog-mode-hook nil
  "Hooks run when `prog-mode-hook' is fired.")

(defvar cats/line-numbers nil
  "If non nil line numbers are turned on in all `prog-mode' and `text-mode'.
derivatives.  If set to `relative', also turns on relative line numbers.")

(defvar cats/prog-mode-spell-checking t
  "Enable `prog-mode' spell checking.")

(defvar cats/prog-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    ;; dash "-" is now a word character in programming mode
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    table))

(defvar pretty-symbols/elisp
  '(("add-hook" . ?)
    ;("add-hook" . #XF13D)
    ;("add-hook" . )
    ("=>" . 8658))
  "Symbols for elisp mode.")

(defvar pretty-symbols/js2
  '(("not" . 172)
    ("!" . 172)
    ("forall" . 8704)
    ("::" . 8759)
    ;; ("." . 8728)
    ("~>" . 8669)
    ;; ("()" . #X2205)
    ("==" . #X225F)
    ("!=" . #X2260)
    ("===" . #X2261)
    ("!==" . #X2262)
    (">=" . #X2265)
    ("<=" . #X2264)
    ("!!" . #X203C)
    ("&&" . #X2227)
    ("||" . #X2228)
    ;; ("null" . 00D8)
    ("sqrt" . #X221A)
    ("undefined" . #X22A5)
    ("pi" . #X3C0)
    ("function" . 955)
    ("->" . 8594)
    ("-<" . 8610)
    ("<-" . 8592)
    ("=>" . 8658)
    ;; ("map" . 8614)
    ("return" . 8592))
  "Symbols for js2 mode.")

(defvar pretty-symbols/prog
  '(("!" . 172)
    ("::" . 8759)
    ("~>" . 8669)
    ("==" . #X225F)
    ("!=" . #X2260)
    ("===" . #X2261)
    ("!==" . #X2262)
    (">=" . #X2265)
    ("<=" . #X2264)
    ("!!" . #X203C)
    ("&&" . #X2227)
    ("||" . #X2228)
    ("->" . 8594)
    ("-<" . 8610)
    ("<-" . 8592)
    ("=>" . 8658))
  "Symbols for general prog mode.")

(defvar cats/ycmd-server-command
  '("/usr/local/bin/python2" "-u" "/usr/local/src/ycmd/ycmd"))

;;; config.el ends here
