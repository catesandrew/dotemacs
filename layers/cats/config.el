;;; config.el --- cats: Configuration

;;; Commentary:

;; My personal configuration.

;;; Code:

(require 'time-date)

(defvar cats/verbose nil)

(defconst emacs-version-short
  (replace-regexp-in-string
   "\\([0-9]+\\)\\.\\([0-9]+\\).*"
   "\\1_\\2" emacs-version))

;; --no-color, oddly enough, is required to allow emacs to colorize the output
(defvar cats/git-grep-switches "--extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'.")

(when (spacemacs/window-system-is-mac)
  ;; To edit OSX binary and xml plist files, use the compressed file framework and
  ;; the plutil provided with OSX. Emacs provides jka-compr which decompresses a
  ;; file to stdout for reading, and compresses the data from stdin to write the
  ;; file back out again.
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])

  ;;It is necessary to perform an update!
  (jka-compr-update)
)


;; window-purpose
(defvar cats//purpose-x-compilation-conf nil
  "Configuration that gives compilation mode its own purpose.")


;; exec-path-from-shell
(defvar cats-envs
  '(
      ;; force GPG to not use an external tool for pin entry. That is particularly
      ;; useful if you don’t want the default GPG Agent pin entry tool to start,
      ;; particularly if you want Emacs to handle the pin entry for you.
      ;; (setf epa-pinentry-mode 'loopback)
      ("GPG_AGENT_INFO" . nil)
      ;; reset other shell vars
      ("PS1" . "\\h:\\W \\$ ")
      ("TERM_PROGRAM" . "")
      ("PROMPT_COMMAND" . "")
      ("HISTCONTROL" . "ignoreboth:erasedups")
      ;; Eternal bash history. Undocumented feature which sets the size to
      ;; "unlimited", http://stackoverflow.com/questions/9457233
      ("HISTFILESIZE" . "")
      ("HISTSIZE" . "")
      ;; Don't record some commands
      ;; - `npm +(ls|install|view|update)` will not record `npm ls`, `npm install`, etc.
      ;; - `ncu -+(a)` will not record `ncu -a`
      ;; - `* --+(h|he|hel|help)` will not record a single-word command followed by
      ;;   double dash `--h`, `--he`, etc.
      ;; - `* -+(h|he|hel|help)` will not record a single-word command followed by
      ;;   single dash `-h`, `-he`, etc.
      ;; - `+([-%+.0-9@A-Z_a-z])` - the best one by far since it will not record any
      ;;   single-word commands, or basically any command executed without parameters.
      ("HISTIGNORE" . "\"npm +(ls|install|view|update):ncu -+(a):cd -:mvim .:em .:* --+(h|he|hel|help):* -+(h|he|hel|help):+([-%+.0-9@A-Z_a-z])\"")
      ;; Useful timestamp format
      ("HISTTIMEFORMAT" . "\"%F %T \"")
      ;; Change the file location because certain bash sessions truncate .bash_history file upon close.
      ;; http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
      ("HISTFILE" . "~/.bash_eternal_history")
      ("AUTOFEATURE" . "true autotest")
      ("TERM" . "xterm-256color")
     ))

;;; config.el ends here
