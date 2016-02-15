;;; core-vars.el --- Settings and variables

;; This file is NOT part of GNU Emacs.

;;; License:

;;; Commentary:

;;; Code:

(defconst dotemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "The storage location for various persistent files.")

(defconst emacs-version-short (replace-regexp-in-string
                               "\\([0-9]+\\)\\.\\([0-9]+\\).*"
                               "\\1_\\2" emacs-version))

(defvar dotemacs-leader-key ","
  "The leader key.")

(defvar dotemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

;; major-mode-leader to 0x00A0 (NO_BREAK_SPACE)
(defvar dotemacs-major-mode-leader-key "\\"
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotemacs-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotemacs-command-key ":"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")
(defvaralias 'dotemacs-emacs-command-key 'dotemacs-command-key
  "New official name for `dotemacs-command-key'")

(defvar dotemacs-remap-Y-to-y$ t
  "If non nil `Y' is remapped to `y$'.")

(defvar dotemacs-ex-substitute-global nil
  "If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.")

(defvar dotemacs-themes '(zenburn
                          solarized-dark
                          solarized-light
                          leuven
                          monokai
                          tao
                          ;; monster theme support
                          afternoon
                          ample
                          ample-zen
                          anti-zenburn
                          birds-of-paradise-plus
                          bubbleberry
                          busybee
                          cherry-blossom
                          clues
                          cyberpunk
                          dakrone
                          darkburn
                          darkmine
                          darktooth
                          django
                          espresso
                          farmhouse
                          firebelly
                          flatland
                          flatui
                          gandalf
                          gotham
                          grandshell
                          gruber-darker
                          gruvbox
                          hc-zenburn
                          heroku
                          inkpot
                          ir-black
                          jazz
                          jbeans
                          light-soap
                          lush
                          majapahit
                          material
                          minimal
                          molokai
                          monochrome
                          mustang
                          naquadah
                          niflheim
                          noctilux
                          obsidian
                          occidental
                          omtose-phellack
                          oldlace
                          organic-green
                          pastels-on-dark
                          phoenix-dark-mono
                          phoenix-dark-pink
                          planet
                          professional
                          purple-haze
                          seti
                          smyx
                          soft-charcoal
                          soft-morning
                          soft-stone
                          soothe
                          spacegray
                          subatomic
                          subatomic256
                          sunny-day
                          tango-2
                          tango-plus
                          tangotango
                          tronesque
                          twilight-anti-bright
                          twilight-bright
                          twilight
                          ujelly
                          underwater
                          zen-and-art)
  "List of themes, the first of the list is loaded when Emacs starts.
Press <Leader> T n to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light.")

(defvar dotemacs--cur-theme nil
  "The current theme.")

;; Source Code Pro for Powerline
;; Pragmata Pro
(defvar dotemacs-default-font '("Hasklig"
                                :size 13
                                :weight normal
                                :width normal
                                :powerline-scale 1.0)
  "Default font. `powerline-scale' allows to quickly tweak the mode-line
size to make separators look not too crappy.")

(defvar dotemacs-distinguish-gui-tab nil
  "If non nil, distinguish C-i and tab in the GUI version of Emacs.")

(defvar dotemacs-scratch-mode 'text-mode
  "Default major mode of the scratch buffer.")

(defvar dotemacs-show-transient-state-title t
  "If non nil show the titles of transient states.")

(defvar dotemacs-show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys.")

(defvar dotemacs-enable-paste-transient-state t
  "If non nil the paste transient-state is enabled. While enabled pressing `p`
several times cycle between the kill ring content.'")

(defvar dotemacs-default-package-repository 'melpa-stable
  "The default package repository used if no explicit repository has been
specified with an installed package.
NOT USED FOR NOW :-)")

(defvar dotemacs-elpa-https t
  "If non nil ELPA repositories are contacted via HTTPS whenever it's
possible. Set it to nil if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to t.")

(defvar dotemacs-elpa-timeout 5
  "Maximum allowed time in seconds to contact an ELPA repository.")

(provide 'core-vars)
;;; core-vars.el ends here
