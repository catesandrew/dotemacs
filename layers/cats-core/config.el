(defvar cats/projectile-require-project-root
  "Require projectile root.")

(defvar cats/project-hook nil
  "Hooks run when a cats/project is fired.")

(defvar cats//executable-eslint nil)

(defvar cats/eslint-executable-hook nil
  "Hooks run when cats//executable-eslint is changed.")

(defvar cats//executable-jscs nil)

(defvar cats/jscs-executable-hook nil
  "Hooks run when cats//executable-jscs is changed.")

(defvar cats//executable-jshint nil)

(defvar cats/jshint-executable-hook nil
  "Hooks run when cats//executable-jshint is changed.")

(defvar cats//executable-tidy nil)

(defvar cats/tidy-executable-hook nil
  "Hooks run when cats//executable-tidy is changed.")

(defvar cats//executable-mocha nil)
(defvar cats/mocha-executable-hook nil
  "Hooks run when cats//executable-mocha is changed.")

(defvar cats//executable-babel-node nil)
(defvar cats/babel-node-executable-hook nil
  "Hooks run when cats//executable-babel-node is changed.")

(defvar cats//executable-coffeelint nil)
(defvar cats/coffeelint-executable-hook nil
  "Hooks run when cats//executable-coffeelint is changed.")

(defvar cats//executable-node nil)
(defvar cats/node-executable-hook nil
  "Hooks run when cats//executable-node is changed.")

(defvar cats//executable-phantomjs nil)
(defvar cats/phantomjs-executable-hook nil
  "Hooks run when cats//executable-phantomjs is changed.")

(defvar cats//executable-handlebars nil)
(defvar cats/handlebars-executable-hook nil
  "Hooks run when cats//executable-handlebars is changed.")

(defvar cats//executable-find nil)
(defvar cats/find-executable-hook nil
  "Hooks run when cats//executable-find is changed.")

(defvar cats/projectile-dir-root nil
  "The current dir root of the projectile project.")

(defvar cats/projectile-dir-base nil
  "The current base dir of the projectile project.")

(defvar cats//frame-width nil
  "Frame width.")

(defvar cats//frame-height nil
  "Frame height.")

(defvar buffer/force-save-some-buffers t
  "Force save buffers when focus is lost.")

(defvar buffer/do-not-kill-important-buffers t
  "Do not kill important buffer.")

;; Don't kill the important buffers
(defvar buffer/do-not-kill-buffer-names
  '("*scratch*"
    "*Messages*"
    "*Require Times*")
  "Names of buffers that should not be killed.")

(defvar desktop/desktop-dirname (concat spacemacs-cache-directory "desktop/")
  "Folder where to save desktop sessions.")

(defvar desktop/desktop-base-file-name
  (concat "emacs_" emacs-version-short ".desktop")
  "File names of desktop files.")

(defvar desktop/desktop-base-lock-name
  (concat "emacs_" emacs-version-short ".desktop.lock")
  "Desktop lock file name.")
