;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; https://github.com/emacs-evil/evil/issues/444
   ;; TODO: only enable in text-mode
   ;; evil-want-fine-undo t
   evil-want-C-u-scroll nil
   ;; Don't move back the cursor one position when exiting insert mode
   evil-move-cursor-back nil
   evil-jumps-post-jump-hook 'recenter
   configuration-layer-private-directory
   (let ((dotspacemacs-configuration-layer-private-directory
          configuration-layer-private-directory)
         (dotspacemacs-private-dir
          (when dotspacemacs-directory
            (expand-file-name
             (concat dotspacemacs-directory "private/")))))
     (if (and dotspacemacs-directory
              (file-exists-p dotspacemacs-private-dir))
         dotspacemacs-private-dir
       dotspacemacs-configuration-layer-private-directory))

   ;; prevent certain movement commands from breaking at the end of the lines
   ;; evil-move-beyond-eol t
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path
   (list (expand-file-name "layers/" dotspacemacs-directory))

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; System integration
     osx
     spacemacs-purpose
     ;; Editing
     helm
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence '"jk"
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      :disabled-for
                      markdown
                      git)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     colors
     emoji
     (typography :variables typography-enable-typographic-editing t)
     ;; Evil setup
     evil-commentary
     ;; Syntax and spell checking
     (spell-checking :variables
                     flyspell-default-dictionary "en")
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t)
     ;; Version control
     (version-control :variables
                      ;; No git-gutter please
                      version-control-diff-tool 'diff-hl)
     (git :variables
          ;; Magit in fullscreen
          git-magit-status-fullscreen t
          ;; Shut up, Magit
          magit-save-repository-buffers 'dontask
          ;; See commit counts for all branches and tags
          magit-refs-show-commit-count 'all
          ;; This is really creepy magit
          magit-revision-show-gravatars nil)
     (github :variables gh-profile-default-profile "catesandrew")
     ;; Development tools
     restclient
     dash
     vagrant
     ;; Programming and markup languages
     ansible
     csv
     docker
     pdf-tools
     rebox
     (ranger :variables
       ranger-show-preview t)
     pandoc
     fasd
     nginx
     emacs-lisp
     common-lisp
     plantuml
     (haskell :variables haskell-enable-hindent-style "johan-tibell"
                         haskell-completion-backend 'dante)
     (html :variables css-indent-offset 2)
     ipython-notebook
     javascript
     (latex :variables latex-enable-auto-fill t)
     markdown
     python
     (ruby :variables
        ruby-enable-enh-ruby-mode t
        ruby-test-runner 'rspec
        ruby-version-manager 'rbenv)
     go
     slack
     (rust :variables
           ;; Enable auto-completion for Rust
           rust-enable-racer t)
     (scala :variables
            ;; Automatically insert asterisk in comments
            scala-auto-insert-asterisk-in-comments t)
     shell-scripts
     sql
     vimscript
     yaml
     ;; Frameworks
     react
     ;; Applications
     (org :variables
          org-enable-org-journal-support t
          org-enable-bootstrap-support t
          org-enable-github-support t
          org-enable-hugo-support t
          org-want-todo-bindings t
          org-projectile-file "TODOs.org"
          org-enable-reveal-js-support t)
     (shell :variables
            shell-default-shell 'shell
            shell-default-term-shell "/bin/bash"
            sh-indentation 2
            sh-basic-offset 2)
     ycmd
     asciidoc
     search-engine
     ;; My personal layers
     cats
     cats-core
     cats-file
     cats-programming
     cats-emacs-lisp
     cats-javascript
     cats-utils
     cats-web
     cats-latex
     cats-markup
     (cats-org :variables
       org-enable-ox-support t
       org-enable-ox-latex-support t
       org-enable-ox-bibtex-support t
       org-enable-ox-beamer-support t
       org-enable-ox-md-support t
       org-enable-ox-publish-support t
       org-enable-ox-html-support t
       org-enable-jira-support t)
     cats-scala
     cats-xml
     cats-grammar
     cats-vm
     cats-langs
     cats-private
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
    '(
       ;; yasnippet-snippets
       ;; M-x all-the-icons-install-fonts
       all-the-icons
       )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     ;; Disable flex matching
     ;; helm-flx
     )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."

  (setq locale-coding-system    'utf-8)    ; pretty
  (set-terminal-coding-system   'utf-8)    ; pretty
  (set-keyboard-coding-system   'utf-8)    ; pretty
  (set-selection-coding-system  'utf-8)    ; please
  (set-language-environment     'utf-8)    ; with sugar on top

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         zenburn
                         farmhouse-dark
                         spacemacs-dark
                         spacemacs-light
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.0))
   dotspacemacs-mode-line-theme '(spacemacs :separator utf-8 :separator-scale 1.0)
   ;; dotspacemacs-mode-line-theme '(all-the-icons :separator-scale 1.0)

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("PragmataPro"
                               :size 10
                               :weight normal
                               :width normal)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key "\\"
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 96
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; Don't nag me compile!
  (setq compilation-ask-about-save nil)

  (setq cats/ycmd-server-command '("/usr/local/bin/python2" "-u" "/usr/local/src/ycmd/ycmd"))
  (setq spacemacs-useless-buffers-regexp '("^\\*[^\\*]+\\*$"))
  (setq spacemacs-useful-buffers-regexp '("\\*scratch\\*" "\\*spacemacs\\*"))

  (setq abbrev-file-name
        (expand-file-name (concat spacemacs-cache-directory "abbrev_defs")))
  (setq spacemacs-auto-save-directory
        (expand-file-name (concat spacemacs-cache-directory "auto-save/")))

  ;; Keep backup files out of the way
  (setq backup-directory-alist `((".*" . ,(concat spacemacs-cache-directory "backups"))))
  ;; don't create backup~ files
  (setq backup-by-copying t)
  (setq create-lockfiles nil)

  ;; Opt out from the startup message in the echo area by simply disabling this
  ;; ridiculously bizarre thing entirely.
  (fset 'display-startup-echo-area-message #'ignore)

  ;; Indicate empty lines at the end of a buffer in the fringe, but require a
  ;; final new line
  (setq indicate-empty-lines t)
  (setq require-final-newline t)

  ;; force GPG to not use an external tool for pin entry. That is particularly
  ;; useful if you don’t want the default GPG Agent pin entry tool to start,
  ;; particularly if you want Emacs to handle the pin entry for you.
  ;; (setf epa-pinentry-mode 'loopback)
  (setenv "GPG_AGENT_INFO" nil)

  (setq mark-ring-max 64)
  (setq kill-ring-max 200)
  (setq global-mark-ring-max 128))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."

  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "❀"))
  (spacemacs|do-after-display-system-init
    (cats//initialize-frame-uuid nil)
    (cats//initialize-frame-transparency)
    (spacemacs/set-default-font '("PragmataPro"
                                   :size 15
                                   :weight normal
                                   :width normal
                                   :powerline-scale 1.0))
    (cats//initialize-frame-fonts (selected-frame))
    (cats//set-frame-size (selected-frame))))

(spacemacs/defer-until-after-user-config
 '(lambda ()
    (setq-default frame-title-format
                  '(:eval
                    (if (cats//current-buffer-remote-p)
                        (format "%s@%s: %s %s"
                                (or (file-remote-p default-directory 'user)
                                    user-real-login-name)
                                (or (file-remote-p default-directory 'host)
                                    system-name)
                                (buffer-name)
                                (cond
                                 (buffer-file-truename
                                  (concat "(" buffer-file-truename ")"))
                                 (dired-directory
                                  (concat "{" dired-directory "}"))
                                 (t
                                  "[no file]")))
                      (format "%s"
                              (cond
                               (buffer-file-name
                                (cats/abbreviate-file-name buffer-file-name))
                               (buffer-file-truename
                                (cats/abbreviate-file-name buffer-file-truename))
                               (t
                                (buffer-name)))))))

    (setq-default c-basic-offset 2)
    (setq-default tab-width 2)
    (cats//locate-name)
    (cats//locate-email)
    (cats//locate-find)
    (cats//locate-tidy)

    ;; Inhibit killing of important buffers
    (when buffer/do-not-kill-important-buffers
      (add-hook 'kill-buffer-query-functions 'cats//do-not-kill-important-buffers))

    (pupo/update-purpose-config)

    ;; Autosave buffers when focus is lost, see
    (when buffer/force-save-some-buffers
      (add-hook 'focus-out-hook 'cats//force-save-some-buffers))))

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
