;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defvar cats-enable-edit-server t
  "If non-nil, start an Emacs server if one is not already running.")

(defvar cats-edit-server-start-run nil
  "Whether `edit-server-start' has been run")

(defvar cats-frame-font-size 16
  "Frame font size.")

(when (display-graphic-p)
  (let* ((ffs (if (> (x-display-pixel-width) 2000) 18 16)))
    (setq confirm-kill-emacs 'yes-or-no-p)
    (setq cats-frame-font-size ffs)))

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; https://github.com/emacs-evil/evil/issues/444
   ;; TODO: only enable in text-mode
   ;; evil-want-fine-undo t
   evil-want-C-u-scroll nil
   ;; Don't move back the cursor one position when exiting insert mode
   evil-move-cursor-back nil
   evil-jumps-post-jump-hook 'recenter
   spacemacs-private-directory
   (let ((dotspacemacs-configuration-layer-private-directory
           spacemacs-private-directory)

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

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path
   (list (expand-file-name "layers/" dotspacemacs-directory))

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; System integration
     osx
     spacemacs-purpose
     spacemacs-evil
     better-defaults
     graphql
     ;; extra-langs

     ;; Editing
     ;; helm-follow-mode sticky - remembers use of C-c C-f
     ;; - follow mode previews when scrolling through a helm list
     (helm :variables
       helm-follow-mode-persistent t
       ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
       helm-enable-auto-resize nil
       ;; if non nil, the helm header is hidden when there is only one source.
       ;; (default nil)
       helm-no-header nil
       ;; define the position to display `helm', options are `bottom', `top',
       ;; `left', or `right'. (default 'bottom)
       helm-position 'bottom
       ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
       ;; in all non-asynchronous sources. If set to `source', preserve individual
       ;; source settings. Else, disable fuzzy matching in all sources.
       ;; (default 'always)
       helm-use-fuzzy 'always)
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence '"jk"
                      ;; Auto-complete less aggressively
                      ;; auto-completion-idle-delay 0.5
                      ;; Set to 0.0 for optimal results with lsp mode
                      auto-completion-idle-delay 0.0
                      ;; auto-completion-minimum-prefix-length 2
                      ;; Set to 1 for optimal results with lsp mode.
                      auto-completion-minimum-prefix-length 1
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-help-tooltip 'manual
                      auto-completion-use-company-box t
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
     (typography :variables typography-enable-typographic-editing nil)
     ;; Evil setup
     evil-commentary
     ;; Syntax and spell checking
     (spell-checking :variables
                     flyspell-default-dictionary "en_US"
                     spell-checking-enable-auto-dictionary nil)
     ;; Use original flycheck fringe bitmaps
     (syntax-checking :variables
                      ;; syntax-checking-use-original-bitmaps nil
                      ;; syntax-checking-indication-symbol t
                      syntax-checking-enable-tooltips t)

     ;; Highlight changes in buffers
     ;; SPC g . transient state for navigating changes
     (version-control :variables
                      version-control-diff-tool 'git-gutter+
                      ;; version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     ;; opens Magit git client full screen (q restores previous layout)
     ;; refine hunk 'all highlights characters changed on each line
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all)

     ;; github
     ;; Development tools
     restclient
     copy-as-format
     dash
     (terraform :variables terraform-auto-format-on-save t)
     lua
     ;; (clojure :variables clojure-enable-linters '(clj-kondo joker))
     (clojure :variables
              clojure-toplevel-inside-comment-form t
              cider-overlays-use-font-lock t
              clojure-enable-fancify-symbols t
              clojure-enable-linters 'clj-kondo
              cider-preferred-build-tool 'clojure-cli)

     ;; SPC a L displays key and command history in a separate buffer
     command-log
     ;; graphviz - open-source graph declaration system
     ;; Used to generated graphs of Clojure project dependencies
     graphviz
     ;; gradle build --refresh-dependencies (from android)
     (groovy :variables
       groovy-backend 'company-groovy)
     (java :variables
       java-backend 'meghanada)
     windows-scripts
     vagrant
     ;; Programming and markup languages
     ansible
     csv
     (cmake :variables
       cmake-backend 'company-cmake
       cmake-enable-cmake-ide-support t)
     neotree
     docker
     rebox
     ;; Text-based file manager with preview
     (ranger :variables
             ranger-show-preview t
             ranger-show-hidden t
             ranger-cleanup-eagerly t
             ranger-cleanup-on-disable t
             ranger-ignored-extensions '("mkv" "flv" "iso" "mp4"))
     pandoc
     nginx
     emacs-lisp
     common-lisp
     plantuml
     (haskell :variables haskell-enable-hindent-style "johan-tibell"
       haskell-enable-ghc-mod-support t
       haskell-completion-backend 'dante)

     ;; npm i -g \
     ;;   vscode-html-languageserver-bin \
     ;;   bash-language-server \
     ;;   dockerfile-language-server-nodejs \
     ;;   flow-language-server \
     ;;   javascript-typescript-langserver \
     ;;   tslint \
     ;;   typescript-formatter \
     ;;   typescript-language-server \
     ;;   typescript \
     ;;   vscode-css-languageserver-bin \
     ;;   vscode-json-languageserver-bin \
     ;;   stylelint \
     ;;   yaml-language-server
     ;;   graphql-language-service-cli
     ;;   vim-language-server
     (html :variables
           css-indent-offset 2
           web-fmt-tool 'web-beautify)
     ipython-notebook
     tide
     (lsp :variables
          lsp-ui-doc-enable nil
          lsp-ui-sideline-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-lens-enable t)

     ;; react layer uses the same backend defined in javascript layer.
     ;; npm i -g eslint babel-eslint eslint-plugin-react js-beautify prettier
     react
     (typescript :variables
                 typescript-fmt-on-save t
                 typescript-linter 'eslint
                 typescript-backend 'lsp
                 typescript-lsp-linter nil
                 typescript-fmt-tool 'typescript-formatter)
     (javascript :variables
                 javascript-fmt-tool 'web-beautify
                 ;; Repl to be configured by the layer, `skewer' for browser
                 ;; based, `nodejs' for server based development.
                 javascript-repl 'nodejs
                 javascript-backend nil
                 javascript-lsp-linter nil
                 javascript-import-tool 'import-js)
     tern
     ;; brew install tree-sitter
     ;; git clone git@github.com:casouri/tree-sitter-module.git
     ;; cd tree-sitter-module/
     ;; ./batch.sh
     ;; mkdir ~/.emacs.d/tree-sitter
     ;; mv dist/* ~/.emacs.d/tree-sitter/
     ;;
     ;; Note that I output the resulting file into the `~/.emacs.d/tree-sitter`
     ;; directory, which is where Emacs is looking for it on my local machine;
     ;; see `treesit-extra-load-path` for details. With all of the above in
     ;; place, run `js-ts-mode` again and you should be all set.
     (tree-sitter :variables
       tree-sitter-indent-enable t
       tree-sitter-syntax-highlight-enable t
       tree-sitter-fold-enable t
       spacemacs-tree-sitter-hl-black-list '(js2-mode rjsx-mode)
       tree-sitter-fold-indicators-enable nil)
     ;; npm install -g import-js
     import-js
     ;; (tern
     ;;   ;; do not use no-port-file under emacs, it'll mess things up when you
     ;;   ;; are editing multiple files in the same project
     ;;   tern-disable-tern-port-files nil)
     (json :variables js-indent-level 2)
     ;; new layer web-beautify extracted from javascript layer
     ;; npm i -g js-beautify
     web-beautify
     bibtex
     (latex :variables latex-enable-auto-fill t
                       latex-build-command "latexmk-osx"
                       latex-enable-magic nil
                       latex-enable-folding t
                       latex-refresh-preview t
                       ;; latex-nofill-env (append latex-nofill-env '("puml"))
                       )
     markdown
     ;; pip install python-language-server
     python
     php
     swift
     (unicode-fonts :variables
       unicode-fonts-enable-ligatures t
       unicode-fonts-force-multi-color-on-mac t)
     octave
     ;; gem install solargraph bundler
     (ruby :variables
        ruby-enable-enh-ruby-mode t
        ruby-test-runner 'rspec
        ruby-version-manager 'rbenv)
     ;; go install github.com/sourcegraph/go-langserver
     go
     slack
     (rust :variables
           ;; Enable auto-completion for Rust
           rust-enable-racer t)
     shell-scripts
     (sql :variables sql-capitalize-keywords t)
     vimscript
     xclipboard
     yaml
     ;; Applications
     (org :variables
          org-enable-roam-support t
          org-enable-org-journal-support t
          org-enable-bootstrap-support t
          org-enable-github-support t
          org-enable-hugo-support t
          org-want-todo-bindings t
          org-projectile-file "TODOs.org"
          org-enable-reveal-js-support t)
     (deft :variables
          deft-zetteldeft t)
     (shell :variables
            shell-default-shell 'vterm
            shell-default-term-shell "/bin/bash"
            shell-default-height 30
            shell-default-position 'bottom
            sh-indentation 2
            sh-basic-offset 2)
     ;; spacemacs-layouts layer added to set variables
     (spacemacs-layouts :variables
       spacemacs-layouts-restrict-spc-tab t
       persp-autokill-buffer-on-remove t)
     asciidoc
     search-engine
     templates
     pass
     (mu4e :variables
       mu4e-enable-mode-line t
       mu4e-org-compose-support t)
     notmuch
     ;; My personal layers
     cats
     cats-core
     cats-file
     cats-programming
     cats-lisp
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
       org-enable-ox-ascii-support t
       org-enable-ox-confluence-support t
       org-enable-jira-support t)
     ;; cats-scala
     cats-xml
     cats-grammar
     cats-vm
     cats-langs
     cats-mail
     ;; cats-private
    )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
    '(
       ;; yasnippet-snippets
       ;; M-x all-the-icons-install-fonts
       all-the-icons
       ng2-mode
       ;; elcord ;; discord
       ;; graphql-mode
       ;; eslint-fix
       ;; oneonone
       ;; dracula-theme
       ;; prettier-js
       ;; nord-theme
       ;; People have reported problems getting this to run from git in
       ;; spacemacs. I believe the cause of this is how require-relative and
       ;; require-relative-list work.
       ;; realgud
       ;; realgud-pry
       ;; realgud-maxima
       ;; realgud-node-inspect
       )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."

  (setq locale-coding-system    'utf-8)    ; pretty
  (set-terminal-coding-system   'utf-8)    ; pretty
  (set-keyboard-coding-system   'utf-8)    ; pretty
  (set-selection-coding-system  'utf-8)    ; please
  (set-language-environment     'utf-8)    ; with sugar on top

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
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

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style
    '(vim :variables
       ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
       vim-style-remap-Y-to-y$ t
       ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
       ;; there. (default t)
       vim-style-retain-visual-state-on-shift t
       ;; If non-nil, J and K move lines up and down when in visual mode.
       vim-style-visual-line-move-text nil
       ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
       vim-style-ex-substitute-global nil)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         zenburn
                         farmhouse-dark
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         gandalf
                         ample-light
                         leuven
                         plan9
                         twilight-bright
                         anti-zenburn
                         moe
                         espresso
                         tango-plus
                         soft-stone
                         flatui
                         faff
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator utf-8 :separator-scale 1.0)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font `("PragmataPro Liga"
                               :size ,cats-frame-font-size
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
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
   dotspacemacs-switch-to-buffer-prefers-purpose t

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 100

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; Don't nag me compile!
  (setq compilation-ask-about-save nil)

  (setq abbrev-file-name
        (expand-file-name (concat spacemacs-cache-directory "abbrev_defs")))
  (setq spacemacs-auto-save-directory
        (expand-file-name (concat spacemacs-cache-directory "auto-save/")))

  ;; Force GPG to not use an external tool for pin entry. That is particularly
  ;; useful if you don’t want the default GPG Agent pin entry tool to start,
  ;; particularly if you want Emacs to handle the pin entry for you.
  ;; (setf epa-pinentry-mode 'loopback)
  (setenv "GPG_AGENT_INFO" nil)
  ;; reset other shell vars
  (setenv "PS1" "\\h:\\W \\$ ")
  (setenv "TERM_PROGRAM" "")
  (setenv "PROMPT_COMMAND" "")
  (setenv "HISTCONTROL" "ignoreboth:erasedups")
  ;; Eternal bash history. Undocumented feature which sets the size to
  ;; "unlimited", http://stackoverflow.com/questions/9457233
  (setenv "HISTFILESIZE" "")
  (setenv "HISTSIZE" "")

  ;; Don't record some commands
  ;; - `npm +(ls|install|view|update)` will not record `npm ls`, `npm install`, etc.
  ;; - `ncu -+(a)` will not record `ncu -a`
  ;; - `* --+(h|he|hel|help)` will not record a single-word command followed by
  ;;   double dash `--h`, `--he`, etc.
  ;; - `* -+(h|he|hel|help)` will not record a single-word command followed by
  ;;   single dash `-h`, `-he`, etc.
  ;; - `+([-%+.0-9@A-Z_a-z])` - the best one by far since it will not record any
  ;;   single-word commands, or basically any command executed without parameters.
  (setenv "HISTIGNORE" "\"npm +(ls|install|view|update):ncu -+(a):cd -:mvim .:em .:* --+(h|he|hel|help):* -+(h|he|hel|help):+([-%+.0-9@A-Z_a-z])\"")

  ;; Useful timestamp format
  (setenv "HISTTIMEFORMAT" "\"%F %T \"")

  ;; Change the file location because certain bash sessions truncate .bash_history file upon close.
  ;; http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
  (setenv "HISTFILE" "~/.bash_eternal_history")
  (setenv "AUTOFEATURE" "true autotest")
  (setenv "TERM" "xterm-24bit")
)

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
)

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

 (add-to-list 'latex-nofill-env 'code)
 (add-to-list 'latex-nofill-env 'puml)

  ;; Opt out from the startup message in the echo area by simply disabling this
  ;; ridiculously bizarre thing entirely.
  (fset 'display-startup-echo-area-message #'ignore)

  ;; termina-her to use iterm2
  (setq terminal-here-mac-terminal-command 'iterm2)

  ;; (push "^\\*[^\\*]+\\*$" spacemacs-useless-buffers-regexp)
  ;; (push "\\*scratch\\*" spacemacs-useful-buffers-regexp)
  ;; (push "\\*spacemacs\\*" spacemacs-useful-buffers-regexp)
  ;; (push "\\*Messages\\*" spacemacs-useful-buffers-regexp)

  ;; Keep backup files out of the way
  (setq backup-directory-alist `((".*" . ,(concat spacemacs-cache-directory "backups"))))
  ;; don't create backup~ files
  (setq backup-by-copying t)
  (setq create-lockfiles nil)

  ;; https://github.com/emacs-lsp/lsp-mode/issues/3173
  (setq lsp-completion-provider :none)

  ;; Indicate empty lines at the end of a buffer in the fringe, but require a
  ;; final new line
  (setq-default indicate-empty-lines t)
  (setq-default require-final-newline t)

  (setq mark-ring-max 256)
  (setq kill-ring-max 960)
  (setq global-mark-ring-max 256)

  ;; Autosave buffers when focus is lost
  (when buffer/force-save-some-buffers
    (add-hook 'focus-out-hook 'cats//force-save-some-buffers))

  ;; find aspell and hunspell automatically
  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  (async-start
    `(lambda ()
       ;; try hunspell at first, if hunspell does NOT exist, use aspell
       (executable-find "hunspell"))
    (lambda (result)
      (if result
        (with-eval-after-load "ispell"
          (setq ispell-program-name result)
          (setq ispell-local-dictionary-alist
            ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters
            ;; passed to hunspell You could use `("-d" "en_US,en_US-med")` to check
            ;; with multiple dictionaries
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
          (setq ispell-local-dictionary "en_US"))
        (async-start
          `(lambda ()
             (executable-find "aspell"))
          (lambda (result)
            (when result
              (with-eval-after-load "ispell"
                (setq ispell-program-name result)
                ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
                (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))))))))

  ;; seems to be needed to avoid weird artefacts with first graphical client
  (spacemacs|do-after-display-system-init
    "Ran on *first* instance of emacsclient."
    (progn
      (when (display-graphic-p)
        (let* ((ffs (if (> (x-display-pixel-width) 2000) 18 16)))
          (setq cats-frame-font-size ffs)))

      (spacemacs/set-default-font `("PragmataPro Liga"
                                     :size ,cats-frame-font-size
                                     :weight normal
                                     :width normal
                                     :powerline-scale 1.0)))


    ;; initialize frame settings with first graphical client
    (cats//initialize-frame-size)
    (cats//initialize-frame-transparency)

    (let* ((frame (selected-frame)))
      (cats//initialize-frame-uuid frame)
      (cats//toggle-frame-fonts frame)
      (cats//toggle-frame-size frame)
      (cats/toggle-transparency frame))

    ;; reuse for new frames created with emacsclient
    (add-hook 'after-make-frame-functions 'cats//toggle-frame-fonts)
    (add-hook 'after-make-frame-functions 'cats//toggle-frame-size)
    (add-hook 'after-make-frame-functions 'cats/toggle-transparency))
)

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
    (setq-default standard-indent 2)
    (cats//locate-name)
    (cats//locate-email)
    (cats//locate-find)
    (cats//locate-tidy)

    ;; Inhibit killing of important buffers
    (when buffer/do-not-kill-important-buffers
      (add-hook 'kill-buffer-query-functions 'cats//do-not-kill-important-buffers))

    ;; TODO: Look for alternative new updated call in window-purpose package
    ;; (pupo/update-purpose-config)
    ;; UPDATE: Is it this?
    ;; (purpose-compile-user-configuration)

    (cats//kickoff-project-hook (selected-frame))
    (when cats-enable-edit-server
      (unless cats-edit-server-start-run
        (edit-server-start)
        (setq cats-edit-server-start-run t)))
    (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))))

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
