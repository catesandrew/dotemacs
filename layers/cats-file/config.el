;; Assert the byte compiler that dired functions are defined, because we never
;; call them for non-dired buffers, so we can be sure that dired is always
;; loaded first.
(declare-function dired-get-marked-files "dired")
(declare-function dired-current-directory "dired")

;; Don't kill the important buffers
(defvar buffer/do-not-kill-buffer-names
  '("*scratch*"
    "*Messages*"
    "*Require Times*")
  "Names of buffers that should not be killed.")


(defvar ignoramus/directory-basename-exact-names
  '(
    ".coverage"                            ; python
    ".git"                                 ; git
     ".gradle"                             ; gradle
    ".circleci"                            ; circle ci
    ".nyc_output"                          ; nyc
    ".hg"                                  ; mercurial
    ".idea"                                ; various
    "build"                                ; various
    "Build"                                ; various
    ".cask"
    ".vagrant"
    "lib-cov"
    "coverage"
    "bower_components"
    "node_modules"
    ".cache"
    ".ccache"
    "Pods"
    ".log"
    "auto"
    ".vscode"
    ".sx"
    "elpa"
    ))

;; List of exact filenames to ignore. These are not regular expressions, but
;; literal strings which exactly match a file or directory name to ignore.
;; The string to match comprises only the last element of a fully-qualified
;; pathname."
(defvar ignoramus/file-basename-exact-names
  '(
    ;; "$RECYCLE.BIN"                         ; ms-windows
    ".AppleDouble"                         ; OS X
    ".DS_Store"                            ; OS X
    ".DocumentRevisions-V100"              ; OS X
    ".LSOverride"                          ; OS X
    ;; ".Rhistory"                            ; R
    ".Spotlight-V100"                      ; OS X
    ".TemporaryItems"                      ; OS X
    ".Trashes"                             ; OS X
    ;; ".actionScriptProperties"              ; actionscript
    ".apt_generated"                       ; gwt
    ".bdfcache.el"                         ; emacs ps-bdf
    ".build"                               ; perl
    ;; ".buildpath"                           ; eclipse
    ;; ".builds"                              ; visualstudio
    ;; ".bzr"                                 ; bazaar
    ;; ".cdv"                                 ; codeville
    ;; ".classpath"                           ; eclipse
    ".com.apple.timemachine.donotpresent"  ; OS X
    ".com.apple.timemachine.supported"     ; OS X
    ;; ".cproject"                            ; eclipse
    ".directory"                           ; KDE
    ;; ".dropbox"                             ; dropbox
    ;; ".dropbox.cache"                       ; dropbox
    ".emacs-places"                        ; emacs saveplace
    ".emacs.desktop"                       ; emacs desktop.el
    ".emacs.desktop.lock"                  ; emacs desktop.el
    ;; ".eunit"                               ; erlang
    ;; ".externalToolBuilders"                ; eclipse
    ;; ".flexProperties"                      ; actionscript
    ;; ".fseventsd"                           ; OS X
    ;; ".idlwave"                             ; emacs idlwave
    ".ido.last"                            ; emacs ido-mode
    ;; ".kkcrc"                               ; emacs kkc Kana Kanji converter
    ;; ".last_cover_stats"                    ; perl
    ".lein-deps-sum"                       ; leiningen
    ".loadpath"                            ; eclipse
    ".netrwhist"                           ; vim
    ;; ".notes"                               ; emacs org-mode / remember.el
    ;; ".org-id-locations"                    ; emacs org-mode
    ;; ".pc"                                  ; quilt
    ;; ".project"                             ; eclipse
    ;; ".projectile"                          ; emacs projectile
    ;; ".prove"                               ; perl
    ;; ".puppet-bak"                          ; puppet
    ".quickurls"                           ; emacs quickurl
    ".recentf"                             ; emacs recentf
    ;; ".redcar"                              ; redcar
    ".rspec"                               ; rails
    ".sass-cache"                          ; sass
    ;; ".scala_dependencies"                  ; scala
    ;; ".shadow_todo"                         ; emacs shadowfile
    ;; ".shadows"                             ; emacs shadowfile
    ;; ".strokes"                             ; emacs strokes.el
    ;; ".svn"                                 ; subversion
    ;; ".timelog"                             ; emacs timeclock
    ;; ".todo-do"                             ; emacs todo-mode
    ;; ".todo-done"                           ; emacs todo-mode
    ;; ".todo-top"                            ; emacs todo-mode
    ".tox"                                 ; python
    ;; ".type-break"                          ; emacs type-break
    ".vip"                                 ; emacs viper-mode
    ".viper"                               ; emacs viper-mode
    ".wmncach.el"                          ; emacs WoMan
    ;; ".yardoc"                              ; yard
    ;; "_MTN"                                 ; monotone
    ;; "__history"                            ; delphi
    ;; "_bdfcache.el"                         ; emacs ps-bdf
    ;; "_build"                               ; perl
    ;; "_cgo_defun.c"                         ; go
    ;; "_cgo_gotypes.go"                      ; go
    ;; "_darcs"                               ; darcs
    ;; "_obj"                                 ; go
    ;; "_sgbak"                               ; vault
    ;; "_site"                                ; jekyll
    ;; "_test"                                ; go
    ;; "_testmain.go"                         ; go
    ;; "_yardoc"                              ; yard
    "aclocal.m4"                           ; automake
    "auto-save-list"                       ; emacs
    "autom4te.cache"                       ; autoconf
    ;; "blib"                                 ; perl
    ;; "Build.bat"                            ; perl
    "COMMIT_EDITMSG"                       ; git
    "cmake_install.cmake"                  ; cmake
    "CMakeCache.txt"                       ; cmake
    "CMakeFiles"                           ; cmake
    ;; "cover_db"                             ; perl
    ;; "cscope.csd"                           ; cscope
    ;; "cscope.files"                         ; cscope
    ;; "cscope.inc"                           ; cscope
    ;; "cscope.lst"                           ; cscope
    ;; "cscope.out"                           ; cscope
    ;; "cscope.out.po"                        ; cscope
    ;; "cscope.tmplist"                       ; cscope
    ;; "CVS"                                  ; CVS
    ;; "Debug"                                ; various
    ;; "debug"                                ; various
    "depcomp"                              ; automake
    ;; "DerivedData"                          ; xcode
    ;; "Desktop.ini"                          ; ms-windows
    ;; "ehthumbs.db"                          ; ms-windows
    ;; "GHI_ISSUE"                            ; git ghi
    "git-rebase-todo"                      ; git
    ;; "gwt-unitCache"                        ; gwt
    ;; "gwt_bree"                             ; gwt
    "install-sh"                           ; automake
    ;; "install_manifest.txt"                 ; cmake
    ;; "InstalledFiles"                       ; ruby
    "Makefile.in"                          ; automake
    ;; "Makefile.old"                         ; perl
    ;; "MCVS"                                 ; meta-CVS
    ;; "META.yml"                             ; perl
    "MERGE_MSG"                            ; git
    ;; "minimal-session-saver-data.el"        ; emacs minimal-session-saver
    ;; "MYMETA.yml"                           ; perl
    ;; "nbbuild"                              ; netbeans
    ;; "nbdist"                               ; netbeans
    "nosetests.xml"                        ; python
    ;; "nytprof"                              ; perl
    ;; "nytprof.out"                          ; perl
    ;; "perltidy.ERR"                         ; perl
    ;; "pm_to_blib"                           ; perl
    ;; "Profile"                              ; various
    ;; "profile"                              ; various
    ;; "RCS"                                  ; RCS
    ;; "Release"                              ; various
    ;; "release"                              ; various
    "SCCS"                                 ; SCCS
    "Session.vim"                          ; vim
    ;; "slprj"                                ; matlab
    "SQUASH_MSG"                           ; git
    "TAGS"                                 ; ctags/etags
    "TAG_EDITMSG"                          ; git
    "tags"                                 ; ctags/etags
    ;; "TestResult"                           ; visualstudio
    ;; "testresult"                           ; visualstudio
    ;; "Thumbs.db"                            ; ms-windows
    ;; "tmtags"                               ; textmate
    "xcuserdata"                           ; xcode
    "xhtml-loader.rnc"                     ; emacs nxhtml
    ;; "{arch}"                               ; arch - todo is this correct?
    "~.dep"                                ; xcode
    "~.dot"                                ; xcode
    "~.nib"                                ; xcode
    "~.plst"                               ; xcode
    ;; "test.out"                             ; generic testing
    ;; "test_out"                             ; generic testing
    ;; "test.output"                          ; generic testing
    ;; "test_output"                          ; generic testing
    ".grunt"
    "yarn.lock"
    "Podfile.lock"
    ".lock-wscript"
    ))

;; List of file beginnings to ignore. These are not regular expressions, but
;; literal strings which occur at the beginnings of file or directory names
;; to ignore. The string to match comprises only the last element of a
;; fully-qualified pathname."
(defvar ignoramus/file-basename-beginnings
  '(
    ".#"                                ; emacs
    ;; "core."                             ; unix
    "._"                                ; thumbnails
    ;; "_cgo_export."                      ; go
    ))

;; List of file endings to ignore. These are not regular expressions, but
;; literal strings which occur at the ends of file names to ignore."
(defvar ignoramus/file-basename-endings
  '(
    ;; ".386"                      ; compiled binary
    ;; ".a"                        ; compiled binary
    ;; ".acn"                      ; latex
    ;; ".acr"                      ; latex
    ;; ".alg"                      ; latex
    ;; ".ap_"                      ; android
    ;; ".apk"                      ; android
    "_archive"                  ; emacs org-mode
    ;; ".asv"                      ; matlab
    "-autoloads.el"             ; emacs package.el
    ;; ".aux"                      ; latex
    ".bak"                      ; generic
    ;; ".bbl"                      ; bibtex
    ;; ".beam"                     ; erlang
    ".bin"                      ; compiled binary
    ;; ".blg"                      ; bibtex
    ;; ".cgo1.go"                  ; go
    ;; ".cgo2.c"                   ; go
    ;; ".chi"                      ; haskell
    ;; ".chi.h"                    ; haskell
    ".class"                    ; java compiled
    ".com"                      ; compiled binary
    ;; ".cp"                       ; texinfo
    ;; ".cps"                      ; texinfo
    ;; ".d64fsl"                   ; LISP
    ;; ".dcu"                      ; delphi
    ;; ".dep"                      ; make
    ;; ".dex"                      ; android
    ;; ".dfsl"                     ; LISP
    ;; ".dll"                      ; compiled binary
    ;; ".drc"                      ; delphi
    ;; ".drv"                      ; compiled binary
    ;; ".dvi"                      ; latex
    ;; ".dx32fsl"                  ; LISP
    ;; ".dx64fsl"                  ; LISP
    ;; ".dxl"                      ; LISP
    ;; ".dylib"                    ; compiled binary
    ;; ".ear"                      ; java
    ".elc"                      ; emacs
    ;; ".esproj"                   ; espresso
    ;; "-Ex.R"                     ; R
    ;; ".exe"                      ; compiled binary
    ;; ".fas"                      ; LISP
    ;; ".fasl"                     ; LISP
    ;; ".fdb_latexmk"              ; latex
    ;; ".fmx"                      ; oracle
    ;; ".fn"                       ; texinfo
    ;; ".fns"                      ; texinfo
    ;; ".fsl"                      ; LISP
    ;; ".fx32fsl"                  ; LISP
    ;; ".fx64fsl"                  ; LISP
    ;; ".gcda"                     ; gcov
    ;; ".gcno"                     ; gcov
    ;; ".gcov"                     ; gcov
    ;; ".glg"                      ; latex
    ;; ".glo"                      ; latex
    ;; ".gls"                      ; latex
    ;; ".gmo"                      ; gettext
    ;; ".hi"                       ; haskell
    ;; ".identcache"               ; delphi
    ;; ".ilg"                      ; latex
    ;; ".ilk"                      ; visualstudio
    ;; ".iml"                      ; intellij
    ;; ".ind"                      ; latex
    ;; ".ipr"                      ; intellij
    ;; ".ist"                      ; latex
    ;; ".iws"                      ; intellij
    ".jar"                      ; java
    ;; ".ky"                       ; texinfo
    ;; ".kys"                      ; texinfo
    ;; ".la"                       ; libtool
    ;; ".lai"                      ; libtool
    ;; ".launch"                   ; eclipse
    ;; ".lbin"                     ; compiled binary
    ;; ".lib"                      ; LISP
    ;; ".lnk"                      ; ms-windows
    ;; ".lo"                       ; libtool
    ".lock"                     ; generic
    ;; ".lof"                      ; latex
    ;; ".lot"                      ; latex
    ;; ".lx32fsl"                  ; LISP
    ;; ".lx64fsl"                  ; LISP
    ;; ".maf"                      ; latex
    ;; ".mem"                      ; LISP
    ".min.css"                  ; minified css
    "-min.css"                  ; minified css
    ".min.js"                   ; minified js
    "-min.js"                   ; minified js
    ;; ".mmx"                      ; oracle
    ;; ".mo"                       ; gettext
    ;; ".moved-aside"              ; xcode
    ;; ".mtc"                      ; latex
    ;; ".mtc0"                     ; latex
    ;; ".nav"                      ; latex
    ;; ".nlo"                      ; latex
    ".o"                        ; compiled binary
    ".obj"                      ; compiled binary
    ;; ".opensdf"                  ; visualstudio
    ;; ".orig"                     ; patch
    ;; ".p64fsl"                   ; LISP
    ;; ".pdfsync"                  ; latex
    ;; ".pfsl"                     ; LISP
    ;; ".pg"                       ; texinfo
    ;; ".pgs"                      ; texinfo
    ".pid"                      ; various
    ;; ".pidb"                     ; monodevelop
    ;; ".plt"                      ; erlang
    ;; ".plx"                      ; oracle
    ;; ".pot"                      ; django
    ;; ".psess"                    ; visualstudio
    ;; ".Publish.xml"              ; visualstudio
    ".pyc"                      ; python
    ".pyd"                      ; python
    ;; ".pydevproject"             ; eclipse
    ".pyo"                      ; python
    ".rbc"                      ; ruby
    ;; ".rej"                      ; patch
    ".sassc"                    ; sass
    ;; ".scc"                      ; visualstudio
    ;; ".sdf"                      ; visualstudio
    ".seed"                     ; node
    ;; ".sln.docstates"            ; visualstudio
    ;; ".slo"                      ; compiled binary
    ;; ".snm"                      ; latex
    ;; ".so"                       ; shared library
    ;; ".sparcf"                   ; LISP
    ;; ".sublime-project"          ; sublimetext
    ;; ".sublime-workspace"        ; sublimetext
    ;; ".suo"                      ; visualstudio
    ".swo"                      ; vim
    ".swp"                      ; vim
    ;; ".sx32fsl"                  ; LISP
    ;; ".sx64fsl"                  ; LISP
    ;; ".synctex.gz"               ; latex
    ;; ".ttc"                      ; template toolkit
    ;; ".tfm"                      ; latex
    ;; ".tmproj"                   ; textmate
    ;; ".tmproject"                ; textmate
    ;; ".toc"                      ; latex
    ;; ".tp"                       ; texinfo
    ;; ".tps"                      ; texinfo
    ;; ".ufsl"                     ; LISP
    ".un~"                      ; vim
    ;; ".vr"                       ; texinfo
    ;; ".vrb"                      ; latex
    ;; ".vrs"                      ; texinfo
    ;; ".vsp"                      ; visualstudio
    ;; ".vspscc"                   ; visualstudio
    ;; ".vssscc"                   ; visualstudio
    ;; ".vxd"                      ; ms-windows driver
    ".war"                      ; java
    ;; ".wx32fsl"                  ; LISP
    ;; ".wx64fsl"                  ; LISP
    ;; ".x86f"                     ; LISP
    ;; ".xdy"                      ; latex
    ;; ".zwc"                      ; zsh
    "~"                         ; emacs
    ;; ".fmt"                      ; latex
    ;; ".idx"                      ; latex
    ;; ".log"                      ; database
    ;; ".out"                      ; latex
    ".map"                      ; various
    ;; ".ln"                       ; ms-windows
    ".7z"
    ".ai"
    ".bmp"
    ".bz2"
    ".bzip"
    ".cab"
    ".deb"
    ".dmg"
    ".egg"
    ".gz"
    ".jpeg"
    ".jpg"
    ".lzma"
    ".mov"
    ;; ".msi"
    ;; ".msm"
    ;; ".msp"
    ".pdf"
    ".png"
    ".psd"
    ".rar"
    ".rpm"
    ".tar"
    ".tern-port"
    ".venv"
    ;; ".xpi"
    ".xz"
    ".zip"
    ))

;; ("~$" "^#.*#$")
(defvar ignoramus/file-basename-regexps
  '(
    "\\`\\.flycheck.*\\'"                    ; flycheck
    "\\`.*_flymake\\..*'"                    ; flymake
    "\\`#.*#\\'"                             ; emacs
    ;; "\\`.*\\.mex[^.]*\\'"                 ; matlab
    "\\`Icon.?\\'"                           ; OS X thumbnails
    "\\`\\..*\\.sw[a-z]\\'"                  ; vim
    "\\`\\.yas.*\\.el\\'"                    ; emacs yasnippet
    "\\`\\..*~undo-tree~\\'"                 ; emacs undo-tree
    ;; "\\`bzr_log\\.[[:alnum:]]+\\'"        ; emacs
    ;; "\\`hg-editor-[[:alnum:]]+\\.txt\\'"  ; emacs
    ;; "\\`svn-commit\\.tmp\\'"              ; emacs
    ;; "\\`zshecl[0-9]+\\'"                  ; zsh
    "\\`bash-fc-[0-9]+\\'"                   ; bash
    "\\.\\(BACKUP\\|LOCAL\\|BASE\\|REMOTE\\)\\.[0-9]\\{3,\\}" ; git
    )
  "List of regexps matching filenames to ignore.

The string to match comprises only the last element of a
fully-qualified pathname.")
