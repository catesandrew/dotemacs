;;; packages.el --- cats-latex: Packages

;;; Commentary:

;; Personal LaTeX layer.

;;; Code:

(defconst cats-latex-packages
  '(
     auctex
     (auctex-skim :location local)
     bibtex
     (latex-pretty-symbols :location local)
     org
     typo
     ))


;; org
(defun cats-latex/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(latex . t))))

(defun cats-latex/pre-init-auctex ()
  (spacemacs|use-package-add-hook tex
    :post-init
    (progn
      (setq TeX-build-directory "build/"
            TeX-electric-sub-and-superscript t
            TeX-electric-math '("\\(" "\\)")
            ;; Don't insert magic quotes right away.
            TeX-quote-after-quote t
            ;; Don't ask for confirmation when cleaning
            TeX-clean-confirm nil
            ;; Provide forward and inverse search with Sync TeX
            TeX-source-correlate-mode t
            ;; Setup reftex style (RefTeX is supported through extension)
            TeX-source-correlate-method 'synctex)

      (setq-default
        ;; Ask for the master file
        TeX-master nil
        ;; Use a modern engine
        TeX-engine 'xetex)

      (add-hook 'LaTeX-mode-hook
        '(lambda ()
           (setq TeX-view-program-selection
             '((output-dvi "skim-viewer")
                (output-pdf "skim-viewer")
                (output-html "skim-viewer"))
             )
           (setq TeX-command-default "latexmk-osx")
           ))
      )
    :post-config
    (progn
      ;; Sample `latexmkrc` for OSX that copies the *.pdf file from the `./build` directory
      ;; to the working directory:
      ;;    $pdflatex = 'pdflatex -file-line-error -synctex=1 %O %S && (cp "%D" "%R.pdf")';
      ;;    $pdf_mode = 1;
      ;;    $out_dir = './build';

      ;; Skim's displayline is used for forward search (from .tex to .pdf)
      ;; option -b highlights the current line
      ;; option -g opens Skim in the background
      ;; option -o open Skim in the foreground with full application focus.

      ;; Skim -- turn on auto-refresh by typing the following into the terminal:
      ;; defaults write -app Skim SKAutoReloadFileUpdate -boolean true
      (add-to-list 'TeX-expand-list
        '("%(tex-file-name)"
           (lambda ()
             (concat "\"" (buffer-file-name) "\""))))

      (add-to-list 'TeX-expand-list
        '("%(pdf-file-name)"
           (lambda () (concat "\"" (cats/pdf-file-name "pdf") "\""))))

      (add-to-list 'TeX-expand-list
        '("%(line-number)" (lambda () (format "%d" (line-number-at-pos)))))

      (add-to-list 'TeX-expand-list
        '("%(latexmkrc-osx)"
           (lambda ()
             (let ((shell-home (getenv "HOME")))
               (concat shell-home "/.latexmkrc")))))

      ;; make latexmk available via C-c C-c
      ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
      (add-to-list 'TeX-command-list
        '("latexmk-osx" "latexmk -r %(latexmkrc-osx) %t" TeX-run-TeX nil t))

      (add-to-list 'TeX-expand-list
        '("%(displayline)" (lambda () "/Applications/Skim.app/Contents/SharedSupport/displayline")))

      ;; (add-to-list 'TeX-command-list
      ;;   '("Skim" "%(displayline) -g %(line-number) %(pdf-file-name) %(tex-file-name)" TeX-run-TeX nil t))

      (add-to-list 'TeX-view-program-list
        '("skim-viewer" "%(displayline) -b -r -g %(line-number) %(pdf-file-name) %(tex-file-name)"))
      ))

  (use-package tex-buf
    :ensure auctex
    :defer t
    ;; Don't ask for confirmation when saving
    :config
    (progn
      (setq TeX-save-query nil)
      (defun TeX-view ()
        "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
        (interactive)
        (let ((output-file (cats/pdf-file-name "pdf")))
          (if (file-exists-p output-file)
	          (TeX-command "View" 'cats/pdf-file-name 0)
            (message "Output file %S does not exist." output-file))))))

  (use-package tex-style
    :ensure auctex
    :defer t
    :config
    ;; Enable support for csquotes
    (setq LaTeX-csquotes-close-quote "}"
          LaTeX-csquotes-open-quote "\\enquote{"))

  (use-package tex-mode
    :ensure auctex
    :defer t
    :config
    (font-lock-add-keywords
     'latex-mode
     `((,(rx "\\"
             symbol-start
             "fx" (1+ (or (syntax word) (syntax symbol)))
             symbol-end)
        . font-lock-warning-face))))

  (use-package latex
    :ensure auctex
    :defer t
    :config
    (setq TeX-outline-extra
          `((,(rx (0+ space) "\\section*{") 2)
            (,(rx (0+ space) "\\subsection*{") 3)
            (,(rx (0+ space) "\\subsubsection*{") 4)
            (,(rx (0+ space) "\\minisec{") 5))
          ;; No language-specific hyphens please
          LaTeX-babel-hyphen nil))
  )

(defun cats-latex/init-auctex-skim ()
  (use-package auctex-skim
    :if (spacemacs/system-is-mac)
    :commands (auctex-skim-select)
    :init (with-eval-after-load 'tex
            (auctex-skim-select))))

(defun cats-latex/init-bibtex ()
  (use-package bibtex
    :defer t
    :config
    (progn
      ;; Run prog mode hooks for bibtex
      (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

      ;; Use a modern BibTeX dialect
      (bibtex-set-dialect 'biblatex))))

(defun cats-latex/post-init-typo ()
  ;; Keep typo mode enabled in LaTeX
  (remove-hook 'LaTeX-mode-hook 'spacemacs//disable-typo-mode))


;; latex-pretty-symbols
(defun cats-latex/init-latex-pretty-symbols ()
  (use-package latex-pretty-symbols
    :commands (latex-unicode-simplified)))

;;; packages.el ends here
