;;; packages.el --- cats-latex: Packages

;;; Commentary:

;; Personal LaTeX layer.

;;; Code:

(defconst cats-latex-packages
  '(
    auctex
    (auctex-skim :location local)
    bibtex
    typo
    ))

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

      (setq-default TeX-master nil        ; Ask for the master file
                    TeX-engine 'luatex    ; Use a modern engine
                    ;; Redundant in 11.88, but keep for older AUCTeX
                    TeX-PDF-mode t)
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
      (setq TeX-view-program-selection '((output-pdf "pdf-viewer")))
      (setq TeX-view-program-list '(("pdf-viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))))

  (use-package tex-buf
    :ensure auctex
    :defer t
    ;; Don't ask for confirmation when saving
    :config (setq TeX-save-query nil))

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

;;; packages.el ends here
