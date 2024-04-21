;;; Code:

(require 'jtsx)
(require 'web-mode)

(defun ng2--re-opt (&rest strs)
  "Optimize, group, and place symbol-ends around a regex matching STRS."
  (concat "\\_<\\(?:" (regexp-opt strs) "\\)\\_>"))

(defun ng2--counterpart-name (file)
  "Return the file name of FILE's counterpart, or FILE if there is no counterpart."
  (when (not (ng2--is-component file)) file)
  (let ((ext (file-name-extension file))
        (base (file-name-sans-extension file)))
    (if (equal ext "ts")
        (concat base ".html")
      (concat base ".ts"))))

(defun ng2--is-component (file)
  "Return whether FILE is a component file."
  (equal (file-name-extension (file-name-sans-extension file)) "component"))

(defun ng2-open-counterpart ()
  "Opens the corresponding template or component file to this one."
  (interactive)
  (find-file (ng2--counterpart-name (buffer-file-name))))

(defconst ng2-ts-name-re
  (concat "\\_<[A-Za-z_$]\\(?:\\s_\\|\\sw\\)*\\_>"))

(defconst ng2-ts-type-name-re
  (concat "\\_<[A-Z_$]\\(?:\\s_\\|\\sw\\)*\\_>"))

(defconst ng2-ts-decorator-re
  (concat "\\(@" ng2-ts-name-re "\\)"))

(defconst ng2-ts-keyword-re
  (ng2--re-opt "is" "infer"))

(defconst ng2-ts-type-keyword-re
  (ng2--re-opt "void" "string" "number" "boolean" "object" "any" "unknown" "never"))

(defconst ng2-ts-interp-re
  "\\(${\\).*?\\(}\\)")

(defconst ng2-ts-type-annotated-re
  (concat "\\(" ng2-ts-name-re "\\)\\s-*[?!]?\\s-*:"))

(defconst ng2-ts-type-re
  (concat
   "\\(?:\\(?:" ng2-ts-type-keyword-re "\\)\\|"
   "\\(?:\\(?:" ng2-ts-type-name-re "\\|" ng2-ts-name-re "\\.\\)*"
   "\\(" ng2-ts-type-name-re "\\)" ; Type name
   "\\(?:\\[\\(" ng2-ts-type-name-re "\\)\\]\\)?\\)\\)")) ; Type subscript

(defconst ng2-ts-type-annotation-re
  (concat ":\\s-*" ng2-ts-type-re "\\s-*"))

(defconst ng2-ts-var-like-search-re
  (concat
   "\\(?:public\\|protected\\|private\\|readonly\\)\\s-+\\("
   "%s" ;; The name of the variable
   "\\)\\s-*[?!]?\\s-*\\(?:[=:].*\\|;?\\)$")) ; Ensure functions don't get picked up

(defconst ng2-ts-var-like-re
  (format ng2-ts-var-like-search-re ng2-ts-name-re))

(defconst ng2-ts-type-arithmetic-re
  (concat "[()]?\\s-*[|&]\\s-*[()]?\\s-*" ng2-ts-type-re "\\s-*"))

(defconst ng2-ts-typedef-re
  (concat "type\\s-\\(" ng2-ts-type-name-re "\\)\\s-*=\\s-*\\(" ng2-ts-type-re "\\)"))

(defconst ng2-ts-postfix-type-like-re
  (concat
   "\\(" ng2-ts-type-re "\\)\\s-+"
   "\\(?:" (ng2--re-opt "implements" "extends")
   "\\|\\(?:in\\|extends\\)\\s-+keyof"
   "\\)"))

(defconst ng2-ts-prefix-type-like-re
  (concat
   (ng2--re-opt "is" "as" "keyof" "instanceof" "infer" "extends" "implements" "class" "interface")
   "\\s-+\\(" ng2-ts-type-re "\\)"))

(defconst ng2-ts-import-default-type-re
  (concat "\\_<import\\s-+\\(" ng2-ts-type-name-re "\\)\\s-+\\(?:as\\s-+\\*\\s-+\\)?from\\_>"))

(defconst ng2-ts-type-condition-re
  (concat
   "extends\\s-+"
   ".+?"
   "\\s-*\\?\\s-*"
   ng2-ts-type-re
   "\\s-*:"))

(defconst ng2-ts-lambda-re
  (concat "=>"))

(defconst ng2-ts-generic-re
  (concat "<" ng2-ts-type-re ".*?>"))

;; -generic-re doesn't match this because it's contained in the .*?
(defconst ng2-ts-inner-generic-re
  (concat "<" ng2-ts-type-re ">"))

(defconst ng2-ts-method-re
  (concat "\\.\\(" ng2-ts-name-re "\\)("))

(defconst ng2-ts-fn-search-re
  (concat
   "\\(%s\\)" ; Function name
   "\\(?:<.*?>\\)?" ; Generic argument
   "([^)]*)\\s-*" ; Argument list
   "\\(?::\\s-*" ng2-ts-type-re "\\)?"; Return type
   "\\(?:<.*?>\\)?\\s-*{"))

(defconst ng2-ts-fn-re
  (format ng2-ts-fn-search-re ng2-ts-name-re))

(defun ng2-ts--inside-import-block-p (pos)
  "Whether POS is inside a Typescript import block."
  (save-match-data
    (and
     (save-excursion
       (goto-char pos)
       (search-backward "{" nil t)
       (forward-symbol -1)
       (looking-at-p "import"))
     (save-excursion
       (goto-char pos)
       (search-forward "}" nil t)
       (forward-symbol 1)
       (forward-symbol -1)
       (looking-at-p "from")))))

(defun ng2-ts--end-of-import (pos)
  "Return the position at the next end of an import statement after POS."
  (save-match-data
    (save-excursion
      (goto-char pos)
      (re-search-forward "}?\\s-*from" nil t)
      (end-of-line)
      (point))))

(defun ng2-ts--highlight-import-block-fn (bound)
  "Match a type inside an import block between point and BOUND."
  (or (when (ng2-ts--inside-import-block-p (point))
        (re-search-forward ng2-ts-type-name-re (min bound (ng2-ts--end-of-import (point))) 1))
      (and (save-match-data (search-forward "{" bound 1))
           (ng2-ts--highlight-import-block-fn bound))))

(defun ng2-ts--inside-lambda-args-p (pos)
  "Return whether POS is inside the arguments to an arrow function."
  (ignore-errors
    (save-match-data
      (<= (save-excursion
            (goto-char pos)
            (search-forward "=>" nil t)
            (backward-sexp)
            (point))
          pos
          (save-excursion
            (goto-char pos)
            (search-forward "=>" nil t)
            (backward-sexp)
            (forward-sexp)
            (1- (point)))))))

(defun ng2-ts--end-of-lambda-args (pos)
  "Return the first end of an arrow function's arguments after POS."
  (save-match-data
    (save-excursion
      (goto-char pos)
      (search-forward "=>" nil t)
      (point))))

(defun ng2-ts--skip-whitespace ()
  "Move POINT past all contiguous whitespace ahead of it."
  (save-match-data (while (looking-at "\\s-\\|\n") (forward-char))))

(defun ng2-ts--highlight-lambda-args-fn (bound)
  "Match a type inside an import block between point and BOUND."
  (or (when (ng2-ts--inside-lambda-args-p (point))
        (re-search-forward (concat "\\(" ng2-ts-name-re "\\)"
                                   "\\(?:\\s-*:\\s-*\\(" ng2-ts-name-re "\\)\\)?"
                                   "\\(?:\\s-*=\\s-*.*?\\(?:[,})]\\|\\]\\)\\)?")
                           (min bound (ng2-ts--end-of-lambda-args (point))) 1))
      (and (ignore-errors
             ;; Fix endless loop on generic return types
             (save-match-data (while (looking-at ">") (forward-char)))
             ;; Skip forward if we wind up in the space between the args and the =>
             (ng2-ts--skip-whitespace)
             (forward-char 2)
             (prog1 (save-match-data (search-forward "=>" bound 1))
               (ignore-errors (backward-sexp))))
           (ng2-ts--highlight-lambda-args-fn bound))))

(defun ng2-ts-goto-name (name)
  "Places the point on the variable or function called NAME."
  (goto-char (point-min))
  (unless (search-forward-regexp (format ng2-ts-fn-search-re name) nil t)
    (unless (search-forward-regexp (format ng2-ts-var-like-search-re name) nil t)
      (message "ng2-ts-mode: Couldn't find %s" name))))

(defvar ng2-ts-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-o") #'ng2-open-counterpart)
    map)
  "Keymap for ng2-ts-mode.")

(defvar ng2-ts-font-lock-keywords
  `((,ng2-ts-type-condition-re (1 font-lock-type-face nil t))
    (,ng2-ts-type-condition-re (2 font-lock-type-face nil t))
    (,ng2-ts-interp-re . (1 font-lock-variable-name-face t))
    (,ng2-ts-interp-re . (2 font-lock-variable-name-face t))
    (,ng2-ts-type-annotated-re (1 font-lock-variable-name-face))
    (,ng2-ts-type-annotation-re (1 font-lock-type-face nil t))
    (,ng2-ts-type-annotation-re (2 font-lock-type-face nil t))
    (,ng2-ts-type-annotation-re (2 font-lock-type-face nil t))
    (,ng2-ts-type-arithmetic-re (1 font-lock-type-face nil t))
    (,ng2-ts-type-arithmetic-re (2 font-lock-type-face nil t))
    (,ng2-ts-prefix-type-like-re (1 font-lock-type-face nil t))
    (,ng2-ts-prefix-type-like-re (2 font-lock-type-face nil t))
    (,ng2-ts-postfix-type-like-re (1 font-lock-type-face nil t))
    (,ng2-ts-postfix-type-like-re (2 font-lock-type-face nil t))
    (,ng2-ts-import-default-type-re (1 font-lock-type-face))
    (,ng2-ts-var-like-re (1 font-lock-variable-name-face))
    (,ng2-ts-method-re (1 font-lock-function-name-face))
    (,ng2-ts-fn-re (1 font-lock-function-name-face))
    (,ng2-ts-generic-re (1 font-lock-type-face nil t))
    (,ng2-ts-generic-re (2 font-lock-type-face nil t))
    (,ng2-ts-inner-generic-re (1 font-lock-type-face nil t))
    (,ng2-ts-inner-generic-re (2 font-lock-type-face nil t))
    (,ng2-ts-lambda-re (0 font-lock-function-name-face))
    (,ng2-ts-decorator-re (0 font-lock-builtin-face))
    (,ng2-ts-type-keyword-re (0 font-lock-type-face))
    (,ng2-ts-keyword-re (0 font-lock-keyword-face))
    (ng2-ts--highlight-import-block-fn (0 font-lock-type-face))
    (ng2-ts--highlight-lambda-args-fn (1 font-lock-variable-name-face))
    (ng2-ts--highlight-lambda-args-fn (2 font-lock-variable-name-face nil t))))

;;;###autoload
(define-derived-mode ng2-ts-mode
  jtsx-typescript-mode "ng2-ts"
  "Major mode for Angular 2 TypeScript"
  (modify-syntax-entry ?$ "_" ng2-ts-mode-syntax-table)
  (font-lock-add-keywords nil ng2-ts-font-lock-keywords))

;; (define-derived-mode ng2-js-mode
;;   jtsx-jsx-mode "ng2-js"
;;   "Major mode for Angular 2 JavaScript"
;;   (modify-syntax-entry ?$ "_" ng2-ts-mode-syntax-table)
;;   (font-lock-add-keywords nil ng2-ts-font-lock-keywords))

(defconst ng2-html-var-regex
  "\\(#\\)\\(\\w+\\)")

(defconst ng2-html-interp-regex
  (concat
   "\\({{\\)" ; Opening brackets
   "\\s-*[^{].+?" ; The expression being interpolated
   "\\(}}\\)")) ; Closing brackets

;; Colors pipes within ng2-html-interp-regex
;; TODO: Prevent false positives like "~x | 2"
(defconst ng2-html-pipe-regex
  (concat
   "|\\s-*" ; Pipe symbol
   "\\([A-Za-z0-9]+\\)")) ; Name of the pipe function

(defconst ng2-html-directive-regex
  "\\([*]\\)\\(.*?\\)[\"= ]")

(defconst ng2-html-binding-regex
  "\\(\\[(?\\)\\(.*?\\)\\()?\\]\\)=\\(\".*?\"\\)")

(defconst ng2-html-event-regex
  "\\((\\)\\(.*?\\)\\()\\)=\".*?\"")

(defun ng2-html-goto-binding ()
  "Opens the corresponding component TypeScript file, then places the cursor at the function corresponding to the binding."
  (interactive)
  (let ((name (symbol-at-point)))
    (ng2-open-counterpart)
    (ng2-ts-goto-name name)))

(defvar ng2-html-font-lock-keywords
  `((,ng2-html-var-regex (1 font-lock-builtin-face))
    (,ng2-html-var-regex (2 font-lock-variable-name-face))
    (,ng2-html-interp-regex . (1 font-lock-variable-name-face))
    (,ng2-html-interp-regex . (2 font-lock-variable-name-face))
    (,ng2-html-pipe-regex . (1 font-lock-function-name-face))
    (,ng2-html-directive-regex . (1 font-lock-builtin-face t))
    (,ng2-html-directive-regex . (2 font-lock-keyword-face t))
    (,ng2-html-binding-regex . (1 font-lock-builtin-face t))
    (,ng2-html-binding-regex . (2 font-lock-builtin-face t))
    (,ng2-html-binding-regex . (3 font-lock-builtin-face t))
    (,ng2-html-event-regex . (1 font-lock-builtin-face t))
    (,ng2-html-event-regex . (2 font-lock-builtin-face t))
    (,ng2-html-event-regex . (3 font-lock-builtin-face t))))

(defvar ng2-html-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-.") 'ng2-html-goto-binding)
    (define-key map (kbd "C-c C-o") 'ng2-open-counterpart)
    map)
  "Keymap for ng2-html-mode.")

;;;###autoload
(define-derived-mode ng2-html-mode
  web-mode "ng2-html"
  "Major mode for Angular 2 templates"
  (font-lock-add-keywords nil ng2-html-font-lock-keywords))

(defgroup ng2 nil
  "Major mode for AngularJS 2 files"
  :prefix "ng2-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/AdamNiederer/ng2-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ng2-mode"))

;;;###autoload
(defun ng2-mode ()
  "Activates the appropriate Angular 2-related mode for the buffer."
  (interactive)
  (if (equal buffer-file-name nil)
    (message "This doesn't appear to be an Angular2 component or service.")
    (let ((file-ext (file-name-extension (buffer-file-name))))
      (cond
        ((equal file-ext "html") (ng2-html-mode))
        ((equal file-ext "ts") (ng2-ts-mode))
        (t (message "This doesn't appear to be an Angular2 component or service."))))))

(provide 'ng2-mode)
