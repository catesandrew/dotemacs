;;; config.el --- cats-org

;;; Commentary:

;; Personal functions

;;; Code:

(defvar cats/org-mode-hook nil
  "Hooks run when `org-mode-hook' is fired.")

(defvar pretty-symbols/org
  '((">=" . ?≥)
    ("<=" . ?≤)
    ("\\geq" . ?≥)
    ("\\leq" . ?≤)
    ("\\neg" . ?¬)
    ("\\rightarrow" . ?→)
    ("\\leftarrow" . ?←)
    ("\\infty" . ?∞)
    ("-->" . ?→)
    ("<--" . ?←)
    ("\\exists" . ?∃)
    ("\\nexists" . ?∄)
    ("\\forall" . ?∀)
    ("\\or" . ?∨)
    ("\\and" . ?∧)
    (":)" . ?☺)
    ("):" . ?☹)
    (":D" . ?☺)
    ("\\checkmark" . ?✓)
    ("\\check" . ?✓)
    ("1/4" . ?¼)
    ("1/2" . ?½)
    ("3/4" . ?¾)
    ("1/7" . ?⅐)
    ("1/5" . ?⅕)
    ("2/5" . ?⅖)
    ("3/5" . ?⅗)
    ("4/5" . ?⅘)
    ("1/6" . ?⅙)
    ("1/6" . ?⅚)
    ("1/6" . ?⅛)
    ("1/6" . ?⅜)
    ("1/6" . ?⅝)
    ("1/6" . ?⅞)
    ("ae" . ?æ)
    ("^_^" . ?☻))
  "Symbols for org mode.")

(defvar org-enable-jira-support nil
  "If non-nil Jira related packages are configured.")

;;; config.el ends here
