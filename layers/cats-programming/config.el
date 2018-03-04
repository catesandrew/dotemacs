;;; config.el --- cats: Programming

;;; Commentary:

;; Personal functions

;;; Code:


;; To augment and/or counteract these defaults your own function
;; to cats/prog-mode-hook, using:
;;
;; (add-hook 'cats/prog-mode-hook 'cats/prog-mode-defaults t)
;;
;; The final optional t sets the *append* argument

(defvar cats/prog-mode-hook nil
  "Hooks run when `prog-mode-hook' is fired.")

(defvar cats/line-numbers nil
  "If non nil line numbers are turned on in all `prog-mode' and `text-mode'.
derivatives.  If set to `relative', also turns on relative line numbers.")

(defvar cats/prog-mode-spell-checking t
  "Enable `prog-mode' spell checking.")

(defvar cats/ycmd-server-command
  '("/usr/local/bin/python2" "-u" "/usr/local/src/ycmd/ycmd"))

(defvar cats/prog-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    ;; dash "-" is now a word character in programming mode
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    table))

(defvar pretty-symbols/elisp
  '(("add-hook" . ?)
    ;("add-hook" . #XF13D)
    ;("add-hook" . )
    ("=>" . 8658))
  "Symbols for elisp mode.")

(defvar pretty-symbols/js2
  '(("not" . 172)
    ("!" . 172)
    ("forall" . 8704)
    ("::" . 8759)
    ;; ("." . 8728)
    ("~>" . 8669)
    ;; ("()" . #X2205)
    ("==" . #X225F)
    ("!=" . #X2260)
    ("===" . #X2261)
    ("!==" . #X2262)
    (">=" . #X2265)
    ("<=" . #X2264)
    ("!!" . #X203C)
    ("&&" . #X2227)
    ("||" . #X2228)
    ;; ("null" . 00D8)
    ("sqrt" . #X221A)
    ("undefined" . #X22A5)
    ("pi" . #X3C0)
    ("function" . 955)
    ("..." . #X2026)
    ("const" . #X2107)
    ;; ("let" . #X2112)
    ;; ("var" . #X2123)
    ("->" . 8594)
    ("-<" . 8610)
    ("<-" . 8592)
    ("=>" . 8658)
    ;; ("map" . 8614)
    ("return" . 8592))
  "Symbols for js2 mode.")

(defvar pretty-symbols/prog
  '(("!" . 172)
    ("::" . 8759)
    ("~>" . 8669)
    ("==" . #X225F)
    ("!=" . #X2260)
    ("===" . #X2261)
    ("!==" . #X2262)
    (">=" . #X2265)
    ("<=" . #X2264)
    ("!!" . #X203C)
    ("&&" . #X2227)
    ("||" . #X2228)
    ("->" . 8594)
    ("-<" . 8610)
    ("<-" . 8592)
    ("=>" . 8658))
  "Symbols for general prog mode.")

(defvar pretty-symbols/pragmatapro
  '(("[ERROR]"   #XE380)
    ("[DEBUG]"   #XE381)
    ("[INFO]"    #XE382)
    ("[WARN]"    #XE383)
    ("[WARNING]" #XE384)
    ("[ERR]"     #XE385)
    ("[FATAL]"   #XE386)
    ("[TRACE]"   #XE387)
    ("[FIXME]"   #XE388)
    ("[TODO]"    #XE389)
    ("[BUG]"     #XE38A)
    ("[NOTE]"    #XE38B)
    ("[HACK]"    #XE38C)
    ("[MARK]"    #XE38D)
    ("!!"        #XE900)
    ("!="        #XE901)
    ("!=="       #XE902)
    ("!!!"       #XE903)
    ("!≡"        #XE904)
    ("!≡≡"       #XE905)
    ("!>"        #XE906)
    ("!=<"       #XE907)
    ("#("        #XE920)
    ("#_"        #XE921)
    ("#{"        #XE922)
    ("#?"        #XE923)
    ("#>"        #XE924)
    ("##"        #XE925)
    ("#_("       #XE926)
    ("%="        #XE930)
    ("%>"        #XE931)
    ("%>%"       #XE932)
    ("%<%"       #XE933)
    ("&%"        #XE940)
    ("&&"        #XE941)
    ("&*"        #XE942)
    ("&+"        #XE943)
    ("&-"        #XE944)
    ("&/"        #XE945)
    ("&="        #XE946)
    ("&&&"       #XE947)
    ("&>"        #XE948)
    ("$>"        #XE955)
    ("***"       #XE960)
    ("*="        #XE961)
    ("*/"        #XE962)
    ("*>"        #XE963)
    ("++"        #XE970)
    ("+++"       #XE971)
    ("+="        #XE972)
    ("+>"        #XE973)
    ("++="       #XE974)
    ("--"        #XE980)
    ("-<"        #XE981)
    ("-<<"       #XE982)
    ("-="        #XE983)
    ("->"        #XE984)
    ("->>"       #XE985)
    ("---"       #XE986)
    ("-->"       #XE987)
    ("-+-"       #XE988)
    ("-\\/"      #XE989)
    ("-|>"       #XE98A)
    ("-<|"       #XE98B)
    (".."        #XE990)
    ("..."       #XE991)
    ("..<"       #XE992)
    (".>"        #XE993)
    (".~"        #XE994)
    (".="        #XE995)
    ("/*"        #XE9A0)
    ("//"        #XE9A1)
    ("/>"        #XE9A2)
    ("/="        #XE9A3)
    ("/=="       #XE9A4)
    ("///"       #XE9A5)
    ("/**"       #XE9A6)
    (":::"       #XE9AF)
    ;; ("::"        #XE9B0)
    (":="        #XE9B1)
    (":≡"        #XE9B2)
    (":>"        #XE9B3)
    (":=>"       #XE9B4)
    (":("        #XE9B5)
    (":-("       #XE9B6)
    (":)"        #XE9B7)
    (":-)"       #XE9B8)
    (":/"        #XE9B9)
    (":\\"       #XE9BA)
    (":3"        #XE9BB)
    (":D"        #XE9BC)
    (":P"        #XE9BD)
    (":>:"       #XE9BE)
    (":<:"       #XE9BF)
    ("<$>"       #XE9C0)
    ("<*"        #XE9C1)
    ("<*>"       #XE9C2)
    ("<+>"       #XE9C3)
    ("<-"        #XE9C4)
    ("<<"        #XE9C5)
    ("<<<"       #XE9C6)
    ("<<="       #XE9C7)
    ("<="        #XE9C8)
    ("<=>"       #XE9C9)
    ("<>"        #XE9CA)
    ("<|>"       #XE9CB)
    ("<<-"       #XE9CC)
    ("<|"        #XE9CD)
    ("<=<"       #XE9CE)
    ("<~"        #XE9CF)
    ("<~~"       #XE9D0)
    ("<<~"       #XE9D1)
    ("<$"        #XE9D2)
    ("<+"        #XE9D3)
    ("<!>"       #XE9D4)
    ("<@>"       #XE9D5)
    ("<#>"       #XE9D6)
    ("<%>"       #XE9D7)
    ("<^>"       #XE9D8)
    ("<&>"       #XE9D9)
    ("<?>"       #XE9DA)
    ("<.>"       #XE9DB)
    ("</>"       #XE9DC)
    ("<\\>"      #XE9DD)
    ("<\">"      #XE9DE)
    ("<:>"       #XE9DF)
    ("<~>"       #XE9E0)
    ("<**>"      #XE9E1)
    ("<<^"       #XE9E2)
    ("<!"        #XE9E3)
    ("<@"        #XE9E4)
    ("<#"        #XE9E5)
    ("<%"        #XE9E6)
    ("<^"        #XE9E7)
    ("<&"        #XE9E8)
    ("<?"        #XE9E9)
    ("<."        #XE9EA)
    ("</"        #XE9EB)
    ("<\\"       #XE9EC)
    ("<\""       #XE9ED)
    ("<:"        #XE9EE)
    ("<->"       #XE9EF)
    ("<!--"      #XE9F0)
    ("<--"       #XE9F1)
    ("<~<"       #XE9F2)
    ("<==>"      #XE9F3)
    ("<|-"       #XE9F4)
    ("<<|"       #XE9F5)
    ("==<"       #XEA00)
    ("=="        #XEA01)
    ("==="       #XEA02)
    ("==>"       #XEA03)
    ("=>"        #XEA04)
    ("=~"        #XEA05)
    ("=>>"       #XEA06)
    ("=/="       #XEA07)
    ("≡≡"        #XEA10)
    ("≡≡≡"       #XEA11)
    ("≡:≡"       #XEA12)
    (">-"        #XEA20)
    (">="        #XEA21)
    (">>"        #XEA22)
    (">>-"       #XEA23)
    (">=="       #XEA24)
    (">>>"       #XEA25)
    (">=>"       #XEA26)
    (">>^"       #XEA27)
    (">>|"       #XEA28)
    (">!="       #XEA29)
    ("??"        #XEA40)
    ("?~"        #XEA41)
    ("?="        #XEA42)
    ("?>"        #XEA43)
    ("???"       #XEA44)
    ("?."        #XEA45)
    ("^="        #XEA48)
    ("^."        #XEA49)
    ("^?"        #XEA4A)
    ("^.."       #XEA4B)
    ("^<<"       #XEA4C)
    ("^>>"       #XEA4D)
    ("^>"        #XEA4E)
    ("\\\\"      #XEA50)
    ("\\>"       #XEA51)
    ("\\/-"      #XEA52)
    ("@>"        #XEA57)
    ("|="        #XEA60)
    ("||"        #XEA61)
    ("|>"        #XEA62)
    ("|||"       #XEA63)
    ("|+|"       #XEA64)
    ("|->"       #XEA65)
    ("|-->"      #XEA66)
    ("|=>"       #XEA67)
    ("|==>"      #XEA68)
    ("|>-"       #XEA69)
    ("|<<"       #XEA6A)
    ("||>"       #XEA6B)
    ("|>>"       #XEA6C)
    ("~="        #XEA70)
    ("~>"        #XEA71)
    ("~~>"       #XEA72)
    ("~>>"       #XEA73)
    ("[["        #XEA80)
    ("]]"        #XEA81)
    ("\">"       #XEA90))
  "Symbols for PragmataPro.")

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vector (decode-char 'ucs (cadr s)))))
          pretty-symbols/pragmatapro))

(defconst pragmatapro-prettify-symbols-alist-width
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat
                       (make-list
                        (- (length (car s)) 1)
                        (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          pretty-symbols/pragmatapro))

;;; config.el ends here
