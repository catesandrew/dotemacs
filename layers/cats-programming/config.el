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

(defvar cats/prog-mode-spell-checking t
  "Enable `prog-mode' spell checking.")

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
  '(("not" 172)
    ;; ("!" 172)
    ("forall" 8704)
    ;; ("::" 8759)
    ;; ("." 8728)
    ;; ("()" #X2205)
    ;; ("==" #X225F)
    ;; ("!=" #X2260)
    ;; ("===" #X2261)
    ;; ("!==" #X2262)
    ;; (">=" #X2265)
    ;; ("<=" #X2264)
    ;; ("!!" #X203C)
    ;; ("&&" #X2227)
    ;; ("||" #X2228)
    ;; ("null" 00D8)
    ("sqrt" #X221A)
    ("undefined" #X22A5)
    ("pi" #X3C0)
    ("function" 955)
    ;; ("..." #X2026)
    ("const" #X2107)
    ;; ("let" #X2112)
    ;; ("var" #X2123)
    ;; ("->" 8594)
    ;; ("-<" 8610)
    ;; ("<-" 8592)
    ;; ("=>" 8658)
    ;; ("map" 8614)
    ("return" 8592))
  "Symbols for js2 mode.")

(defconst pretty-symbols/js2-width
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vector (decode-char 'ucs (cadr s)))))
          pretty-symbols/js2))

(defvar pretty-symbols/prog
  '(("!" . 172)
    ("::" . 8759)
    ;; ("~>" . 8669)
    ;; ("==" . #X225F)
    ;; ("!=" . #X2260)
    ;; ("===" . #X2261)
    ;; ("!==" . #X2262)
    ;; (">=" . #X2265)
    ;; ("<=" . #X2264)
    ;; ("!!" . #X203C) ned to map
    ;; ("&&" . #X2227)
    ;; ("||" . #X2016) need to map
    ;; ("->" . 8594)
    ;; ("-<" . 8610)
    ;; ("<-" . 8592)
    ;; ("=>" . 8658)
     )
  "Symbols for general prog mode.")

(defvar pretty-symbols/pragmatapro
  '(
     ;; ("[INFO ]"    #XE280)
     ;; ("[WARN ]"    #XE281)
     ;; ("[PASS ]"    #XE282)
     ;; ("[VERBOSE]"  #XE283)
     ;; ("[KO]"       #XE284)
     ;; ("[OK]"       #XE285)
     ;; ("[PASS]"     #XE286)
     ("[ERROR]"    #XE2C0)
     ("[DEBUG]"    #XE2C1)
     ("[INFO]"     #XE2C2)
     ("[WARN]"     #XE2C3)
     ("[WARNING]"  #XE2C4)
     ("[ERR]"      #XE2C5)
     ("[FATAL]"    #XE2C6)
     ("[TRACE]"    #XE2C7)
     ("[FIXME]"    #XE2C8)
     ("[TODO]"     #XE2C9)
     ("[BUG]"      #XE2CA)
     ("[NOTE]"     #XE2CB)
     ("[HACK]"     #XE2CC)
     ("[MARK]"     #XE2CD)
     ("[FAIL]"     #XE2CE)
     ("# ERROR"    #XE2F0)
     ("# DEBUG"    #XE2F1)
     ("# INFO"     #XE2F2)
     ("# WARN"     #XE2F3)
     ("# WARNING"  #XE2F4)
     ("# ERR"      #XE2F5)
     ("# FATAL"    #XE2F6)
     ("# TRACE"    #XE2F7)
     ("# FIXME"    #XE2F8)
     ("# TODO"     #XE2F9)
     ("# BUG"      #XE2FA)
     ("# NOTE"     #XE2FB)
     ("# HACK"     #XE2FC)
     ("# MARK"     #XE2FD)
     ("# FAIL"     #XE2FE)
     ("// ERROR"   #XE2E0)
     ("// DEBUG"   #XE2E1)
     ("// INFO"    #XE2E2)
     ("// WARN"    #XE2E3)
     ("// WARNING" #XE2E4)
     ("// ERR"     #XE2E5)
     ("// FATAL"   #XE2E6)
     ("// TRACE"   #XE2E7)
     ("// FIXME"   #XE2E8)
     ("// TODO"    #XE2E9)
     ("// BUG"     #XE2EA)
     ("// NOTE"    #XE2EB)
     ("// HACK"    #XE2EC)
     ("// MARK"    #XE2ED)
     ("// FAIL"    #XE2EE)
     ("!="         #XE900)
     ("!=="        #XE901)
     ("!=="        #XE902)
     ("!≡"         #XE903)
     ("!≡≡"        #XE904)
     ("#("         #XE90C)
     ("#_"         #XE90D)
     ("#{"         #XE90E)
     ("#?"         #XE90F)
     ("##"         #XE910)
     ("#_("        #XE911)
     ("#["         #XE912)
     ("%="         #XE920)
     ("&%"         #XE92C)
     ("&&"         #XE92D)
     ("&+"         #XE92E)
     ("&-"         #XE92F)
     ("&/"         #XE930)
     ("&="         #XE931)
     ("&&&"        #XE932)
     ("$>"         #XE93A)
     ("(|"         #XE940)
     ("*>"         #XE946)
     ("++"         #XE94C)
     ("+++"        #XE94D)
     ("+="         #XE94E)
     ("+>"         #XE94F)
     ("++="        #XE950)
     ("--"         #XE960)
     ("-<"         #XE961)
     ("-<<"        #XE962)
     ("-="         #XE963)
     ("->"         #XE964)
     ("->>"        #XE965)
     ("---"        #XE966)
     ("-->"        #XE967)
     ("-+-"        #XE968)
     ("-\\/"        #XE969)
     ("-|>"        #XE96A)
     ("-<|"        #XE96B)
     ("->-"        #XE96C)
     ("-<-"        #XE96D)
     ("-|"         #XE96E)
     ("-||"        #XE96F)
     ("-|:"        #XE970)
     (".="         #XE979)
     ("//="        #XE994)
     ("/="         #XE995)
     ("/=="        #XE996)
     ("/-\\"       #XE997)
     ("/-:"        #XE998)
     ("/->"        #XE999)
     ("/=>"        #XE99A)
     ("/-<"        #XE99B)
     ("/=<"        #XE99C)
     ("/=:"        #XE99D)
     (":="         #XE9AC)
     (":≡"         #XE9AD)
     (":=>"        #XE9AE)
     (":-\\"       #XE9AF)
     (":=\\"       #XE980)
     (":-/"        #XE981)
     (":=/"        #XE982)
     (":-|"        #XE983)
     (":=|"        #XE984)
     (":|-"        #XE985)
     (":|="        #XE986)
     ("<$>"        #XE9C0)
     ("<*"         #XE9C1)
     ("<*>"        #XE9C2)
     ("<+>"        #XE9C3)
     ("<-"         #XE9C4)
     ("<<="        #XE9C5)
     ("<=>"        #XE9C7)
     ("<>"         #XE9C8)
     ("<|>"        #XE9C9)
     ("<<-"        #XE9CA)
     ("<|"         #XE9CB)
     ("<=<"        #XE9CC)
     ("<~"         #XE9CD)
     ("<~~"        #XE9CE)
     ("<<~"        #XE9CF)
     ("<$"         #XE9D0)
     ("<+"         #XE9D1)
     ("<!>"        #XE9D2)
     ("<@>"        #XE9D3)
     ("<#>"        #XE9D4)
     ("<%>"        #XE9D5)
     ("<^>"        #XE9D6)
     ("<&>"        #XE9D7)
     ("<?>"        #XE9D8)
     ("<.>"        #XE9D9)
     ("</>"        #XE9DA)
     ("<\\>"       #XE9DB)
     ("<\">"       #XE9DC)
     ("<:>"        #XE9DD)
     ("<~>"        #XE9DE)
     ("<**>"       #XE9DF)
     ("<<^"        #XE9E0)
     ("<="         #XE9E1)
     ("<->"        #XE9E2)
     ("<!--"       #XE9E3)
     ("<--"        #XE9E4)
     ("<~<"        #XE9E5)
     ("<==>"       #XE9E6)
     ("<|-"        #XE9E7)
     ("<||"        #XE9E8)
     ("<<|"        #XE9E9)
     ("<-<"        #XE9EA)
     ("<-->"       #XE9EB)
     ("<<=="       #XE9EC)
     ("<=="        #XE9ED)
     ("<-\\"        #XE9EE)
     ("<-/"        #XE9EF)
     ("<=\\"        #XE9F0)
     ("<=/"        #XE9F1)
     ("=<<"        #XEA00)
     ("=="         #XEA01)
     ("==="        #XEA02)
     ("==>"        #XEA03)
     ("=>"         #XEA04)
     ("=~"         #XEA05)
     ("=>>"        #XEA06)
     ("=~="        #XEA07)
     ("==>>"       #XEA08)
     ("=>="        #XEA09)
     ("=<="        #XEA0A)
     ("=<"         #XEA0B)
     ("==<"        #XEA0C)
     ("=<|"        #XEA0D)
     ("=/="        #XEA0F)
     ("=/<"        #XEA10)
     ("=|"         #XEA11)
     ("=||"        #XEA12)
     ("=|:"        #XEA13)
     (">-"         #XEA20)
     (">>-"        #XEA22)
     (">>="        #XEA23)
     (">=>"        #XEA24)
     (">>^"        #XEA25)
     (">>|"        #XEA26)
     (">!="        #XEA27)
     (">->"        #XEA28)
     (">=="        #XEA29)
     (">="         #XEA2A)
     (">/="        #XEA2B)
     (">-|"        #XEA2C)
     (">=|"        #XEA2D)
     (">-\\"        #XEA2E)
     (">=\\"        #XEA2F)
     (">-/"        #XEA30)
     (">=/"        #XEA31)
     (">λ="        #XEA32)
     ("?."         #XEA3F)
     ("^="         #XEA43)
     ("^<<"        #XEA48)
     ("^>>"        #XEA49)
     ("\\="         #XEA54)
     ("\\=="        #XEA55)
     ("\\/="        #XEA56)
     ("\\-/"        #XEA57)
     ("\\-:"        #XEA58)
     ("\\->"        #XEA59)
     ("\\=>"        #XEA5A)
     ("\\-<"        #XEA5B)
     ("\\=<"        #XEA5C)
     ("\\=:"        #XEA5D)
     ("|="         #XEA69)
     ("|>="        #XEA6A)
     ("|>"         #XEA6B)
     ("|+|"        #XEA6C)
     ("|->"        #XEA6D)
     ("|-->"       #XEA6E)
     ("|=>"        #XEA6F)
     ("|==>"       #XEA70)
     ("|>-"        #XEA71)
     ("|<<"        #XEA72)
     ("||>"        #XEA73)
     ("|>>"        #XEA74)
     ("|-"         #XEA75)
     ("||-"        #XEA76)
     ("||="        #XEA77)
     ("|)"         #XEA78)
     ("|]"         #XEA79)
     ("|-:"        #XEA7A)
     ("|=:"        #XEA7B)
     ("|-<"        #XEA7C)
     ("|=<"        #XEA7D)
     ("|--<"       #XEA7E)
     ("|==<"       #XEA7F)
     ("~="         #XEA8A)
     ("~>"         #XEA8B)
     ("~~>"        #XEA8C)
     ("~>>"        #XEA8D)
     ("[["         #XEA8F)
     ("[|"         #XEA90)
     ("_|_"        #XEA97)
     ("]]"         #XEAA0)
     ("≡≡"         #XEAB3)
     ("≡≡≡"        #XEAB4)
     ("≡:≡"        #XEAB5)
     ("≡/"         #XEAB6)
     ("≡/≡"        #XEAB7))
  "Symbols for PragmataPro.")

(defconst pragmatapro-prettify-symbols-alist
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
