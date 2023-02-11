#lang racket

(require racket/match
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)
(provide call-parser knight-parser knight-lexer)

(define-lex-abbrevs
  (lower (:/ #\a #\z))
  (upper (:/ #\A #\Z))
  (digit (:/ #\0 #\9)))

(define-tokens basic-tokens (NUM STR VAR EXTFUNC EXTRKT))
(define-empty-tokens punct-tokens (EOF))
(define-empty-tokens func-tokens (T F N P R B C
                                    Q O D L A W
                                    I G S V U E @ : ! ~
                                    COMMA LBRACKET RBRACKET + - * / % ^ <
                                    > ? & PIPE SEMI = $))

(define knight-lexer
  (lexer-src-pos
   [(:: "#;racket " (:* (:~ #\;)) #\;)
    (token-EXTRKT
     (substring lexeme
                (string-length "#;racket ")
                (sub1 (string-length lexeme))))]
   [(:: #\# (:* (:~ #\newline)) (:? #\newline))
    (return-without-pos (knight-lexer input-port))]
   [(:or whitespace
         #\(
         #\)
         #\:)
    (return-without-pos (knight-lexer input-port))]
   [(eof) (token-EOF)]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: #\" (:* (:~ #\")) #\")
    (token-STR (substring lexeme
                          1
                          (- (string-length lexeme) 1)))]
   [(:: #\' (:* (:~ #\')) #\')
    (token-STR (substring lexeme
                          1
                          (- (string-length lexeme) 1)))]
   [(:: (:or lower #\_) (:* (:or lower #\_ digit)))
    (token-VAR lexeme)]
   [(:: (:or #\T #\F #\N #\P
             #\R #\B #\C #\Q
             #\D #\O #\L #\A
             #\W #\I #\G #\S
             #\V #\U #\E) (:* (:or upper #\_)))
    (string->symbol (substring lexeme 0 1))]
   [(:: #\X (:* (:or upper #\_)))
    (token-EXTFUNC lexeme)]
   [#\@ (token-@)]
   [#\: (token-:)]
   [#\! (token-!)]
   [#\~ (token-~)]
   [#\, (token-COMMA)]
   [#\[ (token-LBRACKET)]
   [#\] (token-RBRACKET)]
   [#\+ (token-+)]
   [#\- (token--)]
   [#\* (token-*)]
   [#\/ (token-/)]
   [#\% (token-%)]
   [#\^ (token-^)]
   [#\< (token-<)]
   [#\> (token->)]
   [#\? (token-?)]
   [#\& (token-&)]
   [#\| (token-PIPE)]
   [#\; (token-SEMI)]
   [#\= (token-=)]
   [#\$ (token-$)]))


(define (knight-parser source-name)
  (parser
   [start expr-or-empty]
   [end EOF]
   [src-pos]
   [error (lambda (a name val start end)
              (raise-read-error 
               "A syntax error was encountered"
               source-name
               (position-line start)
               (position-col start)
               (position-offset start)
               (- (position-offset end)
                  (position-offset start))))]
   [tokens basic-tokens
           punct-tokens
           func-tokens]
   [grammar [expr [(T) #t]
                  [(F) #f]
                  [(N) 'kn-null]
                  [(@) ''()]
                  [(P) '(kn-prompt)]
                  [(R) '(kn-random)]
                  [(B expr) `(kn-block ,$2)]
                  [(C expr) `(kn-call ,$2)]
                  [(Q expr) `(kn-quit ,$2)]
                  [(D expr) `(kn-dump ,$2)]
                  [(O expr) `(kn-output ,$2)]
                  [(L expr) `(kn-length ,$2)]
                  [(! expr) `(kn-not ,$2)]
                  [(~ expr) `(kn-negate ,$2)]
                  [(A expr) `(kn-ascii ,$2)]
                  [(V expr) `(kn-value ,$2)]
                  [(U expr) `(kn-use ,$2)]
                  [(E expr) `(kn-eval ,$2)]
                  [(COMMA expr) `(kn-box ,$2)]
                  [(LBRACKET expr) `(kn-head ,$2)]
                  [(RBRACKET expr) `(kn-tail ,$2)]
                  [(+ expr expr) `(kn+ ,$2 ,$3)]
                  [(- expr expr) `(kn- ,$2 ,$3)]
                  [(* expr expr) `(kn* ,$2 ,$3)]
                  [(/ expr expr) `(kn/ ,$2 ,$3)]
                  [(% expr expr) `(kn% ,$2 ,$3)]
                  [(^ expr expr) `(kn^ ,$2 ,$3)]
                  [(< expr expr) `(kn<? ,$2 ,$3)]
                  [(> expr expr) `(kn>? ,$2 ,$3)]
                  [(? expr expr) `(kn=? ,$2 ,$3)]
                  [(& expr expr) `(kn-and ,$2 #',$3)]
                  [(PIPE expr expr) `(kn-or ,$2 #',$3)]
                  [(SEMI expr expr) `(kn-seq ,$2 ,$3)]
                  [(= expr expr) `(kn= ,$2 ,$3)]
                  [(W expr expr) `(kn-while #',$2 #',$3)]
                  [($ expr expr) `(kn-shell ,$2 ,$3)]
                  [(I expr expr expr) `(kn-if ,$2 #',$3 #',$4)]
                  [(G expr expr expr) `(kn-get ,$2 ,$3 ,$4)]
                  [(S expr expr expr expr) `(kn-set ,$2 ,$3 ,$4 ,$5)]
                  [(NUM) $1]
                  [(STR) $1]
                  [(VAR) `(kn-var ,$1)]
                  [(EXTRKT) (syntax->datum (read-syntax "evaled racket code" (open-input-string $1)))]]
            [expr-or-empty [() 'kn-null]
                           [(expr) $1]]]))

(define (call-parser port-or-str [name "input"])
  (if (port? port-or-str)
      ((knight-parser name) (lambda () (knight-lexer port-or-str)))
      (let [[ip (open-input-string port-or-str)]]
        ((knight-parser "input") (lambda () (knight-lexer ip))))))
