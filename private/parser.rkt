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

(define-tokens basic-tokens (NUM STR VAR EXTFUNC))
(define-empty-tokens punct-tokens (EOF))
(define-empty-tokens func-tokens (T F N P R B C
                                    Q O D L A W
                                    I G S V U E @ : ! ~
                                    COMMA LBRACKET RBRACKET + - * / % ^ <
                                    > ? & PIPE SEMI = $))

(define knight-lexer
  (lexer-src-pos
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
                  [(N) 'null]
                  [(@) '(list)]
                  [(P) 'prompt]
                  [(R) 'random]
                  [(B expr) `(block . ,$2)]
                  [(C expr) `(call . ,$2)]
                  [(Q expr) `(quit . ,$2)]
                  [(D expr) `(dump . ,$2)]
                  [(O expr) `(output . ,$2)]
                  [(L expr) `(length . ,$2)]
                  [(! expr) `(! . ,$2)]
                  [(~ expr) `(~ . ,$2)]
                  [(A expr) `(ascii . ,$2)]
                  [(V expr) `(value . ,$2)]
                  [(U expr) `(use . ,$2)]
                  [(E expr) `(eval . ,$2)]
                  [(COMMA expr) `(box . ,$2)]
                  [(LBRACKET expr) `(head . ,$2)]
                  [(RBRACKET expr) `(tail . ,$2)]
                  [(+ expr expr) `(+ ,$2 . ,$3)]
                  [(- expr expr) `(- ,$2 . ,$3)]
                  [(* expr expr) `(* ,$2 . ,$3)]
                  [(/ expr expr) `(/ ,$2 . ,$3)]
                  [(% expr expr) `(% ,$2 . ,$3)]
                  [(^ expr expr) `(^ ,$2 . ,$3)]
                  [(< expr expr) `(< ,$2 . ,$3)]
                  [(> expr expr) `(> ,$2 . ,$3)]
                  [(? expr expr) `(? ,$2 . ,$3)]
                  [(& expr expr) `(& ,$2 . ,$3)]
                  [(PIPE expr expr) `(or ,$2 . ,$3)]
                  [(SEMI expr expr) `(seq ,$2 . ,$3)]
                  [(= expr expr) `(= ,$2 . ,$3)]
                  [(W expr expr) `(while ,$2 . ,$3)]
                  [($ expr expr) `($ ,$2 . ,$3)]
                  [(I expr expr expr) `(if ,$2 ,$3 . ,$4)]
                  [(G expr expr expr) `(get ,$2 ,$3 . ,$4)]
                  [(S expr expr expr expr) `(set ,$2 ,$3 ,$4 . ,$5)]
                  [(NUM) $1]
                  [(STR) $1]
                  [(VAR) `(var . ,$1)]]
            [expr-or-empty [() '()]
                           [(expr) $1]]]))

(define (call-parser port-or-str [name "input"])
  (if (port? port-or-str)
      ((knight-parser name) (lambda () (knight-lexer port-or-str)))
      (let [[ip (open-input-string port-or-str)]]
        ((knight-parser "input") (lambda () (knight-lexer ip))))))
