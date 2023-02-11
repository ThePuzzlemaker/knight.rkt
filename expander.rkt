#lang racket

(require racket/provide
         "parser.rkt")

(provide (matching-identifiers-out #rx"^kn" (all-defined-out)))

(provide do-setup!)

(define (read-one-line origin port)
  (define one-line (read-line port))
  (if (eof-object? one-line)
      eof
      (call-parser one-line origin)))

(define (do-setup!)
  (current-read-interaction read-one-line)
  (current-print kn-print))

(define-syntax (kn-module-begin stx)
  (syntax-case stx ()
    [(kn-module-begin expr)
     #'(#%module-begin
        (require knight/expander)
        (module configure-runtime racket
          (require knight/expander)
          (do-setup!))
        expr)]))
(provide (rename-out [kn-module-begin #%module-begin]))
(provide #%top-interaction #%app #%top #%datum syntax quote)

(define kn-namespace (make-hash))

(define (kn-var name)
  (hash-ref kn-namespace name))

(define (kn-set-var name value)
  (hash-set! kn-namespace name value))

(define-syntax-rule (kn-block expr)
  (lambda () expr))

(define kn-null 'kn-null)

(define (kn-null? val)
  (eq? val 'kn-null))

(define (kn-typeof val)
  (cond
    [(number? val) 'integer]
    [(string? val) 'string]
    [(boolean? val) 'boolean]
    [(procedure? val) 'block]
    [(kn-null? val) 'null]
    [(list? val) 'list]
    [else (error 'kn-typeof "invalid value: ~v" val)]))

(define (coerce->integer val)
  (case (kn-typeof val)
    ['integer val]
    ['string (or (string->number (list->string (takef (string->list (string-trim val))
                                                      (lambda (ch) (or (char-numeric? ch)
                                                                       (equal? ch #\+)
                                                                       (equal? ch #\-))))))
                 0)]
    ['boolean (if val 1 0)]
    ['null 0]
    ['list (length val)]
    [else (error 'coerce->integer "invalid type: ~a" (kn-typeof val))]))

(define (coerce->string val)
  (case (kn-typeof val)
    ['integer (number->string val)]
    ['string val]
    ['boolean (if val "true" "false")]
    ['null ""]
    ['list (string-join (map coerce->string val) "\n")]
    [else (error 'coerce->string "invalid type: ~a" (kn-typeof val))]))

(define (coerce->boolean val)
  (case (kn-typeof val)
    ['integer (not (= val 0))]
    ['string (non-empty-string? val)]
    ['boolean val]
    ['null #f]
    ['list (not (empty? val))]
    [else (error 'coerce->boolean "invalid type: ~a" (kn-typeof val))]))

(define (coerce->list val)
  (case (kn-typeof val)
    ['integer
     (let number->list ([num (abs val)]
                        [acc empty])
       (let ([mult (if (< val 0) -1 1)])
         (if (= 0 (quotient num 10))
             (cons (* mult (modulo num 10))
                   acc)
             (number->list (quotient num 10)
                           (cons (* mult (modulo num 10))
                                 acc)))))]
    ['string (map string (string->list val))]
    ['boolean (if val (list #t) empty)]
    ['null empty]
    ['list val]
    [else (error 'coerce->list "invalid type: ~a" (kn-typeof val))]))

(define (coerce->ref reference val)
  (case (kn-typeof reference)
    ['integer (coerce->integer val)]
    ['string (coerce->string val)]
    ['boolean (coerce->boolean val)]
    ['list (coerce->list val)]
    [else (error 'coerce->ref "invalid reference type: ~a" (kn-typeof reference))]))

(begin-for-syntax
  (require racket/base
           racket/list)
  (define (define-kn-func-binding first-name name kind)
    (list name (case (syntax->datum kind)
                 ['#:unevaluated name]
                 ['#:unchanged name]
                 ['#:integer #`(coerce->integer #,name)]
                 ['#:string #`(coerce->string #,name)]
                 ['#:boolean #`(coerce->boolean #,name)]
                 ['#:list #`(coerce->list #,name)]
                 ['#:coerced #`(coerce->ref #,first-name #,name)]
                 [else (error 'coerce-binding "invalid kind: ~a" kind)]))))

(define-syntax (define-kn-func stx)
  (syntax-case stx ()
    [(define-kn-func (name (arg kind) ...) body ...)
     (let* [[args (map syntax->list (syntax->list #'((arg kind) ...)))]
            [bindings (apply list
                             (map (lambda (p)                                      
                                    (define-kn-func-binding
                                      (first (first args))
                                      (first p)
                                      (second p)))
                                  args))]
            [bindings (datum->syntax stx bindings)]]
       #`(define (name arg ...)
           (let #,bindings
               body ...)))]))

(define (kn-prompt)
  (let [[line (read-line)]]
    (if (eq? line eof)
        'kn-null
        (string-trim line #px"(\r|\n)+" #:left? #f))))

(define (kn-random)
  (random 0 32768))

(define-syntax-rule (kn-call expr)
  (expr))

(define-kn-func (kn-quit (code #:integer))
  (exit code))

(define-kn-func (kn-output (str #:string))
  (if (string-suffix? str "\\")
      (printf "~a" (substring str
                              0
                              (sub1 (string-length str))))
      (printf "~a~n" str))
  'kn-null)

(define (kn-dump-inner val)
  (case (kn-typeof val)
    ['integer (number->string val)]
    ['boolean (if val "true" "false")]
    ['null "null"]
    ['string (string-append "\""
                            (string-replace
                             (string-replace
                              (string-replace
                               (string-replace
                                (string-replace
                                 val "\\" "\\\\")
                                "\"" "\\\"")
                               "\r" "\\r")
                              "\n" "\\n")
                             "\t" "\\t")
                            "\"")]
    ['list (string-append "["
                          (string-join (map kn-dump-inner val) ", ")
                          "]")]
    [else (error 'kn-dump "invalid type: ~a" (kn-typeof val))]))

(define-kn-func (kn-dump (val #:unchanged))
  (printf "~a" (kn-dump-inner val))
  val)

(define (kn-print datum [out (current-output-port)] [mode 0])
  (unless (void? datum)
    (display (kn-dump-inner datum) out)
    (display "\n")))

(define-kn-func (kn-length (lst #:list))
  (length lst))

(define-kn-func (kn-not (b #:boolean))
  (not b))

(define-kn-func (kn-negate (i #:integer))
  (- i))

(define-kn-func (kn-ascii (val #:unchanged))
  (case (kn-typeof val)
    ['integer (string (integer->char val))]
    ['string (char->integer (string-ref val 0))]
    [else (error 'kn-ascii "expected integer or string, found ~a" (kn-typeof val))]))

(define-kn-func (kn-box (val #:unchanged))
  (list val))

(define-kn-func (kn-head (val #:unchanged))
  (case (kn-typeof val)
    ['string (substring val 0 1)]
    ['list (first val)]
    [else (error 'kn-head "expected string or list, found ~a" (kn-typeof val))]))

(define-kn-func (kn-tail (val #:unchanged))
  (case (kn-typeof val)
    ['string (substring val 1)]
    ['list (rest val)]
    [else (error 'kn-tail "expected string or list, found ~a" (kn-typeof val))]))

; kn-value can be defined as an alias for kn-var
(define-syntax-rule (kn-value name)
  (kn-var name))

(define-kn-func (kn-use (path #:string))
  (eval (call-parser (open-input-file path) path)))

(define-kn-func (kn-eval (val #:string))
  (eval (call-parser val "evaled code")))

(define-kn-func (kn+ (e1 #:unchanged) (e2 #:coerced))
  (case (kn-typeof e1)
    ['integer (+ e1 e2)]
    ['string (string-append e1 e2)]
    ['list (append e1 e2)]
    [else (error 'kn+ "expected integer, string, or list, found ~a" (kn-typeof e1))]))

(define-kn-func (kn- (e1 #:unchanged) (e2 #:coerced))
  (case (kn-typeof e1)
    ['integer (- e1 e2)]
    [else (error 'kn- "expected integer, found ~a" (kn-typeof e1))]))

(define-kn-func (kn* (e1 #:unchanged) (e2 #:integer))
  (case (kn-typeof e1)
    ['integer (* e1 e2)]
    ['string (list->string (flatten (make-list e2 (string->list e1))))]
    ['list (flatten (make-list e2 e1))]
    [else (error 'kn* "expected integer, string, or list, found ~a" (kn-typeof e1))]))

(define-kn-func (kn/ (e1 #:unchanged) (e2 #:coerced))
  (case (kn-typeof e1)
    ['integer (quotient e1 e2)]
    [else (error 'kn/ "expected integer, found ~a" (kn-typeof e1))]))

(define-kn-func (kn% (e1 #:unchanged) (e2 #:coerced))
  (case (kn-typeof e1)
    ['integer (remainder e1 e2)]
    [else (error 'kn% "expected integer, found ~a" (kn-typeof e1))]))

(define-kn-func (kn^ (e1 #:unchanged) (e2 #:unchanged))
  (case (kn-typeof e1)
    ['integer (exact-truncate (expt e1 (coerce->integer e2)))]
    ['list (string-join (map coerce->string e1) (coerce->string e2))]
    [else (error 'kn^ "expected integer or list, found ~a" (kn-typeof e1))]))

(define (kn-list<? e1 e2)
  (if (or (eq? e1 null) (eq? e2 null))
      #f
      (if (kn<?-inner (car e1) (car e2))
          #t
          (kn-list<? (cdr e1) (cdr e2)))))

(define (kn-compare=? e1 e2)
  (if (or (eq? e1 null) (eq? e2 null))
      #t
      (if (equal? (car e1) (car e2))
          (kn-compare=? (cdr e1) (cdr e2))
          #f)))

(define (kn<?-inner e1 e2)
  (case (kn-typeof e1)
    ['integer (< e1 e2)]
    ['string (string<? e1 e2)]
    ['boolean (and (not e1) e2)]
    ['list (if (kn-compare=? e1 e2)
               (< (length e1) (length e2))
               (kn-list<? e1 e2))]
    [else (error 'kn<?-inner "invalid type: ~a" (kn-typeof e1))]))

(define-kn-func (kn<? (e1 #:unchanged) (e2 #:unchanged))
  (kn<?-inner e1 e2))

(define-kn-func (kn>? (e1 #:unchanged) (e2 #:unchanged))
  (kn<?-inner e2 e1))

(define-kn-func (kn=? (e1 #:unchanged) (e2 #:unchanged))
  (equal? e1 e2))

(define-kn-func (kn-and (e1 #:unchanged) (e2 #:unevaluated))
  (if (coerce->boolean e1) (eval-syntax e2) e1))

(define-kn-func (kn-or (e1 #:unchanged) (e2 #:unevaluated))
  (if (coerce->boolean e1) e1 (eval-syntax e2)))

(define-syntax-rule (kn-seq a b)
  (begin
    a
    b))

; this needs to be a macro so that (kn= (kn-var "hello") "world")
; doesn't evaluate (kn-var "hello") to whatever (potentially
; non-existant) value
(define-syntax (kn= stx)
  (syntax-case stx ()
    [(kn= (kn-var name) value)
     #'(kn-set-var name value)]
    [(kn= name value)
     #'(kn-set-var name value)]))

(define-kn-func (kn-while (condition #:unevaluated) (body #:unevaluated))
  (if (coerce->boolean (eval-syntax condition))
      (begin
        (eval-syntax body)
        (kn-while condition body))
      'kn-null))

(define-kn-func (kn-if (condition #:boolean) (iftrue #:unevaluated) (iffalse #:unevaluated))
  (eval-syntax (if condition iftrue iffalse)))

(define-kn-func (kn-get (base #:unchanged) (index #:integer) (len #:integer))
  (case (kn-typeof base)
    ['string (substring base index (+ index len))]
    ['list (take (drop base index) len)]
    [else (error 'kn-get "expected string or list, found ~a" (kn-typeof base))]))

(define-kn-func (kn-set (base #:unchanged) (index #:integer) (len #:integer) (replacee #:coerced))
  (case (kn-typeof base)
    ['string (string-append (substring base 0 index)
                            replacee
                            (substring base
                                       (+ index len)
                                       (string-length base)))]
    ['list (append (take base index)
                   replacee
                   (drop base (+ index len)))]
    [else (error 'kn-set "expected string or list, found ~a" (kn-typeof base))]))

(define fizzbuzz
  '(kn-seq (kn= (kn-var "fizzbuzz")
                (kn-block
                 (kn-seq (kn= (kn-var "n") 0)
                         (kn-while (kn<? (kn-var "n") (kn-var "max"))
                                   (kn-seq (kn= (kn-var "n") (kn+ (kn-var "n") 1))
                                           (kn-output (kn-if (kn-not (kn% (kn-var "n") 15))
                                                             "FizzBuzz"
                                                             (kn-if (kn-not (kn% (kn-var "n") 5))
                                                                    "Fizz" (kn-if (kn-not (kn% (kn-var "n") 3))
                                                                                  "Buzz"
                                                                                  (kn-var "n"))))))))))
           (kn-seq (kn= (kn-var "max") 100)
                   (kn-call (kn-var "fizzbuzz")))))
