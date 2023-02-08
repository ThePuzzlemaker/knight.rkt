#lang racket

(require racket/match
         racket/string
         "parser.rkt")

(provide kn-eval kn-eval-step call-parser)

(define (kn-null? datum)
  (eq? datum 'null))

(define (kn-list? datum)
  (and (pair? datum)
       (eq? (car datum) 'list)))

(define (coerce->integer datum)
  (cond
    [(number? datum) datum]
    [(string? datum)
     (or (string->number (list->string (takef (string->list (string-trim datum))
                                              (lambda (ch) (or (char-numeric? ch)
                                                               (equal? ch #\+)
                                                               (equal? ch #\-))))))
         0)]
    [(boolean? datum) (if datum 1 0)]
    [(kn-null? datum) 0]
    [(kn-list? datum) (length (cdr datum))]
    [else (error "coerce->integer: invalid datum:" datum)]))

(define (coerce->string datum)
  (cond
    [(number? datum) (number->string datum)]
    [(string? datum) datum]
    [(boolean? datum) (if datum "true" "false")]
    [(kn-null? datum) ""]
    [(kn-list? datum) (string-join (map coerce->string (cdr datum)) "\n")]
    [else (error "coerce->string: invalid datum:" datum)]))

(define (coerce->boolean datum)
  (cond
    [(number? datum) (not (= datum 0))]
    [(string? datum) (non-empty-string? datum)]
    [(boolean? datum) datum]
    [(kn-null? datum) #f]
    [(kn-list? datum) (not (empty? (cdr datum)))]
    [else (error "coerce->boolean: invalid datum:" datum)]))

(define (coerce->list datum)
  (cond
    [(number? datum)
     (let number->list ([num (abs datum)]
                        [acc '()])
       (let ([mult (if (< datum 0) -1 1)])
         (if (= 0 (quotient num 10))
             (cons 'list
                   (cons (* mult (modulo num 10))
                         acc))
             (number->list (quotient num 10)
                           (cons (* mult (modulo num 10))
                                 acc)))))]
    [(string? datum)
     (cons 'list (map string (string->list datum)))]
    [(boolean? datum) (if datum '(list #t) '(list))]
    [(kn-null? datum) '(list)]
    [(kn-list? datum) datum]
    [else (error "coerce->list: invalid datum:" datum)]))

(define (coerce reference current)
  (cond
    [(number? reference) (coerce->integer current)]
    [(string? reference) (coerce->string current)]
    [(boolean? reference) (coerce->boolean current)]
    [(kn-list? reference) (coerce->list current)]
    [else (error "coerce: invalid reference datum:" reference)]))

(define (kn-block? datum)
  (and (pair? datum)
       (eq? (car datum) 'block)))

(define (kn-var? datum)
  (and (pair? datum)
       (eq? (car datum) 'var)))

(define (kn-needs-eval? datum)
  (not (or (number? datum)
           (string? datum)
           (boolean? datum)
           (kn-null? datum)
           (kn-list? datum)
           (kn-block? datum))))

(define (kn-func? datum func)
  (and (pair? datum)
       (eq? (car datum) func)))

(define (assert-block datum)
  (unless (kn-block? datum)
    (error "expected a block, found:" datum)))

;; (define (pair-ref pair nth)
;;   (if (= nth 0)
;;       (if (and (pair? pair) (pair? (car pair)))
;;           (car pair)
;;           pair)
;;       (pair-ref (cdr pair) (- nth 1))))

;; (define (pair-length pair [acc 0])
;;   (if (pair? pair)
;;       (pair-length (cdr pair) (+ acc 1))
;;       (+ 1 acc)))

(define (func-ref pair nth len)
  (match (cons len nth)
    [(cons 2 0) (car pair)]
    [(cons 2 1) (cdr pair)]
    [(cons 3 0) (car pair)]
    [(cons 3 1) (cadr pair)]
    [(cons 3 2) (cddr pair)]
    [(cons 4 0) (car pair)]
    [(cons 4 1) (cadr pair)]
    [(cons 4 2) (caddr pair)]
    [(cons 4 3) (cdddr pair)]
    [(cons 5 0) (car pair)]
    [(cons 5 1) (cadr pair)]
    [(cons 5 2) (caddr pair)]
    [(cons 5 3) (cadddr pair)]
    [(cons 5 4) (cddddr pair)]
    [(cons 6 0) (car pair)]
    [(cons 6 1) (cadr pair)]
    [(cons 6 2) (caddr pair)]
    [(cons 6 3) (cadddr pair)]
    [(cons 6 4) (car (cddddr pair))]
    [(cons 6 5) (cdr (cddddr pair))]))

(begin-for-syntax  
  (define (main-binding ctx datum name idx kind arity)
    (if (eq? kind 'unevaluated)
        `[,name (func-ref ,datum ,(+ 1 idx) ,arity)]
        `[,name (kn-eval (func-ref ,datum ,(+ 1 idx) ,arity) ,ctx)]))

  (define (coerce-binding first-name name kind)
    (list name (case kind
                 ['unevaluated name]
                 ['unchanged name]
                 ['integer `(coerce->integer ,name)]
                 ['string `(coerce->string ,name)]
                 ['boolean `(coerce->boolean ,name)]
                 ['list `(coerce->list ,name)]
                 ['block `(begin (assert-block ,name) ,name)]
                 ['coerced `(coerce ,first-name ,name)]
                 [else (error "coerce-binding: invalid kind:" kind)])))

  (define (enumerate lst [acc '()] [idx 0])
    (if (eq? null lst)
        acc
        (enumerate (cdr lst)
                   (append acc (list (cons idx
                                           (car lst))))
                   (+ idx 1)))))

(define-syntax (kn-eval-case stx)
  (syntax-case stx ()
    [(kn-eval-case [[expr kind] ...] [datum ctx] body ...)
     (let* [[args (eval-syntax #'`((expr . ,kind) ...))]
            [ctx (syntax->datum #'ctx)]
            [datum (syntax->datum #'datum)]
            [main-bindings (apply list
                                  (map (lambda (p)
                                         (main-binding
                                          ctx
                                          datum
                                          (cadr p)
                                          (car p)
                                          (cddr p)
                                          (+ 1 (length args))))
                                       (enumerate args)))]
            [coerce-bindings (apply list
                                    (map (lambda (p)
                                           (coerce-binding (car (car args)) (car p) (cdr p))) args))]]
       (with-syntax [[let-bindings (datum->syntax stx (append main-bindings coerce-bindings))]]
         #'(let* let-bindings body ...)))]))

(define (kn-dump datum)
  (cond
    [(number? datum) (number->string datum)]
    [(boolean? datum) (if datum "true" "false")]
    [(kn-null? datum) "null"]
    [(string? datum)
     (string-append "\""
                    (string-replace
                     (string-replace
                      (string-replace
                       (string-replace
                        (string-replace
                         datum "\\" "\\\\")
                        "\"" "\\\"")
                       "\r" "\\r")
                      "\n" "\\n")
                     "\t" "\\t")
                    "\"")]
    [(kn-list? datum) (string-append "[" (string-join (map kn-dump (cdr datum)) ", ") "]")]
    [else (error "kn-dump: invalid datum:" datum)]))

(define (kn-list<? e1 e2)
  (if (or (eq? e1 null) (eq? e2 null))
      #f
      (if (kn<? (car e1) (car e2))
          #t
          (kn-list<? (cdr e1) (cdr e2)))))

(define (kn-compare=? e1 e2)
  (if (or (eq? e1 null) (eq? e2 null))
      #t
      (if (equal? (car e1) (car e2))
          (kn-compare=? (cdr e1) (cdr e2))
          #f)))

(define (kn<? e1 e2)
  (cond
    [(number? e1) (< e1 e2)]
    [(string? e1) (string<? e1 e2)]
    [(boolean? e1) (and (not e1) e2)]
    [(kn-list? e1)
     (if (kn-compare=? e1 e2)
         (< (length (cdr e1)) (length (cdr e2)))
         (kn-list<? (cdr e1) (cdr e2)))]
    [else (error "kn<?: invalid datum:" e1)]))

(begin
  (define (kn-eval datum ctx)
    (if (kn-needs-eval? datum)
        (kn-eval (kn-eval-step datum ctx) ctx)
        datum))

  (define (kn-eval-step datum ctx)
    (cond
      [(not (kn-needs-eval? datum)) datum]
      [(kn-var? datum) (hash-ref ctx (cdr datum))]
      [(eq? datum 'prompt)
       (kn-eval-case [] [datum ctx]
                     (let [[line (read-line)]]
                       (if (eq? line eof)
                           'null
                           (string-trim line #px"(\r|\n)+" #:left? #f))))]
      [(eq? datum 'random)
       (kn-eval-case [] [datum ctx]
                     (random 0 32768))]
      
      [(kn-func? datum 'call)
       (kn-eval-case [[block 'block]] [datum ctx]
                     (cdr block))]
      [(kn-func? datum 'quit)
       (kn-eval-case [[code 'integer]] [datum ctx]
                     (exit code))]
      [(kn-func? datum 'output)
       (kn-eval-case [[str 'string]] [datum ctx]
                     (if (string-suffix? str "\\")
                         (printf "~a" (substring str 0
                                                 (- (string-length str) 1)))
                         (printf "~a~n" str))
                     'null)]
      [(kn-func? datum 'dump)
       (kn-eval-case [[val 'unchanged]] [datum ctx]
                     (printf "~a" (kn-dump val))
                     val)]
      [(kn-func? datum 'length)
       (kn-eval-case [[lst 'list]] [datum ctx]
                     (length (cdr lst)))]
      [(kn-func? datum '!)
       (kn-eval-case [[b 'boolean]] [datum ctx]
                     (not b))]
      [(kn-func? datum '~)
       (kn-eval-case [[i 'integer]] [datum ctx]
                     (- i))]
      [(kn-func? datum 'ascii)
       (kn-eval-case [[val 'unchanged]] [datum ctx]
                     (cond
                       [(integer? val) (string (integer->char val))]
                       [(string? val) (char->integer (string-ref val 0))]
                       [else (error "input to ASCII was not an integer or string")]))]
      [(kn-func? datum 'box)
       (kn-eval-case [[val 'unchanged]] [datum ctx]
                     (list 'list val))]
      [(kn-func? datum 'head)
       (kn-eval-case [[val 'unchanged]] [datum ctx]
                     (cond
                       [(string? val) (substring val 0 1)]
                       [(kn-list? val) (cadr val)]
                       [else (error "input to [ was not a string or list")]))]
      [(kn-func? datum 'tail)
       (kn-eval-case [[val 'unchanged]] [datum ctx]
                     (cond
                       [(string? val) (substring val 1)]
                       [(kn-list? val) (cons 'list (cddr val))]
                       [else (error "input to ] was not a string or list")]))]
      [(kn-func? datum 'value)
       (kn-eval-case [[val 'string]] [datum ctx]
                     (hash-ref ctx val))]
      [(kn-func? datum 'use)
       (kn-eval-case [[val 'string]] [datum ctx]
                     (call-parser (open-input-file val #:mode 'binary) val))]
      [(kn-func? datum 'eval)
       (kn-eval-case [[val 'string]] [datum ctx]
                     (call-parser val "evaled-code"))]
      [(kn-func? datum '+)
       (kn-eval-case [[e1 'unchanged] [e2 'coerced]] [datum ctx]
                     (cond
                       [(number? e1) (+ e1 e2)]
                       [(string? e1) (string-append e1 e2)]
                       [(kn-list? e1) (cons 'list (append (cdr e1) (cdr e2)))]
                       [else (error "first input to + was not an integer, string, or list")]))]
      [(kn-func? datum '-)
       (kn-eval-case [[e1 'unchanged] [e2 'coerced]] [datum ctx]
                     (cond
                       [(number? e1) (- e1 e2)]
                       [else (error "first input to - was not an integer")]))]
      [(kn-func? datum '*)
       (kn-eval-case [[e1 'unchanged] [e2 'integer]] [datum ctx]
                     (cond
                       [(number? e1) (* e1 e2)]
                       [(string? e1) (list->string (flatten (make-list e2 (string->list e1))))]
                       [(kn-list? e1) (cons 'list (flatten (make-list e2 (cdr e1))))]
                       [else (error "first input to * was not an integer, string, or list")]))]
      [(kn-func? datum '/)
       (kn-eval-case [[e1 'unchanged] [e2 'coerced]] [datum ctx]
                     (cond
                       [(number? e1) (quotient e1 e2)]
                       [else (error "first input to / was not an integer")]))]
      [(kn-func? datum '%)
       (kn-eval-case [[e1 'unchanged] [e2 'coerced]] [datum ctx]
                     (cond
                       [(number? e1) (remainder e1 e2)]
                       [else (error "first input to % was not an integer")]))]
      [(kn-func? datum '^)
       (kn-eval-case [[e1 'unchanged] [e2 'unchanged]] [datum ctx]
                     (cond
                       [(number? e1) (exact-truncate (expt e1 (coerce->integer e2)))]
                       [(kn-list? e1) (string-join (map coerce->string (cdr e1)) (coerce->string e2))]
                       [else (error "first input to ^ was not an integer or list")]))]
      [(kn-func? datum '<)
       (kn-eval-case [[e1 'unchanged] [e2 'coerced]] [datum ctx]
                     (kn<? e1 e2))]
      [(kn-func? datum '>)
       (kn-eval-case [[e1 'unchanged] [e2 'coerced]] [datum ctx]
                     (kn<? e2 e1))]
      [(kn-func? datum '?)
       (kn-eval-case [[e1 'unchanged] [e2 'unchanged]] [datum ctx]
                     (equal? e1 e2))]
      [(kn-func? datum '&)
       (kn-eval-case [[e1 'unchanged] [e2 'unevaluated]] [datum ctx]
                     (if (coerce->boolean e1) e2 e1))]
      [(kn-func? datum 'or)
       (kn-eval-case [[e1 'unchanged] [e2 'unevaluated]] [datum ctx]
                     (if (coerce->boolean e1) e1 e2))]
      [(kn-func? datum 'seq)
       (kn-eval-case [[e1 'unchanged] [e2 'unchanged]] [datum ctx]
                     e2)]
      [(kn-func? datum '=)
       (kn-eval-case [[e1 'unevaluated] [e2 'unchanged]] [datum ctx]
                     (cond
                       [(kn-var? e1) (hash-set! ctx (cdr e1) e2)]
                       [else (let [[value (kn-eval e1 ctx)]]
                               (if (string? value)
                                   (hash-set! ctx value e2)
                                   (error "first input to = was not a variable name or string")))])
                     e2)]
      [(kn-func? datum 'while)
       (kn-eval-case [[condition 'unevaluated] [body 'unevaluated]] [datum ctx]
                     (if (coerce->boolean (kn-eval condition ctx))
                         (begin
                           (kn-eval body ctx)
                           `(while ,condition . ,body))
                         'null))]

      [(kn-func? datum 'if)
       (kn-eval-case [[condition 'boolean] [iftrue 'unevaluated] [iffalse 'unevaluated]] [datum ctx]
                     (if condition iftrue iffalse))]
      [(kn-func? datum 'get)
       (kn-eval-case [[base 'unchanged] [index 'integer] [len 'integer]] [datum ctx]
                     (cond
                       [(string? base) (substring base index (+ index len))]
                       [(kn-list? base) (cons 'list (take (drop (cdr base) index) len))]
                       [else (error "first input to GET was not a string or list")]))]

      [(kn-func? datum 'set)
       (kn-eval-case [[base 'unchanged] [index 'integer] [len 'integer] [replacee 'coerced]] [datum ctx]
                     (cond
                       [(string? base)
                        (string-append (substring base 0 index)
                                       replacee
                                       (substring base (+ index len) (string-length base)))]
                       [(kn-list? base)
                        (cons 'list (append (take (cdr base) index)
                                            (cdr replacee)
                                            (drop (cdr base) (+ index len))))]
                       [else (error "first input to SET was not a string or list")]))]
      [(eq? datum '()) 'null]
      [else (error "invalid datum:" datum)])))

;; TODO: $
