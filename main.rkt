#lang racket

(require "parser.rkt"
         syntax/strip-context)

(define (kn-read-syntax path port)
  (define parse-tree (call-parser port path))
  (strip-context
   #`(module knight-mod knight/expander
       #,parse-tree)))

(module+ reader
  (provide (rename-out [kn-read-syntax read-syntax])))

(module+ main
  (require racket/cmdline)

  (define expr-or-file-name (make-parameter null))
  (define mode (make-parameter null))
  
  (command-line
   #:program "knight"
   #:once-any
   [("-e" "--expr") expr "Expression to run"
                    (expr-or-file-name expr)
                    (mode 'expr)]
   [("-f" "--file") file-name "File to run"
                    (expr-or-file-name file-name)
                    (mode 'file)]
   #:args ()
   (unless (eq? (mode) null)
     (namespace-require 'knight/expander)
     (case (mode)
       ['expr (eval (call-parser (open-input-string (expr-or-file-name))
                                 "input"))]
       ['file (eval (call-parser (open-input-file (expr-or-file-name))
                                 (expr-or-file-name)))])
     (void))))
