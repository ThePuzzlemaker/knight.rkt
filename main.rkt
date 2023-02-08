#lang racket

(module+ main
  (require racket/cmdline)
  (require "private/eval.rkt")

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
   (when (eq? (mode) null)
     (printf "Error: one of `-e <expr>` and `-f <file-name>` must be specified~n")
     (exit 1))
   (let [[ctx (make-hash)]]
     (if (eq? (mode) 'expr)
         (kn-eval (call-parser (expr-or-file-name)) ctx)
         (kn-eval (call-parser (open-input-file (expr-or-file-name))
                               (expr-or-file-name)) ctx)))
   (void)))
