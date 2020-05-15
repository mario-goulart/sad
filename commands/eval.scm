(define-command 'eval "Eval"
  (lambda args
    (let ((read-sexp? #f)
          (write-sexp? #f)
          (exp #f)
          (bindings '()))
      (let loop ((args args))
        (unless (null? args)
          (let ((arg (car args)))
            (cond ((equal? arg "-r")
                   (set! read-sexp? #t)
                   (loop (cdr args)))
                  ((equal? arg "-s")
                   (when (null? (cdr args))
                     (die! "eval: -s requires an argument"))
                   (set! sep arg)
                   (loop (cddr args)))
                  ((equal? arg "-b")
                   (when (or (null? (cdr args)) (null? (cddr args)))
                     (die! "eval: -b: missing argument"))
                   (let ((var (cadr args))
                         (val (caddr args)))
                     (set! bindings
                           (cons
                            (list (string->symbol var)
                                  (with-input-from-string val read))
                            bindings)))
                   (loop (cdddr args)))
                  (else
                   (set! exp arg)
                   (loop (cdr args)))))))
      (unless exp
        (die! "eval: missing expression"))

      (set! bindings (reverse bindings))

      (let ((iterator (if read-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp)
           (let ((new-bindings
                  (eval `(let* ((INPUT ,line-or-sexp)
                                ,@bindings)
                           (begin ,@(with-input-from-string exp read-list))
                           (list ,@(map car bindings))))))
             (set! bindings (map list (map car bindings) new-bindings)))))))))
