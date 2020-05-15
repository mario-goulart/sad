(define-command 'filter "filter"
  (lambda args
    (let ((pattern #f)
          (use-sre? #f)
          (use-sexp? #f))
      (let loop ((args args))
        (unless (null? args)
          (let ((arg (car args)))
            (cond ((equal? arg "-s")
                   (set! use-sexp? #t)
                   (loop (cdr args)))
                  ((equal? arg "-S")
                   (set! use-sre? #t)
                   (loop (cdr args)))
                  (else
                   (set! pattern arg)
                   (loop (cdr args)))))))
      (unless pattern
        (die! "filter: missing pattern"))

      (let ((iterator (if use-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp)
           (if use-sexp?
               (when (eval `(let ((INPUT ,line-or-sexp))
                                (begin ,@(with-input-from-string pattern read-list))))
                 (print line-or-sexp))
               (when (irregex-search
                        (if use-sre?
                            (with-input-from-string pattern read)
                            pattern)
                        line-or-sexp)
                 (print line-or-sexp)))))))))
