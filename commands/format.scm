(define-command 'format "Format"
  (lambda args
    (let ((read-sexp? #f)
          (write-sexp? #f)
          (sep " \t")
          (format #f))
      (let loop ((args args))
        (unless (null? args)
          (let ((arg (car args)))
            (cond ((equal? arg "-w")
                   (set! write-sexp? #t)
                   (loop (cdr args)))
                  ((equal? arg "-r")
                   (set! read-sexp? #t)
                   (loop (cdr args)))
                  ((equal? arg "-s")
                   (when (null? (cdr args))
                     (die! "format: -s requires an argument"))
                   (set! sep (cadr args))
                   (loop (cddr args)))
                  (else
                   (set! format arg)
                   (loop (cdr args)))))))
      (unless format
        (die! "format: missing format specification"))

      (let ((iterator (if read-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp)
           (let ((tokens (string-split line-or-sexp sep))
                 (str format))
             (for-each
              (lambda (idx token)
                (set! str (irregex-replace/all `(: "{" ,(number->string idx) "}") str token)))
              (iota (length tokens))
              tokens)
             (set! str (irregex-replace/all `(: "{" numeric "}") str ""))
             (printf str))))))))
