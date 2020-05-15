(define-command 'slice "Slice the input"
  (lambda args
    (let ((ranges '())
          (read-sexp? #f)
          (write-sexp? #f)
          (sep " \t"))
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
                     (die! "slice: -s requires an argument"))
                   (set! sep (cadr args))
                   (loop (cddr args)))
                  (else
                   (let ((range (map string->number (string-split arg ":"))))
                     (unless (every integer? range)
                       (die! "slice: invalid range: ~a" arg))
                     (set! ranges (cons
                                   (cons (and (substring-index ":" arg) #t)
                                         range)
                                   ranges))
                     (loop (cdr args))))))))
      (when (null? ranges)
        (die! "slice: missing slice specification"))
      (set! ranges (reverse ranges))
      (let ((iterator (if read-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp)
           (let* ((items (if read-sexp?
                             line-or-sexp
                             (string-split line-or-sexp sep)))
                  (slices
                   (map (lambda (range)
                          (if (car range)
                              (apply slice (cons items (cdr range)))
                              (list (list-ref* items (cadr range)))))
                        ranges)))
             (if write-sexp?
                 (write (car slices))
                 (print
                  (string-intersperse
                   (map (lambda (s)
                          (string-intersperse (map ->string s)))
                        slices)))))))))))
