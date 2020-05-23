(define-command 'buffer
  "\
buffer [<options>] [<number of lines>]
  Accumulate <number of lines> (a positive integer) and then dump
  them as sexps.  If <number of lines> is not provided, buffer all
  the input.

  <options>:
    --read-sexp | -r
      Assume inputs are sexps.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--read-sexp -r)))))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (num-lines%
            (and-let* ((n (get-opt '(--) args)))
              (and (not (null? n)) (car n))))
           (num-lines (and num-lines% (string->number num-lines%))))

      (handle-command-help 'buffer args)

      (when (and num-lines% (not num-lines))
        (die! "buffer: expected a positive integer, got ~a." num-lines%))

      (when (and num-lines
                 (or (negative? num-lines) (not (integer? num-lines))))
        (die! "buffer: <number of lines> must be a positive integer."))

      (let ((lines-or-sexps '())
            (lineno 0)
            (iterator (if read-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp _)
           (cond ((and num-lines (< (sub1 num-lines) lineno))
                  (write (reverse lines-or-sexps))
                  (set! lineno 1)
                  (set! lines-or-sexps (list line-or-sexp)))
                 (else
                  (set! lineno (add1 lineno))
                  (set! lines-or-sexps (cons line-or-sexp lines-or-sexps)))))
         finalizer: (lambda (lineno)
                      (write (reverse lines-or-sexps))))))))
