(define-command 'buffer "\
buffer [<options>] [<number of lines>]
  Accumulate <number of lines> (a positive integer) and then dump
  them as sexps.  If <number of lines> is not provided, buffer all
  the input.

  <options>:
    --read-sexp | -r
      Assume inputs are sexps.

  Examples:

  $ seq 9 | sad buffer 3
  (\"1\" \"2\" \"3\")(\"4\" \"5\" \"6\")(\"7\" \"8\" \"9\")

  # Select even lines
  $ seq 6 | sad buffer 2 | sad cols 1
  2
  4
  6"
  (lambda (args*)
    (let* ((args (parse-command-line args* '(((--read-sexp -r)))))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (num-lines%
            (and-let* ((n (get-opt '(--) args)))
              (and (not (null? n)) (car n))))
           (num-lines (and num-lines% (string->number num-lines%))))

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
