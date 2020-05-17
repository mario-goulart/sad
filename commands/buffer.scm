(define-command 'buffer
  "\
buffer [<options>] [<number of lines>]
  Accumulate <number of lines> (a positive integer) and then dump
  them as sexps.  If <number of lines> is not provided, buffer all
  the input.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h)))))
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

      (let ((lines '())
            (lineno 0))
        (for-each-line
         (lambda (line _)
           (cond ((and num-lines (< (sub1 num-lines) lineno))
                  (write (reverse lines))
                  (set! lineno 1)
                  (set! lines (list line)))
                 (else
                  (set! lineno (add1 lineno))
                  (set! lines (cons line lines)))))
         finalizer: (lambda ()
                      (write (reverse lines))))))))
