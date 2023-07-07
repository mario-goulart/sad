(define-command 'apply "\
apply <op> [<converter>]
  Expect a Scheme list as input and apply <op> to the list.  <converter>
  defaults to `identity' and will ll be applied to all elements of the
  input list before the application of <op>.

  sad apply <op> <converter>

  is equivalent to

  sad eval -r '(print (apply <op> (map <converter> INPUT)))'

  Examples:

  $ echo 1 2 3 | sad split | sad apply + 'string->number'
  6

  # Factorial
  $ seq 10 | sad buffer | sad apply '*' 'string->number'
  3628800"
  (lambda (args*)
    (when (null? args*)
      (show-command-help 'apply 1))
    (let* ((eval-exp (lambda (str)
                       (if (char=? (string-ref str 0) #\()
                           (eval (with-input-from-string str read))
                           (eval (string->symbol str)))))
           (op (eval-exp (car args*)))
           (converter (if (null? (cdr args*))
                          identity
                          (eval-exp (cadr args*)))))
      (print (apply op (map converter (read)))))))
