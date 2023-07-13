(define-command 'apply "\
apply <options> <op> [<converter>]
  Expect a Scheme list as input and apply <op> to the list.  <converter>
  defaults to `identity' and will ll be applied to all elements of the
  input list before the application of <op>.

  sad apply <op> <converter>

  is equivalent to

  sad eval -r '(print (apply <op> (map <converter> INPUT)))'

  <options>:
    --require-extension | -R <extension>
      Import a CHICKEN extension.  This parameter may be provided
      multiple times.

  Examples:

  $ echo 1 2 3 | sad split | sad apply + 'string->number'
  6

  # Factorial
  $ seq 10 | sad buffer | sad apply '*' 'string->number'
  3628800"
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--require-extension -R) . ,string->symbol)
                    )))
           (extensions
            (or (get-opt '(--require-extension -R) args multiple?: #t) '()))
           (rest (get-opt '(--) args)))

      (when (null? rest)
        (show-command-help 'apply 1))
      (let* ((eval-exp (lambda (str)
                         (let ((exp (if (char=? (string-ref str 0) #\()
                                        (with-input-from-string str read)
                                        (string->symbol str))))
                           (if (null? extensions)
                               (eval exp)
                               (eval
                                `(begin
                                   (import ,@extensions)
                                   ,exp))))))
             (op (eval-exp (car rest)))
             (converter (if (null? (cdr rest))
                            identity
                            (eval-exp (string-trim (cadr rest))))))
        (print (apply op (map converter (read))))))))
