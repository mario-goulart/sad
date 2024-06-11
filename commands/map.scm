(declare (unit sad-map))

(module sad-map ()

(import scheme)
(import (chicken base)
        (chicken port))
(import commands optimism srfi-13)
(import sad)

(define-command 'map "\
map <options> <op> [<converter>]
  Expect a Scheme list as input and apply <op> to the items of the list.
  <converter> defaults to `identity' and will be applied to all elements
  of the input list before the application of <op>.

  sad map <op> <converter>

  is equivalent to

  sad eval -r '(write (map <op> (map <converter> INPUT)))'

  <options>:
    --require-extension | -R <extension>
      Import a CHICKEN extension.  This parameter may be provided
      multiple times.

  Examples:

  $ echo 1 2 3 | sad split | sad map '(lambda (i) (string-append \"Item \" i))'
  (\"Item 1\" \"Item 2\" \"Item 3\")

  $ echo 1 2 3 | sad split | sad map add1 'string->number'
  (2 3 4)"
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--require-extension -R) . ,string->symbol)
                    )))
           (extensions
            (or (get-opt '(--require-extension -R) args multiple?: #t) '()))
           (rest (get-opt '(--) args)))

      (when (null? rest)
        (show-command-help 'map 1))
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
        (write (map op (map converter (read))))))))

) ;; end module
