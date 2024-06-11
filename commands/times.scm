(declare (unit sad-times))

(module sad-times ()

(import scheme)
(import (chicken base))
(import commands)
(import sad)

(define-command '* "\
*
  Multiply items read from a list from the standard input assuming they can
  be converted to numbers.  Items that cannot be converted to number
  are discarded.

  `sad '*'` is equivalent to `sad apply '*' 'string->number'`

  Examples:

  $ echo 1 2 3 | sad split | sad '*'
  6

  # Factorial
  $ seq 10 | sad buffer | sad '*'
  3628800"
  (lambda (args*)
    (print (apply * (read-numbers)))))

) ;; end module
