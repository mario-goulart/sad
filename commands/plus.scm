(declare (unit sad-plus))

(module sad-plus ()

(import scheme)
(import (chicken base))
(import commands)
(import sad)

(define-command '+ "\
+
  Add items read from a list from the standard input assuming they can
  be converted to numbers.  Items that cannot be converted to number
  are discarded.

  `sad +` is equivalent to `sad apply + 'string->number'`

  Examples:

  $ echo 1 2 3 | sad split | sad +
  6

  $ seq 10 | sad buffer | sad +
  55"
  (lambda (args*)
    (print (apply + (read-numbers)))))

) ;; end module
