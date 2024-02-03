(declare (unit sad-join))

(module sad-join ()

(import scheme)
(import (chicken base)
        (chicken string))
(import commands optimism)
(import sad)

(define-command 'join "\
join [<joiner>]
  Join fields in the input with <joiner>.  If <joiner> is not provided,
  a space will be used.  Input is expected to be Scheme lists.

  Examples:

  $ echo 1 2 3 | sad split | sad join
  1 2 3

  $ echo 1 2 3 | sad split | sad join :
  1:2:3"
  (lambda (args*)
    (unless (or (null? args*) (null? (cdr args*)))
      (show-command-help 'join 1))

    (let ((joiner (if (null? args*)
                      " "
                      (car args*))))
      (for-each-sexp
       (lambda (sexp lineno)
         (print (string-intersperse (map ->string sexp) joiner)))))))

) ;; end module
