(declare (unit sad-format))

(module sad-format ()

(import scheme)
(import (chicken base))
(import commands format optimism simple-logger)
(import sad)

(define-command 'format "\
format <format>
  Format items of the input the specified format.  The input must
  be a Scheme list which will be given, alongside with the <format>
  string, as argument to the `format' procedure of the `format' egg.

  Example:

  $ echo 12 12 12 12 12 12 12 |
    sad buffer -r |
    sad format 'dec: ~a bin: ~B octal: ~O hexa: ~X roman: ~@R ord: ~R~%'
  dec: 12 bin: 1100 octal: 14 hexa: c roman: XII ord: twelve"
  (lambda (args*)
    (let* ((args (parse-command-line args* '()))
           (format-string (and-let* ((f (get-opt '(--) args)))
                            (and (not (null? f)) (car f)))))

      (unless format-string
        (die! "format: missing format specification"))

      (for-each-sexp
       (lambda (sexp lineno)
         (apply format (cons #t (cons format-string sexp))))))))

) ;; end module
