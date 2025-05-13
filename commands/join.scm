(declare (unit sad-join))

(module sad-join ()

(import scheme)
(import (chicken base)
        (chicken string))
(import commands optimism)
(import sad)

(define-command 'join "\
join [<options>] [<joiner>]
  Join fields in the input with <joiner>.  If <joiner> is not provided,
  a space will be used.  Input is expected to be Scheme lists.

  <options>:
    --translate-escapes | -e
      Translate escaped characters into their corresponding control
      characters.  The following ones are supported:
      * \\n => newline
      * \\t => tab

  Examples:

  $ echo 1 2 3 | sad split | sad join
  1 2 3

  $ echo 1 2 3 | sad split | sad join :
  1:2:3

  $ echo 1 2 3 | sad split | sad join -e '\\n'
  1
  2
  3"
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--translate-escapes -e))
                    )))
           (translate-escapes?
            (get-opt '(--translate-escapes -e) args flag?: #t))
           (joiner (and-let* ((j (get-opt '(--) args)))
                     (if (null? j)
                         " "
                         (if translate-escapes?
                             (translate-escapes (car j))
                             (car j))))))
      (for-each-sexp
       (lambda (sexp lineno)
         (print (string-intersperse (map ->string sexp) joiner)))))))

) ;; end module
