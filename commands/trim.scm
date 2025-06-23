(declare (unit sad-trim))

(module sad-trim ()

(import scheme)
(import (chicken base)
        (chicken string))
(import commands optimism srfi-13)
(import sad)

(define-command 'trim "\
trim
  Remove blanks around tokens (lines/sexps).

  <option>s:
    --read-sexp | -r
      Assume inputs are sexps.  The output will also be written as sexps.

    --right
      Remove blanks on the right side of tokens.

    --left
      Remove blanks on the left side of tokens.

  Examples:

  $ echo '  foo  ' | sad trim
  foo

  $ echo '  foo  ' | sad trim --right
    foo

  $ echo '  foo  ' | sad trim --left
  foo  "
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--read-sexp -r))
                    ((--right))
                    ((--left))
                    )))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (right? (get-opt '(--right) args flag?: #t))
           (left? (get-opt '(--left) args flag?: #t))
           (trimmer (cond ((or (and (not right?) (not left?))
                               (and right? left?))
                           string-trim-both)
                          (left?
                           string-trim)
                          (right?
                           string-trim-right)))
           (printer (if read-sexp? write print)))

      (if read-sexp?
          (for-each-sexp
           (lambda (sexp _)
             (if (list? sexp)
                 (printer (map (compose ->string trimmer) sexp))
                 (printer (trimmer (->string sexp))))))
          (for-each-line
           (lambda (line _)
             (printer (trimmer line))))))))

) ;; end module
