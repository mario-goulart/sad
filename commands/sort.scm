(declare (unit sad-sort))

(module sad-sort ()

(import scheme)
(import (chicken base)
        (chicken port)
        (chicken sort))
(import commands natural-sort optimism simple-logger srfi-13)
(import sad)

(define-command 'sort "\
sort [<options>]
  Sort the input, which is expected to be sexps.  When no option
  is provided, it sorts the car of the inputs according to
  `string<?'. Its input is typically the output of the buffer
  command.

  <options>
    --column | -c <colnum>
      Sort using column <colnum>.

    --criterion | -C <criterion>
      Sort according to <criterion>.  Available ones are:
      * numeric (columns will be implicitly converted to numbers)
      * alphabetic (default)
      * natural (using `natural-string<?' from the natural-sort egg)
      If --eval is provided, use <criterion> as a sexp to be evaluated.

    --reverse | -r
      Reverse results.

    --eval | -e
      Indicate that <criterion> for --criterion is a sexp."
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  '(((--column -c) . colnum)
                    ((--criterion -C) . criterion)
                    ((--reverse -r))
                    ((--eval -e))
                    )))
           (colnum (or (and-let* ((c (get-opt '(--column -c) args))
                                  (n (string->number c)))
                         (when (and c (or (not n)
                                          (< n 0)
                                          (not (integer? n))))
                           (die! "sort: --column: argument must be a positive integer"))
                         (inexact->exact n))
                       0))
           (eval-criterion? (get-opt '(--eval -e) args flag?: #t))
           (criterion% (get-opt '(--criterion -C) args))
           (criterion
            (if criterion%
                (if eval-criterion?
                    (with-input-from-string criterion% read)
                    (case (string->symbol criterion%)
                      ((numeric) <)
                      ((alphabetic) string<?)
                      ((natural) natural-string<?)
                      (else
                       (die! "sort: --criterion: invalid argument ~a" criterion%))))
                string<?))
           (accessor
            (if (and criterion% (equal? criterion% "numeric"))
                (lambda (l)
                  (string->number
                   (string-trim-both (list-ref l colnum))))
                (lambda (l)
                  (list-ref l colnum))))
           (reverse? (get-opt '(--reverse -r) args flag?: #t)))

      (let ((results
             (sort
              (read)
              (if eval-criterion?
                  (eval criterion)
                  (lambda (l1 l2)
                    (criterion (accessor l1) (accessor l2)))))))
        (for-each write
                  (if reverse?
                      (reverse results)
                      results))))))

) ;; end module
