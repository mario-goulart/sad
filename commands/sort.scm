(define-command 'sort
  "\
sort [<options>]
  Sort the input, which is expected to be sexps.  When no option
  is provided, it sorts the car of the inputs according to
  `string<?'. Its input is typically the output of the buffer
  command.

  <options>
    --column | -c COLNUM
      Sort using column COLNUM.

    --criteria | -C CRITERIA
      Sort according to CRITERIA.  Available ones are:
      * numeric (columns will be implicitly converted to numbers)
      * alphabetic (default)
      * natural (using `natural-string<?' from the natural-sort egg)
      If --eval is provided, use CRITERIA as a sexp to be evaluated.

    --reverse | -r
      Reverse results.

    --eval | -e
      Indicate that CRITERIA for --criteria is a sexp.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--column -c) . colnum)
                    ((--criteria -C) . criteria)
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
           (eval-criteria? (get-opt '(--eval -e) args flag?: #t))
           (criteria% (get-opt '(--criteria -C) args))
           (criteria
            (if criteria%
                (if eval-criteria?
                    (with-input-from-string criteria% read)
                    (case (string->symbol criteria%)
                      ((numeric) <)
                      ((alphabetic) string<?)
                      ((natural) natural-string<?)
                      (else
                       (die! "sort: --criteria: invalid argument ~a" criteria%))))
                string<?))
           (accessor
            (if (and criteria% (equal? criteria% "numeric"))
                (lambda (l)
                  (string->number
                   (string-trim-both (list-ref l colnum))))
                (lambda (l)
                  (list-ref l colnum))))
           (reverse? (get-opt '(--reverse -r) args flag?: #t)))

      (handle-command-help 'sort args)

      (let ((results
             (sort
              (read-stdin-sexp)
              (if eval-criteria?
                  (eval criteria)
                  (lambda (l1 l2)
                    (criteria (accessor l1) (accessor l2)))))))
        (for-each write
                  (if reverse?
                      (reverse results)
                      results))))))
