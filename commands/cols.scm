(define-command 'cols "\
cols [<options>] <range> [<range> ...]
  Select columns based on ranges.  Input is expected to be a Scheme list.

  Syntax of ranges:
  * `<number>': a single column whose index is <number>
  * `:<number>': columns from 0 to <number>
  * `<number>:': columns from <number> to the last column
  Negative indexes are supported.

  <options>:
    --delete | -d
      Delete columns in the given ranges.  Note that selection and
      deletion are mutually exclusive.

    --write-sexp | -w
      Write sexps.

    --joiner | -j
      String to use to join columns when printing (except when
      --write-sexp is given).  The default value is a space."
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--delete -d))
                    ((--write-sexp -w))
                    ((--joiner -j))
                    )))
           (delete? (get-opt '(--delete -d) args flag?: #t))
           (write-sexp? (get-opt '(--write-sexp -w) args flag?: #t))
           (joiner (get-opt '(--joiner -j) args))
           (ranges (parse-ranges 'cols (get-opt '(--) args))))

      (when (null? ranges)
        (die! "cols: missing columns specification."))

      (when (and write-sexp? joiner)
        (die! "--joiner and --write-sexp are mutually exclusive."))

      (for-each-sexp
       (lambda (sexp lineno)
         (let ((slices
                (if delete?
                    (let ((len (length sexp)))
                      (let loop ((cols sexp) (col-idx 0))
                        (if (null? cols)
                            '()
                            (let ((col (car cols)))
                              (if (any (lambda (range)
                                         (in-range? col-idx range len))
                                       ranges)
                                  (loop (cdr cols) (add1 col-idx))
                                  (cons col (loop (cdr cols) (add1 col-idx))))))))
                    (append-map (lambda (range)
                                  (apply list-slice (list sexp range)))
                                ranges))))
           (if write-sexp?
               (write slices)
               (print (string-intersperse slices (or joiner " "))))))))))
