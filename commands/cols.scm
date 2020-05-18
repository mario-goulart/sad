(define-command 'cols
  "\
cols [<options>] <range> [<range> ...]
  Select columns based on ranges.  Columns are split on spaces and tabs
  by default.

  Syntax of ranges:
  * `<number>': a single column whose index is <number>
  * `:<number>': columns from 0 to <number>
  * `<number>:': columns from <number> to the last column
  Negative indexes are supported.

  <options>:
    --delete | -d
      Delete columns in the given ranges.

    --separator | -s <separator>
      Characters used to separate tokens (given to `string-split').

    --joiner | -j <joiner>
      String used to put between columns when printing them after filtering
      on ranges (except when --write-sexp is used).  Default is a space.

    --pattern | -p <pattern>
      Regular expression to be used to split columns.

    --sre | -S
      Indicate that the argument to --pattern is an SRE.

    --read-sexp | -r
      Assume inputs are sexps.

    --write-sexp | -w
      Write sexps.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--delete -d))
                    ((--pattern -p) . pattern)
                    ((--sre -S))
                    ((--read-sexp -r))
                    ((--write-sexp -w))
                    ((--separator -s) . separator)
                    ((--joiner -j) . joiner)
                    )))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (write-sexp? (get-opt '(--write-sexp -w) args flag?: #t))
           (sep (or (get-opt '(--separator -s) args) " \t"))
           (joiner (or (get-opt '(--joiner -j) args) " "))
           (pattern (get-opt '(--pattern -p) args))
           (delete? (get-opt '(--delete -d) args flag?: #t))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (ranges (parse-ranges 'cols (get-opt '(--) args))))

      (handle-command-help 'cols args)

      (when (null? ranges)
        (die! "cols: missing columns specification."))

      (when (and use-sre? (not pattern))
        (die! "cols: --sre requires --pattern."))

      (let ((iterator (if read-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp lineno)
           (let* ((items
                   (if read-sexp?
                       line-or-sexp
                       (if pattern
                           (irregex-split
                            (if use-sre?
                                (with-input-from-string pattern read)
                                pattern)
                            line-or-sexp)
                           (string-split line-or-sexp sep))))
                  (slices
                   (if delete?
                       (let ((len (length items)))
                         (let loop ((cols items) (col-idx 0))
                           (if (null? cols)
                               '()
                               (let ((col (car cols)))
                                 (if (any (lambda (range)
                                            (in-range? col-idx range len))
                                          ranges)
                                     (loop (cdr cols) (add1 col-idx))
                                     (cons (list col) (loop (cdr cols) (add1 col-idx))))))))
                       (map (lambda (range)
                              (apply list-slice (list items range)))
                            ranges))))
             (if write-sexp?
                 (write (car slices))
                 (print
                  (string-intersperse
                   (map (lambda (s)
                          (string-intersperse (map ->string s)))
                        slices)
                   joiner))))))))))
