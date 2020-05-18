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
    --sep | -s
      Characters used to separate tokens (given to `string-split').

    --pattern | -p
      Regular expression to be used to split columns.  --pattern and --sep
      are mutually exclusive.

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
                    ((--pattern -p) . pattern)
                    ((--sre -S))
                    ((--read-sexp -r))
                    ((--write-sexp -w))
                    ((--sep -s) . separator)
                    )))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (write-sexp? (get-opt '(--write-sexp -w) args flag?: #t))
           (sep (get-opt '(--sep -s) args))
           (pattern (get-opt '(--pattern -p) args))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (ranges (parse-ranges 'cols (get-opt '(--) args)))
           (default-sep " \t"))

      (handle-command-help 'cols args)

      (when (null? ranges)
        (die! "cols: missing columns specification."))

      (when (and sep pattern)
        (die! "cols: --sep and --pattern are mutually exclusive."))

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
                           (string-split line-or-sexp (or sep default-sep)))))
                  (slices
                   (map (lambda (range)
                          (apply list-slice (list items range)))
                        ranges)))
             (if write-sexp?
                 (write (car slices))
                 (print
                  (string-intersperse
                   (map (lambda (s)
                          (string-intersperse (map ->string s)))
                        slices)))))))))))
