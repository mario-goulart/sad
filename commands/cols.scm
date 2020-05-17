(define-command 'cols
  "\
cols [<options>] <range> [<range> ...]

  Select columns based on ranges.  Syntax of ranges:
  * `<number>': a single column whose index is <number>
  * `:<number>': columns from 0 to <number>
  * `<number>:': columns from <number> to the last column
  Negative indexes are supported.

  <options>:
    --sep | -s
      String used to separate tokens (given to `string-split').

    --read-sexp | -r
      Assume inputs are sexps.

    --write-sexp | -w
      Write sexps.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--read-sexp -r))
                    ((--write-sexp -w))
                    ((--sep -s) . separator)
                    )))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (write-sexp? (get-opt '(--write-sexp -w) args flag?: #t))
           (sep (or (get-opt '(--sep -s) args) " \t"))
           (ranges (parse-ranges 'cols (get-opt '(--) args))))

      (handle-command-help 'cols args)

      (when (null? ranges)
        (die! "cols: missing columns specification"))

      (let ((iterator (if read-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp lineno)
           (let* ((items (if read-sexp?
                             line-or-sexp
                             (string-split line-or-sexp sep)))
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
