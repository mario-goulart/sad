(define-command 'extract "\
extract [<options>] <pattern>
  Extract strings matching <pattern> in the input.  Produces a Scheme
  list of matches.  If named submatches are used, they are represented
  as pairs (<name> . <match>) in the output.

  <options>:
    --sre | -S
      Indicate that <pattern> uses SRE syntax."
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  `(((--help -help -h))
                    ((--sre -S)))))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (pattern (get-opt '(--) args)))

      (handle-command-help 'extract args)

      (when (null? pattern)
        (die! "extract: missing <pattern>."))

      (let ((pattern
             (if use-sre?
                 (with-input-from-string (car pattern) read)
                 (car pattern))))
        (for-each-line
         (lambda (line lineno)
           (let ((matches (irregex-search pattern line)))
             (write
              (if (irregex-match-data? matches)
                  (irregex-match-substring matches 1)
                  '())))))))))
