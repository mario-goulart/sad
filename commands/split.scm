(define-command 'split
  "\
split [<options>] <pattern>
  Split the input according to <pattern> (a regular expression) and
  output a Scheme list of strings.

  <options>:
    --sre | -S
      Indicate that the argument to --pattern is an SRE.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--sre -S))
                    )))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (pattern (and-let* ((p (get-opt '(--) args)))
                      (and (not (null? p)) (car p)))))

      (handle-command-help 'split args)

      (unless pattern
        (die! "split: missing pattern."))

      (let ((pattern (if use-sre?
                         (with-input-from-string pattern read)
                         pattern)))
        (for-each-line
         (lambda (line lineno)
           (write (irregex-split pattern line))))))))
