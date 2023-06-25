(define-command 'extract "\
extract [<options>] <pattern>
  Extract strings matching <pattern> in the input.  Produce a Scheme
  list of matches.  If named submatches are used, they are represented
  as pairs (<name> . <match>) in the output (only the first match of
  named matches are considered).

  <options>:
    --sre | -S
      Indicate that <pattern> uses SRE syntax."
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--help -help -h))
                    ((--sre -S)))))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (pattern (get-opt '(--) args)))

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
                  (or (and-let* ((names (irregex-match-names matches)))
                        (if (null? names)
                            #f
                            (map (lambda (submatch-name)
                                   (cons submatch-name
                                         (irregex-match-substring matches
                                                                  submatch-name)))
                                 (map car names))))
                      (list (irregex-match-substring matches 1)))
                  '())))))))))
