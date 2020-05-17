(define-command 'remove
  "\
remove [<options>] <pattern>
  Remove lines matching <pattern> (a regular expression or a Scheme
  expression when --eval or --read-sexp is given).

  <options>:
  --eval | -e
    Indicate that <pattern> is a Scheme expression to be evaluated.
    Lines that cause the evaluation of <pattern> to return non-#f
    are removed.

  --read-sexp | -r
    Assume inputs are sexps.  Implies --eval.

  --write-sexp | -w
    Write sexps.

  --sre | -S
    Indicate that <pattern> uses SRE syntax.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--eval -e))
                    ((--read-sexp -r))
                    ((--write-sexp -w))
                    ((--sre -S)))))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (write-sexp? (get-opt '(--write-sexp -w) args flag?: #t))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (use-eval? (get-opt '(--eval -e) args flag?: #t))
           (pattern (and-let* ((p (get-opt '(--) args)))
                      (and (not (null? p)) (car p)))))

      (handle-command-help 'remove args)

      (when (and use-sre? use-eval?)
        (die! "remove: --sre and --eval cannot be used together."))

      (unless pattern
        (die! "remove: missing pattern."))

      (input-iterator
       read-sexp?
       (lambda (sexp lineno)
         (unless (car (eval-scheme pattern '() sexp lineno))
           ((if write-sexp? write print) sexp)))
       (lambda (line lineno)
         (if use-eval?
             (unless (car (eval-scheme pattern '() line lineno))
               ((if write-sexp? write print) line))
             (unless (irregex-search
                      (if use-sre?
                          (with-input-from-string pattern read)
                          pattern)
                      line)
               ((if write-sexp? write print)
                (if write-sexp? (list line) line)))))))))
