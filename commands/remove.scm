(define-command 'remove
  "\
remove [<options>] <pattern>
  Remove lines matching <pattern> (a regular expression).

  <options>:
  --read-sexp | -r
    Assume inputs are sexps.

  --sre | -S
    Indicate that <pattern> uses SRE syntax.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--read-sexp -r))
                    ((--sre -S)))))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (pattern (and-let* ((p (get-opt '(--) args)))
                      (and (not (null? p)) (car p)))))

      (handle-command-help 'remove args)

      (unless pattern
        (die! "remove: missing pattern."))

      (input-iterator
       read-sexp?
       (lambda (sexp)
         (unless (eval `(let ((INPUT ,sexp))
                          (begin ,@(with-input-from-string pattern read-list))))
           (print sexp)))
       (lambda (line)
         (unless (irregex-search
                  (if use-sre?
                      (with-input-from-string pattern read)
                      pattern)
                  line)
           (print line)))))))
