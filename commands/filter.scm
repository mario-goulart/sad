(define-command 'filter
  "\
filter [<options>] <pattern>
  Filter lines matching <pattern> (a regular expression).

  <options>:
  --read-sexp | -r
    Assume inputs are sexps.

  --sre | -S
    Indicate that <pattern> uses SRE syntax.

  --stop-after-matches | -n <num matches>
    Stop after reaching any matches.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  `(((--help -help -h))
                    ((--read-sexp -r))
                    ((--stop-after-matches -n) . matches)
                    ((--sre -S)))))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (stop-after-matches (get-opt '(--stop-after-matches -n) args))
           (pattern (and-let* ((p (get-opt '(--) args)))
                      (and (not (null? p)) (car p)))))

      (handle-command-help 'filter args)

      (when stop-after-matches
        (let ((n (string->number stop-after-matches)))
          (if n
              (set! stop-after-matches n)
              (die! "filter: --stop-after-matches | -n: invalid argument: ~a."
                    stop-after-matches))))

      (unless pattern
        (die! "filter: missing pattern."))

      (let ((matches -1))
        (input-iterator
         read-sexp?
         (lambda (sexp)
           (when (eval `(let ((INPUT ,sexp))
                          (begin ,@(with-input-from-string pattern read-list))))
             (set! matches (add1 matches))
             (when (and stop-after-matches (= matches stop-after-matches))
               (exit 0))
             (print sexp)))
       (lambda (line)
         (when (irregex-search
                (if use-sre?
                    (with-input-from-string pattern read)
                    pattern)
                line)
           (set! matches (add1 matches))
           (when (and stop-after-matches (>= matches stop-after-matches))
             (exit 0))
           (print line))))))))
