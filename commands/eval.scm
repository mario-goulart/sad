(define-command 'eval "\
eval <options> <exp>
  Evaluate the Scheme expression <exp>.  The `INPUT' and `LINENO'
  variables are bound to the given input and to the line number in the
  evaluation context, respectively.  <exp> is implicitly placed in a
  `begin' form.  The chicken.irregex unit is implicitly imported in the
  evaluation context.

  <options>:
    --bind | -b <variable> <value>
      Bind <variable> to <value> in the execution context of <exp>.
      This parameter may be provided multiple times.

    --split-pattern <split pattern>
      Regular expression to be used to split columns.

    --sre | -S
      Indicate that regular expressions are in SRE syntax.

    --read-sexp | -r
      Assume inputs are sexps.

    --require-extension | -R <extension>
      Import a CHICKEN extension.  By default, chicken.irregex is
      imported.  This parameter may be provided multiple times.

    --finalizer | -f <exp>
      Scheme expression to be evaluated after the whole input has been
      consumed.

    --match | -m <pattern>
      Only apply <exp> to lines which match <pattern> (a regular
      expression).  Lines that do not match <pattern> are just printed.
      --match and --read-sexp are mutually exclusive.

    --not-match | -n <pattern>
      Only apply <exp> to lines which do NOT match <pattern> (a regular
      expression).  Lines that match <pattern> are just printed.
      --not-match and --read-sexp are mutually exclusive."
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--help -help -h))
                    ((--bind -b)
                     ,string->symbol
                     ,(lambda (x) (with-input-from-string x read)))
                    ((--require-extension -R) . ,string->symbol)
                    ((--finalizer -f) . finalizer)
                    ((--read-sexp -r))
                    ((--sre -S))
                    ((--match -m) . p)
                    ((--not-match -n) . p)
                    ((--split-pattern -s) . pattern)
                    )))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (bindings (get-opt '(--bind -b) args multiple?: #t))
           (extensions
            (or (get-opt '(--require-extension -R) args multiple?: #t) '()))
           (finalizer (get-opt '(--finalizer -f) args))
           (match-pattern% (get-opt '(--match -m) args))
           (not-match-pattern% (get-opt '(--not-match -n) args))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (match-pattern
            (and match-pattern%
                 (if use-sre?
                     (with-input-from-string match-pattern% read)
                     (string->sre match-pattern%))))
           (not-match-pattern
            (and not-match-pattern%
                 (if use-sre?
                     (with-input-from-string not-match-pattern% read)
                     (string->sre not-match-pattern%))))
           (split-pattern (get-opt '(--split-pattern -s) args))
           (exp (and-let* ((e (get-opt '(--) args)))
                  (and (not (null? e)) (car e)))))

      (unless exp
        (die! "eval: missing expression"))

      (when (and match-pattern read-sexp?)
        (die! "--match and --read-sexp are mutually exclusive."))

      (when (and not-match-pattern read-sexp?)
        (die! "--not-match and --read-sexp are mutually exclusive."))

      (let ((iterator (if read-sexp? for-each-sexp for-each-line))
            (pattern (and split-pattern
                          (if use-sre?
                              (with-input-from-string split-pattern read)
                              (string->sre split-pattern)))))
        (iterator
         (lambda (line-or-sexp lineno)
           (if (or (and (not match-pattern) (not not-match-pattern))
                   (and match-pattern (irregex-search match-pattern line-or-sexp))
                   (and not-match-pattern
                        (not (irregex-search not-match-pattern line-or-sexp))))
               (set! bindings
                     (cdr (eval-scheme
                           exp bindings extensions line-or-sexp lineno pattern)))
               (print line-or-sexp)))
         finalizer: (and finalizer
                         (lambda (lineno)
                           (eval-scheme
                            finalizer bindings extensions "" lineno pattern))))))))
