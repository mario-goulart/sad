(define-command 'filter "\
filter [<options>] <pattern>
  filter lines matching <pattern> (a regular expression or a Scheme
  expression when --eval or --read-sexp is given).

  <options>:
    --delete | -d
      Delete lines that match <pattern>.

    --eval | -e
      Indicate that <pattern> is a Scheme expression to be evaluated.
      Lines that cause the evaluation of <pattern> to return non-#f
      are preserved (or deleted, if --delete is used).

    --bind | -b <variable> <value>
      Bind <variable> to <value> in the execution context of <pattern>.
      (when --eval is given). This parameter may be provided multiple
      times.

    --require-extension | -R <extension>
      Import a CHICKEN extension.  By default, chicken.irregex is
      imported.  This parameter may be provided multiple times and only
      makes sense when --eval is used.

    --finalizer | -f <exp>
      Scheme expression to be evaluated after the whole input has been
      consumed.

    --read-sexp | -r
      Assume inputs are sexps.  Implies --eval.

    --write-sexp | -w
      Write sexps.

    --sre | -S
      Indicate that regular expressions are in SRE syntax.

    --split-pattern <split pattern>
      Regular expression to be used to split columns in lines when --eval
      is used.

    --stop-after-matches | -n <num matches>
      Stop after reaching any matches."
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--delete -d))
                    ((--eval -e))
                    ((--bind -b)
                     ,string->symbol
                     ,(lambda (x) (with-input-from-string x read)))
                    ((--require-extension -R) . ,string->symbol)
                    ((--finalizer -f) . finalizer)
                    ((--read-sexp -r))
                    ((--write-sexp -w))
                    ((--stop-after-matches -n) . matches)
                    ((--split-pattern -s) . pattern)
                    ((--sre -S)))))
           (delete? (get-opt '(--delete -d) args flag?: #t))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (write-sexp? (get-opt '(--write-sexp -w) args flag?: #t))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (use-eval? (get-opt '(--eval -e) args flag?: #t))
           (bindings (get-opt '(--bind -b) args multiple?: #t))
           (extensions
            (or (get-opt '(--require-extension -R) args multiple?: #t) '()))
           (finalizer (get-opt '(--finalizer -f) args))
           (stop-after-matches (get-opt '(--stop-after-matches -n) args))
           (split-pattern (get-opt '(--split-pattern -s) args))
           (pattern (and-let* ((p (get-opt '(--) args)))
                      (and (not (null? p)) (car p)))))

      (when (and use-sre? use-eval?)
        (die! "filter: --sre and --eval cannot be used together."))

      (unless pattern
        (die! "filter: missing pattern."))

      (when stop-after-matches
        (let ((n (string->number stop-after-matches)))
          (if n
              (set! stop-after-matches n)
              (die! "filter: --stop-after-matches | -n: invalid argument: ~a."
                    stop-after-matches))))

      (let* ((matches -1)
             (maybe-stop
              (lambda (line-or-sexp)
                (set! matches (add1 matches))
                (when (and stop-after-matches (>= matches stop-after-matches))
                  (exit 0))
                ((if write-sexp? write print) line-or-sexp)))
             (spattern (and split-pattern
                            (if use-sre?
                                (with-input-from-string split-pattern read)
                                (string->sre split-pattern))))
             (evaluator
              (lambda (line-or-sexp lineno)
                (let ((res/bindings
                       (eval-scheme
                        pattern bindings extensions line-or-sexp lineno spattern)))
                  (set! bindings (cdr res/bindings))
                  (caar res/bindings)))))
        (for-each-input
         read-sexp?
         (lambda (sexp lineno)
           (when ((if delete? not identity) (evaluator sexp lineno))
             (maybe-stop sexp)))
         (lambda (line lineno)
           (if use-eval?
               (when ((if delete? not identity) (evaluator line lineno))
                 (maybe-stop line))
               (when ((if delete? not identity)
                      (irregex-search
                       (if use-sre?
                           (with-input-from-string pattern read)
                           pattern)
                       line))
                 (maybe-stop (if write-sexp? (list line) line)))))
         finalizer:  (and finalizer
                          (lambda (lineno)
                            (eval-scheme
                             finalizer bindings extensions "" lineno spattern))))))))
