(define-command 'eval
  "\
eval <options> <exp>

  Evaluate the Scheme expression <exp>.  The `INPUT' and `LINENO'
  variables are bound to the given input and to the line number in the
  evaluation context, respectively.  <exp> is implicitly placed in a
  `begin' form.  The big-chicken egg is implicitly imported in the
  evaluation context.

  <options>:
    --bind | -b <variable> <value>
      Bind <variable> to <value> in the execution context of <exp>.

    --read-sexp | -r
      Assume inputs are sexps.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  `(((--help -help -h))
                    ((--bind -b)
                     ,string->symbol
                     ,(lambda (x) (with-input-from-string x read)))
                    ((--read-sexp -r))
                    ((--sep -s) . separator)
                    )))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (bindings (get-opt '(--bind -b) args multiple?: #t))
           (exp (and-let* ((e (get-opt '(--) args)))
                  (and (not (null? e)) (car e)))))

      (handle-command-help 'eval args)

      (unless exp
        (die! "eval: missing expression"))

      (let ((iterator (if read-sexp? for-each-sexp for-each-line))
            (lineno -1))
        (iterator
         (lambda (line-or-sexp)
           (set! lineno (add1 lineno))
           (let ((new-bindings
                  (eval `(let* ((INPUT (quote ,line-or-sexp))
                                (LINENO (quote ,lineno))
                                ,@bindings)
                           (begin
                             (import big-chicken)
                             ,@(with-input-from-string exp read-list))
                           (list ,@(map car bindings))))))
             (set! bindings (map list (map car bindings) new-bindings)))))))))
