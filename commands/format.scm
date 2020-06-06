(define-command 'format "\
format [<options>] <format>
  Format items of the input the specified format.  The input must
  be a Scheme list which will be given, alongside with the <format>
  string, as argument to the `format' procedure of the `format' egg."
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    )))
           (format-string (and-let* ((f (get-opt '(--) args)))
                            (and (not (null? f)) (car f)))))

      (handle-command-help 'format args)

      (unless format-string
        (die! "format: missing format specification"))

      (for-each-sexp
       (lambda (sexp lineno)
         (apply format (cons #t (cons format-string sexp))))))))
