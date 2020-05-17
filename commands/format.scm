(define-command 'format
  "\
format [<options>] <format>

  Format tokens of the input split by a separator using a format
  string (<format>).  References to tokens in the the format string
  follow the `{<number>}' syntax, where <number> is the index of the
  token in the list of tokens created by splitting the input on the
  separator.

  <options>:
    --sep | -s
      String used to separate tokens (given to `string-split').
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--sep -s) . separator)
                    )))
           (sep (or (get-opt '(--sep -s) args) " \t"))
           (format (and-let* ((f (get-opt '(--) args)))
                     (and (not (null? f)) (car f)))))

      (handle-command-help 'format args)

      (unless format
        (die! "format: missing format specification"))

      (for-each-line
       (lambda (line lineno)
         (let ((tokens (string-split line sep))
               (str format))
           (for-each
            (lambda (idx token)
              (set! str (irregex-replace/all `(: "{" ,(number->string idx) "}") str token)))
            (iota (length tokens))
            tokens)
           (set! str (irregex-replace/all `(: "{" numeric "}") str ""))
           (printf str)))))))
