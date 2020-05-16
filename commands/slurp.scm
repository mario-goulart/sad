(define-command 'slurp
  "\
slurp
  Accumulate all inputs and dump them as sexps at the end.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))))))

      (handle-command-help 'slurp args)

      (let ((lines '()))
        (for-each-line
         (lambda (line)
           (set! lines (cons line lines))))
        (write (reverse lines))))))
