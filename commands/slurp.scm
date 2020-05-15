(define-command 'slurp "Slurp"
  (lambda args
    (let ((lines '()))
      (for-each-line
       (lambda (line)
         (set! lines (cons line lines))))
      (write (reverse lines)))))
