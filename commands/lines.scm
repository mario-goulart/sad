(define-command 'lines
  "\
lines <range> [<range> ...]

  Select lines by number or range.  Syntax of ranges:
  * `<number>': a single line whose index is <number>
  * `:<number>': lines from 0 to <number>
  * `<number>:': lines from <number> to the last line
  Negative indexes are supported.

  <options>:
    --read-sexp | -r
      Assume inputs are sexps.
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--read-sexp -r))
                    )))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (ranges (parse-ranges 'lines (get-opt '(--) args)))
           (buffer '()))

      (handle-command-help 'lines args)

      (when (null? ranges)
        (die! "lines: missing range specification"))

      (let* ((iterator (if read-sexp? for-each-sexp for-each-line))
             (read-until (ranges-maximum-to ranges))
             (dump-called? #f)
             (dump!
              (lambda ()
                (unless dump-called?
                  (set! dump-called? #t)
                  (let ((buffer (reverse buffer)))
                    (for-each
                     (lambda (range)
                       (for-each print (apply list-slice (list buffer range))))
                     ranges))))))
        (iterator
         (lambda (line-or-sexp lineno)
           (if (or (not read-until) (<= lineno read-until))
               (set! buffer (cons line-or-sexp buffer))
               (dump!)))
         finalizer: dump!)))))
