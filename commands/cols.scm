(declare (unit sad-cols))

(module sad-cols ()

(import scheme)
(import (chicken base)
        (chicken io)
        (chicken irregex)
        (chicken port)
        (chicken string))
(import commands optimism simple-logger srfi-1)
(import sad)

(define-command 'cols "\
cols [<options>] <range> [<range> ...]
  Select columns based on ranges.  If --read-sexp is provided, the
  input is expected to be a Scheme list, otherwise a string will be
  read and split according to the pattern specified by --split-pattern
  (or a space, it if is not provided).

  Syntax of ranges:
  * `<number>': a single column whose index is <number>
  * `:<number>': columns from 0 to <number>
  * `<number>:': columns from <number> to the last column
  Negative indexes are supported.

  <options>:
    --delete | -d
      Delete columns in the given ranges.  Note that selection and
      deletion are mutually exclusive.

    --read-sexp | -r
      Assume inputs are sexps (if provided, output will be a Scheme list).

    --split-pattern | -s <pattern>
      Split input by <pattern>.  --read-sexp and --split-pattern are
      mutually exclusive.  If not provided, a space will be used.

    --sre | -S
      Indicate that <pattern> uses SRE syntax.

  Examples:

  $ echo 1 2 3 | sad cols 0
  1

  $ echo 1 2 3 | sad cols 2 0
  3 1

  $ echo 1 2 3 | sad cols 1:
  2 3

  $ echo 1 2 3 | sad cols -1
  3

  $ echo 1 2 3 | sad cols :-1
  1 2

  $ echo 1 2 3 | sad cols --delete 1
  1 3

  $ echo 1:2:3 | sad cols -s : 1
  2

  $ echo 1:2_3 | sad cols -S -s '(or \":\" \"_\")' 1
  2

  $ echo 1 2 3 | sad split | sad cols -r 1 | sad join
  2"
  (lambda (args*)
    (let* ((args (parse-command-line args* '(((--delete -d))
                                             ((--read-sexp -r))
                                             ((--split-pattern -s) . pattern)
                                             ((--sre -S))
                                             )))
           (delete? (get-opt '(--delete -d) args flag?: #t))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (split-pattern (get-opt '(--split-pattern -s) args))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (ranges (parse-ranges 'cols (get-opt '(--) args))))

      (when (null? ranges)
        (die! "cols: missing columns specification."))

      (when (and split-pattern read-sexp?)
        (die! "cols: --split-pattern and --read-sexp cannot be used together."))

      (when (and use-sre? read-sexp?)
        (log-warning "cols: Ignoring --sre --read-sexp is being used."))

      (define (make-line-reader)
        (let ((spattern (if split-pattern
                            (if use-sre?
                                (with-input-from-string split-pattern read)
                                (string->sre split-pattern))
                            " ")))
          (lambda ()
            (let ((line (read-line)))
              (if (eof-object? line)
                  line
                  (irregex-split spattern line))))))

      (for-each-sexp
       (lambda (sexp _)
         (let ((slices
                (if delete?
                    (let ((len (length sexp)))
                      (let loop ((cols sexp) (col-idx 0))
                        (if (null? cols)
                            '()
                            (let ((col (car cols)))
                              (if (any (lambda (range)
                                         (in-range? col-idx range len))
                                       ranges)
                                  (loop (cdr cols) (add1 col-idx))
                                  (cons col (loop (cdr cols) (add1 col-idx))))))))
                    (append-map (lambda (range)
                                  (apply list-slice (list sexp range)))
                                ranges))))
           (if read-sexp?
               (write slices)
               (print (string-intersperse slices " ")))))
       reader: (if read-sexp? #f (make-line-reader))))))

) ;; end module
