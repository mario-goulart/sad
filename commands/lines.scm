(declare (unit sad-lines))

(module sad-lines ()

(import scheme)
(import (chicken base))
(import commands optimism simple-logger srfi-1)
(import sad)

(define-command 'lines "\
lines <range> [<range> ...]
  Select lines by number or range.  Syntax of ranges:
  * `<number>': a single line whose index is <number>
  * `:<number>': lines from 0 to <number>
  * `<number>:': lines from <number> to the last line
  Negative indexes are supported.

  <options>:
    --delete | -d
      Delete the lines in the given ranges.

    --read-sexp | -r
      Assume inputs are sexps.

  Examples:

  $ seq 10 | sad lines 4:7
  5
  6
  7

  $ seq 10 | sad lines -1
  10

  $ seq 10 | sad lines 1:-7
  2
  3

  $ seq 10 | sad lines 7:
  8
  9
  10"
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  '(((--delete -d))
                    ((--read-sexp -r))
                    )))
           (delete? (get-opt '(--delete -d) args flag?: #t))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (ranges (parse-ranges 'lines (get-opt '(--) args)))
           (buffer '()))

      (when (null? ranges)
        (die! "lines: missing range specification"))

      (let* ((iterator (if read-sexp? for-each-sexp for-each-line))
             (read-until (ranges-maximum-to ranges))
             (read-from (and delete? (ranges-minimum-from ranges)))
             (dump-called? #f)
             (dump!
              (lambda (_)
                (unless dump-called?
                  (set! dump-called? #t)
                  (let ((buffer (reverse buffer)))
                    (if delete?
                        (let ((len (length buffer)))
                          (for-each
                           (lambda (lineno line)
                             (unless (any (lambda (range)
                                            (in-range? lineno range len))
                                          ranges)
                               (print line)))
                           (iota len)
                           buffer))
                        (for-each
                         (lambda (range)
                           (for-each print (apply list-slice (list buffer range))))
                         ranges)))))))
        (iterator
         (lambda (line-or-sexp lineno)
           (if delete?
               (if (or (not read-until) (not read-from))
                   (set! buffer (cons line-or-sexp buffer))
                   (when (or (< lineno read-from)
                             (not (any (lambda (range)
                                         (in-range? lineno range read-until))
                                       ranges)))
                     (print line-or-sexp)))
               (if (or (not read-until) (<= lineno read-until))
                   (set! buffer (cons line-or-sexp buffer))
                   (dump! lineno))))
         finalizer: dump!)))))

) ;; end module
