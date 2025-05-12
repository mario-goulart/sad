(declare (unit sad-tabularize))

(module sad-tabularize ()

(import scheme)
(import (chicken base)
        (chicken format)
        (chicken port)
        (chicken string))
(import commands optimism srfi-1 srfi-13)
(import sad)

(define (get-max-cols table)
  (let loop ((lines table) (max 0))
    (if (null? lines)
        max
        (let ((line-len (length (car lines))))
          (loop (cdr lines)
                (if (> line-len max)
                    line-len
                    max))))))

(define (measure-cols-width table max-cols)
  (let* ((cols/lens (make-vector max-cols 0))
         (update-col!
          (lambda (idx val)
            (let ((col-len (vector-ref cols/lens idx))
                  (val-len (string-length (->string val))))
              (when (> val-len col-len)
                (vector-set! cols/lens idx val-len))))))
    (let loop-lines ((lines table))
      (if (null? lines)
          cols/lens
          (let ((line (car lines)))
            (let loop-cols ((cols line) (colno 0))
              (if (null? cols)
                  (loop-lines (cdr lines))
                  (begin
                    (update-col! colno (car cols))
                    (loop-cols (cdr cols) (add1 colno))))))))))

(define (render-horizontal-border top? max-lines max-cols cols-width padding)
  (let ((corner-left (if top? "┌" "└"))
        (corner-right (if top? "┐" "┘")))
    (let loop ((lines max-lines) (lineno 0))
      (when (zero? lineno)
        (let loop ((colno (sub1 max-cols)))
          (unless (zero? colno)
            (display corner-left)
            (let ((col-len (+ 1 (* 2 padding max-cols) (vector-ref cols-width colno))))
              (let loop-char ((col col-len))
                (display "─")
                (unless (zero? col)
                  (loop-char (sub1 col)))))
            (loop (sub1 colno))
            (print corner-right)))))))

(define (display-* str times)
  (let loop ((times times))
    (unless (zero? times)
      (display str)
      (loop (sub1 times)))))

(define (render-table table #!key padding)
  (let* ((padding (or padding 1))
         (max-lines (length table))
         (max-cols (get-max-cols table))
         (cols-width (measure-cols-width table max-cols))
         (table-width (sub1
                       (+ (* 2 padding max-cols)
                          max-cols
                          (apply + (vector->list cols-width)))))
         (render-horizontal-border
          (lambda (top?)
            (let ((corner-left (if top? "┌" "└"))
                  (corner-right (if top? "┐" "┘")))
              (display corner-left)
              (display-* "─" table-width)
              (print corner-right)))))
    (let loop ((lines table) (lineno 0))
      (when (zero? lineno)
        (render-horizontal-border #t))

      (unless (null? lines)
        (let* ((line (car lines))
               (sep "│"))
          (print
           (string-append
            sep
            (string-intersperse
             (map (lambda (val colno)
                    (with-output-to-string
                      (lambda ()
                        (display-* " " padding)
                        (printf "~a"
                                (string-pad-right
                                 (->string val)
                                 (vector-ref cols-width colno)))
                        (display-* " " padding))))
                  line
                  (iota (length line)))
             sep)
            sep)))
        (loop (cdr lines) (add1 lineno))))
    (render-horizontal-border #f)))


(define-command 'tabularize "\
tabularize
  Tabularize the input, which will be read as a list of lists.

    <options>:
    --padding | -p <num spaces>
      Number of spaces to print around table items.

  Examples:

  $ seq 9 | sad buffer 3 | sad tabularize
  ┌───────────┐
  │ 1 │ 2 │ 3 │
  │ 4 │ 5 │ 6 │
  │ 7 │ 8 │ 9 │
  └───────────┘

  $ cat /etc/passwd | sad lines 0:3 | sad split : | sad tabularize
  ┌─────────────────────────────────────────────────────────────┐
  │ root   │ x │ 0 │ 0 │ root   │ /root     │ /bin/bash         │
  │ daemon │ x │ 1 │ 1 │ daemon │ /usr/sbin │ /usr/sbin/nologin │
  │ bin    │ x │ 2 │ 2 │ bin    │ /bin      │ /usr/sbin/nologin │
  └─────────────────────────────────────────────────────────────┘"
  (lambda (args*)
    (let* ((table '())
           (args (parse-command-line
                  args*
                  `(((--padding -p) . ,string->number))))
           (padding (get-opt '(--padding -p) args)))
      (let loop ()
        (let ((line (read)))
          (if (eof-object? line)
              (set! table (reverse table))
              (begin
                (set! table (cons line table))
                (loop)))))
      (render-table table padding: (or padding 1)))))

) ;; end module
