(declare (unit sad-tabularize))

(module sad-tabularize ()

(import scheme)
(import (chicken base)
        (chicken fixnum)
        (chicken format)
        (chicken port)
        (chicken string))
(import commands optimism srfi-1 utf8-srfi-13)
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

(define (display-* str times)
  (let loop ((times times))
    (unless (zero? times)
      (display str)
      (loop (sub1 times)))))

(define (render-table table #!key padding borderless?)
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
              (display-* (if borderless? " " "─") table-width)
              (print corner-right)))))
    (let loop ((lines table) (lineno 0))
      (when (and (zero? lineno) (not borderless?))
        (render-horizontal-border #t))

      (unless (null? lines)
        (let* ((line (car lines))
               (sep (if borderless? " " "│"))
               (num-cols (length line))
               (last-col (sub1 num-cols))
               (last-col? (lambda (col)
                            (fx= col last-col))))
          (print
           (string-append
            (if borderless? "" sep)
            (string-intersperse
             (map (lambda (val colno)
                    (with-output-to-string
                      (lambda ()
                        (unless borderless?
                          (display-* " " padding))
                        (if (and borderless? (last-col? colno))
                            (display val)
                            (begin
                              (printf "~a"
                                      (string-pad-right
                                       (->string val)
                                       (vector-ref cols-width colno)))
                              (display-* " " padding))))))
                  line
                  (iota num-cols))
             sep)
            (if borderless? "" sep))))
        (loop (cdr lines) (add1 lineno))))
    (unless borderless?
      (render-horizontal-border #f))))


(define-command 'tabularize "\
tabularize
  Tabularize the input, which will be read as a list of lists.

    <options>:
    --padding | -p <num spaces>
      Number of spaces to print around table items.

    --borderless | -B
      Draw tables without borders.

  Examples:

  $ seq 9 | sad buffer 3 | sad tabularize
  ┌───────────┐
  │ 1 │ 2 │ 3 │
  │ 4 │ 5 │ 6 │
  │ 7 │ 8 │ 9 │
  └───────────┘

  $ seq 9 | sad buffer 3 | sad tabularize --borderless
  1  2  3
  4  5  6
  7  8  9

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
                  `(((--borderless -B))
                    ((--padding -p) . ,string->number))))
           (borderless? (get-opt '(--borderless -B) args flag?: #t))
           (padding (get-opt '(--padding -p) args)))
      (let loop ()
        (let ((line (read)))
          (if (eof-object? line)
              (set! table (reverse table))
              (begin
                (set! table (cons line table))
                (loop)))))
      (render-table table
                    padding: (or padding 1)
                    borderless?: borderless?))))

) ;; end module
