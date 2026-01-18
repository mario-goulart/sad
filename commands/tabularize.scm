(declare (unit sad-tabularize))

(module sad-tabularize ()

(import scheme)
(import (chicken base)
        (chicken fixnum)
        (chicken format)
        (chicken port)
        (chicken string))
(import commands optimism simple-logger srfi-1 utf8-srfi-13)
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

(define (render-line-sep cols-width max-cols sep line-char padding)
  (display sep)
  (for-each (lambda (colno)
              (display-* line-char (+ (* 2 padding) (vector-ref cols-width colno)))
              (display sep))
            (iota max-cols))
  (newline))

(define (render-header cols-width max-cols sep line-char padding)
  (display sep)
  (for-each (lambda (colno)
              (display-* " " (+ (* 2 padding) (vector-ref cols-width colno)))
              (display sep))
            (iota max-cols))
  (newline)
  (render-line-sep cols-width max-cols sep line-char padding))

(define (ensure-full-line line cur-num-cols target-num-cols)
  (if (fx= cur-num-cols target-num-cols)
      line
      (append line (make-list (- target-num-cols cur-num-cols) " "))))

(define (render-table table #!key padding borderless? markdown? grid? first-line-is-header?)
  (let* ((padding (or padding 1))
         (max-lines (length table))
         (max-cols (get-max-cols table))
         (cols-width (measure-cols-width table max-cols))
         (table-width (sub1
                       (+ (* 2 padding max-cols)
                          max-cols
                          (apply + (vector->list cols-width)))))
         (line-char (cond (borderless? " ")
                          (markdown? "-")
                          (else "─")))
         (sep (cond (borderless? " ")
                     (markdown? "|")
                     (else "│")))
         (render-horizontal-border
          (lambda (top?)
            (let ((corner-left (if markdown? "|" (if top? "┌" "└")))
                  (corner-right (if markdown? "|" (if top? "┐" "┘"))))
              (display corner-left)
              (display-* line-char table-width)
              (print corner-right)))))

    (when (and markdown? (not first-line-is-header?) (not (null? table)))
      ;; Markdown requires a header
      (render-header cols-width max-cols sep line-char padding))

    (unless (or borderless? markdown?)
      (render-horizontal-border #t))

    (let loop ((lines table) (lineno 0))
      (when (and first-line-is-header? (or (not grid?) markdown?) (fx= lineno 1))
        (render-line-sep cols-width max-cols sep line-char padding))
      (unless (null? lines)
        (let* ((%line (car lines))
               (line (ensure-full-line %line (length %line) max-cols))
               (last-col (sub1 max-cols))
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
                  (iota max-cols))
             sep)
            (if borderless? "" sep)))
          (unless (or borderless? (not grid?) markdown? (null? (cdr lines)))
            (render-line-sep cols-width max-cols sep line-char padding)))
        (loop (cdr lines) (add1 lineno))))
      (unless (or borderless? markdown?)
        (render-horizontal-border #f))))


(define-command 'tabularize "\
tabularize
  Tabularize the input, which will be read as a list of lists.

    <options>:
    --padding | -p <num spaces>
      Number of spaces to print around table items.

    --borderless | -B
      Draw tables without borders.

    --first-line-is-header | -H
      Use the first line as the header of the table.

    --grid | -g
      Draw internal grid (ignored when --markdown or --borderless is used).

    --markdown | -m
      Draw tables using Markdown syntax.

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
                    ((--grid -g))
                    ((--first-line-is-header -H))
                    ((--markdown -m))
                    ((--padding -p) . ,string->number))))
           (borderless? (get-opt '(--borderless -B) args flag?: #t))
           (grid? (get-opt '(--grid -g) args flag?: #t))
           (first-line-is-header? (get-opt '(--first-line-is-header -H) args flag?: #t))
           (markdown? (get-opt '(--markdown -m) args flag?: #t))
           (padding (get-opt '(--padding -p) args)))
      (when (and markdown? borderless?)
        (die! "--markdown and --borderless are mutually exclusive."))
      (when (and grid? (or markdown? borderless?))
        (log-warning
         "--grid is ignored when used together with --markdown or --borderless"))
      (let loop ()
        (let ((line (read)))
          (if (eof-object? line)
              (set! table (reverse table))
              (begin
                (set! table (cons line table))
                (loop)))))
      (unless (null? table)
        (render-table table
                      padding: (or padding 1)
                      borderless?: borderless?
                      markdown?: markdown?
                      grid?: grid?
                      first-line-is-header?: first-line-is-header?
                      )))))

) ;; end module
