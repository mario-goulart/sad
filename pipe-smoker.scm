(module pipe-smoker

(define-command
 get-command
 get-commands
 handle-command-help
 command-name
 command-help
 command-proc
 for-each-line
 for-each-sexp
 input-iterator
 list-ref*
 die!
 get-opt
 parse-ranges
 range?
 range-from
 range-to
 ranges-maximum-to
 list-slice
 eval-scheme
 )

(import scheme)
(import (chicken base)
        (chicken condition)
        (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken port))
(import slice srfi-1)

(define *commands* '())

(define-record command name help proc)

(define (define-command name help proc)
  (set! *commands*
    (cons (cons name (make-command name help proc))
          *commands*)))

(define (get-commands)
  *commands*)

(define (get-command command)
  (alist-ref command *commands*))

(define (read-stdin-line)
  (with-input-from-port (current-input-port) read-line))

(define (read-stdin-sexp)
  (with-input-from-port (current-input-port) read))

(define (die! fmt . args)
  (fprintf (current-error-port)
           (apply sprintf (cons (string-append fmt "\n") args)))
  (exit 1))

(define (for-each-line proc #!key finalizer)
  (let loop ()
    (let ((line (read-stdin-line)))
      (if (eof-object? line)
          (when finalizer
            (finalizer))
          (begin
            (proc line)
            (loop))))))

(define (for-each-sexp proc #!key finalizer)
  (let loop ()
    (let ((sexp (read-stdin-sexp)))
      (if (eof-object? sexp)
          (when finalizer
            (finalizer))
          (begin
            (proc sexp)
            (loop))))))

(define (input-iterator input-is-sexp? sexp-handler line-handler #!key finalizer)
  (if input-is-sexp?
      (for-each-sexp sexp-handler finalizer: finalizer)
      (for-each-line line-handler finalizer: finalizer)))

(define (list-ref* lst idx)
  (handle-exceptions exn
    ""
    (list-ref lst idx)))

(define (handle-command-help command parsed-args)
  (when (get-opt '(--help -help -h) parsed-args flag?: #t)
    (print (command-help (get-command command)))
    (exit 0)))

(define (get-opt options parsed-args #!key multiple? flag?)
  (cond (multiple?
         (filter-map (lambda (opt)
                       (and (memq (car opt) options)
                            (cdr opt)))
                     parsed-args))
        (flag?
         (let loop ((parsed-args parsed-args))
           (if (null? parsed-args)
               #f
               (let ((arg (car parsed-args)))
                 (if (and (pair? arg) (memq (car arg) options))
                     #t
                     (loop (cdr parsed-args)))))))
        (else
         (any (lambda (opt)
                (alist-ref opt parsed-args))
              options))))

(define-record range from to)

(define-record-printer (range obj out)
  (fprintf out "~a:~a"
           (or (range-from obj) "")
           (or (range-to obj) "")))

(define (parse-ranges caller ranges #!key (sep ":"))
  (let loop ((ranges ranges))
    (if (null? ranges)
        '()
        (let ((range (car ranges)))
          (cond
           ;; "<number>"
           ((irregex-match '(: (* "-") (+ numeric)) range)
            (cons (string->number range)
                  (loop (cdr ranges))))

           ;; "<number>:<number>"
           ((irregex-match
             `(: (submatch-named from (* "-") (+ numeric)) ,sep
                 (submatch-named to (* "-") (+ numeric)))
             range)
            => (lambda (m)
                 (cons (make-range (string->number (irregex-match-substring m 'from))
                                   (string->number (irregex-match-substring m 'to)))
                       (loop (cdr ranges)))))

           ;; ":<number>"
           ((irregex-match
             `(: ,sep (submatch-named to (* "-") (+ numeric)))
             range)
            => (lambda (m)
                 (cons (make-range #f (string->number (irregex-match-substring m 'to)))
                       (loop (cdr ranges)))))

           ;; "<number>:"
           ((irregex-match
             `(: (submatch-named from (* "-") (+ numeric)) ,sep)
             range)
            => (lambda (m)
                 (cons (make-range (string->number (irregex-match-substring m 'from)) #f)
                       (loop (cdr ranges)))))

           (else (die! "~a: invalid range: ~a" caller range)))))))

(define (ranges-maximum-to ranges)
  ;; Return #f in case any of the ranges imply requiring the full
  ;; sequence
  (let ((max-to 0))
    (for-each
     (lambda (range)
       (cond ((number? range)
              (if (negative? range)
                  (set! max-to #f)
                  (when (> range max-to)
                    (set! max-to range))))
             ((or (not (range-to range))
                  (and (range-from range) (negative? (range-from range)))
                  (and (range-to range) (negative? (range-to range))))
              (set! max-to #f))
             ((> (range-from range) max-to)
              (set! max-to (range-from range)))
             ((> (range-to range) max-to)
              (set! max-to (range-to range)))))
     ranges)
    max-to))

(define (list-slice lst range)
  (if (number? range)
      (list
       (if (< range 0)
           (list-ref* (reverse lst) (abs (+ range 1)))
           (list-ref* lst range)))
      (slice lst (range-from range) (range-to range))))

(define (eval-scheme exp bindings input lineno)
  (let ((new-bindings
         (eval `(let* ((INPUT (quote ,input))
                       (LINENO (quote ,lineno))
                       ,@bindings)
                  (begin
                    (import big-chicken)
                    ,@(with-input-from-string exp read-list))
                  (list ,@(map car bindings))))))
    (map list (map car bindings) new-bindings)))

) ;; end module
