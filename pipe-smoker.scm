(import (chicken base)
        (chicken io)
        (chicken port)
        (chicken process-context)
        (chicken string))
(import srfi-1 srfi-13 slice)

(define *commands* '())

(define-record command name help proc)

(define (define-command name help proc)
  (set! *commands*
    (cons (cons name (make-command name help proc))
          *commands*)))

(define (read-stdin-line)
  (with-input-from-port (current-input-port) read-line))

(define (read-stdin-sexp)
  (with-input-from-port (current-input-port) read))

(define (die! fmt . args)
  (printf (current-error-port)
          (apply sprintf (cons (string-append fmt "\n") args)))
  (exit 1))


(define (for-each-line proc)
  (let loop ()
    (let ((line (read-stdin-line)))
      (unless (eof-object? line)
        (proc line)
        (loop)))))

(define (for-each-sexp proc)
  (let loop ()
    (let ((sexp (read-stdin-sexp)))
      (unless (eof-object? sexp)
        (proc sexp)
        (loop)))))

(define-command 'split "Split a string and outputs a list of sexps"
  (lambda args
    (let ((sep (if (null? args)
                   " \t"
                   (car args))))
      (for-each-line
       (lambda (line)
         (write (string-split line sep)))))))

(define (list-ref* lst idx)
  (handle-exceptions exn
    ""
    (list-ref lst idx)))

(define-command 'slice "Slice the input"
  (lambda args
    (let ((ranges '())
          (read-sexp? #f)
          (write-sexp? #f)
          (sep " \t")
          (joiner " "))
      (let loop ((args args))
        (unless (null? args)
          (let ((arg (car args)))
            (cond ((equal? arg "-w")
                   (set! write-sexp? #t)
                   (loop (cdr args)))
                  ((equal? arg "-r")
                   (set! read-sexp? #t)
                   (loop (cdr args)))
                  ((equal? arg "-s")
                   (when (null? (cdr args))
                     (die! "slice: -s requires an argument"))
                   (set! sep arg)
                   (loop (cddr args)))
                  ((equal? arg "-j")
                   (when (null? (cdr args))
                     (die! "slice: -j requires an argument"))
                   (set! joiner (cadr args))
                   (loop (cddr args)))
                  (else
                   (let ((range (map string->number (string-split arg ":" #t))))
                     (unless (every integer? range)
                       (die! "slice: invalid range: ~a" arg))
                     (set! ranges (cons
                                   (cons (and (substring-index ":" arg) #t)
                                         range)
                                   ranges))
                     (loop (cdr args))))))))
      (when (null? ranges)
        (die! "slice: missing slice specification"))
      (set! ranges (reverse ranges))
      (let ((iterator (if read-sexp? for-each-sexp for-each-line)))
        (iterator
         (lambda (line-or-sexp)
           (let* ((items (if read-sexp?
                             line-or-sexp
                             (string-split line-or-sexp sep)))
                  (slices
                   (map (lambda (item)
                          (map (lambda (range)
                                 (if (car range)
                                     (apply slice (cons item (cdr range)))
                                     (list-ref* item (cadr range))))
                               ranges))
                        items)))
             (if write-sexp?
                 (write slices)
                 (string-intersperse
                  (map (lambda (s)
                         (string-intersperse s joiner))
                       slices)
                  joiner)))))))))

(define-command 'nth "Get the nth element of a list"
  (lambda args
    (if (null? args)
        (die! "nth: missing argument")
        (or (and-let* ((idx (string->number (car args))))
              (for-each-sexp
               (lambda (sexp)
                 (print (list-ref sexp idx)))))
            (die! "nth: invalid argument ~a" (car args))))))

(define-command 'filter-line "Filter lines -- lines are bound to the LINE variable"
  (lambda args
    (let* ((user-filter (with-input-from-string (car args) read))
           (line-filter
            (lambda (line)
              (eval `(let ((LINE ,line)) ,user-filter)))))
      (if (null? args)
          (die! "filter: missing filter")
          (for-each-line
           (lambda (line)
             (when (line-filter line)
               (print line))))))))

(define-command 'filter-sexp "Filter sexps -- sexps are bound to the SEXP variable"
  (lambda args
    (let* ((user-filter (with-input-from-string (car args) read))
           (sexp-filter
            (lambda (sexp)
              (eval `(let ((SEXP (list ,@sexp))) ,user-filter)))))
      (if (null? args)
          (die! "filter: missing filter")
          (for-each-sexp
           (lambda (sexp)
             (when (sexp-filter sexp)
               (write sexp))))))))


;; join
;; split -> lines-to-sexps
;; no arguments => print line (nop)

(let ((args (command-line-arguments)))
  (or (and-let* ((cmd (string->symbol (car args)))
                 (handler (alist-ref cmd *commands*)))
        (apply (command-proc handler) (cdr args)))
      (error "Invalid command: " cmd)))
