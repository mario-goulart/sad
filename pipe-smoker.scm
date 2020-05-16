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
 )

(import scheme)
(import (chicken base)
        (chicken condition)
        (chicken format)
        (chicken io)
        (chicken port))
(import srfi-1)

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

(define (input-iterator input-is-sexp? sexp-handler line-handler)
  (if input-is-sexp?
      (for-each-sexp sexp-handler)
      (for-each-line line-handler)))

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

) ;; end module
