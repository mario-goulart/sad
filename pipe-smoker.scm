(module pipe-smoker

(define-command
 get-commands
 command-name
 command-help
 command-proc
 for-each-line
 for-each-sexp
 list-ref*
 die!
 )

(import scheme)
(import (chicken base)
        (chicken condition)
        (chicken format)
        (chicken io)
        (chicken port))

(define *commands* '())

(define-record command name help proc)

(define (define-command name help proc)
  (set! *commands*
    (cons (cons name (make-command name help proc))
          *commands*)))

(define (get-commands)
  *commands*)

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

(define (list-ref* lst idx)
  (handle-exceptions exn
    ""
    (list-ref lst idx)))

) ;; end module
