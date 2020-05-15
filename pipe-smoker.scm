(import (chicken base)
        (chicken io)
        (chicken format)
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

(include "commands/slice.scm")
(include "commands/format.scm")
(include "commands/eval.scm")

(let ((args (command-line-arguments)))
  (or (and-let* ((cmd (string->symbol (car args)))
                 (handler (alist-ref cmd *commands*)))
        (apply (command-proc handler) (cdr args)))
      (error "Invalid command: " cmd)))
