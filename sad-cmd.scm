(module sad-cmd ()

(import scheme)
(import (chicken base)
        (chicken io)
        (chicken file)
        (except (chicken format) format)
        (chicken irregex)
        (chicken pathname)
        (chicken port)
        (chicken process-context)
        (chicken sort)
        (chicken string))
(import sad)
(import format natural-sort optimism srfi-1 srfi-13 slice) ;; FIXME: remove slice?

(include "commands/buffer.scm")
(include "commands/cols.scm")
(include "commands/eval.scm")
(include "commands/filter.scm")
(include "commands/format.scm")
(include "commands/lines.scm")
(include "commands/replace.scm")
(include "commands/sort.scm")
(include "commands/split.scm")

(let ((user-conf
       (make-pathname (get-environment-variable "HOME") ".sad.conf")))
  (when (file-exists? user-conf)
    (load user-conf)))

(define (usage #!optional exit-code)
  (let ((out (if (and exit-code (not (zero? exit-code)))
                 (current-error-port)
                 (current-output-port))))
    (fprintf out "Usage: ~a [-h|-help|--help] <command> [<command args>]\n"
             (pathname-file (program-name)))
    (fprintf out "\n<command>s are:\n")
    (for-each (lambda (command)
                (fprintf out "\n~a\n" (command-help (get-command command))))
              (map string->symbol
                   (sort (map (compose symbol->string car) (get-commands))
                         string<)))
    (when exit-code
      (exit exit-code))))

(let ((args (command-line-arguments)))

  (when (null? args)
    (usage 1))

  (when (member (car args) '("-h" "-help" "--help"))
    (usage 0))

  (or (and-let* ((cmd (string->symbol (car args)))
                 (handler (alist-ref cmd (get-commands))))
        (apply (command-proc handler) (cdr args)))
      (die! "Invalid command: ~a" (car args))))

) ;; end module
