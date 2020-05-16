(module pipe-smoker-cmd ()

(import scheme)
(import (chicken base)
        (chicken io)
        (chicken file)
        (chicken format)
        (chicken irregex)
        (chicken pathname)
        (chicken port)
        (chicken process-context)
        (chicken sort)
        (chicken string))
(import pipe-smoker)
(import optimism srfi-1 srfi-13 slice)

(include "commands/cols.scm")
(include "commands/format.scm")
(include "commands/eval.scm")
(include "commands/remove.scm")
(include "commands/filter.scm")
(include "commands/slurp.scm")

(let ((user-conf
       (make-pathname (get-environment-variable "HOME") ".pipe-smoker.conf")))
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
                (fprintf out "~a\n" command))
              (sort (map (compose symbol->string car) (get-commands))
                     string<))
    (when exit-code
      (exit exit-code))))

(let* ((args (parse-command-line
              (command-line-arguments)
              `(((-h -help --help))
                (--))))
       (command (alist-ref '-- args)))

  (when (alist-ref '-h args)
    (usage 0))

  (when (null? command)
    (usage 1))

  (or (and-let* ((cmd (string->symbol (car command)))
                 (handler (alist-ref cmd (get-commands))))
        (apply (command-proc handler) (cdr command)))
      (die! "Invalid command: ~a" (car command))))

) ;; end module
