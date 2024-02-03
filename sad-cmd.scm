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
(import commands optimism simple-logger)

(declare (uses sad-buffer))
(declare (uses sad-cols))
(declare (uses sad-eval))
(declare (uses sad-extract))
(declare (uses sad-filter))
(declare (uses sad-format))
(declare (uses sad-lines))
(declare (uses sad-replace))
(declare (uses sad-sort))
(declare (uses sad-split))
(declare (uses sad-join))
(declare (uses sad-apply))

(define sad-message "
sad -- Scheme-Aware Ducts (\"sed makes me sad\").

sad is a program for text processing on the command line.  It
provides limited commands for usual and specific tasks and more
powerful ones for more elaborated tasks.

All sad commands read from the standard input.

Most sad commands have the ability to read Scheme lists as inputs
and write them as outputs, as a way to provide a better protocol
between sad commands.

Some commands accept regular expressions as inputs.  In those cases,
commands provide options to specify them as strings or as _Extended
Scheme Regular Expression Syntax_ (i.e., the one used by irregex).
")

(let ((user-conf
       (make-pathname (get-environment-variable "HOME") ".sad.conf")))
  (when (file-exists? user-conf)
    (load user-conf)))

(let ((args (command-line-arguments)))

  (when (null? args)
    (show-main-help 1 message: sad-message))

  (when (member (car args) '("-h" "-help" "--help"))
    (show-main-help 0 message: sad-message))

  (or (and-let* ((cmd (string->symbol (car args)))
                 (handler (alist-ref cmd (commands))))
        ((command-proc handler) (cdr args)))
      (die! "Invalid command: ~a" (car args))))

) ;; end module
