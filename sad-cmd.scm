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
(import commands format natural-sort optimism srfi-1 srfi-13 slice) ;; FIXME: remove slice?

(include "commands/buffer.scm")
(include "commands/cols.scm")
(include "commands/eval.scm")
(include "commands/extract.scm")
(include "commands/filter.scm")
(include "commands/format.scm")
(include "commands/lines.scm")
(include "commands/replace.scm")
(include "commands/sort.scm")
(include "commands/split.scm")
(include "commands/join.scm")

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
