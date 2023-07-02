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

(let ((user-conf
       (make-pathname (get-environment-variable "HOME") ".sad.conf")))
  (when (file-exists? user-conf)
    (load user-conf)))

(let ((args (command-line-arguments)))

  (when (null? args)
    (show-main-help 1))

  (when (member (car args) '("-h" "-help" "--help"))
    (show-main-help 0))

  (or (and-let* ((cmd (string->symbol (car args)))
                 (handler (alist-ref cmd (commands))))
        ((command-proc handler) (cdr args)))
      (die! "Invalid command: ~a" (car args))))

) ;; end module
