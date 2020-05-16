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
        (chicken string))
(import pipe-smoker)
(import srfi-1 srfi-13 slice)

(include "commands/slice.scm")
(include "commands/format.scm")
(include "commands/eval.scm")
(include "commands/remove.scm")
(include "commands/filter.scm")
(include "commands/slurp.scm")

(let ((user-conf
       (make-pathname (get-environment-variable "HOME") ".pipe-smoker.conf")))
  (when (file-exists? user-conf)
    (load user-conf)))

(let ((args (command-line-arguments)))
  (or (and-let* ((cmd (string->symbol (car args)))
                 (handler (alist-ref cmd (get-commands))))
        (apply (command-proc handler) (cdr args)))
      (error "Invalid command: " (car args))))

) ;; end module
