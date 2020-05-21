(import (chicken base)
        (chicken io)
        (chicken process))
(import test)

(define (run command)
  (with-input-from-pipe command read-string))

(define-syntax test-awk
  (syntax-rules ()
    ((_ description awk-command sad-command)
     (test description (run awk-command) (run sad-command)))))


(test-begin "sad")

(test-group "awk1line"
  (include "awk1line.scm"))

(test-end "sad")

(test-exit)
