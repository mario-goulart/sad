(import (chicken base)
        (chicken io)
        (chicken process))
(import test)

(define (run command)
  (with-input-from-pipe command read-string))

(define-syntax test-sad
  (syntax-rules ()
    ((_ description awk-command sad-command)
     (test description (run awk-command) (run sad-command)))))


(test-begin "sad")

(test-group "awk1line"
  (include "awk1line.scm"))

(test-group "sed1line"
  (include "sed1line.scm"))

(test-group "sort"
  (include "sort-density.scm"))

(test-group "tabularize"
  (include "test-tabularize.scm"))

(test-group "translate"
  (include "test-translate.scm"))

(test-group "join"
  (include "test-join.scm"))

(test-end "sad")

(test-exit)
