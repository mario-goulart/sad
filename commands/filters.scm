(define (define-filter command)
  (define remove? (eq? command 'remove))
  (define-command command
    (sprintf "\
~a [<options>] <pattern>
  ~a lines matching <pattern> (a regular expression or a Scheme
  expression when --eval or --read-sexp is given).

  <options>:
  --eval | -e
    Indicate that <pattern> is a Scheme expression to be evaluated.
    Lines that cause the evaluation of <pattern> to return non-#f
    are ~a.

  --read-sexp | -r
    Assume inputs are sexps.  Implies --eval.

  --write-sexp | -w
    Write sexps.

  --sre | -S
    Indicate that <pattern> uses SRE syntax.

  --stop-after-matches | -n <num matches>
    Stop after reaching any matches.
"
             command
             (string-titlecase (symbol->string command))
             (if remove? "removed" "preserved"))
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  '(((--help -help -h))
                    ((--eval -e))
                    ((--read-sexp -r))
                    ((--write-sexp -w))
                    ((--stop-after-matches -n) . matches)
                    ((--sre -S)))))
           (read-sexp? (get-opt '(--read-sexp -r) args flag?: #t))
           (write-sexp? (get-opt '(--write-sexp -w) args flag?: #t))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (use-eval? (get-opt '(--eval -e) args flag?: #t))
           (stop-after-matches (get-opt '(--stop-after-matches -n) args))
           (pattern (and-let* ((p (get-opt '(--) args)))
                      (and (not (null? p)) (car p)))))

      (handle-command-help command args)

      (when (and use-sre? use-eval?)
        (die! "~a: --sre and --eval cannot be used together." command))

      (unless pattern
        (die! "~a: missing pattern." command))

      (when stop-after-matches
        (let ((n (string->number stop-after-matches)))
          (if n
              (set! stop-after-matches n)
              (die! "~a: --stop-after-matches | -n: invalid argument: ~a."
                    command stop-after-matches))))

      (let* ((matches -1)
             (maybe-stop
              (lambda (line-or-sexp)
                (set! matches (add1 matches))
                (when (and stop-after-matches (>= matches stop-after-matches))
                  (exit 0))
                ((if write-sexp? write print) line-or-sexp))))
        (input-iterator
         read-sexp?
         (lambda (sexp lineno)
           (when ((if remove? not identity) (car (eval-scheme pattern '() sexp lineno)))
             (maybe-stop sexp)))
         (lambda (line lineno)
           (if use-eval?
               (when ((if remove? not identity) (car (eval-scheme pattern '() line lineno)))
                 (maybe-stop line))
               (when ((if remove? not identity)
                      (irregex-search
                       (if use-sre?
                           (with-input-from-string pattern read)
                           pattern)
                       line))
                 (maybe-stop (if write-sexp? (list line) line)))))))))))

(define-filter 'filter)
(define-filter 'remove)
