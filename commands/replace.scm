(define-command 'replace "\
replace [<options>] <pattern> <replacement>
  Replace strings matching <pattern> with <replacement> in the input.

  <options>:
    --sre | -S
      Indicate that <pattern> uses SRE syntax.

    --all | -a
      Replace all occurrences of <pattern>

    --match | -m <match pattern>
      Only replace in lines which match <match pattern> (a regular
      expression).  Lines that do not match <match pattern> are just
      printed.

    --not-match | -n <not match pattern>
      Only replace in lines which which do NOT match <not match pattern>
      (a regular expression).  Lines that match <not match pattern> are
      just printed."
  (lambda (args*)
    (let* ((args (parse-command-line
                  args*
                  `(((--all -a))
                    ((--sre -S))
                    ((--match -m) . p)
                    ((--not-match -n) . p))))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
           (match-pattern% (get-opt '(--match -m) args))
           (not-match-pattern% (get-opt '(--not-match -n) args))
           (match-pattern
            (and match-pattern%
                 (if use-sre?
                     (with-input-from-string match-pattern% read)
                     (string->sre match-pattern%))))
           (not-match-pattern
            (and not-match-pattern%
                 (if use-sre?
                     (with-input-from-string not-match-pattern% read)
                     (string->sre not-match-pattern%))))
           (replace-all? (get-opt '(--all -a) args flag?: #t))
           (pattern/replacement (get-opt '(--) args)))

      (unless (= (length pattern/replacement) 2)
        (die! "replace: invalid <pattern> <replacement> specification."))

      (let ((pattern
             (if use-sre?
                 (with-input-from-string (car pattern/replacement) read)
                 (car pattern/replacement)))
            (replacement (cadr pattern/replacement))
            (replacer (if replace-all? irregex-replace/all irregex-replace)))
      (for-each-line
       (lambda (line lineno)
         (if (or (and (not match-pattern) (not not-match-pattern))
                 (and match-pattern (irregex-search match-pattern line))
                 (and not-match-pattern
                      (not (irregex-search not-match-pattern line))))
             (print (replacer pattern line replacement))
             (print line))))))))
