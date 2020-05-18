(define-command 'replace
  "\
replace [<options>] <pattern> <replacement>
  Replace strings matching <pattern> with <replacement> in the input.

<options>:
  --sre | -S
    Indicate that <pattern> uses SRE syntax.

  --all | -a
    Replace all occurrences of <pattern>
"
  (lambda args*
    (let* ((args (parse-command-line
                  args*
                  `(((--help -help -h))
                    ((--all -a))
                    ((--sre -S)))))
           (use-sre? (get-opt '(--sre -S) args flag?: #t))
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
         (print (replacer pattern line replacement))))))))
