# sad

```
Usage: sad [-h|-help|--help] <command> [<options>]

sad -- Scheme-Aware Ducts ("sed makes me sad", or "sad makes me sed").

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

<command>s:

*
  Multiply items read from a list from the standard input assuming they can
  be converted to numbers.  Items that cannot be converted to number
  are discarded.

  `sad '*'` is equivalent to `sad apply '*' 'string->number'`

  Examples:

  $ echo 1 2 3 | sad split | sad '*'
  6

  # Factorial
  $ seq 10 | sad buffer | sad '*'
  3628800

+
  Add items read from a list from the standard input assuming they can
  be converted to numbers.  Items that cannot be converted to number
  are discarded.

  `sad +` is equivalent to `sad apply + 'string->number'`

  Examples:

  $ echo 1 2 3 | sad split | sad +
  6

  $ seq 10 | sad buffer | sad +
  55

apply <options> <op> [<converter>]
  Expect a Scheme list as input and apply <op> to the list.  <converter>
  defaults to `identity' and will be applied to all elements of the
  input list before the application of <op>.

  sad apply <op> <converter>

  is equivalent to

  sad eval -r '(print (apply <op> (map <converter> INPUT)))'

  <options>:
    --require-extension | -R <extension>
      Import a CHICKEN extension.  This parameter may be provided
      multiple times.

  Examples:

  $ echo 1 2 3 | sad split | sad apply + 'string->number'
  6

  # Factorial
  $ seq 10 | sad buffer | sad apply '*' 'string->number'
  3628800

buffer [<options>] [<number of lines>]
  Accumulate <number of lines> (a positive integer) and then dump
  them as sexps.  If <number of lines> is not provided, buffer all
  the input.

  <options>:
    --read-sexp | -r
      Assume inputs are sexps.

  Examples:

  $ seq 9 | sad buffer 3
  ("1" "2" "3")("4" "5" "6")("7" "8" "9")

  # Select even lines
  $ seq 6 | sad buffer 2 | sad cols -r 1 | sad join
  2
  4
  6

cols [<options>] <range> [<range> ...]
  Select columns based on ranges.  If --read-sexp is provided, the
  input is expected to be a Scheme list, otherwise a string will be
  read and split according to the pattern specified by --split-pattern
  (or a space, it if is not provided).

  Syntax of ranges:
  * `<number>': a single column whose index is <number>
  * `:<number>': columns from 0 to <number>
  * `<number>:': columns from <number> to the last column
  Negative indexes are supported.

  <options>:
    --delete | -d
      Delete columns in the given ranges.  Note that selection and
      deletion are mutually exclusive.

    --read-sexp | -r
      Assume inputs are sexps (if provided, output will be a Scheme list).

    --split-pattern | -s <pattern>
      Split input by <pattern>.  --read-sexp and --split-pattern are
      mutually exclusive.  If not provided, a space will be used.

    --sre | -S
      Indicate that <pattern> uses SRE syntax.

  Examples:

  $ echo 1 2 3 | sad cols 0
  1

  $ echo 1 2 3 | sad cols 2 0
  3 1

  $ echo 1 2 3 | sad cols 1:
  2 3

  $ echo 1 2 3 | sad cols -1
  3

  $ echo 1 2 3 | sad cols :-1
  1 2

  $ echo 1 2 3 | sad cols --delete 1
  1 3

  $ echo 1:2:3 | sad cols -s : 1
  2

  $ echo 1:2_3 | sad cols -S -s '(or ":" "_")' 1
  2

  $ echo 1 2 3 | sad split | sad cols -r 1 | sad join
  2

eval <options> <exp>
  Evaluate the Scheme expression <exp>.  The `INPUT' and `LINENO'
  variables are bound to the given input and to the line number in the
  evaluation context, respectively.  <exp> is implicitly placed in a
  `begin' form.  The chicken.irregex unit is implicitly imported in the
  evaluation context.

  <options>:
    --bind | -b <variable> <value>
      Bind <variable> to <value> in the execution context of <exp>.
      This parameter may be provided multiple times.

    --split-pattern <split pattern>
      Regular expression to be used to split columns.

    --sre | -S
      Indicate that regular expressions are in SRE syntax.

    --read-sexp | -r
      Assume inputs are sexps.

    --require-extension | -R <extension>
      Import a CHICKEN extension.  By default, chicken.irregex is
      imported.  This parameter may be provided multiple times.

    --finalizer | -f <exp>
      Scheme expression to be evaluated after the whole input has been
      consumed.

    --match | -m <pattern>
      Only apply <exp> to lines which match <pattern> (a regular
      expression).  Lines that do not match <pattern> are just printed.
      --match and --read-sexp are mutually exclusive.

    --not-match | -n <pattern>
      Only apply <exp> to lines which do NOT match <pattern> (a regular
      expression).  Lines that match <pattern> are just printed.
      --not-match and --read-sexp are mutually exclusive.

  Examples:

  # Reverse the order of lines (emulates 'tac')
  $ seq 3 | sad buffer | sad eval -r '(for-each print (reverse INPUT))'
  3
  2
  1

  # Print the number of columns in each line, followed by the line
  $ (echo a; echo a b c; echo a b) | sad eval '(print (length (COLS)) ":" INPUT)'
  1:a
  3:a b c
  2:a b

extract [<options>] <pattern>
  Extract strings matching <pattern> in the input.  <pattern> must use the
  syntax for grouping to indicate what must be extracted.  Produce a Scheme
  list of matches.  If named submatches are used, they are represented
  as pairs (<name> . <match>) in the output (only the first match of
  named matches are considered).

  <options>:
    --sre | -S
      Indicate that <pattern> uses SRE syntax.

  Examples:

  $ (echo Line 1; echo Line 2) |
    sad extract ".*([0-9])" |
    sad join
  1
  2

  $ (echo Line 1; echo Line 2) |
    sad extract --sre "(: (* any) (submatch num))" |
    sad join
  1
  2

  $ (echo Page 1, line 3; echo Page 2, line 9) |
     sad extract --sre '(: "Page " (submatch-named page num) ", line " (submatch-named line num))'
  ((line . "3") (page . "1"))((line . "9") (page . "2"))

filter [<options>] <pattern>
  filter lines matching <pattern> (a regular expression or a Scheme
  expression when --eval or --read-sexp is given).

  <options>:
    --delete | -d
      Delete lines that match <pattern>.

    --eval | -e
      Indicate that <pattern> is a Scheme expression to be evaluated.
      Lines that cause the evaluation of <pattern> to return non-#f
      are preserved (or deleted, if --delete is used).

    --bind | -b <variable> <value>
      Bind <variable> to <value> in the execution context of <pattern>.
      (when --eval is given). This parameter may be provided multiple
      times.

    --require-extension | -R <extension>
      Import a CHICKEN extension.  By default, chicken.irregex is
      imported.  This parameter may be provided multiple times and only
      makes sense when --eval is used.

    --finalizer | -f <exp>
      Scheme expression to be evaluated after the whole input has been
      consumed.

    --read-sexp | -r
      Assume inputs are sexps.  Implies --eval.

    --write-sexp | -w
      Write sexps.

    --sre | -S
      Indicate that regular expressions are in SRE syntax.

    --split-pattern <split pattern>
      Regular expression to be used to split columns in lines when --eval
      is used.

    --stop-after-matches | -n <num matches>
      Stop after reaching any matches.

  Examples:

  # Remove empty lines
  $ printf 'a\n\nb\n\nc' | sad filter -d ^$
  a
  b
  c

  # Print lines with 3 or more characters
  $ (echo aa; echo a; echo aaa; echo aaaa) | sad filter '.{3}'
  aaa
  aaaa

format <format>
  Format items of the input the specified format.  The input must
  be a Scheme list which will be given, alongside with the <format>
  string, as argument to the `format' procedure of the `format' egg.

  Example:

  $ echo 12 12 12 12 12 12 12 |
    sad buffer -r |
    sad format 'dec: ~a bin: ~B octal: ~O hexa: ~X roman: ~@R ord: ~R~%'
  dec: 12 bin: 1100 octal: 14 hexa: c roman: XII ord: twelve

join [<options>] [<joiner>]
  Join fields in the input with <joiner>.  If <joiner> is not provided,
  a space will be used.  Input is expected to be Scheme lists.

  <options>:
    --translate-escapes | -e
      Translate escaped characters into their corresponding control
      characters.  The following ones are supported:
      * \0 => null
      * \n => newline
      * \t => tab

  Examples:

  $ echo 1 2 3 | sad split | sad join
  1 2 3

  $ echo 1 2 3 | sad split | sad join :
  1:2:3

  $ echo 1 2 3 | sad split | sad join -e '\n'
  1
  2
  3

lines <range> [<range> ...]
  Select lines by number or range.  Syntax of ranges:
  * `<number>': a single line whose index is <number>
  * `:<number>': lines from 0 to <number>
  * `<number>:': lines from <number> to the last line
  Negative indexes are supported.

  <options>:
    --delete | -d
      Delete the lines in the given ranges.

    --read-sexp | -r
      Assume inputs are sexps.

  Examples:

  $ seq 10 | sad lines 4:7
  5
  6
  7

  $ seq 10 | sad lines -1
  10

  $ seq 10 | sad lines 1:-7
  2
  3

  $ seq 10 | sad lines 7:
  8
  9
  10

map <options> <op> [<converter>]
  Expect a Scheme list as input and apply <op> to the items of the list.
  <converter> defaults to `identity' and will be applied to all elements
  of the input list before the application of <op>.

  sad map <op> <converter>

  is equivalent to

  sad eval -r '(write (map <op> (map <converter> INPUT)))'

  <options>:
    --require-extension | -R <extension>
      Import a CHICKEN extension.  This parameter may be provided
      multiple times.

  Examples:

  $ echo 1 2 3 | sad split | sad map '(lambda (i) (string-append "Item " i))'
  ("Item 1" "Item 2" "Item 3")

  $ echo 1 2 3 | sad split | sad map add1 'string->number'
  (2 3 4)

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
      just printed.

    --translate-escapes | -e
      Translate escaped characters into their corresponding control
      characters.  The following ones are supported:
      * \0 => null
      * \n => newline
      * \t => tab

  Examples:

  $ echo Hello, world | sad replace Hello, "Bye, cruel"
  Bye, cruel world

  $ (echo foo; echo bar; echo baz) | sad replace --sre '(or "r" "z")' g
  foo
  bag
  bag

sort [<options>]
  Sort the input, which is expected to be sexps.  When no option
  is provided, it sorts the car of the inputs according to
  `string<?'. Its input is typically the output of the buffer
  command.

  <options>
    --column | -c <colnum>
      Sort using column <colnum>.

    --criterion | -C <criterion>
      Sort according to <criterion>.  Available ones are:
      * numeric (columns will be implicitly converted to numbers)
      * alphabetic (default)
      * natural (using `natural-string<?' from the natural-sort egg)
      If --eval is provided, use <criterion> as a sexp to be evaluated.

    --reverse | -r
      Reverse results.

    --eval | -e
      Indicate that <criterion> for --criterion is a sexp.

split [<options>] [<pattern>]
  Split the input according to <pattern> (a regular expression) and
  output a Scheme list of strings.  If <pattern> is not provided,
  split on spaces and tabs.

  <options>:
    --sre | -S
      Indicate that the argument to --pattern is an SRE.

  Examples:

  $ echo 1 2 3 | sad split
  ("1" "2" "3")

  $ echo 1_2_3 | sad split _
  ("1" "2" "3")

  $ echo 1_2:3 | sad split --sre '(or "_" ":")'
  ("1" "2" "3")

tabularize
  Tabularize the input, which will be read as a list of lists.

    <options>:
    --padding | -p <num spaces>
      Number of spaces to print around table items.

    --borderless | -B
      Draw tables without borders.

  Examples:

  $ seq 9 | sad buffer 3 | sad tabularize
  ┌───────────┐
  │ 1 │ 2 │ 3 │
  │ 4 │ 5 │ 6 │
  │ 7 │ 8 │ 9 │
  └───────────┘

  $ seq 9 | sad buffer 3 | sad tabularize --borderless
  1  2  3
  4  5  6
  7  8  9

  $ cat /etc/passwd | sad lines 0:3 | sad split : | sad tabularize
  ┌─────────────────────────────────────────────────────────────┐
  │ root   │ x │ 0 │ 0 │ root   │ /root     │ /bin/bash         │
  │ daemon │ x │ 1 │ 1 │ daemon │ /usr/sbin │ /usr/sbin/nologin │
  │ bin    │ x │ 2 │ 2 │ bin    │ /bin      │ /usr/sbin/nologin │
  └─────────────────────────────────────────────────────────────┘
```

## Extending sad

`sad` loads `$HOME/.sad.conf` at start up.  There you can define your
own commands.

Example:

```
$ cat ~/.sad.conf
(import commands)

(define-command 'hello
  "hello
     Print hello."
  (lambda ()
    (print "hello")))

$ sad hello -h
hello
     Print hello.
```
