;; HANDY ONE-LINE SCRIPTS FOR AWK                             21 August 2019
;; Compiled by Eric Pement                                      version 0.28
;;
;; Latest version of this file (in English) is usually at:
;;    http://www.pement.org/awk/awk1line.txt
;;
;; This file is also available in other languages:
;;    Chinese  - http://www.pement.org/awk/awk1line_zh-CN.txt
;;
;; USAGE:
;;
;;    Unix: awk '/pattern/ {print "$1"}'    # standard Unix shells
;; DOS/Win: awk '/pattern/ {print "$1"}'    # compiled with DJGPP, Cygwin
;;          awk "/pattern/ {print \"$1\"}"  # GnuWin32, UnxUtils, Mingw
;;
;;
;; Notice to Windows users: The examples in this file use 'single quotes'
;; and are intended for use with a shell like bash or ksh. If you use a
;; command interpreter such as CMD.EXE or TCC.EXE, single quotes will not
;; work. You must use "double quotes" instead. Furthermore, Microsoft
;; Windows uses the percent sign (%) to mark environment variables, so
;; this symbol must be doubled (%%) to yield a single percent sign visible
;; to awk compiled for Windows or MS-DOS. (Again, this warning does not
;; apply if your shell is bash, ksh, or a standard Unix shell.)
;;
;; I learned awk in a DOS environment, so some of the notes warn of DOS
;; and Unix newline issues. "\n" represents a newline (LF) in awk.
;;
;; There are 4 versions of awk in common use: original awk (1977), new awk
;; or nawk (1985), mawk (a variant of new awk), and GNU awk or gawk (still
;; actively maintained). These scripts will run under nawk, the most common
;; version in the Unix world.
;;
;; If an example runs only on GNU awk, the command 'gawk' will be used. I
;; consider a one-line script to be 65 characters or less. Shorter scripts
;; usually appear first. Finally, I normally use '1' instead of '{print}'
;; to print a line of output. Let me know of any errors you find.
;;
;;
;; FILE SPACING:
(test-begin "FILE SPACING")

;;  # double space a file
;;  awk '1;{print ""}'
;;  awk 'BEGIN{ORS="\n\n"};1'
(test-sad "double space a file"
          "seq 3 | awk 'BEGIN{ORS=\"\\n\\n\"};1'"
          "seq 3 | sad eval '(print INPUT \"\n\")'")

;;  # double space a file which already has blank lines in it. Output file
;;  # should contain no more than one blank line between lines of text.
;;  # NOTE: On Unix systems, DOS lines which have only CRLF (\r\n) are
;;  # often treated as non-blank, and thus 'NF' alone will return TRUE.
;;  awk 'NF{print $0 "\n"}'
(test-sad "double space a file which already has blank lines in it"
          "printf 'a\n\nb\n\nc' | awk 'NF{print $0 \"\\n\"}'"
          "printf 'a\n\nb\n\nc' | sad filter -d ^$ | sad eval '(print INPUT \"\n\")'")

;;  # triple space a file
;;  awk '1;{print "\n"}'
(test-sad "triple space a file"
          "seq 3 | awk '1;{print \"\\n\"}'"
          "seq 3 | sad eval '(print INPUT \"\n\n\")'")

(test-end "FILE SPACING")

;; NUMBERING AND CALCULATIONS:
(test-begin "NUMBERING AND CALCULATIONS")

;;  # precede each line by its line number FOR THAT FILE (left alignment).
;;  # Using a tab (\t) instead of space will preserve margins.
;;  awk '{print FNR "\t" $0}' files*
(test-sad "precede each line by its line number"
          "seq 3 | awk '{print FNR \"\\t\" $0}'"
          "seq 3 | sad eval '(print (add1 LINENO) \"\t\" INPUT)'")

;;  # precede each line by its line number FOR ALL FILES TOGETHER, with tab.
;;  awk '{print NR "\t" $0}' files*
;;
;;  # number each line of a file (number on left, right-aligned)
;;  # Double the percent signs if typing from the DOS command prompt.
;;  awk '{printf("%5d : %s\n", NR,$0)}'
(test-sad "number each line of a file (number on left, right-aligned)"
          "seq 3 | awk '{printf(\"%5d : %s\\n\", NR,$0)}'"
          "seq 3 | sad eval -R format -r '(format #t \"~5d : ~a~%\" (add1 LINENO) INPUT)'")

;;  # number each line of file, but only print numbers if line is not blank
;;  # Remember caveats about Unix treatment of \r (mentioned above)
;;  awk 'NF{$0=++a " :" $0};1'
;;  awk '{print (NF? ++a " :" :"") $0}'

(test-sad "number each line of file, but only print numbers if line is not blank"
          "seq 3 | awk 'BEGIN{ORS=\"\\n\\n\"};1' | awk 'NF{$0=++a \" :\" $0};1'"
          "seq 3 | awk 'BEGIN{ORS=\"\\n\\n\"};1' | \
           sad eval -b num 0 '(if (equal? INPUT \"\") (print) (begin (set! num (add1 num)) (print num \" :\" INPUT)))'")

;;  # count lines (emulates "wc -l")
;;  awk 'END{print NR}'
(test-sad "count lines (emulates 'wc -l')"
          "seq 3 | awk 'END{print NR}'"
          "seq 3 | sad eval void -f '(print LINENO)'")

;;  # print the sums of the fields of every line
;;  awk '{s=0; for (i=1; i<=NF; i++) s=s+$i; print s}'
(test-sad "print the sums of the fields of every line"
          "seq 3 | awk '{s=0; for (i=1; i<=NF; i++) s=s+$i; print s}'"
          "seq 3 | sad eval '(print (apply + (map string->number (COLS))))'")
;;
;;  # add all fields in all lines and print the sum
;;  awk '{for (i=1; i<=NF; i++) s=s+$i}; END{print s}'
(test-sad "add all fields in all lines and print the sum"
          "seq 3 | awk '{for (i=1; i<=NF; i++) s=s+$i}; END{print s}'"
          "seq 3 | sad split | sad buffer -r | sad apply append | sad +")

(test-sad "add all fields in all lines and print the sum"
          "seq 3 | awk '{for (i=1; i<=NF; i++) s=s+$i}; END{print s}'"
          "seq 3 | sad eval -b s 0 '(set! s (+ s (apply + (map string->number (COLS)))))' -f '(print s)'")

;;  # print every line after replacing each field with its absolute value
;;  awk '{for (i=1; i<=NF; i++) if ($i < 0) $i = -$i; print }'
;;  awk '{for (i=1; i<=NF; i++) $i = ($i < 0) ? -$i : $i; print }'
(test-sad "print every line after replacing each field with its absolute value"
          "seq -2 0 | awk '{for (i=1; i<=NF; i++) if ($i < 0) $i = -$i; print }'"
          "seq -2 0 | sad eval -R chicken.string \
                         '(print (string-intersperse (map (compose number->string abs string->number) (COLS))))'")

;;  # print the total number of fields ("words") in all lines
;;  awk '{ total = total + NF }; END {print total}' file
(test-sad "print the total number of fields ('words') in all lines (1)"
          "(echo a b c; echo 1 2) | awk '{ total = total + NF }; END {print total}'"
          "(echo a b c; echo 1 2) | sad split | sad eval -r -b total 0 '(set! total (+ total (length INPUT)))' -f '(print total)'")

(test-sad "print the total number of fields ('words') in all lines (2)"
          "(echo a b c; echo 1 2) | awk '{ total = total + NF }; END {print total}'"
          "(echo a b c; echo 1 2) | sad eval -b total 0 '(set! total (+ total (length (COLS))))' -f '(print total)'")

;;  # print the total number of lines that contain "Beth"
;;  awk '/Beth/{n++}; END {print n+0}' file
(test-sad "print the total number of lines that contain 'Beth'"
          "(echo Beth; echo foo) | awk '/Beth/{n++}; END {print n+0}'"
          "(echo Beth; echo foo) | sad filter Beth | sad buffer | sad eval -r '(print (length INPUT))'")

;;  # print the largest first field and the line that contains it
;;  # Intended for finding the longest string in field #1
;;  awk '$1 > max {max=$1; maxline=$0}; END{ print max, maxline}'

;; FIXME: improve
(test-sad "print the largest first field and the line that contains it"
          "(echo a; echo abc; echo ab) | awk '$1 > max {max=$1; maxline=$0}; END{ print max, maxline}'"
          "(echo a; echo abc; echo ab) |\
            sad eval -b maxline \\\"\\\" -b max \\\"\\\" \
            '(when (> (string-length (COLS 0 default: \"\")) (string-length maxline)) \
               (set! maxline INPUT) (set! max (COLS 0)))' \
            -f '(print max \" \" maxline)'")

;;  # print the number of fields in each line, followed by the line
;;  awk '{ print NF ":" $0 } '
(test-sad "print the number of fields in each line, followed by the line"
          "(echo a; echo a b c; echo a b) | awk '{ print NF \":\" $0 }'"
          "(echo a; echo a b c; echo a b) | sad eval '(print (length (COLS)) \":\" INPUT)'")

;;  # print the last field of each line
;;  awk '{ print $NF }'
(test-sad "print the last field of each line (1)"
          "(echo a; echo a b c; echo a b) | awk '{ print $NF }'"
          "(echo a; echo a b c; echo a b) | sad cols -1")

(test-sad "print the last field of each line (2)"
          "(echo a; echo a b c; echo a b) | awk '{ print $NF }'"
          "(echo a; echo a b c; echo a b) | sad split | sad eval -r -R srfi-1 '(unless (null? INPUT) (print (last INPUT)))'")

(test-sad "print the last field of each line (3)"
          "(echo a; echo a b c; echo a b) | awk '{ print $NF }'"
          "(echo a; echo a b c; echo a b) | sad eval '(print (COLS -1))'")

;;  # print the last field of the last line
;;  awk '{ field = $NF }; END{ print field }'
(test-sad "print the last field of the last line"
          "(echo a; echo a b c; echo a b) | awk '{ field = $NF }; END{ print field }'"
          "(echo a; echo a b c; echo a b) | sad lines -1 | sad cols -1")

;;  # print every line with more than 4 fields
;;  awk 'NF > 4'
(test-sad "print every line with more than 4 fields"
          "(echo a; echo a b c d e; echo a b) | awk 'NF > 4'"
          "(echo a; echo a b c d e; echo a b) | sad eval '(when (> (length (COLS)) 4) (print INPUT))'")

;;  # print every line where the value of the last field is > 4
;;  awk '$NF > 4'
(test-sad "print every line where the value of the last field is > 4"
          "(echo 1 3; echo 3 5) | awk '$NF > 4'"
          "(echo 1 3; echo 3 5) |\
           sad eval '(when (> (COLS -1 default: 0 conv: string->number) 4) (print INPUT))'")

(test-end "NUMBERING AND CALCULATIONS")


(test-begin "STRING CREATION")

;; STRING CREATION:
;;
;;  # create a string of a specific length (e.g., generate 513 spaces)
;;  awk 'BEGIN{while (a++<513) s=s " "; print s}'

(test-sad "create a string of a specific length (e.g., generate 513 spaces)"
          "awk 'BEGIN{while (a++<513) s=s \" \"; print s}'"
          "echo | sad eval '(print (make-string 513 #\\space))'")

;;  # insert a string of specific length at a certain character position
;;  # Example: insert 49 spaces after column #6 of each input line.
;;  gawk --re-interval 'BEGIN{while(a++<49)s=s " "};{sub(/^.{6}/,"&" s)};1'
;;
;; TODO

(test-end "STRING CREATION")

;;
;; ARRAY CREATION:
;;
;;  # These next 2 entries are not one-line scripts, but the technique
;;  # is so handy that it merits inclusion here.
;;
;;  # create an array named "month", indexed by numbers, so that month[1]
;;  # is 'Jan', month[2] is 'Feb', month[3] is 'Mar' and so on.
;;  split("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec", month, " ")
;;
;;  # create an array named "mdigit", indexed by strings, so that
;;  # mdigit["Jan"] is 1, mdigit["Feb"] is 2, etc. Requires "month" array
;;  for (i=1; i<=12; i++) mdigit[month[i]] = i
;;

(test-begin "TEXT CONVERSION AND SUBSTITUTION")

;; TEXT CONVERSION AND SUBSTITUTION:
;;
;;  # IN UNIX ENVIRONMENT: convert DOS newlines (CR/LF) to Unix format
;;  awk '{sub(/\r$/,"")};1'   # assumes EACH line ends with Ctrl-M

(test-sad "convert DOS newlines (CR/LF) to Unix format"
          "printf 'foo\r\nbar\r\nbaz' | awk '{sub(/\\r$/,\"\")};1'"
          "printf 'foo\r\nbar\r\nbaz' | sad replace '\\r$' ''")
;;
;;  # IN UNIX ENVIRONMENT: convert Unix newlines (LF) to DOS format
;;  awk '{sub(/$/,"\r")};1'

(test-sad "convert Unix newlines (LF) to DOS format"
          "printf 'foo\nbar\nbaz' | awk '{sub(/$/,\"\\r\")};1'"
          "printf 'foo\nbar\nbaz' | sad replace '$' '\r'")

;;  # IN DOS ENVIRONMENT: convert Unix newlines (LF) to DOS format
;;  awk 1
;;
;;  # IN DOS ENVIRONMENT: convert DOS newlines (CR/LF) to Unix format
;;  # Cannot be done with DOS versions of awk, other than gawk:
;;  gawk -v BINMODE="w" '1' infile >outfile
;;
;;  # Use "tr" instead.
;;  tr -d \r <infile >outfile            # GNU tr version 1.22 or higher
;;
;;  # delete leading whitespace (spaces, tabs) from front of each line
;;  # aligns all text flush left
;;  awk '{sub(/^[ \t]+/, "")};1'

(test-sad "delete leading whitespace (spaces, tabs) from front of each line"
          "(printf '   abc\n'; printf '\txyz') | awk '{sub(/^[ \t]+/, \"\")};1'"
          "(printf '   abc\n'; printf '\txyz') | sad replace '^[ \t]+' ''")

;;  # delete trailing whitespace (spaces, tabs) from end of each line
;;  awk '{sub(/[ \t]+$/, "")};1'

(test-sad "delete trailing whitespace (spaces, tabs) from end of each line"
          "(printf 'abc    \n'; printf 'xyz\t') | awk '{sub(/[ \t]+$/, \"\")};1'"
          "(printf 'abc    \n'; printf 'xyz\t') | sad replace '[ \t]+$' ''")

;;  # delete BOTH leading and trailing whitespace from each line
;;  awk '{gsub(/^[ \t]+|[ \t]+$/,"")};1'
;;  awk '{$1=$1};1'           # also removes extra space between fields

(test-sad "delete BOTH leading and trailing whitespace from each line"
          "(printf '  abc  \n'; printf '\txyz\t') | awk '{gsub(/^[ \t]+|[ \t]+$/,\"\")};1'"
          "(printf '  abc  \n'; printf '\txyz\t') | sad replace --all '^[ \t]+|[ \t]+$' ''")

;;  # insert 5 blank spaces at beginning of each line (make page offset)
;;  awk '{sub(/^/, "     ")};1'

(test-sad "insert 5 blank spaces at beginning of each line (make page offset)"
          "seq 3 | awk '{sub(/^/, \"     \")};1'"
          "seq 3 | sad replace '^' '     '")

;;  # align all text flush right on a 79-column width
;;  awk '{printf "%79s\n", $0}' file*
;;
;; TODO
;;
;;  # center all text on a 79-character width
;;  awk '{l=length();s=int((79-l)/2); printf "%"(s+l)"s\n",$0}' file*
;;
;; TODO

;;  # substitute (find and replace) "foo" with "bar" on each line
;;  awk '{sub(/foo/,"bar")}; 1'           # replace only 1st instance
;;  gawk '{$0=gensub(/foo/,"bar",4)}; 1'  # replace only 4th instance
;;  awk '{gsub(/foo/,"bar")}; 1'          # replace ALL instances in a line

(test-sad "substitute (find and replace) 'foo' with 'bar' on each line"
          "(printf 'foomatic\n'; printf 'afoo\n') | awk '{sub(/foo/,\"bar\")}; 1'"
          "(printf 'foomatic\n'; printf 'afoo\n') | sad replace foo bar")

;;  # substitute "foo" with "bar" ONLY for lines which contain "baz"
;;  awk '/baz/{gsub(/foo/, "bar")}; 1'

(test-sad "substitute 'foo' with 'bar' ONLY for lines which contain 'baz'"
          "(printf 'foo bar\n'; printf 'foo baz\n') | awk '/baz/{gsub(/foo/, \"bar\")}; 1'"
          "(printf 'foo bar\n'; printf 'foo baz\n') | sad replace --match baz foo bar")

;;  # substitute "foo" with "bar" EXCEPT for lines which contain "baz"
;;  awk '!/baz/{gsub(/foo/, "bar")}; 1'

(test-sad "substitute 'foo' with 'bar' EXCEPT for lines which contain 'baz'"
          "(printf 'foo bar\n'; printf 'foo baz\n') | awk '!/baz/{gsub(/foo/, \"bar\")}; 1'"
          "(printf 'foo bar\n'; printf 'foo baz\n') | sad replace --not-match baz foo bar")

;;  # change "scarlet" or "ruby" or "puce" to "red"
;;  awk '{gsub(/scarlet|ruby|puce/, "red")}; 1'

(test-sad "change 'scarlet' or 'ruby' or 'puce' to 'red'"
          "(printf 'foo scarlet\n'; printf 'puce baz\n') | awk '{gsub(/scarlet|ruby|puce/, \"red\")}; 1'"
          "(printf 'foo scarlet\n'; printf 'puce baz\n') | sad replace 'scarlet|ruby|puce' red")

;;  # reverse order of lines (emulates "tac")
;;  awk '{a[i++]=$0} END {for (j=i-1; j>=0;) print a[j--] }' file*

(test-sad "reverse order of lines (emulates 'tac')"
          "seq 3 | awk '{a[i++]=$0} END {for (j=i-1; j>=0;) print a[j--] }'"
          "seq 3 | sad buffer | sad eval -r '(for-each print (reverse INPUT))'")
;;
;;  # if a line ends with a backslash, append the next line to it (fails if
;;  # there are multiple lines ending with backslash...)
;;  awk '/\\$/ {sub(/\\$/,""); getline t; print $0 t; next}; 1' file*
(test-sad "if a line ends with a backslash, append the next line to it"
          "(printf 'foo scarlet \\\n'; printf 'nuce baz') |\
           awk '/\\\\$/ {sub(/\\\\$/,\"\"); getline t; print $0 t; next}; 1'"
          "(printf 'foo scarlet \\\n'; printf 'nuce baz') |\
           sad buffer 2 |\
           sad eval -r -R srfi-13 \
             '(if (string-suffix? \"\\\\\" (car INPUT))\
                (print (string-append (string-drop-right (car INPUT) 1) (cadr INPUT)))\
                (for-each print INPUT))'")

;;  # print and sort the login names of all users
;;  awk -F ":" '{print $1 | "sort" }' /etc/passwd
(test-sad "print and sort the login names of all users"
          "cat /etc/passwd | awk -F ':' '{print $1 | \"sort\" }'"
          "cat /etc/passwd | sad cols -s : 0 | sort")

;;  # print the first 2 fields, in opposite order, of every line
;;  awk '{print $2, $1}' file
(test-sad "print the first 2 fields, in opposite order, of every line"
          "(echo 1 2; echo 3 4) | awk '{print $2, $1}'"
          "(echo 1 2; echo 3 4) | sad cols 1 0")

;;  # switch the first 2 fields of every line
;;  awk '{temp = $1; $1 = $2; $2 = temp}' file
;;
;;  # print every line, deleting the second field of that line
;;  awk '{ $2 = ""; print }'
(test-sad "print every line, deleting the second field of that line"
          "(echo 1 2; echo 3 4) | awk '{ $2 = \"\"; print }'"
          "(echo 1 2; echo 3 4) | sad cols 0 | sad replace '$' ' '")

;;  # print in reverse order the fields of every linew
;;  awk '{for (i=NF; i>0; i--) printf("%s ",$i);print ""}' file
(test-sad "print in reverse order the fields of every line"
          "(echo 1 2; echo 3 4) | awk '{for (i=NF; i>0; i--) printf(\"%s \",$i);print \"\"}'"
          "(echo 1 2; echo 3 4) |\
           sad split |\
           sad eval -r -R chicken.string '(print (string-intersperse (reverse (COLS))))' |\
           sad replace '$' ' '")

;;  # concatenate every 5 lines of input, using a comma separator
;;  # between fields
;;  awk 'ORS=NR%5?",":"\n"' file
(test-sad "concatenate every 5 lines of input, using a comma separator between fields"
          "seq 10 | awk 'ORS=NR%5?\",\":\"\\n\"'"
          "seq 10 | sad buffer 5 | sad eval -r -R chicken.string '(print (string-intersperse INPUT \",\"))'")

(test-end "TEXT CONVERSION AND SUBSTITUTION")


(test-begin "SELECTIVE PRINTING OF CERTAIN LINES")
;; SELECTIVE PRINTING OF CERTAIN LINES:
;;
;;  # print first 10 lines of file (emulates behavior of "head")
;;  awk 'NR < 11'
(test-sad "print first 10 lines of file (emulates behavior of 'head')"
          "seq 20 | awk 'NR < 11'"
          "seq 20 | sad lines :10")

;;  # print first line of file (emulates "head -1")
;;  awk 'NR>1{exit};1'
(test-sad "print first line of file (emulates 'head -1')"
          "seq 5 | awk 'NR>1{exit};1'"
          "seq 5 | sad lines 0")

;;   # print the last 2 lines of a file (emulates "tail -2")
;;  awk '{y=x "\n" $0; x=$0};END{print y}'
(test-sad "print the last 2 lines of a file (emulates 'tail -2')"
          "seq 5 | awk '{y=x \"\\n\" $0; x=$0};END{print y}'"
          "seq 5 | sad lines -2:")

;;  # print the last line of a file (emulates "tail -1")
;;  awk 'END{print}'
(test-sad "print the last line of a file (emulates 'tail -1')"
          "seq 5 | awk 'END{print}'"
          "seq 5 | sad lines -1:")

;;  # print only lines which match regular expression (emulates "grep")
;;  awk '/regex/'
(test-sad "print only lines which match regular expression (emulates 'grep')"
          "(echo foo; echo bar) | awk '/bar/'"
          "(echo foo; echo bar) | sad filter bar")

;;  # print only lines which do NOT match regex (emulates "grep -v")
;;  awk '!/regex/'
(test-sad "print only lines which do NOT match regex (emulates 'grep -v')"
          "(echo foo; echo bar) | awk '!/bar/'"
          "(echo foo; echo bar) | sad filter -d bar")

;;  # print any line where field #5 is equal to "abc123"
;;  awk '$5 == "abc123"'
(test-sad "print any line where field #5 is equal to 'abc123'"
          "(echo '1 2 3 4 abc123'; echo '1 2 3 4 5') | awk '$5 == \"abc123\"'"
          "(echo '1 2 3 4 abc123'; echo '1 2 3 4 5') |\
           sad eval '(when (equal? (COLS 4) \"abc123\") (print INPUT))'")

;;  # print only those lines where field #5 is NOT equal to "abc123"
;;  # This will also print lines which have less than 5 fields.
;;  awk '$5 != "abc123"'
;;  awk '!($5 == "abc123")'
(test-sad "print any line where field #5 is NOT equal to 'abc123'"
          "(echo '1 2 3 4 abc123'; echo '1 2 3 4 5') | awk '$5 != \"abc123\"'"
          "(echo '1 2 3 4 abc123'; echo '1 2 3 4 5') |\
           sad eval '(unless (equal? (COLS 4) \"abc123\") (print INPUT))'")

;;  # matching a field against a regular expression
;;  awk '$7  ~ /^[a-f]/'    # print line if field #7 matches regex
(test-sad "print line if field #7 matches regex"
          "(echo '1 2 3 4 5 6 a'; echo '1 2 3 4 5') | awk '$7 ~ /^[a-f]/'"
          "(echo '1 2 3 4 5 6 a'; echo '1 2 3 4 5') |\
           sad eval '(when (irregex-match \"^[a-f]\" (COLS 6)) (print INPUT))'")

;;  awk '$7 !~ /^[a-f]/'    # print line if field #7 does NOT match regex
(test-sad "print line if field #7 does NOT match regex"
          "(echo '1 2 3 4 5 6 a'; echo '1 2 3 4 5') | awk '$7 !~ /^[a-f]/'"
          "(echo '1 2 3 4 5 6 a'; echo '1 2 3 4 5') |\
           sad eval '(unless (irregex-match \"^[a-f]\" (COLS 6)) (print INPUT))'")

;;  # print the line immediately before a regex, but not the line
;;  # containing the regex
;;  awk '/regex/{print x};{x=$0}'
;;  awk '/regex/{print (NR==1 ? "match on line 1" : x)};{x=$0}'
(test-sad "print the line immediately before a regex, but not the line containing the regex"
          "(echo foo; echo bar; echo afoo) | awk '/foo/{print (NR==1 ? \"match on line 1\" : x)};{x=$0}'"
          "(echo foo; echo bar; echo afoo) |\
           sad eval -b prev '#f' \
            '(when (irregex-search \"foo\" INPUT) (print (or prev \"match on line 1\"))) (set! prev INPUT)'")

;;  # print the line immediately after a regex, but not the line
;;  # containing the regex
;;  awk '/regex/{getline;print}'
(test-sad "print the line immediately after a regex, but not the line containing the regex"
          "(echo foo; echo bar; echo afoo) | awk '/foo/{if(getline) print}'"
          "(echo foo; echo bar; echo afoo) |\
           sad buffer 2 |\
           sad eval -r '(when (and (not (null? (cdr INPUT))))\
                          (irregex-search \"foo\" (car INPUT)) (print (cadr INPUT)))'")

;;  # grep for AAA and BBB and CCC (in any order on the same line)
;;  awk '/AAA/ && /BBB/ && /CCC/'
(test-sad "grep for AAA and BBB and CCC (in any order on the same line)"
          "(echo 1 2 3; echo BBB AAA CCC) | awk '/AAA/ && /BBB/ && /CCC/'"
          "(echo 1 2 3; echo BBB AAA CCC) |\
           sad eval -R srfi-1 \
           '(when (every (lambda (p) (irregex-search p INPUT))\
              (quote (\"AAA\" \"BBB\" \"CCC\"))) (print INPUT))'")

;;  # grep for AAA and BBB and CCC (in that order)
;;  awk '/AAA.*BBB.*CCC/'
(test-sad "grep for AAA and BBB and CCC (in that order)"
          "(echo 1 2 3; echo AAA BBB CCC) | awk '/AAA.*BBB.*CCC/'"
          "(echo 1 2 3; echo AAA BBB CCC) | sad filter 'AAA.*BBB.*CCC'")

;;  # print only lines of 65 characters or longer
;;  awk 'length > 64'
(test-sad "print only lines of 65 characters or longer"
          "(echo a; echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) |\
           awk 'length > 64'"
          "(echo a; echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) |\
          sad filter '.{65}'")

;;  # print only lines of less than 65 characters
;;  awk 'length < 64'
(test-sad "print only lines of less than 65 characters"
          "(echo a; echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) |\
           awk 'length < 64'"
          "(echo a; echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) |\
          sad filter -d '.{64}'")

;;  # print section of file from regular expression to end of file
;;  awk '/regex/,0'
;;  awk '/regex/,EOF'

;; TODO

;;  # print section of file based on line numbers (lines 8-12, inclusive)
;;  awk 'NR==8,NR==12'
(test-sad "print section of file based on line numbers (lines 8-12, inclusive)"
          "seq 20 | awk 'NR==8,NR==12'"
          "seq 20 | sad lines 7:12")

;;  # print line number 52
;;  awk 'NR==52'
;;  awk 'NR==52 {print;exit}'          # more efficient on large files
(test-sad "print line number 52"
          "seq 70 | awk 'NR==52'"
          "seq 70 | sad lines 51")

;;  # print section of file between two regular expressions (inclusive)
;;  awk '/Iowa/,/Montana/'             # case sensitive

;; TODO

(test-end "SELECTIVE PRINTING OF CERTAIN LINES")


(test-begin "SELECTIVE DELETION OF CERTAIN LINES")
;; SELECTIVE DELETION OF CERTAIN LINES:
;;
;;  # delete ALL blank lines from a file (same as "grep '.' ")
;;  awk NF
;;  awk '/./'
(test-sad "delete ALL blank lines from a file (same as \"grep '.' \")"
          "(echo a; echo; echo b) | awk NF"
          "(echo a; echo; echo b) | sad filter .")

;;  # remove duplicate, consecutive lines (emulates "uniq")
;;  awk 'a !~ $0; {a=$0}'
(test-sad "remove duplicate, consecutive lines (emulates 'uniq')"
          "(echo a; echo a; echo b) | awk 'a !~ $0; {a=$0}'"
          "(echo a; echo a; echo b) | \
           sad eval -b prev '#f' '(unless (equal? INPUT prev) (print INPUT)) (set! prev INPUT)'")

;;  # remove duplicate, nonconsecutive lines
;;  awk '!a[$0]++'                     # most concise script
;;  awk '!($0 in a){a[$0];print}'      # most efficient script
(test-sad "remove duplicate, nonconsecutive lines"
          "(echo a; echo b; echo a) | awk '!a[$0]++'"
          "(echo a; echo b; echo a) | sad buffer |\
           sad eval -R srfi-1 -r '(for-each print (delete-duplicates INPUT))'")

(test-end "SELECTIVE DELETION OF CERTAIN LINES")
;;
;; CREDITS AND THANKS:
;;
;; Special thanks to the late Peter S. Tillier (U.K.) for helping me with
;; the first release of this file, and to Daniel Jana, Yisu Dong, and
;; others for their suggestions and corrections.
;;
;; The golden reference is "The AWK Programming Language" (1988) by Alfred
;; Aho, Peter Weinberger, and Brian Kernighan, the creators of awk. (The
;; name AWK comes from the first initial of each of their names.)
;;
;; For additional syntax instructions, including the way to apply editing
;; commands from a disk file instead of the command line, consult:
;;
;;   "sed & awk, 2nd Edition," by Dale Dougherty and Arnold Robbins
;;   (O'Reilly, 1997)
;;
;;   "UNIX Text Processing," by Dale Dougherty and Tim O'Reilly (Hayden
;;   Books, 1987)
;;
;;   "GAWK: Effective awk Programming," by Arnold D. Robbins (O'Reilly)
;;   or at http://www.gnu.org/software/gawk/manual/
;;
;; To fully exploit the power of awk, one must know regular expressions.
;; For a detailed study, see "Mastering Regular Expressions, 3d edition"
;; by Jeffrey Friedl (O'Reilly, 2006).
;;
;; The info and manual ("man") pages on Unix systems may be helpful (try
;; "man awk", "man nawk", "man gawk", "man regexp", or the section on
;; regular expressions in "man ed").
;;
;; USE OF '\t' IN awk SCRIPTS: For clarity in documentation, I have used
;; '\t' to indicate a tab character (0x09) in the scripts.  All versions of
;; awk should recognize this abbreviation.
;;
;; #---end of file---
