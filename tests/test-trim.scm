(test "trim line, both sides"
      "\
1
2
3
"
      (run "(echo '  1  '; echo ' 2 '; echo '  3 ') | sad trim"))

(test "trim line, right side"
      "  1
 2
  3
"
      (run "(echo '  1  '; echo ' 2 '; echo '  3 ') | sad trim --right"))

(test "trim line, left side"
      "\
1  
2 
3 
"
      (run "(echo '  1  '; echo ' 2 '; echo '  3 ') | sad trim --left"))

(test "trim sexp, both sides"
      "(\"1\" \"2\" \"3\")"
      (run "seq 3 | \
            sad buffer | \
            sad map '(lambda (i) (conc \"  \" i \"  \"))' | \
            sad trim -r"))
