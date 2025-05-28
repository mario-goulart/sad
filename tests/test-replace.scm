(test "translate with escapes (newline)"
      "\
1
2
3
"
      (run "echo 1 2 3 | sad replace -a -e ' ' '\\n'"))

(test "translate with escapes (tabs)"
      "1 2 3
"
      (run "echo 1 2 3 | sad replace -a -e ' ' '\\t' | sad replace -a -e '\\t' ' '"))

(test "translate with escapes (null)"
      "1 2 3
"
      (run "echo 1 2 3 | sad replace -a -e ' ' '\\0' | sad replace -a -e '\\0' ' '"))
