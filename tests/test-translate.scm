(test "translate with escapes"
      "\
1
2
3
"
      (run "echo 1 2 3 | sad replace -a -e ' ' '\\n'"))

(test "translate with escapes"
      "1 2 3
"
      (run "echo 1 2 3 | sad replace -a -e ' ' '\\t' | sad replace -a -e '\\t' ' '"))
