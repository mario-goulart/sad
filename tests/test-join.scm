(test "join with escapes"
      "\
1
2
3
"
      (run "echo 1 2 3 | sad split | sad join -e '\\n'"))
