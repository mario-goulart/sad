(test "tabularized seq"
          "\
┌───────────┐
│ 1 │ 2 │ 3 │
│ 4 │ 5 │ 6 │
│ 7 │ 8 │ 9 │
└───────────┘
"
          (run "seq 9 | sad buffer 3 | sad tabularize"))


(test "tabularized seq"
          "\
1  2  3
4  5  6
7  8
"
          (run "seq 8 | sad buffer 3 | sad tabularize --borderless"))
