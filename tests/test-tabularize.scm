(test "tabularized seq" "\
┌───────────┐
│ 1 │ 2 │ 3 │
│ 4 │ 5 │ 6 │
│ 7 │ 8 │ 9 │
└───────────┘
"
      (run "seq 9 | sad buffer 3 | sad tabularize"))


(test "tabularized seq (grid)" "\
┌───────────┐
│ 1 │ 2 │ 3 │
│───│───│───│
│ 4 │ 5 │ 6 │
│───│───│───│
│ 7 │ 8 │ 9 │
└───────────┘
"
      (run "seq 9 | sad buffer 3 | sad tabularize --grid"))


(test "tabularized seq (first line is header, grid)" "\
┌───────────┐
│ 1 │ 2 │ 3 │
│───│───│───│
│ 4 │ 5 │ 6 │
│───│───│───│
│ 7 │ 8 │ 9 │
└───────────┘
"
      (run "seq 9 | sad buffer 3 | sad tabularize --first-line-is-header --grid"))


(test "tabularized seq (first-line is header)" "\
┌───────────┐
│ 1 │ 2 │ 3 │
│───│───│───│
│ 4 │ 5 │ 6 │
│ 7 │ 8 │ 9 │
└───────────┘
"
      (run "seq 9 | sad buffer 3 | sad tabularize --first-line-is-header"))


(test "tabularized seq (borderless)" "\
1  2  3
4  5  6
7  8
"
      (run "seq 8 | sad buffer 3 | sad tabularize --borderless"))


(test "tabularized seq (markdown)" "\
|   |   |   |
|---|---|---|
| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |
"
      (run "seq 9 | sad buffer 3 | sad tabularize --markdown"))


(test "tabularized seq (markdown, first line is header)" "\
| 1 | 2 | 3 |
|---|---|---|
| 4 | 5 | 6 |
| 7 | 8 | 9 |
"
      (run "seq 9 | sad buffer 3 | sad tabularize --markdown --first-line-is-header"))


(test "tabularized seq (markdown, first line is header, grid)" "\
| 1 | 2 | 3 |
|---|---|---|
| 4 | 5 | 6 |
| 7 | 8 | 9 |
"
      (run "seq 9 | sad buffer 3 | sad tabularize --markdown --first-line-is-header --grid"))
