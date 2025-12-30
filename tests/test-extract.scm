(let ((input "(echo 'aaa foo'; echo 'aaa bar'; echo 'bbb foo')"))

  (test "extract"
        "(\"foo\")(\"foo\")"
        (run (string-append input " | sad extract '.* (foo)'")))
  )
