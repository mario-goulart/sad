THIS IS JUST A POOR TEMPLATE -- SEE README.md

# sad

```
<here comes sad's help message>
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
