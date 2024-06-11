#! /bin/sh

cat doc.md |
    sad lines 2: |
    sad replace "<here comes sad's help message>" "$(sad -h)" > README.md
