#! /bin/sh

export SAD_NO_CONF_LOAD=1
chicken-install -n >/dev/null
cat doc.md |
    sad lines 2: |
    sad replace "<here comes sad's help message>" "$(sad -h)" > README.md
