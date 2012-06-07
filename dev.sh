#! /bin/bash

erl -name 'ltalk@127.0.0.1' \
-setcookie 'ltalk' \
-pa "./ebin" \
 -s ltalk start 4017
