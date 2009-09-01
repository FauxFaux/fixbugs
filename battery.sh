#!/bin/sh

set -eu

ant run -Da="progs/fixbugs/test/Simple.java" -Db="bin/fixbugs/test/Simple.class" -Dc="foo"
