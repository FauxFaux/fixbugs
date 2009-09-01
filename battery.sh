#!/bin/sh

set -eu

ant run -Da="progs/fixbugs/test/Simple.java" -Db="bin/fixbugs/test/Simple.class" -Dc="contrib/remove_rhs.trans"
ant run -Da="progs/fixbugs/test/Simple.java" -Db="bin/fixbugs/test/Simple.class" -Dc="contrib/remove_ints.trans"
