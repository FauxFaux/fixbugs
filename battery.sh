#!/bin/sh

set -eu

do_test () {
    ant run -Da="$1" -Dc="$2" 2>&1 | less
}

do_test "progs/fixbugs/test/Simple.java" "contrib/remove_for.trans"
do_test "progs/fixbugs/test/Simple.java" "contrib/remove_rhs_or_for.trans"

