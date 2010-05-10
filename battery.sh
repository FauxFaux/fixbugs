#!/bin/sh

set -eu

do_test () {
    ant run -Da="$1" -Dc="$2" 2>&1 | less
}

# Simple Remove Tests

#do_test "progs/fixbugs/test/Simple.java" "contrib/remove_for.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/remove_rhs_or_for.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/remove_rhs_then_for.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/remove_if.trans"

#do_test "progs/fixbugs/test/LockTest.java" "contrib/ensure_lock_release.trans"
do_test "progs/fixbugs/test/JDBCTest.java" "contrib/jdbc_commit_and_rollback.trans"

