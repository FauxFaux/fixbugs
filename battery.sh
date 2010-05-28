#!/bin/sh

set -eu

do_test () {
    ant run -Da="$1" -Dc="$2" 2>&1 | less
}

# Simple Remove Tests

#do_test "progs/fixbugs/test/Simple.java" "contrib/remove_for.trans"
do_test "progs/fixbugs/test/Simple.java" "contrib/remove_rhs_or_for.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/remove_rhs_then_for.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/remove_if.trans"

# Temporal Logic

#do_test "progs/fixbugs/test/Simple.java" "contrib/and.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/or.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/not.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/temp_and.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/temp_or.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/temp_next.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/temp_until.trans"
#do_test "progs/fixbugs/test/Simple.java" "contrib/temp_future.trans"

# real world examples

#do_test "progs/fixbugs/test/LockTest.java" "contrib/ensure_lock_release.trans"
#do_test "progs/fixbugs/test/JDBCTest.java" "contrib/jdbc_commit_and_rollback.trans"
#do_test "progs/fixbugs/test/TestIntType.java" "contrib/test_type_int.trans"
#do_test "progs/fixbugs/test/TestObjType.java" "contrib/test_type_obj.trans"

