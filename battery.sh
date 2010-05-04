#!/bin/sh

set -eu

#ant run -Da="progs/fixbugs/test/Simple.java" -Db="bin/fixbugs/test/Simple.class" -Dc="contrib/remove_rhs.trans" 2>&1 | less
#ant run -Da="progs/fixbugs/test/Simple.java" -Db="bin/fixbugs/test/Simple.class" -Dc="contrib/remove_rhs_then_for.trans" 2>&1 | less
ant run -Da="progs/fixbugs/test/Simple.java" -Db="bin/fixbugs/test/Simple.class" -Dc="contrib/remove_rhs_or_for.trans" 2>&1 | less

#ant run -Da="progs/fixbugs/test/Simple.java" -Db="bin/fixbugs/test/Simple.class" -Dc="contrib/remove_for.trans"
