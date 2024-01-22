#!/bin/sh

# Copyright (c) 2024, Natacha Porté
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

set -Cue

if test "$#" -lt 1; then
	echo "Usage: $0 [start-cmd]" >&2
	exit 1
fi

: "${TEST_DIR:=$(dirname "$0")}"
: "${TMP_DIR:=/tmp}"

TO_CLEAN="test-default.json"

trap 'rm -f ${TO_CLEAN}' EXIT

if test -z "${TEST_DB-}"; then
	TEST_DB="$(mktemp "${TMP_DIR}/pref-matrix-test.XXXXXXXX")"
	TO_CLEAN="${TO_CLEAN} ${TEST_DB}*"
fi

if test -z "${TEST_TRACE-}"; then
	TEST_TRACE="$(mktemp "${TMP_DIR}/pref-matrix-test.XXXXXXXX")"
	TO_CLEAN="${TO_CLEAN} ${TEST_TRACE}"
fi

###################
## Test 1: replay

echo -n "" >|"${TEST_TRACE}"
"$@" :memory: "${TEST_TRACE}" "${TEST_DIR}/test-1.scm"
sed '/; 2[0-9][0-9][0-9]-/d;$s/$/\n(generate-json)\n(exit)/' "${TEST_TRACE}" \
    | diff -u "${TEST_DIR}/test-1.scm" -
diff -u "${TEST_DIR}/test-1.json" test-default.json

####################################
## Test 2: HTTP with default topic

"$@" "${TEST_DB}" "${TEST_TRACE}" "${TEST_DIR}/test-2.scm" &
SRV_PID=$!

trap 'rm -f ${TO_CLEAN}; kill ${SRV_PID}' EXIT

sleep 1

curl -s -d 'name=foo' 'http://localhost:9090/new-subject' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-01.json" test-default.json
curl -s -d 'name=01' 'http://localhost:9090/new-object' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-02.json" test-default.json
curl -s -d 'name=03' 'http://localhost:9090/new-object' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-03.json" test-default.json
curl -s -d 'name=bar' 'http://localhost:9090/new-subject' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-04.json" test-default.json
curl -s -d 'sub=bar' -d '01=3' -d '04=4' \
     'http://localhost:9090/bin/set-pref' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-05.json" test-default.json
curl -s -d 'name=02' 'http://localhost:9090/do/new-object' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-06.json" test-default.json
curl -s -d 'name=bar' 'http://localhost:9090/new-subject' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-06.json" test-default.json
curl -s -d 'name=04' 'http://localhost:9090/new-object' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-07.json" test-default.json
curl -s -d 'sub=meow' -d '04=2' -d '01=4' \
     'http://localhost:9090/set-pref' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-07.json" test-default.json
curl -s -d 'name=meow' 'http://localhost:9090/new-subject' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-08.json" test-default.json
curl -s -d 'sub=foo' -d '01=1' -d '04=2' -d '01=4' \
     'http://localhost:9090/set-pref' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-09.json" test-default.json
curl -s -d 'sub=bar' -d '01=0' \
     'http://localhost:9090/set-pref' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-10.json" test-default.json
curl -s -d 'name=04' 'http://localhost:9090/new-object' >>"${TEST_TRACE}"
diff -u "${TEST_DIR}/test-2-10.json" test-default.json

kill "${SRV_PID}"
trap 'rm -f ${TO_CLEAN}' EXIT

sqlite3 "${TEST_DB}" .dump | diff -u "${TEST_DIR}/test-2-dump.sql" -

##############################################
## Test 3: database migration from schema v1

cp -f "${TEST_DIR}/test-2-v1.sqlite" "${TEST_DB}"
rm -f "${TEST_DB}-shm" "${TEST_DB}-wal"
"$@" "${TEST_DB}" "${TEST_TRACE}" "${TEST_DIR}/test-3.scm"
sqlite3 "${TEST_DB}" .dump \
   | diff -u "${TEST_DIR}/test-2-dump.sql" -
