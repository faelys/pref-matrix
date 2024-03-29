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

TO_CLEAN="test-default.json test-one.json test-two.json"

trap 'rm -f ${TO_CLEAN}' EXIT

if test -z "${TEST_DB-}"; then
	TEST_DB="$(mktemp "${TMP_DIR}/pref-matrix-test.XXXXXXXX")"
	TO_CLEAN="${TO_CLEAN} ${TEST_DB} ${TEST_DB}-shm ${TEST_DB}-wal"
fi

if test -z "${TEST_TRACE-}"; then
	TEST_TRACE="$(mktemp "${TMP_DIR}/pref-matrix-test.XXXXXXXX")"
	TO_CLEAN="${TO_CLEAN} ${TEST_TRACE}"
fi

do_post(){
	URI_PATH="$1"
	EXPECTED_CODE="$2"
	shift 2
	echo "; POST ${URI_PATH} <- $*" >>"${TEST_TRACE}"

	RESULT=$(curl -s "$@" "http://localhost:9090${URI_PATH}" \
	    | tee -a "${TEST_TRACE}" \
	    | sed -n -e '/^; POST/{;s/^.*//;x;}' \
	             -e 's/.*title>\([0-9][0-9][0-9]\) - .*/\1/p')

	if ! test "${EXPECTED_CODE}" = "${RESULT}"; then
		echo "POST ${URI_PATH} <- $*"
		echo "  returned ${RESULT}, expected ${EXPECTED_CODE}"
		false
	fi
}

check_text(){
	diff -u "${TEST_DIR}/$1" "$2"
}


###################
## Test 1: replay

echo -n "" >|"${TEST_TRACE}"
"$@" :memory: "${TEST_TRACE}" "${TEST_DIR}/test-1.scm"
sed '/; 2[0-9][0-9][0-9]-/d;$s/$/\n(generate-json)\n(exit)/' "${TEST_TRACE}" \
    | check_text test-1.scm -
check_text test-1.json test-default.json

####################################
## Test 2: HTTP with default topic

rm -f "${TEST_DB}" "${TEST_DB}-shm" "${TEST_DB}-wal"
"$@" "${TEST_DB}" "${TEST_TRACE}" "${TEST_DIR}/test-2.scm" &
SRV_PID=$!

trap 'rm -f ${TO_CLEAN}; kill ${SRV_PID}' EXIT

sleep 1

do_post '/new-subject' 200 -d 'name=foo'
check_text test-2-01.json test-default.json
do_post '/new-object' 200 -d 'name=01'
check_text test-2-02.json test-default.json
do_post '/new-object' 200 -d 'name=03'
check_text test-2-03.json test-default.json
do_post '/new-subject' 200 -d 'name=bar'
check_text test-2-04.json test-default.json
do_post '/bin/set-pref' 200 -d 'sub=bar' -d '01=3' -d '04=4'
check_text test-2-05.json test-default.json
do_post '/do/new-object' 200 -d 'name=02'
check_text test-2-06.json test-default.json
do_post '/new-subject' 409 -d 'name=bar'
check_text test-2-06.json test-default.json
do_post '/new-object' 200 -d 'name=04'
check_text test-2-07.json test-default.json
do_post '/set-pref' 200 -d 'sub=meow' -d '04=2' -d '01=4'
check_text test-2-07.json test-default.json
do_post '/new-subject' 200 -d 'name=meow'
check_text test-2-08.json test-default.json
do_post '/set-pref' 200 -d 'sub=foo' -d '01=1' -d '04=2' -d '01=4'
check_text test-2-09.json test-default.json
do_post '/set-pref' 200 -d 'sub=bar' -d '01=0'
check_text test-2-10.json test-default.json
do_post '/new-object' 409 -d 'name=04'
check_text test-2-10.json test-default.json

kill "${SRV_PID}"
trap 'rm -f ${TO_CLEAN}' EXIT

sqlite3 "${TEST_DB}" .dump | check_text test-2-dump.sql -

##############################################
## Test 3: database migration from schema v1

cp -f "${TEST_DIR}/test-2-v1.sqlite" "${TEST_DB}"
rm -f "${TEST_DB}-shm" "${TEST_DB}-wal"
"$@" "${TEST_DB}" "${TEST_TRACE}" "${TEST_DIR}/test-3.scm"
sqlite3 "${TEST_DB}" .dump | check_text test-2-dump.sql -

#####################################
## Test 4: HTTP with several topics

rm -f test-default.json test-one.json test-two.json
"$@" :memory: "${TEST_TRACE}" "${TEST_DIR}/test-4.scm" &
SRV_PID=$!

trap 'rm -f ${TO_CLEAN}; kill ${SRV_PID}' EXIT

sleep 1

do_post '/new-topic' 200 -d 'name=one'
check_text test-4-1a.json test-one.json
! test -e test-two.json
do_post '/new-topic' 200 -d 'name=two'
check_text test-4-1a.json test-one.json
check_text test-4-1b.json test-two.json
do_post '/new-topic' 409 -d 'name=two'
check_text test-4-1a.json test-one.json
check_text test-4-1b.json test-two.json
do_post '/new-subject' 409 -d 'name=foo'
check_text test-4-1a.json test-one.json
check_text test-4-1b.json test-two.json
! test -e test-default.json
do_post '/new-subject' 200 -d 'topic=one' -d 'name=foo'
check_text test-4-2a.json test-one.json
check_text test-4-1b.json test-two.json
do_post '/new-subject' 200 -d 'topic=two' -d 'name=foo'
check_text test-4-2a.json test-one.json
check_text test-4-2b.json test-two.json
do_post '/new-subject' 409 -d 'topic=one' -d 'name=foo'
check_text test-4-2a.json test-one.json
check_text test-4-2b.json test-two.json
do_post '/new-subject' 409 -d 'topic=other' -d 'name=bar'
check_text test-4-2a.json test-one.json
check_text test-4-2b.json test-two.json
! test -e test-other.json
do_post '/new-object' 200 -d 'topic=one' -d 'name=some'
check_text test-4-3a.json test-one.json
check_text test-4-2b.json test-two.json
do_post '/new-object' 200 -d 'topic=one' -d 'name=common'
check_text test-4-4a.json test-one.json
check_text test-4-2b.json test-two.json
do_post '/new-object' 200 -d 'topic=two' -d 'name=common'
check_text test-4-4a.json test-one.json
check_text test-4-3b.json test-two.json
do_post '/new-object' 200 -d 'topic=two' -d 'name=thing'
check_text test-4-4a.json test-one.json
check_text test-4-4b.json test-two.json
do_post '/new-subject' 200 -d 'topic=one' -d 'name=bar'
check_text test-4-5a.json test-one.json
check_text test-4-4b.json test-two.json
do_post '/new-subject' 200 -d 'topic=two' -d 'name=baz'
check_text test-4-5a.json test-one.json
check_text test-4-5b.json test-two.json
do_post '/set-pref' 200 -d 'topic=one' -d 'sub=foo' -d 'some=4' -d 'common=3'
check_text test-4-6a.json test-one.json
check_text test-4-5b.json test-two.json
do_post '/set-pref' 200 -d 'topic=two' -d 'sub=foo' -d 'thing=2' -d 'common=1'
check_text test-4-6a.json test-one.json
check_text test-4-6b.json test-two.json
do_post '/set-pref' 200 -d 'topic=one' -d 'sub=baz' -d 'some=4' -d 'common=3'
check_text test-4-6a.json test-one.json
check_text test-4-6b.json test-two.json
do_post '/set-pref' 200 -d 'topic=two' -d 'sub=bar' -d 'thing=2' -d 'common=1'
check_text test-4-6a.json test-one.json
check_text test-4-6b.json test-two.json
do_post '/set-pref' 200 -d 'topic=two' -d 'sub=baz' -d 'some=4' -d 'common=5'
check_text test-4-6a.json test-one.json
check_text test-4-7b.json test-two.json
do_post '/set-pref' 200 -d 'topic=one' -d 'sub=bar' -d 'thing=2' -d 'common=5'
check_text test-4-7a.json test-one.json
check_text test-4-7b.json test-two.json
do_post '/hide-subject' 200 -d 'topic=one' -d 'name=foo'
check_text test-4-8a.json test-one.json
check_text test-4-7b.json test-two.json
do_post '/hide-object' 200 -d 'topic=two' -d 'name=common'
check_text test-4-8a.json test-one.json
check_text test-4-8b.json test-two.json
do_post '/hide-object' 200 -d 'topic=two' -d 'name=non-existent'
check_text test-4-8a.json test-one.json
check_text test-4-8b.json test-two.json
do_post '/new-object' 200 -d 'topic=two' -d 'name=common'
check_text test-4-8a.json test-one.json
check_text test-4-7b.json test-two.json
do_post '/new-subject' 200 -d 'topic=one' -d 'name=foo'
check_text test-4-7a.json test-one.json
check_text test-4-7b.json test-two.json
do_post '/close-topic' 200 -d 'name=one'
check_text test-4-7a.json test-one.json
check_text test-4-7b.json test-two.json
do_post '/set-pref' 200 -d 'topic=two' -d 'sub=foo' -d 'common=5'
check_text test-4-7a.json test-one.json
check_text test-4-9b.json test-two.json
do_post '/set-pref' 200 -d 'topic=one' -d 'sub=foo' -d 'common=5'
check_text test-4-7a.json test-one.json
check_text test-4-9b.json test-two.json
do_post '/new-subject' 409 -d 'topic=one' -d 'name=extra'
check_text test-4-7a.json test-one.json
check_text test-4-9b.json test-two.json

kill "${SRV_PID}"
trap 'rm -f ${TO_CLEAN}' EXIT
