#!/bin/sh

./dist/build/test/test -j4

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES="Main System.Event.Array.Tests System.Event.TimeoutTable.Tests"
EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir=$DIR test >/dev/null 2>&1

rm -f test.tix
