#!/bin/sh
mkfifo /tmp/bld
tee -a build.log </tmp/bld &
$@ >/tmp/bld 2>&1
status=$?
rm /tmp/bld
exit $status
