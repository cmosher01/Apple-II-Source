#!/bin/sh
mkfifo bld
tee -a build.log <bld &
$@ >bld 2>&1
status=$?
rm bld
exit $status
