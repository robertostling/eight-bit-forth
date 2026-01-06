#!/bin/sh

IMAGE=$1
shift
for FILE in $@ ; do
    c1541 $IMAGE -write $FILE `basename $FILE`
done
