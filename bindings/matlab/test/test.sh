#!/bin/sh
s=`expr ./$2 : '.*/\(.*\).m'`
rm -f ./test_failed
$1 -nodesktop -nodisplay -nosplash -nojvm -r "$s; quit;"

#lame
if test -e ./test_failed; then
  rm -f ./test_failed
  exit 1
fi
