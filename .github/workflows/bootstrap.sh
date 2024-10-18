#!/bin/sh

set -e

here=$(cd `dirname $0`; pwd -P)
echo $here
cd $here/../../

autoreconf -i
./configure --disable-php \
            --disable-fortran \
            --disable-perl \
            --disable-matlab \
            --disable-idl \
            --disable-cplusplus \
            --disable-modules \
            --with-python=`which python3` \
            --with-pcre=$PREFIX \
            --with-ltdl=$PREFIX \
            --with-liblzma=$PREFIX \
            --with-libFLAC=$PREFIX \
            --with-libzzip=$PREFIX \
            $@
