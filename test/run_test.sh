#!/bin/sh
# Copyright (C) 2026 G. Smecher <gsmecher@t0.technology>
#
##########################################################################
#
# This file is part of the GetData project.
#
# GetData is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the
# Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.
#
# GetData is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with GetData; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
# Test harness (LOG_COMPILER): run a single test in its own scratch directory
# (test_output/<name>), so that tests (most of which create a "dirfile" in
# their working directory) can run in parallel.

tst=$1
shift

name=`basename "$tst"`

# The harness invokes tests by relative path (e.g. ./add_add); make it
# absolute so we can execute it from inside the scratch directory.
case "$tst" in
  /*) ;;
  *) tst=`pwd`/$tst ;;
esac

dir="test_output/$name"
rm -rf "$dir"
mkdir -p "$dir" || exit 99
cd "$dir" || exit 99

exec "$tst" "$@"
