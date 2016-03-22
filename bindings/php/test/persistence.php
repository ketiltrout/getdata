<?php
# Copyright (C) 2016 D. V. Wiebe
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

$ret = 0;

unlink('dirfile/format');
rmdir('dirfile');

$D1 = gd_popen('dirfile', GD_RDWR | GD_CREAT | GD_EXCL);

$e1 = gd_error($D1);
if ($e1 != GD_E_OK) {
  echo "e1 = ", $e1, "\n";
  $ret = 1;
}

gd_add_spec($D1, "const CONST UINT8 1");

$n1 = gd_nfields_by_type($D1, GD_CONST_ENTRY);
if ($n1 != 1) {
  echo "n1 = ", $n1, "\n";
  $ret = 1;
}

$D2 = gd_popen('dirfile', GD_RDWR | GD_CREAT);

$e2 = gd_error($D2);
if ($e2 != GD_E_OK) {
  echo "e2 = ", $e2, "\n";
  $ret = 1;
}

$n2 = gd_nfields_by_type($D2, GD_CONST_ENTRY);
if ($n2 != 1) {
  echo "n2 = ", $n2, "\n";
  $ret = 1;
}

if (!gd_discard($D2)) {
  echo "dicard(D2) returns FALSE\n";
  $ret = 1;
}

$n3 = gd_nfields_by_type($D1, GD_CONST_ENTRY);
if ($n3 != 1) {
  echo "n3 = ", $n3, "\n";
  $ret = 1;
}

if (!gd_discard($D1)) {
  echo "dicard(D1) returns FALSE\n";
  $ret = 1;
}

unlink('dirfile/format');
rmdir('dirfile');

exit($ret);

?>
