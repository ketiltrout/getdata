<?php
# Copyright (C) 2013 D. V. Wiebe
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

$ret = 2;

function callback($pdata, $extra)
{
  global $ret;

  $ret = 0;

  if (!isset($extra)) {
    echo "extra is unset\n";
    $ret = 1;
  } else if ($extra != 'extra stuff') {
    echo 'extra = ', $extra, "\n";
    $ret = 1;
  }

  if ($pdata['suberror'] != 8) {
    echo 'suberror = ', $pdata['suberror'], "\n";
    $ret = 1;
  }

  if ($pdata['linenum'] != 2) {
    echo 'linenum = ', $pdata['linenum'], "\n";
    $ret = 1;
  }

  if ($pdata['line'] != "bad line\n") {
    echo 'line = ', $pdata['line'], "\n";
    $ret = 1;
  }

  if (preg_match('/dirfile\/format$/', $pdata['filename']) != 1) {
    echo 'filename = ', $pdata['filename'], "\n";
    $ret = 1;
  }

  return GD_SYNTAX_IGNORE;
}

mkdir('dirfile', 0700);
file_put_contents('dirfile/format', "data RAW UINT16 8\nbad line\n");

$D = gd_open('dirfile', GD_RDONLY, 'callback', 'extra stuff');
$e = gd_error($D);

unlink('dirfile/format');
rmdir('dirfile');

if ($ret == 2) {
  echo "never saw callback\n";
  $ret = 1;
}
if ($e != GD_E_OK) {
  echo "error = ", $e, "\n";
  $ret = 1;
}

exit($ret);

?>
