<?php
# Copyright (C) 2013, 2014, 2015 D. V. Wiebe
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

$ne = 0;

ini_set('getdata.unpack', false);
ini_set('getdata.degrade_complex', true);

# clean up 
if (is_file('dirfile/data')) {
  unlink('dirfile/data');
}
if (is_file('dirfile/new1')) {
  unlink('dirfile/new1');
}
if (is_file('dirfile/new135')) {
  unlink('dirfile/new135');
}
if (is_file('dirfile/format')) {
  unlink('dirfile/format');
}
if (is_file('dirfile/format1')) {
  unlink('dirfile/format1');
}
if (is_file('dirfile/format2')) {
  unlink('dirfile/format2');
}
if (is_file('dirfile/form2')) {
  unlink('dirfile/form2');
}

if (is_dir('dirfile')) {
  rmdir('dirfile');
}

function check_var2($t, $n, $v, $g)
{
  global $ne;

  if ($v !== $g) {
    $ne = $ne + 1;
    echo "v[" . $t . ", " . $n . "] = ";
    var_dump($v);
    echo "\nexpected: ";
    var_dump($g);
    echo "\n";
  }
}

function check_var($t, $v, $g)
{
  global $ne;

  if ($v !== $g) {
    $ne = $ne + 1;
    echo "v[" . $t . "] = ";
    var_dump($v);
    echo "\nexpected: ";
    var_dump($g);
    echo "\n";
  }
}

function check_complex2($t, $n, $v, $r, $i)
{
  check_var2($t, $n, $v, array($r, $i));
}

function check_complex($t, $v, $r, $i)
{
  check_var($t, $v, array($r, $i));
}

function check_eostring($t, $v, $g)
{
  global $ne;

  if (substr($v, strlen($v) - strlen($g)) == $v) {
    $ne = $ne + 1;
    echo "s[", $t, "] = ", $v, " (expected [...]", $g, ")\n";
  }
}

function check_error2($t, $n, $D, $e)
{
  global $ne;

  $v = gd_error($D);
  if ($v != $e) {
    $ne = $ne + 1;
    echo "e[", $t, ", ", $n, "] = ", $v, " (expected ", $e, ")\n";
  }
}

function check_error($t, $D, $e)
{
  global $ne;

  $v = gd_error($D);
  if ($v != $e) {
    $ne = $ne + 1;
    echo "e[", $t, "] = ", $v, " (expected ", $e, ")\n";
  }
}

function check_ok2($t, $n, $D)
{
  check_error2($t, $n, $D, 0);
}
function check_ok($t, $D)
{
  check_error($t, $D, 0);
}

$nfields = 20;
$fields = array( 'bit', 'div', 'data', 'mult', 'sbit', 'INDEX', 'alias',
  'const', 'indir', 'mplex', 'phase', 'recip', 'carray', 'lincom', 'sarray',
  'sindir', 'string', 'window', 'linterp', 'polynom');

# make the dirfile
mkdir('dirfile', 0700);
file_put_contents('dirfile/format',
'/ENDIAN little
data RAW INT8 8
lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const
/META data mstr STRING "This is a string constant."
/META data mconst CONST COMPLEX128 3.3;4.4
/META data mcarray CARRAY FLOAT64 1.9 2.8 3.7 4.6 5.5
/META data mlut LINTERP DATA ./lut
const CONST FLOAT64 5.5
carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6
linterp LINTERP data ./lut
polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const
bit BIT data 3 4
sbit SBIT data 5 6
mplex MPLEX data sbit 1 10
mult MULTIPLY data sbit
div DIVIDE mult bit
recip RECIP div 6.5;4.3
phase PHASE data 11
window WINDOW linterp mult LT 4.1
ALIAS alias data
string STRING "Zaphod Beeblebrox"
sarray SARRAY one two three four five six seven
data/msarray SARRAY eight nine ten eleven twelve
indir INDIR data carray
sindir SINDIR data sarray
');
file_put_contents('dirfile/form2', "const2 CONST INT8 -19\n");

$data_data='';
for ($i = 0; $i < 80; ++$i) {
  $data_data .= chr($i + 1);
}
$handle = fopen('dirfile/data', 'wb');
fwrite($handle, $data_data, 80);
fclose($handle);

# 1: gd_error
$D = gd_open('');
check_error(1, $D, GD_E_IO);

# 2: gd_open
$D = gd_open("dirfile\0extra", GD_RDWR);
check_ok(2, $D);

# 3: getdata
$v = gd_getdata($D, 'data', 5, 0, 1, 0, GD_UINT8);
check_ok(3, $D);
check_var(3, $v, chr(41) . chr(42) . chr(43) . chr(44) . chr(45) . chr(46) .
  chr(47) . chr(48));

# 4: getdata (unpacked)
$v = gd_getdata($D, 'data', 5, 0, 1, 0, GD_UINT16, TRUE);
check_ok(4, $D);
check_var(4, $v, range(41, 48));

# 10: getdata (complex unpacked)
$v = gd_getdata($D, 'data', 5, 0, 1, 0, GD_COMPLEX128, TRUE);
check_ok(10, $D);
check_var(10, $v, array(41., 42., 43., 44., 45., 46., 47., 48.));

# 12: gd_get_constant (INT8)
$v = gd_get_constant($D, 'const', GD_UINT8);
check_ok(12, $D);
check_var(12, $v, 5);

# 19: gd_get_constant (COMPLEX128)
$v = gd_get_constant($D, 'const', GD_COMPLEX128);
check_ok(19, $D);
check_var(19, $v, 5.5);

# 23: nfields
$v = gd_nfields($D);
check_ok(23, $D);
check_var(23, $v, $nfields);

# 25: field_list
$v = gd_field_list($D);
check_ok(25, $D);
check_var(25, $v, $fields);

# 26: nmfields
$v = gd_nmfields($D, 'data');
check_ok(26, $D);
check_var(26, $v, 5);

# 27: mfield_list
$v = gd_mfield_list($D, 'data');
check_ok(27, $D);
check_var(27, $v, array('mstr', 'mconst', 'mcarray', 'mlut', 'msarray'));

# 28: nframes
$v = gd_nframes($D);
check_ok(28, $D);
check_var(28, $v, 10);

# 29: spf
$v = gd_spf($D, 'data');
check_ok(29, $D);
check_var(29, $v, 8);

# 30: putdata (packed)
$v = gd_putdata($D, 'data', 5, 1, GD_UINT8, "\x13\x14\x15\x16");
check_ok2(30, 1, $D);
check_var2(30, 2, $v, 4);

$v = gd_getdata($D, 'data', 5, 0, 1, 0, GD_UINT8);
check_ok2(30, 3, $D);
check_var2(30, 4, $v, chr(41) . chr(0x13) . chr(0x14) . chr(0x15) . chr(0x16)
  . chr(46) . chr(47) . chr(48));

# 33: putdata (type array)
$v = gd_putdata($D, 'data', 5, 1, GD_INT64, array(23, 24, 25, array(26, 0)));
check_ok2(33, 1, $D);
check_var2(33, 2, $v, 4);

$v = gd_getdata($D, 'data', 5, 0, 1, 0, GD_UINT8, TRUE);
check_ok2(33, 3, $D);
check_var2(33, 4, $v, array(41, 23, 24, 25, 26, 46, 47, 48));

# 35: putdata (untyped array)
$v = gd_putdata($D, 'data', 5, 1, array(2 => 35., 0 => 33, 3 => array(36, 0),
  4 => array(37, 0)));
check_ok2(35, 1, $D);
check_var2(35, 2, $v, 5);

$v = gd_getdata($D, 'data', 5, 0, 1, 0, GD_UINT8, TRUE);
check_ok2(35, 3, $D);
check_var2(35, 4, $v, array(41, 33, 0, 35, 36, 37, 47, 48));

# 37: putdata (complex)
$v = gd_putdata($D, 'data', 5, 1, GD_COMPLEX128, array(53, 54., array(55., 0),
  array(56, 0)));
check_ok2(37, 1, $D);
check_var2(37, 2, $v, 4);

$v = gd_getdata($D, 'data', 5, 0, 1, 0, GD_UINT8, TRUE);
check_ok2(37, 3, $D);
check_var2(37, 4, $v, array(41, 53, 54, 55, 56, 37, 47, 48));

# 38: error_string
gd_getdata($D, 'x', 5, 0, 1, 0, GD_UINT8);
check_error(38, $D, GD_E_BAD_CODE);
check_var(38, gd_error_string($D), 'Field not found: x');

# 39: entry_type
$v = gd_entry_type($D, 'data');
check_ok(39, $D);
check_var(39, $v, GD_RAW_ENTRY);

# 40: raw entry
$v = gd_entry($D, 'data');
check_ok(40, $D);
check_var(40, $v, array('field' => 'data', 'field_type' => GD_RAW_ENTRY,
  'fragment_index' => 0, 'spf' => 8, 'data_type' => GD_INT8,
  'scalar' => array()));

# 42: lincom entry
$v = gd_entry($D, 'lincom');
check_ok(42, $D);
check_var(42, $v, array('field' => 'lincom', 'field_type' => GD_LINCOM_ENTRY,
  'fragment_index' => 0, 'n_fields' => 3, 'in_fields' => array('data', 'INDEX',
  'linterp'), 'm' => array(1.1, 2.2, 5.5), 'b' => array(2.2, array(3.3, 4.4),
  5.5), 'scalar' => array(2 => array('const', -1), 5 => array('const', -1))));

# 44: polynom entry
$v = gd_entry($D, 'polynom');
check_ok(44, $D);
check_var(44, $v, array('field' => 'polynom',
  'field_type' => GD_POLYNOM_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('data'), 'poly_ord' => 5, 'a' => array(1.1, 2.2, 2.2,
  array(3.3, 4.4), 5.5, 5.5), 'scalar' => array(4 => array('const', -1),
  5 => array('const', -1))));

# 45: linterp entry
$v = gd_entry($D, 'linterp');
check_ok(45, $D);
check_var(45, $v, array('field' => 'linterp',
  'field_type' => GD_LINTERP_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('data'), 'table' => './lut'));

# 46: bit entry
$v = gd_entry($D, 'bit');
check_ok(46, $D);
check_var(46, $v, array('field' => 'bit', 'field_type' => GD_BIT_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('data'), 'bitnum' => 3,
  'numbits' => 4, 'scalar' => array()));

# 47: sbit entry
$v = gd_entry($D, 'sbit');
check_ok(47, $D);
check_var(47, $v, array('field' => 'sbit', 'field_type' => GD_SBIT_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('data'), 'bitnum' => 5,
  'numbits' => 6, 'scalar' => array()));

# 48: multiply entry
$v = gd_entry($D, 'mult');
check_ok(48, $D);
check_var(48, $v, array('field' => 'mult', 'field_type' => GD_MULTIPLY_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('data', 'sbit')));

# 49: phase entry
$v = gd_entry($D, 'phase');
check_ok(49, $D);
check_var(49, $v, array('field' => 'phase', 'field_type' => GD_PHASE_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('data'), 'shift' => 11,
  'scalar' => array()));

# 50: const entry
$v = gd_entry($D, 'const');
check_ok(50, $D);
check_var(50, $v, array('field' => 'const', 'field_type' => GD_CONST_ENTRY,
  'fragment_index' => 0, 'const_type' => GD_FLOAT64));

# 51: string entry
$v = gd_entry($D, 'string');
check_ok(51, $D);
check_var(51, $v, array('field' => 'string', 'field_type' => GD_STRING_ENTRY,
  'fragment_index' => 0));

# 52: fragment_index
$v = gd_fragment_index($D, 'data');
check_ok(52, $D);
check_var(52, $v, 0);

# 53: add raw
$v = gd_add_raw($D, 'new1', GD_FLOAT64, 3);
check_ok2(53, 0, $D);
check_var2(53, 1, $v, TRUE);

$v = gd_entry($D, 'new1');
check_ok2(53, 2, $D);
check_var2(53, 3, $v, array('field' => 'new1', 'field_type' => GD_RAW_ENTRY,
  'fragment_index' => 0, 'spf' => 3, 'data_type' => GD_FLOAT64,
  'scalar' => array()));

# 55: add lincom
$v = gd_add_lincom($D, 'new3', array('in1', 'in2'), array(array(1.1, 1.2),
  array(1.4, 1.5)), array(array(1.3, 1.4), 1.6));
check_ok2(55, 0, $D);
check_var2(55, 1, $v, TRUE);

$v = gd_entry($D, 'new3');
check_ok2(55, 2, $D);
check_var2(55, 3, $v, array('field' => 'new3', 'field_type' => GD_LINCOM_ENTRY,
  'fragment_index' => 0, 'n_fields' => 2, 'in_fields' => array('in1', 'in2'),
  'm' => array(array(1.1, 1.2), array(1.4, 1.5)), 'b' => array(array(1.3, 1.4),
  1.6), 'scalar' => array()));

# 56: add polynom
$v = gd_add_polynom($D, 'new4', 'in2', array(3.9, 4.8, 5.7, 6.6));
check_ok2(56, 0, $D);
check_var2(56, 1, $v, TRUE);

$v = gd_entry($D, 'new4');
check_ok2(56, 2, $D);
check_var2(56, 3, $v, array('field' => 'new4', 'field_type' => GD_POLYNOM_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in2'), 'poly_ord' => 3,
  'a' => array(3.9, 4.8, 5.7, 6.6), 'scalar' => array()));

# 58: add linterp
$v = gd_add_linterp($D, 'new6', 'in', './some/table');
check_ok2(58, 0, $D);
check_var2(58, 1, $v, TRUE);

$v = gd_entry($D, 'new6');
check_ok2(58, 2, $D);
check_var2(58, 3, $v, array('field' => 'new6', 'field_type' => GD_LINTERP_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in'),
  'table' => './some/table'));

# 59: add bit
$v = gd_add_bit($D, 'new7', 'in1', 11, 22);
check_ok2(59, 0, $D);
check_var2(59, 1, $v, TRUE);

$v = gd_entry($D, 'new7');
check_ok2(59, 2, $D);
check_var2(59, 3, $v, array('field' => 'new7', 'field_type' => GD_BIT_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in1'), 'bitnum' => 11,
  'numbits' => 22, 'scalar' => array()));

# 60: add sbit
$v = gd_add_sbit($D, 'new8', 'in2', 5, 10);
check_ok2(60, 0, $D);
check_var2(60, 1, $v, TRUE);

$v = gd_entry($D, 'new8');
check_ok2(60, 2, $D);
check_var2(60, 3, $v, array('field' => 'new8', 'field_type' => GD_SBIT_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in2'), 'bitnum' => 5,
  'numbits' => 10, 'scalar' => array()));

# 61: add multiply
$v = gd_add_multiply($D, 'new9', 'in2', 'in3');
check_ok2(61, 0, $D);
check_var2(61, 1, $v, TRUE);

$v = gd_entry($D, 'new9');
check_ok2(61, 2, $D);
check_var2(61, 3, $v, array('field' => 'new9',
  'field_type' => GD_MULTIPLY_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 62: add phase
$v = gd_add_phase($D, 'new10', 'in6', 62);
check_ok2(62, 0, $D);
check_var2(62, 1, $v, TRUE);

$v = gd_entry($D, 'new10');
check_ok2(62, 2, $D);
check_var2(62, 3, $v, array('field' => 'new10', 'field_type' => GD_PHASE_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in6'), 'shift' => 62,
  'scalar' => array()));

# 63: add const
$v = gd_add_const($D, 'new11', GD_FLOAT64, 33.3);
check_ok2(63, 0, $D);
check_var2(63, 1, $v, TRUE);

$v = gd_entry($D, 'new11');
check_ok2(63, 2, $D);
check_var2(63, 3, $v, array('field' => 'new11', 'field_type' => GD_CONST_ENTRY,
  'fragment_index' => 0, 'const_type' => GD_FLOAT64));

$v = gd_get_constant($D, 'new11', GD_FLOAT64);
check_ok2(63, 4, $D);
check_var2(63, 5, $v, 33.3);

# 64: fragmentname
$v = gd_fragmentname($D, 0);
check_ok(64, $D);
check_eostring(64, $v, "dirfile/format");

# 65: nfragments
$v = gd_nfragments($D);
check_ok(65, $D);
check_var(65, $v, 1);

# 66: gd_include
$v = gd_include($D, 'form2', 0);
check_ok2(66, 0, $D);
check_var2(66, 1, $v, 1);

$v = gd_get_constant($D, 'const2', GD_INT);
check_ok2(66, 2, $D);
check_var2(66, 3, $v, -19);

# 67: nfields by type
$v = gd_nfields_by_type($D, GD_LINCOM_ENTRY);
check_ok(67, $D);
check_var(67, $v, 2);

# 68: field list by type
$v = gd_field_list_by_type($D, GD_LINCOM_ENTRY);
check_ok(68, $D);
check_var(68, $v, array('new3', 'lincom'));

# 69: nfields by type
$v = gd_nvectors($D);
check_ok(69, $D);
check_var(69, $v, 23);

# 70: field list by type
$v = gd_vector_list($D);
check_ok(70, $D);
check_var(70, $v, array( 'bit', 'div', 'data', 'mult', 'new1', 'new3', 'new4',
  'new6', 'new7', 'new8', 'new9', 'sbit', 'INDEX', 'alias', 'indir', 'mplex',
  'new10', 'phase', 'recip', 'lincom', 'window', 'linterp', 'polynom'));

# 72: madd lincom
$v = gd_madd_lincom($D, 'data', 'mnew3', array('in1', 'in2'),
  array(array(1.1, 1.2), array(1.4, 1.5)), array(array(1.3, 1.4), 1.6));
check_ok2(72, 0, $D);
check_var2(72, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew3');
check_ok2(72, 2, $D);
check_var2(72, 3, $v, array('field' => 'data/mnew3',
  'field_type' => GD_LINCOM_ENTRY,
  'fragment_index' => 0, 'n_fields' => 2, 'in_fields' => array('in1', 'in2'),
  'm' => array(array(1.1, 1.2), array(1.4, 1.5)), 'b' => array(array(1.3, 1.4),
  1.6), 'scalar' => array()));

# 73: madd polynom
$v = gd_madd_polynom($D, 'data', 'mnew4', 'in2', array(3.9, 4.8, 5.7, 6.6));
check_ok2(73, 0, $D);
check_var2(73, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew4');
check_ok2(73, 2, $D);
check_var2(73, 3, $v, array('field' => 'data/mnew4',
  'field_type' => GD_POLYNOM_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in2'), 'poly_ord' => 3,
  'a' => array(3.9, 4.8, 5.7, 6.6), 'scalar' => array()));

# 75: madd linterp
$v = gd_madd_linterp($D, 'data', 'mnew6', 'in', './some/table');
check_ok2(75, 0, $D);
check_var2(75, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew6');
check_ok2(75, 2, $D);
check_var2(75, 3, $v, array('field' => 'data/mnew6',
  'field_type' => GD_LINTERP_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in'), 'table' => './some/table'));

# 76: madd bit
$v = gd_madd_bit($D, 'data', 'mnew7', 'in1', 11, 22);
check_ok2(76, 0, $D);
check_var2(76, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew7');
check_ok2(76, 2, $D);
check_var2(76, 3, $v, array('field' => 'data/mnew7',
  'field_type' => GD_BIT_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1'), 'bitnum' => 11, 'numbits' => 22,
  'scalar' => array()));

# 77: madd sbit
$v = gd_madd_sbit($D, 'data', 'mnew8', 'in2', 5, 10);
check_ok2(77, 0, $D);
check_var2(77, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew8');
check_ok2(77, 2, $D);
check_var2(77, 3, $v, array('field' => 'data/mnew8',
  'field_type' => GD_SBIT_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2'), 'bitnum' => 5, 'numbits' => 10,
  'scalar' => array()));

# 78: madd multiply
$v = gd_madd_multiply($D, 'data', 'mnew9', 'in2', 'in3');
check_ok2(78, 0, $D);
check_var2(78, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew9');
check_ok2(78, 2, $D);
check_var2(78, 3, $v, array('field' => 'data/mnew9',
  'field_type' => GD_MULTIPLY_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in2', 'in3')));

# 79: madd phase
$v = gd_madd_phase($D, 'data', 'mnew10', 'in6', 79);
check_ok2(79, 1, $D);
check_var2(79, 2, $v, TRUE);

$v = gd_entry($D, 'data/mnew10');
check_ok2(79, 3, $D);
check_var2(79, 4, $v, array('field' => 'data/mnew10',
  'field_type' => GD_PHASE_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in6'), 'shift' => 79, 'scalar' => array()));

# 80: madd const
$v = gd_madd_const($D, 'data', 'mnew11', GD_FLOAT64, 33.3);
check_ok2(80, 0, $D);
check_var2(80, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew11');
check_ok2(80, 2, $D);
check_var2(80, 3, $v, array('field' => 'data/mnew11',
  'field_type' => GD_CONST_ENTRY,
  'fragment_index' => 0, 'const_type' => GD_FLOAT64));

$v = gd_get_constant($D, 'data/mnew11', GD_FLOAT64);
check_ok2(80, 4, $D);
check_var2(80, 5, $v, 33.3);

# 81: get_string
$v = gd_get_string($D, 'string');
check_ok(81, $D);
check_var(81, $v, 'Zaphod Beeblebrox');

# 82: add string
$v = gd_add_string($D, 'new12', 'a string');
check_ok2(82, 0, $D);
check_var2(82, 1, $v, TRUE);

$v = gd_entry($D, 'new12');
check_ok2(82, 2, $D);
check_var2(82, 3, $v, array('field' => 'new12', 'field_type' => GD_STRING_ENTRY,
  'fragment_index' => 0));

$v = gd_get_string($D, 'new12');
check_ok2(82, 4, $D);
check_var2(82, 5, $v, 'a string');

# 83: add string
$v = gd_madd_string($D, 'data', 'mnew12', 'another string');
check_ok2(83, 0, $D);
check_var2(83, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew12');
check_ok2(83, 2, $D);
check_var2(83, 3, $v, array('field' => 'data/mnew12',
  'field_type' => GD_STRING_ENTRY, 'fragment_index' => 0));

$v = gd_get_string($D, 'data/mnew12');
check_ok2(83, 4, $D);
check_var2(83, 5, $v, 'another string');

# 84: add spec
$v = gd_add_spec($D, 'lorem STRING "Lorem ipsum"', 0);
check_ok2(84, 0, $D);
check_var2(84, 1, $v, TRUE);

$v = gd_entry($D, 'lorem');
check_ok2(84, 2, $D);
check_var2(84, 3, $v, array('field' => 'lorem', 'field_type' => GD_STRING_ENTRY,
  'fragment_index' => 0));

$v = gd_get_string($D, 'lorem');
check_ok2(84, 4, $D);
check_var2(84, 5, $v, 'Lorem ipsum');

# 85: madd spec
$v = gd_madd_spec($D, 'ipsum STRING "dolor sit amet."', 'lorem');
check_ok2(85, 0, $D);
check_var2(85, 1, $v, TRUE);

$v = gd_entry($D, 'lorem/ipsum');
check_ok2(85, 2, $D);
check_var2(85, 3, $v, array('field' => 'lorem/ipsum',
  'field_type' => GD_STRING_ENTRY, 'fragment_index' => 0));

$v = gd_get_string($D, 'lorem/ipsum');
check_ok2(85, 4, $D);
check_var2(85, 5, $v, 'dolor sit amet.');

# 86: put_constant (int)
$v = gd_put_constant($D, 'const', 86);
check_ok2(86, 0, $D);
check_var2(86, 1, $v, TRUE);

$v = gd_get_constant($D, 'const', GD_INT);
check_ok2(86, 2, $D);

# 91: put_constant (float)
$v = gd_put_constant($D, 'const', 13.1);
check_ok2(91, 0, $D);
check_var2(91, 1, $v, TRUE);

$v = gd_get_constant($D, 'const', GD_FLOAT);
check_ok2(91, 2, $D);
check_var2(91, 3, $v, 13.1);

# 93: put_constant (complex)
$v = gd_put_constant($D, 'const', array(93., 13.4));
check_ok2(93, 0, $D);
check_var2(93, 1, $v, TRUE);

$v = gd_get_constant($D, 'const', GD_COMPLEX128);
check_ok2(93, 2, $D);
check_var2(93, 3, $v, 93.);

# 94: put string
$v = gd_put_string($D, 'string', 'Arthur Dent');
check_ok2(94, 0, $D);
check_var2(94, 1, $v, TRUE);

$v = gd_get_string($D, 'string');
check_ok2(94, 2, $D);
check_var2(94, 3, $v, 'Arthur Dent');

# 95: nmfields by type
$v = gd_nmfields_by_type($D, 'data', GD_LINCOM_ENTRY);
check_ok(95, $D);
check_var(95, $v, 1);

# 96: mfield list by type
$v = gd_mfield_list_by_type($D, 'data', GD_LINCOM_ENTRY);
check_ok(96, $D);
check_var(96, $v, array('mnew3'));

# 97: nmvectors
$v = gd_nmvectors($D, 'data');
check_ok(97, $D);
check_var(97, $v, 8);

# 98: mvector list
$v = gd_mvector_list($D, 'data');
check_ok(98, $D);
check_var(98, $v, array('mlut', 'mnew3', 'mnew4', 'mnew6', 'mnew7', 'mnew8',
  'mnew9', 'mnew10'));

# 99: alter raw
$v = gd_alter_raw($D, 'new1', GD_INT32, null);
check_ok2(99, 0, $D);
check_var2(99, 1, $v, TRUE);

$v = gd_entry($D, 'new1');
check_ok2(99, 2, $D);
check_var2(99, 3, $v, array('field' => 'new1', 'field_type' => GD_RAW_ENTRY,
  'fragment_index' => 0, 'spf' => 3, 'data_type' => GD_INT32,
  'scalar' => array()));

# 101: alter lincom
$v = gd_alter_lincom($D, 'new3', null, array('in3', 'in4'), array(3.,
  array(4., 5.)));
check_ok2(101, 0, $D);
check_var2(101, 1, $v, TRUE);

$v = gd_entry($D, 'new3');
check_ok2(101, 1, $D);
check_var2(101, 2, $v, array('field' => 'new3',
  'field_type' => GD_LINCOM_ENTRY, 'fragment_index' => 0, 'n_fields' => 2,
  'in_fields' => array('in3', 'in4'), 'm' => array(3., array(4., 5.)),
  'b' => array(array(1.3, 1.4), 1.6), 'scalar' => array()));

# 103: alter polynom
$v = gd_alter_polynom($D, 'new4', 4, null, array(array(0, 1.), array(0, 2.),
  array(0, 3.), array(0, 4.), array(0, 5.)));
check_ok2(103, 0, $D);
check_var2(103, 1, $v, TRUE);

$v = gd_entry($D, 'new4');
check_ok2(103, 2, $D);
check_var2(103, 3, $v, array('field' => 'new4',
  'field_type' => GD_POLYNOM_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2'), 'poly_ord' => 4, 'a' => array(array(0., 1.),
  array(0., 2.), array(0., 3.), array(0., 4.), array(0., 5.)),
  'scalar' => array()));

# 104: alter linterp
$v = gd_alter_linterp($D, 'new6', null, './other/table');
check_ok2(104, 0, $D);
check_var2(104, 1, $v, TRUE);

$v = gd_entry($D, 'new6');
check_ok2(104, 2, $D);
check_var2(104, 3, $v, array('field' => 'new6',
  'field_type' => GD_LINTERP_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in'), 'table' => './other/table'));

# 105: alter_bit
$v = gd_alter_bit($D, 'new7', 'in3', null, 8);
check_ok2(105, 0, $D);
check_var2(105, 1, $v, TRUE);

$v = gd_entry($D, 'new7');
check_ok2(105, 2, $D);
check_var2(105, 3, $v, array('field' => 'new7', 'field_type' => GD_BIT_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in3'), 'bitnum' => 11,
  'numbits' => 8, 'scalar' => array()));

# 106: alter_bit
$v = gd_alter_sbit($D, 'new8', 'in1', 7);
check_ok2(106, 0, $D);
check_var2(106, 1, $v, TRUE);

$v = gd_entry($D, 'new8');
check_ok2(106, 2, $D);
check_var2(106, 3, $v, array('field' => 'new8', 'field_type' => GD_SBIT_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in1'), 'bitnum' => 7,
  'numbits' => 10, 'scalar' => array()));

# 107: alter_multiply
$v = gd_alter_multiply($D, 'new9', 'in1');
check_ok2(107, 0, $D);
check_var2(107, 1, $v, TRUE);

$v = gd_entry($D, 'new9');
check_ok2(107, 2, $D);
check_var2(107, 3, $v, array('field' => 'new9',
  'field_type' => GD_MULTIPLY_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in3')));

# 108: alter_phase
$v = gd_alter_phase($D, 'new10', 'in5');
check_ok2(108, 0, $D);
check_var2(108, 1, $v, TRUE);

$v = gd_entry($D, 'new10');
check_ok2(108, 2, $D);
check_var2(108, 3, $v, array('field' => 'new10', 'field_type' => GD_PHASE_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in5'), 'shift' => 62,
  'scalar' => array()));

# 109: alter_const
$v = gd_alter_const($D, 'new11', GD_FLOAT32);
check_ok2(109, 0, $D);
check_var2(109, 1, $v, TRUE);

$v = gd_entry($D, 'new11');
check_ok2(109, 2, $D);
check_var2(109, 3, $v, array('field' => 'new11', 'field_type' => GD_CONST_ENTRY,
  'fragment_index' => 0, 'const_type' => GD_FLOAT32));

# 110: encoding
$v = gd_encoding($D, 0);
check_ok(110, $D);
check_var(110, $v, GD_UNENCODED);

# 111: endianness
$v = gd_endianness($D, 0);
check_ok(111, $D);
check_var(111, $v, GD_LITTLE_ENDIAN | GD_NOT_ARM_ENDIAN);

# 112: dirfilename
$v = gd_dirfilename($D);
check_ok(112, $D);
check_eostring(112, $v, "dirfile");

# 113: parent fragment
$v = gd_parent_fragment($D, 1);
check_ok(113, $D);
check_var(113, $v, 0);

# 114: alter protection
$v = gd_alter_protection($D, GD_PROTECT_DATA, 1);
check_ok(114, $D);
check_var(114, $v, TRUE);

# 115: protection
$v = gd_protection($D, 1);
check_ok(115, $D);
check_var(115, $v, GD_PROTECT_DATA);

# 116: raw filename
$v = gd_raw_filename($D, 'data');
check_ok(116, $D);
check_eostring(116, $v, 'dirfile/data');

# 117: reference
$v = gd_reference($D, 'new1');
check_ok(117, $D);
check_var(117, $v, 'new1');

# 118: eof
$v = gd_eof($D, 'lincom');
check_ok(118, $D);
check_var(118, $v, 80);

# 119: alter_encoding
$v = gd_alter_encoding($D, GD_SLIM_ENCODED, 1);
check_ok2(119, 0, $D);
check_var2(119, 1, $v, TRUE);

$v = gd_encoding($D, 1);
check_ok2(119, 2, $D);
check_var2(119, 3, $v, GD_SLIM_ENCODED);

# 120: alter_encoding
$v = gd_alter_endianness($D, GD_BIG_ENDIAN, 1);
check_ok2(120, 0, $D);
check_var2(120, 1, $v, TRUE);

$v = gd_endianness($D, 1);
check_ok2(120, 2, $D);
check_var2(120, 3, $v, GD_BIG_ENDIAN | GD_NOT_ARM_ENDIAN);

# 121: alter spec
$v = gd_alter_spec($D, 'new10 PHASE in const');
check_ok2(121, 0, $D);
check_var2(121, 1, $v, TRUE);

$v = gd_entry($D, 'new10');
check_ok2(121, 2, $D);
check_var2(121, 3, $v, array('field' => 'new10', 'field_type' => GD_PHASE_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in'), 'shift' => 93,
  'scalar' => array(array('const', -1))));

# 122: delete
$v = gd_delete($D, 'new10');
check_ok2(122, 0, $D);
check_var2(122, 1, $v, TRUE);

$v = gd_entry($D, 'new10');
check_error2(122, 2, $D, GD_E_BAD_CODE);
check_var2(122, 3, $v, FALSE);

# 123: malter spec
$v = gd_malter_spec($D, 'mnew10 PHASE in4 11', 'data');
check_ok2(123, 0, $D);
check_var2(123, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew10');
check_ok2(123, 2, $D);
check_var2(123, 3, $v, array('field' => 'data/mnew10',
  'field_type' => GD_PHASE_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in4'), 'shift' => 11, 'scalar' => array()));

# 124: move
$v = gd_move($D, 'new9', 1);
check_ok2(124, 0, $D);
check_var2(124, 1, $v, TRUE);

$v = gd_entry($D, 'new9');
check_ok2(124, 2, $D);
check_var2(124, 3, $v, array('field' => 'new9',
  'field_type' => GD_MULTIPLY_ENTRY, 'fragment_index' => 1,
  'in_fields' => array('in1', 'in3')));

# 125: rename
$v = gd_rename($D, 'new9', 'newer');
check_ok2(125, 0, $D);
check_var2(125, 1, $v, TRUE);

$v = gd_entry($D, 'new9');
check_error2(125, 2, $D, GD_E_BAD_CODE);
check_var2(125, 3, $v, FALSE);

$v = gd_entry($D, 'newer');
check_ok2(125, 4, $D);
check_var2(125, 5, $v, array('field' => 'newer',
  'field_type' => GD_MULTIPLY_ENTRY, 'fragment_index' => 1,
  'in_fields' => array('in1', 'in3')));

# 126: uninclude
$v = gd_uninclude($D, 1);
check_ok2(126, 0, $D);
check_var2(126, 1, $v, TRUE);

$v = gd_entry($D, 'newer');
check_error2(126, 2, $D, GD_E_BAD_CODE);
check_var2(126, 3, $v, FALSE);

# 127: frameoffset
$v = gd_frameoffset($D, 0);
check_ok(127, $D);
check_var(127, $v, 0);

# 128: alter frameoffset
$v = gd_alter_frameoffset($D, 33, 0);
check_ok2(128, 0, $D);
check_var2(128, 1, $v, TRUE);

$v = gd_frameoffset($D, 0);
check_ok2(128, 2, $D);
check_var2(128, 3, $v, 33);

# 129: native type
$v = gd_native_type($D, 'data');
check_ok(129, $D);
check_var(129, $v, GD_INT8);

# 131: validate
$v = gd_validate($D, 'new7');
check_error(131, $D, GD_E_BAD_CODE);
check_var(131, $v, FALSE);

# 133: framenum_subset
$v = gd_framenum($D, 'data', 33.3, 6);
check_ok(133, $D);
check_var(133, $v, 37.0375);

# 135: gd_add
$v = gd_add($D, array('field' => 'new135', 'field_type' => GD_RAW_ENTRY,
  'fragment_index' => 0, 'data_type' => GD_FLOAT32, 'spf' => 5));
check_ok2(135, 0, $D);
check_var2(135, 1, $v, TRUE);

$v = gd_entry($D, 'new135');
check_ok2(135, 2, $D);
check_var2(135, 3, $v, array('field' => 'new135', 'field_type' => GD_RAW_ENTRY,
  'fragment_index' => 0, 'spf' => 5, 'data_type' => GD_FLOAT32,
  'scalar' => array()));

# 136: madd
$v = gd_madd($D, array('field' => 'mnew136',
  'field_type' => GD_PHASE_ENTRY, 'in_fields' => 'data',
  'shift' => 23), 'data');
check_ok2(136, 0, $D);
check_var2(136, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew136');
check_ok2(136, 2, $D);
check_var2(136, 3, $v, array('field' => 'data/mnew136',
  'field_type' => GD_PHASE_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('data'), 'shift' => 23, 'scalar' => array()));

# 141: alter_entry
$v = gd_alter_entry($D, 'new135', array('field_type' => GD_RAW_ENTRY,
  'data_type' => GD_FLOAT64, 'spf' => 141));
check_ok2(141, 0, $D);
check_var2(141, 1, $v, TRUE);

$v = gd_entry($D, 'new135');
check_ok2(141, 2, $D);
check_var2(141, 3, $v, array('field' => 'new135', 'field_type' => GD_RAW_ENTRY,
  'fragment_index' => 0, 'spf' => 141, 'data_type' => GD_FLOAT64,
  'scalar' => array()));

# 142: bof
$v = gd_bof($D, 'lincom');
check_ok(142, $D);
check_var(142, $v, 264);

# 143: divide entry
$v = gd_entry($D, 'div');
check_ok(143, $D);
check_var(143, $v, array('field' => 'div', 'field_type' => GD_DIVIDE_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('mult', 'bit')));

# 145: recip entry
$v = gd_entry($D, 'recip');
check_ok(145, $D);
check_var(145, $v, array('field' => 'recip', 'field_type' => GD_RECIP_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('div'),
  'dividend' => array(6.5, 4.3), 'scalar' => array()));

# 146: add divide
$v = gd_add_divide($D, 'new14', 'in2', 'in3');
check_ok2(146, 0, $D);
check_var2(146, 1, $v, TRUE);

$v = gd_entry($D, 'new14');
check_ok2(146, 2, $D);
check_var2(146, 3, $v, array('field' => 'new14',
  'field_type' => GD_DIVIDE_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 148: add recip
$v = gd_add_recip($D, 'new16', 'in2', array(33.3, 44.4));
check_ok2(148, 0, $D);
check_var2(148, 1, $v, TRUE);

$v = gd_entry($D, 'new16');
check_ok2(148, 2, $D);
check_var2(148, 3, $v, array('field' => 'new16', 'field_type' => GD_RECIP_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in2'),
  'dividend' => array(33.3, 44.4), 'scalar' => array()));

# 149: madd divide
$v = gd_madd_divide($D, 'data', 'mnew14', 'in2', 'in3');
check_ok2(149, 0, $D);
check_var2(149, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew14');
check_ok2(149, 2, $D);
check_var2(149, 3, $v, array('field' => 'data/mnew14',
  'field_type' => GD_DIVIDE_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 151: madd recip
$v = gd_madd_recip($D, 'data', 'mnew16', 'in2', 151);
check_ok2(148, 0, $D);
check_var2(148, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew16');
check_ok2(148, 2, $D);
check_var2(148, 3, $v, array('field' => 'data/mnew16',
  'field_type' => GD_RECIP_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2'), 'dividend' => 151.,
  'scalar' => array()));

# 152: alter_divide
$v = gd_alter_divide($D, 'new14', 'in1');
check_ok2(152, 0, $D);
check_var2(152, 1, $v, TRUE);

$v = gd_entry($D, 'new14');
check_ok2(152, 2, $D);
check_var2(152, 3, $v, array('field' => 'new14',
  'field_type' => GD_DIVIDE_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in3')));

# 154: alter recip
$v = gd_alter_recip($D, 'new16', 'in6');
check_ok2(154, 0, $D);
check_var2(154, 1, $v, TRUE);

$v = gd_entry($D, 'new16');
check_ok2(154, 2, $D);
check_var2(154, 3, $v, array('field' => 'new16', 'field_type' => GD_RECIP_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in6'),
  'dividend' => array(33.3, 44.4), 'scalar' => array()));

# 155: rewrite fragment
$v = gd_rewrite_fragment($D);
check_ok(155, $D);
check_var(155, $v, TRUE);

# 156: invalid dirfile
$v = gd_invalid_dirfile();
check_ok2(156, 0, $v);

gd_nfragments($v);
check_error(156, $v, GD_E_BAD_DIRFILE);

gd_close($v);

# 157: dirfile standards
$v = gd_dirfile_standards($D);
check_ok2(157, 0, $D);
check_var2(157, 1, $v, GD_DIRFILE_STANDARDS_VERSION);

$v = gd_dirfile_standards($D, 0);
check_error2(157, 2, $D, GD_E_ARGUMENT);
check_var2(157, 3, $v, FALSE);

# 158: get carray
$v = gd_get_carray($D, 'carray', GD_INT, null, null, TRUE);
check_ok(158, $D);
check_var(158, $v, array(1, 2, 3, 4, 5, 6));

# 159: get carray slice
$v = gd_get_carray($D, 'carray', GD_INT8, 2, 2);
check_ok(159, $D);
check_var(159, $v, "\03\04");

# 167: gd_carrays
$v = gd_carrays($D, GD_INT8, TRUE);
check_ok(167, $D);
check_var(167, $v, array(array(1, 2, 3, 4, 5, 6)));

# 168: gd_put_carray
$v = gd_put_carray($D, 'carray', array(9, 8, 7, 6, 5, 4));
check_ok2(168, 0, $D);
check_var2(168, 1, $v, TRUE);

$v = gd_get_carray($D, 'carray', GD_INT8);
check_ok2(168, 2, $D);
check_var2(168, 3, $v, "\011\010\07\06\05\04");

# 169: gd_put_carray_slice
$v = gd_put_carray($D, 'carray', 2, array(169, 169));
check_ok2(169, 0, $D);
check_var2(169, 1, $v, TRUE);

$v = gd_get_carray($D, 'carray', GD_INT, null, null, TRUE);
check_ok2(169, 2, $D);
check_var2(169, 3, $v, array(9, 8, 169, 169, 5, 4));

# 177: array len
$v = gd_array_len($D, 'carray');
check_ok(177, $D);
check_var(177, $v, 6);

# 178: carray entry
$v = gd_entry($D, 'carray');
check_ok(178, $D);
check_var(178, $v, array('field' => 'carray', 'field_type' => GD_CARRAY_ENTRY,
  'fragment_index' => 0, 'array_len' => 6, 'const_type' => GD_FLOAT64));

# 179: add carray
$v = gd_add_carray($D, 'new17', GD_INT32, GD_INT8, "\x1\x7\x9");
check_ok2(179, 0, $D);
check_var2(179, 1, $v, TRUE);

$v = gd_entry($D, 'new17');
check_ok2(179, 2, $D);
check_var2(179, 3, $v, array('field' => 'new17',
  'field_type' => GD_CARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 3,
  'const_type' => GD_INT32));

$v = gd_get_carray($D, 'new17', GD_INT8);
check_ok2(179, 4, $D);
check_var2(179, 5, $v, "\x1\x7\x9");

# 180: madd carray
$v = gd_madd_carray($D, 'data', 'mnew17', GD_COMPLEX128, array(array(1, 8),
  18));
check_ok2(180, 0, $D);
check_var2(180, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew17');
check_ok2(180, 2, $D);
check_var2(180, 3, $v, array('field' => 'data/mnew17',
  'field_type' => GD_CARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 2,
  'const_type' => GD_COMPLEX128));

$v = gd_get_carray($D, 'data/mnew17', GD_COMPLEX128, null, null, TRUE);
check_ok2(180, 4, $D);
check_var2(180, 5, $v, array(array(1., 8.), 18.));

# 181: alter carray
$v = gd_alter_carray($D, 'new17', null, 12);
check_ok2(181, 0, $D);
check_var2(181, 1, $v, TRUE);

$v = gd_entry($D, 'new17');
check_ok2(181, 2, $D);
check_var2(181, 3, $v, array('field' => 'new17',
  'field_type' => GD_CARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 12,
  'const_type' => GD_INT32));

# 183: constants
$v = gd_constants($D, GD_FLOAT64, TRUE);
check_ok(183, $D);
check_var(183, $v, array(93., 33.3));

# 191: mconstants
$v = gd_mconstants($D, 'data', GD_FLOAT64, TRUE);
check_ok(191, $D);
check_var(191, $v, array(3.3, 33.3)); # what's this weird obsession with three?

# 199: strings
$v = gd_strings($D);
check_ok(199, $D);
check_var(199, $v, array('Lorem ipsum', 'a string', 'Arthur Dent'));

# 200: mstrings
$v = gd_mstrings($D, 'data');
check_ok(200, $D);
check_var(200, $v, array('This is a string constant.', 'another string'));

# 203: seek
$v = gd_seek($D, 'data', 35, 0, GD_SEEK_SET);
check_ok2(203, 0, $D);
check_var2(203, 1, $v, 280);

$v = gd_getdata($D, 'data', GD_HERE, 0, 1, 0, GD_UINT8);
check_ok2(203, 2, $D);
check_var2(203, 3, $v, "\x11\x12\x13\x14\x15\x16\x17\x18");

# 204: tell
$v = gd_tell($D, 'data');
check_ok(204, $D);
check_var(204, $v, 288);

# 205: hide
$v = gd_hide($D, 'data');
check_ok(205, $D);
check_var(205, $v, TRUE);

# 206: hidden
$v = gd_hidden($D, 'data');
check_ok(206, $D);
check_var(206, $v, TRUE);

# 207: unhide
$v = gd_unhide($D, 'data');
check_ok2(207, 0, $D);
check_var2(207, 1, $v, TRUE);

$v = gd_hidden($D, 'data');
check_ok2(207, 2, $D);
check_var2(207, 3, $v, FALSE);

# 208: sync
$v = gd_sync($D, 'data');
check_ok(208, $D);
check_var(208, $v, TRUE);

# 209: flush
$v = gd_flush($D, 'data');
check_ok(209, $D);
check_var(209, $v, TRUE);

# 210: metaflush
$v = gd_metaflush($D);
check_ok(210, $D);
check_var(210, $v, TRUE);

# 211: window entry
$v = gd_entry($D, 'window');
check_ok(211, $D);
check_var(211, $v, array('field' => 'window', 'field_type' => GD_WINDOW_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('linterp', 'mult'),
  'windop' => GD_WINDOP_LT, 'threshold' => 4.1, 'scalar' => array()));

# 212: add window
$v = gd_add_window($D, 'new18', 'in1', 'in2', GD_WINDOP_NE, 32);
check_ok2(212, 0, $D);
check_var2(212, 1, $v, TRUE);

$v = gd_entry($D, 'new18');
check_ok2(212, 2, $D);
check_var2(212, 3, $v, array('field' => 'new18',
  'field_type' => GD_WINDOW_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in2'), 'windop' => GD_WINDOP_NE,
  'threshold' => 32, 'scalar' => array()));

# 214: madd window
$v = gd_madd_window($D, 'data', 'mnew18', 'in1', 'in2', GD_WINDOP_SET, 0x214);
check_ok2(214, 0, $D);
check_var2(214, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew18');
check_ok2(214, 2, $D);
check_var2(214, 3, $v, array('field' => 'data/mnew18',
  'field_type' => GD_WINDOW_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in2'), 'windop' => GD_WINDOP_SET,
  'threshold' => 0x214, 'scalar' => array()));

# 217: alter window
$v = gd_alter_window($D, 'new18', 'in3', null, GD_WINDOP_EQ);
check_ok2(217, 0, $D);
check_var2(217, 1, $v, TRUE);

$v = gd_entry($D, 'new18');
check_ok2(217, 2, $D);
check_var2(217, 3, $v, array('field' => 'new18',
  'field_type' => GD_WINDOW_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in3', 'in2'), 'windop' => GD_WINDOP_EQ,
  'threshold' => 32, 'scalar' => array()));

# 218: alias target
$v = gd_alias_target($D, 'alias');
check_ok(218, $D);
check_var(218, $v, 'data');

# 219: add alias
$v = gd_add_alias($D, 'new20', 'data');
check_ok2(219, 0, $D);
check_var2(219, 1, $v, TRUE);

$v = gd_alias_target($D, 'new20');
check_ok2(219, 2, $D);
check_var2(219, 3, $v, 'data');

# 220: add alias
$v = gd_madd_alias($D, 'data', 'mnew20', 'data');
check_ok2(220, 0, $D);
check_var2(220, 1, $v, TRUE);

$v = gd_alias_target($D, 'data/mnew20');
check_ok2(220, 2, $D);
check_var2(220, 3, $v, 'data');

# 221: naliases
$v = gd_naliases($D, 'data');
check_ok(221, $D);
check_var(221, $v, 4);

# 222: aliases
$v = gd_aliases($D, 'data');
check_ok(222, $D);
check_var(222, $v, array( 'data', 'alias', 'new20', 'data/mnew20' ));

# 223: gd_include_affix
$v = gd_include_affix($D, 'format1', 0, 'A', 'Z', GD_CREAT | GD_EXCL);
check_ok2(223, 0, $D);
check_var2(223, 1, $v, 1);

# 226: fragment affixes
$v = gd_fragment_affixes($D, 1);
check_ok(226, $D);
check_var(226, $v, array('A', 'Z'));

# 227: alter affixes
$v = gd_alter_affixes($D, 1, "B", "");
check_ok2(227, 0, $D);
check_var2(227, 1, $v, TRUE);

$v = gd_fragment_affixes($D, 1);
check_ok2(227, 2, $D);
check_var2(227, 3, $v, array('B', ''));

# 228: mplex entry
$v = gd_entry($D, 'mplex');
check_ok(228, $D);
check_var(228, $v, array('field' => 'mplex', 'field_type' => GD_MPLEX_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('data', 'sbit'), 'count_val' => 1,
  'period' => 10, 'scalar' => array()));

# 229: add mplex
$v = gd_add_mplex($D, 'new21', 'in1', 'in2', 5, 6);
check_ok2(229, 0, $D);
check_var2(229, 1, $v, TRUE);

$v = gd_entry($D, 'new21');
check_ok2(229, 2, $D);
check_var2(229, 3, $v, array('field' => 'new21', 'field_type' => GD_MPLEX_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in1', 'in2'), 'count_val' => 5,
  'period' => 6, 'scalar' => array()));

# 230: add mplex
$v = gd_madd_mplex($D, 'data', 'mnew21', 'in1', 'in2', 5, 6);
check_ok2(230, 0, $D);
check_var2(230, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew21');
check_ok2(230, 2, $D);
check_var2(230, 3, $v, array('field' => 'data/mnew21',
  'field_type' => GD_MPLEX_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in2'), 'count_val' => 5, 'period' => 6,
  'scalar' => array()));

# 231: alter mplex
$v = gd_alter_mplex($D, 'new21', 'in3', null, null, null);
check_ok2(231, 0, $D);
check_var2(231, 1, $v, TRUE);

$v = gd_entry($D, 'new21');
check_ok2(231, 2, $D);
check_var2(231, 3, $v, array('field' => 'new21', 'field_type' => GD_MPLEX_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in3', 'in2'), 'count_val' => 5,
  'period' => 6, 'scalar' => array()));

# 232: strtok
$v = gd_strtok($D, '"test1 test2" test3\ test4 test5');
check_ok(232, $D);
check_var(232, $v, array('test1 test2', 'test3 test4', 'test5'));

# 233: raw_close
$v = gd_raw_close($D, 'data');
check_ok(233, $D);
check_var(233, $v, TRUE);

# 234: desync
$v = gd_desync($D);
check_ok(234, $D);
check_var(234, $v, FALSE);

# 235: flags
$v = gd_flags($D, GD_PRETTY_PRINT);
check_ok(235, $D);
check_var(235, $v, GD_PRETTY_PRINT);

# 236: verbose_prefix
$v = gd_verbose_prefix($D, 'big_test: ');
check_ok(236, $D);
check_var(236, $v, TRUE);

# 237: nentries
$v = gd_nentries($D, 'data', GD_SCALAR_ENTRIES,
  GD_ENTRIES_HIDDEN | GD_ENTRIES_NOALIAS);
check_ok(237, $D);
check_var(237, $v, 7);

# 239: entry_list
$v = gd_entry_list($D, null, GD_VECTOR_ENTRIES, GD_ENTRIES_NOALIAS);
check_ok(239, $D);
check_var(239, $v, array( 'bit', 'div', 'data', 'mult', 'new1', 'new3', 'new4',
  'new6', 'new7', 'new8', 'sbit', 'INDEX', 'indir', 'mplex', 'new14', 'new16',
  'new18', 'new21', 'phase', 'recip', 'lincom', 'new135', 'window', 'linterp',
  'polynom' ));

# 240: mplex lookback
$v = gd_mplex_lookback($D, GD_LOOKBACK_ALL);
check_ok(240, $D);
check_var(240, $v, true);

# 241: linterp tablename
$v = gd_linterp_tablename($D, 'linterp');
check_ok(241, $D);
check_eostring(241, $v, 'dirfile/lut');

# 242: mcarrays
$v = gd_mcarrays($D, 'data', GD_FLOAT64, TRUE);
check_ok(242, $D);
check_var(242, $v, array(array(1.9, 2.8, 3.7, 4.6, 5.5), array(1., 18.)));

# 243: add lincom
$v = gd_add($D, array('field' => 'new243', 'field_type' => GD_LINCOM_ENTRY,
  'in_fields' => array('in1', 'in2', 'in3'), 'm' => array(1.1, 2 => 1.4),
  'scalar' => array(1 => array('const'), null, array('carray', 3),
  array('carray', 4), array('carray', 5)), 'fragment_index' => 0));
check_ok2(243, 0, $D);
check_var2(243, 1, $v, TRUE);

$v = gd_entry($D, 'new243');
check_ok2(243, 2, $D);
check_var2(243, 3, $v, array('field' => 'new243',
  'field_type' => GD_LINCOM_ENTRY, 'fragment_index' => 0, 'n_fields' => 3,
  'in_fields' => array('in1', 'in2', 'in3'), 'm' => array(1.1, 93., 1.4),
  'b' => array(169., 5., 4.), 'scalar' => array(1 => array('const', -1),
  3 => array('carray', 3), array('carray', 4), array('carray', 5))));

# 244: add polynom
$v = gd_add($D, array('field' => 'new244', 'in_fields' => 'in2',
  'field_type' => GD_POLYNOM_ENTRY, 'a' => array(33., array(44., 55.), 66.),
  'scalar' => array(3 => array('carray', 0)), 'fragment_index' => 0));
check_ok2(244, 0, $D);
check_var2(244, 1, $v, TRUE);

$v = gd_entry($D, 'new244');
check_ok2(244, 2, $D);
check_var2(244, 3, $v, array('field' => 'new244',
  'field_type' => GD_POLYNOM_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2'), 'poly_ord' => 3, 'a' => array(33.,
  array(44., 55.), 66., 9.), 'scalar' => array(3 => array('carray', 0))));

# 245: add linterp
$v = gd_add($D, array('field' => 'new245', 'field_type' => GD_LINTERP_ENTRY,
  'in_fields' => 'in', 'table' => './some/table', 'fragment_index' => 0));
check_ok2(245, 0, $D);
check_var2(245, 1, $v, TRUE);

$v = gd_entry($D, 'new245');
check_ok2(245, 2, $D);
check_var2(245, 3, $v, array('field' => 'new245',
  'field_type' => GD_LINTERP_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in'), 'table' => './some/table'));

# 246: add bit
$v = gd_add($D, array('field' => 'new246', 'field_type' => GD_BIT_ENTRY,
  'in_fields' => array('in1'), 'bitnum' => 11, 'fragment_index' => 0));
check_ok2(246, 0, $D);
check_var2(246, 1, $v, TRUE);

$v = gd_entry($D, 'new246');
check_ok2(246, 2, $D);
check_var2(246, 3, $v, array('field' => 'new246', 'field_type' => GD_BIT_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('in1'), 'bitnum' => 11,
  'numbits' => 1, 'scalar' => array()));

# 247: add multiply
$v = gd_add($D, array('field' => 'new247', 'fragment_index' => 0,
  'field_type' => GD_MULTIPLY_ENTRY, 'in_fields' => array('in2', 'in3')));
check_ok2(247, 0, $D);
check_var2(247, 1, $v, TRUE);

$v = gd_entry($D, 'new247');
check_ok2(247, 2, $D);
check_var2(247, 3, $v, array('field' => 'new247',
  'field_type' => GD_MULTIPLY_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 248: add phase
$v = gd_add($D, array('field' => 'new248', 'in_fields' =>  'in6',
  'field_type' => GD_PHASE_ENTRY, 'fragment_index' => 0,
  'scalar' => array(array('carray', 1))));
check_ok2(248, 0, $D);
check_var2(248, 1, $v, TRUE);

$v = gd_entry($D, 'new248');
check_ok2(248, 2, $D);
check_var2(248, 3, $v, array('field' => 'new248',
  'field_type' => GD_PHASE_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in6'), 'shift' => 8,
  'scalar' => array(array('carray', 1))));

# 249: add const
$v = gd_add($D, array('field' => 'new249', 'field_type' => GD_CONST_ENTRY,
  'fragment_index' => 0, 'const_type' => GD_UINT8));
check_ok2(249, 0, $D);
check_var2(249, 1, $v, TRUE);

$v = gd_entry($D, 'new249');
check_ok2(249, 2, $D);
check_var2(249, 3, $v, array('field' => 'new249',
  'field_type' => GD_CONST_ENTRY,
  'fragment_index' => 0, 'const_type' => GD_UINT8));

$v = gd_get_constant($D, 'new249', GD_FLOAT64);
check_ok2(249, 4, $D);
check_var2(249, 5, $v, 0.);

# 250: add string
$v = gd_add($D, array('field' => 'new250', 'field_type' => GD_STRING_ENTRY,
  'fragment_index' => 0));
check_ok2(250, 0, $D);
check_var2(250, 1, $v, TRUE);

$v = gd_entry($D, 'new250');
check_ok2(250, 2, $D);
check_var2(250, 3, $v, array('field' => 'new250',
  'field_type' => GD_STRING_ENTRY, 'fragment_index' => 0));

$v = gd_get_string($D, 'new250');
check_ok2(250, 4, $D);
check_var2(250, 5, $v, '');

# 251: add recip
$v = gd_add($D, array('field' => 'Bnew251', 'field_type' => GD_RECIP_ENTRY,
  'in_fields' => 'Bin2', 'dividend' => array(33.3, 44.4),
  'fragment_index' => 1, 'scalar' => null));
check_ok2(251, 0, $D);
check_var2(251, 1, $v, TRUE);

$v = gd_entry($D, 'Bnew251');
check_ok2(251, 2, $D);
check_var2(251, 3, $v, array('field' => 'Bnew251',
  'field_type' => GD_RECIP_ENTRY,
  'fragment_index' => 1, 'in_fields' => array('Bin2'),
  'dividend' => array(33.3, 44.4), 'scalar' => array()));

# 252: add carray
$v = gd_add($D, array('field' => 'new252', 'field_type' => GD_CARRAY_ENTRY,
  'fragment_index' => 0, 'const_type' => GD_INT32, 'array_len' => 5));
check_ok2(252, 0, $D);
check_var2(252, 1, $v, TRUE);

$v = gd_entry($D, 'new252');
check_ok2(252, 2, $D);
check_var2(252, 3, $v, array('field' => 'new252',
  'field_type' => GD_CARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 5,
  'const_type' => GD_INT32));

$v = gd_get_carray($D, 'new252', GD_INT8);
check_ok2(252, 4, $D);
check_var2(252, 5, $v, "\0\0\0\0\0");

# 253: add window
$v = gd_add($D, array('field' => 'new253', 'in_fields' => array('in1', 'in2'),
  'windop' => GD_WINDOP_NE, 'threshold' => 32, 'fragment_index' => 0,
  'field_type' => GD_WINDOW_ENTRY));
check_ok2(253, 0, $D);
check_var2(253, 1, $v, TRUE);

$v = gd_entry($D, 'new253');
check_ok2(253, 2, $D);
check_var2(253, 3, $v, array('field' => 'new253',
  'field_type' => GD_WINDOW_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in2'), 'windop' => GD_WINDOP_NE,
  'threshold' => 32, 'scalar' => array()));

# 254: add mplex
$v = gd_add($D, array('field' => 'new254', 'in_fields' => array('in1', 'in2'),
  'fragment_index' => 0, 'field_type' => GD_MPLEX_ENTRY, 'count_val' =>  5));
check_ok2(254, 0, $D);
check_var2(254, 1, $v, TRUE);

$v = gd_entry($D, 'new254');
check_ok2(254, 2, $D);
check_var2(254, 3, $v, array('field' => 'new254',
  'field_type' => GD_MPLEX_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in2'), 'count_val' => 5,
  'period' => 0, 'scalar' => array()));

# 259: alter_entry with scalar
$v = gd_alter_entry($D, 'new243', array('field_type' => GD_LINCOM_ENTRY,
  'scalar' => array(null, array(''), array('const'), array('carray', 4),
  null, array('const', -1)), 'fragment_index' => 0));
check_ok2(259, 0, $D);
check_var2(259, 1, $v, TRUE);

$v = gd_entry($D, 'new243');
check_ok2(259, 2, $D);
check_var2(259, 3, $v, array('field' => 'new243',
  'field_type' => GD_LINCOM_ENTRY, 'fragment_index' => 0, 'n_fields' => 3,
  'in_fields' => array('in1', 'in2', 'in3'), 'm' => array(1.1, 93., 93.),
  'b' => array(5., 5., 93.), 'scalar' => array(1 => array('const', -1),
  array('const', -1), array('carray', 4), 5 => array('const', -1))));

# 270: dirfilekey
$v = gd_dirfilekey($D);
check_ok(270, $D);
check_var(270, $v, "dirfile\0extra");

# 271: encoding_support
$v = gd_encoding_support(GD_SIE_ENCODED);
check_var(271, $v, GD_RDWR);

# 272: NULL return from gd_reference
$D2 = gd_open('dirfile/empty', GD_RDWR | GD_CREAT | GD_EXCL);
check_ok2(272, 1, $D2);

$v = gd_reference($D2);
check_ok2(272, 2, $D2);
check_var(272, $v, null);

gd_discard($D2);

# 273: get carray (NULL)
$v = gd_get_carray($D, 'carray', GD_NULL);
check_ok(273, $D);
check_var(273, $v, true);

# 274: get carray slice (NULL)
$v = gd_get_carray($D, 'carray', GD_NULL, 2, 2);
check_ok(274, $D);
check_var(274, $v, true);

# 277: gd_entry (SARRAY)
$v = gd_entry($D, 'sarray');
check_ok(277, $D);
check_var(277, $v, array('field' => 'sarray', 'field_type' => GD_SARRAY_ENTRY,
  'fragment_index' => 0, 'array_len' => 7));

# 278: get carray
$v = gd_get_sarray($D, 'sarray');
check_ok(278, $D);
check_var(278, $v, array('one', 'two', 'three', 'four', 'five', 'six',
  'seven'));

# 279: get sarray slice
$v = gd_get_sarray($D, 'sarray', 2, 2);
check_ok(279, $D);
check_var(279, $v, array('three', 'four'));

# 280: gd_sarrays
$v = gd_sarrays($D);
check_ok(280, $D);
check_var(280, $v, array(array('one', 'two', 'three', 'four', 'five', 'six',
    'seven')));

# 281: gd_put_sarray
$v = gd_put_sarray($D, 'sarray', array('eka', 'dvi', 'tri', 'catur', 'panca',
  'sas', 'sapta'));
check_ok2(281, 0, $D);
check_var2(281, 1, $v, TRUE);

$v = gd_get_sarray($D, 'sarray');
check_ok2(281, 2, $D);
check_var2(168, 3, $v, array('eka', 'dvi', 'tri', 'catur', 'panca', 'sas',
  'sapta'));

# 282: gd_put_sarray_slice
$v = gd_put_sarray($D, 'sarray', 2, array('asta', 'nava'));
check_ok2(282, 0, $D);
check_var2(282, 1, $v, TRUE);

$v = gd_get_sarray($D, 'sarray');
check_ok2(282, 2, $D);
check_var2(282, 3, $v, array('eka', 'dvi', 'asta', 'nava', 'panca', 'sas',
    'sapta'));

# 283: add sarray
$v = gd_add_sarray($D, 'new283', array('eins', 'zwei', 'drei'), 0);
check_ok2(283, 0, $D);
check_var2(283, 1, $v, TRUE);

$v = gd_entry($D, 'new283');
check_ok2(283, 2, $D);
check_var2(283, 3, $v, array('field' => 'new283',
  'field_type' => GD_SARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 3));

$v = gd_get_sarray($D, 'new283');
check_ok2(283, 4, $D);
check_var2(283, 5, $v, array('eins', 'zwei', 'drei'));

# 284: add (SARRAY)
$v = gd_add($D, array('field' => 'new284', 'field_type' => GD_SARRAY_ENTRY,
  'fragment_index' => 0, 'array_len' => 5));
check_ok2(284, 0, $D);
check_var2(284, 1, $v, TRUE);

$v = gd_entry($D, 'new284');
check_ok2(284, 2, $D);
check_var2(284, 3, $v, array('field' => 'new284',
  'field_type' => GD_SARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 5));

$v = gd_get_sarray($D, 'new284');
check_ok2(284, 4, $D);
check_var2(284, 5, $v, array('', '', '', '', ''));

# 285: madd sarray
$v = gd_madd_sarray($D, 'data', 'mnew285', array('un', 'deux', 'trois'));
check_ok2(285, 0, $D);
check_var2(285, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew285');
check_ok2(285, 2, $D);
check_var2(285, 3, $v, array('field' => 'data/mnew285',
  'field_type' => GD_SARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 3));

$v = gd_get_sarray($D, 'data/mnew285');
check_ok2(285, 4, $D);
check_var2(285, 5, $v, array('un', 'deux', 'trois'));

# 286: alter sarray
$v = gd_alter_sarray($D, 'new283', 2);
check_ok2(286, 0, $D);
check_var2(286, 1, $v, TRUE);

$v = gd_entry($D, 'new283');
check_ok2(286, 2, $D);
check_var2(286, 3, $v, array('field' => 'new283',
  'field_type' => GD_SARRAY_ENTRY, 'fragment_index' => 0, 'array_len' => 2));

# 287: msarrays
$v = gd_msarrays($D, 'data');
check_ok(287, $D);
check_var(287, $v, array(array('eight', 'nine', 'ten', 'eleven', 'twelve'),
  array('un', 'deux', 'trois')));

# 288: INDIR entry
$v = gd_entry($D, 'indir');
check_ok(288, $D);
check_var(288, $v, array('field' => 'indir', 'field_type' => GD_INDIR_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('data', 'carray')));

# 289: add indir
$v = gd_add_indir($D, 'new289', 'in2', 'in3');
check_ok2(289, 0, $D);
check_var2(289, 1, $v, TRUE);

$v = gd_entry($D, 'new289');
check_ok2(289, 2, $D);
check_var2(289, 3, $v, array('field' => 'new289',
  'field_type' => GD_INDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 290: madd indir
$v = gd_madd_indir($D, 'data', 'mnew290', 'in2', 'in3');
check_ok2(290, 0, $D);
check_var2(290, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew290');
check_ok2(290, 2, $D);
check_var2(290, 3, $v, array('field' => 'data/mnew290',
  'field_type' => GD_INDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 291: alter_indir
$v = gd_alter_indir($D, 'new289', 'in1');
check_ok2(291, 0, $D);
check_var2(291, 1, $v, TRUE);

$v = gd_entry($D, 'new289');
check_ok2(291, 2, $D);
check_var2(291, 3, $v, array('field' => 'new289',
  'field_type' => GD_INDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in3')));

# 292: SINDIR entry
$v = gd_entry($D, 'sindir');
check_ok(292, $D);
check_var(292, $v, array('field' => 'sindir', 'field_type' => GD_SINDIR_ENTRY,
  'fragment_index' => 0, 'in_fields' => array('data', 'sarray')));

# 293: add sindir
$v = gd_add_sindir($D, 'new293', 'in2', 'in3');
check_ok2(293, 0, $D);
check_var2(293, 1, $v, TRUE);

$v = gd_entry($D, 'new293');
check_ok2(293, 2, $D);
check_var2(293, 3, $v, array('field' => 'new293',
  'field_type' => GD_SINDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 294: madd sindir
$v = gd_madd_sindir($D, 'data', 'mnew294', 'in2', 'in3');
check_ok2(294, 0, $D);
check_var2(294, 1, $v, TRUE);

$v = gd_entry($D, 'data/mnew294');
check_ok2(294, 2, $D);
check_var2(294, 3, $v, array('field' => 'data/mnew294',
  'field_type' => GD_SINDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in2', 'in3')));

# 295: alter_sindir
$v = gd_alter_sindir($D, 'new293', 'in1');
check_ok2(295, 0, $D);
check_var2(295, 1, $v, TRUE);

$v = gd_entry($D, 'new293');
check_ok2(295, 2, $D);
check_var2(295, 3, $v, array('field' => 'new293',
  'field_type' => GD_SINDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in1', 'in3')));

# 296: getdata (SINDIR)
$v = gd_getdata($D, 'sindir', 0, 0, 1, 0);
check_ok(296, $D);
check_var(296, $v, array('eka', 'eka', 'eka', 'eka', 'eka', 'eka', 'eka',
  'eka'));

# 300: add indir
$v = gd_add($D, array('field' => 'new300', 'in_fields' => array('in3', 'in0'),
  'fragment_index' => 0, 'field_type' => GD_INDIR_ENTRY));
check_ok2(300, 0, $D);
check_var2(300, 1, $v, TRUE);

$v = gd_entry($D, 'new300');
check_ok2(300, 2, $D);
check_var2(300, 3, $v, array('field' => 'new300',
  'field_type' => GD_INDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in3', 'in0')));

# 301: add indir
$v = gd_add($D, array('field' => 'new301', 'in_fields' => array('in3', 'in1'),
  'fragment_index' => 0, 'field_type' => GD_SINDIR_ENTRY));
check_ok2(301, 0, $D);
check_var2(301, 1, $v, TRUE);

$v = gd_entry($D, 'new301');
check_ok2(301, 2, $D);
check_var2(301, 3, $v, array('field' => 'new301',
  'field_type' => GD_SINDIR_ENTRY, 'fragment_index' => 0,
  'in_fields' => array('in3', 'in1')));

# 302: gd_include
$v = gd_include($D, 'format2', 0, 'ns', GD_CREAT | GD_EXCL);
check_ok2(302, 0, $D);
check_var2(302, 1, $v, 2);

# 303: gd_fragment_namespace (read)
$v = gd_fragment_namespace($D, 2);
check_ok2(303, 0, $D);
check_var2(303, 1, $v, 'ns');

# 304: gd_fragment_namespace (alter)
$v = gd_fragment_namespace($D, 2, 'ns2');
check_ok2(304, 0, $D);
check_var2(304, 1, $v, 'ns2');

# 305: gd_match_entries
$v = gd_match_entries($D, "^lin", 0);
check_ok2(305, 0, $D);
check_var2(305, 1, $v, array('lincom', 'linterp'));





# ===========================================

gd_discard($D);
unlink('dirfile/empty/format');
rmdir('dirfile/empty');
unlink('dirfile/data');
unlink('dirfile/new1');
unlink('dirfile/new135');
unlink('dirfile/format');
unlink('dirfile/format1');
unlink('dirfile/format2');
unlink('dirfile/form2');
rmdir('dirfile');

if ($ne > 0) {
  echo "ne = ", $ne, "\n";
  exit(1);
}

exit(0);
?>
