#!/usr/bin/perl -w
# Copyright (C) 2011-2016 D. V. Wiebe
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

use GetData;
use Math::Complex;
use strict;
use Test::More tests => 1754;

my $ne = 0;
my ($s, @a, %h);
select STDERR; $| = 1;
select STDOUT; $| = 1;

sub isn {
  cmp_ok (
    (defined $_[0]) ? $_[0] : "undef",
    (defined $_[0] and defined $_[1]) ? '==' : 'eq',
    (defined $_[1]) ? $_[1] : "undef", $_[2] . " = " .
    ((defined $_[0]) ? $_[0] : "undef") . ", expected " .
    ((defined $_[1]) ? $_[1] : "undef"));
}

sub CheckError {
  my $e = $_->error;
  print "\n";
  is ($e, $_[1], "e[$_[0]] = $e, expected $_[1]");
  print "#";
}

sub CheckError2 {
  my $e = $_->error;
  print "\n";
  is ($e, $_[2], "e[$_[0],$_[1]] = $e, expected $_[2]");
  print "#";
}

sub CheckArray {
  my $i;
  print "\n";
  is ($#{$_[1]}, $#_ - 2,
    "a[$_[0]]: " . (1 + $#{$_[1]}) . " elements, expected " . ($#_ - 1));
  for $i (0 .. $#_ - 2) {
    isn (${$_[1]}[$i], $_[$i + 2], "a($i)[$_[0]]");
  }
  print "#";
}

sub CheckArray2 {
  my $i;
  print "\n";
  is ($#{$_[2]}, $#_ - 3,
    "a[$_[0],$_[1]]: " . (1 + $#{$_[2]}) . " elements, expected " . ($#_ - 2));
  for $i (0 .. $#_ - 3) {
    isn (${$_[2]}[$i], $_[$i + 3], "a($i)[$_[0],$_[1]]" );
  }
  print "#";
}

sub CheckSArray {
  my $i;
  print "\n";
  is ($#{$_[1]}, $#_ - 2,
    "a[$_[0]]: " . (1 + $#{$_[1]}) . " elements, expected " . ($#_ - 1));
  for $i (0 .. $#_ - 2) {
    is (${$_[1]}[$i], $_[$i + 2],
      "s($i)[$_[0]] = \"${$_[1]}[$i]\", expected \"" . $_[$i + 2] . "\"");
  }
  print "#";
}

sub CheckSArray2 {
  my $i;
  print "\n";
  is ($#{$_[2]}, $#_ - 3,
    "a[$_[0],$_[1]]: " . (1 + $#{$_[2]}) . " elements, expected " . ($#_ - 2));
  for $i (0 .. $#_ - 3) {
    is (${$_[2]}[$i], $_[$i + 3], "s($i)[$_[0],$_[1]] = " .
      ((defined ${$_[2]}[$i]) ? "\"" . ${$_[2]}[$i] . "\"" : "undef") .
      ", expected " .
      ((defined $_[$i + 3]) ? "\"" . $_[$i + 3] . "\"" : "undef"));
  }
  print "#";
}

sub CheckNum {
  print "\n";
  isn ($_[1], $_[2], "n[$_[0]]");
  print "#";
}
sub CheckNum2 {
  print "\n";
  isn ($_[2], $_[3], "n[$_[0],$_[1]]");
  print "#";
}

sub CheckString {
  print "\n";
  is ($_[1], $_[2], "s[$_[0]] = \"$_[1]\", expected \"$_[2]\"");
  print "#";
}

sub CheckString2 {
  print "\n";
  is ($_[2], $_[3], "s[$_[0],$_[1]] = \"$_[2]\", expected \"$_[3]\"");
  print "#";
}

sub CheckEOString {
  print "\n";
  ok ($_[1] =~ m#$_[2]$#, "s[$_[0]] = \"$_[1]\", expected \"$_[2]\"");
  print "#";
}

sub CheckEOSArray {
  my $i;
  print "\n";
  is ($#{$_[1]}, $#_ - 2,
    "a[$_[0]]: " . (1 + $#{$_[1]}) . " elements, expected " . ($#_ - 1));
  for $i (0 .. $#_ - 2) {
    ok (${$_[1]}[$i] =~ m#$_[$i + 2]$#,
      "s($i)[$_[0]] = \"${$_[1]}[$i]\", expected \"$_[$i + 2]\"");
  }
  print "#";
}

sub CheckOK { &CheckError($_[0], 0) }
sub CheckOK2 { &CheckError2(@_, 0) }

my $nfields = 20;
my @fields = (qw(bit div data mult sbit INDEX alias const indir mplex phase
  recip carray lincom sarray sindir string window linterp polynom));

#create the dirfile
system "rm -rf dirfile" if (-e "dirfile");
(mkdir "dirfile" or die) unless -e "dirfile";

open GLOB, ">dirfile/data" or die;
print GLOB map chr, 1 .. 81;

open GLOB, ">dirfile/format" or die;
print GLOB <<EOF
/ENDIAN little
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
sarray SARRAY one two three four five six seven
data/msarray SARRAY eight nine ten eleven twelve
indir INDIR data carray
sindir SINDIR data sarray
string STRING \"Zaphod Beeblebrox\"
EOF
  or die;

open GLOB, ">dirfile/form2" or die;
print GLOB "const2 CONST INT8 -19\n" or die;
close GLOB;

# 1: error check
$_ = &GetData::open("x", $GetData::RDONLY);
CheckError(1, $GetData::E_IO);

# 2: open check
$_ = &GetData::open("dirfile", $GetData::RDWR);
CheckOK(2);

# 3: getdata (INT8) check
$s = $_->getdata("data", 5, 0, 1, 0, $GetData::INT8);
CheckOK(3);
CheckString(3, $s, join "", map chr, 41 .. 48);

# 4: getdata (unpacked) check
@a = $_->getdata("data", 5, 0, 1, 0, $GetData::INT16);
CheckOK(4);
CheckArray(4, \@a, 41 .. 48);

# 10: getdata (complex unpacked) check
@a = $_->getdata("data", 5, 0, 1, 0, $GetData::COMPLEX128);
CheckOK(10);
CheckArray(10, \@a, 41 .. 48);

# 11: getdata (GD_NULL) check - in both scalar and array context
$s = $_->getdata("data", 5, 0, 1, 0, $GetData::NULL);
CheckOK2(11,1);
CheckNum(11, $s, 8);
@a = $_->getdata("data", 5, 0, 1, 0, $GetData::NULL);
CheckOK2(11,2);
CheckArray(11, @a);

# 12: constant (INT8) check
$s = $_->get_constant("const", $GetData::INT8);
CheckOK(12);
CheckNum(12, $s, 5);

# 19: constant (COMPLEX128) check
$s = $_->get_constant("const", $GetData::COMPLEX128);
CheckOK(19);
CheckNum(19, $s, 5.5);

# 20: constant (GD_NULL) check
$s = $_->get_constant("const", $GetData::NULL);
CheckOK(20);
CheckNum(20, $s, undef);

# 23: nfields check
$s = $_->field_list;
CheckOK(23);
CheckNum(23, $s, $nfields);

# 25: field_list check
@a = $_->field_list;
CheckOK(25);
CheckSArray(25, \@a, @fields);

# 26: nmfields check
$s = $_->mfield_list("data");
CheckOK(26);
CheckNum(26, $s, 5);

# 27: mfield_list check
@a = $_->mfield_list("data");
CheckOK(27);
CheckSArray(27, \@a, qw(mstr mconst mcarray mlut msarray));

# 28: nframes check
$s = $_->nframes;
CheckOK(28);
CheckNum(28, $s, 10);

# 29: spf check
$s = $_->spf("data");
CheckOK(29);
CheckNum(29, $s, 8);

# 30: putdata (packed) check
$s = $_->putdata("data", 5, 1, $GetData::UINT8, "\15\16\17\20");
CheckOK2(30,1);
CheckNum(30,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(30,2);
CheckArray(30, \@a, 41, 015, 016, 017, 020, 46, 47, 48);

# 31: putdata (typed ref) check
$s = $_->putdata("data", 5, 1, $GetData::UINT16, [ 23, 24, 25, 26 ]);
CheckOK2(31,1);
CheckNum(31,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(31,2);
CheckArray(31, \@a, 41, 23, 24, 25, 26, 46, 47, 48);

# 33: putdata (untyped ref) check
$s = $_->putdata("data", 5, 1, [ 33, 34, 35, 36 ]);
CheckOK2(33,1);
CheckNum(33,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(33,2);
CheckArray(33, \@a, 41, 33, 34, 35, 36, 46, 47, 48);

# 35: putdata (simple list) check
$s = $_->putdata("data", 5, 1, 23., 24., 25., 26.);
CheckOK2(35,1);
CheckNum(35,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(35,2);
CheckArray(35, \@a, 41, 23, 24, 25, 26, 46, 47, 48);

# 37: putdata (undef list) check
$s = $_->putdata("data", 5, 1, undef, 13.+0*i, 14.+0*i, 15.+0*i, 16.+0*i);
CheckOK2(37,1);
CheckNum(37,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(37,2);
CheckArray(37, \@a, 41, 13, 14, 15, 16, 46, 47, 48);

# 38: error_string check
$s = $_->getdata("x", 5, 0, 1, 0, $GetData::UINT8);
CheckError(38,$GetData::E_BAD_CODE);
CheckString(38, $_->error_string, "Field not found: x");

# 39: get_entry check
$s = $_->entry("data");
CheckOK(39);
CheckNum(39,$s,$GetData::RAW_ENTRY);

# 40: get_entry check
%h = $_->entry("data");
CheckOK(40);
CheckSArray2(40, 0, [ sort keys %h ],
  qw(data_type field field_type fragment_index scalar scalar_ind spf));
CheckNum2(40, 1, $h{'data_type'}, $GetData::INT8);
CheckString2(40, 2, $h{'field'}, "data");
CheckNum2(40, 3, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(40, 4, $h{'fragment_index'}, 0);
CheckSArray2(40, 6, $h{'scalar'}, undef);
CheckArray2(40, 7, $h{'scalar_ind'}, undef);
CheckNum2(40, 8, $h{'spf'}, 8);

# 42: get_entry check
%h = $_->entry("lincom");
CheckOK(42);
CheckSArray2(42, 1, [ sort keys %h ], qw(b field field_type),
  qw(fragment_index in_fields m n_fields scalar scalar_ind));
CheckArray2(42, 2, $h{'b'}, 2.2, 3.3+4.4*i, 5.5);
CheckString2(42, 5, $h{'field'}, "lincom");
CheckNum2(42, 6, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(42, 7, $h{'fragment_index'}, 0);
CheckSArray2(42, 8, $h{'in_fields'}, qw(data INDEX linterp));
CheckArray2(42, 3, $h{'m'}, 1.1, 2.2, 5.5);
CheckNum2(42, 10, $h{'n_fields'}, 3);
CheckSArray2(42, 11, $h{'scalar'}, undef, undef, "const", undef, undef,
  "const");
CheckArray2(42, 12, $h{'scalar_ind'}, undef, undef, -1, undef, undef, -1);

# 44: get_entry check
%h = $_->entry("polynom");
CheckOK(44);
CheckSArray2(44, 1, [ sort keys %h ], qw(a field field_type),
  qw(fragment_index in_fields poly_ord scalar scalar_ind));
CheckArray2(44, 2, $h{'a'}, 1.1, 2.2, 2.2, 3.3+4.4*i, 5.5, 5.5);
CheckString2(44, 4, $h{'field'}, "polynom");
CheckNum2(44, 5, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(44, 6, $h{'fragment_index'}, 0);
CheckString2(44, 7, $h{'in_fields'}, "data");
CheckNum2(44, 8, $h{'poly_ord'}, 5);
CheckSArray2(44, 9, $h{'scalar'}, undef, undef, undef, undef, "const", "const");
CheckArray2(44, 10, $h{'scalar_ind'}, undef, undef, undef, undef, -1, -1);

# 45: get_entry check
%h = $_->entry("linterp");
CheckOK(45);
CheckSArray2(45, 0, [ sort keys %h ], qw(field field_type fragment_index),
  qw(in_fields table));
CheckString2(45, 1, $h{'field'}, "linterp");
CheckNum2(45, 2, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(45, 3, $h{'fragment_index'}, 0);
CheckString2(45, 4, $h{'in_fields'}, "data");
CheckString2(45, 5, $h{'table'}, "./lut");

# 46: get_entry check
%h = $_->entry("bit");
CheckOK(46);
CheckSArray2(46, 0, [ sort keys %h ], qw(bitnum field field_type),
  qw(fragment_index in_fields numbits scalar scalar_ind));
CheckNum2(46, 1, $h{'bitnum'}, 3);
CheckString2(46, 2, $h{'field'}, "bit");
CheckNum2(46, 3, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(46, 4, $h{'fragment_index'}, 0);
CheckString2(46, 5, $h{'in_fields'}, "data");
CheckNum2(46, 6, $h{'numbits'}, 4);
CheckSArray2(46, 7, $h{'scalar'}, undef, undef);
CheckArray2(46, 8, $h{'scalar_ind'}, undef, undef);

# 47: get_entry check
%h = $_->entry("sbit");
CheckOK(47);
CheckSArray2(47, 0, [ sort keys %h ], qw(bitnum field field_type),
  qw(fragment_index in_fields numbits scalar scalar_ind));
CheckNum2(47, 1, $h{'bitnum'}, 5);
CheckString2(47, 2, $h{'field'}, "sbit");
CheckNum2(47, 3, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(47, 4, $h{'fragment_index'}, 0);
CheckString2(47, 5, $h{'in_fields'}, "data");
CheckNum2(47, 6, $h{'numbits'}, 6);
CheckSArray2(47, 7, $h{'scalar'}, undef, undef);
CheckArray2(47, 8, $h{'scalar_ind'}, undef, undef);

# 48: get_entry check
%h = $_->entry("mult");
CheckOK(48);
CheckSArray2(48, 0, [ sort keys %h ], qw(field field_type fragment_index),
  "in_fields");
CheckString2(48, 1, $h{'field'}, "mult");
CheckNum2(48, 2, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(48, 3, $h{'fragment_index'}, 0);
CheckSArray2(48, 4, $h{'in_fields'}, qw(data sbit));

# 49: get_entry check
%h = $_->entry("phase");
CheckOK(49);
CheckSArray2(49, 0, [ sort keys %h ], qw(field field_type fragment_index),
  qw(in_fields scalar scalar_ind shift));
CheckString2(49, 1, $h{'field'}, "phase");
CheckNum2(49, 2, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(49, 3, $h{'fragment_index'}, 0);
CheckString2(49, 4, $h{'in_fields'}, "data");
CheckSArray2(49, 5, $h{'scalar'}, undef);
CheckArray2(49, 6, $h{'scalar_ind'}, undef);
CheckNum2(49, 7, $h{'shift'}, 11);

# 50: get_entry check
%h = $_->entry("const");
CheckOK(50);
CheckSArray2(50, 0, [ sort keys %h ], qw(const_type field field_type),
  "fragment_index");
CheckNum2(50, 1, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(50, 2, $h{'field'}, "const");
CheckNum2(50, 3, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(50, 4, $h{'fragment_index'}, 0);

# 51: get_entry check
%h = $_->entry("string");
CheckOK(51);
CheckSArray2(51, 0, [ sort keys %h ], qw(field field_type fragment_index));
CheckString2(51, 1, $h{'field'}, "string");
CheckNum2(51, 2, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(51, 3, $h{'fragment_index'}, 0);

# 52: fragment_index check
$s = $_->fragment_index("data");
CheckOK(52);
CheckNum(52, $s, 0);

# 53: add_raw check
$s = $_->add_raw("new1", $GetData::FLOAT64, 3);
CheckOK2(53, 1);
CheckNum2(53, 2, $s, 0);

%h = $_->entry("new1");
CheckOK2(53, 3);
CheckNum2(53, 4, $h{'data_type'}, $GetData::FLOAT64);
CheckString2(53, 5, $h{'field'}, "new1");
CheckNum2(53, 6, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(53, 7, $h{'fragment_index'}, 0);
CheckSArray2(53, 8, $h{'scalar'}, undef);
CheckArray2(53, 9, $h{'scalar_ind'}, undef);
CheckNum2(53, 10, $h{'spf'}, 3);

#55: add_lincom check
$s = $_->add_lincom("new3", 2, [ qw(in1 in2) ], [ 1.1+1.2*i, 1.4+1.5*i ],
  [ 1.3+1.4*i, 1.6+1.7*i ], 0);
CheckOK2(55, 1);
CheckNum2(55, 2, $s, 0);

%h = $_->entry("new3");
CheckOK2(55, 3);
CheckArray2(55, 4, $h{'b'}, 1.3+1.4*i, 1.6+1.7*i);
CheckString2(55, 6, $h{'field'}, "new3");
CheckNum2(55, 7, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(55, 8, $h{'fragment_index'}, 0);
CheckSArray2(55, 9, $h{'in_fields'}, qw(in1 in2));
CheckArray2(55, 10, $h{'m'}, 1.1+1.2*i, 1.4+1.5*i);
CheckNum2(55, 11, $h{'n_fields'}, 2);
CheckSArray2(55, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(55, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 56: add_polynom
$s = $_->add_polynom("new4", 3, "in1", [ 3.9, 4.8, 5.7, 6.6 ], 0);
CheckOK2(56, 1);
CheckNum2(56, 2, $s, 0);

%h = $_->entry("new4");
CheckOK2(56, 3);
CheckArray2(56, 4, $h{'a'}, 3.9, 4.8, 5.7, 6.6);
CheckString2(56, 7, $h{'field'}, "new4");
CheckNum2(56, 8, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(56, 9, $h{'fragment_index'}, 0);
CheckString2(56, 10, $h{'in_fields'}, "in1");
CheckNum2(56, 11, $h{'poly_ord'}, 3);
CheckSArray2(56, 12, $h{'scalar'}, undef, undef, undef, undef);
CheckArray2(56, 13, $h{'scalar_ind'}, undef, undef, undef, undef);

# 58: add_linterp
$s = $_->add_linterp("new6", "in", "./some/table");
CheckOK2(58, 1);
CheckNum2(58, 2, $s, 0);

%h = $_->entry("new6");
CheckOK2(58, 2);
CheckString2(58, 3, $h{'field'}, "new6");
CheckNum2(58, 4, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(58, 5, $h{'fragment_index'}, 0);
CheckString2(58, 6, $h{'in_fields'}, "in");
CheckString2(58, 7, $h{'table'}, "./some/table");

# 59: add_bit
$s = $_->add_bit("new7", "in1", 11, 22);
CheckOK2(59, 1);
CheckNum2(59, 2, $s, 0);

%h = $_->entry("new7");
CheckOK(59, 2);
CheckNum2(59, 3, $h{'bitnum'}, 11);
CheckString2(59, 4, $h{'field'}, "new7");
CheckNum2(59, 5, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(59, 6, $h{'fragment_index'}, 0);
CheckString2(59, 7, $h{'in_fields'}, "in1");
CheckNum2(59, 8, $h{'numbits'}, 22);
CheckSArray2(59, 9, $h{'scalar'}, undef, undef);
CheckArray2(59, 10, $h{'scalar_ind'}, undef, undef);

# 60: add_sbit
$s = $_->add_sbit("new8", "in2", 5, 10);
CheckOK2(60, 1);
CheckNum2(60, 2, $s, 0);

%h = $_->entry("new8");
CheckOK(60, 2);
CheckNum2(60, 3, $h{'bitnum'}, 5);
CheckString2(60, 4, $h{'field'}, "new8");
CheckNum2(60, 5, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(60, 6, $h{'fragment_index'}, 0);
CheckString2(60, 7, $h{'in_fields'}, "in2");
CheckNum2(60, 8, $h{'numbits'}, 10);
CheckSArray2(60, 9, $h{'scalar'}, undef, undef);
CheckArray2(60, 10, $h{'scalar_ind'}, undef, undef);

# 61: add_mutiply
$s = $_->add_multiply("new9", "in2", "in3");
CheckOK2(61, 1);
CheckNum2(61, 2, $s, 0);

%h = $_->entry("new9");
CheckOK2(61, 3);
CheckString2(61, 4, $h{'field'}, "new9");
CheckNum2(61, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(61, 6, $h{'fragment_index'}, 0);
CheckSArray2(61, 7, $h{'in_fields'}, qw(in2 in3));

# 62: add_phase
$s = $_->add_phase("new10", "in6", 42);
CheckOK2(62, 1);
CheckNum2(62, 2, $s, 0);

%h = $_->entry("new10");
CheckOK2(62, 2);
CheckString2(62, 3, $h{'field'}, "new10");
CheckNum2(62, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(62, 5, $h{'fragment_index'}, 0);
CheckString2(62, 6, $h{'in_fields'}, "in6");
CheckSArray2(62, 7, $h{'scalar'}, undef);
CheckArray2(62, 8, $h{'scalar_ind'}, undef);
CheckNum2(62, 9, $h{'shift'}, 42);

# 63: add_const
$s = $_->add_const("new11", $GetData::FLOAT64, "33.3");
CheckOK2(63, 1);
CheckNum2(63, 2, $s, 0);

%h = $_->entry("new11");
CheckOK2(63, 3);
CheckNum2(63, 4, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(63, 5, $h{'field'}, "new11");
CheckNum2(63, 6, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(63, 7, $h{'fragment_index'}, 0);

$s = $_->get_constant("new11", $GetData::FLOAT64);
CheckOK2(63, 8);
CheckNum2(63, 9, $s, 33.3);

# 64: fragment_name
$s = $_->fragmentname(0);
CheckOK(64);
CheckEOString(64, $s, "dirfile/format");

# 65: nfragments
$s = $_->fragments;
CheckOK(65);
CheckNum(65, $s, 1);

# 66: include
$s = $_->include("form2", 0);
CheckOK2(66, 1);
CheckNum2(66, 2, $s, 1);

$s = $_->get_constant("const2", $GetData::INT32);
CheckOK2(66, 3);
CheckNum2(66, 4, $s, -19);

# 67: nfields_by_type
$s = $_->field_list_by_type($GetData::LINCOM_ENTRY);
CheckOK(67);
CheckNum(67, $s, 2);

# 68: field_list_by_type
@a = $_->field_list_by_type($GetData::LINCOM_ENTRY);
CheckOK(68);
CheckSArray(68, \@a, qw(new3 lincom));

# 69: nvectors
$s = $_->vector_list;
CheckOK(69);
CheckNum(69, $s, 23);

# 70: vector_list
@a = $_->vector_list;
CheckOK(70);
CheckSArray(70, \@a, qw(bit div data mult new1 new3 new4 new6 new7 new8 new9
  sbit INDEX alias indir mplex new10 phase recip lincom window linterp
  polynom));

#72: madd_lincom check
$s = $_->madd_lincom("data", "mnew2", 2, [ qw(in1 in2) ], [ 9.9+8.8*i, 7.7 ],
  [ 6.6, 5.5 ]);
CheckOK2(72, 1);
CheckNum2(72, 2, $s, 0);

%h = $_->entry("data/mnew2");
CheckOK2(72, 3);
CheckArray2(72, 4, $h{'b'}, 6.6, 5.5);
CheckArray2(72, 5, $h{'m'}, 9.9+8.8*i, 7.7);
CheckString2(72, 7, $h{'field'}, "data/mnew2");
CheckNum2(72, 8, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(72, 9, $h{'fragment_index'}, 0);
CheckSArray2(72, 10, $h{'in_fields'}, qw(in1 in2));
CheckNum2(72, 11, $h{'n_fields'}, 2);
CheckSArray2(72, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(72, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 73: madd_polynom
$s = $_->madd_polynom("data", "mnew3", 3, "in1", [ 3.9, 4.8, 5.7, 6.6 ]);
CheckOK2(73, 1);
CheckNum2(73, 2, $s, 0);

%h = $_->entry("data/mnew3");
CheckOK2(73, 3);
CheckArray2(73, 4, $h{'a'}, 3.9, 4.8, 5.7, 6.6);
CheckString2(73, 7, $h{'field'}, "data/mnew3");
CheckNum2(73, 8, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(73, 9, $h{'fragment_index'}, 0);
CheckString2(73, 10, $h{'in_fields'}, "in1");
CheckNum2(73, 11, $h{'poly_ord'}, 3);
CheckSArray2(73, 12, $h{'scalar'}, undef, undef, undef, undef);
CheckArray2(73, 13, $h{'scalar_ind'}, undef, undef, undef, undef);

# 75: madd_linterp
$s = $_->madd_linterp("data", "mnew6", "in", "./more/table");
CheckOK2(75, 1);
CheckNum2(75, 2, $s, 0);

%h = $_->entry("data/mnew6");
CheckOK2(75, 2);
CheckString2(75, 3, $h{'field'}, "data/mnew6");
CheckNum2(75, 4, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(75, 5, $h{'fragment_index'}, 0);
CheckString2(75, 6, $h{'in_fields'}, "in");
CheckString2(75, 7, $h{'table'}, "./more/table");

# 76: madd_bit
$s = $_->madd_bit("data", "mnew7", "in1", 3, 2);
CheckOK2(76, 1);
CheckNum2(76, 2, $s, 0);

%h = $_->entry("data/mnew7");
CheckOK(76, 2);
CheckNum2(76, 3, $h{'bitnum'}, 3);
CheckString2(76, 4, $h{'field'}, "data/mnew7");
CheckNum2(76, 5, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(76, 6, $h{'fragment_index'}, 0);
CheckString2(76, 7, $h{'in_fields'}, "in1");
CheckNum2(76, 8, $h{'numbits'}, 2);
CheckSArray2(76, 9, $h{'scalar'}, undef, undef);
CheckArray2(76, 10, $h{'scalar_ind'}, undef, undef);

# 77: madd_sbit
$s = $_->madd_sbit("data", "mnew8", "in2", 4, 5);
CheckOK2(77, 1);
CheckNum2(77, 2, $s, 0);

%h = $_->entry("data/mnew8");
CheckOK(77, 2);
CheckNum2(77, 3, $h{'bitnum'}, 4);
CheckString2(77, 4, $h{'field'}, "data/mnew8");
CheckNum2(77, 5, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(77, 6, $h{'fragment_index'}, 0);
CheckString2(77, 7, $h{'in_fields'}, "in2");
CheckNum2(77, 8, $h{'numbits'}, 5);
CheckSArray2(77, 9, $h{'scalar'}, undef, undef);
CheckArray2(77, 10, $h{'scalar_ind'}, undef, undef);

# 78: madd_mutiply
$s = $_->madd_multiply("data", "mnew9", "in3", "in2");
CheckOK2(78, 1);
CheckNum2(78, 2, $s, 0);

%h = $_->entry("data/mnew9");
CheckOK2(78, 3);
CheckString2(78, 4, $h{'field'}, "data/mnew9");
CheckNum2(78, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(78, 6, $h{'fragment_index'}, 0);
CheckSArray2(78, 7, $h{'in_fields'}, qw(in3 in2));

# 79: madd_phase
$s = $_->madd_phase("data", "mnew10", "in6", 44);
CheckOK2(79, 1);
CheckNum2(79, 2, $s, 0);

%h = $_->entry("data/mnew10");
CheckOK2(79, 2);
CheckString2(79, 3, $h{'field'}, "data/mnew10");
CheckNum2(79, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(79, 5, $h{'fragment_index'}, 0);
CheckString2(79, 6, $h{'in_fields'}, "in6");
CheckSArray2(79, 7, $h{'scalar'}, undef);
CheckArray2(79, 8, $h{'scalar_ind'}, undef);
CheckNum2(79, 9, $h{'shift'}, 44);

# 80: madd_const
$s = $_->madd_const("data", "mnew11", $GetData::FLOAT64, 9.2);
CheckOK2(80, 1);
CheckNum2(80, 2, $s, 0);

%h = $_->entry("data/mnew11");
CheckOK2(80, 3);
CheckNum2(80, 4, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(80, 5, $h{'field'}, "data/mnew11");
CheckNum2(80, 6, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(80, 7, $h{'fragment_index'}, 0);

$s = $_->get_constant("data/mnew11", $GetData::FLOAT64);
CheckOK2(80, 8);
CheckNum2(80, 9, $s, 9.2);

# 81: get_string
$s = $_->get_string("string");
CheckOK(81);
CheckString(81, $s, "Zaphod Beeblebrox");

# 82: add_string
$s = $_->add_string("new12", "a string");
CheckOK2(82, 1);
CheckNum2(82, 2, $s, 0);

%h = $_->entry("new12");
CheckOK2(82, 3);
CheckString2(82, 4, $h{'field'}, "new12");
CheckNum2(82, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(82, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("new12");
CheckOK2(82, 7);
CheckString2(82, 8, $s, "a string");

# 83: madd_string
$s = $_->madd_string("data", "mnew12", "another string");
CheckOK2(83, 1);
CheckNum2(83, 2, $s, 0);

%h = $_->entry("data/mnew12");
CheckOK2(83, 3);
CheckString2(83, 4, $h{'field'}, "data/mnew12");
CheckNum2(83, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(83, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("data/mnew12");
CheckOK2(83, 7);
CheckString2(83, 8, $s, "another string");

# 84: add_spec
$s = $_->add_spec("lorem STRING \"Lorem ipsum\"", 0);
CheckOK2(84, 1);
CheckNum2(84, 2, $s, 0);

%h = $_->entry("lorem");
CheckOK2(84, 3);
CheckString2(84, 4, $h{'field'}, "lorem");
CheckNum2(84, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(84, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("lorem");
CheckOK2(84, 7);
CheckString2(84, 8, $s, "Lorem ipsum");

# 85: add_spec
$s = $_->madd_spec("ipsum STRING \"dolor sit amet.\"", "lorem");
CheckOK2(85, 1);
CheckNum2(85, 2, $s, 0);

%h = $_->entry("lorem/ipsum");
CheckOK2(85, 3);
CheckString2(85, 4, $h{'field'}, "lorem/ipsum");
CheckNum2(85, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(85, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("lorem/ipsum");
CheckOK2(85, 7);
CheckString2(85, 8, $s, "dolor sit amet.");

# 86: put_constant
$s = $_->put_constant("const", 86);
CheckOK2(86, 1);
CheckNum2(86, 2, $s, 0);

$s = $_->get_constant("const", $GetData::FLOAT64);
CheckOK2(86, 3);
CheckNum2(86, 4, $s, 86.);

# 94: put_string
$s = $_->put_string("string", "Arthur Dent");
CheckOK2(94, 1);
CheckNum2(94, 2, $s, 0);

$s = $_->get_string("string");
CheckOK2(94, 3);
CheckString2(94, 4, $s, "Arthur Dent");

# 95: nmfields_by_type
$s = $_->mfield_list_by_type("data", $GetData::LINCOM_ENTRY);
CheckOK(95);
CheckNum(95, $s, 1);

# 96: mfield_list_by_type
@a = $_->mfield_list_by_type("data", $GetData::LINCOM_ENTRY);
CheckOK(96);
CheckSArray(96, \@a, qw(mnew2));

# 97: nmvectors
$s = $_->mvector_list("data");
CheckOK(97);
CheckNum(97, $s, 8);

# 98: mvector_list
@a = $_->mvector_list("data");
CheckOK(98);
CheckSArray(98, \@a,
  qw(mlut mnew2 mnew3 mnew6 mnew7 mnew8 mnew9 mnew10));

# 99: alter raw
$s = $_->alter_raw("new1", $GetData::INT32, undef);
CheckOK2(99, 1);
CheckNum2(99, 2, $s, 0);

%h = $_->entry("new1");
CheckOK2(99, 3);
CheckNum2(99, 4, $h{'data_type'}, $GetData::INT32);
CheckString2(99, 5, $h{'field'}, "new1");
CheckNum2(99, 6, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(99, 7, $h{'fragment_index'}, 0);
CheckSArray2(99, 8, $h{'scalar'}, undef);
CheckArray2(99, 9, $h{'scalar_ind'}, undef);
CheckNum2(99, 10, $h{'spf'}, 3);

# 100: alter_lincom check
$s = $_->alter_lincom("new3", undef, [ qw(in3 in4) ], [ 3., 4. ]);
CheckOK2(100, 1);
CheckNum2(100, 2, $s, 0);

%h = $_->entry("new3");
CheckOK2(100, 3);
CheckArray2(100, 4, $h{'b'}, 1.3+1.4*i, 1.6+1.7*i);
CheckArray2(100, 5, $h{'m'}, 3, 4);
CheckString2(100, 7, $h{'field'}, "new3");
CheckNum2(100, 8, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(100, 9, $h{'fragment_index'}, 0);
CheckSArray2(100, 10, $h{'in_fields'}, qw(in3 in4));
CheckNum2(100, 11, $h{'n_fields'}, 2);
CheckSArray2(100, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(100, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 102: alter_polynom
$s = $_->alter_polynom("new4", 4, undef, [ 1*i, 2*i, 3*i, 4*i, 5*i ]);
CheckOK2(102, 1);
CheckNum2(102, 2, $s, 0);

%h = $_->entry("new4");
CheckOK2(102, 3);
CheckArray2(102, 4, $h{'a'}, 1*i, 2*i, 3*i, 4*i, 5*i);
CheckString2(102, 7, $h{'field'}, "new4");
CheckNum2(102, 8, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(102, 9, $h{'fragment_index'}, 0);
CheckString2(102, 10, $h{'in_fields'}, "in1");
CheckNum2(102, 11, $h{'poly_ord'}, 4);
CheckSArray2(102, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(102, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 104: alter_linterp
$s = $_->alter_linterp("new6", undef, "./other/table");
CheckOK2(104, 1);
CheckNum2(104, 2, $s, 0);

%h = $_->entry("new6");
CheckOK2(104, 2);
CheckString2(104, 3, $h{'field'}, "new6");
CheckNum2(104, 4, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(104, 5, $h{'fragment_index'}, 0);
CheckString2(104, 6, $h{'in_fields'}, "in");
CheckString2(104, 7, $h{'table'}, "./other/table");

# 105: alter_bit
$s = $_->alter_bit("new7", "in3", undef, 8);
CheckOK2(105, 1);
CheckNum2(105, 2, $s, 0);

%h = $_->entry("new7");
CheckOK(105, 2);
CheckNum2(105, 3, $h{'bitnum'}, 11);
CheckString2(105, 4, $h{'field'}, "new7");
CheckNum2(105, 5, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(105, 6, $h{'fragment_index'}, 0);
CheckString2(105, 7, $h{'in_fields'}, "in3");
CheckNum2(105, 8, $h{'numbits'}, 8);
CheckSArray2(105, 9, $h{'scalar'}, undef, undef);
CheckArray2(105, 10, $h{'scalar_ind'}, undef, undef);

# 106: alter_sbit
$s = $_->alter_sbit("new8", "in1");
CheckOK2(106, 1);
CheckNum2(106, 2, $s, 0);

%h = $_->entry("new8");
CheckOK(106, 2);
CheckNum2(106, 3, $h{'bitnum'}, 5);
CheckString2(106, 4, $h{'field'}, "new8");
CheckNum2(106, 5, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(106, 6, $h{'fragment_index'}, 0);
CheckString2(106, 7, $h{'in_fields'}, "in1");
CheckNum2(106, 8, $h{'numbits'}, 10);
CheckSArray2(106, 9, $h{'scalar'}, undef, undef);
CheckArray2(106, 10, $h{'scalar_ind'}, undef, undef);

# 107: alter_mutiply
$s = $_->alter_multiply("new9", "in1");
CheckOK2(107, 1);
CheckNum2(107, 2, $s, 0);

%h = $_->entry("new9");
CheckOK2(107, 3);
CheckString2(107, 4, $h{'field'}, "new9");
CheckNum2(107, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(107, 6, $h{'fragment_index'}, 0);
CheckSArray2(107, 7, $h{'in_fields'}, qw(in1 in3));

# 108: alter_phase
$s = $_->alter_phase("new10", undef, -3);
CheckOK2(108, 1);
CheckNum2(108, 2, $s, 0);

%h = $_->entry("new10");
CheckOK2(108, 2);
CheckString2(108, 3, $h{'field'}, "new10");
CheckNum2(108, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(108, 5, $h{'fragment_index'}, 0);
CheckString2(108, 6, $h{'in_fields'}, "in6");
CheckSArray2(108, 7, $h{'scalar'}, undef);
CheckArray2(108, 8, $h{'scalar_ind'}, undef);
CheckNum2(108, 9, $h{'shift'}, -3);

# 109: alter_const
$s = $_->alter_const("new11", $GetData::FLOAT32);
CheckOK2(109, 1);
CheckNum2(109, 2, $s, 0);

%h = $_->entry("new11");
CheckOK2(109, 3);
CheckNum2(109, 4, $h{'const_type'}, $GetData::FLOAT32);
CheckString2(109, 5, $h{'field'}, "new11");
CheckNum2(109, 6, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(109, 7, $h{'fragment_index'}, 0);

# 110: encoding check
$s = $_->encoding(0);
CheckOK(110);
CheckNum(110, $s, $GetData::UNENCODED);

# 111: endianness check
$s = $_->endianness(0);
CheckOK(111);
CheckNum(111, $s, $GetData::LITTLE_ENDIAN | $GetData::NOT_ARM_ENDIAN);

# 112: dirfilename
$s = $_->dirfilename;
CheckOK(112);
CheckEOString(112, $s, "dirfile");

# 113: parent_fragment
$s = $_->parent_fragment(1);
CheckOK(113);
CheckNum(113, $s, 0);

# 114: alter_protection
$s = $_->alter_protection($GetData::PROTECT_DATA, 1);
CheckOK(114);
CheckNum(114, $s, 0);

# 115: protection
$s = $_->protection(1);
CheckOK(115);
CheckNum(115, $s, $GetData::PROTECT_DATA);

# 116: raw_filename
$s = $_->raw_filename("data");
CheckOK(116);
CheckEOString(116, $s, "dirfile/data");

# 117: reference
$s = $_->reference("new1");
CheckOK(117);
CheckString(117, $s, "new1");

# 118: gd_eof
$s = $_->eof("lincom");
CheckOK(118);
CheckNum(118, $s, 81);

# 119: alter_encoding
$s = $_->alter_encoding($GetData::SLIM_ENCODED, 1);
CheckOK2(119, 1);
CheckNum2(119, 2, $s, 0);

$s = $_->encoding(1);
CheckOK2(119, 3);
CheckNum2(119, 4, $s, $GetData::SLIM_ENCODED);

# 120: alter_endianness
$s = $_->alter_endianness($GetData::BIG_ENDIAN, 1);
CheckOK2(120, 1);
CheckNum2(120, 2, $s, 0);

$s = $_->endianness(1);
CheckOK2(120, 3);
CheckNum2(120, 4, $s, $GetData::BIG_ENDIAN);

# 121: alter_spec
$s = $_->alter_spec("new10 PHASE in const");
CheckOK2(121, 1);
CheckNum2(121, 2, $s, 0);

%h = $_->entry("new10");
CheckOK2(121, 2);
CheckString2(121, 3, $h{'field'}, "new10");
CheckNum2(121, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(121, 5, $h{'fragment_index'}, 0);
CheckString2(121, 6, $h{'in_fields'}, "in");
CheckSArray2(121, 7, $h{'scalar'}, "const");
CheckArray2(121, 8, $h{'scalar_ind'}, -1);
CheckNum2(121, 9, $h{'shift'}, 86);

# 122: delete
$s = $_->delete("new10");
CheckOK2(122, 0);
CheckNum2(122, 1, $s, 0);

$s = $_->entry("new10");
CheckError2(122, 2, $GetData::E_BAD_CODE);
CheckNum2(122, 3, $s, undef);

# 123: malter_spec
$s = $_->malter_spec("mnew10 PHASE in4 11", "data");
CheckOK2(123, 0);
CheckNum2(123, 1, $s, 0);

%h = $_->entry("data/mnew10");
CheckOK2(123, 2);
CheckString2(123, 3, $h{'field'}, "data/mnew10");
CheckNum2(123, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(123, 5, $h{'fragment_index'}, 0);
CheckString2(123, 6, $h{'in_fields'}, "in4");
CheckSArray2(123, 7, $h{'scalar'}, undef);
CheckArray2(123, 8, $h{'scalar_ind'}, undef);
CheckNum2(123, 9, $h{'shift'}, 11);

# 124: move
$s = $_->move("new9", 1);
CheckOK2(124, 0);
CheckNum2(124, 1, $s, 0);

%h = $_->entry("new9");
CheckOK2(124, 3);
CheckString2(124, 4, $h{'field'}, "new9");
CheckNum2(124, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(124, 6, $h{'fragment_index'}, 1);
CheckSArray2(124, 7, $h{'in_fields'}, qw(in1 in3));

# 125: rename
$s = $_->rename(qw(new9 newer));
CheckOK2(125, 0);
CheckNum2(125, 1, $s, 0);

$s = $_->entry("new9");
CheckError2(125, 2, $GetData::E_BAD_CODE);

%h = $_->entry("newer");
CheckOK2(125, 3);
CheckString2(125, 4, $h{'field'}, "newer");
CheckNum2(125, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(125, 6, $h{'fragment_index'}, 1);
CheckSArray2(125, 7, $h{'in_fields'}, qw(in1 in3));

# 126: uninclude
$s = $_->uninclude(1);
CheckOK2(126, 0);
CheckNum2(126, 1, $s, 0);

$s = $_->entry("newer");
CheckError2(126, 2, $GetData::E_BAD_CODE);

# 127: frameoffset
$s = $_->frameoffset(0);
CheckOK(127);
CheckNum(127, $s, 0);

# 128: alter_frameoffset
$s = $_->alter_frameoffset(33, 0);
CheckOK2(128, 0);
CheckNum2(128, 1, $s, 0);

$s = $_->frameoffset(0);
CheckOK2(128, 2);
CheckNum2(128, 3, $s, 33);

# 129: native_type
$s = $_->native_type("data");
CheckOK(129);
CheckNum(129, $s, $GetData::INT8);

# 131: validate
$s = $_->validate("new7");
CheckError(131, $GetData::E_BAD_CODE);
CheckNum(131, $s, undef);

# 133: framenum
$s = $_->framenum("data", 33.3, 6);
CheckOK(133);
CheckNum(133, $s, 37.0375);

# 135: add check
$s = $_->add({
    field          => "new135",
    field_type     => $GetData::RAW_ENTRY,
    fragment_index => 0,
    data_type      => $GetData::FLOAT32,
    spf            => 5
  });
CheckOK2(135,1);
CheckNum2(135, 2, $s, 0);

%h = $_->entry("new135");
CheckOK2(135, 2);
CheckString2(135, 3, $h{'field'}, "new135");
CheckNum2(135, 4, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(135, 5, $h{'fragment_index'}, 0);
CheckNum2(135, 6, $h{'data_type'}, $GetData::FLOAT32);
CheckNum2(135, 7, $h{'spf'}, 5);
CheckSArray2(135, 8, $h{'scalar'}, undef);
CheckArray2(135, 9, $h{'scalar_ind'}, undef);

# 136: madd check
$s = $_->madd({
    field          => "mnew136",
    field_type     => $GetData::PHASE_ENTRY,
    fragment_index => 0,
    in_fields      => "data",
    shift          => 2
  }, "data");
CheckOK2(136,1);
CheckNum2(136, 2, $s, 0);

%h = $_->entry("data/mnew136");
CheckOK2(136, 2);
CheckString2(136, 3, $h{'field'}, "data/mnew136");
CheckNum2(136, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(136, 5, $h{'fragment_index'}, 0);
CheckString2(136, 6, $h{'in_fields'}, "data");
CheckSArray2(136, 7, $h{'scalar'}, undef);
CheckArray2(136, 8, $h{'scalar_ind'}, undef);
CheckNum2(136, 9, $h{'shift'}, 2);

# 141: alter check
$s = $_->alter_entry("new135", {
    field_type     => $GetData::RAW_ENTRY,
    data_type      => $GetData::FLOAT64,
    spf            => 141
  });
CheckOK2(141,1);
CheckNum2(141, 2, $s, 0);

%h = $_->entry("new135");
CheckOK2(141, 2);
CheckString2(141, 3, $h{'field'}, "new135");
CheckNum2(141, 4, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(141, 5, $h{'fragment_index'}, 0);
CheckString2(141, 6, $h{'data_type'}, $GetData::FLOAT64);
CheckNum2(141, 9, $h{'spf'}, 141);
CheckSArray2(141, 7, $h{'scalar'}, undef);
CheckArray2(141, 8, $h{'scalar_ind'}, undef);

# 142: gd_bof
$s = $_->bof("lincom");
CheckOK(142);
CheckNum(142, $s, 264);

# 143: divide entry
%h = $_->entry("div");
CheckOK(143);
CheckSArray2(143, 0, [ sort keys %h ], qw(field field_type fragment_index),
  "in_fields");
CheckString2(143, 1, $h{'field'}, "div");
CheckNum2(143, 2, $h{'field_type'}, $GetData::DIVIDE_ENTRY);
CheckNum2(143, 3, $h{'fragment_index'}, 0);
CheckSArray2(143, 4, $h{'in_fields'}, qw(mult bit));

# 145: recip entry
%h = $_->entry("recip");
CheckOK(145);
CheckSArray2(145, 1, [ sort keys %h ], qw(dividend field field_type),
  qw(fragment_index in_fields scalar scalar_ind));
CheckNum2(145, 2, $h{'dividend'}, 6.5 + 4.3 * i);
CheckString2(145, 3, $h{'field'}, "recip");
CheckNum2(145, 4, $h{'field_type'}, $GetData::RECIP_ENTRY);
CheckNum2(145, 5, $h{'fragment_index'}, 0);
CheckString2(145, 6, $h{'in_fields'}, "div");
CheckSArray2(145, 7, $h{'scalar'}, undef);
CheckArray2(145, 8, $h{'scalar_ind'}, undef);

# 146: add divide
$s = $_->add_divide(qw(new14 in2 in3));
CheckOK2(146, 0);
CheckNum2(146, 1, $s, 0);

%h = $_->entry("new14");
CheckOK(146);
CheckString2(146, 1, $h{'field'}, "new14");
CheckNum2(146, 2, $h{'field_type'}, $GetData::DIVIDE_ENTRY);
CheckNum2(146, 3, $h{'fragment_index'}, 0);
CheckSArray2(146, 4, $h{'in_fields'}, qw(in2 in3));

# 148: add recip
$s = $_->add_recip(qw(new16 in2), 33.3 + 44.4 * i);
CheckOK2(148, 0);
CheckNum2(148, 1, $s, 0);

%h = $_->entry("new16");
CheckOK(148);
CheckNum2(148, 2, $h{'dividend'}, 33.3 + 44.4 * i);
CheckString2(148, 3, $h{'field'}, "new16");
CheckNum2(148, 4, $h{'field_type'}, $GetData::RECIP_ENTRY);
CheckNum2(148, 5, $h{'fragment_index'}, 0);
CheckString2(148, 6, $h{'in_fields'}, "in2");
CheckSArray2(148, 7, $h{'scalar'}, undef);
CheckArray2(148, 8, $h{'scalar_ind'}, undef);

# 149: madd divide
$s = $_->madd_divide(qw(data mnew14 in1 in8));
CheckOK2(149, 0);
CheckNum2(149, 1, $s, 0);

%h = $_->entry("data/mnew14");
CheckOK(149);
CheckString2(149, 1, $h{'field'}, "data/mnew14");
CheckNum2(149, 2, $h{'field_type'}, $GetData::DIVIDE_ENTRY);
CheckNum2(149, 3, $h{'fragment_index'}, 0);
CheckSArray2(149, 4, $h{'in_fields'}, qw(in1 in8));

# 151: madd recip
$s = $_->madd_recip(qw(data mnew16 in2), 1.0);
CheckOK2(151, 0);
CheckNum2(151, 1, $s, 0);

%h = $_->entry("data/mnew16");
CheckOK(151);
CheckNum2(151, 2, $h{'dividend'}, 1);
CheckString2(151, 3, $h{'field'}, "data/mnew16");
CheckNum2(151, 4, $h{'field_type'}, $GetData::RECIP_ENTRY);
CheckNum2(151, 5, $h{'fragment_index'}, 0);
CheckString2(151, 6, $h{'in_fields'}, "in2");
CheckSArray2(151, 7, $h{'scalar'}, undef);
CheckArray2(151, 8, $h{'scalar_ind'}, undef);

# 152: alter_divide
$s = $_->alter_divide("new14", "in5");
CheckOK2(152, 1);
CheckNum2(152, 2, $s, 0);

%h = $_->entry("new14");
CheckOK2(152, 3);
CheckString2(152, 4, $h{'field'}, "new14");
CheckNum2(152, 5, $h{'field_type'}, $GetData::DIVIDE_ENTRY);
CheckNum2(152, 6, $h{'fragment_index'}, 0);
CheckSArray2(152, 7, $h{'in_fields'}, qw(in5 in3));

# 153: alter recip
$s = $_->alter_recip("new16", "in6", undef);
CheckOK2(153, 1);
CheckNum2(153, 2, $s, 0);

%h = $_->entry("new16");
CheckOK(151);
CheckNum2(151, 2, $h{'dividend'}, 33.3 + 44.4 * i);
CheckString2(151, 3, $h{'field'}, "new16");
CheckNum2(151, 4, $h{'field_type'}, $GetData::RECIP_ENTRY);
CheckNum2(151, 5, $h{'fragment_index'}, 0);
CheckString2(151, 6, $h{'in_fields'}, "in6");
CheckSArray2(151, 7, $h{'scalar'}, undef);
CheckArray2(151, 8, $h{'scalar_ind'}, undef);

# 155: rewrite fragment
$s = $_->rewrite_fragment(0);
CheckOK(155, 0);
CheckNum(155, $s, 0);

# 156: invalid dirfile
my $d = $_;
$_ = &GetData::invalid_dirfile;
CheckOK2(156, 0);
$s = $_->fragments;
CheckError2(156, 1, $GetData::E_BAD_DIRFILE);
CheckNum2(156, 2, $s, $GetData::E_BAD_DIRFILE);
$_->close;
$_ = $d;

# 157: dirfile standards
$s = $_->dirfile_standards;
CheckOK2(157, 1);
CheckNum2(157, 2, $s, $GetData::DIRFILE_STANDARDS_VERSION);

$s = $_->dirfile_standards(0);
CheckError2(157, 3, $GetData::E_ARGUMENT);
CheckNum2(157, 4, $s, undef);

# 158: get_carray
$s = $_->get_carray("carray", $GetData::INT8);
CheckOK(158);
CheckString(158, $s, "\1\2\3\4\5\6");

# 164: get_carray_slice
@a = $_->get_carray_slice("carray", 2, 2, $GetData::FLOAT64);
CheckOK(164);
CheckArray(164, \@a, 3.3, 4.4);

# 167: carrays
$s = $_->carrays($GetData::INT8);
CheckOK2(167, 1);
CheckNum2(167, 2, $#$s, 0);
CheckSArray2(167, 3, $s, "\1\2\3\4\5\6");

@a = $_->carrays($GetData::FLOAT64);
CheckOK2(167, 4);
CheckNum2(167, 5, $#a, 0);
CheckArray2(167, 6, $a[0], 1.1, 2.2, 3.3, 4.4, 5.5, 6.6 );

# 168: put carray
$s = $_->put_carray("carray", 9, 8, 7, 6, 5, 4);
CheckOK2(168, 1);
CheckNum2(168, 2, $s, 0);

@a = $_->get_carray("carray", $GetData::INT8);
CheckOK2(168, 3);
CheckArray2(168, 4, \@a, 9, 8, 7, 6, 5, 4);

# 169: put carray slice
$s = $_->put_carray_slice("carray", 2, [ 169, 169 ]);
CheckOK2(169, 1);
CheckNum2(169, 2, $s, 0);

@a = $_->get_carray("carray", $GetData::UINT8);
CheckOK2(169, 3);
CheckArray2(169, 4, \@a, 9, 8, 169, 169, 5, 4);

# 177: array len
$s = $_->array_len("carray");
CheckOK(177);
CheckNum(177, $s, 6);

# 178: carray entry
%h = $_->entry("carray");
CheckOK2(178, 1);
CheckSArray2(178, 2, [ sort keys %h ],
  qw(array_len const_type field field_type fragment_index));
CheckNum2(178, 3, $h{'array_len'}, 6);
CheckNum2(178, 4, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(178, 5, $h{'field'}, "carray");
CheckNum2(178, 6, $h{'field_type'}, $GetData::CARRAY_ENTRY);
CheckNum2(178, 7, $h{'fragment_index'}, 0);

# 179: add carray
$s = $_->add_carray("new17", $GetData::FLOAT64, 0, undef, 1.79, 17.9);
CheckOK2(179, 1);
CheckNum2(179, 2, $s, 0);

%h = $_->entry("new17");
CheckOK2(179, 3);
CheckNum2(179, 4, $h{'array_len'}, 2);
CheckNum2(179, 5, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(179, 6, $h{'field'}, "new17");
CheckNum2(179, 7, $h{'field_type'}, $GetData::CARRAY_ENTRY);
CheckNum2(179, 8, $h{'fragment_index'}, 0);

@a = $_->get_carray("new17", $GetData::FLOAT64);
CheckOK2(179, 9);
CheckArray2(179, 10, \@a, 1.79, 17.9);

# 180: madd carray
$s = $_->madd_carray("data", "mnew17", $GetData::FLOAT64, undef, 1.80, 18.0);
CheckOK2(180, 1);
CheckNum2(180, 2, $s, 0);

%h = $_->entry("data/mnew17");
CheckOK2(180, 3);
CheckNum2(180, 4, $h{'array_len'}, 2);
CheckNum2(180, 5, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(180, 6, $h{'field'}, "data/mnew17");
CheckNum2(180, 7, $h{'field_type'}, $GetData::CARRAY_ENTRY);
CheckNum2(180, 8, $h{'fragment_index'}, 0);

@a = $_->get_carray("data/mnew17", $GetData::FLOAT64);
CheckOK2(180, 9);
CheckArray2(180, 10, \@a, 1.80, 18.0);

# 181: alter carray
$s = $_->alter_carray("new17", $GetData::FLOAT32, 3);
CheckOK2(181, 0);
CheckNum2(181, 1, $s, 0);

%h = $_->entry("new17");
CheckOK2(181, 3);
CheckNum2(181, 4, $h{'array_len'}, 3);
CheckNum2(181, 5, $h{'const_type'}, $GetData::FLOAT32);
CheckString2(181, 6, $h{'field'}, "new17");
CheckNum2(181, 7, $h{'field_type'}, $GetData::CARRAY_ENTRY);
CheckNum2(181, 8, $h{'fragment_index'}, 0);

@a = $_->get_carray("new17", $GetData::FLOAT64);
CheckOK2(181, 9);
CheckArray2(181, 10, \@a, 1.79, 17.9, 0);

# 182: fragment list
@a = $_->fragments;
CheckOK(182);
CheckEOSArray(182, \@a, "dirfile/format");

# 183: constants
$s = $_->constants($GetData::UINT8);
CheckOK2(183, 0);
CheckString2(183, 1, $s, "V!");

@a = $_->constants($GetData::FLOAT64);
CheckOK2(183, 2);
CheckArray2(183, 3, \@a, 86, 33.3);

# 191: mconstants
$s = $_->mconstants("data", $GetData::UINT8);
CheckOK2(191, 0);
CheckString2(191, 1, $s, "\3\011");

@a = $_->mconstants("data", $GetData::FLOAT64);
CheckOK2(191, 2);
CheckArray2(191, 3, \@a, 3.3, 9.2);

# 199: strings
$s = $_->strings;
CheckOK2(199, 1);
CheckNum2(199, 2, $s, 3);

@a = $_->strings;
CheckOK2(199, 3);
CheckSArray2(199, 4, \@a, "Lorem ipsum", "a string", "Arthur Dent");

# 200: mstrings
$s = $_->mstrings("data");
CheckOK2(200, 1);
CheckNum2(200, 2, $s, 2);

@a = $_->mstrings("data");
CheckOK2(200, 3);
CheckSArray2(200, 4, \@a, "This is a string constant.", "another string");

# 203: seek
$s = $_->seek("data", 35, 0, $GetData::SEEK_SET);
CheckOK2(203, 0);
CheckNum2(203, 0, $s, 280);

$s = $_->getdata("data", $GetData::HERE, 0, 1, 0, $GetData::INT8);
CheckOK2(203, 1);
CheckNum2(203, 1, length($s), 8);
CheckString2(203, 2, $s, join "", map chr, 17 .. 24);

# 204: tell
$s = $_->tell("data");
CheckOK(204);
CheckNum(204,$s,288);

# 205: gd_hide check
$s = $_->hide('data');
CheckOK(205);

# 206: gd_hidden check
$s = $_->hidden('data');
CheckOK2(206, 1);
CheckNum2(206, 1, $s, 1);

$s = $_->hidden('lincom');
CheckOK2(206, 2);
CheckNum2(206, 2, $s, 0);

# 207: gd_unhide check
$s = $_->unhide('data');
CheckOK2(206, 1);
$s = $_->hidden('data');
CheckOK2(206, 2);
CheckNum(206, $s, 0);

# 208: gd_sync check
$s = $_->sync('data');
CheckOK(208);

# 209: gd_flush check
$s = $_->flush('data');
CheckOK(209);

# 210: gd_metaflush check
$s = $_->metaflush();
CheckOK(210);

# 211: gd_entry (WINDOW) check
%h = $_->entry('window');
CheckOK(211);
CheckNum2(211, 1, $h{"field_type"}, $GetData::WINDOW_ENTRY);
CheckNum2(211, 2, $h{"fragment_index"}, 0);
CheckNum2(211, 3, $h{"windop"}, $GetData::WINDOP_LT);
CheckSArray2(211, 4, $h{"in_fields"}, 'linterp', 'mult');
CheckNum2(211, 5, $h{"threshold"}, 4.1);

# 212: gd_add_window check
$s = $_->add_window('new18', 'in1', 'in2', $GetData::WINDOP_NE, 32, 0);
CheckOK2(212, 1);

%h = $_->entry('new18');
CheckOK2(212, 2);
CheckNum2(212, 1, $h{"field_type"}, $GetData::WINDOW_ENTRY);
CheckNum2(212, 2, $h{"fragment_index"}, 0);
CheckNum2(212, 3, $h{"windop"}, $GetData::WINDOP_NE);
CheckSArray2(212, 4, $h{"in_fields"}, 'in1', 'in2');
CheckNum2(212, 5, $h{"threshold"}, 32);

# 214: gd_madd_window check
$s = $_->madd_window('data', 'mnew18', 'in2', 'in3', $GetData::WINDOP_SET, 128);
CheckOK2(214, 1);

%h = $_->entry('data/mnew18');
CheckOK2(214, 2);
CheckNum2(214, 1, $h{"field_type"}, $GetData::WINDOW_ENTRY);
CheckNum2(214, 2, $h{"fragment_index"}, 0);
CheckNum2(214, 3, $h{"windop"}, $GetData::WINDOP_SET);
CheckSArray2(214, 4, $h{"in_fields"}, 'in2', 'in3');
CheckNum2(214, 5, $h{"threshold"}, 128);

# 217: gd_alter_window check
$s = $_->alter_window('new18', 'in3', 'in4', $GetData::WINDOP_GE, 32e3);
CheckOK2(217, 1);

%h = $_->entry('new18');
CheckOK2(217, 2);
CheckNum2(217, 1, $h{"field_type"}, $GetData::WINDOW_ENTRY);
CheckNum2(217, 2, $h{"fragment_index"}, 0);
CheckNum2(217, 3, $h{"windop"}, $GetData::WINDOP_GE);
CheckSArray2(217, 4, $h{"in_fields"}, 'in3', 'in4');
CheckNum2(217, 5, $h{"threshold"}, 32e3);

# 218: gd_alias_target check
$s = $_->alias_target('alias');
CheckOK(218);
CheckString(218, $s, 'data');

# 219: gd_add_alias check
$s = $_->add_alias('new20', 'data', 0);
CheckOK2(219, 1);

$s = $_->alias_target('new20');
CheckOK2(219, 2);
CheckString(219, $s, 'data');

# 220: gd_madd_alias check
$s = $_->madd_alias('data', 'mnew20', 'data');
CheckOK2(220, 1);

$s = $_->alias_target('data/mnew20');
CheckOK2(220, 2);
CheckString(220, $s, 'data');

# 221: gd_naliases check
$s = $_->aliases('data');
CheckOK(221);
CheckNum(221, $s, 4);

# 222: gd_aliases check
@a = $_->aliases('data');
CheckOK(222);
CheckSArray(222, \@a, "data", "alias", "new20", "data/mnew20");

# 223: gd_include_affix check
$s = $_->include_affix('format1', 0, 'A', 'Z',
  $GetData::CREAT | $GetData::EXCL);
CheckOK(223);

# 226: gd_fragment_affixes check
@a = $_->fragment_affixes(1);
CheckOK(226);
CheckSArray(226, \@a, "A", "Z");

# 227: gd_alter_affixes check
$s = $_->alter_affixes(1, 'B', '');
CheckOK2(227, 1);

@a = $_->fragment_affixes(1);
CheckOK2(227, 2);
CheckSArray(227, \@a, "B", "");

# 228: gd_entry (MPLEX) check
%h = $_->entry('mplex');
CheckOK(228);
CheckNum2(228, 1, $h{"field_type"}, $GetData::MPLEX_ENTRY);
CheckNum2(228, 2, $h{"fragment_index"}, 0);
CheckNum2(228, 3, $h{"count_val"}, 1);
CheckSArray2(228, 4, $h{"in_fields"}, 'data', 'sbit');
CheckNum2(228, 5, $h{"period"}, 10);

# 229: gd_add_mplex check
$s = $_->add_mplex('new21', 'in1', 'in2', 5, 6, 0);
CheckOK2(229, 1);

%h = $_->entry('new21');
CheckOK2(229, 2);
CheckNum2(229, 1, $h{"field_type"}, $GetData::MPLEX_ENTRY);
CheckNum2(229, 2, $h{"fragment_index"}, 0);
CheckNum2(229, 3, $h{"count_val"}, 5);
CheckSArray2(229, 4, $h{"in_fields"}, 'in1', 'in2');
CheckNum2(229, 5, $h{"period"}, 6);

# 230: gd_madd_mplex check
$s = $_->madd_mplex('data', 'mnew21', 'in2', 'in3', 0, 12);
CheckOK2(230, 1);

%h = $_->entry('data/mnew21');
CheckOK2(230, 2);
CheckNum2(230, 1, $h{"field_type"}, $GetData::MPLEX_ENTRY);
CheckNum2(230, 2, $h{"fragment_index"}, 0);
CheckNum2(230, 3, $h{"count_val"}, 0);
CheckSArray2(230, 4, $h{"in_fields"}, 'in2', 'in3');
CheckNum2(230, 5, $h{"period"}, 12);

# 231: gd_alter_mplex check
$s = $_->alter_mplex('new21', 'in3', 'in4', 7, -1);
CheckOK2(231, 1);

%h = $_->entry('new21');
CheckOK2(231, 2);
CheckNum2(231, 1, $h{"field_type"}, $GetData::MPLEX_ENTRY);
CheckNum2(231, 2, $h{"fragment_index"}, 0);
CheckNum2(231, 3, $h{"count_val"}, 7);
CheckSArray2(231, 4, $h{"in_fields"}, 'in3', 'in4');
CheckNum2(231, 5, $h{"period"}, 6);

# 232: gd_strtok check
@a = $_->strtok('"test1 test2" test3\ test4 test5');
CheckOK(232);
CheckSArray(232, \@a, "test1 test2", "test3 test4", "test5");

# 233: gd_raw_close check
$s = $_->raw_close('data');
CheckOK(233);

# 234: gd_desync
$s = $_->desync(0);
CheckOK(234);
CheckNum(234, $s, 0);

# 235: gd_flags
$s = $_->flags($GetData::PRETTY_PRINT, 0);
CheckOK(235);
CheckNum(235, $s, $GetData::PRETTY_PRINT);

# 236: gd_verbose_prefix
$s = $_->verbose_prefix("big_test: ");
CheckOK(236);
CheckNum(236, $s, 0);

# 237: gd_nentries
$s = $_->entry_list("data", $GetData::SCALAR_ENTRIES,
  $GetData::ENTRIES_HIDDEN | $GetData::ENTRIES_NOALIAS);
CheckOK2(237, 1);
CheckNum2(237, 1, $s, 7);
$s = $_->entry_list(undef, $GetData::VECTOR_ENTRIES,
  $GetData::ENTRIES_HIDDEN | $GetData::ENTRIES_NOALIAS);
CheckOK2(237, 2);
CheckNum2(237, 2, $s, 25);

# 239: gd_entry_list
@a = $_->entry_list(undef, $GetData::VECTOR_ENTRIES,
  $GetData::ENTRIES_HIDDEN | $GetData::ENTRIES_NOALIAS);
CheckOK(239);
CheckSArray(239, \@a, qw(bit div data mult new1 new3 new4 new6 new7 new8 sbit
  INDEX indir mplex new14 new16 new18 new21 phase recip lincom new135 window
  linterp polynom));

# 240: gd_mplex_lookback
$_->mplex_lookback($GetData::LOOKBACK_ALL);
CheckOK(240);

# 241: linterp_tablename
$s = $_->linterp_tablename("linterp");
CheckOK(241);
CheckEOString(241, $s, "dirfile/lut");

# 242: mcarrays
@a = $_->mcarrays("data", $GetData::FLOAT64);
CheckOK2(242, 1);
CheckNum2(242, 2, $#a, 1);
CheckArray2(242, 3, $a[0], 1.9, 2.8, 3.7, 4.6, 5.5 );
CheckArray2(242, 4, $a[1], 1.8, 18 );

# 243: add lincom
$s = $_->add({
    field          => 'new243',
    field_type     => $GetData::LINCOM_ENTRY,
    fragment_index => 0,
    in_fields      => [qw(in1 in2 in3)],
    m              => [1.1, undef, 1.4],
    scalar         => [undef, 'const', undef, 'carray', 'carray', 'carray'],
    scalar_ind     => [undef, undef,   undef, 3,        4,        5]
  });
CheckOK2(243, 1);
CheckNum2(243, 2, $s, 0);

%h = $_->entry("new243");
CheckOK2(243, 3);
CheckArray2(243, 4, $h{'b'}, 169, 5, 4);
CheckString2(243, 6, $h{'field'}, "new243");
CheckNum2(243, 7, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(243, 8, $h{'fragment_index'}, 0);
CheckSArray2(243, 9, $h{'in_fields'}, qw(in1 in2 in3));
CheckArray2(243, 10, $h{'m'}, 1.1, 86, 1.4);
CheckNum2(243, 11, $h{'n_fields'}, 3);
CheckSArray2(243, 12, $h{'scalar'}, undef, 'const', undef,
  'carray', 'carray', 'carray');
CheckArray2(243, 13, $h{'scalar_ind'}, undef, -1, undef, 3, 4, 5);

# 244: add polynom
$s = $_->add({
    field          => 'new244',
    field_type     => $GetData::POLYNOM_ENTRY,
    fragment_index => 0,
    in_fields      => 'in2',
    a              => [33, 44+i*55, 66],
    scalar         => [undef, undef, undef, 'carray'],
    scalar_ind     => [undef, undef, undef, 3       ]
  });
CheckOK2(244, 1);
CheckNum2(244, 2, $s, 0);

%h = $_->entry("new244");
CheckOK2(244, 3);
CheckArray2(244, 2, $h{'a'}, 33, 44+i*55, 66, 169);
CheckString2(244, 4, $h{'field'}, "new244");
CheckNum2(244, 5, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(244, 6, $h{'fragment_index'}, 0);
CheckString2(244, 7, $h{'in_fields'}, "in2");
CheckNum2(244, 8, $h{'poly_ord'}, 3);
CheckSArray2(244, 9, $h{'scalar'}, undef, undef, undef, "carray");
CheckArray2(244, 10, $h{'scalar_ind'}, undef, undef, undef, 3);

# 245: add linterp
$s = $_->add({
    field          => 'new245',
    field_type     => $GetData::LINTERP_ENTRY,
    fragment_index => 0,
    in_fields      => 'in',
    table          => './some/table'
  });
CheckOK2(245, 1);
CheckNum2(245, 2, $s, 0);

%h = $_->entry("new245");
CheckOK2(245, 2);
CheckString2(245, 3, $h{'field'}, "new245");
CheckNum2(245, 4, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(245, 5, $h{'fragment_index'}, 0);
CheckString2(245, 6, $h{'in_fields'}, "in");
CheckString2(245, 7, $h{'table'}, "./some/table");

# 246: add bit
$s = $_->add({
    field          => 'new246',
    field_type     => $GetData::BIT_ENTRY,
    fragment_index => 0,
    in_fields      => 'in1',
    bitnum         => 11
  });
CheckOK2(246, 1);
CheckNum2(246, 2, $s, 0);

%h = $_->entry("new246");
CheckOK(246, 2);
CheckNum2(246, 3, $h{'bitnum'}, 11);
CheckString2(246, 4, $h{'field'}, "new246");
CheckNum2(246, 5, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(246, 6, $h{'fragment_index'}, 0);
CheckString2(246, 7, $h{'in_fields'}, "in1");
CheckNum2(246, 8, $h{'numbits'}, 1);
CheckSArray2(246, 9, $h{'scalar'}, undef, undef);
CheckArray2(246, 10, $h{'scalar_ind'}, undef, undef);

# 247: add multiply
$s = $_->add({
    field          => 'new247',
    field_type     => $GetData::MULTIPLY_ENTRY,
    fragment_index => 0,
    in_fields      => [qw(in1 in2)]
  });
CheckOK2(247, 1);
CheckNum2(247, 2, $s, 0);

%h = $_->entry("new247");
CheckOK2(247, 3);
CheckString2(247, 4, $h{'field'}, "new247");
CheckNum2(247, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(247, 6, $h{'fragment_index'}, 0);
CheckSArray2(247, 7, $h{'in_fields'}, qw(in1 in2));

# 248: add phase
$s = $_->add({
    field          => "new248",
    field_type     => $GetData::PHASE_ENTRY,
    fragment_index => 0,
    in_fields      => "new9",
    shift          => -88
  });
CheckOK2(248,1);
CheckNum2(248, 2, $s, 0);

%h = $_->entry("new248");
CheckOK2(248, 2);
CheckString2(248, 3, $h{'field'}, "new248");
CheckNum2(248, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(248, 5, $h{'fragment_index'}, 0);
CheckString2(248, 6, $h{'in_fields'}, "new9");
CheckSArray2(248, 7, $h{'scalar'}, undef);
CheckArray2(248, 8, $h{'scalar_ind'}, undef);
CheckNum2(248, 9, $h{'shift'}, -88);

# 249: add const
$s = $_->add({
    field          => 'new249',
    field_type     => $GetData::CONST_ENTRY,
    fragment_index => 0,
    const_type     => $GetData::UINT8
  });
CheckOK2(249, 1);
CheckNum2(249, 2, $s, 0);

%h = $_->entry("new249");
CheckOK2(249, 3);
CheckNum2(249, 4, $h{'const_type'}, $GetData::UINT8);
CheckString2(249, 5, $h{'field'}, "new249");
CheckNum2(249, 6, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(249, 7, $h{'fragment_index'}, 0);

$s = $_->get_constant("new249", $GetData::FLOAT64);
CheckOK2(249, 8);
CheckNum2(249, 9, $s, 0);

# 250: add string
$s = $_->add({
    field          => 'new250',
    field_type     => $GetData::STRING_ENTRY,
    fragment_index => 0,
  });
CheckOK2(250, 1);
CheckNum2(250, 2, $s, 0);

%h = $_->entry("new250");
CheckOK2(250, 3);
CheckString2(250, 4, $h{'field'}, "new250");
CheckNum2(250, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(250, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("new250");
CheckOK2(250, 7);
CheckString2(250, 8, $s, "");

# 251: add recip
$s = $_->add({
    field          => 'Bnew251',
    field_type     => $GetData::RECIP_ENTRY,
    fragment_index => 1,
    in_fields      => 'Bin2',
    dividend       => 33.3+i*44.4,
    scalar         => undef
  });
CheckOK2(251, 0);
CheckNum2(251, 1, $s, 0);

%h = $_->entry("Bnew251");
CheckOK(251);
CheckNum2(251, 2, $h{'dividend'}, 33.3 + 44.4 * i);
CheckString2(251, 3, $h{'field'}, "Bnew251");
CheckNum2(251, 4, $h{'field_type'}, $GetData::RECIP_ENTRY);
CheckNum2(251, 5, $h{'fragment_index'}, 1);
CheckString2(251, 6, $h{'in_fields'}, "Bin2");

# 252: add const
$s = $_->add({
    field          => 'new252',
    field_type     => $GetData::CARRAY_ENTRY,
    fragment_index => 0,
    array_len      => 5,
    const_type     => $GetData::UINT8
  });
CheckOK2(252, 1);
CheckNum2(252, 2, $s, 0);

%h = $_->entry("new252");
CheckOK2(252, 3);
CheckNum2(252, 4, $h{'const_type'}, $GetData::UINT8);
CheckNum2(252, 5, $h{'array_len'}, 5);
CheckString2(252, 6, $h{'field'}, "new252");
CheckNum2(252, 7, $h{'field_type'}, $GetData::CARRAY_ENTRY);
CheckNum2(252, 8, $h{'fragment_index'}, 0);

@a = $_->get_carray("new252", $GetData::FLOAT64);
CheckOK2(252, 9);
CheckArray2(252, 10, \@a, 0, 0, 0, 0, 0);

# 253: add window
$s = $_->add({
    field          => 'new253',
    field_type     => $GetData::WINDOW_ENTRY,
    fragment_index => 0,
    in_fields      => [qw(in1 in2)],
    windop         => $GetData::WINDOP_NE,
    threshold      => 32
  });
CheckOK2(253, 1);
CheckNum2(253, 2, $s, 0);

%h = $_->entry('new253');
CheckOK2(253, 3);
CheckString2(253, 4, $h{'field'}, "new253");
CheckNum2(253, 5, $h{"field_type"}, $GetData::WINDOW_ENTRY);
CheckNum2(253, 6, $h{"fragment_index"}, 0);
CheckNum2(253, 7, $h{"windop"}, $GetData::WINDOP_NE);
CheckNum2(253, 8, $h{"threshold"}, 32);
CheckSArray2(253, 9, $h{"in_fields"}, 'in1', 'in2');

# 254: add mplex
$s = $_->add({
    field          => 'new254',
    field_type     => $GetData::MPLEX_ENTRY,
    fragment_index => 0,
    in_fields      => [qw(in1 in3)],
    count_val      => 254
  });
CheckOK2(254, 1);

%h = $_->entry('new254');
CheckOK2(254, 2);
CheckNum2(254, 1, $h{"field_type"}, $GetData::MPLEX_ENTRY);
CheckNum2(254, 2, $h{"fragment_index"}, 0);
CheckNum2(254, 3, $h{"count_val"}, 254);
CheckSArray2(254, 4, $h{"in_fields"}, 'in1', 'in3');
CheckNum2(254, 5, $h{"period"}, 0);

# 259: alter_entry with scalar
$s = $_->alter_entry('new243', {
    field_type     => $GetData::LINCOM_ENTRY,
    scalar         => [ undef, "", "const", "carray", undef, "const" ],
    scalar_ind     => [ undef, undef, undef,       4, undef,      -1 ]
  });
CheckOK2(259, 1);
CheckNum2(259, 2, $s, 0);

%h = $_->entry("new243");
CheckOK2(259, 3);
CheckArray2(259, 4, $h{'b'}, 5, 5, 86);
CheckString2(259, 6, $h{'field'}, "new243");
CheckNum2(259, 7, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(259, 8, $h{'fragment_index'}, 0);
CheckSArray2(259, 9, $h{'in_fields'}, qw(in1 in2 in3));
CheckArray2(259, 10, $h{'m'}, 1.1, 86, 86);
CheckNum2(259, 11, $h{'n_fields'}, 3);
CheckSArray2(259, 12, $h{'scalar'}, undef, 'const', 'const',
  'carray', undef, 'const');
CheckArray2(259, 13, $h{'scalar_ind'}, undef, -1, -1, 4, undef, -1);

# 272: NULL return from gd_reference
{
  local $_ = &GetData::open("dirfile/empty",
    $GetData::RDWR | $GetData::CREAT | $GetData::EXCL);
  CheckOK2(272, 1);

  $s = $_->reference();
  CheckOK2(272, 2);
  CheckNum(272, $s, undef);
}

# 273: get_carray (GD_NULL)
$s = $_->get_carray("carray", $GetData::NULL);
CheckOK(273);
CheckNum(273, $s, undef);
@a = $_->get_carray("carray", $GetData::NULL);
CheckOK(273);
CheckArray(273, \@a);

# 274: get_carray_slice (GD_NULL)
@a = $_->get_carray_slice("carray", 2, 2, $GetData::NULL);
CheckOK(274);
CheckNum(273, $s, undef);
@a = $_->get_carray_slice("carray", 2, 2, $GetData::NULL);
CheckOK(274);
CheckArray(274, \@a);

# 277: sarray entry
%h = $_->entry("sarray");
CheckOK2(277, 1);
CheckSArray2(277, 2, [ sort keys %h ],
  qw(array_len field field_type fragment_index));
CheckNum2(277, 3, $h{'array_len'}, 7);
CheckString2(277, 4, $h{'field'}, "sarray");
CheckNum2(277, 5, $h{'field_type'}, $GetData::SARRAY_ENTRY);
CheckNum2(277, 6, $h{'fragment_index'}, 0);

# 278: get_sarray
$s = $_->get_sarray("sarray");
CheckOK(278);
CheckSArray(278, $s, qw(one two three four five six seven));

# 279: get_sarray_slice
@a = $_->get_sarray_slice("sarray", 2, 2);
CheckOK(279);
CheckSArray(279, \@a, qw(three four));

# 280: sarrays
@a = $_->sarrays();
CheckOK(280);
CheckNum(280, $#a, 0);
CheckSArray(280, $a[0], qw(one two three four five six seven));

# 281: put sarray
$s = $_->put_sarray("sarray", qw(eka dvi tri catur panca sas sapta));
CheckOK2(281, 1);
CheckNum2(281, 2, $s, 0);

@a = $_->get_sarray("sarray");
CheckOK2(281, 3);
CheckSArray2(281, 4, \@a, qw(eka dvi tri catur panca sas sapta));

# 282: put sarray slice
$s = $_->put_sarray_slice("sarray", 2, [ "asta", "nava" ]);
CheckOK2(282, 1);
CheckNum2(282, 2, $s, 0);

$s = $_->get_sarray("sarray");
CheckOK2(282, 3);
CheckSArray2(282, 4, $s, qw(eka dvi asta nava panca sas sapta));

# 283: add sarray
$s = $_->add_sarray("new283", 0, qw(eins zwei drei));
CheckOK2(283, 1);
CheckNum2(283, 2, $s, 0);

%h = $_->entry("new283");
CheckOK2(283, 3);
CheckNum2(283, 4, $h{'array_len'}, 3);
CheckString2(283, 6, $h{'field'}, "new283");
CheckNum2(283, 7, $h{'field_type'}, $GetData::SARRAY_ENTRY);
CheckNum2(283, 8, $h{'fragment_index'}, 0);

$s = $_->get_sarray("new283");
CheckOK2(283, 9);
CheckSArray2(283, 10, $s, qw(eins zwei drei));

# 285: madd sarray
$s = $_->madd_sarray("data", "mnew285", [qw(un deux trois)]);
CheckOK2(285, 1);
CheckNum2(285, 2, $s, 0);

%h = $_->entry("data/mnew285");
CheckOK2(285, 3);
CheckNum2(285, 4, $h{'array_len'}, 3);
CheckString2(285, 6, $h{'field'}, "data/mnew285");
CheckNum2(285, 7, $h{'field_type'}, $GetData::SARRAY_ENTRY);
CheckNum2(285, 8, $h{'fragment_index'}, 0);

$s = $_->get_sarray("data/mnew285");
CheckOK2(285, 9);
CheckSArray2(285, 10, $s, qw(un deux trois));

# 286 alter sarray
$s = $_->alter_sarray("new283", 2);
CheckOK2(286, 0);
CheckNum2(286, 1, $s, 0);

%h = $_->entry("new283");
CheckOK2(286, 3);
CheckNum2(286, 4, $h{'array_len'}, 2);
CheckString2(286, 6, $h{'field'}, "new283");
CheckNum2(286, 7, $h{'field_type'}, $GetData::SARRAY_ENTRY);

# 287: msarrays
@a = $_->msarrays("data");
CheckOK(287);
CheckNum(287, $#a, 1);
CheckSArray2(287, 1, $a[0], qw(eight nine ten eleven twelve));
CheckSArray2(287, 2, $a[1], qw(un deux trois));

# 288 indir entry
%h = $_->entry("indir");
CheckOK(288);
CheckSArray2(288, 0, [ sort keys %h ], qw(field field_type fragment_index),
  "in_fields");
CheckString2(288, 1, $h{'field'}, "indir");
CheckNum2(288, 2, $h{'field_type'}, $GetData::INDIR_ENTRY);
CheckNum2(288, 3, $h{'fragment_index'}, 0);
CheckSArray2(288, 4, $h{'in_fields'}, qw(data carray));

# 289: add indir
$s = $_->add_indir(qw(new289 in2 in3));
CheckOK2(289, 0);
CheckNum2(289, 1, $s, 0);

%h = $_->entry("new289");
CheckOK(289);
CheckString2(289, 1, $h{'field'}, "new289");
CheckNum2(289, 2, $h{'field_type'}, $GetData::INDIR_ENTRY);
CheckNum2(289, 3, $h{'fragment_index'}, 0);
CheckSArray2(289, 4, $h{'in_fields'}, qw(in2 in3));

# 291 madd indir
$s = $_->madd_indir(qw(data new291 in1 in8));
CheckOK2(291, 0);
CheckNum2(291, 1, $s, 0);

%h = $_->entry("data/new291");
CheckOK(291);
CheckString2(291, 1, $h{'field'}, "data/new291");
CheckNum2(291, 2, $h{'field_type'}, $GetData::INDIR_ENTRY);
CheckNum2(291, 3, $h{'fragment_index'}, 0);
CheckSArray2(291, 4, $h{'in_fields'}, qw(in1 in8));

# 291 alter_indir
$s = $_->alter_indir("new289", "in5");
CheckOK2(291, 1);
CheckNum2(291, 2, $s, 0);

%h = $_->entry("new289");
CheckOK2(291, 3);
CheckString2(291, 4, $h{'field'}, "new289");
CheckNum2(291, 5, $h{'field_type'}, $GetData::INDIR_ENTRY);
CheckNum2(291, 6, $h{'fragment_index'}, 0);
CheckSArray2(291, 7, $h{'in_fields'}, qw(in5 in3));

# 292 sindir entry
%h = $_->entry("sindir");
CheckOK(292);
CheckSArray2(292, 0, [ sort keys %h ], qw(field field_type fragment_index),
  "in_fields");
CheckString2(292, 1, $h{'field'}, "sindir");
CheckNum2(292, 2, $h{'field_type'}, $GetData::SINDIR_ENTRY);
CheckNum2(292, 3, $h{'fragment_index'}, 0);
CheckSArray2(292, 4, $h{'in_fields'}, qw(data sarray));

# 293 add sindir
$s = $_->add_sindir(qw(new293 in2 in3));
CheckOK2(293, 0);
CheckNum2(293, 1, $s, 0);

%h = $_->entry("new293");
CheckOK(293);
CheckString2(293, 1, $h{'field'}, "new293");
CheckNum2(293, 2, $h{'field_type'}, $GetData::SINDIR_ENTRY);
CheckNum2(293, 3, $h{'fragment_index'}, 0);
CheckSArray2(293, 4, $h{'in_fields'}, qw(in2 in3));

# 294 madd sindir
$s = $_->madd_sindir(qw(data new294 in1 in8));
CheckOK2(294, 0);
CheckNum2(294, 1, $s, 0);

%h = $_->entry("data/new294");
CheckOK(294);
CheckString2(294, 1, $h{'field'}, "data/new294");
CheckNum2(294, 2, $h{'field_type'}, $GetData::SINDIR_ENTRY);
CheckNum2(294, 3, $h{'fragment_index'}, 0);
CheckSArray2(294, 4, $h{'in_fields'}, qw(in1 in8));

# 295 alter_sindir
$s = $_->alter_sindir("new293", "in5");
CheckOK2(295, 1);
CheckNum2(295, 2, $s, 0);

%h = $_->entry("new293");
CheckOK2(295, 3);
CheckString2(295, 4, $h{'field'}, "new293");
CheckNum2(295, 5, $h{'field_type'}, $GetData::SINDIR_ENTRY);
CheckNum2(295, 6, $h{'fragment_index'}, 0);
CheckSArray2(295, 7, $h{'in_fields'}, qw(in5 in3));

# 296 getstrdata
$s = $_->getdata("sindir", 0, 0, 1, 0);
CheckOK(296);
CheckSArray(296, $s, qw(eka eka eka eka eka eka eka eka));

# 300 add indir
$s = $_->add({
    field          => "new300",
    field_type     => $GetData::INDIR_ENTRY,
    fragment_index => 0,
    in_fields      => [ qw(in3 in4) ],
  });
CheckOK2(300,1);
CheckNum2(300, 2, $s, 0);

%h = $_->entry("new300");
CheckOK(300);
CheckString2(300, 1, $h{'field'}, "new300");
CheckNum2(300, 2, $h{'field_type'}, $GetData::INDIR_ENTRY);
CheckNum2(300, 3, $h{'fragment_index'}, 0);
CheckSArray2(300, 4, $h{'in_fields'}, qw(in3 in4));

# 301 add sindir
$s = $_->add({
    field          => "new301",
    field_type     => $GetData::SINDIR_ENTRY,
    fragment_index => 0,
    in_fields      => [ qw(in3 in4) ],
  });
CheckOK2(301,1);
CheckNum2(301, 2, $s, 0);

%h = $_->entry("new301");
CheckOK(301);
CheckString2(301, 1, $h{'field'}, "new301");
CheckNum2(301, 2, $h{'field_type'}, $GetData::SINDIR_ENTRY);
CheckNum2(301, 3, $h{'fragment_index'}, 0);
CheckSArray2(301, 4, $h{'in_fields'}, qw(in3 in4));

# 302: include namespace
$s = $_->include('format2', 0, 'ns', $GetData::CREAT | $GetData::EXCL);
CheckOK(302);
CheckNum(302, $s, 2);

# 303: fragment_namespace (read)
$s = $_->fragment_namespace(2);
CheckOK(303);
CheckString(303, $s, "ns");

# 304: fragment_namespace (alter)
$s = $_->fragment_namespace(2, 'ns2');
CheckOK(304);
CheckString(304, $s, "ns2");

# 305: match_entries
$s = scalar $_->match_entries('^lin', 0);
CheckOK2(305, 0);
CheckNum2(305, 1, $s, 2);

@a = $_->match_entries('^lin', 0);
CheckOK2(305, 2);
CheckSArray2(292, 3, \@a, qw(lincom linterp));










$d = $_ = undef;
system "rm -rf dirfile";
