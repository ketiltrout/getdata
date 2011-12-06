#!/usr/bin/perl -w
# Copyright (C) 2011 D. V. Wiebe
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
use Test::More tests => 1142;

my $ne = 0;
my ($s, @a, %h);

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

my $nfields = 14;
my @fields = (qw(INDEX bit carray const data div lincom linterp mult phase
polynom recip sbit string));

#create the dirfile
system "rm -rf dirfile" if (-e "dirfile" and not -d "dirfile");
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
/META data mlut LINTERP DATA ./lut
const CONST FLOAT64 5.5
carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6
linterp LINTERP data /look/up/file
polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const
bit BIT data 3 4
sbit SBIT data 5 6
mult MULTIPLY data sbit
div DIVIDE mult bit
recip RECIP div 6.5;4.3
phase PHASE data 11
string STRING \"Zaphod Beeblebrox\"
EOF
  or die;

open GLOB, ">dirfile/form2" or die;
print GLOB "const2 CONST INT8 -19\n" or die;
close GLOB;

# 0: error check
$_ = &GetData::open("x", $GetData::RDONLY);
CheckError(0, $GetData::E_OPEN);

# 1: open check
$_ = &GetData::open("dirfile", $GetData::RDWR);
CheckOK(1);

# 2: getdata (INT8) check
$s = $_->getdata("data", 5, 0, 1, 0, $GetData::INT8);
CheckOK(2);
CheckString(2, $s, join "", map chr, 41 .. 48);

# 102: getdata (unpacked) check
@a = $_->getdata("data", 5, 0, 1, 0, $GetData::INT16);
CheckOK(102);
CheckArray(102, \@a, 41 .. 48);

# 108: getdata (complex unpacked) check
@a = $_->getdata("data", 5, 0, 1, 0, $GetData::COMPLEX128);
CheckOK(108);
CheckArray(108, \@a, 41 .. 48);

# 3: constant (INT8) check
$s = $_->get_constant("const", $GetData::INT8);
CheckOK(3);
CheckNum(3, $s, 5);

# 116: constant (COMPLEX128) check
$s = $_->get_constant("const", $GetData::COMPLEX128);
CheckOK(116);
CheckNum(116, $s, 5.5);

# 6: nfields check
$s = $_->field_list;
CheckOK(6);
CheckNum(6, $s, $nfields);

# 8: field_list check
@a = $_->field_list;
CheckOK(8);
CheckSArray(8, \@a, @fields);

# 9: nmfields check
$s = $_->mfield_list("data");
CheckOK(9);
CheckNum(9, $s, 3);

# 10: mfield_list check
@a = $_->mfield_list("data");
CheckOK(10);
CheckSArray(10, \@a, qw(mstr mconst mlut));

# 11: nframes check
$s = $_->nframes;
CheckOK(11);
CheckNum(11, $s, 10);

# 12: spf check
$s = $_->spf("data");
CheckOK(12);
CheckNum(12, $s, 8);

# 13: putdata (packed) check
$s = $_->putdata("data", 5, 1, $GetData::UINT8, "\15\16\17\20");
CheckOK2(13,1);
CheckNum(13,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(13,2);
CheckArray(13, \@a, 41, 015, 016, 017, 020, 46, 47, 48);

# 118: putdata (typed ref) check
$s = $_->putdata("data", 5, 1, $GetData::UINT16, [ 23, 24, 25, 26 ]);
CheckOK2(118,1);
CheckNum(118,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(118,2);
CheckArray(118, \@a, 41, 23, 24, 25, 26, 46, 47, 48);

# 120: putdata (untyped ref) check
$s = $_->putdata("data", 5, 1, [ 33, 34, 35, 36 ]);
CheckOK2(120,1);
CheckNum(120,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(120,2);
CheckArray(120, \@a, 41, 33, 34, 35, 36, 46, 47, 48);

# 122: putdata (simple list) check
$s = $_->putdata("data", 5, 1, 23., 24., 25., 26.);
CheckOK2(122,1);
CheckNum(122,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(122,2);
CheckArray(122, \@a, 41, 23, 24, 25, 26, 46, 47, 48);

# 124: putdata (undef list) check
$s = $_->putdata("data", 5, 1, undef, 13.+0*i, 14.+0*i, 15.+0*i, 16.+0*i);
CheckOK2(124,1);
CheckNum(124,$s,4);

@a = $_->getdata("data", 5, 0, 1, 0, $GetData::UINT8);
CheckOK2(124,2);
CheckArray(124, \@a, 41, 13, 14, 15, 16, 46, 47, 48);

# 14: error_string check
$s = $_->getdata("x", 5, 0, 1, 0, $GetData::UINT8);
CheckError(14,$GetData::E_BAD_CODE);
CheckString(14, $_->error_string, "Field not found: x");

# 15: get_entry check
$s = $_->entry("data");
CheckOK(15);
CheckNum(15,$s,$GetData::RAW_ENTRY);

# 16: get_entry check
%h = $_->entry("data");
CheckOK(16);
CheckSArray2(16, 0, [ sort keys %h ],
  qw(data_type field field_type fragment_index scalar scalar_ind spf));
CheckNum2(16, 1, $h{'data_type'}, $GetData::INT8);
CheckString2(16, 2, $h{'field'}, "data");
CheckNum2(16, 3, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(16, 4, $h{'fragment_index'}, 0);
CheckSArray2(16, 6, $h{'scalar'}, undef);
CheckArray2(16, 7, $h{'scalar_ind'}, undef);
CheckNum2(16, 8, $h{'spf'}, 8);

# 18: get_entry check
%h = $_->entry("lincom");
CheckOK(18);
CheckSArray2(18, 1, [ sort keys %h ], qw(b field field_type),
  qw(fragment_index in_fields m n_fields scalar scalar_ind));
CheckArray2(18, 2, $h{'b'}, 2.2, 3.3+4.4*i, 5.5);
CheckString2(18, 5, $h{'field'}, "lincom");
CheckNum2(18, 6, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(18, 7, $h{'fragment_index'}, 0);
CheckSArray2(18, 8, $h{'in_fields'}, qw(data INDEX linterp));
CheckArray2(18, 3, $h{'m'}, 1.1, 2.2, 5.5);
CheckNum2(18, 10, $h{'n_fields'}, 3);
CheckSArray2(18, 11, $h{'scalar'}, undef, undef, "const", undef, undef,
  "const");
CheckArray2(18, 12, $h{'scalar_ind'}, undef, undef, -1, undef, undef, -1);

# 20: get_entry check
%h = $_->entry("polynom");
CheckOK(20);
CheckSArray2(20, 1, [ sort keys %h ], qw(a field field_type),
  qw(fragment_index in_field poly_ord scalar scalar_ind));
CheckArray2(20, 2, $h{'a'}, 1.1, 2.2, 2.2, 3.3+4.4*i, 5.5, 5.5);
CheckString2(20, 4, $h{'field'}, "polynom");
CheckNum2(20, 5, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(20, 6, $h{'fragment_index'}, 0);
CheckString2(20, 7, $h{'in_field'}, "data");
CheckNum2(20, 8, $h{'poly_ord'}, 5);
CheckSArray2(20, 9, $h{'scalar'}, undef, undef, undef, undef, "const", "const");
CheckArray2(20, 10, $h{'scalar_ind'}, undef, undef, undef, undef, -1, -1);

# 21: get_entry check
%h = $_->entry("linterp");
CheckOK(21);
CheckSArray2(21, 0, [ sort keys %h ], qw(field field_type fragment_index),
  qw(in_field table));
CheckString2(21, 1, $h{'field'}, "linterp");
CheckNum2(21, 2, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(21, 3, $h{'fragment_index'}, 0);
CheckString2(21, 4, $h{'in_field'}, "data");
CheckString2(21, 5, $h{'table'}, "/look/up/file");

# 22: get_entry check
%h = $_->entry("bit");
CheckOK(22);
CheckSArray2(22, 0, [ sort keys %h ], qw(bitnum field field_type),
  qw(fragment_index in_field numbits scalar scalar_ind));
CheckNum2(22, 1, $h{'bitnum'}, 3);
CheckString2(22, 2, $h{'field'}, "bit");
CheckNum2(22, 3, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(22, 4, $h{'fragment_index'}, 0);
CheckString2(22, 5, $h{'in_field'}, "data");
CheckNum2(22, 6, $h{'numbits'}, 4);
CheckSArray2(22, 7, $h{'scalar'}, undef, undef);
CheckArray2(22, 8, $h{'scalar_ind'}, undef, undef);

# 23: get_entry check
%h = $_->entry("sbit");
CheckOK(23);
CheckSArray2(23, 0, [ sort keys %h ], qw(bitnum field field_type),
  qw(fragment_index in_field numbits scalar scalar_ind));
CheckNum2(23, 1, $h{'bitnum'}, 5);
CheckString2(23, 2, $h{'field'}, "sbit");
CheckNum2(23, 3, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(23, 4, $h{'fragment_index'}, 0);
CheckString2(23, 5, $h{'in_field'}, "data");
CheckNum2(23, 6, $h{'numbits'}, 6);
CheckSArray2(23, 7, $h{'scalar'}, undef, undef);
CheckArray2(23, 8, $h{'scalar_ind'}, undef, undef);

# 24: get_entry check
%h = $_->entry("mult");
CheckOK(24);
CheckSArray2(24, 0, [ sort keys %h ], qw(field field_type fragment_index),
  "in_fields");
CheckString2(24, 1, $h{'field'}, "mult");
CheckNum2(24, 2, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(24, 3, $h{'fragment_index'}, 0);
CheckSArray2(24, 4, $h{'in_fields'}, qw(data sbit));

# 25: get_entry check
%h = $_->entry("phase");
CheckOK(25);
CheckSArray2(25, 0, [ sort keys %h ], qw(field field_type fragment_index),
  qw(in_field scalar scalar_ind shift));
CheckString2(25, 1, $h{'field'}, "phase");
CheckNum2(25, 2, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(25, 3, $h{'fragment_index'}, 0);
CheckString2(25, 4, $h{'in_field'}, "data");
CheckSArray2(25, 5, $h{'scalar'}, undef);
CheckArray2(25, 6, $h{'scalar_ind'}, undef);
CheckNum2(25, 7, $h{'shift'}, 11);

# 26: get_entry check
%h = $_->entry("const");
CheckOK(26);
CheckSArray2(26, 0, [ sort keys %h ], qw(const_type field field_type),
  "fragment_index");
CheckNum2(26, 1, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(26, 2, $h{'field'}, "const");
CheckNum2(26, 3, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(26, 4, $h{'fragment_index'}, 0);

# 134: get_entry check
%h = $_->entry("string");
CheckOK(134);
CheckSArray2(134, 0, [ sort keys %h ], qw(field field_type fragment_index));
CheckString2(134, 1, $h{'field'}, "string");
CheckNum2(134, 2, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(134, 3, $h{'fragment_index'}, 0);

# 27: fragment_index check
$s = $_->fragment_index("data");
CheckOK(27);
CheckNum(27, $s, 0);

# 28: add_raw check
$s = $_->add_raw("new1", $GetData::FLOAT64, 3);
CheckOK2(28, 1);
CheckNum2(28, 2, $s, 0);

%h = $_->entry("new1");
CheckOK2(28, 3);
CheckNum2(28, 4, $h{'data_type'}, $GetData::FLOAT64);
CheckString2(28, 5, $h{'field'}, "new1");
CheckNum2(28, 6, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(28, 7, $h{'fragment_index'}, 0);
CheckSArray2(28, 8, $h{'scalar'}, undef);
CheckArray2(28, 9, $h{'scalar_ind'}, undef);
CheckNum2(28, 10, $h{'spf'}, 3);

#30: add_lincom check
$s = $_->add_lincom("new3", 2, [ qw(in1 in2) ], [ 1.1+1.2*i, 1.4+1.5*i ],
  [ 1.3+1.4*i, 1.6+1.7*i ], 0);
CheckOK2(30, 1);
CheckNum2(30, 2, $s, 0);

%h = $_->entry("new3");
CheckOK2(30, 3);
CheckArray2(30, 4, $h{'b'}, 1.3+1.4*i, 1.6+1.7*i);
CheckString2(30, 6, $h{'field'}, "new3");
CheckNum2(30, 7, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(30, 8, $h{'fragment_index'}, 0);
CheckSArray2(30, 9, $h{'in_fields'}, qw(in1 in2));
CheckArray2(30, 10, $h{'m'}, 1.1+1.2*i, 1.4+1.5*i);
CheckNum2(30, 11, $h{'n_fields'}, 2);
CheckSArray2(30, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(30, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 31: add_polynom 
$s = $_->add_polynom("new4", 3, "in1", [ 3.9, 4.8, 5.7, 6.6 ], 0);
CheckOK2(31, 1);
CheckNum2(31, 2, $s, 0);

%h = $_->entry("new4");
CheckOK2(31, 3);
CheckArray2(31, 4, $h{'a'}, 3.9, 4.8, 5.7, 6.6);
CheckString2(31, 7, $h{'field'}, "new4");
CheckNum2(31, 8, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(31, 9, $h{'fragment_index'}, 0);
CheckString2(31, 10, $h{'in_field'}, "in1");
CheckNum2(31, 11, $h{'poly_ord'}, 3);
CheckSArray2(31, 12, $h{'scalar'}, undef, undef, undef, undef);
CheckArray2(31, 13, $h{'scalar_ind'}, undef, undef, undef, undef);

# 33: add_linterp
$s = $_->add_linterp("new6", "in", "./some/table");
CheckOK2(33, 1);
CheckNum2(33, 2, $s, 0);

%h = $_->entry("new6");
CheckOK2(33, 2);
CheckString2(33, 3, $h{'field'}, "new6");
CheckNum2(33, 4, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(33, 5, $h{'fragment_index'}, 0);
CheckString2(33, 6, $h{'in_field'}, "in");
CheckString2(33, 7, $h{'table'}, "./some/table");

# 34: add_bit
$s = $_->add_bit("new7", "in1", 11, 22);
CheckOK2(34, 1);
CheckNum2(34, 2, $s, 0);

%h = $_->entry("new7");
CheckOK(34, 2);
CheckNum2(34, 3, $h{'bitnum'}, 11);
CheckString2(34, 4, $h{'field'}, "new7");
CheckNum2(34, 5, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(34, 6, $h{'fragment_index'}, 0);
CheckString2(34, 7, $h{'in_field'}, "in1");
CheckNum2(34, 8, $h{'numbits'}, 22);
CheckSArray2(34, 9, $h{'scalar'}, undef, undef);
CheckArray2(34, 10, $h{'scalar_ind'}, undef, undef);

# 35: add_sbit
$s = $_->add_sbit("new8", "in2", 5, 10);
CheckOK2(35, 1);
CheckNum2(35, 2, $s, 0);

%h = $_->entry("new8");
CheckOK(35, 2);
CheckNum2(35, 3, $h{'bitnum'}, 5);
CheckString2(35, 4, $h{'field'}, "new8");
CheckNum2(35, 5, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(35, 6, $h{'fragment_index'}, 0);
CheckString2(35, 7, $h{'in_field'}, "in2");
CheckNum2(35, 8, $h{'numbits'}, 10);
CheckSArray2(35, 9, $h{'scalar'}, undef, undef);
CheckArray2(35, 10, $h{'scalar_ind'}, undef, undef);

# 36: add_mutiply
$s = $_->add_multiply("new9", "in2", "in3");
CheckOK2(36, 1);
CheckNum2(36, 2, $s, 0);

%h = $_->entry("new9");
CheckOK2(36, 3);
CheckString2(36, 4, $h{'field'}, "new9");
CheckNum2(36, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(36, 6, $h{'fragment_index'}, 0);
CheckSArray2(36, 7, $h{'in_fields'}, qw(in2 in3));

# 37: add_phase
$s = $_->add_phase("new10", "in6", 42);
CheckOK2(37, 1);
CheckNum2(37, 2, $s, 0);

%h = $_->entry("new10");
CheckOK2(37, 2);
CheckString2(37, 3, $h{'field'}, "new10");
CheckNum2(37, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(37, 5, $h{'fragment_index'}, 0);
CheckString2(37, 6, $h{'in_field'}, "in6");
CheckSArray2(37, 7, $h{'scalar'}, undef);
CheckArray2(37, 8, $h{'scalar_ind'}, undef);
CheckNum2(37, 9, $h{'shift'}, 42);

# 38: add_const
$s = $_->add_const("new11", $GetData::FLOAT64, "33.3");
CheckOK2(38, 1);
CheckNum2(38, 2, $s, 0);

%h = $_->entry("new11");
CheckOK2(38, 3);
CheckNum2(38, 4, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(38, 5, $h{'field'}, "new11");
CheckNum2(38, 6, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(38, 7, $h{'fragment_index'}, 0);

$s = $_->get_constant("new11", $GetData::FLOAT64);
CheckOK2(38, 8);
CheckNum2(38, 9, $s, 33.3);

# 125: add check
$s = $_->add({
    field          => "new13",
    field_type     => $GetData::PHASE_ENTRY,
    fragment_index => 0,
    in_field       => "new9",
    shift          => -88
  });
CheckOK2(125,1);
CheckNum2(125, 2, $s, 0);

%h = $_->entry("new13");
CheckOK2(125, 2);
CheckString2(125, 3, $h{'field'}, "new13");
CheckNum2(125, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(125, 5, $h{'fragment_index'}, 0);
CheckString2(125, 6, $h{'in_field'}, "new9");
CheckSArray2(125, 7, $h{'scalar'}, undef);
CheckArray2(125, 8, $h{'scalar_ind'}, undef);
CheckNum2(125, 9, $h{'shift'}, -88);

# 39: fragment_name
$s = $_->fragmentname(0);
CheckOK(39);
CheckEOString(39, $s, "dirfile/format");

# 40: nfragments
$s = $_->fragments;
CheckOK(40);
CheckNum(40, $s, 1);

# 182: fragment list
@a = $_->fragments;
CheckOK(182);
CheckEOSArray(182, \@a, "dirfile/format");

# 41: include
$s = $_->include("form2", 0, 0);
CheckOK2(41, 1);
CheckNum2(41, 2, $s, 1);

$s = $_->get_constant("const2", $GetData::INT32);
CheckOK2(41, 3);
CheckNum2(41, 4, $s, -19);

# 42: nfields_by_type
$s = $_->field_list_by_type($GetData::LINCOM_ENTRY);
CheckOK(42);
CheckNum(42, $s, 2);

# 43: field_list_by_type
@a = $_->field_list_by_type($GetData::LINCOM_ENTRY);
CheckOK(43);
CheckSArray(43, \@a, qw(lincom new3));

# 44: nvectors
$s = $_->vector_list;
CheckOK(44);
CheckNum(44, $s, 20);

# 45: vector_list
@a = $_->vector_list;
CheckOK(45);
CheckSArray(45, \@a, qw(INDEX bit data div lincom linterp mult new1 new10),
  qw(new13 new3 new4 new6 new7 new8 new9 phase polynom recip sbit));

#47: madd_lincom check
$s = $_->madd_lincom("data", "mnew2", 2, [ qw(in1 in2) ], [ 9.9+8.8*i, 7.7 ],
  [ 6.6, 5.5 ]);
CheckOK2(47, 1);
CheckNum2(47, 2, $s, 0);

%h = $_->entry("data/mnew2");
CheckOK2(47, 3);
CheckArray2(47, 4, $h{'b'}, 6.6, 5.5);
CheckArray2(47, 5, $h{'m'}, 9.9+8.8*i, 7.7);
CheckString2(47, 7, $h{'field'}, "data/mnew2");
CheckNum2(47, 8, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(47, 9, $h{'fragment_index'}, 0);
CheckSArray2(47, 10, $h{'in_fields'}, qw(in1 in2));
CheckNum2(47, 11, $h{'n_fields'}, 2);
CheckSArray2(47, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(47, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 48: madd_polynom 
$s = $_->madd_polynom("data", "mnew3", 3, "in1", [ 3.9, 4.8, 5.7, 6.6 ]);
CheckOK2(48, 1);
CheckNum2(48, 2, $s, 0);

%h = $_->entry("data/mnew3");
CheckOK2(48, 3);
CheckArray2(48, 4, $h{'a'}, 3.9, 4.8, 5.7, 6.6);
CheckString2(48, 7, $h{'field'}, "data/mnew3");
CheckNum2(48, 8, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(48, 9, $h{'fragment_index'}, 0);
CheckString2(48, 10, $h{'in_field'}, "in1");
CheckNum2(48, 11, $h{'poly_ord'}, 3);
CheckSArray2(48, 12, $h{'scalar'}, undef, undef, undef, undef);
CheckArray2(48, 13, $h{'scalar_ind'}, undef, undef, undef, undef);

# 50: madd_linterp
$s = $_->madd_linterp("data", "mnew6", "in", "./more/table");
CheckOK2(50, 1);
CheckNum2(50, 2, $s, 0);

%h = $_->entry("data/mnew6");
CheckOK2(50, 2);
CheckString2(50, 3, $h{'field'}, "data/mnew6");
CheckNum2(50, 4, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(50, 5, $h{'fragment_index'}, 0);
CheckString2(50, 6, $h{'in_field'}, "in");
CheckString2(50, 7, $h{'table'}, "./more/table");

# 51: madd_bit
$s = $_->madd_bit("data", "mnew7", "in1", 3, 2);
CheckOK2(51, 1);
CheckNum2(51, 2, $s, 0);

%h = $_->entry("data/mnew7");
CheckOK(51, 2);
CheckNum2(51, 3, $h{'bitnum'}, 3);
CheckString2(51, 4, $h{'field'}, "data/mnew7");
CheckNum2(51, 5, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(51, 6, $h{'fragment_index'}, 0);
CheckString2(51, 7, $h{'in_field'}, "in1");
CheckNum2(51, 8, $h{'numbits'}, 2);
CheckSArray2(51, 9, $h{'scalar'}, undef, undef);
CheckArray2(51, 10, $h{'scalar_ind'}, undef, undef);

# 52: madd_sbit
$s = $_->madd_sbit("data", "mnew8", "in2", 4, 5);
CheckOK2(52, 1);
CheckNum2(52, 2, $s, 0);

%h = $_->entry("data/mnew8");
CheckOK(52, 2);
CheckNum2(52, 3, $h{'bitnum'}, 4);
CheckString2(52, 4, $h{'field'}, "data/mnew8");
CheckNum2(52, 5, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(52, 6, $h{'fragment_index'}, 0);
CheckString2(52, 7, $h{'in_field'}, "in2");
CheckNum2(52, 8, $h{'numbits'}, 5);
CheckSArray2(52, 9, $h{'scalar'}, undef, undef);
CheckArray2(52, 10, $h{'scalar_ind'}, undef, undef);

# 53: madd_mutiply
$s = $_->madd_multiply("data", "mnew9", "in3", "in2");
CheckOK2(53, 1);
CheckNum2(53, 2, $s, 0);

%h = $_->entry("data/mnew9");
CheckOK2(53, 3);
CheckString2(53, 4, $h{'field'}, "data/mnew9");
CheckNum2(53, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(53, 6, $h{'fragment_index'}, 0);
CheckSArray2(53, 7, $h{'in_fields'}, qw(in3 in2));

# 54: madd_phase
$s = $_->madd_phase("data", "mnew10", "in6", 44);
CheckOK2(54, 1);
CheckNum2(54, 2, $s, 0);

%h = $_->entry("data/mnew10");
CheckOK2(54, 2);
CheckString2(54, 3, $h{'field'}, "data/mnew10");
CheckNum2(54, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(54, 5, $h{'fragment_index'}, 0);
CheckString2(54, 6, $h{'in_field'}, "in6");
CheckSArray2(54, 7, $h{'scalar'}, undef);
CheckArray2(54, 8, $h{'scalar_ind'}, undef);
CheckNum2(54, 9, $h{'shift'}, 44);

# 55: madd_const
$s = $_->madd_const("data", "mnew11", $GetData::FLOAT64, 9.2);
CheckOK2(55, 1);
CheckNum2(55, 2, $s, 0);

%h = $_->entry("data/mnew11");
CheckOK2(55, 3);
CheckNum2(55, 4, $h{'const_type'}, $GetData::FLOAT64);
CheckString2(55, 5, $h{'field'}, "data/mnew11");
CheckNum2(55, 6, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(55, 7, $h{'fragment_index'}, 0);

$s = $_->get_constant("data/mnew11", $GetData::FLOAT64);
CheckOK2(55, 8);
CheckNum2(55, 9, $s, 9.2);

# 126: madd check
$s = $_->madd({
    field          => "mnew13",
    field_type     => $GetData::PHASE_ENTRY,
    fragment_index => 0,
    in_field       => "data",
    shift          => 2 
  }, "data");
CheckOK2(126,1);
CheckNum2(126, 2, $s, 0);

%h = $_->entry("data/mnew13");
CheckOK2(126, 2);
CheckString2(126, 3, $h{'field'}, "data/mnew13");
CheckNum2(126, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(126, 5, $h{'fragment_index'}, 0);
CheckString2(126, 6, $h{'in_field'}, "data");
CheckSArray2(126, 7, $h{'scalar'}, undef);
CheckArray2(126, 8, $h{'scalar_ind'}, undef);
CheckNum2(126, 9, $h{'shift'}, 2);

# 56: get_string
$s = $_->get_string("string");
CheckOK(56);
CheckString(56, $s, "Zaphod Beeblebrox");

# 57: add_string
$s = $_->add_string("new12", "a string");
CheckOK2(57, 1);
CheckNum2(57, 2, $s, 0);

%h = $_->entry("new12");
CheckOK2(57, 3);
CheckString2(57, 4, $h{'field'}, "new12");
CheckNum2(57, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(57, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("new12");
CheckOK2(57, 7);
CheckString2(57, 8, $s, "a string");

# 58: madd_string
$s = $_->madd_string("data", "mnew12", "another string");
CheckOK2(58, 1);
CheckNum2(58, 2, $s, 0);

%h = $_->entry("data/mnew12");
CheckOK2(58, 3);
CheckString2(58, 4, $h{'field'}, "data/mnew12");
CheckNum2(58, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(58, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("data/mnew12");
CheckOK2(58, 7);
CheckString2(58, 8, $s, "another string");

# 59: add_spec
$s = $_->add_spec("lorem STRING \"Lorem ipsum\"", 0);
CheckOK2(59, 1);
CheckNum2(59, 2, $s, 0);

%h = $_->entry("lorem");
CheckOK2(59, 3);
CheckString2(59, 4, $h{'field'}, "lorem");
CheckNum2(59, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(59, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("lorem");
CheckOK2(59, 7);
CheckString2(59, 8, $s, "Lorem ipsum");

# 60: add_spec
$s = $_->madd_spec("ipsum STRING \"dolor sit amet.\"", "lorem");
CheckOK2(60, 1);
CheckNum2(60, 2, $s, 0);

%h = $_->entry("lorem/ipsum");
CheckOK2(60, 3);
CheckString2(60, 4, $h{'field'}, "lorem/ipsum");
CheckNum2(60, 5, $h{'field_type'}, $GetData::STRING_ENTRY);
CheckNum2(60, 6, $h{'fragment_index'}, 0);

$s = $_->get_string("lorem/ipsum");
CheckOK2(60, 7);
CheckString2(60, 8, $s, "dolor sit amet.");

# 61: put_constant
$s = $_->put_constant("const", 61);
CheckOK2(61, 1);
CheckNum2(61, 2, $s, 0);

$s = $_->get_constant("const", $GetData::FLOAT64);
CheckOK2(61, 3);
CheckNum2(61, 4, $s, 61.);

# 62: put_string
$s = $_->put_string("string", "Arthur Dent");
CheckOK2(62, 1);
CheckNum2(62, 2, $s, 12);

$s = $_->get_string("string");
CheckOK2(62, 3);
CheckString2(62, 4, $s, "Arthur Dent");

# 63: nmfields_by_type
$s = $_->mfield_list_by_type("data", $GetData::LINCOM_ENTRY);
CheckOK(63);
CheckNum(63, $s, 1);

# 64: mfield_list_by_type
@a = $_->mfield_list_by_type("data", $GetData::LINCOM_ENTRY);
CheckOK(64);
CheckSArray(64, \@a, qw(mnew2));

# 65: nmfields_by_type
$s = $_->mvector_list("data");
CheckOK(65);
CheckNum(65, $s, 9);

# 66: mfield_list_by_type
@a = $_->mvector_list("data");
CheckOK(66);
CheckSArray(66, \@a,
  qw(mlut mnew2 mnew3 mnew6 mnew7 mnew8 mnew9 mnew10 mnew13));

# 67: alter raw
$s = $_->alter_raw("new1", $GetData::INT32, undef);
CheckOK2(67, 1);
CheckNum2(67, 2, $s, 0);

%h = $_->entry("new1");
CheckOK2(67, 3);
CheckNum2(67, 4, $h{'data_type'}, $GetData::INT32);
CheckString2(67, 5, $h{'field'}, "new1");
CheckNum2(67, 6, $h{'field_type'}, $GetData::RAW_ENTRY);
CheckNum2(67, 7, $h{'fragment_index'}, 0);
CheckSArray2(67, 8, $h{'scalar'}, undef);
CheckArray2(67, 9, $h{'scalar_ind'}, undef);
CheckNum2(67, 10, $h{'spf'}, 3);

#69: alter_lincom check
$s = $_->alter_lincom("new3", undef, [ qw(in3 in4) ], [ 3., 4. ]);
CheckOK2(69, 1);
CheckNum2(69, 2, $s, 0);

%h = $_->entry("new3");
CheckOK2(69, 3);
CheckArray2(69, 4, $h{'b'}, 1.3+1.4*i, 1.6+1.7*i);
CheckArray2(69, 5, $h{'m'}, 3, 4);
CheckString2(69, 7, $h{'field'}, "new3");
CheckNum2(69, 8, $h{'field_type'}, $GetData::LINCOM_ENTRY);
CheckNum2(69, 9, $h{'fragment_index'}, 0);
CheckSArray2(69, 10, $h{'in_fields'}, qw(in3 in4));
CheckNum2(69, 11, $h{'n_fields'}, 2);
CheckSArray2(69, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(69, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 71: alter_polynom 
$s = $_->alter_polynom("new4", 4, undef, [ 1*i, 2*i, 3*i, 4*i, 5*i ]);
CheckOK2(71, 1);
CheckNum2(71, 2, $s, 0);

%h = $_->entry("new4");
CheckOK2(71, 3);
CheckArray2(71, 4, $h{'a'}, 1*i, 2*i, 3*i, 4*i, 5*i);
CheckString2(71, 7, $h{'field'}, "new4");
CheckNum2(71, 8, $h{'field_type'}, $GetData::POLYNOM_ENTRY);
CheckNum2(71, 9, $h{'fragment_index'}, 0);
CheckString2(71, 10, $h{'in_field'}, "in1");
CheckNum2(71, 11, $h{'poly_ord'}, 4);
CheckSArray2(71, 12, $h{'scalar'}, undef, undef, undef, undef, undef);
CheckArray2(71, 13, $h{'scalar_ind'}, undef, undef, undef, undef, undef);

# 72: alter_linterp
$s = $_->alter_linterp("new6", undef, "./other/table");
CheckOK2(72, 1);
CheckNum2(72, 2, $s, 0);

%h = $_->entry("new6");
CheckOK2(72, 2);
CheckString2(72, 3, $h{'field'}, "new6");
CheckNum2(72, 4, $h{'field_type'}, $GetData::LINTERP_ENTRY);
CheckNum2(72, 5, $h{'fragment_index'}, 0);
CheckString2(72, 6, $h{'in_field'}, "in");
CheckString2(72, 7, $h{'table'}, "./other/table");

# 73: alter_bit
$s = $_->alter_bit("new7", "in3", undef, 8);
CheckOK2(73, 1);
CheckNum2(73, 2, $s, 0);

%h = $_->entry("new7");
CheckOK(73, 2);
CheckNum2(73, 3, $h{'bitnum'}, 11);
CheckString2(73, 4, $h{'field'}, "new7");
CheckNum2(73, 5, $h{'field_type'}, $GetData::BIT_ENTRY);
CheckNum2(73, 6, $h{'fragment_index'}, 0);
CheckString2(73, 7, $h{'in_field'}, "in3");
CheckNum2(73, 8, $h{'numbits'}, 8);
CheckSArray2(73, 9, $h{'scalar'}, undef, undef);
CheckArray2(73, 10, $h{'scalar_ind'}, undef, undef);

# 74: alter_sbit
$s = $_->alter_sbit("new8", "in1");
CheckOK2(74, 1);
CheckNum2(74, 2, $s, 0);

%h = $_->entry("new8");
CheckOK(74, 2);
CheckNum2(74, 3, $h{'bitnum'}, 5);
CheckString2(74, 4, $h{'field'}, "new8");
CheckNum2(74, 5, $h{'field_type'}, $GetData::SBIT_ENTRY);
CheckNum2(74, 6, $h{'fragment_index'}, 0);
CheckString2(74, 7, $h{'in_field'}, "in1");
CheckNum2(74, 8, $h{'numbits'}, 10);
CheckSArray2(74, 9, $h{'scalar'}, undef, undef);
CheckArray2(74, 10, $h{'scalar_ind'}, undef, undef);

# 75: alter_mutiply
$s = $_->alter_multiply("new9", "in1");
CheckOK2(75, 1);
CheckNum2(75, 2, $s, 0);

%h = $_->entry("new9");
CheckOK2(75, 3);
CheckString2(75, 4, $h{'field'}, "new9");
CheckNum2(75, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(75, 6, $h{'fragment_index'}, 0);
CheckSArray2(75, 7, $h{'in_fields'}, qw(in1 in3));

# 76: alter_phase
$s = $_->alter_phase("new10", undef, -3);
CheckOK2(76, 1);
CheckNum2(76, 2, $s, 0);

%h = $_->entry("new10");
CheckOK2(76, 2);
CheckString2(76, 3, $h{'field'}, "new10");
CheckNum2(76, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(76, 5, $h{'fragment_index'}, 0);
CheckString2(76, 6, $h{'in_field'}, "in6");
CheckSArray2(76, 7, $h{'scalar'}, undef);
CheckArray2(76, 8, $h{'scalar_ind'}, undef);
CheckNum2(76, 9, $h{'shift'}, -3);

# 77: alter_const
$s = $_->alter_const("new11", $GetData::FLOAT32);
CheckOK2(77, 1);
CheckNum2(77, 2, $s, 0);

%h = $_->entry("new11");
CheckOK2(77, 3);
CheckNum2(77, 4, $h{'const_type'}, $GetData::FLOAT32);
CheckString2(77, 5, $h{'field'}, "new11");
CheckNum2(77, 6, $h{'field_type'}, $GetData::CONST_ENTRY);
CheckNum2(77, 7, $h{'fragment_index'}, 0);

# 141: alter check
$s = $_->alter_entry("new13", {
    field          => undef,
    field_type     => $GetData::PHASE_ENTRY,
    fragment_index => 0,
    in_field       => "new3",
    shift          => -2
  });
CheckOK2(141,1);
CheckNum2(141, 2, $s, 0);

%h = $_->entry("new13");
CheckOK2(141, 2);
CheckString2(141, 3, $h{'field'}, "new13");
CheckNum2(141, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(141, 5, $h{'fragment_index'}, 0);
CheckString2(141, 6, $h{'in_field'}, "new3");
CheckSArray2(141, 7, $h{'scalar'}, undef);
CheckArray2(141, 8, $h{'scalar_ind'}, undef);
CheckNum2(141, 9, $h{'shift'}, -2);

# 78: encoding check
$s = $_->encoding(0);
CheckOK(78);
CheckNum(78, $s, $GetData::UNENCODED);

# 79: endianness check
$s = $_->endianness(0);
CheckOK(79);
CheckNum(79, $s, $GetData::LITTLE_ENDIAN | $GetData::NOT_ARM_ENDIAN);

# 80: dirfilename
$s = $_->dirfilename;
CheckOK(80);
CheckString(80, $s, "dirfile");

# 81: parent_fragment
$s = $_->parent_fragment(1);
CheckOK(81);
CheckNum(81, $s, 0);

# 82: alter_protection
$s = $_->alter_protection($GetData::PROTECT_DATA, 1);
CheckOK(82);
CheckNum(82, $s, 0);

# 83: protection
$s = $_->protection(1);
CheckOK(83);
CheckNum(83, $s, $GetData::PROTECT_DATA);

# 84: raw_filename
$s = $_->raw_filename("data");
CheckOK(84);
CheckEOString(84, $s, "dirfile/data");

# 85: reference
$s = $_->reference("new1");
CheckOK(85);
CheckString(85, $s, "new1");

# 87: alter_encoding
$s = $_->alter_encoding($GetData::SLIM_ENCODED, 1);
CheckOK2(87, 1);
CheckNum2(87, 2, $s, 0);

$s = $_->encoding(1);
CheckOK2(87, 3);
CheckNum2(87, 4, $s, $GetData::SLIM_ENCODED);

# 88: alter_endianness
$s = $_->alter_endianness($GetData::BIG_ENDIAN, 1);
CheckOK2(88, 1);
CheckNum2(88, 2, $s, 0);

$s = $_->endianness(1);
CheckOK2(88, 3);
CheckNum2(88, 4, $s, $GetData::BIG_ENDIAN);

# 89: alter_spec
$s = $_->alter_spec("new10 PHASE in const");
CheckOK2(89, 1);
CheckNum2(89, 2, $s, 0);

%h = $_->entry("new10");
CheckOK2(89, 2);
CheckString2(89, 3, $h{'field'}, "new10");
CheckNum2(89, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(89, 5, $h{'fragment_index'}, 0);
CheckString2(89, 6, $h{'in_field'}, "in");
CheckSArray2(89, 7, $h{'scalar'}, "const");
CheckArray2(89, 8, $h{'scalar_ind'}, -1);
CheckNum2(89, 9, $h{'shift'}, 61);

# 90: delete
$s = $_->delete("new10");
CheckOK2(90, 0);
CheckNum2(90, 1, $s, 0);

$s = $_->entry("new10");
CheckError2(90, 2, $GetData::E_BAD_CODE);
CheckNum2(90, 3, $s, undef);

# 91: malter_spec
$s = $_->malter_spec("mnew10 PHASE in4 11", "data");
CheckOK2(91, 0);
CheckNum2(91, 1, $s, 0);

%h = $_->entry("data/mnew10");
CheckOK2(91, 2);
CheckString2(91, 3, $h{'field'}, "data/mnew10");
CheckNum2(91, 4, $h{'field_type'}, $GetData::PHASE_ENTRY);
CheckNum2(91, 5, $h{'fragment_index'}, 0);
CheckString2(91, 6, $h{'in_field'}, "in4");
CheckSArray2(91, 7, $h{'scalar'}, undef);
CheckArray2(91, 8, $h{'scalar_ind'}, undef);
CheckNum2(91, 9, $h{'shift'}, 11);

# 92: move
$s = $_->move("new9", 1);
CheckOK2(92, 0);
CheckNum2(92, 1, $s, 0);

%h = $_->entry("new9");
CheckOK2(92, 3);
CheckString2(92, 4, $h{'field'}, "new9");
CheckNum2(92, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(92, 6, $h{'fragment_index'}, 1);
CheckSArray2(92, 7, $h{'in_fields'}, qw(in1 in3));

# 93: move
$s = $_->rename(qw(new9 newer));
CheckOK2(93, 0);
CheckNum2(93, 1, $s, 0);

$s = $_->entry("new9");
CheckError2(93, 2, $GetData::E_BAD_CODE);

%h = $_->entry("newer");
CheckOK2(93, 3);
CheckString2(93, 4, $h{'field'}, "newer");
CheckNum2(93, 5, $h{'field_type'}, $GetData::MULTIPLY_ENTRY);
CheckNum2(93, 6, $h{'fragment_index'}, 1);
CheckSArray2(93, 7, $h{'in_fields'}, qw(in1 in3));

# 94: uninclude
$s = $_->uninclude(1);
CheckOK2(94, 0);
CheckNum2(94, 1, $s, 0);

$s = $_->entry("newer");
CheckError2(94, 2, $GetData::E_BAD_CODE);

# 95: frameoffset
$s = $_->frameoffset(0);
CheckOK(95);
CheckNum(95, $s, 0);

# 96: alter_frameoffset
$s = $_->alter_frameoffset(33, 0);
CheckOK2(96, 0);
CheckNum2(96, 1, $s, 0);

$s = $_->frameoffset(0);
CheckOK2(96, 2);
CheckNum2(96, 3, $s, 33);

# 97: native_type
$s = $_->native_type("data");
CheckOK(97);
CheckNum(97, $s, $GetData::INT8);

# 99: validate
$s = $_->validate("new7");
CheckError(99, $GetData::E_BAD_CODE);
CheckNum(99, $s, undef);

# 101: framenum
$s = $_->framenum("data", 33.3, 6);
CheckOK(101);
CheckNum(101, $s, 37.0375);

# 86: gd_eof
$s = $_->eof("lincom");
CheckOK(86);
CheckNum(86, $s, 345);

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
  qw(fragment_index in_field scalar scalar_ind));
CheckNum2(145, 2, $h{'dividend'}, 6.5 + 4.3 * i);
CheckString2(145, 3, $h{'field'}, "recip");
CheckNum2(145, 4, $h{'field_type'}, $GetData::RECIP_ENTRY);
CheckNum2(145, 5, $h{'fragment_index'}, 0);
CheckString2(145, 6, $h{'in_field'}, "div");
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
CheckString2(148, 6, $h{'in_field'}, "in2");
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

# 151: add recip
$s = $_->madd_recip(qw(data mnew16 in2), 1.0);
CheckOK2(151, 0);
CheckNum2(151, 1, $s, 0);

%h = $_->entry("data/mnew16");
CheckOK(151);
CheckNum2(151, 2, $h{'dividend'}, 1);
CheckString2(151, 3, $h{'field'}, "data/mnew16");
CheckNum2(151, 4, $h{'field_type'}, $GetData::RECIP_ENTRY);
CheckNum2(151, 5, $h{'fragment_index'}, 0);
CheckString2(151, 6, $h{'in_field'}, "in2");
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
CheckString2(151, 6, $h{'in_field'}, "in6");
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
CheckNum2(156, 2, $s, 0);
$_->close;
$_ = $d;

# 157: dirfile standards
$s = $_->dirfile_standards;
CheckOK2(157, 1);
CheckNum2(157, 2, $s, $GetData::DIRFILE_STANDARDS_VERSION);

$s = $_->dirfile_standards(0);
CheckError2(157, 3, $GetData::E_BAD_VERSION);
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

#169 put caray slice
$s = $_->put_carray_slice("carray", 2, [ 169, 169 ]);
CheckOK2(168, 1);
CheckNum2(168, 2, $s, 0);

@a = $_->get_carray("carray", $GetData::UINT8);
CheckOK2(168, 3);
CheckArray2(168, 4, \@a, 9, 8, 169, 169, 5, 4);

# 177: carray len
$s = $_->carray_len("carray");
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

# 180: add carray
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

# 183: constants
$s = $_->constants($GetData::UINT8);
CheckOK2(183, 0);
CheckString2(183, 1, $s, "=!");

@a = $_->constants($GetData::FLOAT64);
CheckOK2(183, 2);
CheckArray2(183, 3, \@a, 61, 33.3);

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





$d = $_ = undef;
system "rm -rf dirfile";
