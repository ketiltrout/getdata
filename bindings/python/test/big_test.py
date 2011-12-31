# Copyright (C) 2009-2010 D. V. Wiebe
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

import sys
import os
import re
import array
import pygetdata

if (pygetdata.__numpy_supported__):
  import numpy

def CheckOK(t):
  global ne
  ne+=1
  print "e[", t, "] =", sys.exc_type, sys.exc_value

def CheckOK2(t,m):
  global ne
  ne+=1
  print "e[", t, ",", m, "] =", sys.exc_type, sys.exc_value

def CheckException(t,g):
  global ne
  if (sys.exc_type != g):
    ne+=1
    print "e[", t, "] =", sys.exc_type, "expected", g

def CheckException2(t,m,g):
  global ne
  if (sys.exc_type != g):
    ne+=1
    print "e[", t, ",", m, "] =", sys.exc_type, "expected", g

def CheckNumpy(t,v,g):
  global ne
  if (numpy.any(v - g)):
    ne+=1
    print "a[", t, "] =", v, "expected", g

def CheckNumpy2(t,m,v,g):
  global ne
  if (numpy.any(v - g)):
    ne+=1
    print "a[", t, ",", m, "] =", v, "expected", g

def CheckSimple(t,v,g):
  global ne
  if (v != g):
    ne+=1
    print "n[", t, "] =", v, "expected", g

def CheckSimple2(t,m,v,g):
  global ne
  if (v != g):
    ne+=1
    print "n[", t, ",", m, "] =", v, "expected", g

def CheckEOS(t,v,g):
  global ne
  if (re.search(g + "$", v) == None):
    ne+=1
    print "n[", t, "] =", v, "expected", g

# create the dirfile first
data=array.array("B",range(1,81))
os.system("rm -rf dirfile")
os.mkdir("dirfile")
file=open("dirfile/data", 'w')
data.tofile(file)
file.close()

ne = 0

fields = ["INDEX", "alias", "bit", "carray", "const", "data", "div", "lincom",
"linterp", "mult", "phase", "polynom", "recip", "sbit", "string", "window"]

nfields = 16
file=open("dirfile/format", 'w')
file.write(
    "/ENDIAN little\n"
    "data RAW INT8 8\n"
    "lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const\n"
    "/META data mstr STRING \"This is a string constant.\"\n"
    "/META data mconst CONST COMPLEX128 3.3;4.4\n"
    "/META data mlut LINTERP DATA ./lut\n"
    "const CONST FLOAT64 5.5\n"
    "carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6\n"
    "linterp LINTERP data /look/up/file\n"
    "polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const\n"
    "bit BIT data 3 4\n"
    "sbit SBIT data 5 6\n"
    "mult MULTIPLY data sbit\n"
    "div DIVIDE mult bit\n"
    "recip RECIP div 6.5;4.3\n"
    "phase PHASE data 11\n"
    "window WINDOW linterp mult LT 4.1\n"
    "/ALIAS alias data\n"
    "string STRING \"Zaphod Beeblebrox\"\n"
    )
file.close()

file=open("dirfile/form2", 'w')
file.write("const2 CONST INT8 -19\n")
file.close()

# 0: error check
try:
  d = pygetdata.dirfile("x", pygetdata.RDONLY)
except:
  CheckException(0, pygetdata.OpenError)

# 1: dirfile check
try:
  d = pygetdata.dirfile("dirfile", pygetdata.RDWR)
except:
  CheckOK(1)

# 2: getdata (int) check
try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1)
except:
  CheckOK(2)
CheckSimple(2,len(n),8)
if (pygetdata.__numpy_supported__):
  CheckNumpy(2,n,numpy.arange(41,49))
else:
  CheckSimple(2,n,range(41,49))

# 104: getdata (long) check
try:
  n = d.getdata("data", pygetdata.LONG, first_frame=5, num_frames=1)
except:
  CheckOK(104)
CheckSimple(104,len(n),8)
if (pygetdata.__numpy_supported__):
  CheckNumpy(104,n,numpy.arange(41L,49L))
else:
  CheckSimple(104,n,range(41L,49L))

# 106: getdata (float) check
try:
  n = d.getdata("data", pygetdata.FLOAT, first_frame=5, num_frames=1)
except:
  CheckOK(106)
CheckSimple(106,len(n),8)
if (pygetdata.__numpy_supported__):
  CheckNumpy(106,n,numpy.arange(41.,49.))
else:
  CheckSimple(106,n,[41.,42.,43.,44.,45.,46.,47.,48.])

# 108: getdata (complex) check
try:
  n = d.getdata("data", pygetdata.COMPLEX, first_frame=5, num_frames=1)
except:
  CheckOK(108)
CheckSimple(108,len(n),8)
if (pygetdata.__numpy_supported__):
  CheckNumpy(108,n,numpy.arange(41,49,dtype=numpy.complex128))
else:
  CheckSimple(108,n,[41.+0j,42.+0j,43.+0j,44.+0j,45.+0j,46.+0j,47.+0j,48.+0j])

# 3: constant (int) check
try:
  n = d.get_constant("const", pygetdata.INT)
except:
  CheckOK(3)
CheckSimple(3,n,5)

# 112: constant (long) check
try:
  n = d.get_constant("const", pygetdata.LONG)
except:
  CheckOK(112)
CheckSimple(112,n,5L)

# 114: constant (float) check
try:
  n = d.get_constant("const", pygetdata.FLOAT)
except:
  CheckOK(114)
CheckSimple(114,n,5.5)

# 116: constant (float) check
try:
  n = d.get_constant("const", pygetdata.COMPLEX)
except:
  CheckOK(116)
CheckSimple(116,n,5.5+0j)

# 6: nfields check
try:
  n = d.nfields()
except:
  CheckOK(6)
CheckSimple(6,n,nfields)

# 8: field_list check
try:
  n = d.field_list()
except:
  CheckOK(8)
CheckSimple(8,n,fields)

# 9: nmfields check
try:
  n = d.nmfields("data")
except:
  CheckOK(9)
CheckSimple(9,n,3)

# 10: mfield_list check
try:
  n = d.mfield_list("data")
except:
  CheckOK(10)
CheckSimple(10,n,["mstr", "mconst", "mlut"])

# 11: nframes check
try:
  n = d.nframes
except:
  CheckOK(11)
CheckSimple(11,n,10)

# 12: spf check
try:
  n = d.spf("data")
except:
  CheckOK(12)
CheckSimple(12,n,8)

# 13: putdata (int) check
p = [ 13, 14, 15, 16 ]
try:
  n = d.putdata("data", p, pygetdata.INT, first_frame=5, first_sample=1)
except:
  CheckOK2(13,1)
CheckSimple2(13,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK(13,2)
CheckSimple2(13,2,n,[41, 13, 14, 15, 16, 46, 47, 48])

# 119: putdata (numpy) check
if (pygetdata.__numpy_supported__):
  p = numpy.array([ 73, 74, 75, 76 ])
  try:
    n = d.putdata("data", p, first_frame=5, first_sample=1)
  except:
    CheckOK2(13,1)
  CheckSimple2(13,1,n,4)

  try:
    n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1)
  except:
    CheckOK(13,2)
  CheckNumpy2(13,2,n,numpy.array([41, 73, 74, 75, 76, 46, 47, 48]))

# 120: putdata (long) check
p = [ 23L, 24L, 25L, 26L ]
try:
  n = d.putdata("data", p, pygetdata.LONG, first_frame=5, first_sample=1)
except:
  CheckOK2(120,1)
CheckSimple2(120,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK(120,2)
CheckSimple2(120,2,n,[41, 23, 24, 25, 26, 46, 47, 48])

# 122: putdata (float) check
p = [ 33., 34., 35., 36. ]
try:
  n = d.putdata("data", p, pygetdata.FLOAT, first_frame=5, first_sample=1)
except:
  CheckOK2(122,1)
CheckSimple2(122,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK(122,2)
CheckSimple2(122,2,n,[41, 33, 34, 35, 36, 46, 47, 48])

# 124: putdata (complex) check
p = [ 43.+0j, 44.+0j, 45.+0j, 46.+0j ]
try:
  n = d.putdata("data", p, pygetdata.COMPLEX, first_frame=5, first_sample=1)
except:
  CheckOK2(124,1)
CheckSimple2(124,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK2(124,2)
CheckSimple2(124,2,n,[41, 43, 44, 45, 46, 46, 47, 48])

# 136: putdata (auto) check
p = [ 53.+0j, 54.+0j, 55.+0j, 56.+0j ]
try:
  n = d.putdata("data", p, first_frame=5, first_sample=1)
except:
  CheckOK2(136,1)
CheckSimple2(136,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK2(136,2)
CheckSimple2(136,2,n,[41, 53, 54, 55, 56, 46, 47, 48])

# 14: error_string check
try:
  n = d.getdata("x", pygetdata.INT, first_frame=5, num_frames=1)
except:
  CheckException(14,pygetdata.BadCodeError)
  CheckSimple(14,d.error_string,"Field not found: x")

# 16: entry (raw) check
try:
  ent = d.entry("data")
except:
  CheckOK(16)
CheckSimple2(16,1,ent.field_type,pygetdata.RAW_ENTRY)
CheckSimple2(16,2,ent.field_type_name,"RAW_ENTRY")
CheckSimple2(16,3,ent.fragment,0)
CheckSimple2(16,4,ent.data_type,pygetdata.INT8)
CheckSimple2(16,5,ent.data_type_name,"INT8")
CheckSimple2(16,6,ent.spf,8)

# 18: entry (lincom) check
try:
  ent = d.entry("lincom")
except:
  CheckOK(18)
CheckSimple2(18,1,ent.field_type,pygetdata.LINCOM_ENTRY)
CheckSimple2(18,2,ent.field_type_name,"LINCOM_ENTRY")
CheckSimple2(18,3,ent.fragment,0)
CheckSimple2(18,4,ent.n_fields,3)
CheckSimple2(18,5,ent.in_fields,( "data", "INDEX", "linterp" ))
CheckSimple2(18,6,ent.m,(1.1, 2.2, "const"))
CheckSimple2(18,7,ent.b,(2.2, 3.3 + 4.4j, "const"))

# 20: entry (polynom) check
try:
  ent = d.entry("polynom")
except:
  CheckOK(20)
CheckSimple2(20,1,ent.field_type,pygetdata.POLYNOM_ENTRY)
CheckSimple2(20,2,ent.field_type_name,"POLYNOM_ENTRY")
CheckSimple2(20,3,ent.fragment,0)
CheckSimple2(20,4,ent.poly_ord,5)
CheckSimple2(20,5,ent.in_fields,( "data", ))
CheckSimple2(20,6,ent.a,(1.1, 2.2, 2.2, 3.3 + 4.4j, "const", "const"))

# 21: entry (linterp) check
try:
  ent = d.entry("linterp")
except:
  CheckOK(21)
CheckSimple2(21,1,ent.field_type,pygetdata.LINTERP_ENTRY)
CheckSimple2(21,2,ent.field_type_name,"LINTERP_ENTRY")
CheckSimple2(21,3,ent.fragment,0)
CheckSimple2(21,4,ent.in_fields,( "data", ))
CheckSimple2(21,5,ent.table,"/look/up/file")

# 22: entry (bit) check
try:
  ent = d.entry("bit")
except:
  CheckOK(22)
CheckSimple2(22,1,ent.field_type,pygetdata.BIT_ENTRY)
CheckSimple2(22,2,ent.field_type_name,"BIT_ENTRY")
CheckSimple2(22,3,ent.fragment,0)
CheckSimple2(22,4,ent.in_fields,( "data", ))
CheckSimple2(22,5,ent.numbits,4)
CheckSimple2(22,6,ent.bitnum,3)

# 23: entry (sbit) check
try:
  ent = d.entry("sbit")
except:
  CheckOK(23)
CheckSimple2(23,1,ent.field_type,pygetdata.SBIT_ENTRY)
CheckSimple2(23,2,ent.field_type_name,"SBIT_ENTRY")
CheckSimple2(23,3,ent.fragment,0)
CheckSimple2(23,4,ent.in_fields,( "data", ))
CheckSimple2(23,5,ent.numbits,6)
CheckSimple2(23,6,ent.bitnum,5)

# 24: entry (mult) check
try:
  ent = d.entry("mult")
except:
  CheckOK(24)
CheckSimple2(24,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(24,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(24,3,ent.fragment,0)
CheckSimple2(24,4,ent.in_fields,( "data", "sbit"))

# 25: entry (phase) check
try:
  ent = d.entry("phase")
except:
  CheckOK(25)
CheckSimple2(25,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(25,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(25,3,ent.fragment,0)
CheckSimple2(25,4,ent.in_fields,( "data", ))
CheckSimple2(25,5,ent.shift,11)

# 26: entry (const) check
try:
  ent = d.entry("const")
except:
  CheckOK(26)
CheckSimple2(26,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(26,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(26,3,ent.fragment,0)
CheckSimple2(26,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(26,5,ent.data_type_name,"FLOAT64")

# 134: entry (string) check
try:
  ent = d.entry("string")
except:
  CheckOK(134)
CheckSimple2(134,1,ent.field_type,pygetdata.STRING_ENTRY)
CheckSimple2(134,2,ent.field_type_name,"STRING_ENTRY")
CheckSimple2(134,3,ent.fragment,0)

# 27: fragment_index check
try:
  n = d.fragment_index("data")
except:
  CheckOK(27)
CheckSimple(27,n,0)

# 28: add / entry (raw) check
ent = pygetdata.entry(pygetdata.RAW_ENTRY, "new1", 0, (pygetdata.FLOAT64, 3))
try:
  d.add(ent)
except:
  CheckOK2(28,1)

try:
  ent = d.entry("new1")
except:
  CheckOK(28,2)
CheckSimple2(28,1,ent.field_type,pygetdata.RAW_ENTRY)
CheckSimple2(28,2,ent.field_type_name,"RAW_ENTRY")
CheckSimple2(28,3,ent.fragment,0)
CheckSimple2(28,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(28,5,ent.data_type_name,"FLOAT64")
CheckSimple2(28,6,ent.spf,3)

# 29: add / entry (lincom) check
ent = pygetdata.entry(pygetdata.LINCOM_ENTRY, "new2", 0,
    (("in1", "in2"), (9.9, 7.7), (8.8, 6.6)))
try:
  d.add(ent)
except:
  CheckOK2(29,1)

try:
  ent = d.entry("new2")
except:
  CheckOK(29,2)
CheckSimple2(29,1,ent.field_type,pygetdata.LINCOM_ENTRY)
CheckSimple2(29,2,ent.field_type_name,"LINCOM_ENTRY")
CheckSimple2(29,3,ent.fragment,0)
CheckSimple2(29,4,ent.n_fields,2)
CheckSimple2(29,5,ent.in_fields,( "in1", "in2" ))
CheckSimple2(29,6,ent.m,(9.9, 7.7))
CheckSimple2(29,7,ent.b,(8.8, 6.6))

# 31: add / entry (polynom) check
ent = pygetdata.entry(pygetdata.POLYNOM_ENTRY, "new4", 0,
    ("in1", (3.9, 4.8, 5.7, 6.6)))
try:
  d.add(ent)
except:
  CheckOK2(31,1)

try:
  ent = d.entry("new4")
except:
  CheckOK2(31,2)
CheckSimple2(31,1,ent.field_type,pygetdata.POLYNOM_ENTRY)
CheckSimple2(31,2,ent.field_type_name,"POLYNOM_ENTRY")
CheckSimple2(31,3,ent.fragment,0)
CheckSimple2(31,4,ent.poly_ord,3)
CheckSimple2(31,5,ent.in_fields,( "in1", ))
CheckSimple2(31,6,ent.a,(3.9, 4.8, 5.7, 6.6))

# 33: add / entry (linterp) check
ent = pygetdata.entry(pygetdata.LINTERP_ENTRY, "new6", 0,
    ("in", "./some/table"))
try:
  d.add(ent)
except:
  CheckOK2(33,1)

try:
  ent = d.entry("new6")
except:
  CheckOK2(33,2)
CheckSimple2(33,1,ent.field_type,pygetdata.LINTERP_ENTRY)
CheckSimple2(33,2,ent.field_type_name,"LINTERP_ENTRY")
CheckSimple2(33,3,ent.fragment,0)
CheckSimple2(33,4,ent.in_fields,( "in", ))
CheckSimple2(33,5,ent.table,"./some/table")

# 34: add / entry (bit) check
ent = pygetdata.entry(pygetdata.BIT_ENTRY, "new7", 0, ("in", 13, 12))
try:
  d.add(ent)
except:
  CheckOK2(34,1)

try:
  ent = d.entry("new7")
except:
  CheckOK2(34,1)
CheckSimple2(34,1,ent.field_type,pygetdata.BIT_ENTRY)
CheckSimple2(34,2,ent.field_type_name,"BIT_ENTRY")
CheckSimple2(34,3,ent.fragment,0)
CheckSimple2(34,4,ent.in_fields,( "in", ))
CheckSimple2(34,5,ent.numbits,12)
CheckSimple2(34,6,ent.bitnum,13)

# 35: add / entry (sbit) check
ent = pygetdata.entry(pygetdata.SBIT_ENTRY, "new8", 0, ("in2", 14, 15))
try:
  d.add(ent)
except:
  CheckOK2(35,1)

try:
  ent = d.entry("sbit")
except:
  CheckOK2(35,2)
CheckSimple2(35,1,ent.field_type,pygetdata.SBIT_ENTRY)
CheckSimple2(35,2,ent.field_type_name,"SBIT_ENTRY")
CheckSimple2(35,3,ent.fragment,0)
CheckSimple2(35,4,ent.in_fields,( "data", ))
CheckSimple2(35,5,ent.numbits,6)
CheckSimple2(35,6,ent.bitnum,5)

# 36: add / entry (mult) check
ent = pygetdata.entry(pygetdata.MULTIPLY_ENTRY, "new9", 0, ("in1", "in2"))
try:
  d.add(ent)
except:
  CheckOK2(36,1)

try:
  ent = d.entry("new9")
except:
  CheckOK2(36,2)
CheckSimple2(36,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(36,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(36,3,ent.fragment,0)
CheckSimple2(36,4,ent.in_fields,( "in1", "in2"))

# 37: add / entry (phase) check
ent = pygetdata.entry(pygetdata.PHASE_ENTRY, "new10", 0, ("in1", 22))
try:
  d.add(ent)
except:
  CheckOK2(37,1)

try:
  ent = d.entry("new10")
except:
  CheckOK2(37,2)
CheckSimple2(37,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(37,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(37,3,ent.fragment,0)
CheckSimple2(37,4,ent.in_fields,( "in1", ))
CheckSimple2(37,5,ent.shift,22)

# 38: add / entry (const) check
ent = pygetdata.entry(pygetdata.CONST_ENTRY, "new11", 0, (pygetdata.FLOAT64,))
try:
  d.add(ent)
except:
  CheckOK2(38,1)

try:
  ent = d.entry("new11")
except:
  CheckOK2(38,2)
CheckSimple2(38,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(38,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(38,3,ent.fragment,0)
CheckSimple2(38,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(38,5,ent.data_type_name,"FLOAT64")

# 39: fragment check
try:
  f = d.fragment(0)
except:
  CheckOK(39)
CheckEOS(39,f.name,"dirfile/format")

# 40: nfragments check
try:
  n  = d.nfragments
except:
  CheckOK(40)
CheckSimple(40,n,1)

# 41: include check
try:
  n = d.include("form2")
except:
  CheckOK2(41,1)
CheckSimple2(41,1,n,1)

try:
  n = d.get_constant("const2", pygetdata.INT)
except:
  CheckOK2(41,2)
CheckSimple2(41,2,n,-19)

# 42: nfields_by_type check
try:
  n = d.nfields(pygetdata.LINCOM_ENTRY)
except:
  CheckOK(42)
CheckSimple(42,n,2)

# 43: field_list_by_type check
try:
  n = d.field_list(pygetdata.LINCOM_ENTRY)
except:
  CheckOK(43)
CheckSimple(43,n,["lincom", "new2"])

# 44: nvectors check
try:
  n = d.nvectors()
except:
  CheckOK(44)
CheckSimple(44,n,21)

# 45: field_list check
try:
  n = d.vector_list()
except:
  CheckOK(45)
CheckSimple(45,n,['INDEX', 'alias', 'bit', 'data', 'div', 'lincom', 'linterp',
  'mult', 'new1', 'new10', 'new2', 'new4', 'new6', 'new7', 'new8', 'new9',
  'phase', 'polynom', 'recip', 'sbit', 'window'])

# 46: add / entry (lincom) check
ent = pygetdata.entry(pygetdata.LINCOM_ENTRY, "mnew1", 0,
    {"in_fields": ("in1", "in2"), "m": (9.9, 7.7), "b": (8.8, 6.6)})
try:
  d.madd(ent, "data")
except:
  CheckOK2(46,1)

try:
  ent = d.entry("data/mnew1")
except:
  CheckOK(46,2)
CheckSimple2(46,1,ent.field_type,pygetdata.LINCOM_ENTRY)
CheckSimple2(46,2,ent.field_type_name,"LINCOM_ENTRY")
CheckSimple2(46,3,ent.fragment,0)
CheckSimple2(46,4,ent.n_fields,2)
CheckSimple2(46,5,ent.in_fields,( "in1", "in2" ))
CheckSimple2(46,6,ent.m,(9.9, 7.7))
CheckSimple2(46,7,ent.b,(8.8, 6.6))

# 48: add / entry (polynom) check
ent = pygetdata.entry(pygetdata.POLYNOM_ENTRY, "mnew3", 0,
    {"in_field": "in1", "a": (3.9, 4.8, 5.7, 6.6)})
try:
  d.madd(ent, "data")
except:
  CheckOK2(48,1)

try:
  ent = d.entry("data/mnew3")
except:
  CheckOK2(48,2)
CheckSimple2(48,1,ent.field_type,pygetdata.POLYNOM_ENTRY)
CheckSimple2(48,2,ent.field_type_name,"POLYNOM_ENTRY")
CheckSimple2(48,3,ent.fragment,0)
CheckSimple2(48,4,ent.poly_ord,3)
CheckSimple2(48,5,ent.in_fields,( "in1", ))
CheckSimple2(48,6,ent.a,(3.9, 4.8, 5.7, 6.6))

# 50: add / entry (linterp) check
ent = pygetdata.entry(pygetdata.LINTERP_ENTRY, "mnew6", 0,
    {"in_field": "in", "table": "./more/table"})
try:
  d.madd(ent, "data")
except:
  CheckOK2(50,1)

try:
  ent = d.entry("data/mnew6")
except:
  CheckOK2(50,2)
CheckSimple2(50,1,ent.field_type,pygetdata.LINTERP_ENTRY)
CheckSimple2(50,2,ent.field_type_name,"LINTERP_ENTRY")
CheckSimple2(50,3,ent.fragment,0)
CheckSimple2(50,4,ent.in_fields,( "in", ))
CheckSimple2(50,5,ent.table,"./more/table")

# 51: add / entry (bit) check
ent = pygetdata.entry(pygetdata.BIT_ENTRY, "mnew7", 0,
    {"in_field": "in1", "bitnum": 3, "numbits": 2})
try:
  d.madd(ent,"data")
except:
  CheckOK2(51,1)

try:
  ent = d.entry("data/mnew7")
except:
  CheckOK2(51,1)
CheckSimple2(51,1,ent.field_type,pygetdata.BIT_ENTRY)
CheckSimple2(51,2,ent.field_type_name,"BIT_ENTRY")
CheckSimple2(51,3,ent.fragment,0)
CheckSimple2(51,4,ent.in_fields,( "in1", ))
CheckSimple2(51,5,ent.numbits,2)
CheckSimple2(51,6,ent.bitnum,3)

# 52: add / entry (sbit) check
ent = pygetdata.entry(pygetdata.SBIT_ENTRY, "mnew8", 0,
    {"in_field": "in2", "bitnum": 4, "numbits": 5})
try:
  d.madd(ent,"data")
except:
  CheckOK2(52,1)

try:
  ent = d.entry("data/mnew8")
except:
  CheckOK2(52,2)
CheckSimple2(52,1,ent.field_type,pygetdata.SBIT_ENTRY)
CheckSimple2(52,2,ent.field_type_name,"SBIT_ENTRY")
CheckSimple2(52,3,ent.fragment,0)
CheckSimple2(52,4,ent.in_fields,( "in2", ))
CheckSimple2(52,5,ent.numbits,5)
CheckSimple2(52,6,ent.bitnum,4)

# 53: add / entry (mult) check
ent = pygetdata.entry(pygetdata.MULTIPLY_ENTRY, "mnew9", 0,
    {"in_field1": "in3", "in_field2": "in2"})
try:
  d.madd(ent,"data")
except:
  CheckOK2(53,1)

try:
  ent = d.entry("data/mnew9")
except:
  CheckOK2(53,2)
CheckSimple2(53,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(53,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(53,3,ent.fragment,0)
CheckSimple2(53,4,ent.in_fields,( "in3", "in2"))

# 54: add / entry (phase) check
ent = pygetdata.entry(pygetdata.PHASE_ENTRY, "mnew10", 0,
    {"in_field": "in3", "shift": 44})
try:
  d.madd(ent,"data")
except:
  CheckOK2(54,1)

try:
  ent = d.entry("data/mnew10")
except:
  CheckOK2(54,2)
CheckSimple2(54,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(54,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(54,3,ent.fragment,0)
CheckSimple2(54,4,ent.in_fields,( "in3", ))
CheckSimple2(54,5,ent.shift,44)

# 55: add / entry (const) check
ent = pygetdata.entry(pygetdata.CONST_ENTRY, "mnew11", 0,
    {"type": pygetdata.FLOAT64})
try:
  d.madd(ent,"data")
except:
  CheckOK2(55,1)

try:
  ent = d.entry("data/mnew11")
except:
  CheckOK2(55,2)
CheckSimple2(55,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(55,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(55,3,ent.fragment,0)
CheckSimple2(55,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(55,5,ent.data_type_name,"FLOAT64")

#56: string check
try:
  n = d.get_string("string")
except:
  CheckOK(56)
CheckSimple(56,n,"Zaphod Beeblebrox")

# 57: entry (string) check
ent = pygetdata.entry(pygetdata.STRING_ENTRY, "new12", 0)
try:
  d.add(ent)
except:
  CheckOK2(57,1)

try:
  ent = d.entry("new12")
except:
  CheckOK(57)
CheckSimple2(57,1,ent.field_type,pygetdata.STRING_ENTRY)
CheckSimple2(57,2,ent.field_type_name,"STRING_ENTRY")
CheckSimple2(57,3,ent.fragment,0)

# 59: add_spec check
try:
  d.add_spec("lorem STRING \"Lorem ipsum\"", 0)
except:
  CheckOK2(58,1)

try:
  n = d.get_string("lorem")
except:
  CheckOK2(59,2)
CheckSimple(59,n,"Lorem ipsum")

# 60: madd_spec check
try:
  d.madd_spec("ipsum STRING \"dolor sit amet.\"", "lorem")
except:
  CheckOK2(60,1)

try:
  n = d.get_string("lorem/ipsum")
except:
  CheckOK2(60,2)
CheckSimple(60,n,"dolor sit amet.")

# 61: put_constant / int check
try:
  d.put_constant("const", 61)
except:
  CheckOK2(61,1)

try:
  n = d.get_constant("const",pygetdata.INT)
except:
  CheckOK2(61,2)
CheckSimple(61,n,61)

# 128: put_constant / int check
try:
  d.put_constant("const", 128L)
except:
  CheckOK2(128,1)

try:
  n = d.get_constant("const",pygetdata.ULONG)
except:
  CheckOK2(128,2)
CheckSimple(128,n,128L)

# 129: put_constant / int check
try:
  d.put_constant("const", 129L)
except:
  CheckOK2(129,1)

try:
  n = d.get_constant("const",pygetdata.LONG)
except:
  CheckOK2(129,2)
CheckSimple(129,n,129L)

# 131: put_constant / float check
try:
  d.put_constant("const", 131.)
except:
  CheckOK2(131,1)

try:
  n = d.get_constant("const",pygetdata.FLOAT)
except:
  CheckOK2(131,2)
CheckSimple(131,n,131.)

# 133: put_constant / complex check
try:
  d.put_constant("const", 133.+0j)
except:
  CheckOK2(133,1)

try:
  n = d.get_constant("const",pygetdata.COMPLEX)
except:
  CheckOK2(133,2)
CheckSimple(133,n,133.+0j)

# 62: put_string
try:
  d.put_string("string", "Arthur Dent")
except:
  CheckOK2(62,1)

try:
  n = d.get_string("string")
except:
  CheckOK2(62,2)
CheckSimple(62,n,"Arthur Dent")

# 63: nmfields_by_type check
try:
  n = d.nmfields("data",pygetdata.LINCOM_ENTRY)
except:
  CheckOK(63)
CheckSimple(63,n,1)

# 64: mfield_list_by_type check
try:
  n = d.mfield_list("data",pygetdata.LINCOM_ENTRY)
except:
  CheckOK(64)
CheckSimple(64,n,["mnew1"])

# 65: nmfields_by_type check
try:
  n = d.nmvectors("data")
except:
  CheckOK(65)
CheckSimple(65,n,8)

# 66: mfield_list_by_type check
try:
  n = d.mvector_list("data")
except:
  CheckOK(66)
CheckSimple(66,n,['mlut', 'mnew1', 'mnew3', 'mnew6', 'mnew7', 'mnew8', 'mnew9',
  'mnew10'])

# 183: alter / raw check
ent = pygetdata.entry(pygetdata.RAW_ENTRY, "new1", 0,
    {"type": pygetdata.FLOAT32, "spf": 4})
try:
  n = d.alter("new1", ent)
except:
  CheckOK2(183,1)

try:
  ent = d.entry("new1")
except:
  CheckOK(183,2)
CheckSimple2(183,1,ent.field_type,pygetdata.RAW_ENTRY)
CheckSimple2(183,2,ent.field_type_name,"RAW_ENTRY")
CheckSimple2(183,3,ent.fragment,0)
CheckSimple2(183,4,ent.data_type,pygetdata.FLOAT32)
CheckSimple2(183,5,ent.data_type_name,"FLOAT32")
CheckSimple2(183,6,ent.spf,4)

# 78: encoding check
try:
  f = d.fragment(0)
  CheckSimple(78,f.encoding,pygetdata.UNENCODED)
except:
  CheckOK(78)

# 79: endianness check
try:
  CheckSimple(79,f.endianness,pygetdata.LITTLE_ENDIAN |
		  pygetdata.NOT_ARM_ENDIAN)
except:
  CheckOK(79)

# 80: dirfilename check
try:
  CheckSimple(80,d.name,"dirfile")
except:
  CheckOK(80)

# 81: parent_fragment check
try:
  f = d.fragment(1)
  CheckSimple(81,f.parent,0)
except:
  CheckOK(81)

# 82: dirfile_protect check
try:
  f.protection = pygetdata.PROTECT_DATA
except:
  CheckOK(82)
  
# 83: protection check
try:
  f = d.fragment(1)
  CheckSimple(83,f.protection,pygetdata.PROTECT_DATA)
except:
  CheckOK(83)

# 84: raw_filename check
try:
  n = d.raw_filename("data")
except:
  CheckOK(84)
CheckEOS(84,n,"dirfile/data")

# 85: reference check
try:
  d.reference = "new1"
except:
  CheckOK2(85,1)

try:
  CheckSimple(85,d.reference,"new1")
except:
  CheckOK2(85,2)

# 87: alter_encoding check
try:
  f = d.fragment(1)
  f.alter_encoding(pygetdata.SLIM_ENCODED,0)
except:
  CheckOK(87)

# 88: alter_endianness check
try:
  f.alter_endianness(pygetdata.BIG_ENDIAN,0)
except:
  CheckOK(88)

# 89: alter_spec check
try:
  d.alter_spec("new10 PHASE in5 3", 0)
except:
  CheckOK2(89,1)

try:
  ent = d.entry("new10")
except:
  CheckOK2(89,2)
CheckSimple2(89,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(89,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(89,3,ent.fragment,0)
CheckSimple2(89,4,ent.in_fields,( "in5", ))
CheckSimple2(89,5,ent.shift,3)

# 90: delete check
try:
  d.delete("new10",0)
except:
  CheckOK2(90,1)

try:
  ent = d.entry("new10")
except:
  CheckException2(90,2,pygetdata.BadCodeError)

# 91: malter_spec check
try:
  d.malter_spec("mnew10 PHASE in4 11", "data", 0)
except:
  CheckOK2(91,1)

try:
  ent = d.entry("data/mnew10")
except:
  CheckOK2(91,2)
CheckSimple2(91,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(91,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(91,3,ent.fragment,0)
CheckSimple2(91,4,ent.in_fields,( "in4", ))
CheckSimple2(91,5,ent.shift,11)

# 92: move check
try:
  d.move("new9", 1, 0)
except:
  CheckOK2(92,1)

try:
  ent = d.entry("new9")
except:
  CheckOK2(92,2)
CheckSimple2(92,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(92,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(92,3,ent.fragment,1)
CheckSimple2(92,4,ent.in_fields,( "in1", "in2"))

# 93: move check
try:
  d.rename("new9", "newer", 0)
except:
  CheckOK2(93,1)

try:
  ent = d.entry("new9")
except:
  CheckException2(93,2,pygetdata.BadCodeError)

try:
  ent = d.entry("newer")
except:
  CheckOK2(93,3)
CheckSimple2(93,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(93,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(93,3,ent.fragment,1)
CheckSimple2(93,4,ent.in_fields,( "in1", "in2"))

# 94: uninclude check
try:
  d.uninclude(1,0)
except:
  CheckOK2(94,1)

try:
  ent = d.entry("newer")
except:
  CheckException2(93,2,pygetdata.BadCodeError)

# 95: frameoffset check
try:
  f = d.fragment(0)
  CheckSimple(95,f.frameoffset,0)
except:
  CheckOK(95)

# 96: alter_frameoffset check
try:
  f.alter_frameoffset(33, 0)
  CheckSimple(96,f.frameoffset,33)
except:
  CheckOK(96)

# 97: native_type check
try:
  n = d.native_type("data")
except:
  CheckOK(97)
CheckSimple(97,n,pygetdata.INT8)

# 137: native_type_name check
try:
  n = d.native_type_name("data")
except:
  CheckOK(137)
CheckSimple(137,n,"INT8")

# 99: validate check
try:
  d.validate("new7")
except:
  CheckException(99,pygetdata.BadCodeError)

# 101: framenum check
try:
  n = d.framenum("data", 33.3, start=6)
except:
  CheckOK(101)
CheckSimple(101,n,37.0375)

# 86: eof check
try:
  n = d.eof("lincom")
except:
  CheckOK(86)
CheckSimple(86,n,344)

# 142: bof check
try:
  n = d.bof("lincom")
except:
  CheckOK(142)
CheckSimple(142,n,264)

# 143: entry (div) check
try:
  ent = d.entry("div")
except:
  CheckOK(143)
CheckSimple2(143,1,ent.field_type,pygetdata.DIVIDE_ENTRY)
CheckSimple2(143,2,ent.field_type_name,"DIVIDE_ENTRY")
CheckSimple2(143,3,ent.fragment,0)
CheckSimple2(143,4,ent.in_fields,( "mult", "bit"))

# 145: entry (recip) check
try:
  ent = d.entry("recip")
except:
  CheckOK(145)
CheckSimple2(145,1,ent.field_type,pygetdata.RECIP_ENTRY)
CheckSimple2(145,2,ent.field_type_name,"RECIP_ENTRY")
CheckSimple2(145,3,ent.fragment,0)
CheckSimple2(145,4,ent.in_fields,( "div",))
CheckSimple2(145,6,ent.dividend,6.5+4.3j)

# 146: add / entry (divide) check
ent = pygetdata.entry(pygetdata.DIVIDE_ENTRY, "new14", 0, ("in1", "in2"))
try:
  d.add(ent)
except:
  CheckOK2(146,1)

try:
  ent = d.entry("new14")
except:
  CheckOK2(146,2)
CheckSimple2(146,1,ent.field_type,pygetdata.DIVIDE_ENTRY)
CheckSimple2(146,2,ent.fragment,0)
CheckSimple2(146,3,ent.in_fields,( "in1", "in2"))

# 148: add / entry (divide) check
ent = pygetdata.entry(pygetdata.RECIP_ENTRY, "new16", 0, ("in3", 33.3))
try:
  d.add(ent)
except:
  CheckOK2(148,1)

try:
  ent = d.entry("new16")
except:
  CheckOK2(148,2)
CheckSimple2(148,1,ent.field_type,pygetdata.RECIP_ENTRY)
CheckSimple2(148,2,ent.fragment,0)
CheckSimple2(148,3,ent.in_fields,( "in3",))
CheckSimple2(148,4,ent.dividend,33.3)

# 149: add / entry (mult) check
ent = pygetdata.entry(pygetdata.DIVIDE_ENTRY, "mnew14", 0,
    {"in_field1": "in3", "in_field2": "in2"})
try:
  d.madd(ent,"data")
except:
  CheckOK2(149,1)

try:
  ent = d.entry("data/mnew14")
except:
  CheckOK2(149,2)
CheckSimple2(149,1,ent.field_type,pygetdata.DIVIDE_ENTRY)
CheckSimple2(149,2,ent.fragment,0)
CheckSimple2(149,3,ent.in_fields,( "in3", "in2"))

# 151: add / entry (mult) check
ent = pygetdata.entry(pygetdata.RECIP_ENTRY, "mnew16", 0,
    {"in_field": "in3", "dividend": "const"})
try:
  d.madd(ent,"data")
except:
  CheckOK2(151,1)

try:
  ent = d.entry("data/mnew16")
except:
  CheckOK2(151,2)
CheckSimple2(151,1,ent.field_type,pygetdata.RECIP_ENTRY)
CheckSimple2(151,2,ent.fragment,0)
CheckSimple2(151,3,ent.in_fields,( "in3",))
CheckSimple2(151,4,ent.dividend,"const")

# 155: fragment.rewrite check
try:
  f.rewrite()
except:
  CheckOK(155)

# 156: invalid dirfile check
try:
  m = pygetdata.dirfile(None);
except:
  CheckOK2(156,1)

try:
  n = m.nfragments()
except:
  CheckException2(156,2,pygetdata.BadDirfileError)

try:
  m.discard();
except:
  pass;

# 157: standards version
try:
  n = d.standards;
except:
  CheckOK2(157,1);
CheckSimple(157,n,9)

try:
  d.standards = 0;
except:
  CheckException2(157,2,pygetdata.BadVersionError)

# 158: gd_get_carray
try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK(158)

if (pygetdata.__numpy_supported__):
  CheckNumpy(158,n,numpy.arange(1,7))
else:
  CheckSimple(158,n,[1, 2, 3, 4, 5, 6])

# 159: gd_get_carray_slice (INT8)
try:
  n = d.get_carray("carray", pygetdata.INT, start=2, len=2)
except:
  CheckOK(159)

if (pygetdata.__numpy_supported__):
  CheckNumpy(159,n,numpy.arange(3,5))
else:
  CheckSimple(159,n,[3, 4])

# 162: gd_get_carray_slice (INT64)
try:
  n = d.get_carray("carray", pygetdata.LONG, start=2, len=2)
except:
  CheckOK(162)

if (pygetdata.__numpy_supported__):
  CheckNumpy(162,n,numpy.arange(3L,5L))
else:
  CheckSimple(162,n,[3L, 4L])

# 164: gd_get_carray_slice (FLOAT64)
try:
  n = d.get_carray("carray", pygetdata.FLOAT, start=2, len=2)
except:
  CheckOK(164)

if (pygetdata.__numpy_supported__):
  CheckNumpy(164,n,numpy.array([3.3, 4.4]))
else:
  CheckSimple(164,n,[3.3, 4.4])

# 166: gd_get_carray_slice (COMPLEX128)
try:
  n = d.get_carray("carray", pygetdata.COMPLEX, start=2, len=2)
except:
  CheckOK(166)

if (pygetdata.__numpy_supported__):
  CheckNumpy(166,n,numpy.array([3.3+0j, 4.4+0j]))
else:
  CheckSimple(166,n,[3.3+0j, 4.4+0j])

# 167: gd_carrays
try:
  n = d.carrays(pygetdata.INT)
except:
  CheckOK(167)

CheckSimple2(167,1,len(n),1)
if (pygetdata.__numpy_supported__):
  CheckSimple2(167,2,n[0][0],"carray")
  CheckNumpy2(167,2,n[0][1],numpy.arange(1,7))
else:
  CheckSimple(167,n,[("carray", [1,2,3,4,5,6])])

# 168: gd_put_carray
try:
  d.put_carray("carray", [9,8,7,6,5,4])
except:
  CheckOK2(168,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(168,2)

if (pygetdata.__numpy_supported__):
  CheckNumpy(168,n,numpy.arange(9,3,-1))
else:
  CheckSimple(168,n,[9,8,7,6,5,4])

# 169: gd_put_carray_slice (INT8)
try:
  d.put_carray("carray", [169,169], start=2)
except:
  CheckOK2(169,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(169,2)

if (pygetdata.__numpy_supported__):
  CheckNumpy(169,n,numpy.array([9,8,169,169,5,4]))
else:
  CheckSimple(169,n,[9,8,169,169,5,4])

# 172: gd_put_carray_slice (INT64)
try:
  d.put_carray("carray", [172L,172L], start=2)
except:
  CheckOK2(172,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(172,2)

if (pygetdata.__numpy_supported__):
  CheckNumpy(172,n,numpy.array([9,8,172,172,5,4]))
else:
  CheckSimple(172,n,[9,8,172,172,5,4])

# 174: gd_put_carray_slice (FLOAT64)
try:
  d.put_carray("carray", [174.,174.], start=2)
except:
  CheckOK2(174,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(174,2)

if (pygetdata.__numpy_supported__):
  CheckNumpy(174,n,numpy.array([9,8,174,174,5,4]))
else:
  CheckSimple(174,n,[9,8,174,174,5,4])

# 176: gd_put_carray_slice (COMPLEX128)
try:
  d.put_carray("carray", [176.+0j,176.+0j], start=2)
except:
  CheckOK2(176,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(176,2)

if (pygetdata.__numpy_supported__):
  CheckNumpy(176,n,numpy.array([9,8,176,176,5,4]))
else:
  CheckSimple(176,n,[9,8,176,176,5,4])

# 177: gd_carray_len
try:
  n = d.carray_len("carray")
except:
  CheckOK(177)

CheckSimple(177,n,6)

# 178: gd_entry (CARRAY)
try:
  ent = d.entry("carray")
except:
  CheckOK(178)
CheckSimple2(178,1,ent.field_type,pygetdata.CARRAY_ENTRY)
CheckSimple2(178,2,ent.field_type_name,"CARRAY_ENTRY")
CheckSimple2(178,3,ent.fragment,0)
CheckSimple2(178,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(178,5,ent.data_type_name,"FLOAT64")
CheckSimple2(178,6,ent.array_len,6)

# 179: gd_add_carray
ent = pygetdata.entry(pygetdata.CARRAY_ENTRY, "new17", 0, (pygetdata.FLOAT64,2))
try:
  d.add(ent)
except:
  CheckOK2(179,1)

try:
  ent = d.entry("new17")
except:
  CheckOK2(179,2)
CheckSimple2(179,1,ent.field_type,pygetdata.CARRAY_ENTRY)
CheckSimple2(179,2,ent.field_type_name,"CARRAY_ENTRY")
CheckSimple2(179,3,ent.fragment,0)
CheckSimple2(179,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(179,5,ent.data_type_name,"FLOAT64")
CheckSimple2(179,6,ent.array_len,2)

# 180: gd_madd_carray
ent = pygetdata.entry(pygetdata.CARRAY_ENTRY, "mnew17", 0,
    {"type": pygetdata.FLOAT64, "array_len": 2})
try:
  d.madd(ent,"data")
except:
  CheckOK2(180,1)

try:
  ent = d.entry("data/mnew17")
except:
  CheckOK2(180,2)
CheckSimple2(180,1,ent.field_type,pygetdata.CARRAY_ENTRY)
CheckSimple2(180,2,ent.field_type_name,"CARRAY_ENTRY")
CheckSimple2(180,3,ent.fragment,0)
CheckSimple2(180,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(180,5,ent.data_type_name,"FLOAT64")
CheckSimple2(180,6,ent.array_len,2)

# 183: gd_constants (int)
try:
  n = d.constants(pygetdata.INT)
except:
  CheckOK(183)
CheckSimple(183,n,[('const', 133), ('new11', 0)])

# 186: gd_constants (long)
try:
  n = d.constants(pygetdata.LONG)
except:
  CheckOK(186)
CheckSimple(186,n,[('const', 133L), ('new11', 0L)])

# 188: gd_constants (float)
try:
  n = d.constants(pygetdata.FLOAT)
except:
  CheckOK(188)
CheckSimple(188,n,[('const', 133.0), ('new11', 0.0)])

# 190: gd_constants (complex)
try:
  n = d.constants(pygetdata.COMPLEX)
except:
  CheckOK(190)
CheckSimple(190,n,[('const', 133.0), ('new11', 0.0)])

# 191: gd_constants (int)
try:
  n = d.mconstants("data", pygetdata.INT)
except:
  CheckOK(191)
CheckSimple(191,n,[('mconst', 3), ('mnew11', 0)])

# 194: gd_constants (long)
try:
  n = d.mconstants("data", pygetdata.LONG)
except:
  CheckOK(194)
CheckSimple(194,n,[('mconst', 3L), ('mnew11', 0L)])

# 196: gd_constants (float)
try:
  n = d.mconstants("data", pygetdata.FLOAT)
except:
  CheckOK(196)
CheckSimple(196,n,[('mconst', 3.3), ('mnew11', 0.)])

# 198: gd_constants (complex)
try:
  n = d.mconstants("data", pygetdata.COMPLEX)
except:
  CheckOK(198)
CheckSimple(198,n,[('mconst', 3.3+4.4j), ('mnew11', 0j)])

# 199: gd_strings
try:
  n = d.strings()
except:
  CheckOK(199)
CheckSimple(199,n,[('lorem', 'Lorem ipsum'), ('new12', ''), 
  ('string', 'Arthur Dent')])

# 200: gd_strings
try:
  n = d.mstrings("data")
except:
  CheckOK(200)
CheckSimple(200,n,[('mstr', 'This is a string constant.')])

# 203: gd_seek
try:
  n = d.seek("data", pygetdata.SEEK_SET, frame_num=35)
except:
  CheckOK2(203,0)

try:
  m = d.getdata("data", pygetdata.INT, first_frame=pygetdata.HERE, num_frames=1)
except:
  CheckOK2(203,1)
CheckSimple2(203,0,n,280)
CheckSimple2(203,1,len(m),8)
if (pygetdata.__numpy_supported__):
  CheckNumpy2(203,2,m,numpy.arange(17,25))
else:
  CheckSimple2(203,2,m,range(17,25))

# 204: gd_tell
try:
  n = d.tell("data")
except:
  CheckOK(204);
CheckSimple(204,n,288)

# 205: gd_hide check
try:
  d.hide('data')
except:
  CheckOK(205)

# 206: gd_hidden check
try:
  n = d.hidden('data')
except:
  CheckOK2(206, 1)
CheckSimple2(206, 1, n, 1)

try:
  n = d.hidden('lincom')
except:
  CheckOK2(206, 2)
CheckSimple2(206, 2, n, 0)

# 207: gd_unhide check
try:
  d.unhide('data')
except:
  CheckOK2(206, 1)

try:
  n = d.hidden('data')
except:
  CheckOK2(206, 2)
CheckSimple(206, n, 0)

# 208: gd_sync check
try:
  d.sync('data')
except:
  CheckOK(208)

# 209: gd_flush check
try:
  d.flush('data')
except:
  CheckOK(209)

# 210: gd_metaflush check
try:
  d.metaflush()
except:
  CheckOK(210)

# 211: gd_entry (WINDOW) check
try:
  ent = d.entry('window')
except:
  CheckOK(211)
CheckSimple2(211, 1, ent.field_type, pygetdata.WINDOW_ENTRY)
CheckSimple2(211, 2, ent.fragment, 0)
CheckSimple2(211, 3, ent.windop, pygetdata.WINDOP_LT)
CheckSimple2(212, 4, ent.in_fields, ( 'linterp', 'mult' ))
CheckSimple2(211, 5, ent.threshold, 4.1)

# 212: gd_add_window check
ent = pygetdata.entry(pygetdata.WINDOW_ENTRY, "new18", 0,
    ("in1", "in2", pygetdata.WINDOP_NE, 32))
try:
  d.add(ent)
except:
  CheckOK2(212, 1)

try:
  ent = d.entry('new18')
except:
  CheckOK2(212, 2)
CheckSimple2(212, 1, ent.field_type, pygetdata.WINDOW_ENTRY)
CheckSimple2(212, 2, ent.fragment, 0)
CheckSimple2(212, 3, ent.windop, pygetdata.WINDOP_NE)
CheckSimple2(212, 4, ent.in_fields, ( 'in1', 'in2' ))
CheckSimple2(212, 5, ent.threshold, 32)

# 214: gd_madd_window check
ent = pygetdata.entry(pygetdata.WINDOW_ENTRY, "mnew18", 0,
    ("in2", "in3", pygetdata.WINDOP_SET, 128))
try:
  d.madd(ent, "data")
except:
  CheckOK2(214, 1)

try:
  ent = d.entry('data/mnew18')
except:
  CheckOK2(214, 2)
CheckSimple2(214, 1, ent.field_type, pygetdata.WINDOW_ENTRY)
CheckSimple2(214, 2, ent.fragment, 0)
CheckSimple2(214, 3, ent.windop, pygetdata.WINDOP_SET)
CheckSimple2(214, 4, ent.in_fields, ( 'in2', 'in3' ))
CheckSimple2(214, 5, ent.threshold, 128)

# 217: gd_alter_window check
ent = pygetdata.entry(pygetdata.WINDOW_ENTRY, "new18", 0, { "threshold": 32e3,
  "in_field1": "in3", "in_field2": "in4", "windop": pygetdata.WINDOP_GE })
try:
  d.alter('new18', ent)
except:
  CheckOK2(217, 1)

try:
  ent = d.entry('new18')
except:
  CheckOK2(217, 2)
CheckSimple2(217, 1, ent.field_type, pygetdata.WINDOW_ENTRY)
CheckSimple2(217, 2, ent.fragment, 0)
CheckSimple2(217, 3, ent.windop, pygetdata.WINDOP_GE)
CheckSimple2(216, 4, ent.in_fields, ( 'in3', 'in4' ))
CheckSimple2(217, 6, ent.threshold, 32e3)

# 218: gd_alias_target check
try:
  str = d.alias_target('alias')
except:
  CheckOK(218)
CheckSimple(218, str, 'data')

# 219: gd_add_alias check
try:
  d.add_alias('new20', 'data', 0)
except:
  CheckOK2(219, 1)

try:
  str = d.alias_target('new20')
except:
  CheckOK2(219, 2)
CheckSimple(219, str, 'data')

# 220: gd_madd_alias check
try:
  d.madd_alias('data', 'mnew20', 'data')
except:
  CheckOK2(220, 1)

try:
  str = d.alias_target('data/mnew20')
except:
  CheckOK2(220, 2)
CheckSimple(220, str, 'data')

# 221: gd_naliases check
try:
  n = d.naliases('data')
except:
  CheckOK(221)
CheckSimple(221, n, 4)

# 222: gd_aliases check
try:
  n = d.aliases('data')
except:
  CheckOK(222)
CheckSimple(222, n, [ 'data', 'alias', 'data/mnew20', 'new20' ])

# 223: gd_include_affix check
try:
  d.include('format1', 0, prefix='A', suffix='Z',
      flags=pygetdata.CREAT | pygetdata.EXCL)
except:
  CheckOK(223)

# 224: gd_move_alias check
try:
  d.move_alias('new20', 1)
except:
  CheckOK2(224, 1)

try:
  n = d.fragment_index('Anew20Z')
except:
  CheckOK2(224, 2)
CheckSimple(224, n, 1)

# 225: gd_delete_alias check
try:
  d.delete_alias('Anew20Z', 0)
except:
  CheckOK2(225, 1)

try:
  n = d.fragment_index('Anew20Z')
except:
  CheckException2(255, 2, pygetdata.BadCodeError)

# 226: gd_fragment_affixes check
try:
  n = d.fragment(1).prefix
  m = d.fragment(1).suffix
except:
  CheckOK(226)
CheckSimple2(226, 1, n, "A")
CheckSimple2(226, 2, m, "Z")

# 227: gd_alter_affixes check
try:
  d.fragment(1).prefix = "B"
  d.fragment(1).suffix = ""
except:
  CheckOK2(227, 1)

try:
  n = d.fragment(1).prefix
  m = d.fragment(1).suffix
except:
  CheckOK2(227, 2)
CheckSimple2(227, 1, n, "B")
CheckSimple2(227, 2, m, "")

 











# ==========================================================================

d.discard()
del d
os.system("rm -rf dirfile")

if (ne > 0):
  print "ne =", ne
  print "__numpy_supported__ =", pygetdata.__numpy_supported__
  sys.exit(1)
