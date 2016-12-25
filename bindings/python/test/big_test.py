# Copyright (C) 2009-2015 D. V. Wiebe
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
import numpy

# These two functions abstractify differences between Python2 and 3.
def L(n):
  if sys.version[:1] == '3':
    return n
  return long(n)

def B(s):
  if sys.version[:1] == '3':
    return bytes(s, "ASCII")
  return s




def CheckOK(t):
  global ne
  ne+=1
  print ("e[", t, "] =", sys.exc_info()[0], sys.exc_value)

def CheckOK2(t,m):
  global ne
  ne+=1
  print ("e[", t, ",", m, "] =", sys.exc_info()[0], sys.exc_value)

def CheckException(t,g):
  global ne
  if (sys.exc_info()[0] != g):
    ne+=1
    print ("e[", t, "] =", sys.exc_info()[0], "expected", g)

def CheckException2(t,m,g):
  global ne
  if (sys.exc_info()[0] != g):
    ne+=1
    print ("e[", t, ",", m, "] =", sys.exc_info()[0], "expected", g)

def CheckNumpy(t,v,g):
  global ne
  if (numpy.any(v - g)):
    ne+=1
    print ("a[", t, "] =", v, "expected", g)

def CheckNumpy2(t,m,v,g):
  global ne
  if (numpy.any(v - g)):
    ne+=1
    print ("a[", t, ",", m, "] =", v, "expected", g)

def CheckSimple(t,v,g):
  global ne
  if (v != g):
    ne+=1
    print ("n[", t, "] =", v, "expected", g)

def CheckSimple2(t,m,v,g):
  global ne
  if (v != g):
    ne+=1
    print ("n[", t, ",", m, "] =", v, "expected", g)

def CheckEOS(t,v,g):
  global ne
  if (re.search(g + "$", v) == None):
    ne+=1
    print ("n[", t, "] =", v, "expected", g)

# create the dirfile first
data=array.array("B",range(1,81))
os.system("rm -rf dirfile")
os.mkdir("dirfile")
file=open("dirfile/data", 'wb')
data.tofile(file)
file.close()

ne = 0

fields = [ B("bit"), B("div"), B("data"), B("mult"), B("sbit"), B("INDEX"),
        B("alias"), B("const"), B("indir"), B("mplex"), B("phase"), B("recip"),
        B("carray"), B("lincom"), B("sarray"), B("sindir"), B("string"),
        B("window"), B("linterp"), B("polynom"), ]

nfields = 20
file=open("dirfile/format", 'w')
file.write(
    "/ENDIAN little\n"
    "data RAW INT8 8\n"
    "lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const\n"
    "/META data mstr STRING \"This is a string constant.\"\n"
    "/META data mconst CONST COMPLEX128 3.3;4.4\n"
    "/META data mcarray CARRAY FLOAT64 1.9 2.8 3.7 4.6 5.5\n"
    "/META data mlut LINTERP DATA ./lut\n"
    "const CONST FLOAT64 5.5\n"
    "carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6\n"
    "linterp LINTERP data ./lut\n"
    "polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const\n"
    "bit BIT data 3 4\n"
    "sbit SBIT data 5 6\n"
    "mplex MPLEX data sbit 1 10\n"
    "mult MULTIPLY data sbit\n"
    "div DIVIDE mult bit\n"
    "recip RECIP div 6.5;4.3\n"
    "phase PHASE data 11\n"
    "window WINDOW linterp mult LT 4.1\n"
    "/ALIAS alias data\n"
    "string STRING \"Zaphod Beeblebrox\"\n"
    "sarray SARRAY one two three four five six seven\n"
    "data/msarray SARRAY eight nine ten eleven twelve\n"
    "indir INDIR data carray\n"
    "sindir SINDIR data sarray\n"
    )
file.close()

file=open("dirfile/form2", 'w')
file.write("const2 CONST INT8 -19\n")
file.close()

# 1: error check
try:
  d = pygetdata.dirfile("x", pygetdata.RDONLY)
except:
  CheckException(1, pygetdata.IOError)

# 2: dirfile check
try:
  d = pygetdata.dirfile("dirfile", pygetdata.RDWR)
except:
  CheckOK(2)

# 3: getdata (int) check
try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1)
except:
  CheckOK(3)
CheckSimple(3,len(n),8)
CheckNumpy(3,n,numpy.arange(41,49))

# 6: getdata (long) check
try:
  n = d.getdata("data", pygetdata.LONG, first_frame=5, num_frames=1)
except:
  CheckOK(6)
CheckSimple(6,len(n),8)
CheckNumpy(6,n,numpy.arange(L(41),L(49)))

# 8: getdata (float) check
try:
  n = d.getdata("data", pygetdata.FLOAT, first_frame=5, num_frames=1)
except:
  CheckOK(8)
CheckSimple(8,len(n),8)
CheckNumpy(8,n,numpy.arange(41.,49.))

# 10: getdata (complex) check
try:
  n = d.getdata("data", pygetdata.COMPLEX, first_frame=5, num_frames=1)
except:
  CheckOK(10)
CheckSimple(10,len(n),8)
CheckNumpy(10,n,numpy.arange(41,49,dtype=numpy.complex128))

# 11: getdata (GD_NULL) check
try:
  n = d.getdata("data", pygetdata.NULL, first_frame=5, num_frames=1)
except:
  CheckOK(11)
CheckSimple(11, n, 8)

# 12: constant (int) check
try:
  n = d.get_constant("const", pygetdata.INT)
except:
  CheckOK(12)
CheckSimple(12,n,5)

# 15: constant (long) check
try:
  n = d.get_constant("const", pygetdata.LONG)
except:
  CheckOK(15)
CheckSimple(15,n,L(5))

# 16: constant (float) check
try:
  n = d.get_constant("const")
except:
  CheckOK(16)
CheckSimple(16,n,5.5)

# 17: constant (float) check
try:
  n = d.get_constant("const", pygetdata.FLOAT)
except:
  CheckOK(17)
CheckSimple(17,n,5.5)

# 19: constant (float) check
try:
  n = d.get_constant("const", pygetdata.COMPLEX)
except:
  CheckOK(19)
CheckSimple(19,n,5.5+0j)

# 20: constant (GD_NULL) check
try:
  n = d.get_constant("const", pygetdata.NULL)
except:
  CheckOK(20)
CheckSimple(20,n,None)

# 23: nfields check
try:
  n = d.nfields()
except:
  CheckOK(23)
CheckSimple(23,n,nfields)

# 25: field_list check
try:
  n = d.field_list()
except:
  CheckOK(25)
CheckSimple(25,n,fields)

# 26: nmfields check
try:
  n = d.nmfields("data")
except:
  CheckOK(26)
CheckSimple(26,n,5)

# 27: mfield_list check
try:
  n = d.mfield_list("data")
except:
  CheckOK(27)
CheckSimple(27,n,[B("mstr"), B("mconst"), B("mcarray"), B("mlut"),
    B("msarray")])

# 28: nframes check
try:
  n = d.nframes
except:
  CheckOK(28)
CheckSimple(28,n,10)

# 29: spf check
try:
  n = d.spf("data")
except:
  CheckOK(29)
CheckSimple(29,n,8)

# 30: putdata (int) check
p = [ 13, 14, 15, 16 ]
try:
  n = d.putdata("data", p, pygetdata.INT, first_frame=5, first_sample=1)
except:
  CheckOK2(30,1)
CheckSimple2(30,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK(30,2)
CheckSimple2(30,2,n,[41, 13, 14, 15, 16, 46, 47, 48])

# 32: putdata (numpy) check
p = numpy.array([ 73, 74, 75, 76 ])
try:
  n = d.putdata("data", p, first_frame=5, first_sample=1)
except:
  CheckOK2(32,1)
CheckSimple2(32,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1)
except:
  CheckOK(32,2)
CheckNumpy2(32,2,n,numpy.array([41, 73, 74, 75, 76, 46, 47, 48]))

# 33: putdata (long) check
p = [ L(23), L(24), L(25), L(26) ]
try:
  n = d.putdata("data", p, pygetdata.LONG, first_frame=5, first_sample=1)
except:
  CheckOK2(33,1)
CheckSimple2(33,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK(33,2)
CheckSimple2(33,2,n,[41, 23, 24, 25, 26, 46, 47, 48])

# 35: putdata (float) check
p = [ 33., 34., 35., 36. ]
try:
  n = d.putdata("data", p, pygetdata.FLOAT, first_frame=5, first_sample=1)
except:
  CheckOK2(35,1)
CheckSimple2(35,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK(35,2)
CheckSimple2(35,2,n,[41, 33, 34, 35, 36, 46, 47, 48])

# 37: putdata (complex) check
p = [ 124.+1j, 125.+2j, 126.+3j, 127.+4j ]
try:
  n = d.putdata("data", p, pygetdata.COMPLEX, first_frame=5, first_sample=1)
except:
  CheckOK2(37,1)
CheckSimple2(37,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=5, num_frames=1, as_list=1)
except:
  CheckOK2(37,2)
CheckSimple2(37,2,n,[41, 124, 125, 126, 127, 46, 47, 48])

# 38: error_string check
try:
  n = d.getdata("x", pygetdata.INT, first_frame=5, num_frames=1)
except:
  CheckException(38,pygetdata.BadCodeError)
  CheckSimple(38,d.error_string,B("Field not found: x"))

# 40: entry (raw) check
try:
  ent = d.entry("data")
except:
  CheckOK(40)
CheckSimple2(40,1,ent.field_type,pygetdata.RAW_ENTRY)
CheckSimple2(40,2,ent.field_type_name,"RAW_ENTRY")
CheckSimple2(40,3,ent.fragment,0)
CheckSimple2(40,4,ent.data_type,pygetdata.INT8)
CheckSimple2(40,5,ent.data_type_name,"INT8")
CheckSimple2(40,6,ent.spf,8)

# 42: entry (lincom) check
try:
  ent = d.entry("lincom")
except:
  CheckOK(42)
CheckSimple2(42,1,ent.field_type,pygetdata.LINCOM_ENTRY)
CheckSimple2(42,2,ent.field_type_name,"LINCOM_ENTRY")
CheckSimple2(42,3,ent.fragment,0)
CheckSimple2(42,4,ent.n_fields,3)
CheckSimple2(42,5,ent.in_fields,(B("data"), B("INDEX"), B("linterp")))
CheckSimple2(42,6,ent.m,(1.1, 2.2, B("const")))
CheckSimple2(42,7,ent.b,(2.2, 3.3 + 4.4j, B("const")))

# 44: entry (polynom) check
try:
  ent = d.entry("polynom")
except:
  CheckOK(44)
CheckSimple2(44,1,ent.field_type,pygetdata.POLYNOM_ENTRY)
CheckSimple2(44,2,ent.field_type_name,"POLYNOM_ENTRY")
CheckSimple2(44,3,ent.fragment,0)
CheckSimple2(44,4,ent.poly_ord,5)
CheckSimple2(44,5,ent.in_fields,( B("data"), ))
CheckSimple2(44,6,ent.a,(1.1, 2.2, 2.2, 3.3 + 4.4j, B("const"), B("const")))

# 45: entry (linterp) check
try:
  ent = d.entry("linterp")
except:
  CheckOK(45)
CheckSimple2(45,1,ent.field_type,pygetdata.LINTERP_ENTRY)
CheckSimple2(45,2,ent.field_type_name,"LINTERP_ENTRY")
CheckSimple2(45,3,ent.fragment,0)
CheckSimple2(45,4,ent.in_fields,( B("data"), ))
CheckSimple2(45,5,ent.table,"./lut")

# 46: entry (bit) check
try:
  ent = d.entry("bit")
except:
  CheckOK(46)
CheckSimple2(46,1,ent.field_type,pygetdata.BIT_ENTRY)
CheckSimple2(46,2,ent.field_type_name,"BIT_ENTRY")
CheckSimple2(46,3,ent.fragment,0)
CheckSimple2(46,4,ent.in_fields,( B("data"), ))
CheckSimple2(46,5,ent.numbits,4)
CheckSimple2(46,6,ent.bitnum,3)

# 47: entry (sbit) check
try:
  ent = d.entry("sbit")
except:
  CheckOK(47)
CheckSimple2(47,1,ent.field_type,pygetdata.SBIT_ENTRY)
CheckSimple2(47,2,ent.field_type_name,"SBIT_ENTRY")
CheckSimple2(47,3,ent.fragment,0)
CheckSimple2(47,4,ent.in_fields,( B("data"), ))
CheckSimple2(47,5,ent.numbits,6)
CheckSimple2(47,6,ent.bitnum,5)

# 48: entry (mult) check
try:
  ent = d.entry("mult")
except:
  CheckOK(48)
CheckSimple2(48,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(48,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(48,3,ent.fragment,0)
CheckSimple2(48,4,ent.in_fields,( B("data"), B("sbit")))

# 49: entry (phase) check
try:
  ent = d.entry("phase")
except:
  CheckOK(49)
CheckSimple2(49,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(49,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(49,3,ent.fragment,0)
CheckSimple2(49,4,ent.in_fields,( B("data"), ))
CheckSimple2(49,5,ent.shift,11)

# 50: entry (const) check
try:
  ent = d.entry("const")
except:
  CheckOK(50)
CheckSimple2(50,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(50,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(50,3,ent.fragment,0)
CheckSimple2(50,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(50,5,ent.data_type_name,"FLOAT64")

# 51: entry (string) check
try:
  ent = d.entry("string")
except:
  CheckOK(51)
CheckSimple2(51,1,ent.field_type,pygetdata.STRING_ENTRY)
CheckSimple2(51,2,ent.field_type_name,"STRING_ENTRY")
CheckSimple2(51,3,ent.fragment,0)

# 52: fragment_index check
try:
  n = d.fragment_index("data")
except:
  CheckOK(52)
CheckSimple(52,n,0)

# 53: add / entry (raw) check
ent = pygetdata.entry(pygetdata.RAW_ENTRY, "new1", 0, (pygetdata.FLOAT64, 3))
try:
  d.add(ent)
except:
  CheckOK2(53,1)

try:
  ent = d.entry("new1")
except:
  CheckOK(53,2)
CheckSimple2(53,1,ent.field_type,pygetdata.RAW_ENTRY)
CheckSimple2(53,2,ent.field_type_name,"RAW_ENTRY")
CheckSimple2(53,3,ent.fragment,0)
CheckSimple2(53,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(53,5,ent.data_type_name,"FLOAT64")
CheckSimple2(53,6,ent.spf,3)

# 54: add / entry (lincom) check
ent = pygetdata.entry(pygetdata.LINCOM_ENTRY, "new2", 0,
    (("in1", "in2"), (9.9, 7.7), (8.8, 6.6)))
try:
  d.add(ent)
except:
  CheckOK2(54,1)

try:
  ent = d.entry("new2")
except:
  CheckOK(54,2)
CheckSimple2(54,1,ent.field_type,pygetdata.LINCOM_ENTRY)
CheckSimple2(54,2,ent.field_type_name,"LINCOM_ENTRY")
CheckSimple2(54,3,ent.fragment,0)
CheckSimple2(54,4,ent.n_fields,2)
CheckSimple2(54,5,ent.in_fields,( B("in1"), B("in2") ))
CheckSimple2(54,6,ent.m,(9.9, 7.7))
CheckSimple2(54,7,ent.b,(8.8, 6.6))

# 56: add / entry (polynom) check
ent = pygetdata.entry(pygetdata.POLYNOM_ENTRY, "new4", 0,
    (B("in1"), (3.9, 4.8, 5.7, 6.6)))
try:
  d.add(ent)
except:
  CheckOK2(56,1)

try:
  ent = d.entry("new4")
except:
  CheckOK2(56,2)
CheckSimple2(56,1,ent.field_type,pygetdata.POLYNOM_ENTRY)
CheckSimple2(56,2,ent.field_type_name,"POLYNOM_ENTRY")
CheckSimple2(56,3,ent.fragment,0)
CheckSimple2(56,4,ent.poly_ord,3)
CheckSimple2(56,5,ent.in_fields,( B("in1"), ))
CheckSimple2(56,6,ent.a,(3.9, 4.8, 5.7, 6.6))

# 58: add / entry (linterp) check
ent = pygetdata.entry(pygetdata.LINTERP_ENTRY, "new6", 0,
    (B("in"), "./some/table"))
try:
  d.add(ent)
except:
  CheckOK2(58,1)

try:
  ent = d.entry("new6")
except:
  CheckOK2(58,2)
CheckSimple2(58,1,ent.field_type,pygetdata.LINTERP_ENTRY)
CheckSimple2(58,2,ent.field_type_name,"LINTERP_ENTRY")
CheckSimple2(58,3,ent.fragment,0)
CheckSimple2(58,4,ent.in_fields,( B("in"), ))
CheckSimple2(58,5,ent.table,"./some/table")

# 59: add / entry (bit) check
ent = pygetdata.entry(pygetdata.BIT_ENTRY, "new7", 0, ("in", 13, 12))
try:
  d.add(ent)
except:
  CheckOK2(59,1)

try:
  ent = d.entry("new7")
except:
  CheckOK2(59,1)
CheckSimple2(59,1,ent.field_type,pygetdata.BIT_ENTRY)
CheckSimple2(59,2,ent.field_type_name,"BIT_ENTRY")
CheckSimple2(59,3,ent.fragment,0)
CheckSimple2(59,4,ent.in_fields,( B("in"), ))
CheckSimple2(59,5,ent.numbits,12)
CheckSimple2(59,6,ent.bitnum,13)

# 60: add / entry (sbit) check
ent = pygetdata.entry(pygetdata.SBIT_ENTRY, "new8", 0, ("in2", 14, 15))
try:
  d.add(ent)
except:
  CheckOK2(60,1)

try:
  ent = d.entry("new8")
except:
  CheckOK2(60,2)
CheckSimple2(60,1,ent.field_type,pygetdata.SBIT_ENTRY)
CheckSimple2(60,2,ent.field_type_name,"SBIT_ENTRY")
CheckSimple2(60,3,ent.fragment,0)
CheckSimple2(60,4,ent.in_fields,( B("in2"), ))
CheckSimple2(60,5,ent.bitnum,14)
CheckSimple2(60,6,ent.numbits,15)

# 61: add / entry (mult) check
ent = pygetdata.entry(pygetdata.MULTIPLY_ENTRY, "new9", 0, ("in1", "in2"))
try:
  d.add(ent)
except:
  CheckOK2(61,1)

try:
  ent = d.entry("new9")
except:
  CheckOK2(61,2)
CheckSimple2(61,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(61,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(61,3,ent.fragment,0)
CheckSimple2(61,4,ent.in_fields,( B("in1"), B("in2")))

# 62: add / entry (phase) check
ent = pygetdata.entry(pygetdata.PHASE_ENTRY, "new10", 0, ("in1", 22))
try:
  d.add(ent)
except:
  CheckOK2(62,1)

try:
  ent = d.entry("new10")
except:
  CheckOK2(62,2)
CheckSimple2(62,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(62,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(62,3,ent.fragment,0)
CheckSimple2(62,4,ent.in_fields,( B("in1"), ))
CheckSimple2(62,5,ent.shift,22)

# 63: add / entry (const) check
ent = pygetdata.entry(pygetdata.CONST_ENTRY, "new11", 0, (pygetdata.FLOAT64,))
try:
  d.add(ent)
except:
  CheckOK2(63,1)

try:
  ent = d.entry("new11")
except:
  CheckOK2(63,2)
CheckSimple2(63,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(63,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(63,3,ent.fragment,0)
CheckSimple2(63,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(63,5,ent.data_type_name,"FLOAT64")

# 64: fragment check
try:
  f = d.fragment(0)
except:
  CheckOK(64)
CheckEOS(64,f.name,"dirfile/format")

# 65: nfragments check
try:
  n  = d.nfragments
except:
  CheckOK(65)
CheckSimple(65,n,1)

# 66: include check
try:
  n = d.include("form2")
except:
  CheckOK2(66,1)
CheckSimple2(66,1,n,1)

try:
  n = d.get_constant("const2", pygetdata.INT)
except:
  CheckOK2(66,2)
CheckSimple2(66,2,n,-19)

# 67: nfields_by_type check
try:
  n = d.nfields(pygetdata.LINCOM_ENTRY)
except:
  CheckOK(67)
CheckSimple(67,n,2)

# 68: field_list_by_type check
try:
  n = d.field_list(pygetdata.LINCOM_ENTRY)
except:
  CheckOK(68)
CheckSimple(68,n,[B("new2"), B("lincom")])

# 69: nvectors check
try:
  n = d.nvectors()
except:
  CheckOK(69)
CheckSimple(69,n,23)

# 70: vector_list check
try:
  n = d.vector_list()
except:
  CheckOK(70)
CheckSimple(70,n,[ B('bit'), B('div'), B('data'), B('mult'), B('new1'),
    B('new2'), B('new4'), B('new6'), B('new7'), B('new8'), B('new9'), B('sbit'),
    B('INDEX'), B('alias'), B('indir'), B('mplex'), B('new10'), B('phase'),
    B('recip'), B('lincom'), B('window'), B('linterp'), B('polynom') ])

# 71: add / entry (lincom) check
ent = pygetdata.entry(pygetdata.LINCOM_ENTRY, "mnew1", 0,
    {"in_fields": ("in1", "in2"), "m": (9.9, 7.7), "b": (8.8, 6.6)})
try:
  d.madd(ent, "data")
except:
  CheckOK2(71,1)

try:
  ent = d.entry("data/mnew1")
except:
  CheckOK(71,2)
CheckSimple2(71,1,ent.field_type,pygetdata.LINCOM_ENTRY)
CheckSimple2(71,2,ent.field_type_name,"LINCOM_ENTRY")
CheckSimple2(71,3,ent.fragment,0)
CheckSimple2(71,4,ent.n_fields,2)
CheckSimple2(71,5,ent.in_fields,( B("in1"), B("in2") ))
CheckSimple2(71,6,ent.m,(9.9, 7.7))
CheckSimple2(71,7,ent.b,(8.8, 6.6))

# 73: add / entry (polynom) check
ent = pygetdata.entry(pygetdata.POLYNOM_ENTRY, "mnew3", 0,
    {"in_field": "in1", "a": (3.9, 4.8, 5.7, 6.6)})
try:
  d.madd(ent, "data")
except:
  CheckOK2(73,1)

try:
  ent = d.entry("data/mnew3")
except:
  CheckOK2(73,2)
CheckSimple2(73,1,ent.field_type,pygetdata.POLYNOM_ENTRY)
CheckSimple2(73,2,ent.field_type_name,"POLYNOM_ENTRY")
CheckSimple2(73,3,ent.fragment,0)
CheckSimple2(73,4,ent.poly_ord,3)
CheckSimple2(73,5,ent.in_fields,( B("in1"), ))
CheckSimple2(73,6,ent.a,(3.9, 4.8, 5.7, 6.6))

# 75: add / entry (linterp) check
ent = pygetdata.entry(pygetdata.LINTERP_ENTRY, "mnew6", 0,
    {"in_field": "in", "table": "./more/table"})
try:
  d.madd(ent, "data")
except:
  CheckOK2(75,1)

try:
  ent = d.entry("data/mnew6")
except:
  CheckOK2(75,2)
CheckSimple2(75,1,ent.field_type,pygetdata.LINTERP_ENTRY)
CheckSimple2(75,2,ent.field_type_name,"LINTERP_ENTRY")
CheckSimple2(75,3,ent.fragment,0)
CheckSimple2(75,4,ent.in_fields,( B("in"), ))
CheckSimple2(75,5,ent.table,"./more/table")

# 76: add / entry (bit) check
ent = pygetdata.entry(pygetdata.BIT_ENTRY, "mnew7", 0,
    {"in_field": "in1", "bitnum": 3, "numbits": 2})
try:
  d.madd(ent,"data")
except:
  CheckOK2(76,1)

try:
  ent = d.entry("data/mnew7")
except:
  CheckOK2(76,1)
CheckSimple2(76,1,ent.field_type,pygetdata.BIT_ENTRY)
CheckSimple2(76,2,ent.field_type_name,"BIT_ENTRY")
CheckSimple2(76,3,ent.fragment,0)
CheckSimple2(76,4,ent.in_fields,( B("in1"), ))
CheckSimple2(76,5,ent.numbits,2)
CheckSimple2(76,6,ent.bitnum,3)

# 77: add / entry (sbit) check
ent = pygetdata.entry(pygetdata.SBIT_ENTRY, "mnew8", 0,
    {"in_field": "in2", "bitnum": 4, "numbits": 5})
try:
  d.madd(ent,"data")
except:
  CheckOK2(77,1)

try:
  ent = d.entry("data/mnew8")
except:
  CheckOK2(77,2)
CheckSimple2(77,1,ent.field_type,pygetdata.SBIT_ENTRY)
CheckSimple2(77,2,ent.field_type_name,"SBIT_ENTRY")
CheckSimple2(77,3,ent.fragment,0)
CheckSimple2(77,4,ent.in_fields,( B("in2"), ))
CheckSimple2(77,5,ent.numbits,5)
CheckSimple2(77,6,ent.bitnum,4)

# 78: add / entry (mult) check
ent = pygetdata.entry(pygetdata.MULTIPLY_ENTRY, "mnew9", 0,
    {"in_field1": "in3", "in_field2": "in2"})
try:
  d.madd(ent,"data")
except:
  CheckOK2(78,1)

try:
  ent = d.entry("data/mnew9")
except:
  CheckOK2(78,2)
CheckSimple2(78,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(78,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(78,3,ent.fragment,0)
CheckSimple2(78,4,ent.in_fields,( B("in3"), B("in2")))

# 79: add / entry (phase) check
ent = pygetdata.entry(pygetdata.PHASE_ENTRY, "mnew10", 0,
    {"in_field": "in3", "shift": 44})
try:
  d.madd(ent,"data")
except:
  CheckOK2(79,1)

try:
  ent = d.entry("data/mnew10")
except:
  CheckOK2(79,2)
CheckSimple2(79,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(79,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(79,3,ent.fragment,0)
CheckSimple2(79,4,ent.in_fields,( B("in3"), ))
CheckSimple2(79,5,ent.shift,44)

# 80: add / entry (const) check
ent = pygetdata.entry(pygetdata.CONST_ENTRY, "mnew11", 0,
    {"type": pygetdata.FLOAT64})
try:
  d.madd(ent,"data")
except:
  CheckOK2(80,1)

try:
  ent = d.entry("data/mnew11")
except:
  CheckOK2(80,2)
CheckSimple2(80,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(80,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(80,3,ent.fragment,0)
CheckSimple2(80,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(80,5,ent.data_type_name,"FLOAT64")

#81: string check
try:
  n = d.get_string("string")
except:
  CheckOK(81)
CheckSimple(81,n,B("Zaphod Beeblebrox"))

# 82: entry (string) check
ent = pygetdata.entry(pygetdata.STRING_ENTRY, "new12", 0)
try:
  d.add(ent)
except:
  CheckOK2(82,1)

try:
  ent = d.entry("new12")
except:
  CheckOK(82)
CheckSimple2(82,1,ent.field_type,pygetdata.STRING_ENTRY)
CheckSimple2(82,2,ent.field_type_name,"STRING_ENTRY")
CheckSimple2(82,3,ent.fragment,0)

# 84: add_spec check
try:
  d.add_spec("lorem STRING \"Lorem ipsum\"", 0)
except:
  CheckOK2(84,1)

try:
  n = d.get_string("lorem")
except:
  CheckOK2(84,2)
CheckSimple(84,n,B("Lorem ipsum"))

# 85: madd_spec check
try:
  d.madd_spec("ipsum STRING \"dolor sit amet.\"", "lorem")
except:
  CheckOK2(85,1)

try:
  n = d.get_string("lorem/ipsum")
except:
  CheckOK2(85,2)
CheckSimple(85,n,B("dolor sit amet."))

# 86: put_constant / int check
try:
  d.put_constant("const", 86)
except:
  CheckOK2(86,1)

try:
  n = d.get_constant("const",pygetdata.INT)
except:
  CheckOK2(86,2)
CheckSimple(86,n,86)

# 88: put_constant / int check
try:
  d.put_constant("const", L(128))
except:
  CheckOK2(88,1)

try:
  n = d.get_constant("const",pygetdata.ULONG)
except:
  CheckOK2(88,2)
CheckSimple(88,n,L(128))

# 89: put_constant / int check
try:
  d.put_constant("const", L(89))
except:
  CheckOK2(89,1)

try:
  n = d.get_constant("const",pygetdata.LONG)
except:
  CheckOK2(89,2)
CheckSimple(89,n,L(89))

# 91: put_constant / float check
try:
  d.put_constant("const", 91.)
except:
  CheckOK2(91,1)

try:
  n = d.get_constant("const",pygetdata.FLOAT)
except:
  CheckOK2(91,2)
CheckSimple(91,n,91.)

# 93: put_constant / complex check
try:
  d.put_constant("const", 93.+0j)
except:
  CheckOK2(93,1)

try:
  n = d.get_constant("const",pygetdata.COMPLEX)
except:
  CheckOK2(93,2)
CheckSimple(93,n,93.+0j)

# 94: put_string
try:
  d.put_string("string", "Arthur Dent")
except:
  CheckOK2(94,1)

try:
  n = d.get_string("string")
except:
  CheckOK2(94,2)
CheckSimple(94,n,B("Arthur Dent"))

# 95: nmfields_by_type check
try:
  n = d.nmfields("data",pygetdata.LINCOM_ENTRY)
except:
  CheckOK(95)
CheckSimple(95,n,1)

# 96: mfield_list_by_type check
try:
  n = d.mfield_list("data",pygetdata.LINCOM_ENTRY)
except:
  CheckOK(96)
CheckSimple(96,n,[B("mnew1")])

# 97: nmvectors check
try:
  n = d.nmvectors("data")
except:
  CheckOK(97)
CheckSimple(97,n,8)

# 98: mvector_list check
try:
  n = d.mvector_list("data")
except:
  CheckOK(98)
CheckSimple(98,n,[B('mlut'), B('mnew1'), B('mnew3'), B('mnew6'), B('mnew7'),
    B('mnew8'), B('mnew9'), B('mnew10')])

# 99: alter / raw check
ent = pygetdata.entry(pygetdata.RAW_ENTRY, "new1", 0,
    {"type": pygetdata.FLOAT32, "spf": 4})
try:
  n = d.alter("new1", ent)
except:
  CheckOK2(99,1)

try:
  ent = d.entry("new1")
except:
  CheckOK(99,2)
CheckSimple2(99,1,ent.field_type,pygetdata.RAW_ENTRY)
CheckSimple2(99,2,ent.field_type_name,"RAW_ENTRY")
CheckSimple2(99,3,ent.fragment,0)
CheckSimple2(99,4,ent.data_type,pygetdata.FLOAT32)
CheckSimple2(99,5,ent.data_type_name,"FLOAT32")
CheckSimple2(99,6,ent.spf,4)

# 110: encoding check
try:
  f = d.fragment(0)
  CheckSimple(110,f.encoding,pygetdata.UNENCODED)
except:
  CheckOK(110)

# 111: endianness check
try:
  CheckSimple(111,f.endianness,pygetdata.LITTLE_ENDIAN |
		  pygetdata.NOT_ARM_ENDIAN)
except:
  CheckOK(111)

# 112: dirfilename check
try:
  CheckEOS(112,d.name,"dirfile")
except:
  CheckOK(112)

# 113: parent_fragment check
try:
  f = d.fragment(1)
  CheckSimple(113,f.parent,0)
except:
  CheckOK(113)

# 114: dirfile_protect check
try:
  f.protection = pygetdata.PROTECT_DATA
except:
  CheckOK(114)
  
# 115: protection check
try:
  f = d.fragment(1)
  CheckSimple(115,f.protection,pygetdata.PROTECT_DATA)
except:
  CheckOK(115)

# 116: raw_filename check
try:
  n = d.raw_filename("data")
except:
  CheckOK(116)
CheckEOS(116,n,"dirfile/data")

# 117: reference check
try:
  d.reference = "new1"
except:
  CheckOK2(117,1)

try:
  CheckSimple(117,d.reference,B("new1"))
except:
  CheckOK2(117,2)

# 118: eof check
try:
  n = d.eof("lincom")
except:
  CheckOK(118)
CheckSimple(118,n,80)

# 119: alter_encoding check
try:
  f = d.fragment(1)
  f.alter_encoding(pygetdata.SLIM_ENCODED,0)
except:
  CheckOK(119)

# 120: alter_endianness check
try:
  f.alter_endianness(pygetdata.BIG_ENDIAN,0)
except:
  CheckOK(120)

# 121: alter_spec check
try:
  d.alter_spec("new10 PHASE in5 3", 0)
except:
  CheckOK2(121,1)

try:
  ent = d.entry("new10")
except:
  CheckOK2(121,2)
CheckSimple2(121,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(121,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(121,3,ent.fragment,0)
CheckSimple2(121,4,ent.in_fields,( B("in5"), ))
CheckSimple2(121,5,ent.shift,3)

# 122: delete check
try:
  d.delete("new10",0)
except:
  CheckOK2(122,1)

try:
  ent = d.entry("new10")
except:
  CheckException2(122,2,pygetdata.BadCodeError)

# 123: malter_spec check
try:
  d.malter_spec("mnew10 PHASE in4 11", "data", 0)
except:
  CheckOK2(123,1)

try:
  ent = d.entry("data/mnew10")
except:
  CheckOK2(123,2)
CheckSimple2(123,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(123,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(123,3,ent.fragment,0)
CheckSimple2(123,4,ent.in_fields,( B("in4"), ))
CheckSimple2(123,5,ent.shift,11)

# 124: move check
try:
  d.move("new9", 1, 0)
except:
  CheckOK2(124,1)

try:
  ent = d.entry("new9")
except:
  CheckOK2(124,2)
CheckSimple2(124,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(124,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(124,3,ent.fragment,1)
CheckSimple2(124,4,ent.in_fields,( B("in1"), B("in2")))

# 125: rename check
try:
  d.rename("new9", "newer", 0)
except:
  CheckOK2(125,1)

try:
  ent = d.entry("new9")
except:
  CheckException2(125,2,pygetdata.BadCodeError)

try:
  ent = d.entry("newer")
except:
  CheckOK2(125,3)
CheckSimple2(125,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(125,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(125,3,ent.fragment,1)
CheckSimple2(125,4,ent.in_fields,( B("in1"), B("in2")))

# 126: uninclude check
try:
  d.uninclude(1,0)
except:
  CheckOK2(126,1)

try:
  ent = d.entry("newer")
except:
  CheckException2(126,2,pygetdata.BadCodeError)

# 127: frameoffset check
try:
  f = d.fragment(0)
  CheckSimple(127,f.frameoffset,0)
except:
  CheckOK(127)

# 128: alter_frameoffset check
try:
  f.alter_frameoffset(33, 0)
  CheckSimple(128,f.frameoffset,33)
except:
  CheckOK(128)

# 129: native_type check
try:
  n = d.native_type("data")
except:
  CheckOK(129)
CheckSimple(129,n,pygetdata.INT8)

# 131: validate check
try:
  d.validate("new7")
except:
  CheckException(131,pygetdata.BadCodeError)

# 133: framenum check
try:
  n = d.framenum("data", 33.3, start=6)
except:
  CheckOK(133)
CheckSimple(133,n,37.0375)

# 138: putdata (auto) check
p = [ 53.+0j, 54.+0j, 55.+0j, 56.+0j ]
try:
  n = d.putdata("data", p, first_frame=38, first_sample=1)
except:
  CheckOK2(138,1)
CheckSimple2(138,1,n,4)

try:
  n = d.getdata("data", pygetdata.INT, first_frame=38, num_frames=1, as_list=1)
except:
  CheckOK2(138,2)
CheckSimple2(138,2,n,[41, 53, 54, 55, 56, 46, 47, 48])

# 139: native_type_name check
try:
  n = d.native_type_name("data")
except:
  CheckOK(139)
CheckSimple(139,n,"INT8")

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
CheckSimple2(143,4,ent.in_fields,( B("mult"), B("bit")))

# 145: entry (recip) check
try:
  ent = d.entry("recip")
except:
  CheckOK(145)
CheckSimple2(145,1,ent.field_type,pygetdata.RECIP_ENTRY)
CheckSimple2(145,2,ent.field_type_name,"RECIP_ENTRY")
CheckSimple2(145,3,ent.fragment,0)
CheckSimple2(145,4,ent.in_fields,( B("div"),))
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
CheckSimple2(146,3,ent.in_fields,( B("in1"), B("in2")))

# 148: add / entry (recip) check
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
CheckSimple2(148,3,ent.in_fields,( B("in3"),))
CheckSimple2(148,4,ent.dividend,33.3)

# 149: madd / entry (div) check
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
CheckSimple2(149,3,ent.in_fields,( B("in3"), B("in2")))

# 151: madd / entry (recip) check
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
CheckSimple2(151,3,ent.in_fields,( B("in3"),))
CheckSimple2(151,4,ent.dividend,B("const"))

# 155: fragment.rewrite check
try:
  f.rewrite()
except:
  CheckOK(155)

# 156: invalid dirfile check
try:
  m = pygetdata.dirfile(None)
except:
  CheckOK2(156,1)

try:
  n = m.nfragments()
except:
  CheckException2(156,2,pygetdata.BadDirfileError)

try:
  m.discard()
except:
  pass

# 157: standards version
try:
  n = d.standards
except:
  CheckOK2(157,1)
CheckSimple(157,n,pygetdata.DIRFILE_STANDARDS_VERSION)

try:
  d.standards = 0
except:
  CheckException2(157,2,pygetdata.ArgumentError)

# 158: gd_get_carray
try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK(158)

CheckNumpy(158,n,numpy.arange(1,7))

# 159: gd_get_carray_slice (INT8)
try:
  n = d.get_carray("carray", pygetdata.INT, start=2, len=2)
except:
  CheckOK(159)

CheckNumpy(159,n,numpy.arange(3,5))

# 162: gd_get_carray_slice (INT64)
try:
  n = d.get_carray("carray", pygetdata.LONG, start=2, len=2)
except:
  CheckOK(162)

CheckNumpy(162,n,numpy.arange(L(3),L(5)))

# 163: gd_get_carray_slice (auto-type)
try:
  n = d.get_carray("carray", start=2, len=2)
except:
  CheckOK(163)

CheckNumpy(163,n,numpy.array([3.3, 4.4]))

# 164: gd_get_carray_slice (FLOAT64)
try:
  n = d.get_carray("carray", pygetdata.FLOAT, start=2, len=2)
except:
  CheckOK(164)

CheckNumpy(163,n,numpy.array([3.3, 4.4]))

# 166: gd_get_carray_slice (COMPLEX128)
try:
  n = d.get_carray("carray", pygetdata.COMPLEX, start=2, len=2)
except:
  CheckOK(166)

CheckNumpy(166,n,numpy.array([3.3+0j, 4.4+0j]))

# 167: gd_carrays
try:
  n = d.carrays(pygetdata.INT)
except:
  CheckOK(167)

CheckSimple2(167,1,len(n),1)
CheckSimple2(167,2,n[0][0],B("carray"))
CheckNumpy2(167,3,n[0][1],numpy.arange(1,7))

# 168: gd_put_carray
try:
  d.put_carray("carray", [9,8,7,6,5,4])
except:
  CheckOK2(168,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(168,2)

CheckNumpy(168,n,numpy.arange(9,3,-1))

# 169: gd_put_carray_slice (INT8)
try:
  d.put_carray("carray", [169,169], start=2)
except:
  CheckOK2(169,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(169,2)

CheckNumpy(169,n,numpy.array([9,8,169,169,5,4]))

# 172: gd_put_carray_slice (INT64)
try:
  d.put_carray("carray", [L(172),L(172)], start=2)
except:
  CheckOK2(172,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(172,2)

CheckNumpy(172,n,numpy.array([9,8,172,172,5,4]))

# 174: gd_put_carray_slice (FLOAT64)
try:
  d.put_carray("carray", [174.,174.], start=2)
except:
  CheckOK2(174,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(174,2)

CheckNumpy(174,n,numpy.array([9,8,174,174,5,4]))

# 176: gd_put_carray_slice (COMPLEX128)
try:
  d.put_carray("carray", [176.+0j,176.+0j], start=2)
except:
  CheckOK2(176,1)

try:
  n = d.get_carray("carray", pygetdata.INT)
except:
  CheckOK2(176,2)

CheckNumpy(176,n,numpy.array([9,8,176,176,5,4]))

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
CheckSimple(183,n,[(B('const'), 93), (B('new11'), 0)])

# 186: gd_constants (long)
try:
  n = d.constants(pygetdata.LONG)
except:
  CheckOK(186)
CheckSimple(186,n,[(B('const'), L(93)), (B('new11'), L(0))])

# 188: gd_constants (float)
try:
  n = d.constants(pygetdata.FLOAT)
except:
  CheckOK(188)
CheckSimple(188,n,[(B('const'), 93.0), (B('new11'), 0.0)])

# 190: gd_constants (complex)
try:
  n = d.constants(pygetdata.COMPLEX)
except:
  CheckOK(190)
CheckSimple(190,n,[(B('const'), 93.0), (B('new11'), 0.0)])

# 191: gd_constants (int)
try:
  n = d.mconstants("data", pygetdata.INT)
except:
  CheckOK(191)
CheckSimple(191,n,[(B('mconst'), 3), (B('mnew11'), 0)])

# 194: gd_constants (long)
try:
  n = d.mconstants("data", pygetdata.LONG)
except:
  CheckOK(194)
CheckSimple(194,n,[(B('mconst'), L(3)), (B('mnew11'), L(0))])

# 196: gd_constants (float)
try:
  n = d.mconstants("data", pygetdata.FLOAT)
except:
  CheckOK(196)
CheckSimple(196,n,[(B('mconst'), 3.3), (B('mnew11'), 0.)])

# 198: gd_constants (complex)
try:
  n = d.mconstants("data", pygetdata.COMPLEX)
except:
  CheckOK(198)
CheckSimple(198,n,[(B('mconst'), 3.3+4.4j), (B('mnew11'), 0j)])

# 199: gd_strings
try:
  n = d.strings()
except:
  CheckOK(199)
CheckSimple(199,n,[(B('lorem'), B('Lorem ipsum')), (B('new12'), B('')), 
  (B('string'), B('Arthur Dent'))])

# 200: gd_strings
try:
  n = d.mstrings("data")
except:
  CheckOK(200)
CheckSimple(200,n,[(B('mstr'), B('This is a string constant.'))])

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
CheckNumpy2(203,2,m,numpy.arange(17,25))

# 204: gd_tell
try:
  n = d.tell("data")
except:
  CheckOK(204)
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
CheckSimple2(211, 4, ent.in_fields, ( B('linterp'), B('mult') ))
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
CheckSimple2(212, 4, ent.in_fields, ( B('in1'), B('in2') ))
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
CheckSimple2(214, 4, ent.in_fields, ( B('in2'), B('in3') ))
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
CheckSimple2(217, 4, ent.in_fields, ( B('in3'), B('in4') ))
CheckSimple2(217, 5, ent.threshold, 32e3)

# 218: gd_alias_target check
try:
  str = d.alias_target('alias')
except:
  CheckOK(218)
CheckSimple(218, str, B('data'))

# 219: gd_add_alias check
try:
  d.add_alias('new20', 'data', 0)
except:
  CheckOK2(219, 1)

try:
  str = d.alias_target('new20')
except:
  CheckOK2(219, 2)
CheckSimple(219, str, B('data'))

# 220: gd_madd_alias check
try:
  d.madd_alias('data', 'mnew20', 'data')
except:
  CheckOK2(220, 1)

try:
  str = d.alias_target('data/mnew20')
except:
  CheckOK2(220, 2)
CheckSimple(220, str, B('data'))

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
CheckSimple(222, n, [ B('data'), B('alias'), B('new20'), B('data/mnew20') ])

# 223: gd_include_affix check
try:
  d.include('format1', 0, prefix='A', suffix='Z',
      flags=pygetdata.CREAT | pygetdata.EXCL)
except:
  CheckOK(223)

# 226: gd_fragment_affixes check
try:
  n = d.fragment(1).prefix
  m = d.fragment(1).suffix
except:
  CheckOK(226)
CheckSimple2(226, 1, n, B("A"))
CheckSimple2(226, 2, m, B("Z"))

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
CheckSimple2(227, 1, n, B("B"))
CheckSimple2(227, 2, m, B(""))

# 228: gd_entry (MPLEX) check
try:
  ent = d.entry('mplex')
except:
  CheckOK(228)
CheckSimple2(228, 1, ent.field_type, pygetdata.MPLEX_ENTRY)
CheckSimple2(228, 2, ent.fragment, 0)
CheckSimple2(228, 3, ent.count_val, 1)
CheckSimple2(228, 4, ent.in_fields, ( B('data'), B('sbit') ))
CheckSimple2(228, 5, ent.period, 10)

# 229: gd_add_mplex check
ent = pygetdata.entry(pygetdata.MPLEX_ENTRY, "new21", 0, ("in1", "in2", 5, 6))
try:
  d.add(ent)
except:
  CheckOK2(229, 1)

try:
  ent = d.entry('new21')
except:
  CheckOK2(229, 2)
CheckSimple2(229, 1, ent.field_type, pygetdata.MPLEX_ENTRY)
CheckSimple2(229, 2, ent.fragment, 0)
CheckSimple2(229, 3, ent.count_val, 5)
CheckSimple2(229, 4, ent.in_fields, ( B('in1'), B('in2') ))
CheckSimple2(229, 5, ent.period, 6)

# 230: gd_madd_mplex check
ent = pygetdata.entry(pygetdata.MPLEX_ENTRY, "mnew21", 0, ("in2", "in3", 0, 12))
try:
  d.madd(ent, "data")
except:
  CheckOK2(230, 1)

try:
  ent = d.entry('data/mnew21')
except:
  CheckOK2(230, 2)
CheckSimple2(230, 1, ent.field_type, pygetdata.MPLEX_ENTRY)
CheckSimple2(230, 2, ent.fragment, 0)
CheckSimple2(230, 3, ent.count_val, 0)
CheckSimple2(230, 4, ent.in_fields, ( B('in2'), B('in3') ))
CheckSimple2(230, 5, ent.period, 12)

# 231: gd_alter_mplex check
ent = pygetdata.entry(pygetdata.MPLEX_ENTRY, "new21", 0, { "count_val": 3,
  "in_field1": "in3", "in_field2": "in4", "period": 7 })
try:
  d.alter('new21', ent)
except:
  CheckOK2(231, 1)

try:
  ent = d.entry('new21')
except:
  CheckOK2(231, 2)
CheckSimple2(231, 1, ent.field_type, pygetdata.MPLEX_ENTRY)
CheckSimple2(231, 2, ent.fragment, 0)
CheckSimple2(231, 3, ent.count_val, 3)
CheckSimple2(231, 4, ent.in_fields, ( B('in3'), B('in4') ))
CheckSimple2(231, 5, ent.period, 7)

# 232: gd_strtok check
try:
  str = d.strtok("\"test1 test2\" test3\ test4")
except:
  CheckOK2(232, 1)
CheckSimple2(232, 2, str, B("test1 test2"))

try:
  str = d.strtok()
except:
  CheckOK2(232, 3)
CheckSimple2(232, 4, str, B("test3 test4"))

# 233: gd_raw_close check
try:
  d.raw_close('data')
except:
  CheckOK(233)

# 234: gd_desync check
try:
  n = d.desync()
except:
  CheckOK(234)
CheckSimple(234, n, 0)

# 235: gd_flags check
try:
  d.flags = pygetdata.PRETTY_PRINT
except:
  CheckOK2(235,1)

try:
  CheckSimple(235, d.flags, pygetdata.PRETTY_PRINT)
except:
  CheckOK2(235,2)

# 236: gd_verbose_prefix check
try:
  CheckSimple(236, d.verbose_prefix, None)
except:
  CheckOK2(236, 1)

try:
  d.verbose_prefix = "big_test: "
except:
  CheckOK2(236, 2)
CheckSimple(236, d.verbose_prefix, "big_test: ")

# 237: gd_nentries check
try:
  n = d.nentries("data", pygetdata.SCALAR_ENTRIES,
      pygetdata.ENTRIES_HIDDEN | pygetdata.ENTRIES_NOALIAS)
except:
  CheckOK2(237, 1)
CheckSimple2(237, 1, n, 6)

try:
  n = d.nentries(type = pygetdata.VECTOR_ENTRIES,
      flags = pygetdata.ENTRIES_HIDDEN | pygetdata.ENTRIES_NOALIAS)
except:
  CheckOK2(237, 2)
CheckSimple2(237, 2, n, 24)

# 239: gd_entry_list check
try:
  n = d.entry_list(type = pygetdata.VECTOR_ENTRIES,
      flags = pygetdata.ENTRIES_HIDDEN | pygetdata.ENTRIES_NOALIAS)
except:
  CheckOK(239)
CheckSimple(239,n, [ B('bit'), B('div'), B('data'), B('mult'), B('new1'),
    B('new2'), B('new4'), B('new6'), B('new7'), B('new8'), B('sbit'),
    B('INDEX'), B('indir'), B('mplex'), B('new14'), B('new16'), B('new18'),
    B('new21'), B('phase'), B('recip'), B('lincom'), B('window'), B('linterp'),
    B('polynom') ])

# 240: gd_mplex_lookback check
try:
  d.mplex_lookback = pygetdata.LOOKBACK_ALL
except:
  CheckOK(240, 1)

try:
  n = d.mplex_lookback
except:
  CheckOK2(240, 2)
CheckSimple(240, n, pygetdata.LOOKBACK_ALL)

# 241: linterp_tablename check
try:
  n = d.linterp_tablename("linterp")
except:
  CheckOK(241)
CheckEOS(241,n,"dirfile/lut")

# 242: mcarrays
try:
  n = d.mcarrays("data", pygetdata.FLOAT)
except:
  CheckOK(242)

CheckSimple2(242,1,len(n),2)
CheckSimple2(242,2,n[0][0],B("mcarray"))
CheckNumpy2(242,3,n[0][1],1.9 + 0.9 * numpy.arange(0,5))
CheckSimple2(242,4,n[1][0],B("mnew17"))
CheckNumpy2(242,5,n[1][1],[0,0])

# 271: encoding_support
n = pygetdata.encoding_support(pygetdata.SIE_ENCODED)
CheckSimple(271,n,pygetdata.RDWR)

# 272: check NULL return from gd_reference
try:
  m = pygetdata.dirfile("dirfile/empty",
      pygetdata.RDWR | pygetdata.CREAT | pygetdata.EXCL)
except:
  CheckOK2(272, 1)

try:
  n = m.reference
  CheckSimple(272, n, None)
except:
  CheckOK2(272, 2)

# 274: gd_get_carray_slice (NULL)
try:
  n = d.get_carray("carray", pygetdata.NULL, start=2, len=2)
except:
  CheckOK(274)
CheckSimple(274, n, None)

# 277: gd_entry (SARRAY)
try:
  ent = d.entry("sarray")
except:
  CheckOK(277)
CheckSimple2(277,1,ent.field_type,pygetdata.SARRAY_ENTRY)
CheckSimple2(277,2,ent.field_type_name,"SARRAY_ENTRY")
CheckSimple2(277,3,ent.fragment,0)
CheckSimple2(277,4,ent.array_len,7)

# 278: gd_get_sarray
try:
  n = d.get_sarray("sarray")
except:
  CheckOK(278)

CheckSimple(278,n,['one', 'two', 'three', 'four', 'five', 'six', 'seven'])

# 279: gd_get_sarray_slice
try:
  n = d.get_sarray("sarray", start=2, len=2)
except:
  CheckOK(279)

CheckSimple(279,n,['three', 'four'])

# 280: gd_sarrays
try:
  n = d.sarrays()
except:
  CheckOK(280)

CheckSimple2(280,1,len(n),1)
CheckSimple2(280,2,n,[("sarray",
  ['one', 'two', 'three', 'four', 'five', 'six', 'seven'])])

# 281: gd_put_sarray
try:
  d.put_sarray("sarray",
      ['eka', 'dvi', 'tri', 'catur', 'panca', 'sas', 'sapta'])
except:
  CheckOK2(281,1)

try:
  n = d.get_sarray("sarray")
except:
  CheckOK2(281,2)

CheckSimple(281,n,['eka', 'dvi', 'tri', 'catur', 'panca', 'sas', 'sapta'])

# 282: gd_put_sarray_slice
try:
  d.put_sarray("sarray", ['asta', 'nava'], start=2)
except:
  CheckOK2(282,1)

try:
  n = d.get_sarray("sarray")
except:
  CheckOK2(282,2)

CheckSimple(282,n,['eka', 'dvi', 'asta', 'nava', 'panca', 'sas', 'sapta'])

# 283: gd_add_sarray
ent = pygetdata.entry(pygetdata.SARRAY_ENTRY, "new283", 0, (2,))
try:
  d.add(ent)
except:
  CheckOK2(283,1)

try:
  ent = d.entry("new283")
except:
  CheckOK2(283,2)
CheckSimple2(283,1,ent.field_type,pygetdata.SARRAY_ENTRY)
CheckSimple2(283,2,ent.field_type_name,"SARRAY_ENTRY")
CheckSimple2(283,3,ent.fragment,0)
CheckSimple2(283,4,ent.array_len,2)

# 285: gd_madd_sarray
ent = pygetdata.entry(pygetdata.SARRAY_ENTRY, "mnew285", 0, {"array_len": 2})
try:
  d.madd(ent,"data")
except:
  CheckOK2(285,1)

try:
  ent = d.entry("data/mnew285")
except:
  CheckOK2(285,2)
CheckSimple2(285,1,ent.field_type,pygetdata.SARRAY_ENTRY)
CheckSimple2(285,2,ent.field_type_name,"SARRAY_ENTRY")
CheckSimple2(285,3,ent.fragment,0)
CheckSimple2(285,4,ent.array_len,2)

# 287: gd_msarrays
try:
  n = d.msarrays("data")
except:
  CheckOK(287)

CheckSimple2(287,1,len(n),2)
CheckSimple2(287,2,n,[
  ("msarray", ['eight', 'nine', 'ten', 'eleven', 'twelve']),
  ("mnew285", ['', ''])])

# 288: entry (indir) check
try:
  ent = d.entry("indir")
except:
  CheckOK(288)
CheckSimple2(288,1,ent.field_type,pygetdata.INDIR_ENTRY)
CheckSimple2(288,2,ent.field_type_name,"INDIR_ENTRY")
CheckSimple2(288,3,ent.fragment,0)
CheckSimple2(288,4,ent.in_fields,( "data", "carray"))

# 289: add / entry (indir) check
ent = pygetdata.entry(pygetdata.INDIR_ENTRY, "new289", 0, ("in1", "in2"))
try:
  d.add(ent)
except:
  CheckOK2(289,1)

try:
  ent = d.entry("new289")
except:
  CheckOK2(289,2)
CheckSimple2(289,1,ent.field_type,pygetdata.INDIR_ENTRY)
CheckSimple2(289,2,ent.fragment,0)
CheckSimple2(289,3,ent.in_fields,( "in1", "in2"))

# 290: madd / entry (indir) check
ent = pygetdata.entry(pygetdata.INDIR_ENTRY, "mnew290", 0,
    {"in_field1": "in3", "in_field2": "in2"})
try:
  d.madd(ent,"data")
except:
  CheckOK2(290,1)

try:
  ent = d.entry("data/mnew290")
except:
  CheckOK2(290,2)
CheckSimple2(290,1,ent.field_type,pygetdata.INDIR_ENTRY)
CheckSimple2(290,2,ent.fragment,0)
CheckSimple2(290,3,ent.in_fields,( "in3", "in2"))

# 292: entry (sindir) check
try:
  ent = d.entry("sindir")
except:
  CheckOK(292)
CheckSimple2(292,1,ent.field_type,pygetdata.SINDIR_ENTRY)
CheckSimple2(292,2,ent.field_type_name,"SINDIR_ENTRY")
CheckSimple2(292,3,ent.fragment,0)
CheckSimple2(292,4,ent.in_fields,( "data", "sarray"))

# 293: add / entry (dindir) check
ent = pygetdata.entry(pygetdata.SINDIR_ENTRY, "new293", 0, ("in1", "in2"))
try:
  d.add(ent)
except:
  CheckOK2(293,1)

try:
  ent = d.entry("new293")
except:
  CheckOK2(293,2)
CheckSimple2(293,1,ent.field_type,pygetdata.SINDIR_ENTRY)
CheckSimple2(293,2,ent.fragment,0)
CheckSimple2(293,3,ent.in_fields,( "in1", "in2"))

# 294: madd / entry (sindir) check
ent = pygetdata.entry(pygetdata.SINDIR_ENTRY, "mnew294", 0,
    {"in_field1": "in3", "in_field2": "in2"})
try:
  d.madd(ent,"data")
except:
  CheckOK2(294,1)

try:
  ent = d.entry("data/mnew294")
except:
  CheckOK2(294,2)
CheckSimple2(294,1,ent.field_type,pygetdata.SINDIR_ENTRY)
CheckSimple2(294,2,ent.fragment,0)
CheckSimple2(294,3,ent.in_fields,( "in3", "in2"))

# 296: getstrdata
try:
  n = d.getdata("sindir", num_frames=1)
except:
  CheckOK(296)
CheckSimple(296,n,[ "eka", "eka", "eka", "eka", "eka", "eka", "eka", "eka"])

# 302: gd_include_affix check
try:
  d.include('format2', 0, namespace='N', flags=pygetdata.CREAT | pygetdata.EXCL)
except:
  CheckOK(302)

# 303: get namespace
try:
  n = d.fragment(2).namespace
except:
  CheckOK(303)

# 304: set namespace
try:
  d.fragment(2).namespace = 'S'
except:
  CheckOK2(304, 0)

try:
  n = d.fragment(2).namespace
except:
  CheckOK2(304, 1)
CheckSimple2(304, 2, n, B("S"))

try:
  del d.fragment(2).namespace
except:
  CheckOK2(304, 3)

try:
  n = d.fragment(2).namespace
except:
  CheckOK2(304, 4)
CheckSimple2(304, 5, n, B(""))

# 305: gd_match_entries
try:
  n = d.match_entries(regex='^lin', fragment=0)
except:
  CheckOK2(305, 0)
CheckSimple2(305, 1, n, [ B('lincom'), B('linterp') ])














# ==========================================================================

d.discard()
del d
os.system("rm -rf dirfile")

if (ne > 0):
  print ("ne =", ne)
  sys.exit(1)
