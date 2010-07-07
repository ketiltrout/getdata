# (C) 2009-2010 D. V. Wiebe
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
    print "e[", t, "] =", sys.exc_type

def CheckException2(t,m,g):
  global ne
  if (sys.exc_type != g):
    ne+=1
    print "e[", t, ",", m, "] =", sys.exc_type

def CheckNumpy(t,v,g):
  global ne
  if (numpy.any(v - g)):
    ne+=1
    print "a[", t, "] =", v

def CheckNumpy2(t,m,v,g):
  global ne
  if (numpy.any(v - g)):
    ne+=1
    print "a[", t, ",", m, "] =", v

def CheckSimple(t,v,g):
  global ne
  if (v != g):
    ne+=1
    print "n[", t, "] =", v

def CheckSimple2(t,m,v,g):
  global ne
  if (v != g):
    ne+=1
    print "n[", t, ",", m, "] =", v

# create the dirfile first
data=array.array("B",range(1,81))
os.system("rm -rf dirfile")
os.mkdir("dirfile")
file=open("dirfile/data", 'w')
data.tofile(file)
file.close()

ne = 0

fields = ["INDEX", "bit", "const", "data", "lincom", "linterp", "mult",
      "phase", "polynom", "sbit", "string"]

nfields = 11
file=open("dirfile/format", 'w')
file.write(
    "/ENDIAN little\n"
    "data RAW INT8 8\n"
    "lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const\n"
    "/META data mstr STRING \"This is a string constant.\"\n"
    "/META data mconst CONST COMPLEX128 3.3;4.4\n"
    "/META data mlut LINTERP DATA ./lut\n"
    "const CONST FLOAT64 5.5\n"
    "linterp LINTERP data /look/up/file\n"
    "polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const\n"
    "bit BIT data 3 4\n"
    "sbit SBIT data 5 6\n"
    "mult MULTIPLY data sbit\n"
    "phase PHASE data 11\n"
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
  CheckNumpy(104,n,numpy.arange(41.,49.))
else:
  CheckSimple(106,n,[41.,42.,43.,44.,45.,46.,47.,48.])

# 108: getdata (complex) check
try:
  n = d.getdata("data", pygetdata.COMPLEX, first_frame=5, num_frames=1)
except:
  CheckOK(108)
CheckSimple(108,len(n),8)
if (pygetdata.__numpy_supported__):
  CheckNumpy(104,n,numpy.arange(41,49,dtype=numpy.complex128))
else:
  CheckSimple(108,n,[41.+0j,42.+0j,43.+0j,44.+0j,45.+0j,46.+0j,47.+0j,48.+0j])

# 3: get_constant (int) check
try:
  n = d.get_constant("const", pygetdata.INT)
except:
  CheckOK(3)
CheckSimple(3,n,5)

# 112: get_constant (long) check
try:
  n = d.get_constant("const", pygetdata.LONG)
except:
  CheckOK(112)
CheckSimple(112,n,5L)

# 114: get_constant (float) check
try:
  n = d.get_constant("const", pygetdata.FLOAT)
except:
  CheckOK(114)
CheckSimple(114,n,5.5)

# 116: get_constant (float) check
try:
  n = d.get_constant("const", pygetdata.COMPLEX)
except:
  CheckOK(116)
CheckSimple(116,n,5.5+0j)

# 6: get_nfields check
try:
  n = d.get_nfields()
except:
  CheckOK(6)
CheckSimple(6,n,nfields)

# 8: get_field_list check
try:
  n = d.get_field_list()
except:
  CheckOK(8)
CheckSimple(8,n,fields)

# 9: get_nmfields check
try:
  n = d.get_nmfields("data")
except:
  CheckOK(9)
CheckSimple(9,n,3)

# 10: get_mfield_list check
try:
  n = d.get_mfield_list("data")
except:
  CheckOK(10)
CheckSimple(10,n,["mstr", "mconst", "mlut"])

# 11: nframes check
try:
  n = d.nframes
except:
  CheckOK(11)
CheckSimple(11,n,10)

# 12: get_spf check
try:
  n = d.get_spf("data")
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

# 14: get_error_string check
try:
  n = d.getdata("x", pygetdata.INT, first_frame=5, num_frames=1)
except:
  CheckException(14,pygetdata.BadCodeError)
  CheckSimple(14,d.error_string,"Field not found: x")

# 16: entry (raw) check
try:
  ent = d.get_entry("data")
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
  ent = d.get_entry("lincom")
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
  ent = d.get_entry("polynom")
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
  ent = d.get_entry("linterp")
except:
  CheckOK(21)
CheckSimple2(21,1,ent.field_type,pygetdata.LINTERP_ENTRY)
CheckSimple2(21,2,ent.field_type_name,"LINTERP_ENTRY")
CheckSimple2(21,3,ent.fragment,0)
CheckSimple2(21,4,ent.in_fields,( "data", ))
CheckSimple2(21,5,ent.table,"/look/up/file")

# 22: entry (bit) check
try:
  ent = d.get_entry("bit")
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
  ent = d.get_entry("sbit")
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
  ent = d.get_entry("mult")
except:
  CheckOK(24)
CheckSimple2(24,1,ent.field_type,pygetdata.MULTIPLY_ENTRY)
CheckSimple2(24,2,ent.field_type_name,"MULTIPLY_ENTRY")
CheckSimple2(24,3,ent.fragment,0)
CheckSimple2(24,4,ent.in_fields,( "data", "sbit"))

# 25: entry (phase) check
try:
  ent = d.get_entry("phase")
except:
  CheckOK(25)
CheckSimple2(25,1,ent.field_type,pygetdata.PHASE_ENTRY)
CheckSimple2(25,2,ent.field_type_name,"PHASE_ENTRY")
CheckSimple2(25,3,ent.fragment,0)
CheckSimple2(25,4,ent.in_fields,( "data", ))
CheckSimple2(25,5,ent.shift,11)

# 26: entry (const) check
try:
  ent = d.get_entry("const")
except:
  CheckOK(26)
CheckSimple2(26,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(26,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(26,3,ent.fragment,0)
CheckSimple2(26,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(26,5,ent.data_type_name,"FLOAT64")

# 134: entry (string) check
try:
  ent = d.get_entry("string")
except:
  CheckOK(134)
CheckSimple2(134,1,ent.field_type,pygetdata.STRING_ENTRY)
CheckSimple2(134,2,ent.field_type_name,"STRING_ENTRY")
CheckSimple2(134,3,ent.fragment,0)

# 27: fragment_index check
try:
  n = d.get_fragment_index("data")
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
  ent = d.get_entry("new1")
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
  ent = d.get_entry("new2")
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
  ent = d.get_entry("new4")
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
  ent = d.get_entry("new6")
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
  ent = d.get_entry("new7")
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
  ent = d.get_entry("sbit")
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
  ent = d.get_entry("new9")
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
  ent = d.get_entry("new10")
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
  ent = d.get_entry("new11")
except:
  CheckOK2(38,2)
CheckSimple2(38,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(38,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(38,3,ent.fragment,0)
CheckSimple2(38,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(38,5,ent.data_type_name,"FLOAT64")

# 39: get_fragment check
try:
  f = d.get_fragment(0)
except:
  CheckOK(39)
CheckSimple(39,f.name,"dirfile/format")

# 40: get_nfragments check
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

# 42: get_nfields_by_type check
try:
  n = d.get_nfields(pygetdata.LINCOM_ENTRY)
except:
  CheckOK(42)
CheckSimple(42,n,2)

# 43: get_field_list_by_type check
try:
  n = d.get_field_list(pygetdata.LINCOM_ENTRY)
except:
  CheckOK(43)
CheckSimple(43,n,["lincom", "new2"])

# 44: get_nvectors check
try:
  n = d.get_nvectors()
except:
  CheckOK(44)
CheckSimple(44,n,17)

# 45: get_field_list check
try:
  n = d.get_vector_list()
except:
  CheckOK(45)
CheckSimple(45,n,['INDEX', 'bit', 'data', 'lincom', 'linterp', 'mult', 'new1',
  'new10', 'new2', 'new4', 'new6', 'new7', 'new8', 'new9', 'phase', 'polynom',
  'sbit'])

# 46: add / entry (lincom) check
ent = pygetdata.entry(pygetdata.LINCOM_ENTRY, "mnew1", 0,
    {"in_fields": ("in1", "in2"), "m": (9.9, 7.7), "b": (8.8, 6.6)})
try:
  d.madd(ent, "data")
except:
  CheckOK2(46,1)

try:
  ent = d.get_entry("data/mnew1")
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
  ent = d.get_entry("data/mnew3")
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
  ent = d.get_entry("data/mnew6")
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
  ent = d.get_entry("data/mnew7")
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
  ent = d.get_entry("data/mnew8")
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
  ent = d.get_entry("data/mnew9")
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
  ent = d.get_entry("data/mnew10")
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
  ent = d.get_entry("data/mnew11")
except:
  CheckOK2(55,2)
CheckSimple2(55,1,ent.field_type,pygetdata.CONST_ENTRY)
CheckSimple2(55,2,ent.field_type_name,"CONST_ENTRY")
CheckSimple2(55,3,ent.fragment,0)
CheckSimple2(55,4,ent.data_type,pygetdata.FLOAT64)
CheckSimple2(55,5,ent.data_type_name,"FLOAT64")

#56: get_string check
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
  ent = d.get_entry("new12")
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

# 63: get_nmfields_by_type check
try:
  n = d.get_nmfields("data",pygetdata.LINCOM_ENTRY)
except:
  CheckOK(63)
CheckSimple(63,n,1)

# 64: get_mfield_list_by_type check
try:
  n = d.get_mfield_list("data",pygetdata.LINCOM_ENTRY)
except:
  CheckOK(64)
CheckSimple(64,n,["mnew1"])

# 65: get_nmfields_by_type check
try:
  n = d.get_nmvectors("data")
except:
  CheckOK(65)
CheckSimple(65,n,8)

# 66: get_mfield_list_by_type check
try:
  n = d.get_mvector_list("data")
except:
  CheckOK(66)
CheckSimple(66,n,['mlut', 'mnew1', 'mnew3', 'mnew6', 'mnew7', 'mnew8', 'mnew9',
  'mnew10'])

# 67: alter / raw check
ent = pygetdata.entry(pygetdata.RAW_ENTRY, "new1", 0,
    {"type": pygetdata.FLOAT32, "spf": 4})
try:
  n = d.alter("new1", ent)
except:
  CheckOK2(67,1)

try:
  ent = d.get_entry("new1")
except:
  CheckOK(67,2)
CheckSimple2(67,1,ent.field_type,pygetdata.RAW_ENTRY)
CheckSimple2(67,2,ent.field_type_name,"RAW_ENTRY")
CheckSimple2(67,3,ent.fragment,0)
CheckSimple2(67,4,ent.data_type,pygetdata.FLOAT32)
CheckSimple2(67,5,ent.data_type_name,"FLOAT32")
CheckSimple2(67,6,ent.spf,4)

# 78: get_encoding check
try:
  f = d.get_fragment(0)
  CheckSimple(78,f.encoding,pygetdata.UNENCODED)
except:
  CheckOK(78)

# 79: get_endianness check
try:
  CheckSimple(79,f.endianness,pygetdata.LITTLE_ENDIAN)
except:
  CheckOK(79)

# 80: dirfilename check
try:
  CheckSimple(80,d.name,"dirfile")
except:
  CheckOK(80)

# 81: get_parent_fragment check
try:
  f = d.get_fragment(1)
  CheckSimple(81,f.parent,0)
except:
  CheckOK(81)

# 82: dirfile_protect check
try:
  f.protection = pygetdata.PROTECT_DATA
except:
  CheckOK(82)
  
# 83: get_protection check
try:
  f = d.get_fragment(1)
  CheckSimple(83,f.protection,pygetdata.PROTECT_DATA)
except:
  CheckOK(83)

# 84: get_raw_filename check
try:
  n = d.get_raw_filename("data")
except:
  CheckOK(84)
CheckSimple(84,n,"dirfile/data")

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
  f = d.get_fragment(1)
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
  ent = d.get_entry("new10")
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
  ent = d.get_entry("new10")
except:
  CheckException2(90,2,pygetdata.BadCodeError)

# 91: malter_spec check
try:
  d.malter_spec("mnew10 PHASE in4 11", "data", 0)
except:
  CheckOK2(91,1)

try:
  ent = d.get_entry("data/mnew10")
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
  ent = d.get_entry("new9")
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
  ent = d.get_entry("new9")
except:
  CheckException2(93,2,pygetdata.BadCodeError)

try:
  ent = d.get_entry("newer")
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
  ent = d.get_entry("newer")
except:
  CheckException2(93,2,pygetdata.BadCodeError)

# 95: frameoffset check
try:
  f = d.get_fragment(0)
  CheckSimple(95,f.frameoffset,0)
except:
  CheckOK(95)

# 96: alter_frameoffset check
try:
  f.alter_frameoffset(33, 0)
  CheckSimple(96,f.frameoffset,33)
except:
  CheckOK(96)

# 97: get_native_type check
try:
  n = d.get_native_type("data")
except:
  CheckOK(97)
CheckSimple(97,n,pygetdata.INT8)

# 137: get_native_type_name check
try:
  n = d.get_native_type_name("data")
except:
  CheckOK(137)
CheckSimple(137,n,"INT8")

# 99: validate check
try:
  d.validate("new7")
except:
  CheckException(99,pygetdata.BadCodeError)

# 101: get_framenum check
try:
  n = d.get_framenum("data", 33.3, start=6)
except:
  CheckOK(101)
CheckSimple(101,n,37.0375)

# 86: get_eof check
try:
  n = d.get_eof("lincom")
except:
  CheckOK(86)
CheckSimple(86,n,344)

# 142: get_bof check
try:
  n = d.get_bof("lincom")
except:
  CheckOK(142)
CheckSimple(142,n,264)


os.system("rm -rf dirfile")

if (ne > 0):
  print "ne =", ne
  sys.exit(1)
