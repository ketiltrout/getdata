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

# Test character_encoding

import re
import os
import sys
import pygetdata

# Python2/3 abstraction:

# an encoded character (byte)
def E(c):
  if sys.version[:1] == '3':
    return bytes([c])
  return chr(c)

# an encoded string
def B(s):
  if sys.version[:1] == '3':
    return bytes(s, "UTF-8")
  return s

# a filesystem decoded string
def F(s):
  if sys.version[:1] == '3':
    return os.fsdecode(s)
  return s

# a decoded string
def U(s):
  if sys.version[:1] == '3':
    return s
  return unicode(s)

ne = 0

def CheckOK(t):
  global ne
  ne+=1
  print ("e[", t, "] =", sys.exc_info()[0], sys.exc_value)

def CheckSimple(t,v,g):
  global ne
  if (v != g):
    ne+=1
    print ("n[", t, "] =", v, "expected", g)

def CheckEOS(t,v,g):
  global ne
  if (v[-len(g):] != g):
    ne+=1
    print ("n[", t, "] =", repr(v), "expected", repr(g))

def CheckExc(n):
  global ne
  global e
  if (e):
    e=0
    ne+=1
    print ("Missing UnicodeDecodeError for " + str(n))

def CheckParms(n,c):
  global e
  try:
    e = 1
    print (c.parameters)
  except UnicodeDecodeError:
    e = 0

  CheckExc(n)

def CheckIns(n,c):
  global e
  try:
    e = 1
    print (c.in_fields)
  except UnicodeDecodeError:
    e = 0

  CheckExc(n)

# create the dirfile
os.system("rm -rf dirfile")
os.mkdir("dirfile")

# Encoded string (koi8-r)
estring =  E(0xF3) + E(0xD4) + E(0xD2) + E(0xCF) + E(0xCB) + E(0xC1)

# Filesystem *decoded* string
fstring = F(estring)

# byte-escaped (used for Dirfile metadata)
xstring = B('\\xF3\\xD4\\xD2\\xCF\\xCB\\xC1')

f=open("dirfile/format", "wb")
f.write(
        B("/ALIAS ") + xstring + B("_al ") + xstring + B("_t\n") +
        B("s1 STRING ") + xstring + B("1\n") +
        xstring + B("_s2 STRING ") + xstring + B("2\n") +
        xstring + B("_a1 CARRAY UINT64 1 2 3\n") +
        xstring + B("_a2 CARRAY UINT64 1 2 3\n") +
        xstring + B("_c1 CONST UINT8 1\n") +
        xstring + B("_c2 CONST UINT8 1\n") +
        xstring + B("_r1 RAW UINT8 ") + xstring + B("_spf\n") +
        B("r2 RAW UINT8 ") + xstring + B("_spf\n") +
        B("l1 LINTERP in ") + xstring + B("\n") +
        B("l1/") + xstring + B("_s1 STRING ") + xstring + B("1\n") +
        B("l1/") + xstring + B("_s2 STRING ") + xstring + B("2\n") +
        B("l1/") + xstring + B("_a1 CARRAY UINT64 1 2 3\n") +
        B("l1/") + xstring + B("_a2 CARRAY UINT64 1 2 3\n") +
        B("l1/") + xstring + B("_c1 CONST UINT8 1\n") +
        B("l1/") + xstring + B("_c2 CONST UINT8 2\n") +
        B("o1 LINCOM ") + xstring + B("_i ") + xstring + B("_m ") +
        xstring + B("_b\n") +
        B("o2 LINCOM ") +
        xstring + B("_i1 ") + xstring + B("_m1 ") + xstring + B("_b1 ") +
        xstring + B("_i2 ") + xstring + B("_m2 ") + xstring + B("_b2\n") +
        B("o3 LINCOM ") +
        xstring + B("_i1 ") + xstring + B("_m1 ") + xstring + B("_b1 ") +
        xstring + B("_i2 ") + xstring + B("_m2 ") + xstring + B("_b2 ") +
        xstring + B("_i3 ") + xstring + B("_m3 ") + xstring + B("_b3\n") +
        B("b1 BIT ") + xstring + B("_i ") +
        xstring + B("_bn ") + xstring + B("_nb\n") +
        B("e1 RECIP in ") + xstring + B("_dv\n") +
        B("p1 PHASE ") + xstring + B("_i ") + xstring + B("_ps\n") +
        B("m1 MPLEX a b ") + xstring + B("_cv ") + xstring + B("_pd\n") +
        B("y1 POLYNOM ") + xstring + B("_i ") + xstring + B("_y1 2 ") +
        xstring + B("_y3\n") +
        B("w1 WINDOW a b EQ ") + xstring + B("_t1\n") +
        B("w2 WINDOW a b SET ") + xstring + B("_t2\n") +
        B("w3 WINDOW a b GT ") + xstring + B("_t3\n") +
        B("d1 DIVIDE ") + xstring + B("_i1 ") + xstring + B("_i2\n")
        )
f.close()

try:
  D=pygetdata.dirfile("dirfile", pygetdata.RDONLY)
except pygetdata.DirfileError:
  CheckOK(0)

# Attempt 3: use an incorrect correct character encoding
D.character_encoding = 'utf-8'

CheckSimple(1,D.character_encoding,'utf-8')

try:
  D.validate(estring)
except pygetdata.DirfileError:
  CheckEOS(2,D.error_string,estring)

try:
  e = 1
  c = D.carrays(return_type=pygetdata.NULL)
except UnicodeDecodeError:
  e = 0

CheckExc(0)

try:
  e = 1
  c = D.constants(return_type=pygetdata.UINT8)
except UnicodeDecodeError:
  e = 0

CheckExc(1)

try:
  e = 1
  c = D.mcarrays("l1", return_type=pygetdata.NULL)
except UnicodeDecodeError:
  e = 0

CheckExc(2)

try:
  e = 1
  c = D.mconstants("l1", return_type=pygetdata.UINT8)
except UnicodeDecodeError:
  e = 0

CheckExc(2)

try:
  e = 1
  c = D.strings()
except UnicodeDecodeError:
  e = 0

CheckExc(3)

try:
  e = 1
  c = D.mstrings("l1")
except UnicodeDecodeError:
  e = 0

CheckExc(4)

try:
  e = 1
  c = D.reference
except UnicodeDecodeError:
  e = 0

CheckExc(5)

try:
  e = 1
  c = D.get_string("s1")
except UnicodeDecodeError:
  e = 0

CheckExc(6)

try:
  e = 1
  c = D.alias_target(estring + B("_al"))
except UnicodeDecodeError:
  e = 0

CheckExc(7)

c = D.entry("r2")
try:
  e = 1
  print (c.spf)
except UnicodeDecodeError:
  e = 0

CheckExc(8)
CheckParms(9,c)

# This works because LINTERP table is FS encoded
c = D.entry("l1")
CheckSimple(10,c.parameters,("in", fstring))

c = D.entry("d1")
CheckIns(11, c)
CheckParms(12,c)

c = D.entry("e1")
try:
  e = 1
  print (c.dividend)
except UnicodeDecodeError:
  e = 0

CheckExc(13)
CheckParms(14,c)

c = D.entry("p1")
try:
  e = 1
  print (c.shift)
except UnicodeDecodeError:
  e = 0

CheckExc(15)
CheckIns(16,c)
CheckParms(17,c)

c = D.entry("y1")
try:
  e = 1
  print (c.a)
except UnicodeDecodeError:
  e = 0

CheckExc(18)
CheckParms(19, c)

c = D.entry("o1")
try:
  e = 1
  print (c.m)
except UnicodeDecodeError:
  e = 0

CheckExc(20)
try:
  e = 1
  print (c.b)
except UnicodeDecodeError:
  e = 0

CheckExc(21)
CheckIns(22,c)
CheckParms(23,c)

c = D.entry("o2")
try:
  e = 1
  print (c.m)
except UnicodeDecodeError:
  e = 0

CheckExc(24)
try:
  e = 1
  print (c.b)
except UnicodeDecodeError:
  e = 0

CheckExc(25)
CheckIns(26,c)
CheckParms(27,c)

c = D.entry("o3")
try:
  e = 1
  print (c.m)
except UnicodeDecodeError:
  e = 0

CheckExc(28)
try:
  e = 1
  print (c.b)
except UnicodeDecodeError:
  e = 0

CheckExc(29)
CheckIns(30,c)
CheckParms(31,c)

c = D.entry("w1")
CheckParms(32,c)

c = D.entry("w2")
CheckParms(33,c)

c = D.entry("w3")
CheckParms(34,c)

c = D.entry("m1")
try:
  e = 1
  print (c.count_val)
except UnicodeDecodeError:
  e = 0

CheckExc(35)
try:
  e = 1
  print (c.period)
except UnicodeDecodeError:
  e = 0

CheckExc(36)
CheckParms(35,c)

D.discard()
del D
os.system("rm -rf dirfile")

if (ne > 0):
  print ("ne = ",ne)
  sys.exit(1)
  
