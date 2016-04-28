# Copyright (C) 2009, 2011 D. V. Wiebe
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

#callback
def parser_callback(pdata, extra):
  if (extra != "extra stuff"):
    print ("extra =", extra)
    sys.exit(1)

  if (pdata["suberror"] != 8):
    print ("suberror =", pdata["suberror"])
    sys.exit(1)

  if (pdata["line"] != "bad line\n"):
    print ("line =", pdata["line"])
    sys.exit(1)

  if (pdata["linenum"] != 2):
    print ("linenum =", pdata["linenum"])
    sys.exit(1)

  if (re.search("dirfile/format$", pdata["filename"]) == None):
    print ("filename =", pdata["filename"])
    sys.exit(1)

  return pygetdata.SYNTAX_IGNORE

# create the dirfile first
data=array.array("H",range(3,7000,7))
os.system("rm -rf dirfile")
os.mkdir("dirfile")
file=open("dirfile/data", 'wb')
data.tofile(file)
file.close()

file=open("dirfile/format", "w")
file.write("data RAW UINT16 8\nbad line\n")
file.close()

d=pygetdata.dirfile("dirfile", pygetdata.RDONLY, callback=parser_callback,
    extra="extra stuff")
error=d.error

os.system("rm -rf dirfile")

if (error != pygetdata.E_OK):
  print ("error = ", error)
  sys.exit(1)
