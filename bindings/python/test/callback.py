import sys
import os
import array
import getdata

#callback
def parser_callback(estring, suberror, line, extra):
  if (extra != "extra stuff"):
    sys.exit(1)

  if (suberror != 8):
    print "suberror = ", suberror
    sys.exit(1);

  if (line != "bad line\n"):
    print "line = ", line
    sys.exit(1);

  return getdata.SYNTAX_IGNORE;

# create the dirfile first
data=array.array("H",range(3,7000,7))
os.system("rm -rf dirfile")
os.mkdir("dirfile")
file=open("dirfile/data", 'w')
data.tofile(file)
file.close()

file=open("dirfile/format", "w")
file.write("data RAW UINT16 8\nbad line\n")
file.close()

d=getdata.dirfile("dirfile", getdata.RDONLY, callback=parser_callback,
    extra="extra stuff");
error=d.error;

if (error != getdata.E_OK):
  print "error = ", error
  sys.exit(1)
