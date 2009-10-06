import sys
import os
import array
import pygetdata

#callback
def parser_callback(pdata, extra):
  if (extra != "extra stuff"):
    print "extra =", extra;
    sys.exit(1)

  if (pdata["suberror"] != 8):
    print "suberror =", pdata["suberror"]
    sys.exit(1);

  if (pdata["line"] != "bad line\n"):
    print "line =", pdata["line"]
    sys.exit(1);

  if (pdata["linenum"] != 2):
    print "linenum =", pdata["linenum"]
    sys.exit(1);

  if (pdata["filename"] != "dirfile/format"):
    print "filename =", pdata["filename"]
    sys.exit(1);

  return pygetdata.SYNTAX_IGNORE;

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

d=pygetdata.dirfile("dirfile", pygetdata.RDONLY, callback=parser_callback,
    extra="extra stuff");
error=d.error;

os.system("rm -rf dirfile")

if (error != pygetdata.E_OK):
  print "error = ", error
  sys.exit(1)
