import sys
import os
import array
import getdata

# create the dirfile first
data=array.array("H",range(3,7000,7))
os.system("rm -rf dirfile")
os.mkdir("dirfile")
file=open("dirfile/data", 'w')
data.tofile(file)
file.close()

file=open("dirfile/format", "w")
file.write("data RAW UINT16 8\n")
file.close()

d=getdata.dirfile("dirfile", getdata.RDONLY | getdata.VERBOSE);
data=d.getdata("data", getdata.LONG, first_frame=4, first_sample=5,
    num_frames=1)

if (len(data) != 8):
  sys.exit(1)

for i in range(8):
  if (data[i] != 262 + 7 * i):
    sys.exit(1)
