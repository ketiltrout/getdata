#include "getdata/dirfile.h"
#include "getdata/entry.h"

#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>

using namespace GetData;

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* data = __TEST__ "dirfile/data";
  unsigned char c[8];

  memset(c, 0, 8);

  Dirfile dirfile = Dirfile(filedir, GD_CREAT | GD_RDWR | GD_UNENCODED);
  RawEntry entry = RawEntry("data", UInt8, 8);
  dirfile.Add(entry);
  int error = dirfile.Error();
  int n = dirfile.NFields();

  unlink(data);
  unlink(format);
  rmdir(filedir);

  if (strcmp(entry.Code(), "data") != 0)
    return 1;

  if (entry.Type() != RawEntryType)
    return 1;

  if (entry.SamplesPerFrame() != 8)
    return 1;

  if (entry.RawType() != UInt8)
    return 1;

  if (error)
    return 1;

  if (n != 2)
    return 1;

  return 0;
}
