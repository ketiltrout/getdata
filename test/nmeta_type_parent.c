/* Retreiving the number of fields of a field should succeed cleanly */
#include "../src/getdata.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* format_data =
    "raw1 RAW UINT8 1\n"
    "META raw1 linterp1 LINTERP raw2 table\n"
    "META raw1 linterp2 LINTERP raw3 table\n"
    "META raw1 linterp3 LINTERP raw4 table\n"
    "META raw1 const CONST UINT8 1\n"
    "META raw1 string STRING value\n"
    "META raw1 string2 STRING value\n"
    "raw2 RAW UINT8 1\n"
    "raw3 RAW UINT8 1\n"
    "raw4 RAW UINT8 1\n"
    "const CONST UINT8 1\n"
    "string STRING value\n"
    "string2 STRING value\n";
  int fd;

  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  DIRFILE* D = dirfile_open(filedir, GD_RDONLY);
  unsigned int nfields = get_nmeta_fields_by_type(D, "raw9", GD_STRING_ENTRY);
  int error = get_error(D);
  dirfile_close(D);

  unlink(format);
  rmdir(filedir);

  return (error != GD_E_BAD_CODE);
}
