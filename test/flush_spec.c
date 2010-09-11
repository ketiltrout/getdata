/* Check the writing of field specs */
#include "test.h"

#include <stdio.h>
#include <inttypes.h>
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
  const char* apath = __TEST__ "dirfile/a";
  const char* jpath = __TEST__ "dirfile/j";
  const char* spec[] = {
    "a RAW UINT8 1\n",
    "c CONST UINT64 1\n",
    "d CONST UINT64 2\n",
    "e LINCOM 2 a c 1 h 3 d\n",
    "f LINTERP a /lut/table\n",
    "g MULTIPLY e f\n",
    "h BIT a 2 d\n",
    "i PHASE h c\n",
    "j RAW UINT16 d\n",
    "k PHASE h 3\n",
    "l SBIT a d 2\n",
    "m POLYNOM a 1 c 2 d\n",
    "n STRING \"a b c \\x01 ÿ\"\n",
    NULL
  };

  int i = 0;

  DIRFILE* D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_TRUNC |
      GD_VERBOSE);
  for (i = 0; spec[i] != NULL; ++i)
    gd_add_spec(D, spec[i], 0);
  int error = gd_error(D);

  gd_close(D);

  FILE* stream = fopen(format, "rt");
  i = 0;
  while (!feof(stream)) {
    char line[GD_MAX_LINE_LENGTH];
    if (fgets(line, GD_MAX_LINE_LENGTH, stream) == NULL)
      break;

    if (line[0] == '/' || line[0] == '#' || line[0] < ' ')
      continue;

    if (strcmp(line, spec[i]) != 0) {
      fprintf(stderr, "%s <=> %s", spec[i], line);
      error = 1;
    }
    ++i;
  }
  fclose(stream);

  unlink(apath);
  unlink(jpath);
  unlink(format);
  rmdir(filedir);

  if (error)
    return 1;

  return 0;
}
