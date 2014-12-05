/* Copyright (C) 2013 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#define GOOD  "z\\x1y\\xcx\\xDw\\x0eE\\x0Ff\\x10g\\x1ah\\x1Ei"
#define GOODs "z\x1y\xcx\xDw\x0e" "E\x0F" "f\x10g\x1ah\x1Ei"
#define BAD0  "a\\x00g"
#define BAD1  "a\\xg"
static const char *goods = GOODs;

static int callback(gd_parser_data_t *p, void *extra)
{
  int *le = (int*)extra;
  le[p->linenum - 1] = 1;
  return GD_SYNTAX_IGNORE;
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data =
    "good STRING " GOOD "\n"
    "bad0 STRING " BAD0 "\n"
    "bad1 STRING " BAD1 "\n";
  int fd, error, r = 0;
  int le[3] = {0, 0, 0};
  char s[100];
  size_t i;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_cbopen(filedir, GD_RDONLY, callback, le);
  error = gd_error(D);
  gd_get_string(D, "good", 100, s);
  gd_discard(D);

  CHECKI(le[0], 0);
  CHECKI(le[1], 1);
  CHECKI(le[2], 1);

  for (i = 0; i < sizeof(GOODs); ++i)
    CHECKIi(i, s[i], goods[i]);

  unlink(format);
  rmdir(filedir);

  CHECKI(error,GD_E_OK);
  return r;
}
