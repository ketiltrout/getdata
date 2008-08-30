/* (C) 2007, 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * checkdirfile is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with checkdirfile; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include "getdata.h"

int main(int argc, char* argv[])
{
  DIRFILE* dirfile;
  char getdata_error[2048];

  if (argc < 2) {
    printf("No dirfile.\n");
    return 1;
  }

  dirfile = dirfile_open(argv[1], GD_RDONLY);

  if (dirfile->error) {
    printf("getdata error: %s\n", getdata_error_string(dirfile, getdata_error, 2048));
    dirfile_close(dirfile);
    return 1;
  }
  off_t n = get_nframes(dirfile);

  if (dirfile->error) {
    printf("getdata error: %s\n", getdata_error_string(dirfile, getdata_error, 2048));
    dirfile_close(dirfile);
    return 1;
  }

  printf("syntax OK.  Found %lu frames\n", (unsigned long)n);
  dirfile_close(dirfile);
  return 0;
}
