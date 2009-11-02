/* (C) 2007, 2008, 2009 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include "getdata.h"

static int callback(gd_parser_data_t *pdata, void* ne)
{
  char getdata_error[2048];
  printf("  syntax error: %s\n", get_error_string(pdata->dirfile,
        getdata_error, 2048));

  (*(int*)ne)++;

  return GD_SYNTAX_IGNORE;
}

int main(int argc, char* argv[])
{
  DIRFILE* dirfile;
  int e, i, j;
  char getdata_error[2048];
  off_t n;
  int ne = 0;
  unsigned int nfields;
  const char **flist, **mflist;

  if (argc < 2) {
    printf("No dirfile.\n");
    return 1;
  }

  /* open the dirfile */
  puts("Checking syntax...");
  dirfile = dirfile_cbopen(argv[1], GD_RDONLY, callback, &ne);

  /* open error */
  e = get_error(dirfile);
  if (e) {
    if (e != GD_E_FORMAT) {
      printf("  getdata error: %s\n", get_error_string(dirfile, getdata_error,
            2048));
      dirfile_close(dirfile);
      return 1;
    }
  }

  if (ne > 0)
    printf("  Found %i line%s with syntax errors.\n", ne, (ne == 1) ? "" : "s");
  else
    printf("  Syntax OK.\n");

  /* Check the validity of each field defined */
  ne = 0;
  puts("\nChecking fields...");
  flist = get_field_list(dirfile);
  for (i = 0; flist[i] != NULL; ++i) {
    if (dirfile_validate(dirfile, flist[i])) {
      printf("  getdata error checking %s: %s\n", flist[i],
          get_error_string(dirfile, getdata_error, 2048));
      ne++;
    }
    mflist = get_mfield_list(dirfile, flist[i]);
    for (j = 0; mflist[j] != NULL; ++j) {
      char code[GD_MAX_LINE_LENGTH];
      snprintf(code, GD_MAX_LINE_LENGTH, "%s/%s", flist[i], mflist[j]);
      if (dirfile_validate(dirfile, code)) {
        printf("  getdata error checking %s: %s\n", code,
            get_error_string(dirfile, getdata_error, 2048));
        ne++;
      }
    }
  }

  nfields = get_nfields(dirfile);

  if (ne > 0)
    printf("  Found %i problems in %u fields.\n", ne, nfields);
  else
    printf("  No problems found in %u fields.\n", nfields);

  /* try to retrieve the number of frames in the dirfile */
  puts("\nChecking frames...");
  n = get_nframes(dirfile);

  if (get_error(dirfile)) {
    printf("  getdata error: %s\n", get_error_string(dirfile, getdata_error,
          2048));
    dirfile_close(dirfile);
    return 1;
  }

  printf("  Found %llu frames.\n", (unsigned long long)n);

  dirfile_close(dirfile);
  return 0;
}
