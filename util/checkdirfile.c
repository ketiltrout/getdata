/* Copyright (C) 2007-2010, 2012, 2014-2017 D. V. Wiebe
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
#include "gd_config.h"
#endif

#ifndef HAVE_SNPRINTF
#ifdef HAVE__SNPRINTF
#define snprintf _snprintf
#endif
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "getdata.h"

#ifndef PACKAGE_STRING
#define PACKAGE_STRING PACKAGE_NAME " " PACKAGE_VERSION
#endif

static int callback(gd_parser_data_t *pdata, void* ne)
{
  char getdata_error[2048];
  printf("  syntax error: %s\n", gd_error_string(pdata->dirfile,
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
  unsigned int nfields = 0;
  const char **flist, **mflist;

  if (argc < 2 || !strcmp(argv[1], "--version") || !strcmp(argv[1], "--help")) {
    printf("Usage:\n"
        "  checkdirfile DIRFILE                 Check the Dirfile database "
        "DIRFILE for\n"
        "                                         errors.\n"
        "  checkdirfile [ --help | --version ]  Print this message and exit.\n"
        "\n\n"
        "This program is part of %s.\n"
        "Copyright (C) 2008-2010, 2012, 2014-2017  D. V. Wiebe\n"
        "Please send reports of bugs and other communication to:\n\n  %s\n\n"
        "This program comes with NO WARRANTY, not even for MERCHANTABILITY "
        "or FITNESS\n"
        "FOR A PARTICULAR PURPOSE. You may redistribute it under the terms of "
        "the GNU\n"
        "Lesser General Public License, either version 2.1 of the License, or "
        "(at your\n"
        "option) any later version.\n\n"
        "You should have received a copy of the GNU Lesser General Public "
        "License along\n"
        "with this program; if not, write to the Free Software Foundation,"
        "Inc.,\n"
        "51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA\n",
        PACKAGE_STRING, PACKAGE_BUGREPORT);
    return 1;
  }

  /* open the dirfile -- the callback will print syntax errors as
   * found, and return the number of lines with errors in ne. */
  puts("Checking syntax...");
  dirfile = gd_cbopen(argv[1], GD_RDONLY, callback, &ne);

  /* open error */
  e = gd_error(dirfile);
  if (e) {
    if (e != GD_E_FORMAT) {
      printf("  getdata error: %s\n", gd_error_string(dirfile, getdata_error,
            2048));
      gd_close(dirfile);
      return 1;
    }
  }

  if (ne > 0)
    printf("  Found %i %s with syntax errors.\n", ne,
        (ne == 1) ? "line" : "lines");
  else {
    int vers[GD_DIRFILE_STANDARDS_VERSION + 1];
    int nvers = 0;
    int first = -1;
    int start = 1;

    printf("  Syntax OK.\n\n");

    /* Run through every known standards version and check whether the dirfile
     * is conformant by trying to set the loaded dirfile's version to that
     * value
     */
    for (i = 0; i <= GD_DIRFILE_STANDARDS_VERSION; ++i) {
      if (gd_dirfile_standards(dirfile, i) == i) {
        vers[i] = 1;
        nvers++;
      } else
        vers[i] = 0;
    }

    if (nvers == 0) {
      puts("WARNING: Dirfile conforms to no Standards Version.");
    } else {
      printf("Dirfile conforms to Standards %s ",
          (nvers == 1) ? "Version" : "Versions");

      /* pretty-print the list of conformed versions */
      for (i = 0; i <= GD_DIRFILE_STANDARDS_VERSION; ++i) {
        if (vers[i]) {
          if (first == -1)
            first = i;
        } else if (first != -1) {
          if (!start)
            fputs(", ", stdout);
          else
            start = 0;

          if (first == i)
            printf("%i", i);
          else if (first + 1 == i)
            printf("%i, %i", first, i);
          else
            printf("%i-%i", first, i);

          first = -1;
        }
      }

      if (first != -1) {
        if (!start)
          fputs(", ", stdout);

        if (first == GD_DIRFILE_STANDARDS_VERSION)
          printf("%i", GD_DIRFILE_STANDARDS_VERSION);
        else if (first + 1 == GD_DIRFILE_STANDARDS_VERSION)
          printf("%i, %i", first, GD_DIRFILE_STANDARDS_VERSION);
        else
          printf("%i-%i", first, GD_DIRFILE_STANDARDS_VERSION);

        fputs(" (the latest version)", stdout);
      }

      puts("");
    }

  }

  /* Check the validity of each entry defined */
  ne = 0;
  puts("\nChecking fields...");
  flist = gd_entry_list(dirfile, NULL, 0,
      GD_ENTRIES_HIDDEN | GD_ENTRIES_NOALIAS);
  for (i = 0; flist[i] != NULL; ++i) {
    if (gd_validate(dirfile, flist[i])) {
      printf("  getdata error checking %s: %s\n", flist[i],
          gd_error_string(dirfile, getdata_error, 2048));
      ne++;
    }
    nfields++;
    mflist = gd_entry_list(dirfile, flist[i], 0,
        GD_ENTRIES_HIDDEN | GD_ENTRIES_NOALIAS);
    for (j = 0; mflist[j] != NULL; ++j) {
      char code[GD_MAX_LINE_LENGTH];
      snprintf(code, GD_MAX_LINE_LENGTH, "%s/%s", flist[i], mflist[j]);
      if (gd_validate(dirfile, code)) {
        printf("  getdata error checking %s: %s\n", code,
            gd_error_string(dirfile, getdata_error, 2048));
        ne++;
      }
      nfields++;
    }

    /* ferret out dangling meta ALIASes by first collecting a list
     * of all of them, and then trying to use them as field codes */
    mflist = gd_entry_list(dirfile, flist[i], GD_ALIAS_ENTRIES,
        GD_ENTRIES_HIDDEN);
    for (j = 0; mflist[j] != NULL; ++j) {
      char code[GD_MAX_LINE_LENGTH];
      snprintf(code, GD_MAX_LINE_LENGTH, "%s/%s", flist[i], mflist[j]);
      if (gd_entry_type(dirfile, code) == GD_NO_ENTRY) {
        if (gd_error(dirfile) == GD_E_BAD_CODE) {
          printf("  dangling alias %s pointing to non-existent %s\n", code,
              gd_alias_target(dirfile, code));
        } else {
          printf("  getdata error checking alias %s: %s\n", code,
              gd_error_string(dirfile, getdata_error, 2048));
        }
        ne++;
      }
      nfields++;
    }
  }

  /* ferret out dangling ALIASes by first collecting a list
   * of all of them, and then trying to use them as field codes */
  flist = gd_entry_list(dirfile, NULL, GD_ALIAS_ENTRIES, GD_ENTRIES_HIDDEN);
  for (i = 0; flist[i] != NULL; ++i) {
    if (gd_entry_type(dirfile, flist[i]) == GD_NO_ENTRY) {
      if (gd_error(dirfile) == GD_E_BAD_CODE) {
        printf("  dangling alias %s pointing to non-existent %s\n", flist[i],
            gd_alias_target(dirfile, flist[i]));
      } else {
        printf("  getdata error checking alias %s: %s\n", flist[i],
            gd_error_string(dirfile, getdata_error, 2048));
      }
      ne++;
    }
    nfields++;
  }

  if (ne > 0)
    printf("  Found %i problems in %u %s.\n", ne, nfields,
        nfields > 1 ? "entries" : "entry");
  else
    printf("  No problems found in %u %s.\n", nfields,
        nfields > 1 ? "entries" : "entry");

  /* try to retrieve the number of frames in the dirfile */
  puts("\nChecking frames...");
  n = gd_nframes(dirfile);

  if (gd_error(dirfile)) {
    printf("  getdata error: %s\n", gd_error_string(dirfile, getdata_error,
          2048));
    gd_close(dirfile);
    return 1;
  }

  printf("  Found %" PRIu64 " %s.\n", (uint64_t)n, n == 1 ? "frame" : "frames");

  gd_close(dirfile);
  return 0;
}
