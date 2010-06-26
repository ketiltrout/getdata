/* (C) 2010 D. V. Wiebe
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
#include "internal.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#endif

/* The MSVCRT gmtime() is threadsafe */
#ifndef HAVE_GMTIME_R
struct tm *gmtime_r(const time_t *timep, struct tm *result)
{
  dtrace("%p, %p", timep, result);

  struct tm *ptr = gmtime(timep);
  if (!ptr)
    return NULL;

  memcpy(result, ptr, sizeof(struct tm));

  dreturn("%p", result);
  return result;
}
#endif

/* MSVCRT based implementation of mkstemp(3). */
#ifndef HAVE_MKSTEMP
int mkstemp(char *template) {
  int ret = -1;

  dtrace("\"%s\"", template);

  char *template_template = strdup(template);

  /* for sanity's sake */
  errno = 0;

  /* loop while open returns EEXIST */
  do {
    strcpy(template, template_template);
    char* ptr = mktemp(template);

    if (ptr == NULL)
      errno = EINVAL;
    else
      ret = open(ptr, O_RDWR | O_BINARY | O_CREAT | O_EXCL | O_BINARY, 0600);
  } while (errno == EEXIST);

  free(template_template);
  dreturn("%i", ret);
  return ret;
}
#endif

/* An overwriting rename for use with the MSVCRT */
#ifdef __MSVCRT__
int _GD_Rename(const char *oldname, const char *newname)
{
  dtrace("\"%s\", \"%s\"", oldname, newname);

  if (unlink(newname)) {
    if (errno != ENOENT) {
      dreturn("%i", -1);
      return -1;
    }
  }

  int ret = rename(oldname, newname);

  dreturn("%i", ret);
  return ret;
}
#endif

/* Non-threadsafe version of strerror_r */
#ifndef HAVE_STRERROR_R
int strerror_r(int errnum, char *buf, size_t buflen)
{
  dtrace("%i, %p, %zu", errnum, buf, buflen);
  
  char *ptr = strerror(errnum);
  strncpy(buf, ptr, buflen);

  return 0;
}
#endif
