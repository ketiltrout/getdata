/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

#ifdef STDC_HEADERS
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

/* encoding schemas */
const struct encoding_t encode[] = {
  { GD_UNENCODED, "", 1,
    &_GD_RawOpen, &_GD_RawClose, &_GD_RawTouch, &_GD_RawSeek, &_GD_RawRead,
    &_GD_RawSize,  &_GD_RawWrite, &_GD_RawSync, &_GD_RawUnlink, NULL },
  { GD_TEXT_ENCODED, ".txt", 0,
    &_GD_AsciiOpen, &_GD_AsciiClose, &_GD_AsciiTouch, &_GD_AsciiSeek,
    &_GD_AsciiRead, &_GD_AsciiSize, &_GD_AsciiWrite, &_GD_AsciiSync,
    &_GD_AsciiUnlink, NULL },
  { GD_SLIM_ENCODED, ".slm", 1,
#ifdef USE_SLIMLIB
    &_GD_SlimOpen, &_GD_SlimClose, NULL /* TOUCH */, &_GD_SlimSeek,
    &_GD_SlimRead, &_GD_SlimSize, NULL /* WRITE */, NULL /* SYNC */,
    &_GD_SlimUnlink, NULL /* MKTEMP */
#else
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
#endif
  },
  { GD_ENC_UNSUPPORTED, "", 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL },
};

/* Figure out the encoding scheme */
static unsigned int _GD_ResolveEncoding(const char* name, unsigned int scheme,
    struct _gd_private_entry *e)
{
  char candidate[FILENAME_MAX];
  char* ptr;
  int i, len = strlen(name);
  struct stat64 statbuf;

  dtrace("\"%s\", 0x%08x, %p", name, scheme, e);

  scheme &= GD_ENCODING;
  strcpy(candidate, name);
  ptr = candidate + len;
  len = FILENAME_MAX - len;

  for (i = 0; encode[i].scheme != GD_ENC_UNSUPPORTED; i++) {
    if (scheme == GD_AUTO_ENCODED || scheme == encode[i].scheme) {
      strcpy(ptr, encode[i].ext);

      if (stat64(candidate, &statbuf) == 0) 
        if (S_ISREG(statbuf.st_mode)) {
          if (e != NULL)
            e->encoding = i;
          dreturn("%08x", encode[i].scheme);
          return encode[i].scheme;
        }
    }
  }

  if (scheme != 0 && e != NULL) {
    for (i = 0; encode[i].scheme != GD_ENC_UNSUPPORTED; i++)
      if (scheme == encode[i].scheme) {
        e->encoding = i;
        dreturn("0x%08x", encode[i].scheme);
        return encode[i].scheme;;
      }
  }

  dreturn("%08x", GD_AUTO_ENCODED);
  return GD_AUTO_ENCODED;
}

int _GD_Supports(DIRFILE* D, gd_entry_t* E, unsigned int funcs)
{
  dtrace("%p, %p, %x", D, E, funcs);

  /* Figure out the dirfile encoding type, if required */
  if ((D->fragment[E->fragment_index].flags & GD_ENCODING) == GD_AUTO_ENCODED) {
    D->fragment[E->fragment_index].flags =
      (D->fragment[E->fragment_index].flags & ~GD_ENCODING) |
      _GD_ResolveEncoding(E->e->file, GD_AUTO_ENCODED, E->e);
  }

  /* If the encoding scheme is unknown, we can't delete the field */
  if ((D->fragment[E->fragment_index].flags & GD_ENCODING) == GD_AUTO_ENCODED) {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  /* Figure out the encoding subtype, if required */
  if (E->e->encoding == GD_ENC_UNKNOWN)
    _GD_ResolveEncoding(E->e->file, D->fragment[E->fragment_index].flags, E->e);

  /* check for our function(s) */
  if ((funcs & GD_EF_OPEN   && encode[E->e->encoding].open   == NULL) ||
      (funcs & GD_EF_CLOSE  && encode[E->e->encoding].close  == NULL) ||
      (funcs & GD_EF_TOUCH  && encode[E->e->encoding].touch  == NULL) ||
      (funcs & GD_EF_SEEK   && encode[E->e->encoding].seek   == NULL) ||
      (funcs & GD_EF_READ   && encode[E->e->encoding].read   == NULL) ||
      (funcs & GD_EF_SIZE   && encode[E->e->encoding].size   == NULL) ||
      (funcs & GD_EF_WRITE  && encode[E->e->encoding].write  == NULL) ||
      (funcs & GD_EF_SYNC   && encode[E->e->encoding].sync   == NULL) ||
      (funcs & GD_EF_UNLINK && encode[E->e->encoding].unlink == NULL) ||
      (funcs & GD_EF_MKTEMP && encode[E->e->encoding].mktemp == NULL))
  {
    _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

int _GD_GenericTouch(const char* name, const char* ext)
{
  char encodedname[FILENAME_MAX];

  dtrace("\"%s\", \"%s\"", name, ext);
  snprintf(encodedname, FILENAME_MAX, "%s%s", name, ext);

  int fd = open(encodedname, O_RDWR | O_CREAT | O_TRUNC, 0666);

  if (fd != -1)
    fd = close(fd);

  dreturn("%i", fd);
  return fd;
}

int _GD_GenericUnlink(const char* name, const char* ext)
{
  char encodedname[FILENAME_MAX];

  dtrace("\"%s\", \"%s\"", name, ext);
  snprintf(encodedname, FILENAME_MAX, "%s%s", name, ext);

  int r = unlink(encodedname);

  dreturn("%i", r);
  return r;
}
/* vim: ts=2 sw=2 et tw=80
*/
