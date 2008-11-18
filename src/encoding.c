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
    &_GD_RawOpen, &_GD_RawClose, &_GD_GenericTouch, &_GD_RawSeek, &_GD_RawRead,
    &_GD_RawSize, &_GD_RawWrite, &_GD_RawSync, &_GD_GenericUnlink, &_GD_RawTemp
  },
  { GD_TEXT_ENCODED, ".txt", 0,
    &_GD_AsciiOpen, &_GD_AsciiClose, &_GD_GenericTouch, &_GD_AsciiSeek,
    &_GD_AsciiRead, &_GD_AsciiSize, &_GD_AsciiWrite, &_GD_AsciiSync,
    &_GD_GenericUnlink, &_GD_AsciiTemp },
  { GD_SLIM_ENCODED, ".slm", 1,
#ifdef USE_SLIMLIB
    &_GD_SlimOpen, &_GD_SlimClose, NULL /* TOUCH */, &_GD_SlimSeek,
    &_GD_SlimRead, &_GD_SlimSize, NULL /* WRITE */, NULL /* SYNC */,
    &_GD_GenericUnlink, NULL /* TEMP */
#else
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
#endif
  },
  { GD_ENC_UNSUPPORTED, "", 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL },
};

/* Figure out the encoding scheme */
static unsigned int _GD_ResolveEncoding(const char* name, unsigned int scheme,
    struct _gd_raw_file *file)
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
          if (file != NULL)
            file->encoding = i;
          dreturn("%08x", encode[i].scheme);
          return encode[i].scheme;
        }
    }
  }

  if (scheme != 0 && file != NULL) {
    for (i = 0; encode[i].scheme != GD_ENC_UNSUPPORTED; i++)
      if (scheme == encode[i].scheme) {
        file->encoding = i;
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
      _GD_ResolveEncoding(E->e->filebase, GD_AUTO_ENCODED, E->e->file);
  }

  /* If the encoding scheme is unknown, complain */
  if ((D->fragment[E->fragment_index].flags & GD_ENCODING) == GD_AUTO_ENCODED) {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  /* Figure out the encoding subtype, if required */
  if (E->e->file[0].encoding == GD_ENC_UNKNOWN)
    _GD_ResolveEncoding(E->e->filebase, D->fragment[E->fragment_index].flags,
        E->e->file);

  /* check for our function(s) */
  if ((funcs & GD_EF_OPEN   && encode[E->e->file[0].encoding].open   == NULL) ||
      (funcs & GD_EF_CLOSE  && encode[E->e->file[0].encoding].close  == NULL) ||
      (funcs & GD_EF_TOUCH  && encode[E->e->file[0].encoding].touch  == NULL) ||
      (funcs & GD_EF_SEEK   && encode[E->e->file[0].encoding].seek   == NULL) ||
      (funcs & GD_EF_READ   && encode[E->e->file[0].encoding].read   == NULL) ||
      (funcs & GD_EF_SIZE   && encode[E->e->file[0].encoding].size   == NULL) ||
      (funcs & GD_EF_WRITE  && encode[E->e->file[0].encoding].write  == NULL) ||
      (funcs & GD_EF_SYNC   && encode[E->e->file[0].encoding].sync   == NULL) ||
      (funcs & GD_EF_UNLINK && encode[E->e->file[0].encoding].unlink == NULL) ||
      (funcs & GD_EF_TEMP   && encode[E->e->file[0].encoding].temp   == NULL))
  {
    _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

int _GD_SetEncodedName(struct _gd_raw_file* file, const char* base, int temp)
{
  dtrace("%p, \"%s\", %i", file, base, temp);

  if (file->name == NULL) {
    file->name = malloc(FILENAME_MAX);
    if (file->name == NULL) {
      dreturn("%i", -1);
      return -1;
    }

    snprintf(file->name, FILENAME_MAX, "%s%s", base, temp ? "_XXXXXX" :
        encode[file->encoding].ext);
  }

  dreturn("%i", 0);
  return 0;
}

int _GD_GenericTouch(struct _gd_raw_file* file, const char* base)
{
  dtrace("%p, \"%s\"", file, base);

  if (_GD_SetEncodedName(file, base, 0)) {
    dreturn("%i", -1);
    return -1;
  }

  int fd = open(file->name, O_RDWR | O_CREAT | O_TRUNC, 0666);

  if (fd != -1)
    fd = close(fd);

  dreturn("%i", fd);
  return fd;
}

int _GD_GenericUnlink(struct _gd_raw_file* file, const char* base)
{
  dtrace("%p, \"%s\"", file, base);

  if (_GD_SetEncodedName(file, base, 0)) {
    dreturn("%i", -1);
    return -1;
  }

  int r = unlink(file->name);

  dreturn("%i", r);
  return r;
}
/* vim: ts=2 sw=2 et tw=80
*/
