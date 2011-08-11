/* Copyright (C) 2008-2011 D. V. Wiebe
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
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

#ifdef USE_MODULES
#ifdef USE_PTHREAD
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
static pthread_mutex_t _gd_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

static int framework_initialised = 0;
#endif

/* encoding schemas */
#define GD_EF_NULL_SET NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, \
  &_GD_GenericMove, &_GD_GenericUnlink, NULL, &_GD_GenericTMove, NULL
#ifdef USE_MODULES
#define GD_EXT_ENCODING(sc,ex,ec,af,ff) \
{ sc,ex,ec,af,ff,GD_EF_PROVIDES,GD_EF_NULL_SET }
#else
#define GD_EXT_ENCODING(sc,ex,ec,af,ff) { sc,ex,ec,af,ff,0,GD_INT_FUNCS }
#endif
struct encoding_t _gd_ef[GD_N_SUBENCODINGS] = {
  { GD_UNENCODED, "", 1, NULL, "none", 0,
    &_GD_RawOpen, &_GD_RawClose, &_GD_GenericTouch, &_GD_RawSeek, &_GD_RawRead,
    &_GD_RawSize, &_GD_RawWrite, &_GD_RawSync, &_GD_GenericMove,
    &_GD_GenericUnlink, &_GD_RawTOpen, &_GD_GenericTMove, &_GD_RawTUnlink
  },

#ifdef USE_GZIP
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE
#define GD_INT_FUNCS \
  &_GD_GzipOpen, &_GD_GzipClose, NULL /* TOUCH */, &_GD_GzipSeek, \
  &_GD_GzipRead, &_GD_GzipSize, NULL /* WRITE */, NULL /* SYNC */, \
  &_GD_GenericMove, &_GD_GenericUnlink, NULL /* TOPEN */, &_GD_GenericTMove, \
  NULL /* TUNLINK */
#else
#define GD_EF_PROVIDES 0
#define GD_INT_FUNCS GD_EF_NULL_SET
#endif
  GD_EXT_ENCODING(GD_GZIP_ENCODED, ".gz", 1, "Gzip", "gzip"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_BZIP2
#define GD_EF_PROVIDES \
    GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE,
#define GD_INT_FUNCS \
  &_GD_Bzip2Open, &_GD_Bzip2Close, NULL /* TOUCH */, &_GD_Bzip2Seek, \
  &_GD_Bzip2Read, &_GD_Bzip2Size, NULL /* WRITE */, NULL /* SYNC */, \
  &_GD_GenericMove, &_GD_GenericUnlink, NULL /* TOPEN */, &_GD_GenericTMove, \
  NULL /* TUNLINK */
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING(GD_BZIP2_ENCODED, ".bz2", 1, "Bzip2", "bzip2"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_SLIM
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE,
#define GD_INT_FUNCS \
  &_GD_SlimOpen, &_GD_SlimClose, NULL /* TOUCH */, &_GD_SlimSeek, \
  &_GD_SlimRead, &_GD_SlimSize, NULL /* WRITE */, NULL /* SYNC */, \
  &_GD_GenericMove, &_GD_GenericUnlink, NULL /* TOPEN */, &_GD_GenericTMove, \
  NULL /* TUNLINK */
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING(GD_SLIM_ENCODED, ".slm", 1, "Slim", "slim"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_LZMA
#define GD_EF_PROVIDES \
    GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE,
#define GD_INT_FUNCS \
  &_GD_LzmaOpen, &_GD_LzmaClose, NULL /* TOUCH */, &_GD_LzmaSeek, \
  &_GD_LzmaRead, &_GD_LzmaSize, NULL /* WRITE */, NULL /* SYNC */, \
  &_GD_GenericMove, &_GD_GenericUnlink, NULL /* TOPEN */, &_GD_GenericTMove, \
  NULL /* TUNLINK */
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING(GD_LZMA_ENCODED, ".xz", 1, "Lzma", "lzma"),
  GD_EXT_ENCODING(GD_LZMA_ENCODED, ".lzma", 1, "Lzma", "lzma"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


  { GD_TEXT_ENCODED, ".txt", 0, NULL, "text", 0,
    &_GD_AsciiOpen, &_GD_AsciiClose, &_GD_GenericTouch, &_GD_AsciiSeek,
    &_GD_AsciiRead, &_GD_AsciiSize, &_GD_AsciiWrite, &_GD_AsciiSync,
    &_GD_GenericMove, &_GD_GenericUnlink, &_GD_AsciiTOpen, &_GD_GenericTMove,
    &_GD_AsciiTUnlink },

  { GD_SIE_ENCODED, ".sie", 0, NULL, "sie", 0,
    &_GD_SampIndOpen, &_GD_SampIndClose, &_GD_GenericTouch, &_GD_SampIndSeek,
    &_GD_SampIndRead, &_GD_SampIndSize, &_GD_SampIndWrite, &_GD_SampIndSync,
    &_GD_GenericMove, &_GD_GenericUnlink, &_GD_SampIndTOpen, &_GD_GenericTMove,
    &_GD_SampIndTUnlink },

  { GD_ENC_UNSUPPORTED, "", 0, "", "", 0,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
  },
};

void _GD_InitialiseFramework(void)
{
  dtracevoid();
#ifdef USE_MODULES
#ifdef USE_PTHREAD
  pthread_mutex_lock(&_gd_mutex);
#endif
  if (!framework_initialised) {
    framework_initialised = 1;
    lt_dlinit();
  }
#ifdef USE_PTHREAD
  pthread_mutex_unlock(&_gd_mutex);
#endif
#endif
  dreturnvoid();
}

#define _GD_EncodingUnderstood(encoding) \
  ((encoding == GD_UNENCODED || encoding == GD_SLIM_ENCODED || \
    encoding == GD_GZIP_ENCODED || encoding == GD_BZIP2_ENCODED || \
    encoding == GD_TEXT_ENCODED || encoding == GD_LZMA_ENCODED || \
    encoding == GD_SIE_ENCODED))

#ifdef USE_MODULES
static void* _GD_ResolveSymbol(lt_dlhandle lib, struct encoding_t* enc,
    const char* name)
{
  void* func;
  char symbol[100];

  dtrace("%p, %p, \"%s\"", lib, enc, name);
  /* create the symbol name */
  sprintf(symbol, "libgetdata%s_LTX_GD_%s%s", enc->affix, enc->affix, name);
  symbol[10] -= 'A' - 'a';
  func = lt_dlsym(lib, symbol);

  dreturn("%p", func);
  return func;
}
#endif

int _GD_MissingFramework(int encoding, unsigned int funcs)
{
  int ret;

  dtrace("%i, %x", encoding, funcs);

#ifdef USE_MODULES
#ifdef USE_PTHREAD
  pthread_mutex_lock(&_gd_mutex);
#endif

  /* set up the encoding library if required */
  if (_gd_ef[encoding].provides) {
    char *library;
    lt_dlhandle lib;

    /* make the library name */
    library = (char *)malloc(sizeof(GETDATA_MODULEDIR) +
        strlen(_gd_ef[encoding].affix) + sizeof(PACKAGE_VERSION) + 13);
    if (!library) {
      _gd_ef[encoding].provides = 0;
#ifdef USE_PTHREAD
      pthread_mutex_unlock(&_gd_mutex);
#endif
      dreturn("%i", 1);
      return 1;
    }

    strcat(strcat(strcpy(library, GETDATA_MODULEDIR "/libgetdata"),
          _gd_ef[encoding].affix), "-" PACKAGE_VERSION);
    library[sizeof(GETDATA_MODULEDIR) + 10] -= 'A' - 'a';

    /* open */
    if ((lib = lt_dlopenext(library)) == NULL) {
      /* if that didn't work, look for it in the search path */
      if ((lib = lt_dlopenext(library + sizeof(GETDATA_MODULEDIR))) == NULL)
      {
        free(library);
        _gd_ef[encoding].provides = 0;
#ifdef USE_PTHREAD
        pthread_mutex_unlock(&_gd_mutex);
#endif
        dreturn("%i", 1);
        return 1;
      }
    }
    free(library);

    /* Try to resolve the symbols */
    if (_gd_ef[encoding].provides & GD_EF_OPEN)
      _gd_ef[encoding].open = (int (*)(int, struct _gd_raw_file*, int,
            int))_GD_ResolveSymbol(lib, _gd_ef + encoding, "Open");
    if (_gd_ef[encoding].provides & GD_EF_CLOSE)
      _gd_ef[encoding].close =
        (int (*)(struct _gd_raw_file*))_GD_ResolveSymbol(lib, _gd_ef + encoding,
            "Close");
    if (_gd_ef[encoding].provides & GD_EF_TOUCH)
      _gd_ef[encoding].touch = (int (*)(int, struct _gd_raw_file*))
        _GD_ResolveSymbol(lib, _gd_ef + encoding, "Touch");
    if (_gd_ef[encoding].provides & GD_EF_SEEK)
      _gd_ef[encoding].seek = (off64_t (*)(struct _gd_raw_file*, off64_t,
            gd_type_t, int))_GD_ResolveSymbol(lib, _gd_ef + encoding, "Seek");
    if (_gd_ef[encoding].provides & GD_EF_READ)
      _gd_ef[encoding].read = (ssize_t (*)(struct _gd_raw_file*, void*,
            gd_type_t, size_t))_GD_ResolveSymbol(lib, _gd_ef + encoding,
            "Read");
    if (_gd_ef[encoding].provides & GD_EF_SIZE)
      _gd_ef[encoding].size = (off64_t (*)(int, struct _gd_raw_file*,
            gd_type_t))_GD_ResolveSymbol(lib, _gd_ef + encoding, "Size");
    if (_gd_ef[encoding].provides & GD_EF_WRITE)
      _gd_ef[encoding].write = (ssize_t (*)(struct _gd_raw_file*, const void*,
            gd_type_t, size_t))_GD_ResolveSymbol(lib, _gd_ef + encoding,
            "Write");
    if (_gd_ef[encoding].provides & GD_EF_SYNC)
      _gd_ef[encoding].sync =
        (int (*)(struct _gd_raw_file*))_GD_ResolveSymbol(lib, _gd_ef + encoding,
            "Sync");
    if (_gd_ef[encoding].provides & GD_EF_UNLINK)
      _gd_ef[encoding].unlink = (int (*)(int,
            struct _gd_raw_file*))_GD_ResolveSymbol(lib, _gd_ef + encoding,
            "Unlink");
    if (_gd_ef[encoding].provides & GD_EF_TEMP)
      _gd_ef[encoding].temp = (int (*)(int, int, struct _gd_raw_file*, int,
            int))_GD_ResolveSymbol(lib, _gd_ef + encoding, "Temp");
    if (_gd_ef[encoding].provides & GD_EF_TOPEN)
      _gd_ef[encoding].topen = (gd_ef_topen_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "TOpen");

    /* we tried our best, don't bother trying again */
    _gd_ef[encoding].provides = 0;
  }
#ifdef USE_PTHREAD
  pthread_mutex_unlock(&_gd_mutex);
#endif
#endif

  ret =
    (funcs & GD_EF_OPEN    && _gd_ef[encoding].open    == NULL) ||
    (funcs & GD_EF_CLOSE   && _gd_ef[encoding].close   == NULL) ||
    (funcs & GD_EF_TOUCH   && _gd_ef[encoding].touch   == NULL) ||
    (funcs & GD_EF_SEEK    && _gd_ef[encoding].seek    == NULL) ||
    (funcs & GD_EF_READ    && _gd_ef[encoding].read    == NULL) ||
    (funcs & GD_EF_SIZE    && _gd_ef[encoding].size    == NULL) ||
    (funcs & GD_EF_WRITE   && _gd_ef[encoding].write   == NULL) ||
    (funcs & GD_EF_SYNC    && _gd_ef[encoding].sync    == NULL) ||
    (funcs & GD_EF_UNLINK  && _gd_ef[encoding].unlink  == NULL) ||
    (funcs & GD_EF_TOPEN   && _gd_ef[encoding].topen   == NULL) ||
    (funcs & GD_EF_TMOVE   && _gd_ef[encoding].tmove   == NULL) ||
    (funcs & GD_EF_TUNLINK && _gd_ef[encoding].tunlink == NULL);

  dreturn("%i", ret);
  return ret;
}

/* Figure out the encoding scheme */
static unsigned long _GD_ResolveEncoding(const DIRFILE *D gd_unused_d,
    const char* name, unsigned long scheme, int dirfd,
    struct _gd_raw_file *file)
{
  char *candidate;
  int i;
  const size_t len = strlen(name);
  struct stat statbuf;

  dtrace("%p, \"%s\", 0x%08lx, %i, %p", D, name, scheme, dirfd, file);

  for (i = 0; _gd_ef[i].scheme != GD_ENC_UNSUPPORTED; i++) {
    if (scheme == GD_AUTO_ENCODED || scheme == _gd_ef[i].scheme) {
      candidate = (char *)malloc(len + strlen(_gd_ef[i].ext) + 1);
      strcat(strcpy(candidate, name), _gd_ef[i].ext);

      if (gd_StatAt(D, dirfd, candidate, &statbuf, 0) == 0) 
        if (S_ISREG(statbuf.st_mode)) {
          if (file != NULL)
            file->subenc = i;
          free(candidate);
          dreturn("%08lx", _gd_ef[i].scheme);
          return _gd_ef[i].scheme;
        }
      free(candidate);
    }
  }

  if (scheme != 0 && file != NULL) {
    for (i = 0; _gd_ef[i].scheme != GD_ENC_UNSUPPORTED; i++)
      if (scheme == _gd_ef[i].scheme) {
        file->subenc = i;
        dreturn("0x%08lx", _gd_ef[i].scheme);
        return _gd_ef[i].scheme;;
      }
  }

  dreturn("%08lx", (unsigned long)GD_AUTO_ENCODED);
  return GD_AUTO_ENCODED;
}

int _GD_Supports(DIRFILE* D, gd_entry_t* E, unsigned int funcs)
{
  dtrace("%p, %p, %x", D, E, funcs);

  /* Figure out the dirfile encoding type, if required */
  if (D->fragment[E->fragment_index].encoding == GD_AUTO_ENCODED) {
    D->fragment[E->fragment_index].encoding =
      _GD_ResolveEncoding(D, E->e->u.raw.filebase, GD_AUTO_ENCODED,
          D->fragment[E->fragment_index].dirfd, E->e->u.raw.file);
  }

  /* If the encoding scheme is unknown, complain */
  if (D->fragment[E->fragment_index].encoding == GD_AUTO_ENCODED) {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, GD_E_UNENC_UNDET, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  /* Figure out the encoding subtype, if required */
  if (E->e->u.raw.file[0].subenc == GD_ENC_UNKNOWN)
    _GD_ResolveEncoding(D, E->e->u.raw.filebase,
        D->fragment[E->fragment_index].encoding,
        D->fragment[E->fragment_index].dirfd, E->e->u.raw.file);

  /* check for our function(s) */
  if (_GD_MissingFramework(E->e->u.raw.file[0].subenc, funcs)) {
    _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

int _GD_SetEncodedName(DIRFILE* D, struct _gd_raw_file* file, const char* base,
    int temp)
{
  dtrace("%p, %p, \"%s\", %i", D, file, base, temp);

  if (file->name == NULL) {
    file->D = D;
    file->name = (char *)malloc(strlen(base) + (temp ? 8 :
          strlen(_gd_ef[file->subenc].ext) + 1));
    if (file->name == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }

    strcat(strcpy(file->name, base), temp ? "_XXXXXX" :
        _gd_ef[file->subenc].ext);
  }

  dreturn("%i (%s)", 0, file->name);
  return 0;
}

static void _GD_RecodeFragment(DIRFILE* D, unsigned long encoding, int fragment,
    int move)
{
  unsigned int i, n_raw = 0;

  dtrace("%p, %lx, %i, %i", D, encoding, fragment, move);

  /* check protection */
  if (D->fragment[fragment].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment].cname);
    dreturnvoid();
    return;
  }

  if (move && encoding != D->fragment[fragment].encoding) {
    gd_entry_t **raw_entry = (gd_entry_t **)malloc(sizeof(gd_entry_t*) *
        D->n_entries);

    if (raw_entry == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturnvoid();
      return;
    }

    /* Because it may fail, the move must occur out-of-place and then be copied
     * back over the affected files once success is assured */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        if (!_GD_Supports(D, D->entry[i], GD_EF_UNLINK))
          continue;

        /* add this raw field to the list */
        raw_entry[n_raw++] = D->entry[i];

        if (_GD_MogrifyFile(D, D->entry[i], encoding,
              D->fragment[D->entry[i]->fragment_index].byte_sex,
              D->fragment[D->entry[i]->fragment_index].frame_offset, 0, -1,
              NULL))
          break;
      }

    /* If successful, move the temporary file over the old file, otherwise
     * remove the temporary files */
    if (D->error) {
      for (i = 0; i < n_raw; ++i)
        if (!_GD_MissingFramework(raw_entry[i]->e->u.raw.file[1].subenc,
              GD_EF_TUNLINK) && 
            (*_gd_ef[raw_entry[i]->e->u.raw.file[1].subenc].tunlink)(
              D->fragment[fragment].dirfd, raw_entry[i]->e->u.raw.file + 1))
        {
          _GD_SetError(D, GD_E_RAW_IO, 0, raw_entry[i]->e->u.raw.file[0].name,
              errno, NULL);
        }
    } else 
      for (i = 0; i < n_raw; ++i) {
        struct _gd_raw_file temp;
        memcpy(&temp, raw_entry[i]->e->u.raw.file, sizeof(temp));

        raw_entry[i]->e->u.raw.file[0].name = NULL;
        raw_entry[i]->e->u.raw.file[0].subenc =
          raw_entry[i]->e->u.raw.file[1].subenc;

        if (_GD_SetEncodedName(D, raw_entry[i]->e->u.raw.file,
              raw_entry[i]->e->u.raw.filebase, 0))
        {
          raw_entry[i]->e->u.raw.file[0].name = temp.name;
          raw_entry[i]->e->u.raw.file[0].subenc = temp.subenc;
        } else if ((*_gd_ef[raw_entry[i]->e->u.raw.file[1].subenc].tmove)(
              D->fragment[fragment].dirfd, D->fragment[fragment].dirfd,
              raw_entry[i]->e->u.raw.file,
              _gd_ef[raw_entry[i]->e->u.raw.file[1].subenc].tunlink))
        {
          _GD_SetError(D, GD_E_UNCLEAN_DB, 0,
              D->fragment[D->entry[i]->fragment_index].cname, 0, NULL);
          D->flags |= GD_INVALID;
          raw_entry[i]->e->u.raw.file[0].name = temp.name;
          raw_entry[i]->e->u.raw.file[0].subenc = temp.subenc;
        } else if ((*_gd_ef[temp.subenc].unlink)(D->fragment[fragment].dirfd,
              &temp))
        {
          _GD_SetError(D, GD_E_UNCLEAN_DB, 0,
              D->fragment[D->entry[i]->fragment_index].cname, 0, NULL);
          D->flags |= GD_INVALID;
          raw_entry[i]->e->u.raw.file[0].name = temp.name;
          raw_entry[i]->e->u.raw.file[0].subenc = temp.subenc;
        } else
          free(temp.name);
      }

    free(raw_entry);

    if (D->error) {
      dreturnvoid();
      return;
    }
  } else {
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        /* close the old file */
        if (D->entry[i]->e->u.raw.file[0].idata != -1 &&
            (*_gd_ef[D->entry[i]->e->u.raw.file[0].subenc].close)(
              D->entry[i]->e->u.raw.file))
        {
          _GD_SetError(D, GD_E_RAW_IO, 0, D->entry[i]->e->u.raw.file[1].name,
              errno, NULL);
          break;
        }
        /* reset encoding subscheme. */
        D->entry[i]->e->u.raw.file[0].subenc = GD_ENC_UNKNOWN;
      }
  }

  D->fragment[fragment].encoding = encoding;
  D->fragment[fragment].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  dreturnvoid();
}

int gd_alter_encoding(DIRFILE* D, unsigned long encoding, int fragment,
    int move)
{
  int i;

  dtrace("%p, %lu, %i, %i", D, (unsigned long)encoding, fragment, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (!_GD_EncodingUnderstood(encoding)) {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, GD_E_UNENC_TARGET, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i) {
      _GD_RecodeFragment(D, encoding, i, move);

      if (D->error)
        break;
    }
  } else
    _GD_RecodeFragment(D, encoding, fragment, move);

  dreturn("%i", (D->error) ? -1 : 0);
  return (D->error) ? -1 : 0;
}

unsigned long gd_encoding(DIRFILE* D, int fragment) gd_nothrow
{
  unsigned long reported_encoding = GD_ENC_UNSUPPORTED;
  unsigned int i;

  dtrace("%p, %i", D, fragment);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if (fragment < 0 || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  /* Attempt to figure out the encoding, if it's not known */
  if (D->fragment[fragment].encoding == GD_AUTO_ENCODED) {
    /* locate a RAW field in this fragment */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        D->fragment[fragment].encoding =
          _GD_ResolveEncoding(D, D->entry[i]->e->u.raw.filebase,
              GD_AUTO_ENCODED, D->fragment[fragment].dirfd,
              D->entry[i]->e->u.raw.file);

        if (D->fragment[fragment].encoding != GD_AUTO_ENCODED)
          break;
      }
  }

  if (D->fragment[fragment].encoding != GD_AUTO_ENCODED)
    reported_encoding = D->fragment[fragment].encoding;

  dreturn("%lx", (unsigned long)reported_encoding);
  return reported_encoding;
}

/* This is basically the non-existant POSIX funcion mkstempat.  There are two
 * approaches we could take here:
 * 1) fchdir to dirfd, use mkstemp to grab a file descriptor; fchdir back to
 *    cwd, but this isn't thread-safe, so we're stuck with:
 * 2) use mktemp to generate a "unique" file name, and then try to openat it
 *    exclusively; repeat as necessary.
 */
int _GD_MakeTempFile(const DIRFILE *D gd_unused_d, int dirfd, char *template)
{
  int fd = -1;
  char *tmp = strdup(template);

  dtrace("%i, \"%s\"", dirfd, template);

  if (!tmp) {
    dreturn("%i", -1);
    return -1;
  }

  do {
    strcpy(template, tmp);
    mktemp(template);
    if (template[0] == 0) {
      free(tmp);
      dreturn("%i", -1);
      return -1;
    }

    fd = gd_OpenAt(D, dirfd, template, O_RDWR | O_CREAT | O_EXCL, 0666);
  } while (errno == EEXIST);

  free(tmp);

  dreturn("%i", fd);
  return fd;
}

int _GD_GenericTouch(int dirfd, struct _gd_raw_file* file, int swap __gd_unused)
{
  int fd;

  dtrace("%i, %p, <unused>", dirfd, file);

  fd = gd_OpenAt(file->D, dirfd, file->name, O_RDWR | O_CREAT | O_TRUNC
      | O_BINARY, 0666);

  if (fd != -1)
    fd = close(fd);

  dreturn("%i", fd);
  return fd;
}

int _GD_GenericUnlink(int dirfd, struct _gd_raw_file* file)
{
  int r;

  dtrace("%i, %p", dirfd, file);

  r = gd_UnlinkAt(file->D, dirfd, file->name, 0);

  dreturn("%i", r);
  return r;
}

int _GD_GenericMove(int olddirfd, struct _gd_raw_file* file, int newdirfd,
    char* new_path)
{
  int r, rename_errno;

  dtrace("%i, %p, %i, \"%s\"", olddirfd, file, newdirfd, new_path);

  r = gd_RenameAt(file->D, olddirfd, file->name, newdirfd, new_path);

  rename_errno = errno;

  if (!r) {
    free(file->name);
    file->name = new_path;
  } else
    free(new_path);

  errno = rename_errno;

  dreturn("%i", r);
  return r;
}

int _GD_GenericTMove(int dirfd0, int dirfd1, struct _gd_raw_file *file,
    gd_ef_tunlink_t tunlink)
{
#ifdef HAVE_FCHMOD
  struct stat stat_buf;
  mode_t mode;
#endif
  dtrace("%i, %i, %p, %p", dirfd0, dirfd1, file, tunlink);
  
  if (file[1].name == NULL) {
    dreturn("%i", 0);
    return 0;
  }

#ifdef HAVE_FCHMOD
  if (gd_StatAt(file[0].D, dirfd0, file[0].name, &stat_buf, 0))
    mode = 0644;
  else
    mode = stat_buf.st_mode;
#endif

  if (gd_RenameAt(file->D, dirfd1, file[1].name, dirfd0, file[0].name)) {
    int move_errno = errno;
    (*tunlink)(dirfd1, file + 1);
    errno = move_errno;
    dreturn("%i", -1);
    return -1;
  }

#ifdef HAVE_FCHMOD
  int fd = gd_OpenAt(file->D, dirfd0, file[0].name, O_RDONLY, 0666);
  fchmod(fd, mode);
  close(fd);
#endif

  dreturn("%i", 0);
  return 0;
}
/* vim: ts=2 sw=2 et tw=80
*/
