/* Copyright (C) 2008-2017 D. V. Wiebe
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

#ifdef USE_MODULES
#ifdef USE_PTHREAD
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
static pthread_mutex_t gd_mutex_ = PTHREAD_MUTEX_INITIALIZER;
#endif

#ifdef HAVE_LTDL_H
#include <ltdl.h>
#endif

static int framework_initialised = 0;
#endif

/* encoding schemas */
#define GD_EF_NULL_SET NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, \
  NULL, NULL
#define GD_EF_GENERIC_SET &_GD_GenericName, NULL, NULL, NULL, NULL, NULL, \
  NULL, NULL, &_GD_GenericMove, &_GD_GenericUnlink, NULL
#define GD_EF_GENERICNOP_SET &_GD_GenericName, NULL, NULL, NULL, NULL, NULL, \
  NULL, &_GD_NopSync, &_GD_GenericMove, &_GD_GenericUnlink, NULL

#ifdef USE_MODULES
#define GD_EXT_ENCODING_NULL(sc,ex,ec,af,ff) \
{ sc,ex,ec,af,ff,GD_EF_PROVIDES,GD_EF_NULL_SET }
#define GD_EXT_ENCODING_GEN(sc,ex,ec,af,ff) \
{ sc,ex,ec,af,ff,GD_EF_PROVIDES,GD_EF_GENERIC_SET }
#define GD_EXT_ENCODING_GENOP(sc,ex,ec,af,ff) \
{ sc,ex,ec,af,ff,GD_EF_PROVIDES,GD_EF_GENERICNOP_SET }
#else
#define GD_EXT_ENCODING(sc,ex,ec,af,ff) { sc,ex,ec,af,ff,0,GD_INT_FUNCS }
#define GD_EXT_ENCODING_NULL GD_EXT_ENCODING
#define GD_EXT_ENCODING_GEN GD_EXT_ENCODING
#define GD_EXT_ENCODING_GENOP GD_EXT_ENCODING
#endif
struct encoding_t _GD_ef[GD_N_SUBENCODINGS] = {
  { GD_UNENCODED, "", GD_EF_ECOR, NULL, "none", 0,
    &_GD_GenericName, &_GD_RawOpen, &_GD_RawClose, &_GD_RawSeek, &_GD_RawRead,
    &_GD_RawSize, &_GD_RawWrite, &_GD_RawSync, &_GD_GenericMove,
    &_GD_GenericUnlink, NULL /* strerr */
  },

#ifdef USE_GZIP
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE | \
  GD_EF_WRITE | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_GenericName, &_GD_GzipOpen, &_GD_GzipClose, &_GD_GzipSeek, \
  &_GD_GzipRead, &_GD_GzipSize, &_GD_GzipWrite, &_GD_NopSync, \
  &_GD_GenericMove, &_GD_GenericUnlink, &_GD_GzipStrerr
#else
#define GD_EF_PROVIDES 0
#define GD_INT_FUNCS GD_EF_GENERICNOP_SET
#endif
  GD_EXT_ENCODING_GENOP(GD_GZIP_ENCODED, ".gz", GD_EF_ECOR | GD_EF_OOP, "Gzip",
      "gzip"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_BZIP2
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE | \
  GD_EF_WRITE | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_GenericName, &_GD_Bzip2Open, &_GD_Bzip2Close, &_GD_Bzip2Seek, \
  &_GD_Bzip2Read, &_GD_Bzip2Size, &_GD_Bzip2Write, &_GD_NopSync, \
  &_GD_GenericMove, &_GD_GenericUnlink, &_GD_Bzip2Strerr
#else
#define GD_INT_FUNCS GD_EF_GENERICNOP_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING_GENOP(GD_BZIP2_ENCODED, ".bz2", GD_EF_ECOR | GD_EF_OOP,
      "Bzip2", "bzip2"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_SLIM
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_GenericName, &_GD_SlimOpen, &_GD_SlimClose, &_GD_SlimSeek, \
  &_GD_SlimRead, &_GD_SlimSize, NULL /* WRITE */, &_GD_NopSync, \
  &_GD_GenericMove, &_GD_GenericUnlink, &_GD_SlimStrerr
#else
#define GD_INT_FUNCS GD_EF_GENERICNOP_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING_GENOP(GD_SLIM_ENCODED, ".slm", GD_EF_ECOR, "Slim", "slim"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


/* We only provide write support for .xz files, not .lzma */
#ifdef USE_LZMA
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE | \
  GD_EF_WRITE | GD_EF_SYNC | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_GenericName, &_GD_LzmaOpen, &_GD_LzmaClose, &_GD_LzmaSeek, \
  &_GD_LzmaRead, &_GD_LzmaSize, &_GD_LzmaWrite, &_GD_LzmaSync, \
  &_GD_GenericMove, &_GD_GenericUnlink, &_GD_LzmaStrerr
#else
#define GD_INT_FUNCS GD_EF_GENERIC_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING_GEN(GD_LZMA_ENCODED, ".xz", GD_EF_ECOR | GD_EF_OOP, "Lzma",
      "lzma"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES

#ifdef USE_LZMA
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_GenericName, &_GD_LzmaOpen, &_GD_LzmaClose, &_GD_LzmaSeek, \
  &_GD_LzmaRead, &_GD_LzmaSize, NULL /* WRITE */, NULL /* SYNC */, \
  &_GD_GenericMove, &_GD_GenericUnlink, &_GD_LzmaStrerr
#else
#define GD_INT_FUNCS GD_EF_GENERIC_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING_GEN(GD_LZMA_ENCODED, ".lzma", GD_EF_ECOR, "Lzma", "lzma"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


  { GD_TEXT_ENCODED, ".txt", 0, NULL, "text", 0,
    &_GD_GenericName, &_GD_AsciiOpen, &_GD_AsciiClose, &_GD_AsciiSeek,
    &_GD_AsciiRead, &_GD_AsciiSize, &_GD_AsciiWrite, &_GD_AsciiSync,
    &_GD_GenericMove, &_GD_GenericUnlink, NULL /* strerr */
  },

  { GD_SIE_ENCODED, ".sie", GD_EF_ECOR | GD_EF_SWAP, NULL, "sie", 0,
    &_GD_GenericName, &_GD_SampIndOpen, &_GD_SampIndClose, &_GD_SampIndSeek,
    &_GD_SampIndRead, &_GD_SampIndSize, &_GD_SampIndWrite, &_GD_SampIndSync,
    &_GD_GenericMove, &_GD_GenericUnlink, NULL /* strerr */
  },


#ifdef USE_ZZIP
#define GD_EF_PROVIDES \
  GD_EF_NAME | GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | \
  GD_EF_SIZE | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_ZzipName, &_GD_ZzipOpen, &_GD_ZzipClose, &_GD_ZzipSeek, &_GD_ZzipRead, \
  &_GD_ZzipSize, NULL /* WRITE */, NULL /* SYNC */, NULL /* MOVE */, \
  NULL /* UNLINK */, &_GD_ZzipStrerr
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING_NULL(GD_ZZIP_ENCODED, NULL, GD_EF_ECOR | GD_EF_EDAT, "Zzip",
      "zzip"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_ZZSLIM
#define GD_EF_PROVIDES \
  GD_EF_NAME | GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | \
  GD_EF_SIZE | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_ZzslimName, &_GD_ZzslimOpen, &_GD_ZzslimClose, &_GD_ZzslimSeek, \
  &_GD_ZzslimRead, &_GD_ZzslimSize, NULL /* WRITE */, NULL /* SYNC */, \
  NULL /* MOVE */, NULL /* UNLINK */, &_GD_ZzslimStrerr
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING_NULL(GD_ZZSLIM_ENCODED, NULL, GD_EF_ECOR | GD_EF_EDAT,
      "Zzslim", "zzslim"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_FLAC
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE | \
  GD_EF_WRITE | GD_EF_STRERR
#define GD_INT_FUNCS \
  &_GD_GenericName, &_GD_FlacOpen, &_GD_FlacClose, &_GD_FlacSeek, \
  &_GD_FlacRead, &_GD_FlacSize, &_GD_FlacWrite, &_GD_NopSync, \
  &_GD_GenericMove, &_GD_GenericUnlink, &_GD_FlacStrerr
#else
#define GD_INT_FUNCS GD_EF_GENERICNOP_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING_GENOP(GD_FLAC_ENCODED, ".flac", GD_EF_SWAP | GD_EF_OOP,
      "Flac", "flac"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


  { GD_ENC_UNSUPPORTED, NULL, 0, "", "", 0,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
  }
};

void _GD_InitialiseFramework(void)
{
  dtracevoid();

#ifdef USE_MODULES
#ifdef USE_PTHREAD
  if (!framework_initialised) {
    pthread_mutex_lock(&gd_mutex_);
#endif
    /* check again */
    if (!framework_initialised) {
      framework_initialised = 1;
      lt_dlinit();
    }
#ifdef USE_PTHREAD
    pthread_mutex_unlock(&gd_mutex_);
  }
#endif
#endif
  dreturnvoid();
}

#define _GD_EncodingUnderstood(encoding) \
  ((encoding == GD_UNENCODED || encoding == GD_SLIM_ENCODED || \
    encoding == GD_GZIP_ENCODED || encoding == GD_BZIP2_ENCODED || \
    encoding == GD_TEXT_ENCODED || encoding == GD_LZMA_ENCODED || \
    encoding == GD_SIE_ENCODED || encoding == GD_ZZIP_ENCODED || \
    encoding == GD_ZZSLIM_ENCODED || encoding == GD_FLAC_ENCODED))

#ifdef USE_MODULES
static void *_GD_ResolveSymbol(lt_dlhandle lib, struct encoding_t *restrict enc,
    const char *restrict name)
{
  void* func;
  char symbol[100];

  dtrace("%p, %p, \"%s\"", lib, enc, name);
  /* create the symbol name */
  sprintf(symbol, "lt_libgetdata%s_LTX_GD_%s%s", enc->affix, enc->affix, name);
  symbol[13] -= 'A' - 'a';
  func = lt_dlsym(lib, symbol);

  dreturn("%p", func);
  return func;
}
#endif

#define GETDATA_MODULEPREFIX GETDATA_MODULEDIR "/libgetdata"
int _GD_MissingFramework(int encoding, unsigned int funcs)
{
  int ret;

  dtrace("%i, 0x%X", encoding, funcs);

#ifdef USE_MODULES
#ifdef USE_PTHREAD
  pthread_mutex_lock(&gd_mutex_);
#endif

  /* set up the encoding library if required */
  if (_GD_ef[encoding].provides) {
    char *library;
    lt_dlhandle lib;

    /* make the library name */
    library = malloc(sizeof(GETDATA_MODULEDIR) +
        strlen(_GD_ef[encoding].affix) + sizeof(GD_GETDATA_VERSION) + 13);
    if (!library) {
      _GD_ef[encoding].provides = 0;
#ifdef USE_PTHREAD
      pthread_mutex_unlock(&gd_mutex_);
#endif
      dreturn("%i", 1);
      return 1;
    }

    sprintf(library, GETDATA_MODULEPREFIX "%s-" GD_GETDATA_VERSION,
        _GD_ef[encoding].affix);

    /* affix starts with a capital letter, we need to lowercasify it --
     * also, sizeof includes the trailing NUL in its count */
    library[sizeof(GETDATA_MODULEPREFIX) - 1] -= 'A' - 'a';

    /* open */
    if ((lib = lt_dlopenext(library)) == NULL) {
      /* if that didn't work, look for it in the search path */
      if ((lib = lt_dlopenext(library + sizeof(GETDATA_MODULEDIR))) == NULL)
      {
        free(library);
        _GD_ef[encoding].provides = 0;
#ifdef USE_PTHREAD
        pthread_mutex_unlock(&gd_mutex_);
#endif
        dreturn("%i", 1);
        return 1;
      }
    }
    free(library);

    /* Try to resolve the symbols */
    if (_GD_ef[encoding].provides & GD_EF_NAME)
      _GD_ef[encoding].name = (gd_ef_name_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Name");
    if (_GD_ef[encoding].provides & GD_EF_OPEN)
      _GD_ef[encoding].open = (gd_ef_open_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Open");
    if (_GD_ef[encoding].provides & GD_EF_CLOSE)
      _GD_ef[encoding].close = (gd_ef_close_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Close");
    if (_GD_ef[encoding].provides & GD_EF_SEEK)
      _GD_ef[encoding].seek = (gd_ef_seek_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Seek");
    if (_GD_ef[encoding].provides & GD_EF_READ)
      _GD_ef[encoding].read = (gd_ef_read_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Read");
    if (_GD_ef[encoding].provides & GD_EF_SIZE)
      _GD_ef[encoding].size = (gd_ef_size_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Size");
    if (_GD_ef[encoding].provides & GD_EF_WRITE)
      _GD_ef[encoding].write = (gd_ef_write_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Write");
    if (_GD_ef[encoding].provides & GD_EF_SYNC)
      _GD_ef[encoding].sync = (gd_ef_sync_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Sync");
    if (_GD_ef[encoding].provides & GD_EF_UNLINK)
      _GD_ef[encoding].unlink = (gd_ef_unlink_t)_GD_ResolveSymbol(lib,
          _GD_ef + encoding, "Unlink");

    /* we tried our best, don't bother trying again */
    _GD_ef[encoding].provides = 0;
  }
#ifdef USE_PTHREAD
  pthread_mutex_unlock(&gd_mutex_);
#endif
#endif

  ret =
    (funcs & GD_EF_NAME    && _GD_ef[encoding].name    == NULL) ||
    (funcs & GD_EF_OPEN    && _GD_ef[encoding].open    == NULL) ||
    (funcs & GD_EF_CLOSE   && _GD_ef[encoding].close   == NULL) ||
    (funcs & GD_EF_SEEK    && _GD_ef[encoding].seek    == NULL) ||
    (funcs & GD_EF_READ    && _GD_ef[encoding].read    == NULL) ||
    (funcs & GD_EF_SIZE    && _GD_ef[encoding].size    == NULL) ||
    (funcs & GD_EF_WRITE   && _GD_ef[encoding].write   == NULL) ||
    (funcs & GD_EF_SYNC    && _GD_ef[encoding].sync    == NULL) ||
    (funcs & GD_EF_UNLINK  && _GD_ef[encoding].unlink  == NULL) ||
    (funcs & GD_EF_STRERR  && _GD_ef[encoding].strerr  == NULL);

  dreturn("%i", ret);
  return ret;
}

static int _GD_MoveOver(DIRFILE *restrict D, int fragment,
    struct gd_raw_file_ *restrict file)
{
  const int dirfd = D->fragment[fragment].dirfd;
#ifdef HAVE_FCHMOD
  int fd;
  struct stat stat_buf;
  mode_t mode, tmode;
#endif
  dtrace("%p, %i, %p", D, fragment, file);

#ifdef HAVE_FCHMOD
  if (gd_StatAt(D, dirfd, file[1].name, &stat_buf, 0))
    tmode = 0644;
  else
    tmode = stat_buf.st_mode;

  if (gd_StatAt(D, dirfd, file[0].name, &stat_buf, 0))
    mode = tmode;
  else
    mode = stat_buf.st_mode;
#endif

  if (gd_RenameAt(D, dirfd, file[1].name, dirfd, file[0].name)) {
    int move_errno = errno;
    if (gd_UnlinkAt(D, dirfd, file[1].name, 0) == 0) {
      free(file[1].name);
      file[1].name = NULL;
    }
    errno = move_errno;
    _GD_SetError(D, GD_E_UNCLEAN_DB, GD_E_UNCLEAN_CALL,
        D->fragment[fragment].cname, 0, "gd_RenameAt");
    D->flags |= GD_INVALID;
    dreturn("%i", -1);
    return -1;
  }

#ifdef HAVE_FCHMOD
  if (tmode != mode) {
    fd = gd_OpenAt(file->D, dirfd, file[0].name, O_RDONLY, 0666);
    fchmod(fd, mode);
    close(fd);
  }
#endif

  dreturn("%i", 0);
  return 0;
}

/* Close a raw file, taking care of cleaning-up out-of-place writes, and
 * discarding temporary files */
int _GD_FiniRawIO(DIRFILE *D, const gd_entry_t *E, int fragment, int flags)
{
  const int clotemp = (flags & GD_FINIRAW_CLOTEMP) ? 1 : 0;
  const int old_mode = E->e->u.raw.file[0].mode;
  const int oop_write = ((_GD_ef[E->e->u.raw.file[0].subenc].flags & GD_EF_OOP)
      && (old_mode & GD_FILE_WRITE)) ? 1 : 0;
  dtrace("%p, %p, %i, 0x%X", D, E, fragment, flags);

  if ((E->e->u.raw.file[clotemp].idata >= 0) ||
      (clotemp == 0 && oop_write && (E->e->u.raw.file[1].idata >= 0)))
  {
    /* close the secondary file in write mode (but not temp mode) */
    if (oop_write && E->e->u.raw.file[1].idata >= 0) {
      if (E->e->u.raw.file[0].idata >= 0) {
        /* copy the rest of the input to the output */
        char *buffer;
        int n_read, n_wrote, n_to_write;

        buffer = _GD_Malloc(D, GD_BUFFER_SIZE);
        if (buffer == NULL) {
          dreturn("%i", -1);
          return -1;
        }

        do {
          n_to_write = n_read = (*_GD_ef[E->e->u.raw.file[0].subenc].read)(
              E->e->u.raw.file, buffer, E->EN(raw,data_type),
              GD_BUFFER_SIZE / GD_SIZE(E->EN(raw,data_type)));
          if (n_read < 0) {
            free(buffer);
            _GD_SetEncIOError(D, GD_E_IO_READ, E->e->u.raw.file + 0);
            dreturn("%i", -1);
            return -1;
          } else while (n_to_write > 0) {
            n_wrote = (*_GD_ef[E->e->u.raw.file[0].subenc].write)(
                E->e->u.raw.file + 1, buffer, E->EN(raw,data_type), n_to_write);
            if (n_wrote < 0) {
              free(buffer);
              _GD_SetEncIOError(D, GD_E_IO_WRITE, E->e->u.raw.file + 0);
              dreturn("%i", -1);
              return -1;
            }
            n_to_write -= n_wrote;
          }
        } while (n_read > 0);

        free(buffer);
      }

      if ((*_GD_ef[E->e->u.raw.file[0].subenc].close)(E->e->u.raw.file + 1)) {
        dreturn("%i", -1);
        return -1;
      }
    }

    /* close the file */
    if ((E->e->u.raw.file[clotemp].idata >= 0) &&
      (*_GD_ef[E->e->u.raw.file[clotemp].subenc].close)(E->e->u.raw.file +
          clotemp))
    {
      if (D->error == GD_E_OK)
        _GD_SetEncIOError(D, GD_E_IO_CLOSE, E->e->u.raw.file + clotemp);
      dreturn("%i", 1);
      return 1;
    }
  }

  if (flags & GD_FINIRAW_DEFER) {
    dreturn("%i", 0);
    return 0;
  }

  /* take care of moving things into place */
  if (oop_write || clotemp) {
    if (flags & GD_FINIRAW_DISCARD) {
      /* Throw away the temporary file */
      if (E->e->u.raw.file[1].name != NULL &&
          gd_UnlinkAt(D, D->fragment[fragment].dirfd, E->e->u.raw.file[1].name,
            0))
      {
        if (D->error == GD_E_OK)
          _GD_SetEncIOError(D, GD_E_IO_UNLINK, E->e->u.raw.file + 1);
        dreturn("%i", -1);
        return -1;
      }
    } else {
      /* Move the old file over the new file */
      if (_GD_MoveOver(D, fragment, E->e->u.raw.file)) {
        dreturn("%i", -1);
        return -1;
      }
    }

    free(E->e->u.raw.file[1].name);
    E->e->u.raw.file[1].name = NULL;
  }

  dreturn("%i", 0);
  return 0;
}

/* Perform a RAW field write */
ssize_t _GD_WriteOut(const gd_entry_t *E, const struct encoding_t *enc,
    const void *ptr, gd_type_t type, size_t n, int temp)
{
  ssize_t n_wrote;

  dtrace("%p, %p, %p, 0x%X, %" PRIuSIZE ", %i", E, enc, ptr, type, n, temp);

  if (temp)
    n_wrote = (*enc->write)(E->e->u.raw.file + 1, ptr, type, n);
  else {
    if (enc->flags & GD_EF_OOP) {
      n_wrote = (*enc->write)(E->e->u.raw.file + 1, ptr, type, n);

      if (n_wrote > 0 && E->e->u.raw.file[0].idata >= 0) {
        /* advance the read pointer by the appropriate amount */
        if ((*enc->seek)(E->e->u.raw.file, E->e->u.raw.file[0].pos + n_wrote,
              E->EN(raw,data_type), GD_FILE_READ) < 0)
        {
          n_wrote = -1;
        }
      }
    } else 
      n_wrote = (*enc->write)(E->e->u.raw.file, ptr, type, n);
  }

  dreturn("%" PRIdSIZE, n_wrote);
  return n_wrote;
}

/* Open a raw file, if necessary; also check for required functions */
int _GD_InitRawIO(DIRFILE *D, const gd_entry_t *E, const char *filebase,
    int fragment, const struct encoding_t *enc, unsigned int funcs,
    unsigned int mode, int swap)
{
  const int touch = mode & GD_FILE_TOUCH;
  int oop_write = 0;

  dtrace("%p, %p, \"%s\", %i, %p, 0x%X, 0x%X, %i", D, E, filebase, fragment,
      enc, funcs, mode, swap);

  if (mode & (GD_FILE_WRITE | GD_FILE_TOUCH))
    funcs |= GD_EF_WRITE;

  mode &= ~GD_FILE_TOUCH;

  if (!(mode & GD_FILE_TEMP)) {
    if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_OPEN | funcs)) {
      dreturn("%i", 1);
      return 1;
    }

    enc = _GD_ef + E->e->u.raw.file[0].subenc;
    oop_write = ((enc->flags & GD_EF_OOP) && mode == GD_FILE_WRITE) ? 1 : 0;

    /* Do nothing, if possible */
    if (!touch && (((mode & GD_FILE_READ) && (E->e->u.raw.file[0].idata >= 0)
            && (E->e->u.raw.file[0].mode & GD_FILE_READ))
          || ((mode & GD_FILE_WRITE) && (E->e->u.raw.file[oop_write].idata >= 0)
            && (E->e->u.raw.file[0].mode & GD_FILE_WRITE))))
    {
      dreturn("%i", 0);
      return 0;
    }

    /* close the file, if necessary */
    if ((E->e->u.raw.file[0].idata >= 0 || ((enc->flags & GD_EF_OOP)
            && (E->e->u.raw.file[1].idata >= 0)))
        && ((mode == GD_FILE_READ && (enc->flags & GD_EF_OOP)
            && (E->e->u.raw.file[0].mode & GD_FILE_WRITE))
          || (mode == GD_FILE_WRITE
            && !(E->e->u.raw.file[0].mode & GD_FILE_WRITE))))
    {
      if (_GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_KEEP)) {
        dreturn("%i", 1);
        return 1;
      }
    }
    if (oop_write)
      E->e->u.raw.file[1].subenc = E->e->u.raw.file[0].subenc;
  }

  if (filebase == NULL)
    filebase = E->e->u.raw.filebase;

  if (fragment == -1)
    fragment = E->fragment_index;

  if (oop_write || mode & GD_FILE_TEMP) {
    /* create temporary file in file[1] */
    if ((*enc->name)(D, (const char*)D->fragment[fragment].enc_data,
          E->e->u.raw.file + 1, filebase, 1, 0))
    {
      ; /* error already set */
      dreturn("%i", 1);
      return 1;
    } else if ((*enc->open)(D->fragment[fragment].dirfd, E->e->u.raw.file + 1,
          E->EN(raw,data_type), swap, GD_FILE_WRITE | GD_FILE_TEMP))
    {
      _GD_SetEncIOError(D, GD_E_IO_OPEN, E->e->u.raw.file + 1);
      dreturn("%i", 1);
      return 1;
    }

    if (oop_write) {
      /* The read file in OOP mode is flagged as RW. */
      mode = GD_FILE_RDWR;
    } else {
      /* Temp file creation complete */
      dreturn("%i", 0);
      return 0;
    }
  }

  /* open a regular file, if necessary */
  if (E->e->u.raw.file[0].idata < 0) {
    if ((*enc->name)(D, (const char*)D->fragment[fragment].enc_data,
          E->e->u.raw.file, filebase, 0, 0))
    {
      dreturn("%i", 1);
      return 1;
    } else if ((*enc->open)(D->fragment[fragment].dirfd, E->e->u.raw.file,
          E->EN(raw,data_type), swap, mode))
    {
      /* In oop_write mode, it doesn't matter if the old file doesn't exist */
      if (!oop_write || errno != ENOENT) {
        _GD_SetEncIOError(D, GD_E_IO_OPEN, E->e->u.raw.file + 0);
        dreturn("%i", 1);
        return 1;
      }
      E->e->u.raw.file[0].mode = mode;
    }
  }

  if (touch)
    _GD_FiniRawIO(D, E, fragment, GD_FINIRAW_KEEP);

  dreturn("%i", 0);
  return 0;
}

/* Figure out the encoding scheme */
static unsigned long _GD_ResolveEncoding(DIRFILE *restrict D,
    const char *restrict name, const char *restrict enc_data,
    unsigned long scheme, int dirfd, struct gd_raw_file_ *restrict file)
{
  char *candidate;
  int i;
  const size_t len = strlen(name);
  struct stat statbuf;

  dtrace("%p, \"%s\", \"%s\", 0x%08lx, %i, %p", D, name, enc_data, scheme,
      dirfd, file);

  for (i = 0; _GD_ef[i].scheme != GD_ENC_UNSUPPORTED; i++) {
    if (scheme == GD_AUTO_ENCODED || scheme == _GD_ef[i].scheme) {
      if (_GD_ef[i].ext) {
        candidate = malloc(len + strlen(_GD_ef[i].ext) + 1);
        if (!candidate)
          continue;

        sprintf(candidate, "%s%s", name, _GD_ef[i].ext);
      } else {
        if (_GD_MissingFramework(i, GD_EF_NAME))
          continue;

        if ((*_GD_ef[i].name)(D, enc_data, file, name, 0, 1))
          continue;

        candidate = file->name;
        file->name = NULL;
      }

      if (gd_StatAt(D, dirfd, candidate, &statbuf, 0) == 0)
        if (S_ISREG(statbuf.st_mode)) {
          if (file != NULL)
            file->subenc = i;
          free(candidate);
          dreturn("%08lx", _GD_ef[i].scheme);
          return _GD_ef[i].scheme;
        }
      free(candidate);
    }
  }

  if (scheme != 0 && file != NULL) {
    for (i = 0; _GD_ef[i].scheme != GD_ENC_UNSUPPORTED; i++)
      if (scheme == _GD_ef[i].scheme) {
        file->subenc = i;
        dreturn("0x%08lx", _GD_ef[i].scheme);
        return _GD_ef[i].scheme;
      }
  }

  dreturn("%08lx", (unsigned long)GD_AUTO_ENCODED);
  return GD_AUTO_ENCODED;
}

int _GD_Supports(DIRFILE *D, const gd_entry_t *E, unsigned int funcs)
{
  dtrace("%p, %p, 0x%X", D, E, funcs);

  /* Figure out the dirfile encoding type, if required */
  if (D->fragment[E->fragment_index].encoding == GD_AUTO_ENCODED) {
    D->fragment[E->fragment_index].encoding =
      _GD_ResolveEncoding(D, E->e->u.raw.filebase,
          (const char*)D->fragment[E->fragment_index].enc_data, GD_AUTO_ENCODED,
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
        (const char*)D->fragment[E->fragment_index].enc_data,
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

int _GD_GenericName(DIRFILE *restrict D,
    const char *restrict enc_data gd_unused_,
    struct gd_raw_file_ *restrict file, const char *restrict base, int temp,
    int resolv gd_unused_)
{
  dtrace("%p, <unused>, %p, \"%s\", %i, <unused>", D, file, base, temp);

  if (file->name == NULL) {
    file->D = D;
    file->name = _GD_Malloc(D, strlen(base) + (temp ? 8 :
          strlen(_GD_ef[file->subenc].ext) + 1));
    if (file->name == NULL) {
      dreturn("%i", -1);
      return -1;
    }

    sprintf(file->name, "%s%s", base,
        temp ? "_XXXXXX" : _GD_ef[file->subenc].ext);
  }

  dreturn("%i (%s)", 0, file->name);
  return 0;
}

/* This function assumes that the new encoding has no fragment->enc_data. */
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
    gd_entry_t **raw_entry = _GD_Malloc(D, sizeof(*raw_entry) * D->n_entries);

    if (raw_entry == NULL) {
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
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_DISCARD |
            GD_FINIRAW_CLOTEMP);
    } else
      for (i = 0; i < n_raw; ++i) {
        struct gd_raw_file_ temp;
        memcpy(&temp, raw_entry[i]->e->u.raw.file, sizeof(temp));

        raw_entry[i]->e->u.raw.file[0].name = NULL;
        raw_entry[i]->e->u.raw.file[0].subenc =
          raw_entry[i]->e->u.raw.file[1].subenc;

        /* discard the old file */
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_DISCARD);

        if ((*_GD_ef[temp.subenc].name)(D,
              (const char*)D->fragment[raw_entry[i]->fragment_index].enc_data,
              raw_entry[i]->e->u.raw.file, raw_entry[i]->e->u.raw.filebase, 0,
              0))
        {
          raw_entry[i]->e->u.raw.file[0].name = temp.name;
          raw_entry[i]->e->u.raw.file[0].subenc = temp.subenc;
        } else if (_GD_FiniRawIO(D, raw_entry[i], fragment,
              GD_FINIRAW_KEEP | GD_FINIRAW_CLOTEMP))
        {
          raw_entry[i]->e->u.raw.file[0].name = temp.name;
          raw_entry[i]->e->u.raw.file[0].subenc = temp.subenc;
        } else if ((*_GD_ef[temp.subenc].unlink)(D->fragment[fragment].dirfd,
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
        _GD_FiniRawIO(D, D->entry[i], fragment, GD_FINIRAW_KEEP);

        /* reset encoding subscheme. */
        D->entry[i]->e->u.raw.file[0].subenc = GD_ENC_UNKNOWN;

        /* delete name */
        free(D->entry[i]->e->u.raw.file[0].name);
        D->entry[i]->e->u.raw.file[0].name = NULL;
      }
  }

  free(D->fragment[fragment].enc_data);
  D->fragment[fragment].enc_data = NULL;
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

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment)
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
  else if (!_GD_EncodingUnderstood(encoding))
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, GD_E_UNENC_TARGET, NULL, 0, NULL);
  else if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i) {
      _GD_RecodeFragment(D, encoding, i, move);

      if (D->error)
        break;
    }
  } else
    _GD_RecodeFragment(D, encoding, fragment, move);

  GD_RETURN_ERROR(D);
}

unsigned long gd_encoding(DIRFILE* D, int fragment) gd_nothrow
{
  unsigned long reported_encoding = GD_ENC_UNSUPPORTED;
  unsigned int i;

  dtrace("%p, %i", D, fragment);

  GD_RETURN_IF_INVALID(D, "%i", 0);

  if (fragment < 0 || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  /* Attempt to figure out the encoding, if it's not known */
  if (D->fragment[fragment].encoding == GD_AUTO_ENCODED) {
    /* locate a RAW field in this fragment */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        D->fragment[fragment].encoding =
          _GD_ResolveEncoding(D, D->entry[i]->e->u.raw.filebase,
              (const char*)D->fragment[fragment].enc_data, GD_AUTO_ENCODED,
              D->fragment[fragment].dirfd, D->entry[i]->e->u.raw.file);

        if (D->fragment[fragment].encoding != GD_AUTO_ENCODED)
          break;
      }
  }

  if (D->fragment[fragment].encoding != GD_AUTO_ENCODED)
    reported_encoding = D->fragment[fragment].encoding;

  dreturn("%lx", (unsigned long)reported_encoding);
  return reported_encoding;
}

/* report whether a particular encoding is supported */
int gd_encoding_support(unsigned long encoding) gd_nothrow
{
  int i;

  const unsigned int read_funcs = GD_EF_NAME | GD_EF_OPEN | GD_EF_CLOSE |
    GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE;
  const unsigned int write_funcs = read_funcs | GD_EF_WRITE | GD_EF_SYNC |
    GD_EF_MOVE | GD_EF_UNLINK;

  dtrace("0x%lX", encoding);

  /* make sure we have a valid encoding */
  if (!_GD_EncodingUnderstood(encoding)) {
    dreturn("%i", GD_E_UNKNOWN_ENCODING);
    return GD_E_UNKNOWN_ENCODING;
  }

  /* spin up ltdl if needed */
  _GD_InitialiseFramework();

  /* Loop through valid subencodings checking for write support */
  for (i = 0; _GD_ef[i].scheme != GD_ENC_UNSUPPORTED; i++)
    if (_GD_ef[i].scheme == encoding) {
      if (!_GD_MissingFramework(i, write_funcs)) {
        dreturn("%i", GD_RDWR);
        return GD_RDWR;
      }
    }

  /* No write support; try read support */
  for (i = 0; _GD_ef[i].scheme != GD_ENC_UNSUPPORTED; i++)
    if (_GD_ef[i].scheme == encoding) {
      if (!_GD_MissingFramework(i, read_funcs)) {
        dreturn("%i", GD_RDONLY);
        return GD_RDONLY;
      }
    }

  /* nope */
  dreturn("%i", GD_E_UNSUPPORTED);
  return GD_E_UNSUPPORTED;
}

/* This is basically the non-existant POSIX funcion mkstempat.  There are two
 * approaches we could take here:
 * 1) fchdir to dirfd, use mkstemp to grab a file descriptor; fchdir back to
 *    cwd, but this isn't thread-safe, so we're stuck with:
 * 2) use mktemp to generate a "unique" file name, and then try to openat it
 *    exclusively; repeat as necessary.
 */
int _GD_MakeTempFile(const DIRFILE *D gd_unused_d, int dirfd, char *tmpl)
{
  int fd = -1;
  char *tmp = strdup(tmpl);

  dtrace("%p, %i, \"%s\"", D, dirfd, tmpl);

  if (!tmp) {
    dreturn("%i", -1);
    return -1;
  }

  do {
    strcpy(tmpl, tmp);
    mktemp(tmpl);
    if (tmpl[0] == 0) {
      free(tmp);
      dreturn("%i", -1);
      return -1;
    }

    fd = gd_OpenAt(D, dirfd, tmpl, O_RDWR | O_CREAT | O_EXCL, 0666);
  } while (errno == EEXIST);

  free(tmp);

  dreturn("%i", fd);
  return fd;
}

int _GD_GenericUnlink(int dirfd, struct gd_raw_file_* file)
{
  int r;

  dtrace("%i, %p", dirfd, file);

  r = gd_UnlinkAt(file->D, dirfd, file->name, 0);

  dreturn("%i", r);
  return r;
}

int _GD_GenericMove(int olddirfd, struct gd_raw_file_ *restrict file,
    int newdirfd, char *restrict new_path)
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

/* This function does nothing */
int _GD_NopSync(struct gd_raw_file_ *file gd_unused_)
{
  dtrace("<unused>");

  dreturn("%i", 0);
  return 0;
}
/* vim: ts=2 sw=2 et tw=80
*/
