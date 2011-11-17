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
#define GD_EF_NULL_SET NULL, NULL, NULL, NULL, NULL, NULL, NULL, \
  &_GD_GenericMove, &_GD_GenericUnlink
#ifdef USE_MODULES
#define GD_EXT_ENCODING(sc,ex,ec,af,ff) \
{ sc,ex,ec,af,ff,GD_EF_PROVIDES,GD_EF_NULL_SET }
#else
#define GD_EXT_ENCODING(sc,ex,ec,af,ff) { sc,ex,ec,af,ff,0,GD_INT_FUNCS }
#endif
struct encoding_t _gd_ef[GD_N_SUBENCODINGS] = {
  { GD_UNENCODED, "", GD_EF_ECOR, NULL, "none", 0,
    &_GD_RawOpen, &_GD_RawClose, &_GD_RawSeek, &_GD_RawRead, &_GD_RawSize,
    &_GD_RawWrite, &_GD_RawSync, &_GD_GenericMove, &_GD_GenericUnlink
  },

#ifdef USE_GZIP
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE | \
  GD_EF_WRITE | GD_EF_SYNC
#define GD_INT_FUNCS \
  &_GD_GzipOpen, &_GD_GzipClose, &_GD_GzipSeek, &_GD_GzipRead, &_GD_GzipSize, \
  &_GD_GzipWrite, &_GD_GzipSync, &_GD_GenericMove, &_GD_GenericUnlink
#else
#define GD_EF_PROVIDES 0
#define GD_INT_FUNCS GD_EF_NULL_SET
#endif
  GD_EXT_ENCODING(GD_GZIP_ENCODED, ".gz", GD_EF_ECOR | GD_EF_OOP, "Gzip",
      "gzip"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_BZIP2
#define GD_EF_PROVIDES \
    GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE
#define GD_INT_FUNCS \
  &_GD_Bzip2Open, &_GD_Bzip2Close, &_GD_Bzip2Seek, &_GD_Bzip2Read, \
  &_GD_Bzip2Size, NULL /* WRITE */, NULL /* SYNC */, &_GD_GenericMove, \
  &_GD_GenericUnlink
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING(GD_BZIP2_ENCODED, ".bz2", GD_EF_ECOR, "Bzip2", "bzip2"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_SLIM
#define GD_EF_PROVIDES \
  GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE
#define GD_INT_FUNCS \
  &_GD_SlimOpen, &_GD_SlimClose, &_GD_SlimSeek, &_GD_SlimRead, &_GD_SlimSize, \
  NULL /* WRITE */, NULL /* SYNC */, &_GD_GenericMove, &_GD_GenericUnlink
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING(GD_SLIM_ENCODED, ".slm", GD_EF_ECOR, "Slim", "slim"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


#ifdef USE_LZMA
#define GD_EF_PROVIDES \
    GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE
#define GD_INT_FUNCS \
  &_GD_LzmaOpen, &_GD_LzmaClose, &_GD_LzmaSeek, &_GD_LzmaRead, &_GD_LzmaSize, \
  NULL /* WRITE */, NULL /* SYNC */, &_GD_GenericMove, &_GD_GenericUnlink
#else
#define GD_INT_FUNCS GD_EF_NULL_SET
#define GD_EF_PROVIDES 0
#endif
  GD_EXT_ENCODING(GD_LZMA_ENCODED, ".xz", GD_EF_ECOR, "Lzma", "lzma"),
  GD_EXT_ENCODING(GD_LZMA_ENCODED, ".lzma", GD_EF_ECOR, "Lzma", "lzma"),
#undef GD_INT_FUNCS
#undef GD_EF_PROVIDES


  { GD_TEXT_ENCODED, ".txt", 0, NULL, "text", 0,
    &_GD_AsciiOpen, &_GD_AsciiClose, &_GD_AsciiSeek, &_GD_AsciiRead,
    &_GD_AsciiSize, &_GD_AsciiWrite, &_GD_AsciiSync, &_GD_GenericMove,
    &_GD_GenericUnlink
  },

  { GD_SIE_ENCODED, ".sie", GD_EF_ECOR | GD_EF_SWAP, NULL, "sie", 0,
    &_GD_SampIndOpen, &_GD_SampIndClose, &_GD_SampIndSeek, &_GD_SampIndRead,
    &_GD_SampIndSize, &_GD_SampIndWrite, &_GD_SampIndSync, &_GD_GenericMove,
    &_GD_GenericUnlink
  },

  { GD_ENC_UNSUPPORTED, "", 0, "", "", 0,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
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

  dtrace("%i, 0x%X", encoding, funcs);

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
      _gd_ef[encoding].open = (gd_ef_open_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Open");
    if (_gd_ef[encoding].provides & GD_EF_CLOSE)
      _gd_ef[encoding].close = (gd_ef_close_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Close");
    if (_gd_ef[encoding].provides & GD_EF_SEEK)
      _gd_ef[encoding].seek = (gd_ef_seek_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Seek");
    if (_gd_ef[encoding].provides & GD_EF_READ)
      _gd_ef[encoding].read = (gd_ef_read_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Read");
    if (_gd_ef[encoding].provides & GD_EF_SIZE)
      _gd_ef[encoding].size = (gd_ef_size_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Size");
    if (_gd_ef[encoding].provides & GD_EF_WRITE)
      _gd_ef[encoding].write = (gd_ef_write_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Write");
    if (_gd_ef[encoding].provides & GD_EF_SYNC)
      _gd_ef[encoding].sync = (gd_ef_sync_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Sync");
    if (_gd_ef[encoding].provides & GD_EF_UNLINK)
      _gd_ef[encoding].unlink = (gd_ef_unlink_t)_GD_ResolveSymbol(lib,
          _gd_ef + encoding, "Unlink");

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
    (funcs & GD_EF_SEEK    && _gd_ef[encoding].seek    == NULL) ||
    (funcs & GD_EF_READ    && _gd_ef[encoding].read    == NULL) ||
    (funcs & GD_EF_SIZE    && _gd_ef[encoding].size    == NULL) ||
    (funcs & GD_EF_WRITE   && _gd_ef[encoding].write   == NULL) ||
    (funcs & GD_EF_SYNC    && _gd_ef[encoding].sync    == NULL) ||
    (funcs & GD_EF_UNLINK  && _gd_ef[encoding].unlink  == NULL);

  dreturn("%i", ret);
  return ret;
}

static int _GD_MoveOver(DIRFILE *D, int fragment, struct _gd_raw_file *file)
{
  const int dirfd = D->fragment[fragment].dirfd;
#ifdef HAVE_FCHMOD
  struct stat stat_buf;
  mode_t mode;
#endif
  dtrace("%p, %i, %p", D, fragment, file);

#ifdef HAVE_FCHMOD
  if (gd_StatAt(D, dirfd, file[0].name, &stat_buf, 0))
    mode = 0644;
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
    _GD_SetError(D, GD_E_UNCLEAN_DB, 0, D->fragment[fragment].cname, 0, NULL);
    D->flags |= GD_INVALID;
    dreturn("%i", -1);
    return -1;
  }

#ifdef HAVE_FCHMOD
  int fd = gd_OpenAt(file->D, dirfd, file[0].name, O_RDONLY, 0666);
  fchmod(fd, mode);
  close(fd);
#endif

  dreturn("%i", 0);
  return 0;
}

/* Close a raw file, taking care of cleaning-up out-of-place writes, and
 * discarding temporary files */
int _GD_FiniRawIO(DIRFILE *D, gd_entry_t *E, int fragment, int flags)
{
  const int clotemp = (flags & GD_FINIRAW_CLOTEMP) ? 1 : 0;
  const int old_mode = E->e->u.raw.file[0].mode;
  const int oop_write = ((_gd_ef[E->e->u.raw.file[0].subenc].flags & GD_EF_OOP)
      && (old_mode & GD_FILE_WRITE)) ? 1 : 0;
  dtrace("%p, %p, %i, 0x%X", D, E, fragment, flags);

  if ((E->e->u.raw.file[clotemp].idata >= 0) ||
      (clotemp == 0 && oop_write && (E->e->u.raw.file[1].idata >= 0)))
  {
    /* close the secondary file in write mode (but not temp mode) */
    if (oop_write && E->e->u.raw.file[1].idata >= 0) {
      if (E->e->u.raw.file[0].idata >= 0) {
        /* copy the rest of the input to the output */
        char buffer[GD_BUFFER_SIZE];
        int n_read, n_wrote;

        do {
          n_read = (*_gd_ef[E->e->u.raw.file[0].subenc].read)(E->e->u.raw.file,
              buffer, E->EN(raw,data_type), GD_BUFFER_SIZE);
          if (n_read > 0)
            n_wrote = (*_gd_ef[E->e->u.raw.file[0].subenc].write)(
                E->e->u.raw.file + 1, buffer, E->EN(raw,data_type), n_read);
        } while (n_read == GD_BUFFER_SIZE);
      }

      if ((*_gd_ef[E->e->u.raw.file[0].subenc].close)(E->e->u.raw.file + 1)) {
        dreturn("%i", -1);
        return -1;
      }
    }

    /* close the file */
    if ((E->e->u.raw.file[clotemp].idata >= 0) &&
      (*_gd_ef[E->e->u.raw.file[clotemp].subenc].close)(E->e->u.raw.file +
          clotemp))
    {
      if (D->error == GD_E_OK)
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[clotemp].name, errno,
            NULL);
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
      if (gd_UnlinkAt(D, D->fragment[fragment].dirfd, E->e->u.raw.file[1].name,
            0))
      {
        if (D->error == GD_E_OK)
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
              NULL);
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
ssize_t _GD_WriteOut(DIRFILE *D gd_unused_d, gd_entry_t *E,
    const struct encoding_t *enc, const void *ptr, gd_type_t type, size_t n,
    int temp)
{
  ssize_t n_wrote;

  dtrace("%p, %p, %p, %p, 0x%X, %zu, %i", D, E, enc, ptr, type, n, temp);

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

  dreturn("%zi", n_wrote);
  return n_wrote;
}

/* Open a raw file, if necessary; also check for required functions */
int _GD_InitRawIO(DIRFILE *D, gd_entry_t *E, const char *filebase,
    int fragment, const struct encoding_t *enc, unsigned int funcs,
    unsigned int mode, int swap)
{
  int temp_fd = -1;
  const int touch = mode & GD_FILE_TOUCH;
  int oop_write = 0;

  dtrace("%p, %p, \"%s\", %i, %p, 0x%X, 0x%X, %i", D, E, filebase, fragment,
      enc, funcs, mode, swap);

  if (mode & (GD_FILE_WRITE | GD_FILE_TOUCH))
    funcs |= GD_EF_WRITE;

  mode &= ~GD_FILE_TOUCH;

  if (!(mode & GD_FILE_TEMP)) {
    if (!_GD_Supports(D, E, GD_EF_OPEN | funcs)) {
      dreturn("%i", 1);
      return 1;
    }

    enc = _gd_ef + E->e->u.raw.file[0].subenc;
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
  }

  if (filebase == NULL)
    filebase = E->e->u.raw.filebase;

  if (fragment == -1)
    fragment = E->fragment_index;

  if (mode & GD_FILE_TEMP) {
    /* create temporary file in file[1] */
    if (_GD_SetEncodedName(D, E->e->u.raw.file + 1, filebase, 1))
    {
      ; /* error already set */
    } else if ((temp_fd = _GD_MakeTempFile(D, D->fragment[fragment].dirfd,
            E->e->u.raw.file[1].name)) < 0)
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno, NULL);
    } else if ((*enc->open)(temp_fd, E->e->u.raw.file + 1, swap,
          GD_FILE_WRITE | GD_FILE_TEMP))
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno, NULL);
    }

    if (D->error) {
      dreturn("%i", 1);
      return 1;
    }
    dreturn("%i", 0);
    return 0;
  }

  if (oop_write) {
    /* an out-of-place write requires us to open a temporary file and pass
     * in its fd */
    if (_GD_SetEncodedName(D, E->e->u.raw.file + 1, filebase, 1)) {
      dreturn("%i", 1);
      return 1;
    } else if ((temp_fd = _GD_MakeTempFile(D,
            D->fragment[E->fragment_index].dirfd, E->e->u.raw.file[1].name))
          < 0)
    {
      dreturn("%i", 1);
      return 1;
    } else if ((*enc->open)(temp_fd, E->e->u.raw.file + 1, _GD_FileSwapBytes(D,
            E->fragment_index), GD_FILE_WRITE)) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      dreturn("%i", 1);
      return 1;
    }
    /* The read file in OOP mode is flagged as RW. */
    mode = GD_FILE_RDWR;
  }

  /* open a regular file, if necessary */
  if (E->e->u.raw.file[0].idata < 0) {
    if (_GD_SetEncodedName(D, E->e->u.raw.file, filebase, 0)) {
      dreturn("%i", 1);
      return 1;
    } else if ((*enc->open)(D->fragment[E->fragment_index].dirfd,
          E->e->u.raw.file, _GD_FileSwapBytes(D, E->fragment_index), mode))
    {
      /* In oop_write mode, it doesn't matter if the old file doesn't exist */
      if (!oop_write || errno != ENOENT) {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
        dreturn("%i", 1);
        return 1;
      }
      E->e->u.raw.file[0].mode = mode;
    }
  }

  if (touch)
    _GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_KEEP);

  dreturn("%i", 0);
  return 0;
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
      if (candidate) {
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
  dtrace("%p, %p, 0x%X", D, E, funcs);

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
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_DISCARD |
            GD_FINIRAW_CLOTEMP);
    } else
      for (i = 0; i < n_raw; ++i) {
        struct _gd_raw_file temp;
        memcpy(&temp, raw_entry[i]->e->u.raw.file, sizeof(temp));

        raw_entry[i]->e->u.raw.file[0].name = NULL;
        raw_entry[i]->e->u.raw.file[0].subenc =
          raw_entry[i]->e->u.raw.file[1].subenc;

        /* discard the old file */
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_DISCARD);

        if (_GD_SetEncodedName(D, raw_entry[i]->e->u.raw.file,
              raw_entry[i]->e->u.raw.filebase, 0))
        {
          raw_entry[i]->e->u.raw.file[0].name = temp.name;
          raw_entry[i]->e->u.raw.file[0].subenc = temp.subenc;
        } else if (_GD_FiniRawIO(D, raw_entry[i], fragment,
              GD_FINIRAW_KEEP | GD_FINIRAW_CLOTEMP))
        {
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

  dtrace("%p, %i, \"%s\"", D, dirfd, template);

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
/* vim: ts=2 sw=2 et tw=80
*/
