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
#include <ltdl.h>

#ifdef STDC_HEADERS
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

#ifdef USE_PTHREAD
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
static pthread_mutex_t _gd_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

/* encoding schemas */
static const struct encoding_t encoding_framework[GD_N_SUBENCODINGS] = {
  { GD_UNENCODED, "", 1, NULL, 0,
    &_GD_RawOpen, &_GD_RawClose, &_GD_GenericTouch, &_GD_RawSeek,
    &_GD_RawRead, &_GD_RawSize, &_GD_RawWrite, &_GD_RawSync,
    &_GD_GenericUnlink, &_GD_RawTemp
  },
#ifdef USE_MODULES
  /* Modules are external */
  { GD_GZIP_ENCODED, ".gz", 1, "Gzip",
# ifdef USE_GZIP
    GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE,
# else
    0,
# endif
    NULL, NULL, NULL, NULL, NULL, NULL, NULL , NULL, &_GD_GenericUnlink, NULL },
  { GD_BZIP2_ENCODED, ".bz2", 1, "Bzip2",
# ifdef USE_GZIP
    GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE,
# else
    0,
# endif
    NULL, NULL, NULL, NULL, NULL, NULL, NULL , NULL, &_GD_GenericUnlink, NULL },
  { GD_SLIM_ENCODED, ".slm", 1, "Slim",
# ifdef USE_SLIMLIB
    GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ | GD_EF_SIZE,
# else
    0,
# endif
    NULL, NULL, NULL, NULL, NULL, NULL, NULL , NULL, &_GD_GenericUnlink, NULL },
#else
  /* Modules are internal */
  { GD_GZIP_ENCODED, ".gz", 1, NULL, 0,
# ifdef USE_GZIP
    &_GD_GzipOpen, &_GD_GzipClose, NULL /* TOUCH */,
    &_GD_GzipSeek, &_GD_GzipRead, &_GD_GzipSize, NULL /* WRITE */,
    NULL /* SYNC */, &_GD_GenericUnlink, NULL /* TEMP */
# else
    NULL, NULL, NULL, NULL, NULL, NULL, NULL , NULL, &_GD_GenericUnlink, NULL
# endif
  },
  { GD_BZIP2_ENCODED, ".bz2", 1, NULL, 0,
# ifdef USE_GZIP
    &_GD_Bzip2Open, &_GD_Bzip2Close, NULL /* TOUCH */,
    &_GD_Bzip2Seek, &_GD_Bzip2Read, &_GD_Bzip2Size, NULL /* WRITE */,
    NULL /* SYNC */, &_GD_GenericUnlink, NULL /* TEMP */
# else
    NULL, NULL, NULL, NULL, NULL, NULL, NULL , NULL, &_GD_GenericUnlink, NULL
# endif
  },
  { GD_SLIM_ENCODED, ".slm", 1, NULL, 0,
# ifdef USE_SLIMLIB
    &_GD_SlimOpen, &_GD_SlimClose, NULL /* TOUCH */,
    &_GD_SlimSeek, &_GD_SlimRead, &_GD_SlimSize, NULL /* WRITE */,
    NULL /* SYNC */, &_GD_GenericUnlink, NULL /* TEMP */
# else
    NULL, NULL, NULL, NULL, NULL, NULL, NULL , NULL, &_GD_GenericUnlink, NULL
# endif
  },
#endif
  { GD_TEXT_ENCODED, ".txt", 0, NULL, 0,
    &_GD_AsciiOpen, &_GD_AsciiClose, &_GD_GenericTouch,
    &_GD_AsciiSeek, &_GD_AsciiRead, &_GD_AsciiSize, &_GD_AsciiWrite,
    &_GD_AsciiSync, &_GD_GenericUnlink, &_GD_AsciiTemp },
  { GD_ENC_UNSUPPORTED, "", 0, "", 0,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL },
};

void _GD_InitialiseFramework(DIRFILE* D)
{
  memcpy(D->ef, encoding_framework, sizeof(struct encoding_t) * GD_N_SUBENCODINGS);
}

/* Check for a valid field name -- returns input on error */
char* _GD_ValidateField(const gd_entry_t* parent, const char* field_code,
    int strict)
{
  size_t len = strlen(field_code);
  size_t i;
  char* ptr;

  dtrace("%p, \"%s\", %i", parent, field_code, strict);

  if (field_code[0] == '\0' || len >= GD_MAX_LINE_LENGTH) {
    dreturn("%p", field_code);
    return (char*)field_code;
  }

  for (i = 0; i < len; ++i)
    if (field_code[i] == '/' || field_code[i] == '<' || field_code[i] == '>' ||
        field_code[i] == ';' || field_code[i] == '|' || field_code[i] == '&' ||
        (strict && field_code[i] == '.'))
    {
      dreturn("%p", field_code);
      return (char*)field_code;
    }

  if (strcmp("FRAMEOFFSET", field_code) == 0 ||
      strcmp("ENCODING", field_code) == 0 ||
      strcmp("ENDIAN", field_code) == 0 ||
      strcmp("INCLUDE", field_code) == 0 ||
      strcmp("META", field_code) == 0 ||
      strcmp("VERSION", field_code) == 0)
  {
    dreturn("%p", field_code);
    return (char*)field_code;
  }

  if (!strict && len > 3 && ((len > 4 && field_code[len - 4] == '.') ||
        field_code[len - 3] == '.'))
    for (i = 0; i < GD_N_SUBENCODINGS; ++i)
      if (encoding_framework[i].ext[0] != '\0' && strcmp(field_code + len -
            strlen(encoding_framework[i].ext), encoding_framework[i].ext) == 0)
        {
          dreturn("%p", field_code);
          return (char*)field_code;
        }

  if (parent != NULL) {
    ptr = malloc(strlen(parent->field) + strlen(field_code) + 2);
    sprintf(ptr, "%s/%s", parent->field, field_code);
  } else
    ptr = strdup(field_code);

  dreturn("\"%s\"", ptr);
  return ptr;
}

#ifdef USE_MODULES
static void* _GD_ResolveSymbol(void* lib, struct encoding_t* enc,
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

int _GD_MissingFramework(DIRFILE* D, int encoding, unsigned int funcs)
{
  dtrace("%p, %x, %x", D, encoding, funcs);
 
#ifdef USE_MODULES
#ifdef USE_PTHREAD
    pthread_mutex_lock(&_gd_mutex);
#endif

  /* set up the encoding library if required */
  if (D->ef[encoding].provides) {
    char library[FILENAME_MAX];
    lt_dlhandle lib;

    /* make the library name */
    sprintf(library, "libgetdata%s-%s", D->ef[encoding].affix, PACKAGE_VERSION);
    library[10] -= 'A' - 'a';

    /* open */
    if ((lib = lt_dlopenext(library)) == NULL) {
      /* if that didn't work, try opening an unversioned version */
      sprintf(library, "libgetdata%s", D->ef[encoding].affix);
      library[10] -= 'A' - 'a';
      if ((lib = lt_dlopenext(library)) == NULL) {
        dreturn("%i", 1);
        return 1;
      }
    }

    /* Try to resolve the symbols */
    if (D->ef[encoding].provides & GD_EF_OPEN)
      D->ef[encoding].open = _GD_ResolveSymbol(lib, D->ef + encoding, "Open");
    if (D->ef[encoding].provides & GD_EF_CLOSE)
      D->ef[encoding].close = _GD_ResolveSymbol(lib, D->ef + encoding, "Close");
    if (D->ef[encoding].provides & GD_EF_TOUCH)
      D->ef[encoding].touch = _GD_ResolveSymbol(lib, D->ef + encoding, "Touch");
    if (D->ef[encoding].provides & GD_EF_SEEK)
      D->ef[encoding].seek = _GD_ResolveSymbol(lib, D->ef + encoding, "Seek");
    if (D->ef[encoding].provides & GD_EF_READ)
      D->ef[encoding].read = _GD_ResolveSymbol(lib, D->ef + encoding, "Read");
    if (D->ef[encoding].provides & GD_EF_SIZE)
      D->ef[encoding].size = _GD_ResolveSymbol(lib, D->ef + encoding, "Size");
    if (D->ef[encoding].provides & GD_EF_WRITE)
      D->ef[encoding].write = _GD_ResolveSymbol(lib, D->ef + encoding, "Write");
    if (D->ef[encoding].provides & GD_EF_SYNC)
      D->ef[encoding].sync = _GD_ResolveSymbol(lib, D->ef + encoding, "Sync");
    if (D->ef[encoding].provides & GD_EF_UNLINK)
      D->ef[encoding].unlink = _GD_ResolveSymbol(lib, D->ef + encoding,
          "Unlink");
    if (D->ef[encoding].provides & GD_EF_TEMP)
      D->ef[encoding].temp = _GD_ResolveSymbol(lib, D->ef + encoding, "Temp");

    /* we tried our best, don't bother trying again */
    D->ef[encoding].provides = 0;
  }
#ifdef USE_PTHREAD
    pthread_mutex_unlock(&_gd_mutex);
#endif
#endif

  int ret =
    (funcs & GD_EF_OPEN   && D->ef[encoding].open   == NULL) ||
    (funcs & GD_EF_CLOSE  && D->ef[encoding].close  == NULL) ||
    (funcs & GD_EF_TOUCH  && D->ef[encoding].touch  == NULL) ||
    (funcs & GD_EF_SEEK   && D->ef[encoding].seek   == NULL) ||
    (funcs & GD_EF_READ   && D->ef[encoding].read   == NULL) ||
    (funcs & GD_EF_SIZE   && D->ef[encoding].size   == NULL) ||
    (funcs & GD_EF_WRITE  && D->ef[encoding].write  == NULL) ||
    (funcs & GD_EF_SYNC   && D->ef[encoding].sync   == NULL) ||
    (funcs & GD_EF_UNLINK && D->ef[encoding].unlink == NULL) ||
    (funcs & GD_EF_TEMP   && D->ef[encoding].temp   == NULL);

  dreturn("%i", ret);
  return ret;
}

/* Figure out the encoding scheme */
static unsigned int _GD_ResolveEncoding(DIRFILE* D, const char* name,
    unsigned int scheme, struct _gd_raw_file *file)
{
  char candidate[FILENAME_MAX];
  char* ptr;
  int i, len = strlen(name);
  struct stat64 statbuf;

  dtrace("%p, \"%s\", 0x%08x, %p", D, name, scheme, file);

  strcpy(candidate, name);
  ptr = candidate + len;
  len = FILENAME_MAX - len;

  for (i = 0; D->ef[i].scheme != GD_ENC_UNSUPPORTED; i++) {
    if (scheme == GD_AUTO_ENCODED || scheme == D->ef[i].scheme) {
      strcpy(ptr, D->ef[i].ext);

      if (stat64(candidate, &statbuf) == 0) 
        if (S_ISREG(statbuf.st_mode)) {
          if (file != NULL)
            file->encoding = i;
          dreturn("%08x", D->ef[i].scheme);
          return D->ef[i].scheme;
        }
    }
  }

  if (scheme != 0 && file != NULL) {
    for (i = 0; D->ef[i].scheme != GD_ENC_UNSUPPORTED; i++)
      if (scheme == D->ef[i].scheme) {
        file->encoding = i;
        dreturn("0x%08x", D->ef[i].scheme);
        return D->ef[i].scheme;;
      }
  }

  dreturn("%08x", GD_AUTO_ENCODED);
  return GD_AUTO_ENCODED;
}

int _GD_Supports(DIRFILE* D, gd_entry_t* E, unsigned int funcs)
{
  dtrace("%p, %p, %x", D, E, funcs);

  /* Figure out the dirfile encoding type, if required */
  if (D->fragment[E->fragment_index].encoding == GD_AUTO_ENCODED) {
    D->fragment[E->fragment_index].encoding =
      _GD_ResolveEncoding(D, E->e->filebase, GD_AUTO_ENCODED, E->e->file);
  }

  /* If the encoding scheme is unknown, complain */
  if (D->fragment[E->fragment_index].encoding == GD_AUTO_ENCODED) {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  /* Figure out the encoding subtype, if required */
  if (E->e->file[0].encoding == GD_ENC_UNKNOWN)
    _GD_ResolveEncoding(D, E->e->filebase,
        D->fragment[E->fragment_index].encoding, E->e->file);

  /* check for our function(s) */
  if (_GD_MissingFramework(D, E->e->file[0].encoding, funcs)) {
    _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* this function is needed by the encoding libraries and is not makred with
 * an initial underscore.  Furthermore, it uses the global encoding framework
 * array to retrieve the encoding extension. */
int GD_SetEncodedName(struct _gd_raw_file* file, const char* base, int temp)
{
  dtrace("%p, \"%s\", %i", file, base, temp);

  if (file->name == NULL) {
    file->name = malloc(FILENAME_MAX);
    if (file->name == NULL) {
      dreturn("%i", -1);
      return -1;
    }

    snprintf(file->name, FILENAME_MAX, "%s%s", base, temp ? "_XXXXXX" :
        encoding_framework[file->encoding].ext);
  }

  dreturn("%i", 0);
  return 0;
}

static void _GD_RecodeFragment(DIRFILE* D, unsigned int encoding, int fragment,
    int move)
{
  unsigned int i, n_raw = 0;

  dtrace("%p, %u, %i, %i\n", D, encoding, fragment, move);

  /* check protection */
  if (D->fragment[fragment].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment].cname);
    dreturnvoid();
    return;
  }

  if (move && encoding != D->fragment[fragment].encoding) {
    gd_entry_t **raw_entry = malloc(sizeof(gd_entry_t*) * D->n_entries);

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
        if (!_GD_Supports(D, D->entry[i], GD_EF_TEMP))
          break;

        /* add this raw field to the list */
        raw_entry[n_raw++] = D->entry[i];

        if (_GD_MogrifyFile(D, D->entry[i], encoding,
              D->fragment[D->entry[i]->fragment_index].byte_sex,
              D->fragment[D->entry[i]->fragment_index].frame_offset, 0))
          break;
      }

    /* If successful, move the temporary file over the old file, otherwise
     * remove the temporary files */
    for (i = 0; i < n_raw; ++i)
      if ((*D->ef[raw_entry[i]->e->file[0].encoding].temp)
          (raw_entry[i]->e->file, (D->error) ? GD_TEMP_DESTROY : GD_TEMP_MOVE))
        _GD_SetError(D, GD_E_RAW_IO, 0, raw_entry[i]->e->file[0].name,
            errno, NULL);

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
        if (D->entry[i]->e->file[0].fp != -1 &&
            (*D->ef[D->entry[i]->e->file[0].encoding].close)
            (D->entry[i]->e->file))
        {
          _GD_SetError(D, GD_E_RAW_IO, 0, D->entry[i]->e->file[1].name,
              errno, NULL);
          break;
        }
        /* reset encoding subscheme. */
        D->entry[i]->e->file[0].encoding = GD_ENC_UNKNOWN;
      }
  }

  D->fragment[fragment].encoding = encoding;
  D->fragment[fragment].modified = 1;

  dreturnvoid();
}

int put_encoding(DIRFILE* D, unsigned int encoding, int fragment, int move)
{
  int i;

  dtrace("%p, %u, %i, %i\n", D, encoding, fragment, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%zi", -1);
    return -1;
  }

  if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (encoding != GD_UNENCODED && encoding != GD_TEXT_ENCODED &&
      encoding != GD_SLIM_ENCODED)
  {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
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

unsigned int get_encoding(DIRFILE* D, int fragment)
{
  dtrace("%p, %i\n", D, fragment);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment < 0 || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  dreturn("%i", D->fragment[fragment].encoding);
  return D->fragment[fragment].encoding;
}

int _GD_GenericTouch(struct _gd_raw_file* file, const char* base)
{
  dtrace("%p, \"%s\"", file, base);

  if (GD_SetEncodedName(file, base, 0)) {
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

  if (GD_SetEncodedName(file, base, 0)) {
    dreturn("%i", -1);
    return -1;
  }

  int r = unlink(file->name);

  dreturn("%i", r);
  return r;
}
/* vim: ts=2 sw=2 et tw=80
*/
