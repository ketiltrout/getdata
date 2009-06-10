/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2009 D. V. Wiebe
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
#ifndef GETDATA_INTERNAL_H
#define GETDATA_INTERNAL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "getdata.h"

/* Type conventions:
 *
 *  - samples per frame is always unsigned int
 *  - variables holding offsets or file sizes should be of type off64_t (which
 *    may be simply off_t, depending on local LFS support)
 *  - variables holding object sizes or counts of items read or written should
 *    be of type size_t
 *  - public functions taking or returning types of off64_t should have both
 *    a off_t prototype and and off64_t type prototype.
 */

/* if we don't have off64_t, we probably don't have the rest of the transitional
 * LFS API
 */
#ifndef HAVE_OFF64_T
typedef off_t off64_t
# define lseek64 lseek
# define stat64 stat
#endif

#ifndef __attribute_malloc__
# define __attribute_malloc__
#endif

#ifndef __wur
# define __wur
#endif

#define __gd_unused __attribute__ (( unused ))

/* disable the "unspecified order" remark in ICC */
#ifdef __INTEL_COMPILER
#  pragma warning (disable : 981)
#endif

#ifdef USE_MODULES
# ifdef HAVE_LTDL_H
#  include <ltdl.h>
# else
#  include "gd_ltdl.h"
# endif
#endif

/* debugging macros */
#ifdef GETDATA_DEBUG
const char* _gd_colnil(void);
const char* _gd_coladd(void);
const char* _gd_colsub(void);
#define dtracevoid() printf("%s %s()\n", _gd_coladd(), __FUNCTION__)
#define dtrace(fmt, ...) printf("%s %s(" fmt ")\n", _gd_coladd(), \
    __FUNCTION__, __VA_ARGS__)
#define dreturnvoid() printf("%s %s = (nil)\n", _gd_colsub(), __FUNCTION__)
#define dreturn(fmt, ...) printf("%s %s = " fmt "\n", _gd_colsub(), \
    __FUNCTION__, __VA_ARGS__)
#else
#define dtracevoid()
#define dtrace(...)
#define dreturnvoid()
#define dreturn(...)
#endif

#ifndef HAVE_ATOLL
# ifdef HAVE_ATOQ
#  define atoll atoq
# else
#  define atoll atol
# endif
#endif

/* maximum number of recursions */
#define GD_MAX_RECURSE_LEVEL  32

/* For FILENAME_MAX */
#include <stdio.h>
#include <inttypes.h>

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 5) /* for META lincom */

#ifndef FILENAME_MAX
#  define FILENAME_MAX 4096
#endif

/* Suberror codes */
#define GD_E_OPEN_NOT_EXIST    1
#define GD_E_OPEN_NOT_DIRFILE  2
#define GD_E_OPEN_NO_ACCESS    3

#define GD_E_TRUNC_STAT        1
#define GD_E_TRUNC_UNLINK      2
#define GD_E_TRUNC_DIR         3

#define GD_E_CREAT_FORMAT      1
#define GD_E_CREAT_EXCL        2
#define GD_E_CREAT_DIR         3

/* GD_E_FORMAT suberrors are in getdata.h */

#define GD_E_LINFILE_LENGTH    1
#define GD_E_LINFILE_OPEN      2

#define GD_E_FIELD_PUT         1
#define GD_E_FIELD_BAD         2
#define GD_E_FIELD_MATCH       3

#define GD_E_BAD_ENTRY_TYPE     1
#define GD_E_BAD_ENTRY_METARAW  2
#define GD_E_BAD_ENTRY_SPF      3 
#define GD_E_BAD_ENTRY_NFIELDS  4
#define GD_E_BAD_ENTRY_NUMBITS  5
#define GD_E_BAD_ENTRY_BITNUM   6
#define GD_E_BAD_ENTRY_BITSIZE  7
#define GD_E_BAD_ENTRY_POLYORD  8

#define GD_E_SCALAR_CODE        1
#define GD_E_SCALAR_TYPE        2

#define GD_E_REFERENCE_CODE     1
#define GD_E_REFERENCE_TYPE     2

#define GD_E_PROTECTED_FORMAT   1
#define GD_E_PROTECTED_DATA     2

#define GD_E_DEL_META           1
#define GD_E_DEL_CONST          2
#define GD_E_DEL_DERIVED        3

struct _gd_raw_file {
  char* name;
  int fp;
  void* edata;
  int encoding;
};

/* Unified entry struct */
struct _gd_private_entry {
  gd_entry_t* entry[GD_MAX_LINCOM];

  int calculated;
  char *scalar[GD_MAX_POLYNOM + 1];

  int n_meta;
  int n_meta_string;
  int n_meta_const;
  union {
    gd_entry_t** meta_entry;
    const gd_entry_t* parent;
  };

  /* field lists */
  const char** field_list;
  const char** vector_list;
  char** type_list[GD_N_ENTYPES];
  const char** string_value_list;
  void* const_value_list;

  union {
    struct { /* RAW */
      char* filebase;
      size_t size;
      struct _gd_raw_file file[2];
    };
    struct { /* LINTERP */
      int table_len; /* internal */
      double* x; /* internal */
      double* y; /* internal */
    };
    struct { /* CONST */
      union {
        double dconst;
        uint64_t uconst;
        int64_t iconst;
      };
      int n_client;
      gd_entry_t** client;
    };
    char* string;
  };
};

#define GD_ENC_NONE       0
#define GD_ENC_SLIM       1
#define GD_ENC_GZ_RAW     2
#define GD_ENC_BZ2_RAW    3
#define GD_ENC_ASCII      4
#define GD_ENC_UNKNOWN    5

#define GD_N_SUBENCODINGS (GD_ENC_UNKNOWN + 1)

#define GD_EF_OPEN   0x001
#define GD_EF_CLOSE  0x002
#define GD_EF_TOUCH  0x004
#define GD_EF_SEEK   0x008
#define GD_EF_READ   0x010
#define GD_EF_SIZE   0x020
#define GD_EF_WRITE  0x040
#define GD_EF_SYNC   0x080
#define GD_EF_MOVE   0x100
#define GD_EF_UNLINK 0x200
#define GD_EF_TEMP   0x400

#define GD_TEMP_OPEN    0
#define GD_TEMP_MOVE    1
#define GD_TEMP_DESTROY 2

#define BUFFER_SIZE 9000000

/* Encoding schemes */
extern struct encoding_t {
  unsigned int scheme;
  const char* ext;
  int ecor; /* encoding requires byte-sex correction */
  const char* affix;
  unsigned int provides;
  int (*open)(struct _gd_raw_file*, int, int);
  int (*close)(struct _gd_raw_file*);
  int (*touch)(struct _gd_raw_file*);
  off64_t (*seek)(struct _gd_raw_file*, off64_t, gd_type_t, int);
  ssize_t (*read)(struct _gd_raw_file*, void*, gd_type_t, size_t);
  off64_t (*size)(struct _gd_raw_file*, gd_type_t);
  ssize_t (*write)(struct _gd_raw_file*, const void*, gd_type_t,
      size_t);
  int (*sync)(struct _gd_raw_file*);
  int (*move)(struct _gd_raw_file*, char*);
  int (*unlink)(struct _gd_raw_file*);
  int (*temp)(struct _gd_raw_file*, int);
} _gd_ef[GD_N_SUBENCODINGS];

/* Format file fragment metadata */
struct gd_fragment_t {
  /* Canonical name (full path) */
  char* cname;
  /* Subdirectory name */
  char* sname;
  /* External name (relative to the parent format file fragment) */
  char* ename;
  int modified;
  int parent;
  unsigned long encoding;
  unsigned long byte_sex;
  int protection;
  char* ref_name;

#ifndef __USE_FILE_OFFSET64
  off_t
#else
    __off64_t
#endif
    frame_offset;
};

/* internal flags */
#define GD_INVALID         0x80000000 /* the dirfile is invalid */

#define LIST_VALID_FIELD        0x01
#define LIST_VALID_VECTOR       0x02
#define LIST_VALID_STRING_VALUE 0x04

/* The DIRFILE struct.  */
struct _GD_DIRFILE {
  /* field counts */
  unsigned int n_entries;
  unsigned int n_string;
  unsigned int n_const;
  unsigned int n_meta;

  /* field array */
  gd_entry_t** entry;

  /* the reference field */
  gd_entry_t* reference_field;

  /* directory name */
  char* name;

  /* recursion counter */
  int recurse_level;

  /* fragment list */
  struct gd_fragment_t* fragment;
  int n_fragment;

  /* field lists */
  const char** field_list;
  const char** vector_list;
  const char** type_list[GD_N_ENTYPES];
  const char** string_value_list;
  void* const_value_list;
  int list_validity;
  int type_list_validity;

  /* syntax error callback */
  int (*sehandler)(const DIRFILE*, int, char*);

  /* library error data */
  int error;
  int suberror;
  char* error_string;
  char* error_file;
  int error_line;
  unsigned int flags;
};

extern const gd_entype_t _gd_entype_index[GD_N_ENTYPES];

void _GD_AddData(DIRFILE* D, void *A, unsigned int spfA, void *B,
    unsigned int spfB, gd_type_t type, size_t n);
void* _GD_Alloc(DIRFILE* D, gd_type_t type, size_t n);
int _GD_CalculateEntry(DIRFILE* D, gd_entry_t* E);

/* _GD_ClearError: Everything's A-OK; clear the last error. */
#define _GD_ClearError(D) (D)->error = 0

void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, size_t n) __THROW;
size_t _GD_DoConst(DIRFILE *D, const gd_entry_t *E, gd_type_t return_type,
    void *data_out);
size_t  _GD_DoField(DIRFILE *D, gd_entry_t *E, const char* field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out);
size_t _GD_DoFieldOut(DIRFILE* D, gd_entry_t *E, const char *field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames,
    size_t num_samp, gd_type_t data_type, const void *data_in);
int _GD_EntryCmp(const void *A, const void *B);
int _GD_EncodingUnderstood(unsigned long encoding); 
gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code,
    unsigned int *next);
void _GD_FixEndianness(char* databuffer, size_t size, size_t ns);
void _GD_Flush(DIRFILE* D, gd_entry_t *E, const char* field_code);
void _GD_FlushMeta(DIRFILE* D, int fragment);
void _GD_FreeE(gd_entry_t* E, int priv);
int _GD_GetLine(FILE *fp, char *line, int* linenum);
unsigned int _GD_GetSPF(DIRFILE* D, gd_entry_t* E);
int _GD_Include(DIRFILE* D, const char* ename, const char* format_file,
    int linenum, char** ref_name, int me, int* standards, int flags);
void _GD_InitialiseFramework(void);
void _GD_InsertSort(DIRFILE* D, gd_entry_t* E, int u) __THROW;

#define _GD_InternalError(D) \
  _GD_SetError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL)

gd_type_t _GD_LegacyType(char c);
void _GD_LinterpData(DIRFILE* D, const void *data, gd_type_t type, size_t npts,
    double *lx, double *ly, size_t n_ln);
int _GD_MissingFramework(int encoding, unsigned int funcs);
int _GD_MogrifyFile(DIRFILE* D, gd_entry_t* E, unsigned int encoding,
    unsigned int byte_sex, off64_t offset, int finalise, int new_fragment,
    char* new_filebase);
gd_entry_t* _GD_ParseFieldSpec(DIRFILE* D, int n_cols, const char** in_cols,
    const gd_entry_t* parent, const char* format_file, int linenum,
    unsigned int me, int standards, int creat, int pedantic, int insert);
char* _GD_ParseFragment(FILE* fp, DIRFILE *D, int me, int* standards,
    unsigned int flags);
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E);
void _GD_ScaleData(DIRFILE* D, void *data, gd_type_t type, size_t npts,
    double m, double b);
void _GD_ScanFormat(char* fmt, gd_type_t data_type);
int _GD_SetEncodedName(DIRFILE* D, struct _gd_raw_file* file, const char* base,
    int temp);
void _GD_SetError(DIRFILE* D, int error, int suberror, const char* format_file,
    int line, const char* token);
int _GD_Supports(DIRFILE* D, gd_entry_t* E, unsigned int funcs);
int _GD_Tokenise(DIRFILE *D, const char* instring, char* outstring,
    const char** in_cols, const char* format_file, int linenum);
char* _GD_ValidateField(const gd_entry_t* parent, const char* field_code,
    int strict);

/* generic I/O methods */
int _GD_GenericTouch(struct _gd_raw_file* file);
int _GD_GenericMove(struct _gd_raw_file* file, char* new_path);
int _GD_GenericUnlink(struct _gd_raw_file* file);

/* unencoded I/O methods */
int _GD_RawOpen(struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_RawSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_RawRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
ssize_t _GD_RawWrite(struct _gd_raw_file* file, const void *ptr,
    gd_type_t data_type, size_t nmemb);
int _GD_RawSync(struct _gd_raw_file* file);
int _GD_RawClose(struct _gd_raw_file* file);
off64_t _GD_RawSize(struct _gd_raw_file* file, gd_type_t data_type);
int _GD_RawTemp(struct _gd_raw_file *file, int mode);

/* text I/O methods */
int _GD_AsciiOpen(struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_AsciiSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_AsciiRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
ssize_t _GD_AsciiWrite(struct _gd_raw_file* file, const void *ptr,
    gd_type_t data_type,
    size_t nmemb);
int _GD_AsciiSync(struct _gd_raw_file* file);
int _GD_AsciiClose(struct _gd_raw_file* file);
off64_t _GD_AsciiSize(struct _gd_raw_file* file, gd_type_t data_type);
int _GD_AsciiTemp(struct _gd_raw_file *file, int mode);

/* bzip I/O methods */
int _GD_Bzip2Open(struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_Bzip2Seek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_Bzip2Read(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_Bzip2Close(struct _gd_raw_file* file);
off64_t _GD_Bzip2Size(struct _gd_raw_file* file, gd_type_t data_type);

/* gzip I/O methods */
int _GD_GzipOpen(struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_GzipSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_GzipRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_GzipClose(struct _gd_raw_file* file);
off64_t _GD_GzipSize(struct _gd_raw_file* file, gd_type_t data_type);

/* slim I/O methods */
int _GD_SlimOpen(struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_SlimSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_SlimRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_SlimClose(struct _gd_raw_file* file);
off64_t _GD_SlimSize(struct _gd_raw_file* file, gd_type_t data_type);

#endif
