/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2009 D. V. Wiebe
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
#ifndef GETDATA_INTERNAL_H
#define GETDATA_INTERNAL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "getdata.h"

/* Type conventions:
 *
 *  - samples per frame is always gd_spf_t (aka uin16_t)
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
#define dprintf(fmt, ...) printf("%s %s:%i " fmt "\n", _gd_colnil(), \
    __FUNCTION__, __LINE__, __VA_ARGS__)
#define dreturnvoid() printf("%s %s = (nil)\n", _gd_colsub(), __FUNCTION__)
#define dreturn(fmt, ...) printf("%s %s = " fmt "\n", _gd_colsub(), \
    __FUNCTION__, __VA_ARGS__)
#else
#define dtracevoid()
#define dtrace(...)
#define dprintf(...)
#define dreturnvoid()
#define dreturn(...)
#endif

#ifndef HAVE_STRTOLL
#  define strtoll strtol
#endif

#ifndef HAVE_STRTOULL
#  define stroull strtoul
#endif

/* maximum number of recursions */
#define GD_MAX_RECURSE_LEVEL  32

/* For FILENAME_MAX */
#include <stdio.h>

/* For the C99 integer types */
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

#define GD_E_REPR_UNKNOWN       1
#define GD_E_REPR_PUT           2

#define GD_E_DOMAIN_COMPLEX     1
#define GD_E_DOMAIN_EMPTY       2

#define GD_E_OUT_OF_RANGE       1
#define GD_E_SINGULAR_RANGE     2

struct _gd_raw_file {
  char* name;
  int fp;
  void* edata;
  int encoding;
};

/* Unified entry struct */
struct _gd_private_entry {
  gd_entry_t* entry[GD_MAX_LINCOM];
  int repr[GD_MAX_LINCOM];

  int calculated;

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
      struct _gd_raw_file file[2]; /* encoding framework data */
    };
    struct { /* LINTERP */
      char *table_path;
      int table_len;
      int complex_table;
      double* x;
      union {
        double* y;
        double complex *cy;
      };
    };
    struct { /* CONST */
      union {
        double complex cconst;
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
#define GD_ENC_LZMA_RAW   5
#define GD_ENC_XZ_RAW     6
#define GD_ENC_UNKNOWN    7

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
  unsigned long int scheme;
  const char* ext;
  int ecor; /* encoding requires byte-sex correction */
  const char* affix;
  const char* ffname;
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
  unsigned long int encoding;
  unsigned long int byte_sex;
  int protection;
  char* ref_name;
  off64_t frame_offset;
};

/* internal flags */
#define GD_INVALID         0x80000000 /* the dirfile is invalid */

#define LIST_VALID_FIELD        0x01
#define LIST_VALID_VECTOR       0x02
#define LIST_VALID_STRING_VALUE 0x04

#define GD_REPR_NONE 0
#define GD_REPR_REAL 'r'
#define GD_REPR_IMAG 'i'
#define GD_REPR_MOD  'm'
#define GD_REPR_ARG  'a'

#define GD_REPR_AUTO GD_REPR_REAL

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
  gd_parser_callback_t sehandler;
  void* sehandler_extra;

  /* library error data */
  int error;
  int suberror;
  char* error_string;
  char* error_file;
  int error_line;
  unsigned long int flags;
};

extern const gd_entype_t _gd_entype_index[GD_N_ENTYPES];

void _GD_AddData(DIRFILE* D, void *A, gd_spf_t spfA, void *B, gd_spf_t spfB,
    gd_type_t type, size_t n);
void* _GD_Alloc(DIRFILE* D, gd_type_t type, size_t n);
int _GD_BadInput(DIRFILE* D, gd_entry_t* E, int i);
int _GD_CalculateEntry(DIRFILE* D, gd_entry_t* E);

/* _GD_ClearError: Everything's A-OK; clear the last error. */
#define _GD_ClearError(D) (D)->error = 0

void _GD_CLincomData(DIRFILE* D, int n, void* data1, gd_type_t return_type,
    double complex *data2, double complex *data3, double complex* m,
    double complex *b, gd_spf_t *spf, size_t n_read);
void _GD_CLinterpData(DIRFILE* D, void *data, gd_type_t type,
    const double *data_in, size_t npts, const double *lx,
    const double complex *ly, size_t n_ln);
void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, size_t n) __THROW;
size_t _GD_DoField(DIRFILE*, gd_entry_t*, int, off64_t, size_t, gd_type_t,
    void*);
size_t _GD_DoFieldOut(DIRFILE*, gd_entry_t*, int, off64_t, size_t, gd_type_t,
    const void*);
int _GD_EntryCmp(const void *A, const void *B);
int _GD_EncodingUnderstood(unsigned long int encoding); 
int _GD_FillZero(void *databuffer, gd_type_t type, size_t nz);
gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code,
    unsigned int *next);
void _GD_FixEndianness(char* databuffer, size_t size, size_t ns);
void _GD_Flush(DIRFILE* D, gd_entry_t *E);
void _GD_FlushMeta(DIRFILE* D, int fragment);
void _GD_FreeE(gd_entry_t* E, int priv);
int _GD_GetLine(FILE *fp, char *line, int* linenum);
int _GD_GetRepr(DIRFILE*, const char*, char**);
gd_spf_t _GD_GetSPF(DIRFILE* D, gd_entry_t* E);
int _GD_Include(DIRFILE* D, const char* ename, const char* format_file,
    int linenum, char** ref_name, int me, int* standards, int flags);
void _GD_InitialiseFramework(void);
void _GD_InsertSort(DIRFILE* D, gd_entry_t* E, int u) __THROW;

#define _GD_InternalError(D) \
  _GD_SetError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL)

gd_type_t _GD_LegacyType(char c);
void _GD_LincomData(DIRFILE* D, int n, void* data1, gd_type_t return_type,
    double *data2, double *data3, double* m, double *b, gd_spf_t *spf,
    size_t n_read);
void _GD_LinterpData(DIRFILE* D, void *data, gd_type_t type,
    const double *data_in, size_t npts, const double *lx, const double *ly,
    size_t n_ln);
int _GD_MissingFramework(int32_t encoding, unsigned int funcs);
int _GD_MogrifyFile(DIRFILE* D, gd_entry_t* E, unsigned long int encoding,
    unsigned long int byte_sex, off64_t offset, int finalise, int new_fragment,
    char* new_filebase);
gd_type_t _GD_NativeType(DIRFILE* D, gd_entry_t* E, int repr);
gd_entry_t* _GD_ParseFieldSpec(DIRFILE* D, int n_cols, char** in_cols,
    const gd_entry_t* parent, const char* format_file, int linenum,
    int me, int standards, int creat, int pedantic, int insert);
char* _GD_ParseFragment(FILE* fp, DIRFILE *D, int me, int* standards,
    unsigned long int flags);
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E);
void _GD_ScanFormat(char* fmt, gd_type_t data_type);
int _GD_SetEncodedName(DIRFILE* D, struct _gd_raw_file* file, const char* base,
    int temp);
void _GD_SetError(DIRFILE* D, int error, int suberror, const char* format_file,
    int line, const char* token);
int _GD_SetTablePath(DIRFILE *D, gd_entry_t *E, struct _gd_private_entry *e);
int _GD_Supports(DIRFILE* D, gd_entry_t* E, unsigned int funcs);
int _GD_Tokenise(DIRFILE *D, const char* instring, char* outstring,
    char** in_cols, const char* format_file, int linenum);
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

/* lzma I/O methods */
int _GD_LzmaOpen(struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_LzmaSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_LzmaRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_LzmaClose(struct _gd_raw_file* file);
off64_t _GD_LzmaSize(struct _gd_raw_file* file, gd_type_t data_type);

/* slim I/O methods */
int _GD_SlimOpen(struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_SlimSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_SlimRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_SlimClose(struct _gd_raw_file* file);
off64_t _GD_SlimSize(struct _gd_raw_file* file, gd_type_t data_type);

/* The following has been extracted from internal.cpp from kjs */

/*
 * For systems without NAN, this is a NAN in IEEE double format.
 */

#if !defined(NAN)
static inline __attribute__ ((__const__)) double __NAN()
{
  /* work around some strict alignment requirements
     for double variables on some architectures (e.g. PA-RISC) */
  typedef union { unsigned char b[8]; double d; } nan_t;
#ifdef WORDS_BIGENDIAN
  static const nan_t NaN_Bytes = { { 0x7f, 0xf8, 0, 0, 0, 0, 0, 0 } };
#elif defined(arm)
  static const nan_t NaN_Bytes = { { 0, 0, 0xf8, 0x7f, 0, 0, 0, 0 } };
#else
  static const nan_t NaN_Bytes = { { 0, 0, 0, 0, 0, 0, 0xf8, 0x7f } };
#endif

  const double NaN = NaN_Bytes.d;
  return NaN;
}
#define NAN __NAN()
#endif /* !defined(NAN) */

#endif
