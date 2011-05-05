/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2011 D. V. Wiebe
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
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

/* MSVC types */
#ifdef _MSC_VER
typedef size_t ssize_t;
typedef int mode_t;
#endif


#ifdef _MSC_VER
// missing in sys/stat.h
#define S_ISREG(m)  (((m) & _S_IFMT) == _S_IFREG)
#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#define snprintf _snprintf
#endif

#ifdef __APPLE__
typedef off_t off64_t;
#endif

#ifdef GD_NO_C99_API
#  define GD_DCOMPLEXP_t double *
#  define GD_DCOMPLEXA(v) double v[2]
#  define GD_DCOMPLEXV(v) double v[][2]
#  define cabs(z)  sqrt((z)[0] * (z)[0] + (z)[1] * (z)[1])
#  define carg(z)  atan2((z)[1], (z)[0])
#  define creal(z) ((z)[0])
#  define cimag(z) ((z)[1])
#  define _gd_a2c _gd_c2c
#  define _gd_c2c(a,b) do { (a)[0] = (b)[0]; (a)[1] = (b)[1]; } while(0)
#  define _gd_c2cp _gd_c2c
#  define _gd_ca2c(a,b,i) _gd_c2c((a),(b) + 2 * i)
#  define _gd_cp2ca(a,i,b) do { \
  (a)[2 * i] = (b)[0]; (a)[2 * i + 1] = (b)[1]; \
} while(0)
#  define _gd_l2c(a,x,y) do { (a)[0] = (x); (a)[1] = (y); } while(0)
#  define _gd_r2c(a,b) do { (a)[0] = b; (a)[1] = 0; } while(0)
#  define _gd_r2ca(a,i,b,t) do { \
  ((t*)a)[2 * i] = (t)(b); ((t*)a)[2 * i + 1] = 0; \
} while(0)
#  define _gd_ccmpl(a,x,y) ((a)[0] == x && (a)[1] == y)
#  define _gd_ccmpc(a,b) ((a)[0] == (b)[0] && (a)[1] == (b)[1])
#else
#  define GD_DCOMPLEXP_t double _Complex *
#  define GD_DCOMPLEXA(v) double _Complex v
#  define GD_DCOMPLEXV(v) double _Complex* v
#  define _gd_a2c(a,b) a = *((double complex*)(b))
#  define _gd_c2c(a,b) a = b
#  define _gd_c2cp(a,b) *a = b
#  define _gd_ca2c(a,b,i) a = b[i]
#  define _gd_cp2ca(a,i,b) (a)[i] = *(b)
#  define _gd_l2c(a,x,y) a = (x + _Complex_I * y)
#  define _gd_r2c(a,b) a = b
#  define _gd_r2ca(a,i,b,t) ((complex t*)a)[i] = (complex t)(b)
#  define _gd_ccmpl(a,x,y) (a == (x + _Complex_I * y))
#  define _gd_ccmpc(a,b) (a == b)

#ifdef HAVE_COMPLEX_H
#include <complex.h>
#elif defined HAVE_CABS && defined __GNUC__ && (__GNUC__ > 3)
/* This is a cygwin hack: the Cygwin C library isn't C99 compliant, but gcc 3+
 * contains built-in versions of these functions */
#define complex _Complex
#define _Complex_I (__extension__ 1.0iF)
double cabs(double complex z);
double carg(double complex z);
#define cexp(z) (exp(__real__ (z)) * (cos(__imag__ (z)) + _Complex_I \
      * sin(__imag__ (z))))
double creal(double complex z);
double cimag(double complex z);
#endif
#endif

/* For FILE */
#include <stdio.h>

/* For the C99 integer types */
#ifdef HAVE_INTTYPES_H
# ifndef __STDC_FORMAT_MACROS
#  define __STDC_FORMAT_MACROS
# endif
#include <inttypes.h>
#endif

#ifdef HAVE_IO_H
#  include <io.h>
#endif

#define GD_ARM_FLAG (GD_ARM_ENDIAN | GD_NOT_ARM_ENDIAN)

/* Type conventions:
 *
 *  - samples per frame is always gd_spf_t (aka uin16_t)
 *  - variables holding offsets or file sizes should be of type off64_t (which
 *    may be simply off_t, depending on local LFS support)
 *  - variables holding object sizes or counts of items read or written should
 *    be of type size_t
 *  - public functions taking or returning types of off64_t should have both
 *    a off_t prototype and an off64_t type prototype.
 */

#ifndef __attribute_malloc__
# define __attribute_malloc__
#endif

#ifndef __wur
# define __wur
#endif

#ifdef _MSC_VER
# define __gd_unused
#else
# define __gd_unused __attribute__ (( unused ))
#endif

/* disable the "unspecified order" remark in ICC */
#ifdef __INTEL_COMPILER
#  pragma warning (disable : 981)
#endif

#ifdef USE_MODULES
# ifdef HAVE_LTDL_H
#  include <ltdl.h>
# endif
#endif

/* debugging macros */
#ifdef GETDATA_DEBUG
#define GD_COL_SIZE 100
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
#define dwatch(fmt, v) printf("%s %s = " fmt "\n", _gd_colnil(), #v, v)
#else
#define dtracevoid()
#define dtrace(...)
#define dprintf(...)
#define dreturnvoid()
#define dreturn(...)
#define dwatch(...)
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#define FOPEN_TEXT
#else
#define FOPEN_TEXT "t"
#endif

/* The Microsoft CRT appears to treat %hh as %h */
#ifdef __MSVCRT__
#define NO_8BIT_INT_PREFIX
#endif

/* function aliases */
#ifndef HAVE_FSEEKO64
#  ifndef HAVE_FSEEKO
#    define fseeko64(a,b,c) fseek(a,(long)(b),c)
#  else
#    define fseeko64(a,b,c) fseeko(a,(off_t)(b),c)
#  endif
#endif

#ifndef HAVE_FTELLO64
#  ifndef HAVE_FTELLO
#    define ftello64 (off64_t)ftell
#  else
#    define ftello64 (off64_t)ftello
#  endif
#endif

#ifdef HAVE__STRTOI64
#  define gd_strtoll _strtoi64
#elif defined(HAVE_STRTOLL)
#  define gd_strtoll strtoll
#else
#  define gd_strtoll strtol
#endif

#ifdef HAVE__STRTOUI64
#  define gd_strtoull _strtoi64
#elif defined(HAVE_STRTOULL)
#  define gd_strtoull strtoull
#else
#  define gd_strtoull strtoul
#endif

#if defined __MSVCRT__ && defined HAVE__FDOPEN
#define fdopen _fdopen
#endif

#if !HAVE_FSYNC && HAVE__COMMIT
#  define fsync _commit
#endif

#ifndef HAVE_GMTIME_R
#include <time.h>
struct tm *gmtime_r(const time_t *timep, struct tm *result);
#endif

#ifdef HAVE__LSEEKI64
#define lseek64 (off64_t)_lseeki64
#endif

#ifdef MKDIR_NO_MODE
#ifdef HAVE__MKDIR
#define mkdir(f,m) _mkdir(f)
#else
#define mkdir(f,m) mkdir(f)
#endif
#endif

#ifndef EOVERFLOW
#define EOVERFLOW EINVAL
#endif

#if defined __MSVCRT__ && defined HAVE__OPEN
#define open _open
#endif

#if defined __MSVCRT__ && defined HAVE__READ
#define read _read
#endif

#if defined __MSVCRT__ && defined HAVE__RMDIR
#define rmdir _rmdir
#endif

#if HAVE_STAT64
#  define gd_stat64 stat64
#elif HAVE__STAT64
#  define gd_stat64 _stat64
#else
#  define gd_stat64 stat
#endif

#if HAVE_STRUCT_STAT64
typedef struct stat64 gd_stat64_t;
#elif HAVE_STRUCT__STAT64
typedef struct _stat64 gd_stat64_t;
#elif HAVE_STRUCT___STAT64
typedef struct __stat64 gd_stat64_t;
#else
typedef struct stat gd_stat64_t;
#endif

#ifndef AT_SYMLINK_NOFOLLOW
#define AT_SYMLINK_NOFOLLOW 0x100
#endif

#ifdef HAVE_OPENAT
# define gd_unused_d __gd_unused
# define gd_OpenAt(d,...) openat(__VA_ARGS__)
#else
# define gd_unused_d /**/
int gd_OpenAt(const DIRFILE*, int, const char*, int, mode_t);
#endif

#ifdef HAVE_FSTATAT
# define gd_StatAt(d,...) fstatat(__VA_ARGS__)
#else
int gd_StatAt(const DIRFILE*, int, const char*, struct stat*, int);
#endif

#ifdef HAVE_RENAMEAT
#define gd_RenameAt(d,...) renameat(__VA_ARGS__)
#else
int gd_RenameAt(const DIRFILE *D, int, const char*, int, const char*);
#endif

#ifdef HAVE_UNLINKAT
# define gd_UnlinkAt(d,...) unlinkat(__VA_ARGS__)
#else
int gd_UnlinkAt(const DIRFILE*, int, const char*, int);
#endif

#ifdef HAVE_FSTATAT64
# define gd_StatAt64(d,...) fstatat64(__VA_ARGS__)
#else
int gd_StatAt64(const DIRFILE*, int, const char*, gd_stat64_t*, int);
#endif

#if ! HAVE_DECL_STRERROR_R
#ifdef STRERROR_R_CHAR_P
char* strerror_r(int, char*, size_t);
#else
int strerror_r(int, char*, size_t);
#endif
#endif

#if defined __MSVCRT__ && defined HAVE__UNLINK
#define unlink _unlink
#endif

#if defined __MSVCRT__ && defined HAVE__WRITE
#define write _write
#endif

#ifndef HAVE_GETDELIM
ssize_t getdelim(char**, size_t*, int, FILE*);
#endif


/* maximum number of recursions */
#define GD_MAX_RECURSE_LEVEL  32

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 5) /* for META lincom */

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
#define GD_E_DOMAIN_ANTITONIC   3

#define GD_E_OUT_OF_RANGE       1
#define GD_E_SINGULAR_RANGE     2

#define GD_E_DIM_FORMAT         1
#define GD_E_DIM_CALLER         2

#define GD_E_FLUSH_MKTMP        1
#define GD_E_FLUSH_OPEN         2
#define GD_E_FLUSH_RENAME       3

#define GD_E_VERS_NONE          1
#define GD_E_VERS_MISSING       2

struct _gd_raw_file {
  char* name;
  int fp;
  void* edata;
  int encoding;
  const DIRFILE *D;
};

struct _gd_lut {
  double x;
  union {
    double r;
    GD_DCOMPLEXM(c);
  } y;
};

/* Unified entry struct */
struct _gd_private_entry {
  gd_entry_t* entry[GD_MAX_LINCOM];
  int repr[GD_MAX_LINCOM];

  int calculated;

  int n_meta;
  int n_meta_string;
  int n_meta_carray;
  int n_meta_const;
  union {
    gd_entry_t** meta_entry;
    const gd_entry_t* parent;
  } p;

  /* field lists */
  const char** field_list;
  const char** vector_list;
  char** type_list[GD_N_ENTYPES];
  const char** string_value_list;
  void* const_value_list;
  gd_carray_t *carray_value_list;

  union {
    struct { /* RAW */
      char* filebase;
      size_t size;
      struct _gd_raw_file file[2]; /* encoding framework data */
    } raw;
    struct { /* LINTERP */
      char *table_file;
      int table_dirfd;
      int table_len;
      int complex_table;
      int table_monotonic;
      struct _gd_lut *lut;
    } linterp;
    struct { /* CONST */
      void *d;
      int n_client;
      gd_entry_t** client;
    } scalar;
    char* string;
  } u;
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

#ifndef HAVE_OFF64_T
# ifndef __APPLE__
typedef off_t off64_t;
# endif
# define lseek64 lseek
# define stat64 stat
#endif

/* helper macro */
#if defined ARM_ENDIAN_FLOATS || \
  ((defined WORDS_BIGENDIAN) ^ (defined FLOATS_BIGENDIAN))
#  define SCREWY_FLOATS
#endif

/* Encoding schemes */
extern struct encoding_t {
  unsigned long int scheme;
  const char* ext;
  int ecor; /* encoding requires byte-sex correction */
  const char* affix;
  const char* ffname;
  unsigned int provides;
  int (*open)(int, struct _gd_raw_file*, int, int);
  int (*close)(struct _gd_raw_file*);
  int (*touch)(int, struct _gd_raw_file*);
  off64_t (*seek)(struct _gd_raw_file*, off64_t, gd_type_t, int);
  ssize_t (*read)(struct _gd_raw_file*, void*, gd_type_t, size_t);
  off64_t (*size)(int, struct _gd_raw_file*, gd_type_t);
  ssize_t (*write)(struct _gd_raw_file*, const void*, gd_type_t,
      size_t);
  int (*sync)(struct _gd_raw_file*);
  int (*move)(int, struct _gd_raw_file*, int, char*);
  int (*unlink)(int, struct _gd_raw_file*);
  int (*temp)(int, int, struct _gd_raw_file*, int);
} _gd_ef[GD_N_SUBENCODINGS];

/* Format file fragment metadata */
struct gd_fragment_t {
  /* Canonical name (full path) */
  char* cname;
  /* Subdirectory name */
  const char* sname;
  /* External name (relative to the parent format file fragment) */
  char* ename;
  int modified;
  int parent;
  int dirfd;
  unsigned long int encoding;
  unsigned long int byte_sex;
  int protection;
  char* ref_name;
  off64_t frame_offset;
  uint32_t vers;
};

/* directory metadata */
struct gd_dir_t {
  char *path;
  int fd;
  int rc;
};

/* internal flags */
#define GD_HAVE_VERSION    0x40000000 /* have computed the version */
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
  /* library error data */
  int error;
  int suberror;
  char* error_string;
  char* error_file;
  int error_line;

  /* global data */
  unsigned long int flags;
  uint64_t av;
  int standards;
  int n_error;

  /* field counts */
  unsigned int n_entries;
  unsigned int n_string;
  unsigned int n_carray;
  unsigned int n_const;
  unsigned int n_meta;
  unsigned int n_dot;

  /* field array */
  gd_entry_t** entry;
  gd_entry_t** dot_list;

  /* the reference field */
  gd_entry_t* reference_field;

  /* directory name */
  char* name;

  /* directory list */
  unsigned int ndir;
  struct gd_dir_t *dir;

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
  gd_carray_t *carray_value_list;
  int list_validity;
  int type_list_validity;

  /* syntax error callback */
  gd_parser_callback_t sehandler;
  void* sehandler_extra;
};

extern const gd_entype_t _gd_entype_index[GD_N_ENTYPES];

void* _GD_Alloc(DIRFILE* D, gd_type_t type, size_t n);
void _GD_ArmEndianise(uint64_t* databuffer, int is_complex, size_t ns);
int _GD_BadInput(DIRFILE* D, gd_entry_t* E, int i);
int _GD_CalculateEntry(DIRFILE* D, gd_entry_t* E);
void _GD_CInvertData(DIRFILE* D, void* data, gd_type_t return_type,
    GD_DCOMPLEXA(dividend), size_t n_read);

/* _GD_ClearError: Everything's A-OK; clear the last error. */
#define _GD_ClearError(D) (D)->error = 0

void _GD_CLincomData(DIRFILE* D, int n, void* data1, gd_type_t return_type,
    GD_DCOMPLEXP(data2), GD_DCOMPLEXP(data3), GD_DCOMPLEXV(m), GD_DCOMPLEXV(b),
    gd_spf_t *spf, size_t n_read);
void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, size_t n) gd_nothrow;
gd_type_t _GD_ConstType(DIRFILE *D, gd_type_t type);
const char *_GD_DirName(const DIRFILE *D, int dirfd);
size_t _GD_DoField(DIRFILE*, gd_entry_t*, int, off64_t, size_t, gd_type_t,
    void*);
size_t _GD_DoFieldOut(DIRFILE*, gd_entry_t*, int, off64_t, size_t, gd_type_t,
    const void*);
gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code,
    gd_entry_t** list, unsigned int u, unsigned int *index);
gd_entry_t* _GD_FindFieldAndRepr(DIRFILE* D, const char* field_code_in,
    char** field_code, int* repr, unsigned int *index, int set);
uint64_t _GD_FindVersion(DIRFILE *D);
void _GD_FixEndianness(char* databuffer, size_t size, size_t ns);
void _GD_Flush(DIRFILE* D, gd_entry_t *E);
void _GD_FlushMeta(DIRFILE* D, int fragment, int force);
void _GD_FreeE(DIRFILE *D, gd_entry_t* E, int priv);
char *_GD_GetLine(FILE *fp, size_t *n, int* linenum);
int _GD_GetRepr(DIRFILE*, const char*, char**);
gd_spf_t _GD_GetSPF(DIRFILE* D, gd_entry_t* E);
int _GD_GrabDir(DIRFILE *D, int, const char *name);
int _GD_Include(DIRFILE* D, const char* ename, const char* format_file,
    int linenum, char** ref_name, int me, int* standards, unsigned long *flags);
void _GD_InitialiseFramework(void);
void _GD_InvertData(DIRFILE* D, void* data, gd_type_t return_type,
    double dividend, size_t n_read);
void _GD_InsertSort(DIRFILE* D, gd_entry_t* E, int u) gd_nothrow;

#define _GD_InternalError(D) \
  _GD_SetError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL)

gd_type_t _GD_LegacyType(char c);
void _GD_LincomData(DIRFILE* D, int n, void* data1, gd_type_t return_type,
    double *data2, double *data3, double* m, double *b, gd_spf_t *spf,
    size_t n_read);
void _GD_LinterpData(DIRFILE* D, void *data, gd_type_t type, int complex_table,
    const double *data_in, size_t npts, const struct _gd_lut *lut, size_t n_ln);
#define _GD_MakeFullPath gd_MakeFullPath
char *_GD_MakeFullPath(const DIRFILE *D, int dirfd, const char *name);
int gd_MakeTempFile(const DIRFILE*, int, char*);
int _GD_MissingFramework(int encoding, unsigned int funcs);
int _GD_MogrifyFile(DIRFILE* D, gd_entry_t* E, unsigned long int encoding,
    unsigned long int byte_sex, off64_t offset, int finalise, int new_fragment,
    char* new_filebase);
gd_type_t _GD_NativeType(DIRFILE* D, gd_entry_t* E, int repr);
gd_entry_t* _GD_ParseFieldSpec(DIRFILE* D, int n_cols, char** in_cols,
    const gd_entry_t* P, const char* format_file, int linenum, int me,
    int standards, int creat, unsigned long flags, int insert, char **outstring,
    const char *tok_pos);
char* _GD_ParseFragment(FILE* fp, DIRFILE *D, int me, int* standards,
    unsigned long int *flags);
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E);
void _GD_ReleaseDir(DIRFILE *D, int dirfd);
int _GD_SetEncodedName(DIRFILE* D, struct _gd_raw_file* file, const char* base,
    int temp);
void _GD_SetError(DIRFILE* D, int error, int suberror, const char* format_file,
    int line, const char* token);
int _GD_SetTablePath(DIRFILE *D, gd_entry_t *E, struct _gd_private_entry *e);
int _GD_Supports(DIRFILE* D, gd_entry_t* E, unsigned int funcs);
int _GD_Tokenise(DIRFILE *D, const char* instring, char **outstring,
    const char **pos, char** in_cols, const char* format_file, int linenum,
    int standards, int pedantic);
char* _GD_ValidateField(const gd_entry_t* parent, const char* field_code,
    int standards, int pedantic, int* is_dot);

/* generic I/O methods */
int _GD_GenericTouch(int, struct _gd_raw_file* file);
int _GD_GenericMove(int, struct _gd_raw_file* file, int, char* new_path);
int _GD_GenericUnlink(int, struct _gd_raw_file* file);

/* unencoded I/O methods */
int _GD_RawOpen(int, struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_RawSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_RawRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
ssize_t _GD_RawWrite(struct _gd_raw_file* file, const void *ptr,
    gd_type_t data_type, size_t nmemb);
int _GD_RawSync(struct _gd_raw_file* file);
int _GD_RawClose(struct _gd_raw_file* file);
off64_t _GD_RawSize(int, struct _gd_raw_file* file, gd_type_t data_type);
int _GD_RawTemp(int, int, struct _gd_raw_file *file, int mode);

/* text I/O methods */
int _GD_AsciiOpen(int, struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_AsciiSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_AsciiRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
ssize_t _GD_AsciiWrite(struct _gd_raw_file* file, const void *ptr,
    gd_type_t data_type,
    size_t nmemb);
int _GD_AsciiSync(struct _gd_raw_file* file);
int _GD_AsciiClose(struct _gd_raw_file* file);
off64_t _GD_AsciiSize(int, struct _gd_raw_file* file, gd_type_t data_type);
int _GD_AsciiTemp(int, int, struct _gd_raw_file *file, int mode);

/* bzip I/O methods */
int _GD_Bzip2Open(int, struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_Bzip2Seek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_Bzip2Read(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_Bzip2Close(struct _gd_raw_file* file);
off64_t _GD_Bzip2Size(int, struct _gd_raw_file* file, gd_type_t data_type);

/* gzip I/O methods */
int _GD_GzipOpen(int, struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_GzipSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_GzipRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_GzipClose(struct _gd_raw_file* file);
off64_t _GD_GzipSize(int, struct _gd_raw_file* file, gd_type_t data_type);

/* lzma I/O methods */
int _GD_LzmaOpen(int, struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_LzmaSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_LzmaRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_LzmaClose(struct _gd_raw_file* file);
off64_t _GD_LzmaSize(int, struct _gd_raw_file* file, gd_type_t data_type);

/* slim I/O methods */
int _GD_SlimOpen(int, struct _gd_raw_file* file, int mode, int creat);
off64_t _GD_SlimSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, int pad);
ssize_t _GD_SlimRead(struct _gd_raw_file* file, void *ptr, gd_type_t data_type,
    size_t nmemb);
int _GD_SlimClose(struct _gd_raw_file* file);
off64_t _GD_SlimSize(int, struct _gd_raw_file* file, gd_type_t data_type);

#ifdef _MSC_VER
# define _gd_static_inline static
#else
# define _gd_static_inline static inline
#endif
_gd_static_inline int entry_cmp(const void *a, const void *b)
{
  return strcmp((*(gd_entry_t**)a)->field, (*(gd_entry_t**)b)->field);
}

#ifndef __cplusplus
# undef gd_nothrow
# define gd_nothrow
#endif

#ifdef GD_C89_API
# define EN(t,v) u.t.v
#else
# define EN(t,v) v
#endif

#endif
