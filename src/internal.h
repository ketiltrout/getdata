/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2012 D. V. Wiebe
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

/* OS X 10.6 deprecates lstat64 */
#ifdef HAVE_AVAILABILITY_H
#include <Availability.h>
#endif

#if defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && \
  (__MAC_OS_X_VERSION_MIN_REQUIRED >= 1060)
# undef HAVE_LSTAT64
# undef HAVE_STRUCT_STAT64
# ifndef _DARWIN_FEATURE_64_BIT_INODE
#  define _DARWIN_FEATURE_64_BIT_INODE
# endif
#endif

/* library headers */
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <time.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* MSVC types */
#ifdef _MSC_VER
#ifdef _WIN64
typedef __int64 ssize_t;
#else
typedef int ssize_t;
#endif
typedef int mode_t;
#endif

#ifndef HAVE_OFF64_T
typedef off_t off64_t;
#endif

#ifdef _MSC_VER
/* missing in sys/stat.h */
#define S_ISREG(m)  (((m) & _S_IFMT) == _S_IFREG)
#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#endif

/* the open() in the MSVCRT doesn't permit open()ing directories */
#ifdef __MSVCRT__
#define GD_NO_DIR_OPEN
#endif

#ifdef GD_NO_C99_API
#  define GD_DCOMPLEXP_t double *
#  define GD_DCOMPLEXA(v) double v[2]
#  define GD_DCOMPLEXV(v) double v[restrict][2]
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
#  define GD_DCOMPLEXV(v) double _Complex *restrict v
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

#ifndef PATH_MAX
# ifdef _POSIX_PATH_MAX
#  define PATH_MAX _POSIX_PATH_MAX
# elif defined MAXPATHLEN
#  define PATH_MAX MAXPATHLEN
# else
/* POSIX says we're supposed to check _pathconf in this case, but it goes on to
 * say that the PATH_MAX value reported by _pathconf isn't guaranteed to be
 * suitable for mallocing, so its not clear what they're trying to do there.
 * The following will have to do.
 */
#  define PATH_MAX 4096
# endif
#endif

#if SIZEOF_INT < 4
#define GD_BUFFER_SIZE 32767
#else
#define GD_BUFFER_SIZE 1000000
#endif

/* the lookback length (in mplex counter revolutions) to search for the previous
 * mplex value */
#define GD_MPLEX_LOOKBACK 10

#ifdef _MSC_VER
# define _gd_static_inline static
#else
# define _gd_static_inline static inline
#endif

/* unaligned access */
#ifdef UNALIGNED_ACCESS_OK
#define gd_get_unaligned64(p) (*(p))
#define gd_put_unaligned64(v,p) *(p) = (v)
#else
#ifdef HAVE_ASM_UNALIGNED_H
#include <asm/unaligned.h>
#endif
#if defined HAVE_DECL_GET_UNALIGNED && HAVE_DECL_GET_UNALIGNED == 1
#define gd_get_unaligned64 get_unaligned
#else
_gd_static_inline int64_t gd_get_unaligned64(const void *p)
{
  int64_t v;
  memcpy(&v, p, 8);
  return v;
}
#endif
#if defined HAVE_DECL_PUT_UNALIGNED && HAVE_DECL_PUT_UNALIGNED == 1
#define gd_put_unaligned64 put_unaligned
#else
_gd_static_inline int64_t gd_put_unalinged64(int64_t v, void *p)
{
  memcpy(p, &v, 8);
  return v;
}
#endif
#endif

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
const char* gd_colnil(void);
const char* gd_coladd(void);
const char* gd_colsub(void);
#define dtracevoid() printf("%s %s()\n", gd_coladd(), __FUNCTION__)
#define dtrace(fmt, ...) printf("%s %s(" fmt ")\n", gd_coladd(), \
    __FUNCTION__, ##__VA_ARGS__)
#define dprintf(fmt, ...) printf("%s %s:%i " fmt "\n", gd_colnil(), \
    __FUNCTION__, __LINE__, ##__VA_ARGS__)
#define dreturnvoid() printf("%s %s = (nil)\n", gd_colsub(), __FUNCTION__)
#define dreturn(fmt, ...) printf("%s %s = " fmt "\n", gd_colsub(), \
    __FUNCTION__, ##__VA_ARGS__)
#define dwatch(fmt, v) printf("%s %s = " fmt "\n", gd_colnil(), #v, v)
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

#ifdef HAVE__GETCWD
# define gd_getcwd _getcwd
#elif defined HAVE_GETCWD
# define gd_getcwd getcwd
#else
# define gd_getcwd(...) (NULL)
#endif

#ifndef HAVE_SNPRINTF
# ifdef HAVE__SNPRINTF
#  define snprintf _snprintf
# endif
#endif

#ifndef HAVE_FTELLO64
#  ifndef HAVE_FTELLO
#    define ftello64 (off64_t)ftell
#  else
#    define ftello64 (off64_t)ftello
#  endif
#endif

#ifndef HAVE_LSTAT64
# ifdef HAVE_LSTAT
#  define lstat64 lstat
#  define HAVE_LSTAT64
# endif
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
struct tm *gmtime_r(const time_t *timep, struct tm *result);
#endif

#ifndef HAVE_LSEEK64
# ifdef HAVE__LSEEKI64
#  define lseek64 (off64_t)_lseeki64
# else
#  define lseek64 (off64_t)lseek
# endif
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

#if HAVE_STRUCT_STAT64
typedef struct stat64 gd_stat64_t;
#elif HAVE_STRUCT__STAT64
typedef struct _stat64 gd_stat64_t;
#elif HAVE_STRUCT___STAT64
typedef struct __stat64 gd_stat64_t;
#else
typedef struct stat gd_stat64_t;
#define GD_NO_64BIT_STAT
#endif

#ifdef GD_NO_64BIT_STAT
# define gd_stat64 stat
# define gd_fstat64 fstat
#else
# if HAVE_STAT64
#  define gd_stat64 stat64
# elif HAVE__STAT64
#  define gd_stat64 _stat64
# else
#  define gd_stat64 stat
# endif

# if HAVE_FSTAT64
#  define gd_fstat64 fstat64
# elif HAVE__FSTAT64
#  define gd_fstat64 _fstat64
# elif HAVE__FSTAT
#  define gd_fstat64 _fstat
# else
#  define gd_fstat64 fstat
# endif
#endif

#ifdef HAVE_OPENAT
# ifdef GETDATA_DEBUG
#  define gd_unused_d /**/
# else
#  define gd_unused_d __gd_unused
# endif
# define gd_OpenAt(d,...) openat(__VA_ARGS__)
#else
# define gd_unused_d /**/
int gd_OpenAt(const DIRFILE*, int, const char*, int, mode_t);
#endif

#ifdef HAVE_FSTATAT
# define gd_StatAt(d,...) fstatat(__VA_ARGS__)
#else
#ifndef HAVE_SYS_STAT_H
struct stat;
#endif
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

#if defined HAVE_FTRUNCATE64
#define gd_truncate ftruncate64
#elif defined HAVE_FTRUNCATE
#define gd_truncate(d,l) ftruncate(d, (off_t)l)
#elif defined HAVE__CHSIZE_S
#define gd_truncate(d,l) _chsize_s(d, (int64_t)l)
#elif defined HAVE__CHSIZE
#define gd_truncate(d,l) _chsize(d, (long)l)
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

/* byte swapping */
#ifdef HAVE_BYTESWAP_H
#include <byteswap.h>
#endif
#ifdef HAVE_SYS_ENDIAN_H
#include <sys/endian.h>
#endif
#ifdef HAVE_LIBKERN_OSBYTEORDER_H
#include <libkern/OSByteOrder.h>
#endif
#if defined HAVE_DECL_BSWAP_16 && HAVE_DECL_BSWAP_16 == 1
# define gd_swap16 bswap_16
# define gd_swap32 bswap_32
# define gd_swap64 bswap_64
#elif defined HAVE_DECL_OSSWAPINT16 && HAVE_DECL_OSSWAPINT16 == 1
# define gd_swap16 OSSwapInt16
# define gd_swap32 OSSwapInt32
# define gd_swap64 OSSwapInt64
#elif defined HAVE_DECL_BSWAP16 && HAVE_DECL_BSWAP16 == 1
# define gd_swap16 bswap16
# define gd_swap32 bswap32
# define gd_swap64 bswap64
#else
# define gd_swap16(x) (((uint16_t)(x) << 8) | ((uint16_t)(x) >> 8))
# define gd_swap32(x) ( \
    (((uint32_t)(x) << 24) & 0xff000000UL) | \
    (((uint32_t)(x) << 8)  & 0xff0000UL) | \
    (((uint32_t)(x) >> 8)  & 0xff00UL) | \
    ((uint32_t)(x) >> 24))
#define gd_swap64(x) ( \
     (((uint64_t)(x) << 56) | \
     (((uint64_t)(x) << 40) & 0xff000000000000ULL) | \
     (((uint64_t)(x) << 24) & 0xff0000000000ULL) | \
     (((uint64_t)(x) << 8)  & 0xff00000000ULL) | \
     (((uint64_t)(x) >> 8)  & 0xff000000ULL) | \
     (((uint64_t)(x) >> 24) & 0xff0000ULL) | \
     (((uint64_t)(x) >> 40) & 0xff00ULL) | \
     ((uint64_t)(x)  >> 56)))
#endif

/* path malarkey */
#if defined _WIN32 || defined _WIN64
# define _GD_AbsPath(s)  ((s)[0] == '/' || (s)[0] == GD_DIRSEP || \
                                  ((s)[0] != '\0' && (s)[1] == ':'))
# define _GD_Root(s,d,l) \
  do { \
    if ((d)[0] == '/' || (d)[0] == GD_DIRSEP) { \
      (s)[0] = (d)[0]; \
      l = 1; \
      if ((d)[1] == '/' || (d)[1] == GD_DIRSEP) { \
        (s)[1] = (d)[1]; \
        l = 2; \
      } \
    } else { \
      (s)[0] = (d)[0]; \
      (s)[1] = ':'; \
      (s)[2] = '\\'; \
      (s)[3] = '\0'; \
      l = 3; \
    } \
  } while (0)
# define _GD_RootLen(d) ( \
    ((d)[0] == '/' || (d)[0] == GD_DIRSEP) ? \
    ((d)[1] == '/' || (d)[1] == GD_DIRSEP) ? 2 : 1 : 3 \
    )
#else
# define _GD_AbsPath(s)  ((s)[0] == '/')
# define _GD_Root(s,d,l) \
  do { \
    (s)[0] = '/'; \
    (s)[1] = '\0'; \
    l = 1; \
  } while (0)
# define _GD_RootLen(d) 1
#endif

/* maximum number of recursions */
#define GD_MAX_RECURSE_LEVEL  32

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 5) /* for META lincom */

/* Suberror codes */
/* GD_E_FORMAT suberrors are in getdata.h */

#define GD_E_OPEN_NOT_EXIST    1
#define GD_E_OPEN_NOT_DIRFILE  2
#define GD_E_OPEN_NO_ACCESS    3
#define GD_E_OPEN_PATH         4

#define GD_E_TRUNC_STAT        1
#define GD_E_TRUNC_UNLINK      2
#define GD_E_TRUNC_DIR         3

#define GD_E_CREAT_FORMAT      1
#define GD_E_CREAT_EXCL        2
#define GD_E_CREAT_DIR         3
#define GD_E_CREAT_OPEN        4

#define GD_E_CODE_MISSING      1
#define GD_E_CODE_INVALID      2

#define GD_E_LINFILE_LENGTH    1
#define GD_E_LINFILE_OPEN      2

#define GD_E_RECURSE_CODE       1
#define GD_E_RECURSE_INCLUDE    2

#define GD_E_FIELD_PUT         1
#define GD_E_FIELD_BAD         2
#define GD_E_FIELD_MATCH       3

#define GD_E_ENTRY_TYPE      1
#define GD_E_ENTRY_METARAW   2
#define GD_E_ENTRY_SPF       3
#define GD_E_ENTRY_NFIELDS   4
#define GD_E_ENTRY_NUMBITS   5
#define GD_E_ENTRY_BITNUM    6
#define GD_E_ENTRY_BITSIZE   7
#define GD_E_ENTRY_POLYORD   8
#define GD_E_ENTRY_WINDOP    9
#define GD_E_ENTRY_CNTVAL   10
#define GD_E_ENTRY_CNTMAX   11

#define GD_E_SCALAR_CODE        1
#define GD_E_SCALAR_TYPE        2

#define GD_E_REFERENCE_CODE     1
#define GD_E_REFERENCE_TYPE     2

#define GD_E_PROTECTED_FORMAT   1
#define GD_E_PROTECTED_DATA     2

#define GD_E_DEL_META           1
#define GD_E_DEL_CONST          2
#define GD_E_DEL_DERIVED        3
#define GD_E_DEL_ALIAS          4

#define GD_E_REPR_UNKNOWN       1
#define GD_E_REPR_PUT           2

#define GD_E_DOMAIN_COMPLEX     1
#define GD_E_DOMAIN_EMPTY       2
#define GD_E_DOMAIN_ANTITONIC   3
#define GD_E_DOMAIN_MULTIPOS    4

#define GD_E_OUT_OF_RANGE       1
#define GD_E_SINGULAR_RANGE     2

#define GD_E_DIM_FORMAT         1
#define GD_E_DIM_CALLER         2

#define GD_E_FLUSH_MKTMP        1
#define GD_E_FLUSH_OPEN         2
#define GD_E_FLUSH_RENAME       3

#define GD_E_UNENC_UNDET        1
#define GD_E_UNENC_TARGET       2

#define GD_E_VERS_NONE          1
#define GD_E_VERS_MISSING       2

#define GD_E_ARG_WHENCE         1
#define GD_E_ARG_ENDIANNESS     2
#define GD_E_ARG_PROTECTION     3
#define GD_E_ARG_NODATA         4

#define GD_FILE_READ  0x1
#define GD_FILE_WRITE 0x2
#define GD_FILE_RDWR  ( GD_FILE_READ | GD_FILE_WRITE )
#define GD_FILE_TEMP  0x4
#define GD_FILE_TOUCH 0x8

struct _gd_raw_file {
  char* name;
  int idata;
  void* edata;
  int subenc;
  const DIRFILE *D;
  unsigned int mode;
  off64_t pos;
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
  unsigned int n_hidden;
  unsigned int n[GD_N_ENTYPES];
  union {
    gd_entry_t** meta_entry;
    const gd_entry_t* parent;
  } p;

  /* field lists */
  const char **alias_list;
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
    struct { /* MPLEX */
      gd_type_t type;
      off64_t sample;
      char d[16];
    } mplex;
    char *string; /* STRING */
    off64_t index_pos; /* INDEX */
  } u;
};

#define GD_ENC_NONE       0
#define GD_ENC_SLIM       1
#define GD_ENC_GZ_RAW     2
#define GD_ENC_BZ2_RAW    3
#define GD_ENC_ASCII      4
#define GD_ENC_LZMA_RAW   5
#define GD_ENC_XZ_RAW     6
#define GD_ENC_SIE        7
#define GD_ENC_ZZIP       8
#define GD_ENC_ZZSLIM     9
#define GD_ENC_UNKNOWN   10

#define GD_N_SUBENCODINGS (GD_ENC_UNKNOWN + 1)

#define GD_EF_NAME    0x0001
#define GD_EF_OPEN    0x0002
#define GD_EF_CLOSE   0x0004
#define GD_EF_SEEK    0x0008
#define GD_EF_READ    0x0010
#define GD_EF_SIZE    0x0020
#define GD_EF_WRITE   0x0040
#define GD_EF_SYNC    0x0080
#define GD_EF_MOVE    0x0100
#define GD_EF_UNLINK  0x0200

#define GD_FINIRAW_KEEP      0x0
#define GD_FINIRAW_DISCARD   0x1
#define GD_FINIRAW_DEFER     0x2
#define GD_FINIRAW_CLOTEMP   0x4

#define BUFFER_SIZE 9000000

/* helper macro */
#if defined ARM_ENDIAN_FLOATS || \
  ((defined WORDS_BIGENDIAN) ^ (defined FLOATS_BIGENDIAN))
#  define SCREWY_FLOATS
#endif

typedef int (*gd_ef_name_t)(DIRFILE *D, const char *, struct _gd_raw_file*,
    const char*, int, int);
typedef int (*gd_ef_open_t)(int, struct _gd_raw_file*, int, unsigned int);
typedef off64_t (*gd_ef_seek_t)(struct _gd_raw_file*, off64_t, gd_type_t,
    unsigned int);
typedef off64_t (*gd_ef_size_t)(int, struct _gd_raw_file*, gd_type_t, int);
typedef ssize_t (*gd_ef_read_t)(struct _gd_raw_file*, void*, gd_type_t, size_t);
typedef ssize_t (*gd_ef_write_t)(struct _gd_raw_file*, const void*, gd_type_t,
    size_t);
typedef int (*gd_ef_close_t)(struct _gd_raw_file*);
typedef int (*gd_ef_sync_t)(struct _gd_raw_file*);
typedef int (*gd_ef_unlink_t)(int, struct _gd_raw_file*);
typedef int (*gd_ef_move_t)(int, struct _gd_raw_file*, int, char*);

/* Encoding scheme flags */
#define GD_EF_ECOR 0x1 /* post-framework byte-sex correction required */
#define GD_EF_SWAP 0x2 /* in-framework byte-sex metadata correction required */
#define GD_EF_OOP  0x4 /* writes occur out-of-place */
#define GD_EF_EDAT 0x8 /* The /ENCODING datum is used */
/* Encoding schemes */
extern struct encoding_t {
  unsigned long int scheme;
  const char* ext;
  int flags; /* flags */
  const char* affix;
  const char* ffname;
  unsigned int provides;
  gd_ef_name_t name;
  gd_ef_open_t open;
  gd_ef_close_t close;
  gd_ef_seek_t seek;
  gd_ef_read_t read;
  gd_ef_size_t size;
  gd_ef_write_t write;
  gd_ef_sync_t sync;
  gd_ef_move_t move;
  gd_ef_unlink_t unlink;
} _gd_ef[GD_N_SUBENCODINGS];

/* Format file fragment metadata */
struct gd_fragment_t {
  /* Canonical name (full path) */
  char* cname;
  /* Subdirectory name */
  const char* sname;
  /* basename */
  char *bname;
  /* External name (the one that appears in the format file) */
  char* ename;
  void *enc_data;
  int modified;
  int parent;
  int dirfd;
  unsigned long int encoding;
  unsigned long int byte_sex;
  int protection;
  char* ref_name;
  off64_t frame_offset;
  uint32_t vers;

  char *prefix;
  char *suffix;
};

/* directory metadata */
struct gd_dir_t {
  char *path;
  int fd;
  int rc;
};

/* internal flags */
#define GD_MULTISTANDARD   0x20000000 /* have multiple standards in format */
#define GD_HAVE_VERSION    0x40000000 /* have computed the version */
#define GD_INVALID         0x80000000 /* the dirfile is invalid */

/* aliases */
#define GD_ALIAS_ENTRY ((gd_entype_t)-1)

#define GD_LIST_VALID_FIELD        0x01
#define GD_LIST_VALID_VECTOR       0x02
#define GD_LIST_VALID_STRING_VALUE 0x04

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

  /* for the public tokeniser */
  const char *tok_pos;

  /* field counts */
  unsigned int n_entries;
  unsigned int n_hidden;
  unsigned int n[GD_N_ENTYPES];
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

/* forward declarations */
void *_GD_Alloc(DIRFILE*, gd_type_t, size_t) __attribute_malloc__;
void _GD_ArmEndianise(uint64_t*, int, size_t);
int _GD_BadInput(DIRFILE *, const gd_entry_t *, int, int);

#define _GD_BadWindop(op) \
  ( \
   (op != GD_WINDOP_EQ) && (op != GD_WINDOP_GE) && (op != GD_WINDOP_GT) && \
   (op != GD_WINDOP_LE) && (op != GD_WINDOP_LT) && (op != GD_WINDOP_NE) && \
   (op != GD_WINDOP_SET) && (op != GD_WINDOP_CLR) \
  )

int _GD_CalculateEntry(DIRFILE *restrict, gd_entry_t *restrict, int);
char *_GD_CanonicalPath(const char *restrict, const char *restrict);
gd_entry_t *_GD_CheckParent(DIRFILE *restrict D, char **restrict name, int me,
    int linenum);
void _GD_CInvertData(DIRFILE *restrict, void *restrict, gd_type_t return_type,
    GD_DCOMPLEXA(dividend), size_t);

/* _GD_ClearError: Everything's A-OK; clear the last error. */
#define _GD_ClearError(D) (D)->error = 0

void _GD_CLincomData(DIRFILE *restrict, int, void *restrict, gd_type_t,
    const GD_DCOMPLEXP_t restrict, const GD_DCOMPLEXP_t restrict,
    GD_DCOMPLEXV(m), GD_DCOMPLEXV(b), const gd_spf_t *restrict, size_t);
void _GD_ConvertType(DIRFILE *restrict, const void *restrict, gd_type_t,
    void *restrict, gd_type_t, size_t) gd_nothrow;
gd_type_t _GD_ConstType(DIRFILE *D, gd_type_t type);
const char *_GD_DirName(const DIRFILE *D, int dirfd);

#define _GD_EntryIndex(t) \
  ( \
    ((t) == GD_RAW_ENTRY)      ?  0 : ((t) == GD_LINCOM_ENTRY)   ?  1 : \
    ((t) == GD_LINTERP_ENTRY)  ?  2 : ((t) == GD_BIT_ENTRY)      ?  3 : \
    ((t) == GD_MULTIPLY_ENTRY) ?  4 : ((t) == GD_PHASE_ENTRY)    ?  5 : \
    ((t) == GD_INDEX_ENTRY)    ?  6 : ((t) == GD_POLYNOM_ENTRY)  ?  7 : \
    ((t) == GD_SBIT_ENTRY)     ?  8 : ((t) == GD_DIVIDE_ENTRY)   ?  9 : \
    ((t) == GD_RECIP_ENTRY)    ? 10 : ((t) == GD_WINDOW_ENTRY)   ? 11 : \
    ((t) == GD_MPLEX_ENTRY)    ? 12 : ((t) == GD_CONST_ENTRY)    ? 13 : \
    ((t) == GD_CARRAY_ENTRY)   ? 14 : ((t) == GD_STRING_ENTRY)   ? 15 : -1 \
  )

size_t _GD_DoField(DIRFILE *restrict, gd_entry_t *restrict, int, off64_t,
    size_t, gd_type_t, void *restrict);
size_t _GD_DoFieldOut(DIRFILE *restrict, gd_entry_t *restrict, int, off64_t,
    size_t, gd_type_t, const void *restrict);
int _GD_EntryCmp(const void*, const void*);
gd_entry_t *_GD_FindField(const DIRFILE *restrict, const char *restrict,
    gd_entry_t *const *, unsigned int, int, unsigned int *restrict);
gd_entry_t *_GD_FindFieldAndRepr(DIRFILE *restrict, const char *restrict,
    char **restrict, int *restrict, unsigned int *restrict, int, int);
uint64_t _GD_FindVersion(DIRFILE *D);
void _GD_FixEndianness(void* databuffer, size_t size, size_t ns);
#ifdef WORDS_BIGENDIAN
#define _GD_FileSwapBytes(D,i) ((D)->fragment[i].byte_sex & GD_LITTLE_ENDIAN)
#else
#define _GD_FileSwapBytes(D,i) ((D)->fragment[i].byte_sex & GD_BIG_ENDIAN)
#endif
int _GD_FiniRawIO(DIRFILE*, const gd_entry_t*, int, int);
void _GD_Flush(DIRFILE *restrict, gd_entry_t *restrict, int);
void _GD_FlushMeta(DIRFILE* D, int fragment, int force);
void _GD_FreeE(DIRFILE *restrict, gd_entry_t *restrict, int);
off64_t _GD_GetEOF(DIRFILE *restrict, const gd_entry_t *restrict,
    const char *restrict, int *restrict);
off64_t _GD_GetFilePos(DIRFILE *restrict, gd_entry_t *restrict, off64_t);
char *_GD_GetLine(FILE *restrict, size_t *restrict, int *restrict);
int _GD_GetRepr(DIRFILE *restrict, const char *restrict,
    char **restrict, int);
gd_spf_t _GD_GetSPF(DIRFILE*, gd_entry_t*);
int _GD_GrabDir(DIRFILE*, int, const char *restrict);
int _GD_Include(DIRFILE*, const char *restrict, const char *restrict, int,
    char **restrict, int, const char *restrict, const char *restrict,
    int *restrict, unsigned long *restrict, int);
void _GD_InitialiseFramework(void);
int _GD_InitRawIO(DIRFILE*, const gd_entry_t*, const char*, int,
    const struct encoding_t*, unsigned int, unsigned int, int);
void _GD_InvertData(DIRFILE *restrict, void *restrict, gd_type_t return_type,
    double dividend, size_t n_read);
void _GD_InsertSort(DIRFILE *restrict, gd_entry_t *restrict, int u) gd_nothrow;

#define _GD_InternalError(D) \
  _GD_SetError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL)

gd_type_t _GD_LegacyType(char c);
void _GD_LincomData(DIRFILE *restrict, int n, void *restrict,
    gd_type_t return_type, const double *restrict, const double *restrict,
    const double *restrict, const double *restrict, const gd_spf_t *restrict,
    size_t);
void _GD_LinterpData(DIRFILE *restrict, void *restrict, gd_type_t, int,
    const double *restrict, size_t, const struct _gd_lut *restrict, size_t);
int _GD_ListEntry(const gd_entry_t*, int, int, gd_entype_t);
char *_GD_MakeFullPath(DIRFILE *restrict, int, const char *restrict, int);
#define _GD_MakeFullPathOnly gd_MakeFullPathOnly
char *_GD_MakeFullPathOnly(const DIRFILE *D, int dirfd, const char *name);
int _GD_MakeTempFile(const DIRFILE*, int, char*);
void *_GD_Malloc(DIRFILE *D, size_t size);
int _GD_MissingFramework(int encoding, unsigned int funcs);
int _GD_MogrifyFile(DIRFILE *restrict, gd_entry_t *restrict, unsigned long int,
    unsigned long int, off64_t, int, int, char *restrict);
char *_GD_MungeCode(DIRFILE *restrict, const gd_entry_t *restrict,
    const char *restrict, const char *restrict, const char *restrict,
    const char *restrict, const char *restrict, int *restrict);
char *_GD_MungeFromFrag(DIRFILE *restrict, const gd_entry_t *restrict, int,
    const char *restrict, int *restrict);
gd_type_t _GD_NativeType(DIRFILE *restrict, gd_entry_t *restrict, int);
gd_entry_t *_GD_ParseFieldSpec(DIRFILE *restrict, int, char**,
    const gd_entry_t *restrict, const char *restrict, int, int, int, int,
    unsigned long, int, char**, const char*);
char *_GD_ParseFragment(FILE *restrict, DIRFILE *restrict, int, int *restrict,
    unsigned long int *restrict, int);
void _GD_ReadLinterpFile(DIRFILE *restrict, gd_entry_t *restrict);
void *_GD_Realloc(DIRFILE *restrict, void *restrict, size_t size);
void _GD_ReleaseDir(DIRFILE *D, int dirfd);
int _GD_Seek(DIRFILE *restrict, gd_entry_t *restrict, off64_t offset,
    unsigned int mode);
void _GD_SetError(DIRFILE*, int, int, const char*, int, const char*);
int _GD_SetTablePath(DIRFILE *restrict, const gd_entry_t *restrict,
    struct _gd_private_entry *restrict);
int _GD_StrCmpNull(const char *restrict, const char *restrict);
char *_GD_Strdup(DIRFILE *restrict, const char *restrict);
int _GD_Supports(DIRFILE *, const gd_entry_t*, unsigned int funcs);
int _GD_Tokenise(DIRFILE *restrict, const char *restrict, char **,
    const char **, int, char **, const char *restrict, int linenum,
    int standards, int pedantic);
void _GD_UpdateAliases(DIRFILE*);
int _GD_ValidateField(const char*, int, int, int, int*);
off64_t _GD_WriteSeek(DIRFILE *restrict, gd_entry_t *restrict,
    const struct encoding_t *restrict, off64_t, unsigned int mode);
ssize_t _GD_WriteOut(DIRFILE*, const gd_entry_t*, const struct encoding_t*,
    const void*, gd_type_t, size_t, int);

/* generic I/O methods */
int _GD_GenericMove(int, struct _gd_raw_file *restrict, int, char *restrict);
int _GD_GenericName(DIRFILE *restrict, const char *restrict,
    struct _gd_raw_file *restrict, const char *restrict, int, int);
int _GD_GenericUnlink(int, struct _gd_raw_file* file);

/* unencoded I/O methods */
int _GD_RawOpen(int, struct _gd_raw_file* file, int swap, unsigned int);
off64_t _GD_RawSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_RawRead(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_RawWrite(struct _gd_raw_file *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_RawSync(struct _gd_raw_file* file);
int _GD_RawClose(struct _gd_raw_file* file);
off64_t _GD_RawSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* text I/O methods */
int _GD_AsciiOpen(int, struct _gd_raw_file* file, int swap, unsigned int);
off64_t _GD_AsciiSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_AsciiRead(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_AsciiWrite(struct _gd_raw_file *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_AsciiSync(struct _gd_raw_file* file);
int _GD_AsciiClose(struct _gd_raw_file* file);
off64_t _GD_AsciiSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* bzip I/O methods */
int _GD_Bzip2Open(int, struct _gd_raw_file* file, int swap, unsigned int);
off64_t _GD_Bzip2Seek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_Bzip2Read(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_Bzip2Close(struct _gd_raw_file* file);
off64_t _GD_Bzip2Size(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* gzip I/O methods */
int _GD_GzipOpen(int, struct _gd_raw_file* file, int swap, unsigned int);
off64_t _GD_GzipSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_GzipRead(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_GzipWrite(struct _gd_raw_file *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_GzipSync(struct _gd_raw_file* file);
int _GD_GzipClose(struct _gd_raw_file* file);
off64_t _GD_GzipSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* lzma I/O methods */
int _GD_LzmaOpen(int, struct _gd_raw_file* file, int swap, unsigned int);
off64_t _GD_LzmaSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_LzmaRead(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_LzmaClose(struct _gd_raw_file* file);
off64_t _GD_LzmaSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* slim I/O methods */
int _GD_SlimOpen(int, struct _gd_raw_file* file, int, unsigned int);
off64_t _GD_SlimSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_SlimRead(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_SlimClose(struct _gd_raw_file* file);
off64_t _GD_SlimSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* SIE I/O methods */
int _GD_SampIndOpen(int, struct _gd_raw_file* file, int swap,
    unsigned int);
off64_t _GD_SampIndSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_SampIndRead(struct _gd_raw_file *restrict, void *restrict,
    gd_type_t, size_t);
ssize_t _GD_SampIndWrite(struct _gd_raw_file *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_SampIndSync(struct _gd_raw_file* file);
int _GD_SampIndClose(struct _gd_raw_file* file);
off64_t _GD_SampIndSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* zzip I/O methods */
int _GD_ZzipName(DIRFILE *restrict, const char *restrict,
    struct _gd_raw_file *restrict, const char *restrict, int, int);
int _GD_ZzipOpen(int, struct _gd_raw_file* file, int swap, unsigned int);
off64_t _GD_ZzipSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_ZzipRead(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_ZzipClose(struct _gd_raw_file* file);
off64_t _GD_ZzipSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

/* zzslim I/O methods */
int _GD_ZzslimName(DIRFILE *restrict, const char *restrict,
    struct _gd_raw_file *restrict, const char *restrict, int, int);
int _GD_ZzslimOpen(int, struct _gd_raw_file* file, int swap, unsigned int);
off64_t _GD_ZzslimSeek(struct _gd_raw_file* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_ZzslimRead(struct _gd_raw_file *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_ZzslimClose(struct _gd_raw_file* file);
off64_t _GD_ZzslimSize(int, struct _gd_raw_file* file, gd_type_t data_type,
    int swap);

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
