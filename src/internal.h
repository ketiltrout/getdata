/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2017 D. V. Wiebe
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

#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif

#ifdef HAVE_CONFIG_H
#include "gd_config.h"
#endif

#define GD_64BIT_API
#include "getdata.h"

/* MacOS 10.6 deprecates the LFS transitional API */
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
#ifdef HAVE_FEATURES_H
#include <features.h>
#endif
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
#include <stddef.h>
#include <errno.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <time.h>
#endif

#ifdef HAVE_CRTDEFS_H
#include <crtdefs.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_INTTYPES_H
#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif
#include <inttypes.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif
#ifdef HAVE_REGEX_H
#include <regex.h>
#endif


/* MSCVRT defines size_t but not ssize_t */
#ifdef __MSVCRT__
#undef SIZEOF_SIZE_T
#ifdef _WIN64
#define SIZEOF_SIZE_T 8
typedef __int64 ssize_t;
#else
#define SIZEOF_SIZE_T 4
typedef int ssize_t;
#endif
#endif

#ifndef SIZEOF_UNSIGNED_LONG_LONG
#define SIZEOF_UNSIGNED_LONG_LONG 0
#endif

#if !defined(PRIuSIZE) || !defined(PRIdSIZE)
# ifdef __MSVCRT__
#  define GD_PRISIZE_PREFIX "I"
# elif defined (__GNUC__)
#  define GD_PRISIZE_PREFIX "z"
# elif SIZEOF_SIZE_T == SIZEOF_UNSIGNED_LONG
#  define GD_PRISIZE_PREFIX "l"
# elif SIZEOF_SIZE_T == SIZEOF_UNSIGNED_LONG_LONG
#  define GD_PRISIZE_PREFIX "ll"
# else
#  define GD_PRISIZE_PREFIX ""
# endif
#endif

#ifndef PRIuSIZE
# define PRIuSIZE GD_PRISIZE_PREFIX "u"
#endif

#ifndef PRIdSIZE
# define PRIdSIZE GD_PRISIZE_PREFIX "d"
#endif

#ifndef HAVE_OFF64_T
typedef gd_off64_t off64_t;
#endif

#ifdef __MSVCRT__
/* missing in sys/stat.h */
#define S_ISREG(m)  (((m) & _S_IFMT) == _S_IFREG)
#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#endif

/* the open() in the MSVCRT doesn't permit open()ing directories */
#ifdef __MSVCRT__
#define GD_NO_DIR_OPEN
/* rename open() 0 */
#define O_RDWR _O_RDWR
#define O_RDONLY _O_RDONLY
#define O_CREAT _O_CREAT
#define O_EXCL _O_EXCL
#define O_TRUNC _O_TRUNC
#endif

#ifdef GD_NO_C99_API
/* generic dcomplex pointer for passing around malloc'd vectors */
#  define GD_DCOMPLEXP_t double *
/* a dcomplex scalar */
#  define GD_DCOMPLEXA(v) double v[2]
/* used when passing the complex arrays from the entry struct */
#  define GD_DCOMPLEXV(v) double v[][2]
/* norm */
#  define cabs(z)  sqrt((z)[0] * (z)[0] + (z)[1] * (z)[1])
/* phase */
#  define carg(z)  atan2((z)[1], (z)[0])
/* real part of z */
#  define creal(z) ((z)[0])
/* imaginary part of z */
#  define cimag(z) ((z)[1])
/* real part of (*z) */
#  define crealp(z) creal(z)
/* imaginary part of (*z) */
#  define cimagp(z) cimag(z)
/* a pointer to element i of GD_DCOMPLEXP_t array a */
#  define gd_cap_(a,i) (((GD_DCOMPLEXP_t)(a)) + 2 * i)
/* a pointer to a complex scalar */
#  define gd_csp_(a) ((GD_DCOMPLEXP_t)a)
/* assign real two-element array b to scalar a */
#  define gd_ra2cs_(a,b) gd_cs2cs_(a,b)
/* assign scalar b to scalar a */
#  define gd_cs2cs_(a,b) do { (a)[0] = (b)[0]; (a)[1] = (b)[1]; } while(0)
/* assign scalar *b to scalar a */
#  define gd_cp2cs_(a,b) gd_cs2cs_(a,b)
/* assign scalar b to scalar (*a) */
#  define gd_cs2cp_(a,b) gd_cs2cs_(a,b)
/* assign b[i] to scalar a */
#  define gd_ca2cs_(a,b,i) gd_cs2cs_((a),(b) + 2 * i)
/* assign scalar (*b) to a[i] */
#  define gd_cp2ca_(a,i,b) do { \
  (a)[2 * i] = (b)[0]; (a)[2 * i + 1] = (b)[1]; \
} while(0)
/* assign complex pair (x;y) to scalar a */
#  define gd_li2cs_(a,x,y) do { (a)[0] = (x); (a)[1] = (y); } while(0)
/* assign complex pair (x;y) to scalar (*a) */
#  define gd_li2cp_(a,x,y) gd_li2cs_(a,x,y)
/* assign polar (r,p) to scalar a */
#  define gd_po2cs_(a,r,p) do { \
  (a)[0] = (r) * cos(p); (a)[1] = (r) * sin(p); \
} while (0)
/* assign polar (r,p) to scalar (*a) */
#  define gd_po2cp_(a,r,p) gd_po2cs_(a,r,p)
/* assign real scalar b to scalar a */
#  define gd_rs2cs_(a,b) gd_li2cs_(a,b,0)
/* assign real scalar b to scalar (*a) */
#  define gd_rs2cp_(a,b) gd_rs2cs_(a,b)
/* assign complex scalar b to a[i], both of type t */
#  define gd_cs2ca_(a,i,b,t) do { \
  ((t*)a)[2 * i] = (t)(b)[0]; ((t*)a)[2 * i + 1] = (t)(b)[1]; \
} while(0)
/* assign real scalar b to a[i], both of type t */
#  define gd_rs2ca_(a,i,b,t) do { \
  ((t*)a)[2 * i] = (t)(b); ((t*)a)[2 * i + 1] = 0; \
} while(0)
/* compare a to complex pair (x;y) */
#  define gd_ccmpl_(a,x,y) ((a)[0] == x && (a)[1] == y)
/* compare a to b */
#  define gd_ccmpc_(a,b) ((a)[0] == (b)[0] && (a)[1] == (b)[1])
#else
#  define GD_DCOMPLEXP_t double _Complex *
#  define GD_DCOMPLEXA(v) double _Complex v
#  define GD_DCOMPLEXV(v) double _Complex *restrict v
#  define crealp(z) creal(*(z))
#  define cimagp(z) cimag(*(z))
#  define gd_cap_(a,i) (a + i)
#  define gd_csp_(a) (&(a))
#  define gd_ra2cs_(a,b) a = *((double complex*)(b))
#  define gd_cs2cs_(a,b) a = b
#  define gd_cp2cs_(a,b) a = *(b)
#  define gd_cs2cp_(a,b) *(a) = b
#  define gd_ca2cs_(a,b,i) a = b[i]
#  define gd_cp2ca_(a,i,b) (a)[i] = *(b)
#  define gd_li2cs_(a,x,y) a = (x + _Complex_I * y)
#  define gd_li2cp_(a,x,y) *(a) = (x + _Complex_I * y)
#  define gd_po2cs_(a,r,p) a = (r) * cexp(p)
#  define gd_po2cp_(a,r,p) *(a) = (r) * cexp(p)
#  define gd_rs2cs_(a,b) a = b
#  define gd_rs2cp_(a,b) *(a) = b
#  define gd_cs2ca_(a,i,b,t) ((complex t*)a)[i] = (complex t)(b)
#  define gd_rs2ca_(a,i,b,t) gd_cs2ca_(a,i,b,t)
#  define gd_ccmpl_(a,x,y) (a == (x + _Complex_I * y))
#  define gd_ccmpc_(a,b) (a == b)

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

#ifndef NAN
# if HAVE_NAN
#  define NAN nan("")
# else
#  define NAN gd_strtod("NAN", NULL)
# endif
#endif

#ifdef GD_RESTRICT_ARRAY_OK
#define gd_restrict_arr restrict
#else
#define gd_restrict_arr
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

/* gd_type_t type for native integers */
#ifndef SIZEOF_INT
#define SIZEOF_INT (sizeof(int))
#endif

#ifndef SIZEOF_UNSIGNED_INT
#define SIZEOF_UNSIGNED_INT (sizeof(unsigned int))
#endif

#define GD_INT_TYPE ((gd_type_t)(SIZEOF_INT | GD_SIGNED))
#define GD_UINT_TYPE ((gd_type_t)(SIZEOF_UNSIGNED_INT))

/* a few integer limits */
#ifndef SIZEOF_SIZE_T
#define SIZEOF_SIZE_T (sizeof(size_t))
#endif

#define GD_INT64_MAX ((int64_t)((uint64_t)-1>>1))
#define GD_SSIZE_T_MAX ((ssize_t)((size_t)-1>>1))
#define GD_SIZE_T_MAX ((size_t)-1)

/* Maximum samples in getdata and putdata */
#define GD_TRANSACTION_MAX(t) \
  (GD_SIZE(t) ? (GD_SSIZE_T_MAX / GD_SIZE(t)) : GD_SSIZE_T_MAX)

/* default buffer size */
#if SIZEOF_INT < 4
#define GD_BUFFER_SIZE 32767
#else
#define GD_BUFFER_SIZE 9000000
#endif

/* the default mplex cycle length */
#define GD_MPLEX_CYCLE 10

#ifdef _MSC_VER
# define gd_static_inline_ static
#else
# define gd_static_inline_ static inline
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
gd_static_inline_ int64_t gd_get_unaligned64(const void *p)
{
  int64_t v;
  memcpy(&v, p, 8);
  return v;
}
#endif
#if defined HAVE_DECL_PUT_UNALIGNED && HAVE_DECL_PUT_UNALIGNED == 1
#define gd_put_unaligned64 put_unaligned
#else
gd_static_inline_ int64_t gd_put_unaligned64(int64_t v, void *p)
{
  memcpy(p, &v, 8);
  return v;
}
#endif
#endif

#define GD_ARM_FLAG (GD_ARM_ENDIAN | GD_NOT_ARM_ENDIAN)

/* Internal type conventions:
 *
 *  - samples per frame is always unsigned int
 *  - variables holding offsets or file sizes should be of type gd_off64_t
 *    (which may be simply off_t, if it's the right size)
 *  - variables holding object sizes or counts of items read or written should
 *    be of type size_t
 *  - public functions taking or returning types of off64_t should have both
 *    a off_t prototype and a gd_off64_t type prototype.
 */

#ifndef __attribute_malloc__
# define __attribute_malloc__
#endif

#ifndef __wur
# define __wur
#endif

#ifdef _MSC_VER
# define gd_unused_
#else
# define gd_unused_ __attribute__ (( unused ))
#endif

/* disable the "unspecified order" remark in ICC */
#ifdef __INTEL_COMPILER
#  pragma warning (disable : 981)
#endif

/* debugging macros */
#ifdef GETDATA_DEBUG
#define GD_COL_SIZE 100

#ifdef __cplusplus
extern "C" {
#endif

  const char* gd_colnil(void);
  const char* gd_coladd(void);
  const char* gd_colsub(void);
  void gd_colclear(void);

#ifdef __cplusplus
}
#endif

#ifdef WIN32
#define __func__ __FUNCTION__
#endif

#define dtracevoid() printf("%s %s()\n", gd_coladd(), __func__)
#define dtrace(fmt, ...) printf("%s %s(" fmt ")\n", gd_coladd(), \
    __func__, ##__VA_ARGS__)
#define dprintf(fmt, ...) printf("%s %s:%i " fmt "\n", gd_colnil(), \
    __func__, __LINE__, ##__VA_ARGS__)
#define dreturnvoid() printf("%s %s = (nil)\n", gd_colsub(), __func__)
#define dreturn(fmt, ...) printf("%s %s = " fmt "\n", gd_colsub(), \
    __func__, ##__VA_ARGS__)
#define dwatch(fmt, v) printf("%s %s = " fmt "\n", gd_colnil(), #v, v)
#else
#define gd_colclear()
#define dtracevoid()
#define dtrace(...)
#define dprintf(...)
#define dreturnvoid()
#define dreturn(...)
#define dwatch(...)
#endif

/* These doesn't return */
#define GD_SET_RETURN_ERROR(D,e,s,f,l,t) do { \
  _GD_SetError(D,e,s,f,l,t); \
  dreturn("%i", e); \
  return e; \
} while(0)

#define GD_RETURN_ERROR(D) do { \
  dreturn("%i", D->error); \
  return D->error; \
} while(0)

#define GD_RETURN_ERR_IF_INVALID(D) do { \
  if (D->flags & GD_INVALID) \
    GD_SET_RETURN_ERROR(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL); \
  else \
    _GD_ClearError(D); \
} while(0)

#define GD_RETURN_IF_INVALID(D,f,v) do { \
  if (D->flags & GD_INVALID) { \
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL); \
    dreturn(f, v); \
    return v; \
  } else \
    _GD_ClearError(D); \
} while(0)

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

#ifndef HAVE_BASENAME
char *basename(char *path);
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

#ifndef offsetof
#define offsetof(t,m) ((size_t)(((char*)&((t*)0)->m) - (char*)0))
#endif

/* glibc 2.24 deprecates readdir_r */
#if defined(__GLIBC__) && (__GLIBC__ > 2 || (__GLIBC__ == 2 \
      && defined(__GLIBC_MINOR__) && __GLIBC_MINOR__ >= 24))
#undef HAVE_READDIR_R
#endif

#ifdef HAVE_READDIR_R
# define _GD_ReadDir readdir_r
#else
int _GD_ReadDir(DIR *dirp, struct dirent *entry, struct dirent **result);
#endif

#ifdef HAVE__STRTOI64
#if HAVE_DECL__STRTOI64 == 0
__int64 _strtoi64(const char *nptr, char **endptr, int base);
#endif
#  define gd_strtoll _strtoi64
#elif defined(HAVE_STRTOLL)
#  define gd_strtoll strtoll
#elif defined(HAVE_STRTOQ)
#  define gd_strtoll strtoq
#else
#  define gd_strtoll strtol
#endif

#ifdef HAVE__STRTOUI64
#if HAVE_DECL__STRTOUI64 == 0
unsigned __int64 _strtoui64(const char *nptr, char **endptr, int base);
#endif
#  define gd_strtoull _strtoi64
#elif defined(HAVE_STRTOULL)
#  define gd_strtoull strtoull
#elif defined(HAVE_STRTOUQ)
#  define gd_strtoll strtouq
#else
#  define gd_strtoull strtoul
#endif

/* the MSVCRT's strtod is not POSIX compliant */
#ifdef __MSVCRT__
double gd_strtod(const char *nptr, char **endptr);
#else
#define gd_strtod strtod
#endif

#if defined __MSVCRT__ && defined HAVE__FDOPEN
#define fdopen _fdopen
#endif

#if !HAVE_FSYNC && HAVE__COMMIT
#  define fsync _commit
#endif

#ifndef __MINGW64_VERSION_MAJOR /* mingw-w64 has pthread.h*/
#ifndef HAVE_PTHREAD_H
#ifndef HAVE_GMTIME_R
struct tm *gmtime_r(const time_t *timep, struct tm *result);
#endif
#endif
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
# define gd_StatAt64 gd_StatAt
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

# ifdef HAVE_FSTATAT64
#  define gd_StatAt64(d,...) fstatat64(__VA_ARGS__)
# else
int gd_StatAt64(const DIRFILE*, int, const char*, gd_stat64_t*, int);
# endif
#endif

#ifdef HAVE_OPENAT
# ifdef GETDATA_DEBUG
#  define gd_unused_d /**/
# else
#  define gd_unused_d gd_unused_
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

#if defined HAVE_STERROR_R && ! defined HAVE_DECL_STRERROR_R
# if ! HAVE_DECL_STRERROR_R
char* strerror_r(int, char*, size_t);
# else
int strerror_r(int, char*, size_t);
# endif
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
#define _GD_IsDirSep(c) (((c) == GD_DIRSEP) || ((c) == '/'))
#if defined _WIN32 || defined _WIN64
# define _GD_AbsPath(s)  (s && ((s)[0] == '/' || (s)[0] == GD_DIRSEP || \
                                  ((s)[0] != '\0' && (s)[1] == ':')))
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
# define _GD_AbsPath(s)  (s && (s)[0] == '/')
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

#define GD_E_CREAT_FORMAT      1
#define GD_E_CREAT_DIR         2
#define GD_E_CREAT_OPEN        3

#define GD_E_CODE_MISSING      1
#define GD_E_CODE_INVALID      2
#define GD_E_CODE_AMBIGUOUS    3
#define GD_E_CODE_INVALID_NS   4
#define GD_E_CODE_REPR         5

#define GD_E_TYPE_NULL         1

#define GD_E_IO_OPEN           1
#define GD_E_IO_READ           2
#define GD_E_IO_WRITE          3
#define GD_E_IO_CLOSE          4
#define GD_E_IO_UNLINK         5
#define GD_E_IO_RENAME         6
#define GD_E_IO_INCL           7

#define GD_E_IO_ENC_OFFSET     GD_E_IO_INCL
#define GD_E_IO_ENC_OPEN       ( GD_E_IO_OPEN   + GD_E_IO_ENC_OFFSET )
#define GD_E_IO_ENC_READ       ( GD_E_IO_READ   + GD_E_IO_ENC_OFFSET )
#define GD_E_IO_ENC_WRITE      ( GD_E_IO_WRITE  + GD_E_IO_ENC_OFFSET )
#define GD_E_IO_ENC_CLOSE      ( GD_E_IO_CLOSE  + GD_E_IO_ENC_OFFSET )
#define GD_E_IO_ENC_UNLINK     ( GD_E_IO_UNLINK + GD_E_IO_ENC_OFFSET )
#define GD_E_IO_ENC_RENAME     ( GD_E_IO_RENAME + GD_E_IO_ENC_OFFSET )

#define GD_E_RECURSE_CODE       1
#define GD_E_RECURSE_INCLUDE    2

#define GD_E_FIELD_PUT         1
#define GD_E_FIELD_BAD         2
#define GD_E_FIELD_MATCH       3
#define GD_E_FIELD_FORMAT      4
#define GD_E_FIELD_STR         5

#define GD_E_SUPPORT_REGEX     1

#define GD_E_ENTRY_TYPE      1
#define GD_E_ENTRY_METARAW   2
#define GD_E_ENTRY_SPF       3
#define GD_E_ENTRY_NFIELDS   4
#define GD_E_ENTRY_NUMBITS   5
#define GD_E_ENTRY_BITNUM    6
#define GD_E_ENTRY_BITSIZE   7
#define GD_E_ENTRY_POLYORD   8
#define GD_E_ENTRY_WINDOP    9
#define GD_E_ENTRY_PERIOD   10

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

#define GD_E_UNCLEAN_CALL       1

#define GD_E_DOMAIN_COMPLEX     1
#define GD_E_DOMAIN_EMPTY       2
#define GD_E_DOMAIN_ANTITONIC   3
#define GD_E_DOMAIN_MULTIPOS    4

#define GD_E_OUT_OF_RANGE       1
#define GD_E_SINGULAR_RANGE     2

#define GD_E_LUT_LENGTH         1
#define GD_E_LUT_SYNTAX         2

#define GD_E_DIM_FORMAT         1
#define GD_E_DIM_CALLER         2

#define GD_E_UNENC_UNDET        1
#define GD_E_UNENC_TARGET       2

#define GD_E_ARG_WHENCE         1
#define GD_E_ARG_ENDIANNESS     2
#define GD_E_ARG_PROTECTION     3
#define GD_E_ARG_NODATA         4
#define GD_E_ARG_NO_VERS        5
#define GD_E_ARG_BAD_VERS       6
#define GD_E_ARG_REGEX          7
#define GD_E_ARG_PCRE           8

#define GD_E_LONG_FLUSH         1

/* number of lines chunked-in from a LINTERP table at a time */
#define GD_LUT_CHUNK 100

/* I/O flags */
#define GD_FILE_READ  0x1
#define GD_FILE_WRITE 0x2
#define GD_FILE_RDWR  ( GD_FILE_READ | GD_FILE_WRITE )
#define GD_FILE_TEMP  0x4
#define GD_FILE_TOUCH 0x8

/* lists -- all the entry types plus alias, scalar, vector, all */
#define GD_N_ENTRY_LISTS (GD_N_ENTYPES + 4)

#define GD_LIST_VALID_STRING_VALUE 0x01
#define GD_LIST_VALID_SARRAY_VALUE 0x02

/* name types for ValidateField */
#define GD_VF_NAME  0
#define GD_VF_AFFIX 1
#define GD_VF_NS    2
#define GD_VF_CODE  3

/* CodeOffsets flags (also used by StripCode, SlashDot, CheckCodeAffixes) */
#define GD_CO_NSALL  0x001
#define GD_CO_NSROOT 0x002
#define GD_CO_ASSERT 0x004
#define GD_CO_CHECK  0x008
#define GD_CO_EARLY  0x010
#define GD_CO_NAME   0x020
#define GD_CO_ERROR  0x040
#define GD_CO_REPR   0x080
#define GD_CO_REPRZ  0x100

/* Number of offsets returned by CodeOffsets */
#define GD_N_CODEOFFSETS 10

/* Internal rename flags */
#define GD_REN_META 0x10 /* Updating a metafield */

/* database metadata update data for a field rename */
struct gd_rename_update_ {
  char *new_code; /* The newly minted code */
  size_t new_len; /* The new code's length */
  char **dst; /* A pointer to the old code */
  size_t *dst_len; /* Non-NULL if we need to store new_len here */
  int index; /* The affected metadata fragment */
};

struct gd_rename_data_ {
  gd_entype_t type; /* the type of the renamed field */
  unsigned flags; /* GD_REN_ flags */
  struct gd_flist_ *fl; /* The affected field_list struct */

  int src_frag; /* Source fragment */
  int dst_frag; /* Destination fragment for moves (for renames, this is -1) */

  /* Old and new names */
  size_t old_len, new_len;
  const char *old_name, *new_name;

  struct gd_rename_update_ *up; /* The list of updates */
  unsigned n_up, up_size;
};

/* data file book-keeping record */
struct gd_raw_file_ {
  char* name;
  int idata;
  void* edata;
  int subenc;
  int error;
  DIRFILE *D;
  unsigned int mode;
  off64_t pos;
};

/* linterp table datum */
struct gd_lut_ {
  double x;
  union {
    double r;
    GD_DCOMPLEXM(c);
  } y;
};

/* field lists */
struct gd_flist_ {
  const char **entry_list[GD_N_ENTRY_LISTS];
  unsigned int entry_list_flags[GD_N_ENTRY_LISTS];
  const char **string_value_list;
  const char ***sarray_value_list;
  void *const_value_list;
  gd_carray_t *carray_value_list;
  uint32_t value_list_validity;
  uint32_t entry_list_validity;
};

/* Unified entry struct */
struct gd_private_entry_ {
  size_t len; /* strlen(E->field) */

  /* Input entries.  Aliases use entry[0] for the proximal target and
   * entry[1] for the distal target */
  gd_entry_t* entry[GD_MAX_LINCOM];
  int repr[GD_MAX_LINCOM];

  int n_meta;
  union {
    gd_entry_t** meta_entry;
    const gd_entry_t* parent;
  } p;

  /* field lists */
  const char **alias_list;
  struct gd_flist_ fl;

  union {
    struct { /* RAW */
      char* filebase;
      size_t size;
      struct gd_raw_file_ file[2]; /* encoding framework data */
    } raw;
    struct { /* LINTERP */
      char *table_file;
      int table_dirfd;
      int table_len;
      int complex_table;
      int table_monotonic;
      struct gd_lut_ *lut;
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

/* _GD_FiniRawIO flags */
#define GD_FINIRAW_KEEP      0x0
#define GD_FINIRAW_DISCARD   0x1
#define GD_FINIRAW_DEFER     0x2
#define GD_FINIRAW_CLOTEMP   0x4

/* number of subencodings (ie. the length of the _GD_ef array */
#define GD_N_SUBENCODINGS 12

/* the last record of the _GD_ef array is always the unknown encoding */
#define GD_ENC_UNKNOWN (GD_N_SUBENCODINGS - 1)

/* Internal gd_entry_t flags */
#define GD_EN_EARLY 0x8000 /* field spec. contains non-namespace-related '.' */

/* external module function provides flags */
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
#define GD_EF_STRERR  0x0400

/* encoding scheme method prototypes */
typedef int (*gd_ef_name_t)(DIRFILE *D, const char *, struct gd_raw_file_*,
    const char*, int, int);
typedef int (*gd_ef_open_t)(int, struct gd_raw_file_*, gd_type_t, int,
    unsigned int);
typedef off64_t (*gd_ef_seek_t)(struct gd_raw_file_*, off64_t, gd_type_t,
    unsigned int);
typedef off64_t (*gd_ef_size_t)(int, struct gd_raw_file_*, gd_type_t, int);
typedef ssize_t (*gd_ef_read_t)(struct gd_raw_file_*, void*, gd_type_t, size_t);
typedef ssize_t (*gd_ef_write_t)(struct gd_raw_file_*, const void*, gd_type_t,
    size_t);
typedef int (*gd_ef_close_t)(struct gd_raw_file_*);
typedef int (*gd_ef_sync_t)(struct gd_raw_file_*);
typedef int (*gd_ef_unlink_t)(int, struct gd_raw_file_*);
typedef int (*gd_ef_move_t)(int, struct gd_raw_file_*, int, char*);
typedef int (*gd_ef_strerr_t)(const struct gd_raw_file_*, char *, size_t);

/* Encoding scheme flags */
#define GD_EF_ECOR 0x1 /* post-framework byte-sex correction required */
#define GD_EF_SWAP 0x2 /* in-framework byte-sex metadata correction occurs */
#define GD_EF_OOP  0x4 /* writes occur out-of-place */
#define GD_EF_EDAT 0x8 /* The /ENCODING datum is used */

/* Just so we're clear on the difference between GD_EF_ECOR and GD_EF_SWAP:
 *
 * - ECOR means the data returned by the encoding framework has the byte sex of
 *   the fragment; GetData needs to swap bytes around after the framework
 *   finishes if this is different than the machine endianness.  Most binary
 *   formats set ECOR, but TEXT doesn't, since sscanf() puts stuff into the
 *   machine endianness.
 *
 * - SWAP means that internal workings of the encoding needs to know whether
 *   the byte sex of the fragment is different than the machine endianness.
 *   This is set by SIE since its sample indices are stored in the fragment
 *   endianness, which need to be converted by within the encoding scheme itself
 *   to be able to read opposite endian data files.
 *
 * Note: any encoding scheme could set SWAP instead of ECOR and then perform its
 *   own byte sex correction to hide it from GetData proper, but this should be
 *   avoided because it can lead to more byte swapping than necessary.
 */

/* Encoding schemes */
extern struct encoding_t {
  unsigned long int scheme; /* scheme number (the gd_open() flag value) */
  const char* ext;          /* filename extension */
  unsigned int flags;       /* encoding flags */
  const char* affix;        /* function name prefix (NULL for internal scheme)*/
  const char* ffname;       /* /ENCODING directive name */
  unsigned int provides;    /* bitfield of functions provided by external
                               module (0 for internal scheme) */
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
  gd_ef_strerr_t strerr;
} _GD_ef[GD_N_SUBENCODINGS];

/* parser data */
struct parser_state {
  const char *file;
  int line;
  int standards;
  int pedantic;
  char *ns;
  size_t nsl;
  unsigned long flags;
};
#define GD_PVERS_GE(p, v) (!(p).pedantic || (p).standards >= (v))
#define GD_PVERS_LT(p, v) (!(p).pedantic || (p).standards < (v))

/* Format file fragment metadata */
struct gd_fragment_t {
  char* cname; /* Canonical name (full path) */
  char* sname; /* Subdirectory name (path relative to dirfile or absolute) */
  char *bname; /* basename (filename) */
  char* ename; /* External name (the one that appears in the format file) */
  void *enc_data;
  int modified;
  int parent;
  int dirfd;
  time_t mtime;
  unsigned long int encoding;
  unsigned long int byte_sex;
  int protection;
  char* ref_name;
  off64_t frame_offset;
  uint32_t vers;

  char *ns; /* root namespace */
  size_t nsl; /* strlen(ns) */
  char *px; /* prefix */
  size_t pxl; /* strlen(px) */
  char *sx; /* suffix */
  size_t sxl; /* strlen(sx) */
};

/* directory metadata */
struct gd_dir_t {
  char *path;
  int fd;
  int rc;
};

/* internal dirfile flags */
#define GD_MULTISTANDARD   0x20000000 /* have multiple standards in format */
#define GD_HAVE_VERSION    0x40000000 /* have computed the version */
#define GD_INVALID         0x80000000 /* the dirfile is invalid */

/* This has been the meaning of GD_PERMISSIVE post-open for a while; let's
 * alias it to avoid confusing ourselves */
#define GD_NOSTANDARD      GD_PERMISSIVE /* dirfile conforms to no standard */

/* representation suffixes */
#define GD_REPR_NONE 0
#define GD_REPR_REAL 'r'
#define GD_REPR_IMAG 'i'
#define GD_REPR_MOD  'm'
#define GD_REPR_ARG  'a'

/* the implicit representation */
#define GD_REPR_AUTO GD_REPR_REAL

/* The DIRFILE struct.  */
struct gd_dirfile_ {
  /* library error data */
  int error;
  int suberror;
  char* error_string;
  char* error_file;
  int error_line;
  int stdlib_errno;

  /* global data */
  unsigned long int flags;

  char *error_prefix;
  unsigned long int open_flags; /* the original flags (used in gd_desynced) */
  uint64_t av;
  int standards;
  int n_error;
  int lookback;

  /* for the public tokeniser */
  char *tok_base;
  const char *tok_pos;

  /* field counts */
  unsigned int n_entries;

  /* field array */
  gd_entry_t** entry;

  /* the reference field */
  gd_entry_t* reference_field;

  /* directory name (this is just whatever was passed to gd_open() */
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
  const char **regex_list;
  struct gd_flist_ fl;

  /* syntax error callback */
  gd_parser_callback_t sehandler;
  void* sehandler_extra;
};

/* The caller's preferred memory manager */
extern void *(*_GD_CMalloc)(size_t);
extern char *(*_GD_CStrdup)(const char*);
extern void (*_GD_CFree)(void*);

/* These are internal functions we need to expose so that modules can use them
 */
#ifdef USE_MODULES
/* Alias the internal symbols to the external ones */
#define _GD_MakeFullPathOnly gd_MakeFullPathOnly
#define _GD_MakeTempFile gd_MakeTempFile
#define _GD_StrError gd_StrError

#ifdef __cplusplus
extern "C" {
#endif

#else
/* Alias the external symbols to the internal ones */
#define gd_StrError _GD_StrError
#define gd_MakeFullPathOnly _GD_MakeFullPathOnly
#define gd_MakeTempFile _GD_MakeTempFile
#endif

#if !defined HAVE_STRERROR_R || defined STRERROR_R_CHAR_P
int _GD_StrError(int errnum, char *buf, size_t buflen);
#else
# ifdef USE_MODULES
#  define gd_StrError strerror_r
# else
#  define _GD_StrError strerror_r
# endif
#endif

char *_GD_MakeFullPathOnly(const DIRFILE *D, int dirfd, const char *name);
int _GD_MakeTempFile(const DIRFILE*, int, char*);

#ifdef USE_MODULES
#ifdef __cplusplus
}
#endif
#endif


/* forward declarations */
void *_GD_Alloc(DIRFILE*, gd_type_t, size_t) __attribute_malloc__;

#define _GD_BadWindop(op) \
  ( \
   (op != GD_WINDOP_EQ) && (op != GD_WINDOP_GE) && (op != GD_WINDOP_GT) && \
   (op != GD_WINDOP_LE) && (op != GD_WINDOP_LT) && (op != GD_WINDOP_NE) && \
   (op != GD_WINDOP_SET) && (op != GD_WINDOP_CLR) \
  )

int _GD_BadType(int, gd_type_t);
char *_GD_BuildCode(DIRFILE*, int, const char*, size_t, const char*, int,
    size_t *restrict) __attribute_malloc__;
int _GD_CalculateEntry(DIRFILE *restrict, gd_entry_t *restrict, int);
char *_GD_CanonicalPath(const char *restrict, const char *restrict);
int _GD_CheckByteSex(gd_type_t, unsigned, unsigned, int, int *restrict);
gd_entry_t *_GD_CheckParent(DIRFILE *restrict,
    const struct parser_state *restrict, char **restrict, size_t *restrict,
    int);
int _GD_CheckCodeAffixes(DIRFILE *restrict, const char *restrict, int,
    unsigned);
void _GD_CInvertData(DIRFILE *restrict, void *restrict, gd_type_t,
    GD_DCOMPLEXA(dividend), size_t);
void _GD_CleanUpRename(struct gd_rename_data_*, int);

/* _GD_ClearError: Everything's A-OK; clear the last error. */
#define _GD_ClearError(D) (D)->error = 0

void _GD_CLincomData(DIRFILE *restrict, int, void *restrict, gd_type_t,
    const GD_DCOMPLEXP_t restrict, const GD_DCOMPLEXP_t restrict,
    GD_DCOMPLEXV(m), GD_DCOMPLEXV(b), const unsigned int *restrict, size_t);
int _GD_CodeOffsets(DIRFILE *restrict, int, const char *restrict, unsigned,
    size_t offset[GD_N_CODEOFFSETS]);
int _GD_ContainsFragment(const int *, int, int);
void _GD_ConvertType(DIRFILE *restrict, const void *restrict, gd_type_t,
    void *restrict, gd_type_t, size_t) gd_nothrow;
gd_type_t _GD_ConstType(DIRFILE *D, gd_type_t type);
const char *_GD_DirName(const DIRFILE *D, int dirfd);
size_t _GD_DoField(DIRFILE *restrict, gd_entry_t *restrict, int, off64_t,
    size_t, gd_type_t, void *restrict);
size_t _GD_DoFieldOut(DIRFILE *restrict, gd_entry_t *restrict, off64_t, size_t,
    gd_type_t, const void *restrict);
off64_t _GD_DoSeek(DIRFILE *restrict, gd_entry_t *restrict,
    const struct encoding_t *restrict, off64_t, unsigned int mode);
int _GD_EntryCmp(const void*, const void*);
gd_entry_t *_GD_FindEntry(DIRFILE *restrict, const char *restrict);
gd_entry_t *_GD_FindField(const DIRFILE *restrict, const char *restrict,
    size_t, gd_entry_t *const *, unsigned int, int, unsigned int *restrict);
gd_entry_t *_GD_FindFieldAndRepr(DIRFILE *restrict, const char *restrict,
    int *restrict, unsigned int *restrict, int);
int _GD_FindInputs(DIRFILE *restrict, gd_entry_t *restrict, int) gd_nothrow;
uint64_t _GD_FindVersion(DIRFILE *D);
void _GD_FixEndianness(void*, size_t, gd_type_t, unsigned, unsigned);
int _GD_FileSwapBytes(const DIRFILE *restrict, const gd_entry_t *restrict);
int _GD_FiniRawIO(DIRFILE*, const gd_entry_t*, int, int);
void _GD_Flush(DIRFILE *restrict, gd_entry_t *restrict, int, int);
void _GD_FlushMeta(DIRFILE* D, int fragment, int force);
void _GD_FreeE(DIRFILE *restrict, gd_entry_t *restrict, int);
void _GD_FreeF(DIRFILE *restrict, int, int);
void _GD_FreeFL(struct gd_flist_ *);
off64_t _GD_GetEOF(DIRFILE *restrict, gd_entry_t *restrict,
    const char *restrict, int *restrict);
off64_t _GD_GetIOPos(DIRFILE *restrict, gd_entry_t *restrict, off64_t);
char *_GD_GetLine(FILE *restrict, size_t *restrict, int *restrict);
int _GD_GetRepr(const char *restrict, size_t *restrict);
int _GD_GetScalar(DIRFILE *restrict, const char *restrict, int*, gd_type_t,
   void *restrict, gd_entry_t*);
unsigned int _GD_GetSPF(DIRFILE*, gd_entry_t*);
int _GD_GrabDir(DIRFILE*, int, const char *restrict, int);
int _GD_Include(DIRFILE*, struct parser_state *restrict, const char *restrict,
    char **restrict, int, const char *restrict, const char *restrict, int);
void _GD_InitialiseFramework(void);
int _GD_InitRawIO(DIRFILE*, const gd_entry_t*, const char*, int,
    const struct encoding_t*, unsigned int, unsigned int, int);
void _GD_InvertData(DIRFILE *restrict, void *restrict, gd_type_t return_type,
    double dividend, size_t n_read);
void _GD_InsertSort(DIRFILE *restrict, gd_entry_t *restrict, int u) gd_nothrow;

#define _GD_InternalError(D) \
  _GD_SetError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL)

int _GD_InvalidEntype(gd_entype_t t);
gd_type_t _GD_LegacyType(char c);
void _GD_LincomData(DIRFILE *restrict, int n, void *restrict,
    gd_type_t return_type, const double *restrict, const double *restrict,
    const double *restrict, const double *restrict,
    const unsigned int *restrict, size_t);
void _GD_LinterpData(DIRFILE *restrict, void *restrict, gd_type_t, int,
    const double *restrict, size_t, const struct gd_lut_ *restrict, size_t);
int _GD_ListEntry(const gd_entry_t*, int, int, int, int, int, gd_entype_t);
char *_GD_MakeFullPath(DIRFILE *restrict, int, const char *restrict, int);
void *_GD_Malloc(DIRFILE *D, size_t size) __attribute_malloc__;
int _GD_MissingFramework(int encoding, unsigned int funcs);
int _GD_MogrifyFile(DIRFILE *restrict, gd_entry_t *restrict, unsigned long int,
    unsigned long int, off64_t, int, int, char *restrict);
gd_type_t _GD_NativeType(DIRFILE *restrict, gd_entry_t *restrict, int);
char *_GD_NormaliseNamespace(DIRFILE *restrict, const char *restrict,
    size_t *restrict) __attribute_malloc__;
gd_entry_t *_GD_ParseFieldSpec(DIRFILE *restrict,
    const struct parser_state *restrict, int, char**, size_t,
    const gd_entry_t *restrict, int, int, int, char **, const char *);
char *_GD_ParseFragment(FILE *restrict, DIRFILE*, struct parser_state *restrict,
    int, int);
void _GD_PerformRename(DIRFILE *restrict, struct gd_rename_data_ *restrict);
struct gd_rename_data_ *_GD_PrepareRename(DIRFILE *restrict, char *restrict,
    size_t, gd_entry_t *restrict, int, unsigned);
int _GD_ReadLinterpFile(DIRFILE *restrict, gd_entry_t *restrict);
void *_GD_Realloc(DIRFILE *restrict, void *restrict, size_t size);
void _GD_ReleaseDir(DIRFILE *D, int dirfd);
int _GD_SlashDot(const char*, size_t, unsigned, const char**, const char**);
int _GD_Seek(DIRFILE *restrict, gd_entry_t *restrict, off64_t offset,
    unsigned int mode);
void _GD_SetEncIOError(DIRFILE*, int suberror, const struct gd_raw_file_ *);
void _GD_SetError(DIRFILE*, int, int, const char*, int, const char*);
void _GD_SetError2(DIRFILE*, int, int, const char*, int, const char*, int);
int _GD_SetTablePath(DIRFILE *restrict, const gd_entry_t *restrict,
    struct gd_private_entry_ *restrict);
void _GD_SimpleParserInit(DIRFILE *restrict D, const char *restrict name,
    struct parser_state *restrict p);
int _GD_ShutdownDirfile(DIRFILE*, int, int);
int _GD_StrCmpNull(const char *restrict, const char *restrict);
char *_GD_Strdup(DIRFILE *restrict, const char *restrict);
char *_GD_StripCode(DIRFILE *restrict, int, const char *restrict, unsigned)
  __attribute_malloc__;
int _GD_SubFragmentList(DIRFILE *restrict, int, int **restrict);
int _GD_Supports(DIRFILE *, const gd_entry_t*, unsigned int funcs);
int _GD_Tokenise(DIRFILE *restrict, const struct parser_state *restrict,
    const char *restrict, char **, const char **, int, char **);
int _GD_TokToNum(const char *restrict, int, int, double*, double*, uint64_t*,
    int64_t*);
void _GD_UpdateAliases(DIRFILE*, int);
char *_GD_UpdateCode(DIRFILE *restrict, int, const char *restrict, int,
    const char *restrict, size_t, const char *restrict, size_t,
    const char *restrict, size_t) __attribute_malloc__;
int _GD_ValidateField(const char*, size_t, int, int, unsigned);
ssize_t _GD_WriteOut(const gd_entry_t*, const struct encoding_t*, const void*,
    gd_type_t, size_t, int);

/* generic I/O methods */
int _GD_GenericMove(int, struct gd_raw_file_ *restrict, int, char *restrict);
int _GD_GenericName(DIRFILE *restrict, const char *restrict,
    struct gd_raw_file_ *restrict, const char *restrict, int, int);
int _GD_GenericUnlink(int, struct gd_raw_file_* file);
int _GD_NopSync(struct gd_raw_file_*);

/* unencoded I/O methods */
int _GD_RawOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_RawSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_RawRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_RawWrite(struct gd_raw_file_ *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_RawSync(struct gd_raw_file_* file);
int _GD_RawClose(struct gd_raw_file_* file);
off64_t _GD_RawSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);

/* text I/O methods */
int _GD_AsciiOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_AsciiSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_AsciiRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_AsciiWrite(struct gd_raw_file_ *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_AsciiSync(struct gd_raw_file_* file);
int _GD_AsciiClose(struct gd_raw_file_* file);
off64_t _GD_AsciiSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);

/* SIE I/O methods */
int _GD_SampIndOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_SampIndSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_SampIndRead(struct gd_raw_file_ *restrict, void *restrict,
    gd_type_t, size_t);
ssize_t _GD_SampIndWrite(struct gd_raw_file_ *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_SampIndSync(struct gd_raw_file_* file);
int _GD_SampIndClose(struct gd_raw_file_* file);
off64_t _GD_SampIndSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);

#ifdef USE_MODULES
#define _GD_Bzip2Open lt_libgetdatabzip2_LTX_GD_Bzip2Open
#define _GD_Bzip2Seek lt_libgetdatabzip2_LTX_GD_Bzip2Seek
#define _GD_Bzip2Read lt_libgetdatabzip2_LTX_GD_Bzip2Read
#define _GD_Bzip2Write lt_libgetdatabzip2_LTX_GD_Bzip2Write
#define _GD_Bzip2Close lt_libgetdatabzip2_LTX_GD_Bzip2Close
#define _GD_Bzip2Size lt_libgetdatabzip2_LTX_GD_Bzip2Size
#define _GD_Bzip2Strerr lt_libgetdatabzip2_LTX_GD_Bzip2Strerr

#define _GD_FlacOpen lt_libgetdataflac_LTX_GD_FlacOpen
#define _GD_FlacSeek lt_libgetdataflac_LTX_GD_FlacSeek
#define _GD_FlacRead lt_libgetdataflac_LTX_GD_FlacRead
#define _GD_FlacWrite lt_libgetdataflac_LTX_GD_FlacWrite
#define _GD_FlacClose lt_libgetdataflac_LTX_GD_FlacClose
#define _GD_FlacSize lt_libgetdataflac_LTX_GD_FlacSize
#define _GD_FlacStrerr lt_libgetdataflac_LTX_GD_FlacStrerr

#define _GD_GzipOpen lt_libgetdatagzip_LTX_GD_GzipOpen
#define _GD_GzipSeek lt_libgetdatagzip_LTX_GD_GzipSeek
#define _GD_GzipRead lt_libgetdatagzip_LTX_GD_GzipRead
#define _GD_GzipWrite lt_libgetdatagzip_LTX_GD_GzipWrite
#define _GD_GzipClose lt_libgetdatagzip_LTX_GD_GzipClose
#define _GD_GzipSize lt_libgetdatagzip_LTX_GD_GzipSize
#define _GD_GzipStrerr lt_libgetdatagzip_LTX_GD_GzipStrerr

#define _GD_LzmaOpen lt_libgetdatalzma_LTX_GD_LzmaOpen
#define _GD_LzmaSeek lt_libgetdatalzma_LTX_GD_LzmaSeek
#define _GD_LzmaRead lt_libgetdatalzma_LTX_GD_LzmaRead
#define _GD_LzmaWrite lt_libgetdatalzma_LTX_GD_LzmaWrite
#define _GD_LzmaSync lt_libgetdatalzma_LTX_GD_LzmaSync
#define _GD_LzmaClose lt_libgetdatalzma_LTX_GD_LzmaClose
#define _GD_LzmaSize lt_libgetdatalzma_LTX_GD_LzmaSize
#define _GD_LzmaStrerr lt_libgetdatalzma_LTX_GD_LzmaStrerr

#define _GD_SlimOpen lt_libgetdataslim_LTX_GD_SlimOpen
#define _GD_SlimSeek lt_libgetdataslim_LTX_GD_SlimSeek
#define _GD_SlimRead lt_libgetdataslim_LTX_GD_SlimRead
#define _GD_SlimClose lt_libgetdataslim_LTX_GD_SlimClose
#define _GD_SlimSize lt_libgetdataslim_LTX_GD_SlimSize
#define _GD_SlimStrerr lt_libgetdataslim_LTX_GD_SlimStrerr

#define _GD_ZzipName lt_libgetdatazzip_LTX_GD_ZzipName
#define _GD_ZzipOpen lt_libgetdatazzip_LTX_GD_ZzipOpen
#define _GD_ZzipSeek lt_libgetdatazzip_LTX_GD_ZzipSeek
#define _GD_ZzipRead lt_libgetdatazzip_LTX_GD_ZzipRead
#define _GD_ZzipClose lt_libgetdatazzip_LTX_GD_ZzipClose
#define _GD_ZzipSize lt_libgetdatazzip_LTX_GD_ZzipSize
#define _GD_ZzipStrerr lt_libgetdatazzip_LTX_GD_ZzipStrerr

#define _GD_ZzslimName lt_libgetdatazzslim_LTX_GD_ZzslimName
#define _GD_ZzslimOpen lt_libgetdatazzslim_LTX_GD_ZzslimOpen
#define _GD_ZzslimSeek lt_libgetdatazzslim_LTX_GD_ZzslimSeek
#define _GD_ZzslimRead lt_libgetdatazzslim_LTX_GD_ZzslimRead
#define _GD_ZzslimClose lt_libgetdatazzslim_LTX_GD_ZzslimClose
#define _GD_ZzslimSize lt_libgetdatazzslim_LTX_GD_ZzslimSize
#define _GD_ZzslimStrerr lt_libgetdatazzslim_LTX_GD_ZzslimStrerr

#ifdef __cplusplus
extern "C" {
#endif
#endif

/* bzip I/O methods */
int _GD_Bzip2Open(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_Bzip2Seek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_Bzip2Read(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_Bzip2Write(struct gd_raw_file_ *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_Bzip2Close(struct gd_raw_file_*);
off64_t _GD_Bzip2Size(int, struct gd_raw_file_*, gd_type_t, int);
int _GD_Bzip2Strerr(const struct gd_raw_file_*, char*, size_t);

/* flac I/O methods */
int _GD_FlacOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_FlacSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_FlacRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_FlacWrite(struct gd_raw_file_ *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_FlacClose(struct gd_raw_file_* file);
off64_t _GD_FlacSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);
int _GD_FlacStrerr(const struct gd_raw_file_*, char*, size_t);

/* gzip I/O methods */
int _GD_GzipOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_GzipSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_GzipRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_GzipWrite(struct gd_raw_file_ *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_GzipClose(struct gd_raw_file_* file);
off64_t _GD_GzipSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);
int _GD_GzipStrerr(const struct gd_raw_file_*, char*, size_t);

/* lzma I/O methods */
int _GD_LzmaOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_LzmaSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_LzmaRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
ssize_t _GD_LzmaWrite(struct gd_raw_file_ *restrict, const void *restrict,
    gd_type_t, size_t);
int _GD_LzmaClose(struct gd_raw_file_* file);
int _GD_LzmaSync(struct gd_raw_file_ *file);
off64_t _GD_LzmaSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);
int _GD_LzmaStrerr(const struct gd_raw_file_*, char*, size_t);

/* slim I/O methods */
int _GD_SlimOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_SlimSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_SlimRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_SlimClose(struct gd_raw_file_* file);
off64_t _GD_SlimSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);
int _GD_SlimStrerr(const struct gd_raw_file_*, char*, size_t);

/* zzip I/O methods */
int _GD_ZzipName(DIRFILE *restrict, const char *restrict,
    struct gd_raw_file_ *restrict, const char *restrict, int, int);
int _GD_ZzipOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_ZzipSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_ZzipRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_ZzipClose(struct gd_raw_file_* file);
off64_t _GD_ZzipSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);
int _GD_ZzipStrerr(const struct gd_raw_file_*, char*, size_t);

/* zzslim I/O methods */
int _GD_ZzslimName(DIRFILE *restrict, const char *restrict,
    struct gd_raw_file_ *restrict, const char *restrict, int, int);
int _GD_ZzslimOpen(int, struct gd_raw_file_*, gd_type_t, int, unsigned int);
off64_t _GD_ZzslimSeek(struct gd_raw_file_* file, off64_t count,
    gd_type_t data_type, unsigned int);
ssize_t _GD_ZzslimRead(struct gd_raw_file_ *restrict, void *restrict, gd_type_t,
    size_t);
int _GD_ZzslimClose(struct gd_raw_file_* file);
off64_t _GD_ZzslimSize(int, struct gd_raw_file_* file, gd_type_t data_type,
    int swap);
int _GD_ZzslimStrerr(const struct gd_raw_file_*, char*, size_t);

#ifdef USE_MODULES
#ifdef __cplusplus
}
#endif
#endif

#ifndef __cplusplus
# undef gd_nothrow
# define gd_nothrow
#endif

/* deal with GD_ANON */
#ifdef GD_C89_API
# define EN(t,v) u.t.v
#else
# define EN(t,v) v
#endif

#endif
