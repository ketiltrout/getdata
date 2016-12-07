/* Copyright (C) 2010-2016 D. V. Wiebe
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

/* The MSVCRT gmtime() is threadsafe */
#ifndef HAVE_GMTIME_R
struct tm *gmtime_r(const time_t *restrict timep, struct tm *restrict result)
{
  struct tm *ptr;

  dtrace("%p, %p", timep, result);

  ptr = gmtime(timep);
  if (!ptr)
    return NULL;

  memcpy(result, ptr, sizeof(struct tm));

  dreturn("%p", result);
  return result;
}
#endif

/* ...at() functions for platforms lacking them.  These are originally Solaris
 * extensions which have subsequently been included in POSIX.1-2008.
 */
#ifndef HAVE_OPENAT
int gd_OpenAt(const DIRFILE *D, int dirfd, const char *name, int flags,
    mode_t mode)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", 0x%X, 0%o", D, dirfd, name, flags, mode);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
  ret = open(path, flags | O_BINARY, mode);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

#ifndef HAVE_RENAMEAT
int gd_RenameAt(const DIRFILE *D, int olddirfd, const char *oldname,
    int newdirfd, const char *newname)
{
  int ret;
  char *oldpath, *newpath;

  dtrace("%p, %i, \"%s\", %i, \"%s\"", D, olddirfd, oldname, newdirfd, newname);

  newpath = _GD_MakeFullPathOnly(D, newdirfd, newname);
#ifdef __MSVCRT__
  if (unlink(newpath)) {
    if (errno != ENOENT) {
      free(newpath);
      dreturn("%i", -1);
      return -1;
    }
  }
#endif

  oldpath = _GD_MakeFullPathOnly(D, olddirfd, oldname);
  ret = rename(oldpath, newpath);
  free(newpath);
  free(oldpath);

  dreturn("%i", ret);
  return ret;
}
#endif

#ifndef HAVE_FSTATAT
#ifndef AT_SYMLINK_NOFOLLOW
#define AT_SYMLINK_NOFOLLOW 0 /* will never match */
#endif
int gd_StatAt(const DIRFILE* D, int dirfd, const char* name, struct stat* buf,
    int flags)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", %p, 0x%X", D, dirfd, name, buf, flags);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
#ifdef HAVE_LSTAT
  if (flags & AT_SYMLINK_NOFOLLOW)
    ret = lstat(path, buf);
  else
#endif
    ret = stat(path, buf);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

#if !defined HAVE_FSTATAT64 && !defined GD_NO_64BIT_STAT
int gd_StatAt64(const DIRFILE* D, int dirfd, const char* name, gd_stat64_t* buf,
    int flags gd_unused_)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", %p, <unused>", D, dirfd, name, buf);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
  ret = gd_stat64(path, buf);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

#ifndef HAVE_UNLINKAT
int gd_UnlinkAt(const DIRFILE *D, int dirfd, const char *name,
    int flags gd_unused_)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", <unused>", D, dirfd, name);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
  ret = unlink(path);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

/* Non-threadsafe version of strerror_r */
#ifndef HAVE_STRERROR_R
int strerror_r(int errnum, char *buf, size_t buflen)
{
  char *ptr;

  dtrace("%i, %p, %" PRIuSIZE, errnum, buf, buflen);

  ptr = strerror(errnum);
  strncpy(buf, ptr, buflen);

  dreturn("%i", 0);
  return 0;
}
#endif

/* A getdelim() for platforms lacking it.  getdelim was originally a GNU
 * extension and has subsequently been POSIXised in POSIX.1-2008.
 */
#ifndef HAVE_GETDELIM
ssize_t getdelim(char **lineptr, size_t *n, int delim, FILE *stream)
{
  size_t nread;
  ssize_t count = 0;
  char *p, *q;
  size_t len, new_len;
  off64_t pos;

  dtrace("%p, %p, '\\x%02x', %p", lineptr, n, (char)delim, stream);

  /* create a new buffer, if necessary */
  if (*lineptr == NULL || *n == 0) {
    *lineptr = malloc(*n = 100);
    if (*lineptr == NULL) {
      dreturn("%i", -1);
      return -1;
    }
  }
  p = *lineptr;
  len = *n;

  /* apparently getdelim returns -1 if encountering EOF at the start of
   * a read, so try reading some text before beginning the main loop */
  pos = ftello64(stream);
  nread = fread(p, 1, len, stream);

  if (nread == 0) {
    /* this is an error or EOF with no data read */
    dreturn("%i", -1);
    return -1;
  }

  for (;;) {
    /* look for delim */
    q = (char *)memchr(p, delim, nread);
    if (q) {
#ifdef __MSVCRT__
      int r;
      off64_t new_pos;
#endif

      /* found delim */
      count += (q - p) + 1;

      /* make sure we have room for a terminating NUL */
      new_len = count;
      /* rewind */
      pos += (q - p);
#ifndef __MSVCRT__
      pos++;
#endif
      fseeko64(stream, pos, SEEK_SET);
#ifdef __MSVCRT__
      /* Even when we open a text file in binary mode, fseek/ftell seem able
       * to screw up.  So, do things the hard way. */
      r = fgetc(stream);
      new_pos = ftello64(stream);
      while (r != EOF && (new_pos <= pos || r != '\n')) {
        r = fgetc(stream);
        new_pos = ftello64(stream);
      }
#endif
    } else {
      /* no delim, increase the buffer size */
      count += nread;
      p += nread;

      if (count == GD_SSIZE_T_MAX) {
        /* out of ssize_t room */
        errno = EOVERFLOW;
        dreturn("%i", -1);
        return -1;
      } else if (count >= GD_SSIZE_T_MAX / 2)
        new_len = GD_SSIZE_T_MAX;
      else
        new_len = count * 2;
      len = new_len - count;
    }

    /* realloc, if necessary */
    if (*n < new_len) {
      char *ptr = realloc(*lineptr, new_len);
      if (!ptr) {
        dreturn("%i", -1);
        return -1;
      }
      *n = new_len;
      p = ptr + (p - *lineptr);
      *lineptr = ptr;
    }

    /* quit if there's no need to read more */
    if (q)
      break;

    if (feof(stream)) {
      q = p - 1;
      break;
    }

    /* read some more */
    pos = ftello64(stream);
    nread = fread(p, 1, len, stream);
  }
  *(q + 1) = '\0';

  dreturn("%li %li (\"%s\")", (long int)count, (long int)*n, *lineptr);
  return count;
}
#endif

#ifndef HAVE_BASENAME
char *basename(char *path)
{
  char *last_elem, *ptr;

  last_elem = path;

  for (ptr = path; *ptr; ++ptr)
    if ((*ptr == '/' || *ptr == GD_DIRSEP) && *(ptr + 1))
      last_elem = ptr + 1;

  return last_elem;
}
#endif

/* emulate readdir_r(3) with readdir(3).  This implementation ignores
 * 'entry' completely */
#ifndef HAVE_READDIR_R
int _GD_ReadDir(DIR *dirp, struct dirent *entry gd_unused_,
    struct dirent **result)
{
  dtrace("%p, <unused>, %p", dirp, result);

  errno = 0;
  *result = readdir(dirp);
  if (*result == NULL && errno) {
    dreturn("%i", errno);
    return errno;
  }

  dreturn("%i (%p)", 0, *result);
  return 0;
}
#endif

/* the MSVCRT's strtod isn't POSIX compliant */
#ifdef __MSVCRT__
double gd_strtod(const char *nptr, char **endptr)
{
  const char *ptr = nptr;
  double r = 0;
  int sign = 0;

  dtrace("\"%s\", %p", nptr, endptr);

  /* the basic problem here is that MSVCRT's strtod() doesn't properly covert
   * hexadecimal numbers, nor does it do the special values "INF" and "NAN".
   */

  /* skip sign */
  if (*ptr == '+' || *ptr == '-') {
    if (*ptr == '-')
      sign = 0x80;
    ptr++;
  }

  /* check for hex, either integer or floating point */
  if (*ptr == '0' && (*(ptr + 1) | 0x20) == 'x') {
    double fact = 1.;
    /* we store the mantissa in r */
    for (ptr += 2; *ptr; ++ptr) {
      if ((*ptr >= '0' && *ptr <= '9') ||
          ((*ptr | 0x20) >= 'a' && (*ptr | 0x20) <= 'f'))
      {
        if (fact == 1.)
          r *= 16;
        else if (fact == 0.)
          fact = 1./16;
        else
          fact /= 16;

        if (*ptr >= '0' && *ptr <= '9')
          r += fact * (*ptr - '0');
        else
          r += fact * ((*ptr | 0x20) - 'a' + 10);
      } else if (*ptr == '.' && fact == 1.)
        fact = 0.;
      else if ((*ptr | 0x20) == 'p') {
        /* use strtol to get exponent, which is in decimal */
        long exp = strtol(ptr + 1, endptr, 10);
        r *= pow(2., exp);

        /* to avoid setting it again */
        endptr = NULL;
        break;
      } else
        break;
    }

    if (endptr)
      *endptr = (char*)ptr;

    if (sign)
      r = -r;

    dreturn("%g (%c)", r, endptr ? **endptr : '-');
    return r;
  } else if ((*ptr | 0x20) == 'n') {
    if (((*(ptr + 1) | 0x20) == 'a') && ((*(ptr + 2) | 0x20) == 'n')) {
      /* an IEEE-754 double-precision quiet NaN:
       *
       * 1111 1111 1111 1111 1111 1111 1111 1111
       * 1111 1111 1111 1111 1111 1111 1111 1111
       * = 0xFFFFFFFFFFFFFFFF
       *
       * (a signalling NaN doesn't set the MSB of the mantissa)
       *
       * There are several of these; this one is nice because it's byte-sex
       * independent, unlike INF below.
       */
      const union {
        unsigned char x[8];
        double d;
      } nan_punning = {{ 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF }};

      if (endptr) {
        *endptr = (char*)(ptr += 3);
        /* a NaN may end with something in parentheses */
        if (*ptr == '(') {
          while (*++ptr) {
            if (*ptr == ')') {
              *endptr = (char*)(ptr + 1);
              break;
            }
          }
        }
      }

      /* return NaN */
      dreturn("%g (%c)", nan_punning.d, endptr ? **endptr : '-');
      return nan_punning.d;
    }
  } else if ((*ptr | 0x20) == 'i') {
    if (((*(ptr + 1) | 0x20) == 'n') && ((*(ptr + 2) | 0x20) == 'f')) {
      /* an IEEE-754 double-precision infinity (s is the sign bit):
       *
       * s111 1111 1111 1000 0000 0000 0000 0000
       * 0000 0000 0000 0000 0000 0000 0000 0000
       * = 0x7FF0000000000000 (+INF), or
       *   0xFFF0000000000000 (-INF)
       */
      const union {
        unsigned char x[8];
        double d;
      } inf_punning = {{
#ifdef ARM_ENDIAN_DOUBLES
        0x00, 0x00, 0xF0, 0x7F | sign, 0x00, 0x00, 0x00, 0x00
#elif defined(FLOATS_BIGENDIAN)
          0x7F | sign, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
#else
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x7F | sign
#endif
      }};

      if (endptr) {
        *endptr = (char*)(ptr += 3);
        /* INF may also be INFINITY, disregarding case */
        if (
            ((*ptr | 0x20) == 'i') &&
            ((*(ptr + 1) | 0x20) == 'n') &&
            ((*(ptr + 2) | 0x20) == 'i') &&
            ((*(ptr + 3) | 0x20) == 't') &&
            ((*(ptr + 4) | 0x20) == 'y'))
        {
          *endptr += 5;
        }
      }

      /* return signed infinity */
      dreturn("%g (%c)", inf_punning.d, endptr ? **endptr : '-');
      return inf_punning.d;
    }
  }

  /* otherwise, just run strtod */
  r = strtod(nptr, endptr);
  dreturn("%g (%c)", r, endptr ? **endptr : '-');
  return r;
}
#endif

/* There are two versions of this function: one deals with no strerror_r,
 * the other with GNU's non XSI-conforming strerror_r */
#if !defined HAVE_STRERROR_R || defined STRERROR_R_CHAR_P
int _GD_StrError(int errnum, char *buf, size_t buflen)
{
  char *ptr;

  dtrace("%i, %p, %" PRIuSIZE, errnum, buf, buflen);
  
#ifdef STRERROR_R_CHAR_P
  ptr = strerror_r(errnum, buf, buflen);
  if (ptr != buf)
#else
    ptr = strerror(errnum);
#endif
  {
    strncpy(buf, ptr, buflen);
    buf[buflen - 1] = 0;
  }

  dreturn("%i", 0);
  return 0;
}
#endif
