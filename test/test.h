/* Copyright (C) 2010-2017 D. V. Wiebe
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
#define GD_TEST
#include "internal.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#if defined HAVE__ISNAN && ! defined HAVE_ISNAN
#define isnan _isnan
#endif

#ifndef INFINITY
#define INFINITY gd_strtod("INF", NULL)
#endif

#if defined HAVE__FINITE && \
  (! defined HAVE_DECL_ISFINITE || HAVE_DECL_ISFINITE == 0)
#define isfinite _finite
#endif

/* System call kludge for Win32 */
#if defined __MSVCRT__ && defined MSYS_SHELL
#include <process.h>
int gd_system(const char* command)
{
  int ret = -1;

  char* ptr = malloc(strlen(command) + 3);
  sprintf(ptr, "\"%s\"", command);
  
  ret = _spawnlp(_P_WAIT, MSYS_SHELL, MSYS_SHELL, "-c", ptr, NULL); 
  free(ptr);

  return ret;
}
#else
#define gd_system system
#endif

/* rm for WIN32 */
#ifdef _WIN32
#define rmdirfile() system("rmdir /q/s dirfile");
#else
#define rmdirfile() do { \
  chmod("dirfile", 0700); \
  if (system("rm -rf dirfile")) { perror("system"); exit(1); } \
} while (0)
#endif

/* sleep for WIN32/64 */
#if defined _WIN32 || defined _WIN64
#include <windows.h>
#define sleep(x) Sleep(1000 * (x))
#endif

#ifdef NO_LARGE_TESTS
#define BIG_JUMP 10000
#else
#define BIG_JUMP 1000000
#endif

/* path munging for format files */
#define gd_pathwrite(x,y) do { \
  char *ptr; \
  const char esc = '\\'; \
  for (ptr = y; *ptr != '\0'; ++ptr) { \
    if (*ptr == '\\' || *ptr == '#' || *ptr == ' ') write(x,&esc,1); \
    write(x,ptr,1); \
  } \
} while (0)

/* getcwd abstraction */
#if defined HAVE_GETCWD || defined HAVE__GETCWD
# ifdef HAVE__GETCWD
#  define getcwd _getcwd
# endif
# define gdtest_getcwd(ptr,cwd,cwd_size) \
  do { \
    ptr = (char*)realloc(cwd, cwd_size *= 2); \
    if (ptr == NULL) { \
      fprintf(stderr, "out of memory for cwd!\n"); \
      exit(1); \
    } \
  } while (!getcwd(cwd = ptr, cwd_size))
#else
# define GD_NO_GETCWD
#endif


#define CHECK(e,n,nf,vf,...) \
  do { if (e) { r = 1; \
    fprintf(stderr, #n " = " nf " (expected " vf ")\n", __VA_ARGS__); } \
  } while(0)

#define CHECKi(i,e,n,nf,vf,...) \
  do { if (e) { r = 1; \
    fprintf(stderr, #i ":%i " #n " = " nf " (expected " vf ")\n", (int)(i), \
        __VA_ARGS__); } \
  } while(0)

/* Create an empty file with mode m */
#define MAKEEMPTYFILE(f,m) \
  do { \
    int fd = open(f, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, m); \
    if (fd < 0) { perror("open"); exit(1); } \
    if (close(fd)) { perror("close"); exit(1); } \
  } while (0)

/* Write string literal t to format file f. */
#define MAKEFORMATFILE(f,t) \
  do { \
    int fd = open(f, O_CREAT | O_EXCL | O_WRONLY, 0666); \
    if (fd < 0) { perror("open"); exit(1); } \
    if (write(fd, t, -1 + sizeof t) < 0) { perror("write"); exit(1); } \
    if (close(fd)) { perror("close"); exit(1); } \
  } while (0)

/* Write n data of type t initialialised with expression expr to file f. */
#define MAKEDATAFILE(f,t,expr,n) \
  do { \
    int i; \
    t *data_data = malloc(sizeof(*data_data) * n); \
    if (data_data == NULL) { perror("malloc"); exit(1); } \
    for (i = 0; i < n; ++i) data_data[i] = (t)(expr); \
    i = open(f, O_BINARY | O_CREAT | O_EXCL | O_WRONLY, 0666); \
    if (i < 0) { perror("open"); exit(1); } \
    if (write(i, data_data, sizeof(t) * n) < 0) { perror("write"); exit(1); } \
    if (close(i)) { perror("close"); exit(1); } \
    free(data_data); \
  } while(0)

#ifdef GD_NO_C99_API
#define CHECKC(n,v)    CHECK(sqrt(((n)[0]-(v)[0])*((n)[0]-(v)[0]) + \
      (((n)[1]-(v)[1])*((n)[1]-(v)[1])))>1e-10,n,"%.15g;%.15g","%.15g;%.15g",\
    creal((n)), cimag((n)), creal((v)), cimag((v)))
#define CHECKCi(i,n,v) CHECKi(i,sqrt(((n)[0]-(v)[0])*((n)[0]-(v)[0]) + \
            (((n)[1]-(v)[1])*((n)[1]-(v)[1]))) / cabs((v))>3e-6,n,\
    "%.15g;%.15g","%.15g;%.15g",creal((n)), cimag((n)), creal((v)), cimag((v)))
#else
#define CHECKC(n,v)    CHECK(cabs((n)-(v))>1e-10,n,"%.15g;%.15g","%.15g;%.15g",\
    creal((n)), cimag((n)), creal((v)), cimag((v)))
#define CHECKCi(i,n,v) CHECKi(i,cabs((n)-(v)) / cabs((v))>3e-6,n,"%.15g;%.15g",\
    "%.15g;%.15g",creal((n)), cimag((n)), creal((v)), cimag((v)))
#endif

#define CHECKF(n,v)    CHECK(fabs((n)-(v))>1e-10,n,"%.15g","%.15g",(double)(n),\
    (double)(v))
#define CHECKFi(i,n,v) CHECKi(i,fabs((n)-(v)) > 1e-10,n,"%.15g","%.15g",\
    (double)(n),(double)(v))
#define CHECKI(n,v)    CHECK((n) != (v),n,"%" PRId64,"%" PRId64,(int64_t)(n),\
    (int64_t)(v))
#define CHECKIi(i,n,v) CHECKi(i,(int64_t)(n) != (int64_t)(v),n,"%" PRId64,\
    "%" PRId64, (int64_t)(n),(int64_t)(v))
#define CHECKNAN(n)    CHECK(!isnan(n),n,"%.15g","%s",(double)(n),"nan")
#define CHECKNANi(i,n) CHECKi(i,!isnan(n),n,"%.15g","%s",(double)(n),"nan")
#define CHECKP(n)      CHECK((n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPi(i,n)   CHECKi(i,(n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPN(n)     CHECK((n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKPNi(i,n)  CHECKi(i,(n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKPP(n,v)   CHECK((n) != (v),n,"%p","%p",n,v)
#define CHECKPPi(i,n,v) CHECKi(i,(n) != (v),n,"%p","%p",n,v)
#define CHECKS(n,v)    CHECK((n == NULL) || strcmp((n),(v)),n,"\"%s\"",\
    "\"%s\"",(n),(v));
#define CHECKSi(i,n,v) CHECKi(i,(n == NULL) || strcmp((n),(v)),n,"\"%s\"",\
    "\"%s\"",(n),(v));
#define CHECKSp(n,v)   CHECK((n == NULL) || strncmp((n),(v), sizeof(v) - 1),n,\
    "\"%s\"","\"%s\"",(n),(v));
#define CHECKBOS(n,v)  CHECK((n == NULL) || strncmp((n),(v),strlen(v)-1),n,\
    "\"%s\"","\"%s\"",(n),(v));
#define CHECKSS(n,v)  CHECK((n == NULL) || strstr((n),(v)) == NULL,n,\
    "\"%s\"","...\"%s\"...",(n),(v));
#define CHECKEOS(n,v)  CHECK(strcmp((n) + strlen(n) - sizeof(v) + 1,(v)),n,\
    "...\"%s\"","\"%s\"",(n) + strlen(n) - sizeof(v) + 1,(v));
#define CHECKU(n,v)    CHECK((n) != (v),n,"%" PRIu64,"%" PRIu64,\
    (uint64_t)(n),(uint64_t)(v))
#define CHECKUi(i,n,v) CHECKi(i,(n) != (v),n,"%" PRIu64,"%" PRIu64,\
    (uint64_t)(n),(uint64_t)(v))
#define CHECKX(n,v)    CHECK((n) != (v),n,"0x%" PRIX64,"0x%" PRIX64,\
    (uint64_t)(n),(uint64_t)(v))
#define CHECKXi(i,n,v) CHECKi(i,(n) != (v),n,"0x%" PRIX64,"0x%" PRIX64,\
    (uint64_t)(n),(uint64_t)(v))
