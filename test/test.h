/* Copyright (C) 2010-2012 D. V. Wiebe
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
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* for MSVCRT _getcwd */
#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#if defined HAVE__ISNAN && ! defined HAVE_ISNAN
#define isnan _isnan
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

/* rm for MSVC */
#ifdef _MSC_VER
#define rmdirfile() system("rmdir /q/s dirfile");
#else
#define rmdirfile() chmod("dirfile", 0755); system("rm -rf dirfile");
#endif

/* sleep for WIN32/64 */
#if defined _WIN32 || defined _WIN64
#include <windows.h>
#define sleep(x) Sleep(1000 * (x))
#endif

/* path munging for WIN32/64 */
#if defined _WIN32 || defined _WIN64
#define gd_pathwrite(x,y) do { \
  char *ptr; \
  for (ptr = y; *ptr != '\0'; ++ptr) { \
    if (*ptr == '\\') write(x,ptr,1); \
    write(x,ptr,1); \
  } \
} while (0)
#else
#define gd_pathwrite(x,y) write(x,y,strlen(y))
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

#define strcmpn(n,v) (((n) == NULL) ? 1 : strcmp((n),(v)))

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
#define CHECKI(n,v)    CHECK((n) != (v),n,"%lli","%lli",(long long)(n),\
    (long long)(v))
#define CHECKIi(i,n,v) CHECKi(i,(long long)(n) != (long long)(v),n,"%lli",\
    "%lli", (long long)(n),(long long)(v))
#define CHECKNAN(n)    CHECK(!isnan(n),n,"%.15g","%s",(double)(n),"nan")
#define CHECKNANi(i,n) CHECKi(i,!isnan(n),n,"%.15g","%s",(double)(n),"nan")
#define CHECKP(n)      CHECK((n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPi(i,n)   CHECKi(i,(n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPN(n)     CHECK((n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKPNi(i,n)  CHECKi(i,(n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKS(n,v)    CHECK(strcmpn((n),(v)),n,"\"%s\"","\"%s\"",(n),(v));
#define CHECKSi(n,v)   CHECKi(i,strcmpn((n),(v)),n,"\"%s\"","\"%s\"",(n),(v));
#define CHECKSp(n,v)   CHECK(strncmp((n),(v), sizeof(v) - 1),n,"\"%s\"",\
    "\"%s\"",(n),(v));
#define CHECKBOS(n,v)  CHECK(strncmp((n),(v),strlen(v)-1),n,"\"%s\"","\"%s\"",\
    (n),(v));
#define CHECKEOS(n,v)  CHECK(strcmp((n) + strlen(n) - sizeof(v) + 1,(v)),n,\
    "...\"%s\"","\"%s\"",(n) + strlen(n) - sizeof(v) + 1,(v));
#define CHECKU(n,v)    CHECK((n) != (v),n,"%llu","%llu",\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKUi(i,n,v) CHECKi(i,(n) != (v),n,"%llu","%llu",\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKX(n,v)    CHECK((n) != (v),n,"0x%llX","0x%llX",\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKXi(i,n,v) CHECKi(i,(n) != (v),n,"0x%llX","0x%llX",\
    (unsigned long long)(n),(unsigned long long)(v))
