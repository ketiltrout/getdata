#define GD_TEST
#include "../src/internal.h"
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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

#define CHECK(e,n,nf,vf,...) \
  if (e) { r = 1; \
    fprintf(stderr, #n " = " nf " (expected " vf ")\n", __VA_ARGS__); }

#define CHECKi(i,e,n,nf,vf,...) \
  if (e) { r = 1; \
    fprintf(stderr, #i ":%i " #n " = " nf " (expected " vf ")\n", (int)i, \
        __VA_ARGS__); }

#define CHECKC(n,v)    CHECK(cabs((n)-(v))>1e-10,n,"%.15g;%.15g","%.15g;%.15g", \
    creal((n)), cimag((n)), creal((v)), cimag((v)))
#define CHECKCi(i,n,v) CHECKi(i,cabs((n)-(v)) / cabs((v))>3e-6,n,"%.15g;%.15g",\
    "%.15g;%.15g",creal((n)), cimag((n)), creal((v)), cimag((v)))
#define CHECKF(n,v)    CHECK(fabs((n)-(v))>1e-10,n,"%.15g","%.15g",(double)(n),\
    (double)(v))
#define CHECKFi(i,n,v) CHECKi(i,fabs((n)-(v)) > 1e-10,n,"%.15g","%.15g",\
    (double)(n),(double)(v))
#define CHECKI(n,v)    CHECK((n) != (v),n,"%" PRIi64,"%" PRIi64,(long long)(n),\
    (long long)(v))
#define CHECKIi(i,n,v) CHECKi(i,(n) != (v),n,"%" PRIi64,"%" PRIi64,\
    (long long)(n),(long long)(v))
#define CHECKP(n)      CHECK((n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPi(i,n)   CHECKi(i,(n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPN(n)     CHECK((n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKPNi(i,n)  CHECKi(i,(n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKS(n,v)    CHECK(strcmp((n),(v)),n,"\"%s\"","\"%s\"",(n),(v));
#define CHECKSi(n,v)   CHECKi(i,strcmp((n),(v)),n,"\"%s\"","\"%s\"",(n),(v));
#define CHECKU(n,v)    CHECK((n) != (v),n,"%" PRIu64,"%" PRIu64,\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKUi(i,n,v) CHECKi(i,(n) != (v),n,"%" PRIu64,"%" PRIu64,\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKX(n,v)    CHECK((n) != (v),n,"0x%" PRIX64,"0x%" PRIX64,\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKXi(i,n,v) CHECKi(i,(n) != (v),n,"0x%" PRIX64,"0x%" PRIX64,\
    (unsigned long long)(n),(unsigned long long)(v))
