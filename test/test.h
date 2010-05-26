#include "../src/getdata.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <complex.h>

#define CHECK(e,n,nf,vf,...) \
  if (e) { r = 1; \
    fprintf(stderr, #n " = " nf " (expected " vf ")\n", __VA_ARGS__); }

#define CHECKi(i,e,n,nf,vf,...) \
  if (e) { r = 1; \
    fprintf(stderr, #i ":%i " #n " = " nf " (expected " vf ")\n", (int)i, \
        __VA_ARGS__); }

#define CHECKC(n,v)    CHECK(cabs((n)-(v))>1e-10,n,"%.15g;%.15g","%.15g;%.15g", \
    creal((n)), cimag((n)), creal((v)), cimag((v)))
#define CHECKCi(i,n,v) CHECKi(i,cabs((n)-(v))>1e-10,n,"%.15g;%.15g",\
    "%.15g;%.15g",creal((n)), cimag((n)), creal((v)), cimag((v)))
#define CHECKF(n,v)    CHECK(fabs((n)-(v))>1e-10,n,"%.15g","%.15g",(double)(n),\
    (double)(v))
#define CHECKFi(i,n,v) CHECKi(i,fabs((n)-(v)) > 1e-10,n,"%.15g","%.15g",\
    (double)(n),(double)(v))
#define CHECKI(n,v)    CHECK((n) != (v),n,"%lli","%lli",(long long)(n),\
    (long long)(v))
#define CHECKIi(i,n,v) CHECKi(i,(n) != (v),n,"%lli","%lli",(long long)(n),\
    (long long)(v))
#define CHECKP(n)      CHECK((n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPi(i,n)   CHECKi(i,(n) != NULL,n,"%p","%s",n,"NULL")
#define CHECKPN(n)     CHECK((n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKPNi(i,n)  CHECKi(i,(n) == NULL,n,"%p","%s",n,"non-NULL")
#define CHECKS(n,v)    CHECK(strcmp((n),(v)),n,"\"%s\"","\"%s\"",(n),(v));
#define CHECKSi(n,v)   CHECKi(i,strcmp((n),(v)),n,"\"%s\"","\"%s\"",(n),(v));
#define CHECKU(n,v)    CHECK((n) != (v),n,"%llu","%llu",\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKUi(i,n,v) CHECKi(i,(n) != (v),n,"%llu","%llu",\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKX(n,v)    CHECK((n) != (v),n,"0x%llX","0x%llX",\
    (unsigned long long)(n),(unsigned long long)(v))
#define CHECKXi(i,n,v) CHECKi(i,(n) != (v),n,"0x%llX","0x%llX",\
    (unsigned long long)(n),(unsigned long long)(v))
