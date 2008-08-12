AUTOMAKE_OPTIONS = foreign
INCLUDES = -Wall

if INCLUDE_LEGACY_API
LEGACY_C = legacy.c
GETDATA_LEGACY_H = getdata_legacy.h
endif

include_HEADERS = getdata.h ${GETDATA_LEGACY_H}

lib_LTLIBRARIES = libgetdata.la
libgetdata_la_SOURCES = getdata.c putdata.c ${LEGACY_C} getdata.h \
												getdata_internal.h
libgetdata_la_LDFLAGS = -version-info @GETDATA_IFACE_VERSION@:@GETDATA_IMPL_REVISION@:@GETDATA_IFACE_AGE@
libgetdata_la_LIBADD = -lm
