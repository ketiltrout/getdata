/* Copyright (C) 2013, 2014 D. V. Wiebe
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
#ifndef PHP_GETDATA_H
#define PHP_GETDATA_H

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"

/* undefine cruft */
#undef NDEBUG
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_URL
#undef PACKAGE_VERSION
#undef restrict
#include "internal.h"

extern zend_module_entry getdata_module_entry;
#define phpext_getdata_ptr &getdata_module_entry

#ifdef PHP_WIN32
# define PHP_GETDATA_API __declspec(dllexport)
#elif defined(__GNUC__) && __GNUC__ >= 4
# define PHP_GETDATA_API __attribute__ ((visibility("default")))
#else
# define PHP_GETDATA_API
#endif

#ifdef ZTS
#include "TSRM.h"
#endif

#ifndef PHP_FE_END
#define PHP_FE_END ZEND_FE_END
#endif

#ifndef ZEND_FE_END
#define ZEND_FE_END { NULL, NULL, NULL, 0, 0 }
#endif

void gdphp_register_constants(int module_number);

#endif
