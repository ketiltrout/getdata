/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "getdata.h"

static void parameter(FILE* stream, const char* cname, const char* name,
    int value, int free_form)
{
  if (free_form)
    fprintf(stream, "integer, parameter :: %s=%i\\\n", cname, value);
  else
    fprintf(stream,
        "C     Corresponding to %s\\\n"
        "      INTEGER %s\\\n"
        "      PARAMETER (%s=%i)\\\n", cname, name, name, value);
}

int main(void)
{
  FILE* stream;
  int i;
  char c;

  stream = fopen("make_parameters.sed.in", "w");
  if (stream == NULL) {
    perror("fopen: ");
    exit(1);
  }

  for (i = 0; i < 2; ++i) {
    c = (i == 0) ? 'C' : '!';
    if (i == 0)
      fprintf(stream, "s/@PARAMETERS@/\\\nC     Error codes\\\n");
    else
      fprintf(stream, "s/@PARAMETERS95@/\\\n! Error codes\\\n");

    parameter(stream, "GD_E_OK",             "GD_EOK", GD_E_OK,             i);
    parameter(stream, "GD_E_OPEN",           "GD_EOP", GD_E_OPEN,           i);
    parameter(stream, "GD_E_FORMAT",         "GD_EFO", GD_E_FORMAT,         i);
    parameter(stream, "GD_E_TRUNC",          "GD_ETR", GD_E_TRUNC,          i);
    parameter(stream, "GD_E_CREAT",          "GD_ECR", GD_E_CREAT,          i);
    parameter(stream, "GD_E_BAD_CODE",       "GD_EBC", GD_E_BAD_CODE,       i);
    parameter(stream, "GD_E_BAD_TYPE",       "GD_EBT", GD_E_BAD_TYPE,       i);
    parameter(stream, "GD_E_RAW_IO",         "GD_ERW", GD_E_RAW_IO,         i);
    parameter(stream, "GD_E_OPEN_INCLUDE",   "GD_EOI", GD_E_OPEN_INCLUDE,   i);
    parameter(stream, "GD_E_INTERNAL_ERROR", "GD_EIE", GD_E_INTERNAL_ERROR, i);
    parameter(stream, "GD_E_ALLOC",          "GD_EAL", GD_E_ALLOC,          i);
    parameter(stream, "GD_E_RANGE",          "GD_ERA", GD_E_RANGE,          i);
    parameter(stream, "GD_E_OPEN_LINFILE",   "GD_EOL", GD_E_OPEN_LINFILE,   i);
    parameter(stream, "GD_E_RECURSE_LEVEL",  "GD_ERL", GD_E_RECURSE_LEVEL,  i);
    parameter(stream, "GD_E_BAD_DIRFILE",    "GD_EBD", GD_E_BAD_DIRFILE,    i);
    parameter(stream, "GD_E_BAD_FIELD_TYPE", "GD_EBF", GD_E_BAD_FIELD_TYPE, i);
    parameter(stream, "GD_E_ACCMODE",        "GD_EAC", GD_E_ACCMODE,        i);
    parameter(stream, "GD_E_UNSUPPORTED",    "GD_UNS", GD_E_UNSUPPORTED,    i);
    parameter(stream, "GD_E_BAD_ENTRY",      "GD_EBE", GD_E_BAD_ENTRY,      i);
    parameter(stream, "GD_E_DUPLICATE",      "GD_EDU", GD_E_DUPLICATE,      i);
    parameter(stream, "GD_E_DIMENSION",      "GD_EDM", GD_E_DIMENSION,      i);
    parameter(stream, "GD_E_BAD_INDEX",      "GD_EBI", GD_E_BAD_INDEX,      i);
    parameter(stream, "GD_E_BAD_SCALAR",     "GD_EBS", GD_E_BAD_SCALAR,     i);
    parameter(stream, "GD_E_BAD_REFERENCE",  "GD_EBR", GD_E_BAD_REFERENCE,  i);
    parameter(stream, "GD_E_PROTECTED",      "GD_EPT", GD_E_PROTECTED,      i);
    parameter(stream, "GD_E_DELETE",         "GD_EDL", GD_E_DELETE,         i);
    parameter(stream, "GD_E_BAD_ENDIANNESS", "GD_EEN", GD_E_BAD_ENDIANNESS, i);
    parameter(stream, "GD_E_CALLBACK",       "GD_ECB", GD_E_CALLBACK,       i);
    parameter(stream, "GD_E_BAD_PROTECTION", "GD_EBP", GD_E_BAD_PROTECTION, i);
    parameter(stream, "GD_E_UNCLEAN_DB",     "GD_UCL", GD_E_UNCLEAN_DB,     i);

    fprintf(stream, "\\\n%c Open flags\\\n", c);

    parameter(stream, "GD_RDONLY",           "GD_RO",  GD_RDONLY,           i);
    parameter(stream, "GD_RDWR",             "GD_RW",  GD_RDWR,             i);
    parameter(stream, "GD_FORCE_ENDIAN",     "GD_FE",  GD_FORCE_ENDIAN,     i);
    parameter(stream, "GD_BIG_ENDIAN",       "GD_BE",  GD_BIG_ENDIAN,       i);
    parameter(stream, "GD_LITTLE_ENDIAN",    "GD_LE",  GD_LITTLE_ENDIAN,    i);
    parameter(stream, "GD_CREAT",            "GD_CR",  GD_CREAT,            i);
    parameter(stream, "GD_EXCL",             "GD_EX",  GD_EXCL,             i);
    parameter(stream, "GD_TRUNC",            "GD_TR",  GD_TRUNC,            i);
    parameter(stream, "GD_PEDANTIC",         "GD_PE",  GD_PEDANTIC,         i);
    parameter(stream, "GD_FORCE_ENCODING",   "GD_FC",  GD_FORCE_ENCODING,   i);
    parameter(stream, "GD_VERBOSE",          "GD_VB",  GD_VERBOSE,          i);
    parameter(stream, "GD_IGNORE_DUPS",      "GD_ID",  GD_IGNORE_DUPS,      i);
    parameter(stream, "GD_IGNORE_REFS",      "GD_IR",  GD_IGNORE_REFS,      i);

    parameter(stream, "GD_AUTO_ENCODED",     "GD_EA",  GD_AUTO_ENCODED,     i);
    parameter(stream, "GD_UNENCODED",        "GD_EN",  GD_UNENCODED,        i);
    parameter(stream, "GD_TEXT_ENCODED",     "GD_ET",  GD_TEXT_ENCODED,     i);
    parameter(stream, "GD_SLIM_ENCODED",     "GD_ES",  GD_SLIM_ENCODED,     i);
    parameter(stream, "GD_GZIP_ENCODED",     "GD_EG",  GD_GZIP_ENCODED,     i);
    parameter(stream, "GD_BZIP2_ENCODED",    "GD_EB",  GD_BZIP2_ENCODED,    i);

    fprintf(stream, "\\\n%c Field types\\\n", c);

    parameter(stream, "GD_NO_ENTRY",         "GD_NOE", GD_NO_ENTRY,         i);
    parameter(stream, "GD_RAW_ENTRY",        "GD_RWE", GD_RAW_ENTRY,        i);
    parameter(stream, "GD_LINCOM_ENTRY",     "GD_LCE", GD_LINCOM_ENTRY,     i);
    parameter(stream, "GD_LINTERP_ENTRY",    "GD_LTE", GD_LINTERP_ENTRY,    i);
    parameter(stream, "GD_BIT_ENTRY",        "GD_BTE", GD_BIT_ENTRY,        i);
    parameter(stream, "GD_MULTIPLY_ENTRY",   "GD_MTE", GD_MULTIPLY_ENTRY,   i);
    parameter(stream, "GD_PHASE_ENTRY",      "GD_PHE", GD_PHASE_ENTRY,      i);
    parameter(stream, "GD_INDEX_ENTRY",      "GD_IXE", GD_INDEX_ENTRY,      i);
    parameter(stream, "GD_CONST_ENTRY",      "GD_COE", GD_CONST_ENTRY,      i);
    parameter(stream, "GD_STRING_ENTRY",     "GD_STE", GD_STRING_ENTRY,     i);

    fprintf(stream,
        "\\\n%c Data types -- the unsigned types won't work when passed as\\\n"
        "%c               a return type, but we keep them anyways, since\\\n"
        "%c               they might appear as a result of calling %s\\\n",
        c, c, c, (i == 0) ? "GDGERW" : "fget_entry");

    parameter(stream, "GD_NULL",             "GD_NUL", GD_NULL,             i);
    parameter(stream, "GD_UINT8",            "GD_U8",  GD_UINT8,            i);
    parameter(stream, "GD_INT8",             "GD_I8",  GD_INT8,             i);
    parameter(stream, "GD_UINT16",           "GD_U16", GD_UINT16,           i);
    parameter(stream, "GD_INT16",            "GD_I16", GD_INT16,            i);
    parameter(stream, "GD_UINT32",           "GD_U32", GD_UINT32,           i);
    parameter(stream, "GD_INT32",            "GD_I32", GD_INT32,            i);
    parameter(stream, "GD_UINT64",           "GD_U64", GD_UINT64,           i);
    parameter(stream, "GD_INT64",            "GD_I64", GD_INT64,            i);
    parameter(stream, "GD_FLOAT32",          "GD_F32", GD_FLOAT32,          i);
    parameter(stream, "GD_FLOAT64",          "GD_F64", GD_FLOAT64,          i);
    fprintf(stream, "/\n");
  }

  fclose(stream);
}
