/* Copyright (C) 2008-2011 D. V. Wiebe
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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#define NO_GETDATA_LEGACY_API
#include "getdata.h"

/* The type parameter:
 *   0: error codes
 *   1: open flags represented as INT in IDL
 *   2: open flags represented as LONG in IDL
 *   3: entry types
 *   4: data types
 *   5: delete/rename flags (not in IDL)
 *   6: protection levels
 *   7: callback actions (not in IDL)
 *   8: GD_E_FORMAT suberrors (not in IDL)
 *   9: special version codes
 *  10: gd_seek whence values
 *  11: gd_seek flags (not in IDL)
 *  12: window operations
 *  99: miscellaneous constants
 */
#define CONSTANT(s,f,t) { "GD_" #s, #s, f, GD_ ## s, t }
static struct {
  const char* lname; /* Long name */
  const char* sname; /* Short name */
  const char* fname; /* F77 name */
  long int value;
  int type;
} constant_list[] = {
  CONSTANT(E_OK,             "GD_EOK", 0),
  CONSTANT(E_OPEN,           "GD_EOP", 0),
  CONSTANT(E_FORMAT,         "GD_EFO", 0),
  CONSTANT(E_TRUNC,          "GD_ETR", 0),
  CONSTANT(E_CREAT,          "GD_ECR", 0),
  CONSTANT(E_BAD_CODE,       "GD_EBC", 0),
  CONSTANT(E_BAD_TYPE,       "GD_EBT", 0),
  CONSTANT(E_RAW_IO,         "GD_ERW", 0),
  CONSTANT(E_OPEN_FRAGMENT,  "GD_EOF", 0),
  CONSTANT(E_OPEN_INCLUDE,   "GD_EOI", 0), /* deprecated */
  CONSTANT(E_INTERNAL_ERROR, "GD_EIE", 0),
  CONSTANT(E_ALLOC,          "GD_EAL", 0),
  CONSTANT(E_RANGE,          "GD_ERA", 0),
  CONSTANT(E_OPEN_LINFILE,   "GD_EOL", 0),
  CONSTANT(E_RECURSE_LEVEL,  "GD_ERL", 0),
  CONSTANT(E_BAD_DIRFILE,    "GD_EBD", 0),
  CONSTANT(E_BAD_FIELD_TYPE, "GD_EBF", 0),
  CONSTANT(E_ACCMODE,        "GD_EAC", 0),
  CONSTANT(E_UNSUPPORTED,    "GD_UNS", 0),
  CONSTANT(E_UNKNOWN_ENCODING,"GD_EUE",0),
  CONSTANT(E_BAD_ENTRY,      "GD_EBE", 0),
  CONSTANT(E_DUPLICATE,      "GD_EDU", 0),
  CONSTANT(E_DIMENSION,      "GD_EDM", 0),
  CONSTANT(E_BAD_INDEX,      "GD_EBI", 0),
  CONSTANT(E_BAD_SCALAR,     "GD_EBS", 0),
  CONSTANT(E_BAD_REFERENCE,  "GD_EBR", 0),
  CONSTANT(E_PROTECTED,      "GD_EPT", 0),
  CONSTANT(E_DELETE,         "GD_EDL", 0),
  CONSTANT(E_ARGUMENT,       "GD_EAR", 0),
  CONSTANT(E_BAD_ENDIANNESS, "GD_EEN", 0), /* deprecated */
  CONSTANT(E_CALLBACK,       "GD_ECB", 0),
  CONSTANT(E_BAD_PROTECTION, "GD_EBP", 0), /* deprecated */
  CONSTANT(E_UNCLEAN_DB,     "GD_UCL", 0),
  CONSTANT(E_DOMAIN,         "GD_EDO", 0),
  CONSTANT(E_BAD_REPR,       "GD_ERP", 0),
  CONSTANT(E_BAD_VERSION,    "GD_EVR", 0),
  CONSTANT(E_FLUSH,          "GD_EFL", 0),
  CONSTANT(E_BOUNDS,         "GD_EBO", 0),
  CONSTANT(E_LINE_TOO_LONG,  "GD_ETL", 0),

  CONSTANT(RDONLY,           "GD_RO",  1),
  CONSTANT(RDWR,             "GD_RW",  1),
  CONSTANT(FORCE_ENDIAN,     "GD_FE",  1),
  CONSTANT(BIG_ENDIAN,       "GD_BE",  2),
  CONSTANT(LITTLE_ENDIAN,    "GD_LE",  2),
  CONSTANT(CREAT,            "GD_CR",  1),
  CONSTANT(EXCL,             "GD_EX",  1),
  CONSTANT(TRUNC,            "GD_TR",  1),
  CONSTANT(PEDANTIC,         "GD_PE",  1),
  CONSTANT(FORCE_ENCODING,   "GD_FC",  1),
  CONSTANT(VERBOSE,          "GD_VB",  1),
  CONSTANT(IGNORE_DUPS,      "GD_ID",  1),
  CONSTANT(IGNORE_REFS,      "GD_IR",  1),
  CONSTANT(PRETTY_PRINT,     "GD_PP",  1),
  CONSTANT(ARM_ENDIAN,       "GD_AE",  2),
  CONSTANT(NOT_ARM_ENDIAN,   "GD_NA",  2),
  CONSTANT(PERMISSIVE,       "GD_PM",  1),

  CONSTANT(AUTO_ENCODED,     "GD_EA",  1),
  CONSTANT(BZIP2_ENCODED,    "GD_EB",  2),
  CONSTANT(LZMA_ENCODED,     "GD_EL",  2),
  CONSTANT(UNENCODED,        "GD_EN",  2),
  CONSTANT(GZIP_ENCODED,     "GD_EG",  2),
  CONSTANT(TEXT_ENCODED,     "GD_ET",  2),
  CONSTANT(SLIM_ENCODED,     "GD_ES",  2),
  CONSTANT(ZZIP_ENCODED,     "GD_EZ",  2),

  CONSTANT(NO_ENTRY,         "GD_NOE", 3),
  CONSTANT(RAW_ENTRY,        "GD_RWE", 3),
  CONSTANT(LINCOM_ENTRY,     "GD_LCE", 3),
  CONSTANT(LINTERP_ENTRY,    "GD_LTE", 3),
  CONSTANT(BIT_ENTRY,        "GD_BTE", 3),
  CONSTANT(MULTIPLY_ENTRY,   "GD_MTE", 3),
  CONSTANT(PHASE_ENTRY,      "GD_PHE", 3),
  CONSTANT(INDEX_ENTRY,      "GD_IXE", 3),
  CONSTANT(POLYNOM_ENTRY,    "GD_PNE", 3),
  CONSTANT(SBIT_ENTRY,       "GD_SBE", 3),
  CONSTANT(DIVIDE_ENTRY,     "GD_DVE", 3),
  CONSTANT(RECIP_ENTRY,      "GD_RCE", 3),
  CONSTANT(WINDOW_ENTRY,     "GD_WDE", 3),
  CONSTANT(CONST_ENTRY,      "GD_COE", 3),
  CONSTANT(CARRAY_ENTRY,     "GD_CAE", 3),
  CONSTANT(STRING_ENTRY,     "GD_STE", 3),

  CONSTANT(NULL,             "GD_NUL", 4),
  CONSTANT(UINT8,            "GD_U8",  4),
  CONSTANT(INT8,             "GD_I8",  4),
  CONSTANT(UINT16,           "GD_U16", 4),
  CONSTANT(INT16,            "GD_I16", 4),
  CONSTANT(UINT32,           "GD_U32", 4),
  CONSTANT(INT32,            "GD_I32", 4),
  CONSTANT(UINT64,           "GD_U64", 4),
  CONSTANT(INT64,            "GD_I64", 4),
  CONSTANT(FLOAT32,          "GD_F32", 4),
  CONSTANT(FLOAT64,          "GD_F64", 4),
  CONSTANT(COMPLEX64,        "GD_C64", 4),
  CONSTANT(COMPLEX128,       "GDC128", 4),

  CONSTANT(DEL_META,         "GDD_MT", 5),
  CONSTANT(DEL_DATA,         "GDD_DT", 5),
  CONSTANT(DEL_DEREF,        "GDD_DR", 5),
  CONSTANT(DEL_FORCE,        "GDD_FO", 5),
  CONSTANT(REN_DATA,         "GDR_DT", 5),
  CONSTANT(REN_UPDB,         "GDR_UP", 5),

  CONSTANT(PROTECT_NONE,     "GDPR_N", 6),
  CONSTANT(PROTECT_FORMAT,   "GDPR_F", 6),
  CONSTANT(PROTECT_DATA,     "GDPR_D", 6),
  CONSTANT(PROTECT_ALL,      "GDPR_A", 6),

  CONSTANT(SYNTAX_ABORT,     "GDSX_A", 7),
  CONSTANT(SYNTAX_RESCAN,    "GDSX_S", 7),
  CONSTANT(SYNTAX_IGNORE,    "GDSX_I", 7),
  CONSTANT(SYNTAX_CONTINUE,  "GDSX_C", 7),

  CONSTANT(E_FORMAT_BAD_SPF, "GDF_SF", 8),
  CONSTANT(E_FORMAT_N_FIELDS,"GDF_NF", 8),
  CONSTANT(E_FORMAT_N_TOK,   "GDF_NT", 8),
  CONSTANT(E_FORMAT_NUMBITS, "GDF_NB", 8),
  CONSTANT(E_FORMAT_BITNUM,  "GDF_BN", 8),
  CONSTANT(E_FORMAT_BITSIZE, "GDF_SZ", 8),
  CONSTANT(E_FORMAT_CHARACTER,"GDF_CH", 8),
  CONSTANT(E_FORMAT_BAD_LINE,"GDF_LI", 8),
  CONSTANT(E_FORMAT_RES_NAME,"GDF_RN", 8),
  CONSTANT(E_FORMAT_ENDIAN,  "GDF_EN", 8),
  CONSTANT(E_FORMAT_BAD_TYPE,"GDF_TY", 8),
  CONSTANT(E_FORMAT_BAD_NAME,"GDF_NA", 8),
  CONSTANT(E_FORMAT_UNTERM,  "GDF_UM", 8),
  CONSTANT(E_FORMAT_METARAW, "GDF_MR", 8),
  CONSTANT(E_FORMAT_NO_PARENT,"GDF_PA", 8),
  CONSTANT(E_FORMAT_DUPLICATE,"GDF_DU", 8),
  CONSTANT(E_FORMAT_LOCATION,"GDF_LO", 8),
  CONSTANT(E_FORMAT_PROTECT, "GDF_PR", 8),
  CONSTANT(E_FORMAT_LITERAL, "GDF_LT", 8),
  CONSTANT(E_FORMAT_WINDOP,  "GDF_WO", 8),
  CONSTANT(E_FORMAT_META_META,"GDF_MM", 8),
  CONSTANT(E_FORMAT_ALIAS,   "GDF_AL", 8),

  CONSTANT(VERSION_CURRENT,  "GDSV_C", 9),
  CONSTANT(VERSION_LATEST,   "GDSV_L", 9),
  CONSTANT(VERSION_EARLIEST, "GDSV_E", 9),

  CONSTANT(SEEK_SET,         "GDSK_S", 10),
  CONSTANT(SEEK_CUR,         "GDSK_C", 10),
  CONSTANT(SEEK_END,         "GDSK_E", 10),
  CONSTANT(SEEK_WRITE,       "GDSK_W", 11),

  CONSTANT(WINDOP_UNK,       "GDW_UN", 12),
  CONSTANT(WINDOP_EQ,        "GDW_EQ", 12),
  CONSTANT(WINDOP_GE,        "GDW_GE", 12),
  CONSTANT(WINDOP_GT,        "GDW_GT", 12),
  CONSTANT(WINDOP_LE,        "GDW_LE", 12),
  CONSTANT(WINDOP_LT,        "GDW_LT", 12),
  CONSTANT(WINDOP_NE,        "GDW_NE", 12),
  CONSTANT(WINDOP_SET,       "GDW_ST", 12),
  CONSTANT(WINDOP_CLR,       "GDW_CL", 12),

  CONSTANT(MAX_LINE_LENGTH,  "GD_MLL", 99),
  CONSTANT(ALL_FRAGMENTS,    "GD_ALL", 99),
  CONSTANT(HERE,             "GD_HER", 99),
  CONSTANT(DIRFILE_STANDARDS_VERSION, "GD_DSV", 99),
  { NULL }
};

static void parameter(const char* cname, const char* name, int value,
    int free_form)
{
  if (free_form)
    printf("integer, parameter :: %s=%i\\\n", cname, value);
  else
    printf(
        "C     Corresponding to %s\\\n"
        "      INTEGER %s\\\n"
        "      PARAMETER (%s=%i)\\\n", cname, name, name, value);
}

void Fortran(void)
{
  int i, j;
  char c;

  for (i = 0; i < 2; ++i) {
    c = (i == 0) ? 'C' : '!';
    if (i == 0)
      printf("s/@PARAMETERS@/\\\nC     Error codes\\\n");
    else
      printf("s/@PARAMETERS95@/\\\n! Error codes\\\n");

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 0)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Open flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 1 || constant_list[j].type == 2)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Field types\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 3)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf(
        "\\\n%c Data types -- the unsigned types won't work when passed as\\\n"
        "%c               a return type, but we keep them anyways, since\\\n"
        "%c               they might appear as a result of calling %s\\\n",
        c, c, c, (i == 0) ? "GDGERW" : "fget_entry");

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 4)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Delete flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 5)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Protection levels\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 6)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Callback actions\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 7)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Syntax suberrors\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 8)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Special version codes\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 9)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Seek flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 10 || constant_list[j].type == 11)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Window operations\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 12)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Miscellaneous parameters\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == 99)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("/\n");
  }
  printf("s/@GD_MAX_LINE_LENGTH@/%i/\n", GD_MAX_LINE_LENGTH);
}

void Python(void)
{
  int i;

  printf(
      "/* This code is automatically generated.  "
      "Changes made here will be lost. */\n#define NO_IMPORT_ARRAY\n"
      "#include \"pygetdata.h\"\n"
      "const struct gdpy_constant_t gdpy_constant_list[] = {\n");
  
  for (i = 0; constant_list[i].lname != NULL; ++i)
    printf("{\"%s\", %s}, ", constant_list[i].sname, constant_list[i].lname);

  /* Python numerical type aliases */
  printf(
      "{\"INT\", GD_INT32}, "
      "{\"LONG\", GD_INT64}, "
      "{\"ULONG\", GD_UINT64}, "
      "{\"FLOAT\", GD_FLOAT64}, "
      "{\"COMPLEX\", GD_COMPLEX128}, "
      "{NULL, 0}};\n"
      );
}

void IDL(void)
{
  int i, n;

  /* The structure */
  printf(
      "/* This code is automatically generated.  "
      "Changes made here will be lost. */\n#define _LARGEFILE64_SOURCE 1\n"
      "#include <stdio.h>\n#include <idl_export.h>\n"
      "IDL_STRUCT_TAG_DEF gdidl_constants[] = {\n"
      );

  for (i = 0; constant_list[i].lname != NULL; ++i)
    if ((constant_list[i].type != 1) && (constant_list[i].type != 5) &&
        (constant_list[i].type != 7) && (constant_list[i].type != 8) &&
        (constant_list[i].type != 11))
    {
      printf("{ \"%s\", 0, (void*)IDL_TYP_%s }, ", constant_list[i].sname,
          (constant_list[i].type == 2) ? "LONG" : "INT"); 
    }

  printf("{ NULL }};\n");

  /* The initialisation function */
  printf(
      "extern IDL_StructDefPtr gdidl_const_def;"
      "IDL_VPTR gdidl_generate_constants(int argc, IDL_VPTR argv[], char *argk)"
      "{"
      "IDL_VPTR r;"
      "IDL_MEMINT dims[] = { 1 };"
      "void* data = IDL_MakeTempStruct(gdidl_const_def, 1,dims, &r, IDL_TRUE);"
      "\n");

  for (n = i = 0; constant_list[i].lname != NULL; ++i)
    if ((constant_list[i].type != 1) && (constant_list[i].type != 5) &&
        (constant_list[i].type != 7) && (constant_list[i].type != 8) &&
        (constant_list[i].type != 11))
    {
      printf("*(IDL_%s*)(data + IDL_StructTagInfoByIndex(gdidl_const_def, %i, "
          "IDL_MSG_LONGJMP, NULL)) = %li;\n", (constant_list[i].type == 2) ?
          "LONG" : "INT", n++, constant_list[i].value);
    }

  printf("return r; }\n");
}

void Perl(void)
{
  int i;

  printf("s/@PARAMETERS@/");

  printf("our $VERSION = %i.%04i;\\\n", GETDATA_MAJOR, GETDATA_MINOR * 100 +
      GETDATA_REVISION);
  for (i = 0; constant_list[i].lname != NULL; ++i)
    printf("our $%s = %li;\\\n", constant_list[i].sname,
        constant_list[i].value);

  printf("/\n");
  printf("s/@PARAMLIST@/");

  for (i = 0; constant_list[i].lname != NULL; ++i)
    printf("%s ", constant_list[i].sname, constant_list[i].value);
  printf("/\n");
}

int main(int argc, char* argv[])
{
  if (argv[1][0] == 'f')
    Fortran();
  else if (argv[1][0] == 'p')
    Python();
  else if (argv[1][0] == 'i')
    IDL();
  else if (argv[1][0] == 'P')
    Perl();

  return 0;
}
