/* Copyright (C) 2008-2016 D. V. Wiebe
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
#include "gd_config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#define GD_NO_LEGACY_API
#include "getdata.h"

/* parameter types (used by Fortran, IDL, and PHP) */
#define GDMP_ERR     0x00000001 /* error code */
#define GDMP_DEPERR  0x00000002 /* deprecated error codes not in PHP */
#define GDMP_OFLAG   0x00000004 /* open flags not in IDL */
#define GDMP_OFLAG_L 0x00000008 /* open flags represented as LONG in IDL */
#define GDMP_ENTYPE  0x00000010 /* entry types */
#define GDMP_DTYPE   0x00000020 /* data types */
#define GDMP_RFLAG   0x00000040 /* delete and rename flags (not in IDL) */
#define GDMP_PROT    0x00000080 /* protection levels */
#define GDMP_CALLBAK 0x00000100 /* callback actions (not in IDL) */
#define GDMP_FORMAT  0x00000200 /* GD_E_FORMAT suberrors (not in IDL) */
#define GDMP_VERS    0x00000400 /* special version codes */
#define GDMP_WHENCE  0x00000800 /* gd_seek whence values */
#define GDMP_SEEK    0x00001000 /* gd_seek flags (not in IDL) */
#define GDMP_WINDOP  0x00002000 /* window operations */
#define GDMP_DESYNC  0x00004000 /* desync flags (not in IDL) */
#define GDMP_ENLIST  0x00008000 /* entry_list constants (not in IDL) */
#define GDMP_EFLAG   0x00010000 /* entry flags */
#define GDMP_MISC_I  0x00020000 /* miscellaneous constants not in IDL */
#define GDMP_MISC    0x00040000 /* miscellaneous constants */

#define CONSTANT(s,f,t) { "GD_" #s, #s, f, GD_ ## s, t }
static struct {
  const char* lname; /* Long name */
  const char* sname; /* Short name */
  const char* fname; /* F77 name */
  long int value;
  unsigned type;
} constant_list[] = {
  CONSTANT(E_OK,             "GD_EOK", GDMP_ERR),
  CONSTANT(E_FORMAT,         "GD_EFO", GDMP_ERR),
  CONSTANT(E_CREAT,          "GD_ECR", GDMP_ERR),
  CONSTANT(E_BAD_CODE,       "GD_EBC", GDMP_ERR),
  CONSTANT(E_BAD_TYPE,       "GD_EBT", GDMP_ERR),
  CONSTANT(E_IO,             "GD_EIO", GDMP_ERR),
  CONSTANT(E_INTERNAL_ERROR, "GD_EIE", GDMP_ERR),
  CONSTANT(E_ALLOC,          "GD_EAL", GDMP_ERR),
  CONSTANT(E_RANGE,          "GD_ERA", GDMP_ERR),
  CONSTANT(E_LUT,            "GD_ELT", GDMP_ERR),
  CONSTANT(E_RECURSE_LEVEL,  "GD_ERL", GDMP_ERR),
  CONSTANT(E_BAD_DIRFILE,    "GD_EBD", GDMP_ERR),
  CONSTANT(E_BAD_FIELD_TYPE, "GD_EBF", GDMP_ERR),
  CONSTANT(E_ACCMODE,        "GD_EAC", GDMP_ERR),
  CONSTANT(E_UNSUPPORTED,    "GD_UNS", GDMP_ERR),
  CONSTANT(E_UNKNOWN_ENCODING,"GD_EUE",GDMP_ERR),
  CONSTANT(E_BAD_ENTRY,      "GD_EBE", GDMP_ERR),
  CONSTANT(E_DUPLICATE,      "GD_EDU", GDMP_ERR),
  CONSTANT(E_DIMENSION,      "GD_EDM", GDMP_ERR),
  CONSTANT(E_BAD_INDEX,      "GD_EBI", GDMP_ERR),
  CONSTANT(E_BAD_SCALAR,     "GD_EBS", GDMP_ERR),
  CONSTANT(E_BAD_REFERENCE,  "GD_EBR", GDMP_ERR),
  CONSTANT(E_PROTECTED,      "GD_EPT", GDMP_ERR),
  CONSTANT(E_DELETE,         "GD_EDL", GDMP_ERR),
  CONSTANT(E_ARGUMENT,       "GD_EAR", GDMP_ERR),
  CONSTANT(E_CALLBACK,       "GD_ECB", GDMP_ERR),
  CONSTANT(E_EXISTS,         "GD_EEX", GDMP_ERR),
  CONSTANT(E_UNCLEAN_DB,     "GD_UCL", GDMP_ERR),
  CONSTANT(E_DOMAIN,         "GD_EDO", GDMP_ERR),
  CONSTANT(E_BAD_REPR,       "GD_ERP", GDMP_ERR),
  CONSTANT(E_BOUNDS,         "GD_EBO", GDMP_ERR),
  CONSTANT(E_LINE_TOO_LONG,  "GD_ETL", GDMP_ERR),

  CONSTANT(E_BAD_ENDIANNESS, "GD_EEN", GDMP_DEPERR),
  CONSTANT(E_BAD_PROTECTION, "GD_EBP", GDMP_DEPERR),
  CONSTANT(E_BAD_VERSION,    "GD_EVR", GDMP_DEPERR),
  CONSTANT(E_OPEN_LINFILE,   "GD_EOL", GDMP_DEPERR),
  CONSTANT(E_FLUSH,          "GD_EFL", GDMP_DEPERR),
  CONSTANT(E_OPEN,           "GD_EOP", GDMP_DEPERR),
  CONSTANT(E_OPEN_FRAGMENT,  "GD_EOF", GDMP_DEPERR),
  CONSTANT(E_OPEN_INCLUDE,   "GD_EOI", GDMP_DEPERR),
  CONSTANT(E_RAW_IO,         "GD_ERW", GDMP_DEPERR),
  CONSTANT(E_TRUNC,          "GD_ETR", GDMP_DEPERR),

  CONSTANT(RDONLY,           "GD_RO", GDMP_OFLAG_L),
  CONSTANT(RDWR,             "GD_RW", GDMP_OFLAG_L),
  CONSTANT(FORCE_ENDIAN,     "GD_FE", GDMP_OFLAG),
  CONSTANT(BIG_ENDIAN,       "GD_BE", GDMP_OFLAG_L),
  CONSTANT(LITTLE_ENDIAN,    "GD_LE", GDMP_OFLAG_L),
  CONSTANT(CREAT,            "GD_CR", GDMP_OFLAG),
  CONSTANT(EXCL,             "GD_EX", GDMP_OFLAG),
  CONSTANT(TRUNC,            "GD_TR", GDMP_OFLAG),
  CONSTANT(PEDANTIC,         "GD_PE", GDMP_OFLAG),
  CONSTANT(FORCE_ENCODING,   "GD_FC", GDMP_OFLAG),
  CONSTANT(VERBOSE,          "GD_VB", GDMP_OFLAG_L),
  CONSTANT(IGNORE_DUPS,      "GD_ID", GDMP_OFLAG),
  CONSTANT(IGNORE_REFS,      "GD_IR", GDMP_OFLAG),
  CONSTANT(PRETTY_PRINT,     "GD_PP", GDMP_OFLAG_L),
  CONSTANT(ARM_ENDIAN,       "GD_AE", GDMP_OFLAG_L),
  CONSTANT(NOT_ARM_ENDIAN,   "GD_NA", GDMP_OFLAG_L),
  CONSTANT(PERMISSIVE,       "GD_PM", GDMP_OFLAG),
  CONSTANT(TRUNCSUB,         "GD_TS", GDMP_OFLAG),

  CONSTANT(AUTO_ENCODED,     "GDE_AU", GDMP_OFLAG),
  CONSTANT(BZIP2_ENCODED,    "GDE_BZ", GDMP_OFLAG_L),
  CONSTANT(FLAC_ENCODED,     "GDE_FL", GDMP_OFLAG_L),
  CONSTANT(GZIP_ENCODED,     "GDE_GZ", GDMP_OFLAG_L),
  CONSTANT(LZMA_ENCODED,     "GDE_LZ", GDMP_OFLAG_L),
  CONSTANT(SIE_ENCODED,      "GDE_SI", GDMP_OFLAG_L),
  CONSTANT(SLIM_ENCODED,     "GDE_SL", GDMP_OFLAG_L),
  CONSTANT(TEXT_ENCODED,     "GDE_TX", GDMP_OFLAG_L),
  CONSTANT(UNENCODED,        "GDE_UN", GDMP_OFLAG_L),
  CONSTANT(ZZSLIM_ENCODED,   "GDE_ZS", GDMP_OFLAG_L),
  CONSTANT(ZZIP_ENCODED,     "GDE_ZZ", GDMP_OFLAG_L),

  CONSTANT(NO_ENTRY,         "GD_NOE", GDMP_ENTYPE),
  CONSTANT(RAW_ENTRY,        "GD_RWE", GDMP_ENTYPE),
  CONSTANT(LINCOM_ENTRY,     "GD_LCE", GDMP_ENTYPE),
  CONSTANT(LINTERP_ENTRY,    "GD_LTE", GDMP_ENTYPE),
  CONSTANT(BIT_ENTRY,        "GD_BTE", GDMP_ENTYPE),
  CONSTANT(MULTIPLY_ENTRY,   "GD_MTE", GDMP_ENTYPE),
  CONSTANT(PHASE_ENTRY,      "GD_PHE", GDMP_ENTYPE),
  CONSTANT(INDEX_ENTRY,      "GD_IXE", GDMP_ENTYPE),
  CONSTANT(POLYNOM_ENTRY,    "GD_PNE", GDMP_ENTYPE),
  CONSTANT(SBIT_ENTRY,       "GD_SBE", GDMP_ENTYPE),
  CONSTANT(DIVIDE_ENTRY,     "GD_DVE", GDMP_ENTYPE),
  CONSTANT(RECIP_ENTRY,      "GD_RCE", GDMP_ENTYPE),
  CONSTANT(WINDOW_ENTRY,     "GD_WDE", GDMP_ENTYPE),
  CONSTANT(MPLEX_ENTRY,      "GD_MXE", GDMP_ENTYPE),
  CONSTANT(INDIR_ENTRY,      "GD_IDE", GDMP_ENTYPE),
  CONSTANT(SINDIR_ENTRY,     "GD_SDE", GDMP_ENTYPE),
  CONSTANT(CONST_ENTRY,      "GD_COE", GDMP_ENTYPE),
  CONSTANT(CARRAY_ENTRY,     "GD_CAE", GDMP_ENTYPE),
  CONSTANT(SARRAY_ENTRY,     "GD_SAE", GDMP_ENTYPE),
  CONSTANT(STRING_ENTRY,     "GD_STE", GDMP_ENTYPE),

  CONSTANT(NULL,             "GD_NUL", GDMP_DTYPE),
  CONSTANT(UINT8,            "GD_U8",  GDMP_DTYPE),
  CONSTANT(INT8,             "GD_I8",  GDMP_DTYPE),
  CONSTANT(UINT16,           "GD_U16", GDMP_DTYPE),
  CONSTANT(INT16,            "GD_I16", GDMP_DTYPE),
  CONSTANT(UINT32,           "GD_U32", GDMP_DTYPE),
  CONSTANT(INT32,            "GD_I32", GDMP_DTYPE),
  CONSTANT(UINT64,           "GD_U64", GDMP_DTYPE),
  CONSTANT(INT64,            "GD_I64", GDMP_DTYPE),
  CONSTANT(FLOAT32,          "GD_F32", GDMP_DTYPE),
  CONSTANT(FLOAT64,          "GD_F64", GDMP_DTYPE),
  CONSTANT(COMPLEX64,        "GD_C64", GDMP_DTYPE),
  CONSTANT(COMPLEX128,       "GDC128", GDMP_DTYPE),
  CONSTANT(STRING,           "GD_STR", GDMP_DTYPE),

  CONSTANT(DEL_META,         "GDD_MT", GDMP_RFLAG),
  CONSTANT(DEL_DATA,         "GDD_DT", GDMP_RFLAG),
  CONSTANT(DEL_DEREF,        "GDD_DR", GDMP_RFLAG),
  CONSTANT(DEL_FORCE,        "GDD_FO", GDMP_RFLAG),
  CONSTANT(REN_DATA,         "GDR_DT", GDMP_RFLAG),
  CONSTANT(REN_UPDB,         "GDR_UP", GDMP_RFLAG),
  CONSTANT(REN_DANGLE,       "GDR_DL", GDMP_RFLAG),
  CONSTANT(REN_FORCE,        "GDR_FO", GDMP_RFLAG),

  CONSTANT(PROTECT_NONE,     "GDPR_N", GDMP_PROT),
  CONSTANT(PROTECT_FORMAT,   "GDPR_F", GDMP_PROT),
  CONSTANT(PROTECT_DATA,     "GDPR_D", GDMP_PROT),
  CONSTANT(PROTECT_ALL,      "GDPR_A", GDMP_PROT),

  CONSTANT(SYNTAX_ABORT,     "GDSX_A", GDMP_CALLBAK),
  CONSTANT(SYNTAX_RESCAN,    "GDSX_S", GDMP_CALLBAK),
  CONSTANT(SYNTAX_IGNORE,    "GDSX_I", GDMP_CALLBAK),
  CONSTANT(SYNTAX_CONTINUE,  "GDSX_C", GDMP_CALLBAK),

  CONSTANT(E_FORMAT_BAD_SPF,  "GDF_SF", GDMP_FORMAT),
  CONSTANT(E_FORMAT_N_FIELDS, "GDF_NF", GDMP_FORMAT),
  CONSTANT(E_FORMAT_N_TOK,    "GDF_NT", GDMP_FORMAT),
  CONSTANT(E_FORMAT_NUMBITS,  "GDF_NB", GDMP_FORMAT),
  CONSTANT(E_FORMAT_BITNUM,   "GDF_BN", GDMP_FORMAT),
  CONSTANT(E_FORMAT_BITSIZE,  "GDF_SZ", GDMP_FORMAT),
  CONSTANT(E_FORMAT_CHARACTER,"GDF_CH", GDMP_FORMAT),
  CONSTANT(E_FORMAT_BAD_LINE, "GDF_LI", GDMP_FORMAT),
  CONSTANT(E_FORMAT_RES_NAME, "GDF_RN", GDMP_FORMAT),
  CONSTANT(E_FORMAT_ENDIAN,   "GDF_EN", GDMP_FORMAT),
  CONSTANT(E_FORMAT_BAD_TYPE, "GDF_TY", GDMP_FORMAT),
  CONSTANT(E_FORMAT_BAD_NAME, "GDF_NA", GDMP_FORMAT),
  CONSTANT(E_FORMAT_UNTERM,   "GDF_UM", GDMP_FORMAT),
  CONSTANT(E_FORMAT_METARAW,  "GDF_MR", GDMP_FORMAT),
  CONSTANT(E_FORMAT_NO_PARENT,"GDF_PA", GDMP_FORMAT),
  CONSTANT(E_FORMAT_DUPLICATE,"GDF_DU", GDMP_FORMAT),
  CONSTANT(E_FORMAT_LOCATION, "GDF_LO", GDMP_FORMAT),
  CONSTANT(E_FORMAT_PROTECT,  "GDF_PR", GDMP_FORMAT),
  CONSTANT(E_FORMAT_LITERAL,  "GDF_LT", GDMP_FORMAT),
  CONSTANT(E_FORMAT_WINDOP,   "GDF_WO", GDMP_FORMAT),
  CONSTANT(E_FORMAT_META_META,"GDF_MM", GDMP_FORMAT),
  CONSTANT(E_FORMAT_ALIAS,    "GDF_AL", GDMP_FORMAT),
  CONSTANT(E_FORMAT_MPLEXVAL, "GDF_MV", GDMP_FORMAT),

  CONSTANT(VERSION_CURRENT,  "GDSV_C", GDMP_VERS),
  CONSTANT(VERSION_LATEST,   "GDSV_L", GDMP_VERS),
  CONSTANT(VERSION_EARLIEST, "GDSV_E", GDMP_VERS),

  CONSTANT(SEEK_SET,         "GDSK_S", GDMP_WHENCE),
  CONSTANT(SEEK_CUR,         "GDSK_C", GDMP_WHENCE),
  CONSTANT(SEEK_END,         "GDSK_E", GDMP_WHENCE),
  CONSTANT(SEEK_WRITE,       "GDSK_W", GDMP_SEEK),

  CONSTANT(WINDOP_UNK,       "GDW_UN", GDMP_WINDOP),
  CONSTANT(WINDOP_EQ,        "GDW_EQ", GDMP_WINDOP),
  CONSTANT(WINDOP_GE,        "GDW_GE", GDMP_WINDOP),
  CONSTANT(WINDOP_GT,        "GDW_GT", GDMP_WINDOP),
  CONSTANT(WINDOP_LE,        "GDW_LE", GDMP_WINDOP),
  CONSTANT(WINDOP_LT,        "GDW_LT", GDMP_WINDOP),
  CONSTANT(WINDOP_NE,        "GDW_NE", GDMP_WINDOP),
  CONSTANT(WINDOP_SET,       "GDW_ST", GDMP_WINDOP),
  CONSTANT(WINDOP_CLR,       "GDW_CL", GDMP_WINDOP),

  CONSTANT(DESYNC_PATHCHECK, "GDDS_P", GDMP_DESYNC),
  CONSTANT(DESYNC_REOPEN,    "GDDS_O", GDMP_DESYNC),

  CONSTANT(ALL_ENTRIES,      "GDEN_X", GDMP_ENLIST),
  CONSTANT(VECTOR_ENTRIES,   "GDEN_V", GDMP_ENLIST),
  CONSTANT(SCALAR_ENTRIES,   "GDEN_S", GDMP_ENLIST),
  CONSTANT(ALIAS_ENTRIES,    "GDEN_A", GDMP_ENLIST),
  CONSTANT(ENTRIES_HIDDEN,   "GDEN_H", GDMP_ENLIST),
  CONSTANT(ENTRIES_NOALIAS,  "GDEN_N", GDMP_ENLIST),

  CONSTANT(REGEX_PCRE,       "GDRX_P", GDMP_ENLIST),
  CONSTANT(REGEX_EXTENDED,   "GDRX_X", GDMP_ENLIST),
  CONSTANT(REGEX_ICASE,      "GDRX_I", GDMP_ENLIST),
  CONSTANT(REGEX_CASELESS,   "GDRX_C", GDMP_ENLIST),
  CONSTANT(REGEX_JAVASCRIPT, "GDRX_J", GDMP_ENLIST),
  CONSTANT(REGEX_UNICODE,    "GDRX_U", GDMP_ENLIST),

  CONSTANT(EN_CALC,          "GDE_CA", GDMP_EFLAG),
  CONSTANT(EN_HIDDEN,        "GDE_HI", GDMP_EFLAG),
  CONSTANT(EN_COMPSCAL,      "GDE_CS", GDMP_EFLAG),

  CONSTANT(ALL_FRAGMENTS,    "GD_ALL", GDMP_MISC),
  CONSTANT(DEFAULT_LOOKBACK, "GDLB_D", GDMP_MISC),
  CONSTANT(DIRFILE_STANDARDS_VERSION, "GD_DSV", GDMP_MISC),
  CONSTANT(HERE,             "GD_HER", GDMP_MISC_I),
  CONSTANT(LOOKBACK_ALL,     "GDLB_A", GDMP_MISC_I),
  CONSTANT(MAX_LINE_LENGTH,  "GD_MLL", GDMP_MISC),
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
      if (constant_list[j].type == GDMP_ERR)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Deprecated error codes\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_DEPERR)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Open flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type & (GDMP_OFLAG | GDMP_OFLAG_L))
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Entry types\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_ENTYPE)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf(
        "\\\n%c Data types -- the unsigned types won't work when passed as\\\n"
        "%c               a return type; they are defined because they\\\n"
        "%c               may be returned by %s or %s\\\n", c, c, c,
        (i == 0) ? "GDGERW" : "fget_entry",
        (i == 0) ? "GDNTYP" : "fget_native_type");

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_DTYPE)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Delete and Rename flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_RFLAG)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Protection levels\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_PROT)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Callback actions\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_CALLBAK)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Syntax suberrors\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_FORMAT)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Special version codes\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_VERS)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Seek flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type & (GDMP_WHENCE | GDMP_SEEK))
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Window operations\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_WINDOP)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Desync flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_DESYNC)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Entry List codes and flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_ENLIST)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Entry object flags\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type == GDMP_EFLAG)
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("\\\n%c Miscellaneous parameters\\\n", c);

    for (j = 0; constant_list[j].lname != NULL; ++j)
      if (constant_list[j].type & (GDMP_MISC | GDMP_MISC_I))
        parameter(constant_list[j].lname, constant_list[j].fname,
            constant_list[j].value, i);

    printf("/\n");
  }
  printf("s/@GD_MAX_LINE_LENGTH@/%i/\n", GD_MAX_LINE_LENGTH);
  printf("s/@SIZEOF_VOID_P@/%i/\n", SIZEOF_VOID_P);
}

void Python(void)
{
  int i;

  printf(
      "/* This code is automatically generated.  "
      "Changes made here will be lost. */\n"
      "#include \"gdpy_intern.h\"\n"
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
    if (constant_list[i].type & (GDMP_ERR | GDMP_DEPERR | GDMP_OFLAG_L |
          GDMP_ENTYPE | GDMP_DTYPE | GDMP_PROT | GDMP_VERS | GDMP_WHENCE |
          GDMP_WINDOP | GDMP_EFLAG | GDMP_MISC))
    {
      printf("{ \"%s\", 0, (void*)IDL_TYP_%s }, ", constant_list[i].sname,
          (constant_list[i].type == GDMP_OFLAG_L) ? "LONG" : "INT"); 
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
    if (constant_list[i].type & (GDMP_ERR | GDMP_DEPERR | GDMP_OFLAG_L |
          GDMP_ENTYPE | GDMP_DTYPE | GDMP_PROT | GDMP_VERS | GDMP_WHENCE |
          GDMP_WINDOP | GDMP_EFLAG | GDMP_MISC))
    {
      printf("*(IDL_%s*)(data + IDL_StructTagInfoByIndex(gdidl_const_def, %i, "
          "IDL_MSG_LONGJMP, NULL)) = %li;\n",
          (constant_list[i].type == GDMP_OFLAG_L) ?  "LONG" : "INT",
          n++, constant_list[i].value);
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
    printf("%s ", constant_list[i].sname);
  printf("/\n");
}

/* Since this file gets installed, we're a little more chatty here than usual */
void MatLab(void)
{
  int i;

  fputs("function GD = getdata_constants()\n", stdout);

  fputs("% GETDATA_CONSTANTS  Define GetData symbolic constants\n"
      "%\n"
      "%   GETDATA_CONSTANTS  produces a structure containing the symbolic "
      "constants\n"
      "%   used by the GetData bindings.  Member names of the structure "
      "correspond to\n"
      "%   names of symbolic constants used in the GetData C API.\n"
      "%\n"
      "%   Although it can be used in immediate context by doing something "
      "like\n"
      "%\n"
      "%     >> GETDATA_CONSTANTS.FLOAT64\n"
      "%\n"
      "%     ans =\n"
      "%\n"
      "%              136\n"
      "%\n"
      "%   it is usually assigned to a variable, which prevents having to "
      "evaluate this\n"
      "%   function more than once.  We recommend calling this variable GD:\n"
      "%\n"
      "%     >> GD = GETDATA_CONSTANTS;\n"
      "%     >> GD.FLOAT64\n"
      "%\n"
      "%     ans =\n"
      "%\n"
      "%              136\n"
      "%\n"
      "%   providing more succinct symbol names which closely resemble the "
      "cor-\n"
      "%   respondng C API symbol names (e.g. GD_FLOAT64).  In the "
      "documentation for\n"
      "%   these bindings, we assume such a GD variable has been defined, and "
      "refer to\n"
      "%   symbolic constants as GD.<...> when necessary.\n"
      "%\n"
      "%   See also GETDATA\n\n", stdout);

  fputs("  GD = struct(...\n"
      "    'VERSION', '" GD_GETDATA_VERSION "'", stdout);

  for (i = 0; constant_list[i].lname != NULL; ++i)
    printf(", ...\n    '%s', int32(%li)", constant_list[i].sname,
        constant_list[i].value);

  printf(" ...\n  );\nend\n");

  fputs("\n% Copyright (C) 2013 D. V. Wiebe\n%\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n%\n"
      "% This file is part of the GetData project.\n%\n"
      "% GetData is free software; you can redistribute it and/or modify it "
      "under\n"
      "% the terms of the GNU Lesser General Public License as published by "
      "the\n"
      "% Free Software Foundation; either version 2.1 of the License, or (at "
      "your\n"
      "% option) any later version.\n%\n"
      "% GetData is distributed in the hope that it will be useful, but "
      "WITHOUT\n"
      "% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY "
      "or\n"
      "% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public\n"
      "% License for more details.\n%\n"
      "% You should have received a copy of the GNU Lesser General Public "
      "License\n"
      "% along with GetData; if not, write to the Free Software Foundation, "
      "Inc.,\n"
      "% 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA\n", stdout);
}

void PHP(void)
{
  int i;

  puts(
      "/* This code is automatically generated.  "
      "Changes made here will be lost. */\n"
      "#include \"php_getdata.h\"\n"
      "void gdphp_register_constants(int module_number) {"
      "dtrace(\"%i\", module_number); TSRMLS_FETCH();"
      );
  
  for (i = 0; constant_list[i].lname != NULL; ++i)
    if (constant_list[i].type != GDMP_DEPERR)
      printf("GDPHP_REGISTER_LONG_CONSTANT(\"%s\", %li, module_number); ",
          constant_list[i].lname, constant_list[i].value);

  puts("dreturnvoid();}");
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
  else if (argv[1][0] == 'm')
    MatLab();
  else if (argv[1][0] == 'h')
    PHP();
  else
    return 1;

  return 0;
}
