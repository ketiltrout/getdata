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
 * You should have received a copy of the GNU General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "getdata.h"

int main(void)
{
  FILE* stream;

  stream = fopen("make_parameters.sed.in", "w");
  if (stream == NULL) {
    perror("fopen: ");
    exit(1);
  }

  fprintf(stream, "s/@PARAMETERS@/\\\n"
      "C     Error codes\\\n"
      "C     Corresponding to GD_E_OK\\\n"
      "      INTEGER GD_EOK\\\n"
      "      PARAMETER (GD_EOK=%i)\\\n"
      "C     Corresponding to GD_E_OPEN\\\n"
      "      INTEGER GD_EOP\\\n"
      "      PARAMETER (GD_EOP=%i)\\\n"
      "C     Corresponding to GD_E_FORMAT\\\n"
      "      INTEGER GD_EFO\\\n"
      "      PARAMETER (GD_EFO=%i)\\\n"
      "C     Corresponding to GD_E_TRUNC\\\n"
      "      INTEGER GD_ETR\\\n"
      "      PARAMETER (GD_ETR=%i)\\\n"
      "C     Corresponding to GD_E_CREAT\\\n"
      "      INTEGER GD_ECR\\\n"
      "      PARAMETER (GD_ECR=%i)\\\n"
      "C     Corresponding to GD_E_BAD_CODE\\\n"
      "      INTEGER GD_EBC\\\n"
      "      PARAMETER (GD_EBC=%i)\\\n"
      "C     Corresponding to GD_E_BAD_TYPE\\\n"
      "      INTEGER GD_EBT\\\n"
      "      PARAMETER (GD_EBT=%i)\\\n"
      "C     Corresponding to GD_E_RAW_IO\\\n"
      "      INTEGER GD_ERW\\\n"
      "      PARAMETER (GD_ERW=%i)\\\n"
      "C     Corresponding to GD_E_OPEN_INCLUDE\\\n"
      "      INTEGER GD_EOI\\\n"
      "      PARAMETER (GD_EOI=%i)\\\n"
      "C     Corresponding to GD_E_INTERNAL_ERROR\\\n"
      "      INTEGER GD_EIE\\\n"
      "      PARAMETER (GD_EIE=%i)\\\n"
      "C     Corresponding to GD_E_EMPTY\\\n"
      "      INTEGER GD_EEM\\\n"
      "      PARAMETER (GD_EEM=%i)\\\n"
      "C     Corresponding to GD_E_ALLOC\\\n"
      "      INTEGER GD_EAL\\\n"
      "      PARAMETER (GD_EAL=%i)\\\n"
      "C     Corresponding to GD_E_RANGE\\\n"
      "      INTEGER GD_ERA\\\n"
      "      PARAMETER (GD_ERA=%i)\\\n"
      "C     Corresponding to GD_E_OPEN_LINFILE\\\n"
      "      INTEGER GD_EOL\\\n"
      "      PARAMETER (GD_EOL=%i)\\\n"
      "C     Corresponding to GD_E_RECURSE_LEVEL\\\n"
      "      INTEGER GD_ERL\\\n"
      "      PARAMETER (GD_ERL=%i)\\\n"
      "C     Corresponding to GD_E_BAD_DIRFILE\\\n"
      "      INTEGER GD_EBD\\\n"
      "      PARAMETER (GD_EBD=%i)\\\n"
      "C     Corresponding to GD_E_BAD_PUT_FIELD\\\n"
      "      INTEGER GD_EBP\\\n"
      "      PARAMETER (GD_EBP=%i)\\\n"
      "C     Corresponding to GD_E_ACCMODE\\\n"
      "      INTEGER GD_EAC\\\n"
      "      PARAMETER (GD_EAC=%i)\\\n",
    GD_E_OK, GD_E_OPEN, GD_E_FORMAT, GD_E_TRUNC, GD_E_CREAT, GD_E_BAD_CODE,
    GD_E_BAD_TYPE, GD_E_RAW_IO, GD_E_OPEN_INCLUDE, GD_E_INTERNAL_ERROR,
    GD_E_EMPTY, GD_E_ALLOC, GD_E_RANGE, GD_E_OPEN_LINFILE, GD_E_RECURSE_LEVEL,
    GD_E_BAD_DIRFILE, GD_E_BAD_PUT_FIELD, GD_E_ACCMODE);

  fprintf(stream, "C Open flags\\\n"
      "C     Correponding to GD_RDONLY\\\n"
      "      INTEGER GD_RO\\\n"
      "      PARAMETER (GD_RO=%i)\\\n"
      "C     Corresponding to GD_RDWR\\\n"
      "      INTEGER GD_RW\\\n"
      "      PARAMETER (GD_RW=%i)\\\n"
      "C     Corresponding to GD_CREAT\\\n"
      "      INTEGER GD_CR\\\n"
      "      PARAMETER (GD_CR=%i)\\\n"
      "C     Corresponding to GD_EXCL\\\n"
      "      INTEGER GD_EX\\\n"
      "      PARAMETER (GD_EX=%i)\\\n"
      "C     Corresponding to GD_TRUNC\\\n"
      "      INTEGER GD_TR\\\n"
      "      PARAMETER (GD_TR=%i)\\\n"
      "C     Corresponding to GD_BIG_ENDIAN\\\n"
      "      INTEGER GD_BE\\\n"
      "      PARAMETER (GD_BE=%i)\\\n"
      "C     Corresponding to GD_LITTLE_ENDIAN\\\n"
      "      INTEGER GD_LE\\\n"
      "      PARAMETER (GD_LE=%i)\\\n"
      "C     Corresponding to GD_FORCE_ENDIAN\\\n"
      "      INTEGER GD_FE\\\n"
      "      PARAMETER (GD_FE=%i)\\\n"
      "C     Corresponding to GD_PEDANTIC\\\n"
      "      INTEGER GD_PE\\\n"
      "      PARAMETER (GD_PE=%i)\\\n",
    GD_RDONLY, GD_RDWR, GD_CREAT, GD_EXCL, GD_TRUNC, GD_BIG_ENDIAN,
    GD_LITTLE_ENDIAN, GD_FORCE_ENDIAN, GD_PEDANTIC);

  fprintf(stream,
      "C Field types\\\n"
      "C     Correpsonding to GD_NO_ENTRY\\\n"
      "      INTEGER GD_NOE\\\n"
      "      PARAMETER (GD_NOE=%i)\\\n"
      "C     Correpsonding to GD_RAW_ENTRY\\\n"
      "      INTEGER GD_RWE\\\n"
      "      PARAMETER (GD_RWE=%i)\\\n"
      "C     Correpsonding to GD_LINCOM_ENTRY\\\n"
      "      INTEGER GD_LCE\\\n"
      "      PARAMETER (GD_LCE=%i)\\\n"
      "C     Correpsonding to GD_LINTERP_ENTRY\\\n"
      "      INTEGER GD_LTE\\\n"
      "      PARAMETER (GD_LTE=%i)\\\n"
      "C     Correpsonding to GD_BIT_ENTRY\\\n"
      "      INTEGER GD_BTE\\\n"
      "      PARAMETER (GD_BTE=%i)\\\n"
      "C     Correpsonding to GD_MULTIPLY_ENTRY\\\n"
      "      INTEGER GD_MTE\\\n"
      "      PARAMETER (GD_MTE=%i)\\\n"
      "C     Correpsonding to GD_PHASE_ENTRY\\\n"
      "      INTEGER GD_PHE\\\n"
      "      PARAMETER (GD_PHE=%i)\\\n\\\n",
      GD_NO_ENTRY, GD_RAW_ENTRY, GD_LINCOM_ENTRY, GD_LINTERP_ENTRY,
      GD_BIT_ENTRY, GD_MULTIPLY_ENTRY, GD_PHASE_ENTRY);

  fprintf(stream,
      "C Data types -- the unsigned type won't work when passed as a return\\\n"
      "C               type, but we keep them anyways, since they might\\\n"
      "C               appear as a result of calling GDFERW\\\n"
      "C     Corresponding to GD_NULL\\\n"
      "      INTEGER GD_NUL\\\n"
      "      PARAMETER (GD_NUL=%i)\\\n"
      "C     Corresponding to GD_UINT8\\\n"
      "      INTEGER GD_U8\\\n"
      "      PARAMETER (GD_U8=%i)\\\n"
      "C     Corresponding to GD_INT8\\\n"
      "      INTEGER GD_I8\\\n"
      "      PARAMETER (GD_I8=%i)\\\n"
      "C     Corresponding to GD_UINT16\\\n"
      "      INTEGER GD_U16\\\n"
      "      PARAMETER (GD_U16=%i)\\\n"
      "C     Corresponding to GD_INT16\\\n"
      "      INTEGER GD_I16\\\n"
      "      PARAMETER (GD_I16=%i)\\\n"
      "C     Corresponding to GD_UINT32\\\n"
      "      INTEGER GD_U32\\\n"
      "      PARAMETER (GD_U32=%i)\\\n"
      "C     Corresponding to GD_INT32\\\n"
      "      INTEGER GD_I32\\\n"
      "      PARAMETER (GD_I32=%i)\\\n"
      "C     Corresponding to GD_UINT64\\\n"
      "      INTEGER GD_U64\\\n"
      "      PARAMETER (GD_U64=%i)\\\n"
      "C     Corresponding to GD_INT64\\\n"
      "      INTEGER GD_I64\\\n"
      "      PARAMETER (GD_I64=%i)\\\n"
      "C     Corresponding to GD_FLOAT32\\\n"
      "      INTEGER GD_F32\\\n"
      "      PARAMETER (GD_F32=%i)\\\n"
      "C     Corresponding to GD_FLOAT64\\\n"
      "      INTEGER GD_F64\\\n"
      "      PARAMETER (GD_F64=%i)/\n",
    GD_NULL, GD_UINT8, GD_INT8, GD_UINT16, GD_INT16, GD_UINT32, GD_INT32,
    GD_UINT64, GD_INT64, GD_FLOAT32, GD_FLOAT64);

  fprintf(stream, "s/@PARAMETERS95@/\\\n"
      "! Error codes\\\n"
      "integer, parameter :: GD_E_OK=%i\\\n"
      "integer, parameter :: GD_E_OPEN=%i\\\n"
      "integer, parameter :: GD_E_FORMAT=%i\\\n"
      "integer, parameter :: GD_E_TRUNC=%i\\\n"
      "integer, parameter :: GD_E_CREAT=%i\\\n"
      "integer, parameter :: GD_E_BAD_CODE=%i\\\n"
      "integer, parameter :: GD_E_BAD_TYPE=%i\\\n"
      "integer, parameter :: GD_E_RAW_IO=%i\\\n"
      "integer, parameter :: GD_E_OPEN_INCLUDE=%i\\\n"
      "integer, parameter :: GD_E_INTERNAL_ERROR=%i\\\n"
      "integer, parameter :: GD_E_EMPTY=%i\\\n"
      "integer, parameter :: GD_E_ALLOC=%i\\\n"
      "integer, parameter :: GD_E_RANGE=%i\\\n"
      "integer, parameter :: GD_E_OPEN_LINFILE=%i\\\n"
      "integer, parameter :: GD_E_RECURSE_LEVEL=%i\\\n"
      "integer, parameter :: GD_E_BAD_DIRFILE=%i\\\n"
      "integer, parameter :: GD_E_BAD_PUT_FIELD=%i\\\n"
      "integer, parameter :: GD_E_ACCMODE=%i\\\n\\\n",
      GD_E_OK, GD_E_OPEN, GD_E_FORMAT, GD_E_TRUNC, GD_E_CREAT, GD_E_BAD_CODE,
      GD_E_BAD_TYPE, GD_E_RAW_IO, GD_E_OPEN_INCLUDE, GD_E_INTERNAL_ERROR,
      GD_E_EMPTY, GD_E_ALLOC, GD_E_RANGE, GD_E_OPEN_LINFILE, GD_E_RECURSE_LEVEL,
      GD_E_BAD_DIRFILE, GD_E_BAD_PUT_FIELD, GD_E_ACCMODE);

  fprintf(stream, "! Open flags\\\n"
      "integer, parameter :: GD_RDONLY=%i\\\n"
      "integer, parameter :: GD_RDWR=%i\\\n"
      "integer, parameter :: GD_CREAT=%i\\\n"
      "integer, parameter :: GD_EXCL=%i\\\n"
      "integer, parameter :: GD_TRUNC=%i\\\n"
      "integer, parameter :: GD_BIG_ENDIAN=%i\\\n"
      "integer, parameter :: GD_LITTLE_ENDIAN=%i\\\n"
      "integer, parameter :: GD_FORCE_ENDIAN=%i\\\n"
      "integer, parameter :: GD_PEDANTIC=%i\\\n\\\n",
      GD_RDONLY, GD_RDWR, GD_CREAT, GD_EXCL, GD_TRUNC, GD_BIG_ENDIAN,
      GD_LITTLE_ENDIAN, GD_FORCE_ENDIAN, GD_PEDANTIC);

  fprintf(stream, "! Field types\\\n"
      "integer, parameter :: GD_NO_ENTRY=%i\\\n"
      "integer, parameter :: GD_RAW_ENTRY=%i\\\n"
      "integer, parameter :: GD_LINCOM_ENTRY=%i\\\n"
      "integer, parameter :: GD_LINTERP_ENTRY=%i\\\n"
      "integer, parameter :: GD_BIT_ENTRY=%i\\\n"
      "integer, parameter :: GD_MULTIPLY_ENTRY=%i\\\n"
      "integer, parameter :: GD_PHASE_ENTRY=%i\\\n\\\n",
      GD_NO_ENTRY, GD_RAW_ENTRY, GD_LINCOM_ENTRY, GD_LINTERP_ENTRY,
      GD_BIT_ENTRY, GD_MULTIPLY_ENTRY, GD_PHASE_ENTRY);

  fprintf(stream,
      "! Data types -- F77 can't handle unsigned types, so we skip them\\\n"
      "integer, parameter :: GD_NULL=%i\\\n"
      "integer, parameter :: GD_UINT8=%i\\\n"
      "integer, parameter :: GD_INT8=%i\\\n"
      "integer, parameter :: GD_UINT16=%i\\\n"
      "integer, parameter :: GD_INT16=%i\\\n"
      "integer, parameter :: GD_UINT32=%i\\\n"
      "integer, parameter :: GD_INT32=%i\\\n"
      "integer, parameter :: GD_UINT64=%i\\\n"
      "integer, parameter :: GD_INT64=%i\\\n"
      "integer, parameter :: GD_FLOAT32=%i\\\n"
      "integer, parameter :: GD_FLOAT64=%i\\\n/",
    GD_NULL, GD_UINT8, GD_INT8, GD_UINT16, GD_INT16, GD_UINT32, GD_INT32,
    GD_UINT64, GD_INT64, GD_FLOAT32, GD_FLOAT64);


  fclose(stream);
}
