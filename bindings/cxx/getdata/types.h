// Copyright (C) 2008-2012, 2015, 2016 D. V. Wiebe
//
///////////////////////////////////////////////////////////////////////////
//
// This file is part of the GetData project.
//
// GetData is free software; you can redistribute it and/or modify it under
// the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation; either version 2.1 of the License, or (at your
// option) any later version.
//
// GetData is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
// License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with GetData; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//

#ifndef GETDATA_TYPES_H
#define GETDATA_TYPES_H

// Enable the explicit 64-bit API (gd_getdata64() &c.)
#ifndef GD_64BIT_API
# define GD_64BIT_API
#endif

// Disable the legacy API since its symbols clash with us.
#ifndef GD_NO_LEGACY_API
# define GD_NO_LEGACY_API
#endif

// Use the C89 API since C++ compilers aren't required to support the
// C99 _Complex keyword
#ifndef GD_C89_API
# define GD_C89_API
#endif

#include <getdata.h>

namespace GetData {

  enum EncodingScheme {
    AutoEncoding   = GD_AUTO_ENCODED,
    Bzip2Encoding  = GD_BZIP2_ENCODED,
    FlacEncoding   = GD_FLAC_ENCODED,
    GzipEncoding   = GD_GZIP_ENCODED,
    RawEncoding    = GD_UNENCODED,
    SieEncoding    = GD_SIE_ENCODED,
    SlimEncoding   = GD_SLIM_ENCODED,
    TextEncoding   = GD_TEXT_ENCODED,
    ZzipEncoding   = GD_ZZIP_ENCODED,
    ZzslimEncoding = GD_ZZSLIM_ENCODED,
    UnsupportedEncoding = GD_ENC_UNSUPPORTED
  };

  enum DataType {
    Null      = GD_NULL,      Unknown    = GD_UNKNOWN,
    UInt8     = GD_UINT8,     Int8       = GD_INT8,
    UInt16    = GD_UINT16,    Int16      = GD_INT16,
    UInt32    = GD_UINT32,    Int32      = GD_INT32,
    UInt64    = GD_UINT64,    Int64      = GD_INT64,
    Float32   = GD_FLOAT32,   Float64    = GD_FLOAT64,
    Complex64 = GD_COMPLEX64, Complex128 = GD_COMPLEX128,
    String    = GD_STRING
  };

  enum EntryType {
    NoEntryType       = GD_NO_ENTRY,
    RawEntryType      = GD_RAW_ENTRY,
    LincomEntryType   = GD_LINCOM_ENTRY,
    LinterpEntryType  = GD_LINTERP_ENTRY,
    BitEntryType      = GD_BIT_ENTRY,
    MultiplyEntryType = GD_MULTIPLY_ENTRY,
    PhaseEntryType    = GD_PHASE_ENTRY,
    SBitEntryType     = GD_SBIT_ENTRY,
    PolynomEntryType  = GD_POLYNOM_ENTRY,
    ConstEntryType    = GD_CONST_ENTRY,
    CarrayEntryType   = GD_CARRAY_ENTRY,
    SarrayEntryType   = GD_SARRAY_ENTRY,
    StringEntryType   = GD_STRING_ENTRY,
    IndexEntryType    = GD_INDEX_ENTRY,
    DivideEntryType   = GD_DIVIDE_ENTRY,
    RecipEntryType    = GD_RECIP_ENTRY,
    WindowEntryType   = GD_WINDOW_ENTRY,
    MplexEntryType    = GD_MPLEX_ENTRY,
    IndirEntryType    = GD_INDIR_ENTRY,
    SindirEntryType   = GD_SINDIR_ENTRY
  };

  enum WindOpType {
    WindOpEq = GD_WINDOP_EQ,
    WindOpNe = GD_WINDOP_NE,
    WindOpGe = GD_WINDOP_GE,
    WindOpGt = GD_WINDOP_GT,
    WindOpLe = GD_WINDOP_LE,
    WindOpLt = GD_WINDOP_LT,
    WindOpSet = GD_WINDOP_SET,
    WindOpClr = GD_WINDOP_CLR
  };
}

#endif

