// Copyright (C) 2008-2012 D. V. Wiebe
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

#ifndef GETDATA_ENTRY_H
#define GETDATA_ENTRY_H

#ifndef _FILE_OFFSET_BITS
# define _FILE_OFFSET_BITS 64
#endif

#ifndef GD_NO_LEGACY_API
# define GD_NO_LEGACY_API
#endif
#ifndef GD_C89_API
# define GD_C89_API
#endif

extern "C" {
#include <getdata.h>
}

#include <complex>

namespace GetData {

  class Dirfile;

  enum DataType {
    Null      = GD_NULL,      Unknown    = GD_UNKNOWN,
    UInt8     = GD_UINT8,     Int8       = GD_INT8,
    UInt16    = GD_UINT16,    Int16      = GD_INT16,
    UInt32    = GD_UINT32,    Int32      = GD_INT32,
    UInt64    = GD_UINT64,    Int64      = GD_INT64,
    Float32   = GD_FLOAT32,   Float64    = GD_FLOAT64,
    Complex64 = GD_COMPLEX64, Complex128 = GD_COMPLEX128
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
    StringEntryType   = GD_STRING_ENTRY,
    IndexEntryType    = GD_INDEX_ENTRY,
    DivideEntryType   = GD_DIVIDE_ENTRY,
    RecipEntryType    = GD_RECIP_ENTRY,
    WindowEntryType   = GD_WINDOW_ENTRY,
    MplexEntryType    = GD_MPLEX_ENTRY
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

  class Entry {
    friend class Dirfile;

    public:
      Entry();

      virtual ~Entry();

      /* Generic data */
      int Associated() const { return (D != NULL); };

      const char *Name() const { return E.field; };

      EntryType Type() const { return (EntryType)E.field_type; };
      
      void Dissociate() { D = NULL; };

      int FragmentIndex() const { return E.fragment_index; };

      int Move(int new_fragment, int move_data = 0);

      int Rename(const char* new_name, int move_data = 0);

      /* Specific data */
      virtual const char *Input(int index = 0) const {
        return (CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.in_fields[index] : NULL;
      };

      virtual int ComplexScalars() const {
        return (E.field_type == GD_LINCOM_ENTRY ||
            E.field_type == GD_POLYNOM_ENTRY) ? E.comp_scal : 0;
      }

      virtual const char *Scalar(int index = 0) const;

      virtual int ScalarIndex(int index = 0) const;

      /* RAW methods */
      virtual unsigned int SamplesPerFrame() const {
        return (E.field_type == GD_RAW_ENTRY) ? E.u.raw.spf : 0;
      };

      virtual DataType RawType() const {
        return (E.field_type == GD_RAW_ENTRY) ? (DataType)E.u.raw.data_type :
          Unknown;
      };

      /* LINCOM methods */
      virtual int NFields() const {
        return (E.field_type == GD_LINCOM_ENTRY) ? E.u.lincom.n_fields : 0;
      };

      virtual double Scale(int index = 0) const {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.u.lincom.m[index] : 0;
      }

      virtual std::complex<double> CScale(int index = 0) const {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.u.lincom.n_fields, index))
          ? std::complex<double>(E.u.lincom.cm[index][0],
              E.u.lincom.cm[index][1]) : 0;
      }

      virtual double Offset(int index = 0) const {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.u.lincom.b[index] : 0;
      }

      virtual std::complex<double> COffset(int index = 0) const {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.u.lincom.n_fields, index))
          ? std::complex<double>(E.u.lincom.cb[index][0],
              E.u.lincom.cb[index][1]) : 0;
      }

      /* LINTERP methods */
      virtual const char *Table() const {
        return (E.field_type == GD_LINTERP_ENTRY) ? E.u.linterp.table : NULL;
      };

      /* (S)BIT methods */
      virtual int FirstBit() const {
        return (E.field_type == GD_BIT_ENTRY) ? E.u.bit.bitnum : -1;
      };

      virtual int NumBits() const {
        return (E.field_type == GD_BIT_ENTRY) ? E.u.bit.numbits : -1;
      };

      /* PHASE methods */
      virtual gd_shift_t Shift() const {
        return (E.field_type == GD_PHASE_ENTRY) ? E.u.phase.shift : 0;
      };

      /* CONST methods */
      virtual DataType ConstType() const {
        return (E.field_type == GD_CONST_ENTRY || E.field_type ==
            GD_CARRAY_ENTRY) ? (DataType)E.u.scalar.const_type : Unknown;
      };

      /* CARRAY methods */
      virtual size_t ArrayLen() const {
        return (E.field_type == GD_CARRAY_ENTRY) ? E.u.scalar.array_len : 0;
      };

      /* POLYNOM methods */
      virtual int PolyOrd() const {
        return (E.field_type == GD_POLYNOM_ENTRY) ? E.u.polynom.poly_ord : 0;
      };

      virtual double Coefficient(int index = 0) const {
        return (E.field_type == GD_POLYNOM_ENTRY && index <=
            E.u.polynom.poly_ord) ? E.u.polynom.a[index] : 0;
      }

      virtual std::complex<double> CCoefficient(int index = 0) const {
        return (E.field_type == GD_POLYNOM_ENTRY && index <=
            E.u.polynom.poly_ord) ?
          std::complex<double>(E.u.polynom.ca[index][0],
              E.u.polynom.ca[index][1]) : 0;
      }

      /* RECIP methods */
      virtual double Dividend() const {
        return (E.field_type == GD_RECIP_ENTRY) ? E.u.recip.dividend : 0;
      };

      virtual std::complex<double> CDividend() const {
        return (E.field_type == GD_RECIP_ENTRY) ?
          std::complex<double>(E.u.recip.cdividend[0], E.u.recip.cdividend[1]) :
          0;
      };

      /* WINDOW methods */
      virtual WindOpType WindOp() const {
        return (E.field_type == GD_WINDOW_ENTRY) ? (WindOpType)E.u.window.windop
          : (WindOpType)0;
      }

      virtual gd_triplet_t Threshold() const {
        gd_triplet_t zero;
        zero.r = 0;
        return (E.field_type == GD_WINDOW_ENTRY) ? E.u.window.threshold : zero;
      }

      /* MPLEX methods */
      virtual int CountVal() const {
        return (E.field_type == GD_MPLEX_ENTRY) ? E.u.mplex.count_val : 0;
      }

      virtual int CountMax() const {
        return (E.field_type == GD_MPLEX_ENTRY) ? E.u.mplex.count_max : 0;
      }

      /* Set methods */
      void SetName(const char* name);

      void SetFragmentIndex(int fragment_index);

    protected:
      Entry(const Dirfile *dirfile, const char* field_code);

      static int CheckIndex(gd_entype_t field_type, int n_fields,
          int index);

      void SetDirfile(const GetData::Dirfile* dirfile);

      void SetScalar(int n, const char *code);

      gd_entry_t E;
      const Dirfile* D;
  };
}

#endif
