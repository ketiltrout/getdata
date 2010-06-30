// (C) 2008-2010 D. V. Wiebe
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

#define NO_GETDATA_LEGACY_API
#define GETDATA_C89_API

extern "C" {
#include <getdata.h>
}

#include <complex>

#define __gd_unused __attribute__ (( unused ))

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
    StringEntryType   = GD_STRING_ENTRY,
    IndexEntryType    = GD_INDEX_ENTRY
  };

  class Entry {
    friend class Dirfile;

    public:
      Entry();

      virtual ~Entry();

      /* Generic data */
      int Associated() { return (D != NULL); };

      const char *Name() { return E.field; };

      EntryType Type() { return (EntryType)E.field_type; };
      
      void Dissociate() { D = NULL; };

      int FragmentIndex() { return E.fragment_index; };

      int Move(int new_fragment, int move_data = 0);

      int Rename(const char* new_name, int move_data = 0);

      /* Specific data */
      virtual const char *Input(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ?
          E.in_fields[index] : NULL;
      };

      virtual int ComplexScalars() {
        return (E.field_type == GD_LINCOM_ENTRY ||
            E.field_type == GD_POLYNOM_ENTRY) ? E.comp_scal : 0;
      }

      virtual const char *Scalar(int index = 0);

      /* RAW methods */
      virtual gd_spf_t SamplesPerFrame() {
        return (E.field_type == GD_RAW_ENTRY) ? E.spf : 0;
      };

      virtual DataType RawType() {
        return (E.field_type == GD_RAW_ENTRY) ? (DataType)E.data_type : Unknown;
      };

      /* LINCOM methods */
      virtual int NFields() {
        return (E.field_type == GD_LINCOM_ENTRY) ? E.n_fields : 0;
      };

      virtual double Scale(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index)) ? E.m[index] : 0;
      }

      virtual std::complex<double> CScale(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index))
          ? std::complex<double>(E.cm[index][0], E.cm[index][1]) : 0;
      }

      virtual double Offset(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index)) ? E.b[index] : 0;
      }

      virtual std::complex<double> COffset(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index))
          ? std::complex<double>(E.cb[index][0], E.cb[index][1]) : 0;
      }

      /* LINTERP methods */
      virtual const char *Table() {
        return (E.field_type == GD_LINTERP_ENTRY) ? E.table : NULL;
      };

      /* (S)BIT methods */
      virtual gd_bit_t FirstBit() {
        return (E.field_type == GD_BIT_ENTRY) ? E.bitnum : -1;
      };

      virtual gd_bit_t NumBits() {
        return (E.field_type == GD_BIT_ENTRY) ? E.numbits : -1;
      };

      /* PHASE methods */
      virtual gd_shift_t Shift() {
        return (E.field_type == GD_PHASE_ENTRY) ? E.shift : 0;
      };

      /* CONST methods */
      virtual DataType ConstType() {
        return (E.field_type == GD_CONST_ENTRY) ? (DataType)E.const_type :
          Unknown;
      };

      /* POLYNOM methods */
      virtual int PolyOrd() {
        return (E.field_type == GD_POLYNOM_ENTRY) ? E.poly_ord : 0;
      };

      virtual double Coefficient(int index = 0) {
        return (E.field_type == GD_POLYNOM_ENTRY && index <= E.poly_ord)
          ? E.a[index] : 0;
      }

      virtual std::complex<double> CCoefficient(int index = 0) {
        return (E.field_type == GD_POLYNOM_ENTRY && index <= E.poly_ord)
          ? std::complex<double>(E.ca[index][0], E.ca[index][1]) : 0;
      }

      void SetName(const char* name);

      void SetFragmentIndex(int fragment_index);

    protected:
      Entry(Dirfile *dirfile, const char* field_code);

      static int CheckIndex(gd_entype_t field_type, int n_fields, int index);

      void SetDirfile(GetData::Dirfile* dirfile);

      gd_entry_t E;
      Dirfile* D;
  };
}

#endif
