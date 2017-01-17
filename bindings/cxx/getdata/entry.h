// Copyright (C) 2008-2013, 2015-2017 D. V. Wiebe
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

#include <getdata/types.h>
#include <complex>

namespace GetData {

  class Dirfile;

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

      int Move(int new_fragment, unsigned flags = 0);

      int Rename(const char* new_name, unsigned flags = 0);

      /* Specific data */
      virtual const char *Input(int index) const {
        return (CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.in_fields[index] : NULL;
      };

      virtual int ComplexScalars() const {
        if (E.field_type == GD_LINCOM_ENTRY ||
            E.field_type == GD_POLYNOM_ENTRY ||
            E.field_type == GD_RECIP_ENTRY)
          if (E.flags & GD_EN_COMPSCAL)
            return 1;

        return 0;
      }

      unsigned int Flags() const { return E.flags; };

      virtual const char *Scalar(int index) const;

      virtual int ScalarIndex(int index) const;

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

      virtual double Scale(int index) const {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.u.lincom.m[index] : 0;
      }

      virtual std::complex<double> CScale(int index) const {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.u.lincom.n_fields, index))
          ? std::complex<double>(E.u.lincom.cm[index][0],
              E.u.lincom.cm[index][1]) : 0;
      }

      virtual double Offset(int index) const {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.u.lincom.b[index] : 0;
      }

      virtual std::complex<double> COffset(int index) const {
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
      virtual gd_int64_t Shift() const {
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

      virtual double Coefficient(int index) const {
        return (E.field_type == GD_POLYNOM_ENTRY && index <=
            E.u.polynom.poly_ord) ? E.u.polynom.a[index] : 0;
      }

      virtual std::complex<double> CCoefficient(int index) const {
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

      virtual int Period() const {
        return (E.field_type == GD_MPLEX_ENTRY) ? E.u.mplex.period : 0;
      }

      /* deprecated in GetData-0.8.4; use Period() instead */
      virtual int gd_deprecated CountMax() const { return Period(); }

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
