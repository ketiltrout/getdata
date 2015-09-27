// Copyright (C) 2008-2011, 2013, 2015 D. V. Wiebe
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

#ifndef GETDATA_LINCOMENTRY_H
#define GETDATA_LINCOMENTRY_H

#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class LincomEntry : public Entry {
    friend class Dirfile;

    public:
      LincomEntry() : Entry() { E.field_type = GD_LINCOM_ENTRY; };

      LincomEntry(const char* field_code, int n_fields, const char** in_fields,
          double* m, double* b, int fragment_index = 0);

      LincomEntry(const char* field_code, int n_fields, const char** in_fields,
          std::complex<double>* cm, std::complex<double>* cb,
          int fragment_index = 0);

      virtual const char *Input(int index) const {
        return (CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ? 
            E.in_fields[index] : NULL;
      };

      virtual int ComplexScalars() const {
        return (E.flags & GD_EN_COMPSCAL) ? 1 : 0;
      }

      virtual int NFields() const { return E.u.lincom.n_fields; };

      virtual double Scale(int index) const {
        return (CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.u.lincom.m[index] : 0;
      };

      virtual std::complex<double> CScale(int index) const {
        return (CheckIndex(E.field_type, E.u.lincom.n_fields, index))
          ? std::complex<double>(E.u.lincom.cm[index][0],
              E.u.lincom.cm[index][1]) : 0;
      };

      virtual double Offset(int index) const {
        return (CheckIndex(E.field_type, E.u.lincom.n_fields, index)) ?
          E.u.lincom.b[index] : 0;
      };

      virtual std::complex<double> COffset(int index) const {
        return (CheckIndex(E.field_type, E.u.lincom.n_fields, index))
          ? std::complex<double>(E.u.lincom.cb[index][0],
              E.u.lincom.cb[index][1]) : 0;
      };

      virtual const char *Scalar(int index) const;

      virtual int ScalarIndex(int index) const;

      int SetNFields(int nfields);
      int SetInput(const char* field, int index);
      int SetScale(double scale, int index);
      int SetScale(const char* scale, int index);
      int SetScale(std::complex<double> scale, int index);
      int SetOffset(double offset, int index);
      int SetOffset(const char* scale, int index);
      int SetOffset(std::complex<double> offset, int index);

    private:
      LincomEntry(const GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
