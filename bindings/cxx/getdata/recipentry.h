// Copyright (C) 2010, 2011, 2013, 2015 D. V. Wiebe
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

#ifndef GETDATA_RECIPENTRY_H
#define GETDATA_RECIPENTRY_H

#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class RecipEntry : public Entry {
    friend class Dirfile;

    public:
      RecipEntry() : Entry() { E.field_type = GD_RECIP_ENTRY; };

      RecipEntry(const char* field_code, const char* in_field,
          double dividend, int fragment_index = 0);

      RecipEntry(const char* field_code, const char* in_field,
          std::complex<double> cdividend, int fragment_index = 0);

      virtual const char *Input(int index = 0) const {
        return (index == 0) ? E.in_fields[0] : NULL;
      };

      virtual const char *Scalar(int index = 0) const {
        return (index == 0) ? E.scalar[0] :  NULL;
      };

      virtual int ScalarIndex(int index = 0) const {
        return (index == 0) ? E.scalar_ind[0] :  0;
      };

      virtual int ComplexScalars() const {
        return (E.flags & GD_EN_COMPSCAL) ? 1 : 0;
      }

      virtual double Dividend() const { return E.u.recip.dividend; };

      virtual std::complex<double> CDividend() const {
        return std::complex<double>(E.u.recip.cdividend[0],
            E.u.recip.cdividend[1]);
      };

      virtual int SetInput(const char* field);
      virtual int SetDividend(double coeff);
      virtual int SetDividend(const char* coeff);
      virtual int SetDividend(std::complex<double> coeff);

    private:
      RecipEntry(const GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
