// (C) 2009 D. V. Wiebe
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

#ifndef GETDATA_POLYNOMENTRY_H
#define GETDATA_POLYNOMENTRY_H

#define NO_GETDATA_LEGACY_API

extern "C" {
#include <getdata.h>
}
#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class PolynomEntry : public Entry {
    friend class Dirfile;

    public:
      PolynomEntry(const char* field_code, int poly_ord, const char* in_field,
          double* a, int fragment_index = 0);

      PolynomEntry(const char* field_code, int poly_ord, const char* in_field,
          double complex* ca, int fragment_index = 0);

      virtual const char *Input() {
        return E.in_fields[0];
      };

      virtual int NFields() {
        return E.n_fields;
      };

      virtual double Coefficient(int index = 0) {
        return (CheckIndex(E.field_type, E.poly_ord, index)) ? E.a[index] : 0;
      };

      virtual double complex CCoefficient(int index = 0) {
        return (CheckIndex(E.field_type, E.poly_ord, index)) ? E.a[index] : 0;
      };

      int SetPolyOrd(int poly_ord);
      int SetInput(const char* field);
      int SetCoefficient(double coeff, int index = 0);

    private:
      PolynomEntry(GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
