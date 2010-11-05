// (C) 2009, 2010 D. V. Wiebe
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

#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class PolynomEntry : public Entry {
    friend class Dirfile;

    public:
      PolynomEntry() : Entry::Entry() { E.field_type = GD_POLYNOM_ENTRY; };

      PolynomEntry(const char* field_code, int poly_ord, const char* in_field,
          double* a, int fragment_index = 0);

      PolynomEntry(const char* field_code, int poly_ord, const char* in_field,
          std::complex<double>* ca, int fragment_index = 0);

      virtual const char *Input() const { return E.in_fields[0]; };

      virtual int ComplexScalars() const { return E.comp_scal; }

      virtual int PolyOrd() const { return E.u.polynom.poly_ord; };

      virtual double Coefficient(int index = 0) const {
        return (index <= E.u.polynom.poly_ord) ? E.u.polynom.a[index] : 0;
      };

      virtual std::complex<double> CCoefficient(int index = 0) const {
        return (index <= E.u.polynom.poly_ord)
          ? std::complex<double>(E.u.polynom.ca[index][0],
              E.u.polynom.ca[index][1]) : 0;
      };

      virtual const char *Scalar(int index = 0) const;

      virtual int ScalarIndex(int index = 0) const;

      int SetPolyOrd(int poly_ord);
      int SetInput(const char* field);
      int SetCoefficient(double coeff, int index = 0);
      int SetCoefficient(const char* coeff, int index = 0);
      int SetCoefficient(std::complex<double> coeff, int index = 0);

    private:
      PolynomEntry(const GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
