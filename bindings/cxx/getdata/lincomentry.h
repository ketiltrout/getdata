// (C) 2008 D. V. Wiebe
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

#define NO_GETDATA_LEGACY_API

extern "C" {
#include <getdata.h>
}
#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class LincomEntry : public Entry {
    friend class Dirfile;

    public:
      LincomEntry(const char* field_code, int n_fields, const char** in_fields,
          double* m, double* b, int fragment_index = 0);

      LincomEntry(const char* field_code, int n_fields, const char** in_fields,
          double complex* cm, double complex* cb, int fragment_index = 0);

      virtual const char *Input(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? 
            E.in_fields[index] : NULL;
      };

      virtual int NFields() {
        return E.n_fields;
      };

      virtual double Scale(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? E.m[index] : 0;
      };

      virtual double complex CScale(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? E.cm[index] : 0;
      };

      virtual double Offset(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? E.b[index] : 0;
      };

      virtual double complex COffset(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? E.cb[index] : 0;
      };

      int SetNFields(int nfields);
      int SetInput(const char* field, int index = 0);
      int SetScale(double scale, int index = 0);
      int SetOffset(double offset, int index = 0);

    private:
      LincomEntry(GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
