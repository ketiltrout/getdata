// Copyright (C) 2012 D. V. Wiebe
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

#ifndef GETDATA_MPLEXENTRY_H
#define GETDATA_MPLEXENTRY_H

#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class MplexEntry : public Entry {
    friend class Dirfile;

    public:
      MplexEntry() : Entry() { E.field_type = GD_MPLEX_ENTRY; };

      MplexEntry(const char* field_code, const char* in_field,
          const char *check_Field, int count_val, int count_max,
          int fragment_index = 0);

      virtual const char *Input(int index = 0) const {
        return E.in_fields[(index == 0) ? 0 : 1];
      };

      virtual const char *Scalar() const { return E.scalar[0]; }

      virtual int ScalarIndex() const { return E.scalar_ind[0]; };

      virtual int CountVal() const { return E.u.mplex.count_val; };

      virtual int CountMax() const { return E.u.mplex.count_max; };

      int SetInput(const char* field, int index);
      virtual int SetCountVal(int count_val);
      virtual int SetCountVal(const char* count_val);
      virtual int SetCountMax(int count_max);
      virtual int SetCountMax(const char* count_max);

    private:
      MplexEntry(const GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
