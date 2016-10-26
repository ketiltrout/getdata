// Copyright (C) 2012, 2013, 2015 D. V. Wiebe
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
          const char *check_field, int count_val, int period,
          int fragment_index = 0);

      virtual const char *Input(int index) const {
        return (index == 0 || index == 1) ? E.in_fields[index] : NULL;
      };

      virtual const char *Scalar(int index) const {
        return (index == 0 || index == 1) ? E.scalar[index] : NULL;
      };

      virtual int ScalarIndex(int index) const {
        return (index == 0 || index == 1) ? E.scalar_ind[index] : 0;
      };

      virtual int CountVal() const { return E.u.mplex.count_val; };

      virtual int Period() const { return E.u.mplex.period; };

      int SetInput(const char* field, int index);
      virtual int SetCountVal(int count_val);
      virtual int SetCountVal(const char* count_val);
      virtual int SetPeriod(int period);
      virtual int SetPeriod(const char* period);

      /* deprecated in GetData-0.8.4; use Period() or SetPeriod() instead */
      virtual int gd_deprecated CountMax() const { return Period(); };
      virtual int gd_deprecated SetCountMax(int period) {
        return SetPeriod(period);
      }
      virtual int gd_deprecated SetCountMax(const char* period) {
        return SetPeriod(period);
      };

    private:
      MplexEntry(const GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
