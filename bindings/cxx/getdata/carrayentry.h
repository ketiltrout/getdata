// Copyright (C) 2010, 2011 D. V. Wiebe
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

#ifndef GETDATA_CARRAYENTRY_H
#define GETDATA_CARRAYENTRY_H

#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class CarrayEntry : public Entry {
    friend class Dirfile;

    public:
      CarrayEntry() : Entry() { E.field_type = GD_CARRAY_ENTRY; };

      CarrayEntry(const char* field_code, DataType type, size_t array_len,
          int fragment_index = 0);

      virtual DataType ConstType() const
      {
        return (DataType)E.u.scalar.const_type;
      }

      virtual size_t ArrayLen() const { return E.u.scalar.array_len; }

      int SetArrayLen(size_t array_len);

      int SetType(DataType type);

    private:
      CarrayEntry(const GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
