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

#ifndef GETDATA_RAWENTRY_H
#define GETDATA_RAWENTRY_H

#define NO_GETDATA_LEGACY_API

extern "C" {
#include <getdata.h>
}
#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class RawEntry : public Entry {
    friend class Dirfile;

    public:
      RawEntry(const char* field_code, DataType data_type, unsigned int spf,
          int fragment_index = 0);

      virtual unsigned int SamplesPerFrame() {
        return E.spf;
      };

      virtual DataType RawType() {
        return (DataType)E.data_type;
      };

      const char* FileName();
      int SetSamplesPerFrame(unsigned int spf, int recode);
      int SetType(DataType type, int recode);

    private:
      RawEntry(GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
