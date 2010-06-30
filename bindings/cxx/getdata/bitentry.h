// (C) 2008-2010 D. V. Wiebe
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

#ifndef GETDATA_BITENTRY_H
#define GETDATA_BITENTRY_H

#ifndef _FILE_OFFSET_BITS
# define _FILE_OFFSET_BITS 64
#endif

#define NO_GETDATA_LEGACY_API
#define GETDATA_C89_API

extern "C" {
#include <getdata.h>
}
#include <getdata/entry.h>

namespace GetData {

  class Dirfile;

  class BitEntry : public Entry {
    friend class Dirfile;

    public:
      BitEntry() : Entry::Entry() { E.field_type = GD_BIT_ENTRY; };

      BitEntry(const char* field_code, const char* in_field, gd_bit_t bitnum,
          gd_bit_t numbits = 1, int fragment_index = 0);

      virtual const char *Input(int __gd_unused index = 0) {
        return E.in_fields[0];
      };

      virtual gd_bit_t FirstBit() {
        return E.bitnum;
      };

      virtual gd_bit_t NumBits() {
        return E.numbits;
      };

      int SetInput(const char* field);
      int SetFirstBit(gd_bit_t first_bit);
      int SetFirstBit(const char* first_bit);
      int SetNumBits(gd_bit_t num_bits);
      int SetNumBits(const char* num_bits);

      virtual const char *Scalar(int index = 0);

    private:
      BitEntry(GetData::Dirfile *dirfile, const char* field_code) :
        Entry(dirfile, field_code) { };
  };
}

#endif
