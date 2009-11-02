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

#ifndef GETDATA_FRAGMENT_H
#define GETDATA_FRAGMENT_H

#define NO_GETDATA_LEGACY_API
#define GETDATA_C89_API

extern "C" {
#include <getdata.h>
}
#include <sys/types.h>
#include <unistd.h>

namespace GetData {

  enum EncodingScheme {
    AutoEncoding  = GD_AUTO_ENCODED, RawEncoding   = GD_UNENCODED,
    TextEncoding  = GD_TEXT_ENCODED, SlimEncoding  = GD_SLIM_ENCODED,
    GzipEncoding  = GD_GZIP_ENCODED, Bzip2Encoding = GD_BZIP2_ENCODED,
    UnsupportedEncoding = GD_ENC_UNSUPPORTED
  };

  class Dirfile;

  class Fragment {
    friend class Dirfile;

    public:
      virtual ~Fragment();

      EncodingScheme Encoding() { return enc; };

      unsigned long Endianness() { return end; };

      off_t FrameOffset() { return (off_t)off; };

      int Index() { return ind; }

      const char* Name() { return name; }

      int Parent() { return parent; }

      int Protection() { return prot; }

      int SetEncoding(EncodingScheme encoding, int recode = 0);

      int SetEndianness(unsigned long byte_sex, int recode = 0);

      int SetFrameOffset(off_t offset, int recode = 0);

      int SetProtection(int protection_level);

    protected:
      Fragment(Dirfile *dirfile, int index);

      Dirfile* D;
      EncodingScheme enc;
      unsigned long end;
      int ind;
      int prot;
      __off64_t off;
      char* name;
      int parent;
  };
}

#endif
