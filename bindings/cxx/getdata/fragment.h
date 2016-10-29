// Copyright (C) 2008-2012, 2016 D. V. Wiebe
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

#include <getdata/types.h>

namespace GetData {

  class Dirfile;

  class Fragment {
    friend class Dirfile;

    public:
      virtual ~Fragment();

      EncodingScheme Encoding() const { return enc; };

      unsigned long Endianness() const { return end; };

      gd_off64_t FrameOffset() const { return off; };

      int Index() const { return ind; }

      const char* Name() const { return name; }

      const char* Namespace() const { return ns; }

      int Parent() const { return parent; }

      const char *Prefix() { return prefix; };

      int Protection() const { return prot; }

      int ReWrite() const;

      const char *Suffix() { return suffix; };

      int SetEncoding(EncodingScheme encoding, int recode = 0);

      int SetEndianness(unsigned long byte_sex, int recode = 0);

      int SetFrameOffset(gd_off64_t offset, int recode = 0);

      int SetNamespace(const char* new_namespace);

      int SetPrefix(const char *preix);

      int SetProtection(int protection_level);

      int SetSuffix(const char *suffix);

    protected:
      Fragment(const Dirfile *dirfile, int index);

      const Dirfile *D;
      EncodingScheme enc;
      unsigned long end;
      int ind;
      int prot;
      gd_off64_t off;
      const char* name;
      const char* ns;
      char* prefix;
      char* suffix;
      int parent;
  };
}

#endif
