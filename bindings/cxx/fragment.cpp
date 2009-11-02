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

#define __USE_LARGEFILE64

#include "getdata/dirfile.h"

#include <stdlib.h>
#include <cstring>

using namespace GetData;

Fragment::Fragment(GetData::Dirfile *dirfile, int index)
{
  D = dirfile;

  ind = index;
  enc = (GetData::EncodingScheme)get_encoding(D->D, index);
  end = get_endianness(D->D, index);
  off = get_frameoffset64(D->D, index);
  prot = get_protection(D->D, index);
  name = strdup(get_fragmentname(D->D, index));
  parent = (index == 0) ? -1 : get_parent_fragment(D->D, index);
}

Fragment::~Fragment()
{
  free(name);
}

int Fragment::SetEncoding(GetData::EncodingScheme encoding, int recode)
{
  int ret = dirfile_alter_encoding(D->D, (unsigned long)encoding, ind, recode);

  if (!ret)
    enc = encoding;

  return ret;
}

int Fragment::SetEndianness(unsigned long byte_sex, int recode)
{
  int ret = dirfile_alter_endianness(D->D, byte_sex, ind, recode);

  if (!ret)
    end = byte_sex;

  return ret;
}

int Fragment::SetFrameOffset(off_t offset, int recode)
{
  int ret = dirfile_alter_frameoffset64(D->D, (off64_t)offset, ind, recode);

  if (!ret)
    off = offset;

  return ret;
}

int Fragment::SetProtection(int protection_level)
{
  int ret = dirfile_protect(D->D, protection_level, ind);

  if (!ret)
    prot = protection_level;

  return ret;
}
