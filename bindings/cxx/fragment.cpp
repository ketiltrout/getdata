// Copyright (C) 2008-2012, 2014 D. V. Wiebe
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
#include "internal.h"

using namespace GetData;

Fragment::Fragment(const GetData::Dirfile *dirfile, int index)
{
  dtrace("%p, %i", dirfile, index);

  D = dirfile;

  ind = index;
  enc = (GetData::EncodingScheme)gd_encoding(D->D, index);
  end = gd_endianness(D->D, index);
  off = gd_frameoffset64(D->D, index);
  prot = gd_protection(D->D, index);
  name = gd_fragmentname(D->D, index);
  parent = (index == 0) ? -1 : gd_parent_fragment(D->D, index);

  if (gd_fragment_affixes(D->D, index, &prefix, &suffix) < 0)
    prefix = suffix = NULL;

  ns = gd_fragment_namespace(D->D, index, NULL);

  dreturnvoid();
}

Fragment::~Fragment()
{
  free(prefix);
  free(suffix);
}

int Fragment::ReWrite() const
{
  return gd_rewrite_fragment(D->D, ind);
}

int Fragment::SetEncoding(GetData::EncodingScheme encoding, int recode)
{
  int ret = gd_alter_encoding(D->D, (unsigned long)encoding, ind, recode);

  if (!ret)
    enc = encoding;

  return ret;
}

int Fragment::SetEndianness(unsigned long byte_sex, int recode)
{
  int ret = gd_alter_endianness(D->D, byte_sex, ind, recode);

  if (!ret)
    end = byte_sex;

  return ret;
}

int Fragment::SetFrameOffset(gd_off64_t offset, int recode)
{
  int ret = gd_alter_frameoffset64(D->D, offset, ind, recode);

  if (!ret)
    off = offset;

  return ret;
}

int Fragment::SetProtection(int protection_level)
{
  int ret = gd_alter_protection(D->D, protection_level, ind);

  if (!ret)
    prot = protection_level;

  return ret;
}

int Fragment::SetNamespace(const char* new_namespace)
{
  const char *ret = gd_fragment_namespace(D->D, ind, new_namespace);

  if (ret)
    ns = ret;
  return gd_error(D->D);
}

int Fragment::SetPrefix(const char* new_prefix)
{
  int ret = gd_alter_affixes(D->D, ind, new_prefix, suffix);

  free(prefix);
  free(suffix);
  if (!ret) {
    ns = gd_fragment_namespace(D->D, ind, NULL);
    ret = gd_fragment_affixes(D->D, ind, &prefix, &suffix);
  }
  return ret;
}

int Fragment::SetSuffix(const char* new_suffix)
{
  int ret = gd_alter_affixes(D->D, ind, prefix, new_suffix);

  free(prefix);
  free(suffix);
  if (!ret)
    ret = gd_fragment_affixes(D->D, ind, &prefix, &suffix);
  return ret;
}
