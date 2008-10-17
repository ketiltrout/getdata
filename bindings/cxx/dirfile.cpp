// (C) 2008 D. V. Wiebe
//
//#########################################################################
//
// This file is part of the GetData project.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// GetData is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GetData; if not, write to the Free Software Foundation,
// Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
#include "dirfile.h"

using namespace GetData;

Dirfile::Dirfile(const char* filedir, unsigned int flags)
{
  D = dirfile_open(filedir, flags);
  error_string = NULL;
}

Dirfile::~Dirfile()
{
  if (error_string != NULL)
    delete error_string;
  dirfile_close(D);
}

int Dirfile::Add(const GetData::Entry &entry)
{
  return dirfile_add(D, &entry.E);
}

int Dirfile::AddSpec(const char *spec, int format_file)
{
  return dirfile_add_spec(D, spec, format_file);
}

int Dirfile::MAdd(const GetData::Entry &entry, const char *parent)
{
  return dirfile_madd(D, &entry.E, parent);
}

int Dirfile::MAddSpec(const char *spec, const char *parent)
{
  return dirfile_madd_spec(D, spec, parent);
}

Entry *Dirfile::Entry(const char* field_code)
{
  return new GetData::Entry(D, field_code);
}

int Dirfile::Flush(const char* field_code)
{
  return dirfile_flush(D, field_code);
}

int Dirfile::MetaFlush()
{
  return dirfile_metaflush(D);
}

int Dirfile::Error()
{
  return get_error(D);
}

const char* Dirfile::ErrorString(size_t len)
{
  if (error_string == NULL)
    error_string = new char[4096];

  if (len > 4096)
    len = 4096;

  return get_error_string(D, error_string, len);
}

int Dirfile::Include(const char* file, int format_file, unsigned int flags)
{
  return dirfile_include(D, file, format_file, flags);
}

unsigned int Dirfile::SamplesPerFrame(const char* field_code)
{
  return get_spf(D, field_code);
}

unsigned int Dirfile::NFields()
{
  return get_nfields(D);
}

unsigned int Dirfile::NFieldsByType(EntryType type)
{
  return get_nfields_by_type(D, (gd_entype_t)type);
}

const char** Dirfile::FieldListByType(EntryType type)
{
  return get_field_list_by_type(D, (gd_entype_t)type);
}

unsigned int Dirfile::NMFields(const char *parent)
{
  return get_nmfields(D, parent);
}

unsigned int Dirfile::NMFieldsByType(const char *parent, EntryType type)
{
  return get_nmfields_by_type(D, parent, (gd_entype_t)type);
}

const char** Dirfile::MFieldListByType(const char *parent, EntryType type)
{
  return get_mfield_list_by_type(D, parent, (gd_entype_t)type);
}

const void *Dirfile::Constants(DataType type)
{
  return get_constants(D, (gd_type_t)type);
}

const char **Dirfile::Strings()
{
  return get_strings(D);
}

const void *Dirfile::MConstants(const char *parent, DataType type)
{
  return get_mconstants(D, parent, (gd_type_t)type);
}

const char **Dirfile::MStrings(const char *parent)
{
  return get_mstrings(D, parent);
}

const char** Dirfile::FieldList()
{
  return get_field_list(D);
}

const char** Dirfile::MFieldList(const char *parent)
{
  return get_mfield_list(D, parent);
}

unsigned int Dirfile::NVectors()
{
  return get_nvectors(D);
}

const char** Dirfile::VectorList()
{
  return get_vector_list(D);
}

unsigned int Dirfile::NMVectors(const char *parent)
{
  return get_nmvectors(D, parent);
}

const char** Dirfile::MVectorList(const char *parent)
{
  return get_mvector_list(D, parent);
}

off_t Dirfile::NFrames()
{
  return get_nframes(D);
}

size_t Dirfile::GetConstant(const char *field_code, DataType type,
    void *data_out)
{
  return get_constant(D, field_code, (gd_type_t)type, data_out);
}

size_t Dirfile::GetData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, void* data_out)
{
  return getdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_out);
}

size_t Dirfile::GetString(const char *field_code, size_t len, char* data_out)
{
  return get_string(D, field_code, len, data_out);
}

size_t Dirfile::PutConstant(const char *field_code, DataType type,
    const void *data_in)
{
  return put_constant(D, field_code, (gd_type_t)type, data_in);
}

size_t Dirfile::PutData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, const void* data_in)
{
  return putdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_in);
}

size_t Dirfile::PutString(const char *field_code, const char* data_in)
{
  return put_string(D, field_code, data_in);
}

const char* Dirfile::FragmentName(int index)
{
  return get_fragmentname(D, index);
}

int Dirfile::NFragments()
{
  return get_nfragments(D);
}
