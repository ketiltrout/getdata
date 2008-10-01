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

Entry *Dirfile::Entry(const char* field_code)
{
  return new GetData::Entry(D, field_code);
}

void Dirfile::Flush(const char* field_code)
{
  dirfile_flush(D, field_code);
}

void Dirfile::FlushMetaData()
{
  dirfile_flush_metadata(D);
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

unsigned int Dirfile::SamplesPerFrame(const char* field_code)
{
  return get_spf(D, field_code);
}

unsigned int Dirfile::NFields()
{
  return get_nfields(D);
}

const char** Dirfile::FieldList()
{
  return get_field_list(D);
}

off_t Dirfile::NFrames()
{
  return get_nframes(D);
}

size_t Dirfile::GetData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, void* data_out)
{
  return getdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_out);
}

size_t Dirfile::PutData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, const void* data_in)
{
  return putdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_in);
}

const char* Dirfile::FormatFileName(int index)
{
  return get_format_filename(D, index);
}

int Dirfile::NFormats()
{
  return get_nformats(D);
}
