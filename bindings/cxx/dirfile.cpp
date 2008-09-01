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
// along with the GetData; if not, write to the Free Software Foundation,
// Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
//
#include "dirfile.h"

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

void Dirfile::Flush(const char* field_code)
{
  dirfile_flush(D, field_code);
}

int Dirfile::Error()
{
  return D->error;
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
    Dirfile::DataType type, void* data_out)
{
  return getdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_out);
}

size_t Dirfile::PutData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    Dirfile::DataType type, const void* data_in)
{
  return putdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_in);
}
