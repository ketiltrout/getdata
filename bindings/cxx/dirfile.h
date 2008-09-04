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

#ifndef GETDATA_DIRFILE_H
#define GETDATA_DIRFILE_H

// We don't want the legacy API since its symbols clash with us.
#define NO_GETDATA_LEGACY_API

extern "C" {
#include <getdata.h>
}
#include <entry.h>

namespace GetData {
  class Dirfile {
    public:
      Dirfile(const char* dirfilename, unsigned int flags = GD_RDWR);

      ~Dirfile();

      GetData::Entry* Entry(const char* field_code);

      int Error();

      const char* ErrorString(size_t len = 4096);

      const char** FieldList();

      void Flush(const char* field_code = NULL);

      size_t GetData(const char* field_code, off_t first_frame,
          off_t first_sample, size_t num_frames, size_t num_samples,
          DataType type, void* data_out);

      unsigned int NFields();

      off_t NFrames();

      size_t PutData(const char* field_code, off_t first_frame,
          off_t first_sample, size_t num_frames, size_t num_samples,
          DataType type, const void* data_in);

      unsigned int SamplesPerFrame(const char* field_code);

    private:
      DIRFILE* D; 

      char* error_string;
  };
};

#endif
