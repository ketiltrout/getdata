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

#ifndef DIRFILE_H
#define DIRFILE_H

extern "C" {
#include <getdata.h>
}

class Dirfile {
  public:
    enum DataType {
      Null    = GD_NULL,    Unknown = GD_UNKNOWN,
      UInt8   = GD_UINT8,   Int8    = GD_INT8,
      UInt16  = GD_UINT16,  Int16   = GD_INT16,
      UInt32  = GD_UINT32,  Int32   = GD_INT32,
      UInt64  = GD_UINT64,  Int64   = GD_INT64,
      Float32 = GD_FLOAT32, Float64 = GD_FLOAT64
    };

    Dirfile(const char* dirfilename, unsigned int flags = GD_RDWR);
    ~Dirfile();

    int Error();
    void Flush(const char* field_code = NULL);
    const char* ErrorString(size_t len = 4096);
    unsigned int SamplesPerFrame(const char* field_code);
    off_t NFrames();
    unsigned int NFields();
    const char** FieldList();
    size_t GetData(const char* field_code, off_t first_frame,
        off_t first_sample, size_t num_frames, size_t num_samples,
        DataType type, void* data_out);
    size_t PutData(const char* field_code, off_t first_frame,
        off_t first_sample, size_t num_frames, size_t num_samples,
        DataType type, const void* data_in);

  private:
    DIRFILE* D; 
    char* error_string;
};

#endif
