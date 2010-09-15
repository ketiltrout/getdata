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

#ifndef GETDATA_DIRFILE_H
#define GETDATA_DIRFILE_H

// We don't want the legacy API since its symbols clash with us.
#define GD_NO_LEGACY_API

// We use the C89 API since C++ compilers aren't required to support the
// C99 _Complex keyword
#define GD_C89_API

#include <getdata.h>

#include <getdata/fragment.h>
#include <getdata/entry.h>
#include <getdata/rawentry.h>
#include <getdata/lincomentry.h>
#include <getdata/linterpentry.h>
#include <getdata/bitentry.h>
#include <getdata/sbitentry.h>
#include <getdata/phaseentry.h>
#include <getdata/indexentry.h>
#include <getdata/polynomentry.h>
#include <getdata/constentry.h>
#include <getdata/stringentry.h>
#include <getdata/multiplyentry.h>
#include <getdata/divideentry.h>
#include <getdata/recipentry.h>

namespace GetData {
  
  class Entry;
  class RawEntry;

  class Dirfile {
    friend class Entry;
    friend class RawEntry;
    friend class LincomEntry;
    friend class LinterpEntry;
    friend class BitEntry;
    friend class SBitEntry;
    friend class MultiplyEntry;
    friend class DivideEntry;
    friend class RecipEntry;
    friend class PhaseEntry;
    friend class PolynomEntry;
    friend class ConstEntry;
    friend class StringEntry;
    friend class IndexEntry;
    friend class Fragment;

    public:
      Dirfile();

      Dirfile(const char *dirfilename, unsigned long flags = GD_RDWR,
          gd_parser_callback_t sehandler = NULL, void* extra = NULL);

      Dirfile(DIRFILE *D);

      ~Dirfile();

      int Add(GetData::Entry &entry);

      int AddSpec(const char *spec, int fragment_index = 0);

      int AlterSpec(const char* spec, int recode = 0);

      off_t BoF(const char *field_code);

      int Close();

      const void *Constants(DataType type = Float64);

      int Delete(const char* field_code, int flags = 0);

      int Discard();

      GetData::Entry* Entry(const char *field_code);

      off_t EoF(const char *field_code);

      int Error();

      const char *ErrorString(size_t len = 4096);

      const char **FieldList();

      const char **FieldListByType(EntryType type);

      int Flush(const char *field_code = NULL);

      GetData::Fragment *Fragment(int index);

      int FragmentIndex(const char* field_code);

      double FrameNum(const char* field_code, double value,
          off_t frame_start = 0, off_t frame_end = 0);

      size_t GetConstant(const char *field_code, DataType type, void *data_out);

      size_t GetData(const char *field_code, off_t first_frame,
          off_t first_sample, size_t num_frames, size_t num_samples,
          DataType type, void* data_out);

      size_t GetString(const char *field_code, size_t len, char *data_out);

      int Include(const char *file, int fragment_index = 0,
          unsigned long flags = 0);

      int MAdd(GetData::Entry &entry, const char *parent);

      int MAddSpec(const char *spec, const char *parent);

      int MAlterSpec(const char *line, const char *parent, int recode = 0);

      const void *MConstants(const char *parent, DataType type = Float64);

      int MetaFlush();

      const char **MFieldList(const char *parent);

      const char **MFieldListByType(const char *parent, EntryType type);

      const char **MStrings(const char *parent);

      const char **MVectorList(const char *parent);

      unsigned int NFields();

      unsigned int NFieldsByType(EntryType type);

      unsigned int NMFields(const char *parent);

      unsigned int NMFieldsByType(const char *parent, EntryType type);

      unsigned int NMVectors(const char *parent);

      const char* Name();

      DataType NativeType(const char* field_code);

      int NFragments();

      off_t NFrames();

      unsigned int NVectors();

      size_t PutConstant(const char *field_code, DataType type,
          const void *data_in);

      size_t PutData(const char *field_code, off_t first_frame,
          off_t first_sample, size_t num_frames, size_t num_samples,
          DataType type, const void* data_in);

      size_t PutString(const char *field_code, const char *data_in);

      GetData::RawEntry *Reference(const char* field_code = NULL);

      const char *ReferenceFilename();

      unsigned int SamplesPerFrame(const char *field_code);

      void SetCallback(gd_parser_callback_t sehandler, void* extra = NULL);

      const char **Strings();

      int UnInclude(int fragment_index, int del = 0);

      int Validate(const char* field_code);

      const char **VectorList();

    private:
      DIRFILE* D; 

      char *error_string;
  };
}

#endif
