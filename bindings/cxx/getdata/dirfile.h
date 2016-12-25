// Copyright (C) 2008-2012, 2014, 2016 D. V. Wiebe
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

#include <getdata/types.h>
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
#include <getdata/carrayentry.h>
#include <getdata/stringentry.h>
#include <getdata/sarrayentry.h>
#include <getdata/mplexentry.h>
#include <getdata/multiplyentry.h>
#include <getdata/divideentry.h>
#include <getdata/recipentry.h>
#include <getdata/windowentry.h>
#include <getdata/indirentry.h>
#include <getdata/sindirentry.h>

namespace GetData {
  
  class Entry;
  class RawEntry;

  int EncodingSupport(EncodingScheme encoding);

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
    friend class WindowEntry;
    friend class MplexEntry;
    friend class IndirEntry;
    friend class SindirEntry;
    friend class ConstEntry;
    friend class CarrayEntry;
    friend class SarrayEntry;
    friend class StringEntry;
    friend class IndexEntry;
    friend class Fragment;

    public:
      Dirfile();

      Dirfile(const char *dirfilename, unsigned long flags = GD_RDONLY,
          gd_parser_callback_t sehandler = NULL, void* extra = NULL);

      Dirfile(DIRFILE *D);

      ~Dirfile();

      int Add(GetData::Entry &entry) const;

      int AddAlias(const char *field_code, const char *target,
          int fragment_index = 0) const;

      int AddSpec(const char *spec, int fragment_index = 0) const;

      const char** Aliases(const char* field_code) const;

      const char* AliasTarget(const char* field_code) const;

      int AlterSpec(const char* spec, int recode = 0) const;

      size_t ArrayLen(const char *field_code) const;

      gd_off64_t BoF(const char *field_code) const;

      size_t CarrayLen(const char *field_code) const gd_deprecated;

      const gd_carray_t *Carrays(DataType type = Float64) const;

      int Close();

      const void *Constants(DataType type = Float64) const;

      int Delete(const char* field_code, unsigned flags = 0) const;

      int DeSync(unsigned int flags = 0);

      int Discard();

      GetData::Entry* Entry(const char *field_code) const;

      const char **EntryList(const char *parent = NULL, int type = 0,
          unsigned int flags = 0) const;

      gd_off64_t EoF(const char *field_code) const;

      int Error() const;

      int ErrorCount() const;

      const char *ErrorString();
      const char *ErrorString(size_t n) gd_deprecated;

      unsigned long Flags(unsigned long set = 0, unsigned long reset = 0);

      const char **FieldList() const;

      const char **FieldListByType(EntryType type) const;

      int Flush(const char *field_code = NULL) const;

      GetData::Fragment *Fragment(int index) const;

      int FragmentIndex(const char* field_code) const;

      double FrameNum(const char* field_code, double value,
          gd_off64_t frame_start = 0, gd_off64_t frame_end = 0) const;

      int GetCarray(const char *field_code, DataType type, void *data_out,
          unsigned int start = 0, size_t len = 0) const;

      int GetConstant(const char *field_code, DataType type, void *data_out)
        const;

      size_t GetData(const char *field_code, gd_off64_t first_frame,
          gd_off64_t first_sample, size_t num_frames, size_t num_samples,
          DataType type, void* data_out) const;

			size_t GetData(const char *field_code, gd_off64_t first_frame,
          gd_off64_t first_sample, size_t num_frames, size_t num_samples,
          const char** data_out) const;

      int GetSarray(const char *field_code, const char**data_out,
          unsigned int start = 0, size_t len = 0) const;

      size_t GetString(const char *field_code, size_t len, char *data_out)
        const;

      int Hide(const char* field_code) const;

      int Hidden(const char* field_code) const;

      int Include(const char *file, int fragment_index = 0,
          unsigned long flags = 0) const;

      int IncludeAffix(const char *file, int fragment_index = 0,
          const char* prefix = NULL, const char* suffix = NULL,
          unsigned long flags = 0) const;

      int IncludeNS(const char *file, int fragment_index = 0,
          const char* ns = NULL, unsigned long flags = 0) const;

      char *LinterpTableName(const char *field_code);

      int MAdd(GetData::Entry &entry, const char *parent) const;

      int MAddAlias(const char* parent, const char* name, const char* target)
        const;

      int MAddSpec(const char *spec, const char *parent) const;

      int MAlterSpec(const char *line, const char *parent, int recode = 0)
        const;

      unsigned int MatchEntries(const char *regex = NULL,
          int fragment = GD_ALL_FRAGMENTS, int type = 0, unsigned int flags = 0,
          const char ***entries = NULL);

      const gd_carray_t *MCarrays(const char *parent, DataType type = Float64)
        const;

      const void *MConstants(const char *parent, DataType type = Float64) const;

      int MetaFlush() const;

      const char **MFieldList(const char *parent) const;

      const char **MFieldListByType(const char *parent, EntryType type) const;

      void MplexLookback(int lookback) const;

			const char*** MSarrays(const char *parent) const;

      const char **MStrings(const char *parent) const;

      const char **MVectorList(const char *parent) const;

      int NAliases(const char* field_code) const;

      unsigned int NEntries(const char *parent = NULL, int type = 0,
          unsigned int flags = 0) const;

      unsigned int NFields() const;

      unsigned int NFieldsByType(EntryType type) const;

      unsigned int NMFields(const char *parent) const;

      unsigned int NMFieldsByType(const char *parent, EntryType type) const;

      unsigned int NMVectors(const char *parent) const;

      const char* Name() const;

      DataType NativeType(const char* field_code) const;

      int NFragments() const;

      gd_off64_t NFrames() const;

      unsigned int NVectors() const;

      int PutCarray(const char *field_code, DataType type, const void *data_in,
          unsigned int start = 0, size_t len = 0) const;

      int PutConstant(const char *field_code, DataType type,
          const void *data_in) const;

      size_t PutData(const char *field_code, gd_off64_t first_frame,
          gd_off64_t first_sample, size_t num_frames, size_t num_samples,
          DataType type, const void* data_in) const;


			int PutSarray(const char *field_code, const char **data_in,
          unsigned int start = 0, size_t len = 0) const;

      int PutString(const char *field_code, const char *data_in) const;

      int RawClose(const char *field_code = NULL) const;

      GetData::RawEntry *Reference(const char* field_code = NULL) const;

      const char *ReferenceFilename();

      unsigned int SamplesPerFrame(const char *field_code) const;

			const char*** Sarrays() const;

      gd_off64_t Seek(const char* field_code, gd_off64_t frame_num,
          gd_off64_t sample_num, int flags) const;

      void SetCallback(gd_parser_callback_t sehandler, void* extra = NULL)
        const;

      int Standards(int version = GD_VERSION_CURRENT) const;

      const char **Strings() const;

      int Sync(const char *field_code = NULL) const;

      gd_off64_t Tell(const char* field_code) const;

      char *StrTok(const char *string = NULL);

      int UnHide(const char* field_code) const;

      int UnInclude(int fragment_index, int del = 0) const;

      int Validate(const char* field_code) const;

      int VerbosePrefix(const char *prefix = NULL) const;

      const char **VectorList() const;

    private:
      DIRFILE* D; 

      char *error_string;
      char *reference_name;
  };
}

#endif
