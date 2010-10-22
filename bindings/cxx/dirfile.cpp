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
#ifdef HAVE_CONFIG_H
#include "../../src/config.h"
#endif
#undef GETDATA_LEGACY_API
#include "getdata/dirfile.h"

using namespace GetData;

Dirfile::Dirfile()
{
  D = gd_invalid_dirfile();
  error_string = NULL;
}

Dirfile::Dirfile(const char* filedir, unsigned long flags,
    gd_parser_callback_t sehandler, void* extra)
{
  D = gd_cbopen(filedir, flags, sehandler, extra);
  error_string = NULL;
}

Dirfile::Dirfile(DIRFILE* dirfile)
{
  D = dirfile;
  error_string = NULL;
}

Dirfile::~Dirfile()
{
  if (error_string != NULL)
    delete error_string;

  gd_close(D);
}

int Dirfile::Add(GetData::Entry &entry) const
{
  int ret = gd_add(D, &entry.E);
  entry.SetDirfile(this);
  return ret;
}

int Dirfile::AddSpec(const char *spec, int format_file) const
{
  return gd_add_spec(D, spec, format_file);
}

int Dirfile::MAdd(GetData::Entry &entry, const char *parent) const
{
  int ret = gd_madd(D, &entry.E, parent);
  entry.SetDirfile(this);
  return ret;
}

int Dirfile::MAddSpec(const char *spec, const char *parent) const
{
  return gd_madd_spec(D, spec, parent);
}

Entry *Dirfile::Entry(const char* field_code) const
{
  GetData::EntryType type = (GetData::EntryType)gd_entry_type(D, field_code);

  switch(type) {
    case RawEntryType:
      return new GetData::RawEntry(this, field_code);
    case LincomEntryType:
      return new GetData::LincomEntry(this, field_code);
    case LinterpEntryType:
      return new GetData::LinterpEntry(this, field_code);
    case BitEntryType:
      return new GetData::BitEntry(this, field_code);
    case SBitEntryType:
      return new GetData::SBitEntry(this, field_code);
    case MultiplyEntryType:
      return new GetData::MultiplyEntry(this, field_code);
    case DivideEntryType:
      return new GetData::DivideEntry(this, field_code);
    case RecipEntryType:
      return new GetData::RecipEntry(this, field_code);
    case PhaseEntryType:
      return new GetData::PhaseEntry(this, field_code);
    case PolynomEntryType:
      return new GetData::PolynomEntry(this, field_code);
    case ConstEntryType:
      return new GetData::ConstEntry(this, field_code);
    case StringEntryType:
      return new GetData::StringEntry(this, field_code);
    case IndexEntryType:
      return new GetData::IndexEntry(this, field_code);
    case NoEntryType:
      break;
  }

  return NULL;
}

int Dirfile::Flush(const char* field_code) const
{
  return gd_flush(D, field_code);
}

int Dirfile::MetaFlush() const
{
  return gd_metaflush(D);
}

int Dirfile::Error() const
{
  return gd_error(D);
}

const char* Dirfile::ErrorString(size_t len)
{
  if (error_string == NULL)
    error_string = new char[4096];

  if (len > 4096)
    len = 4096;

  return gd_error_string(D, error_string, len);
}

int Dirfile::Include(const char* file, int format_file, unsigned long flags)
  const
{
  return gd_include(D, file, format_file, flags);
}

unsigned int Dirfile::SamplesPerFrame(const char* field_code) const
{
  return gd_spf(D, field_code);
}

unsigned int Dirfile::NFields() const
{
  return gd_nfields(D);
}

unsigned int Dirfile::NFieldsByType(EntryType type) const
{
  return gd_nfields_by_type(D, (gd_entype_t)type);
}

const char** Dirfile::FieldListByType(EntryType type) const
{
  return gd_field_list_by_type(D, (gd_entype_t)type);
}

unsigned int Dirfile::NMFields(const char *parent) const
{
  return gd_nmfields(D, parent);
}

unsigned int Dirfile::NMFieldsByType(const char *parent, EntryType type) const
{
  return gd_nmfields_by_type(D, parent, (gd_entype_t)type);
}

const char** Dirfile::MFieldListByType(const char *parent, EntryType type) const
{
  return gd_mfield_list_by_type(D, parent, (gd_entype_t)type);
}

const void *Dirfile::Constants(DataType type) const
{
  return gd_constants(D, (gd_type_t)type);
}

const char **Dirfile::Strings() const
{
  return gd_strings(D);
}

const void *Dirfile::MConstants(const char *parent, DataType type) const
{
  return gd_mconstants(D, parent, (gd_type_t)type);
}

const char **Dirfile::MStrings(const char *parent) const
{
  return gd_mstrings(D, parent);
}

const char** Dirfile::FieldList() const
{
  return gd_field_list(D);
}

const char** Dirfile::MFieldList(const char *parent) const
{
  return gd_mfield_list(D, parent);
}

unsigned int Dirfile::NVectors() const
{
  return gd_nvectors(D);
}

const char** Dirfile::VectorList() const
{
  return gd_vector_list(D);
}

unsigned int Dirfile::NMVectors(const char *parent) const
{
  return gd_nmvectors(D, parent);
}

const char** Dirfile::MVectorList(const char *parent) const
{
  return gd_mvector_list(D, parent);
}

off_t Dirfile::NFrames() const
{
  return gd_nframes(D);
}

off_t Dirfile::EoF(const char *field_code) const
{
  return gd_eof(D, field_code);
}

off_t Dirfile::BoF(const char *field_code) const
{
  return gd_bof(D, field_code);
}

size_t Dirfile::GetConstant(const char *field_code, DataType type,
    void *data_out) const
{
  return gd_get_constant(D, field_code, (gd_type_t)type, data_out);
}

size_t Dirfile::GetData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, void* data_out) const
{
  return gd_getdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_out);
}

size_t Dirfile::GetString(const char *field_code, size_t len, char* data_out)
  const
{
  return gd_get_string(D, field_code, len, data_out);
}

size_t Dirfile::PutConstant(const char *field_code, DataType type,
    const void *data_in) const
{
  return gd_put_constant(D, field_code, (gd_type_t)type, data_in);
}

size_t Dirfile::PutData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, const void* data_in) const
{
  return gd_putdata(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_in);
}

size_t Dirfile::PutString(const char *field_code, const char* data_in) const
{
  return gd_put_string(D, field_code, data_in);
}

GetData::Fragment* Dirfile::Fragment(int index) const
{
  if (index < 0 || index >= gd_nfragments(D))
    return NULL;

  return new GetData::Fragment(this, index);
}

int Dirfile::NFragments() const
{
  return gd_nfragments(D);
}

const char* Dirfile::ReferenceFilename() const
{
  const char* ref = gd_reference(D, NULL);

  if (ref == NULL)
    return NULL;

  return gd_raw_filename(D, ref);
}

int Dirfile::Discard()
{
  int ret = gd_discard(D);

  if (!ret)
    D = gd_invalid_dirfile();

  return ret;
}

int Dirfile::Close()
{
  int ret = gd_close(D);

  if (!ret)
    D = gd_invalid_dirfile();

  return ret;
}

void Dirfile::SetCallback(gd_parser_callback_t sehandler, void* extra) const
{
  gd_parser_callback(D, sehandler, extra);
}

RawEntry* Dirfile::Reference(const char* field_code) const
{
  const char* ref = gd_reference(D, field_code);

  if (ref == NULL)
    return NULL;

  return new RawEntry(this, ref);
}

int Dirfile::AlterSpec(const char *line, int recode) const
{
  return gd_alter_spec(D, line, recode);
}

int Dirfile::MAlterSpec(const char* line, const char *parent, int recode) const
{
  return gd_malter_spec(D, line, parent, recode);
}

int Dirfile::Delete(const char* field_code, int flags) const
{
  return gd_delete(D, field_code, flags);
}

int Dirfile::UnInclude(int fragment_index, int del) const
{
  return gd_uninclude(D, fragment_index, del);
}

DataType Dirfile::NativeType(const char* field_code) const
{
  return (DataType)gd_native_type(D, field_code);
}

int Dirfile::Validate(const char* field_code) const
{
  return gd_validate(D, field_code);
}

double Dirfile::FrameNum(const char* field_code, double value,
    off_t frame_start, off_t frame_end) const
{
  return gd_framenum_subset(D, field_code, value, frame_start, frame_end);
}

int Dirfile::FragmentIndex(const char* field_code) const
{
  return gd_fragment_index(D, field_code);
}

const char* Dirfile::Name() const
{
  return gd_dirfilename(D);
}

int Dirfile::Standards(int version) const
{
  return gd_dirfile_standards(D, version);
}
