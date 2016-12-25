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

// This is not part of the Dirfile class, but it's convenient to put it here
int GetData::EncodingSupport(GetData::EncodingScheme encoding)
{
  return gd_encoding_support((unsigned long)encoding);
}

Dirfile::Dirfile()
{
  D = gd_invalid_dirfile();
  error_string = NULL;
  reference_name = NULL;
}

Dirfile::Dirfile(const char* filedir, unsigned long flags,
    gd_parser_callback_t sehandler, void* extra)
{
  D = gd_cbopen(filedir, flags, sehandler, extra);
  error_string = NULL;
  reference_name = NULL;
}

Dirfile::Dirfile(DIRFILE* dirfile)
{
  D = dirfile;
  error_string = NULL;
  reference_name = NULL;
}

Dirfile::~Dirfile()
{
  free(error_string);
  free(reference_name);

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
    case CarrayEntryType:
      return new GetData::CarrayEntry(this, field_code);
    case StringEntryType:
      return new GetData::StringEntry(this, field_code);
    case SarrayEntryType:
      return new GetData::SarrayEntry(this, field_code);
    case IndexEntryType:
      return new GetData::IndexEntry(this, field_code);
    case WindowEntryType:
      return new GetData::WindowEntry(this, field_code);
    case MplexEntryType:
      return new GetData::MplexEntry(this, field_code);
    case IndirEntryType:
      return new GetData::IndirEntry(this, field_code);
    case SindirEntryType:
      return new GetData::SindirEntry(this, field_code);
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

int Dirfile::ErrorCount() const
{
  return gd_error_count(D);
}

const char *Dirfile::ErrorString(size_t __gd_unused n)
{
  return ErrorString();
}

const char *Dirfile::ErrorString()
{
  if (error_string)
    free(error_string);
  error_string = gd_error_string(D, NULL, 0);
  
  return error_string;
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

const gd_carray_t *Dirfile::Carrays(DataType type) const
{
  return gd_carrays(D, (gd_type_t)type);
}

size_t Dirfile::ArrayLen(const char *field_code) const
{
  return gd_array_len(D, field_code);
}

size_t Dirfile::CarrayLen(const char *field_code) const
{
  return ArrayLen(field_code);
}

const void *Dirfile::Constants(DataType type) const
{
  return gd_constants(D, (gd_type_t)type);
}

const char **Dirfile::Strings() const
{
  return gd_strings(D);
}

const gd_carray_t *Dirfile::MCarrays(const char *parent, DataType type) const
{
  return gd_mcarrays(D, parent, (gd_type_t)type);
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

gd_off64_t Dirfile::NFrames() const
{
  return gd_nframes64(D);
}

gd_off64_t Dirfile::EoF(const char *field_code) const
{
  return gd_eof64(D, field_code);
}

gd_off64_t Dirfile::BoF(const char *field_code) const
{
  return gd_bof64(D, field_code);
}

int Dirfile::GetCarray(const char *field_code, DataType type, void *data_out,
    unsigned int start, size_t len) const
{
  if (len == 0)
    return gd_get_carray(D, field_code, (gd_type_t)type, data_out);
  else
    return gd_get_carray_slice(D, field_code, start, len, (gd_type_t)type,
        data_out);
}

int Dirfile::GetConstant(const char *field_code, DataType type, void *data_out)
  const
{
  return gd_get_constant(D, field_code, (gd_type_t)type, data_out);
}

size_t Dirfile::GetData(const char* field_code, gd_off64_t first_frame,
    gd_off64_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, void* data_out) const
{
  return gd_getdata64(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_out);
}

size_t Dirfile::GetData(const char* field_code, gd_off64_t first_frame,
    gd_off64_t first_sample, size_t num_frames, size_t num_samples,
    const char** data_out) const
{
  return gd_getdata64(D, field_code, first_frame, first_sample, num_frames,
      num_samples, GD_STRING, data_out);
}

size_t Dirfile::GetString(const char *field_code, size_t len, char* data_out)
  const
{
  return gd_get_string(D, field_code, len, data_out);
}

int Dirfile::PutCarray(const char *field_code, DataType type,
    const void *data_in, unsigned int start, size_t len) const
{
  if (len == 0)
    return gd_put_carray(D, field_code, (gd_type_t)type, data_in);
  else
    return gd_put_carray_slice(D, field_code, start, len, (gd_type_t)type,
        data_in);
}

int Dirfile::PutConstant(const char *field_code, DataType type,
    const void *data_in) const
{
  return gd_put_constant(D, field_code, (gd_type_t)type, data_in);
}

size_t Dirfile::PutData(const char* field_code, gd_off64_t first_frame,
    gd_off64_t first_sample, size_t num_frames, size_t num_samples,
    DataType type, const void* data_in) const
{
  return gd_putdata64(D, field_code, first_frame, first_sample, num_frames,
      num_samples, (gd_type_t)type, data_in);
}

int Dirfile::PutString(const char *field_code, const char* data_in) const
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

const char* Dirfile::ReferenceFilename()
{
  const char* ref = gd_reference(D, NULL);

  if (ref == NULL)
    return NULL;

  free(reference_name);
  reference_name = gd_raw_filename(D, ref);
  return reference_name;
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

int Dirfile::Delete(const char* field_code, unsigned flags) const
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
    gd_off64_t frame_start, gd_off64_t frame_end) const
{
  return gd_framenum_subset64(D, field_code, value, frame_start, frame_end);
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

gd_off64_t Dirfile::Seek(const char* field_code, gd_off64_t frame_num,
    gd_off64_t sample_num, int flags) const
{
  return gd_seek64(D, field_code, frame_num, sample_num, flags);
}

gd_off64_t Dirfile::Tell(const char* field_code) const
{
  return gd_tell64(D, field_code);
}

int Dirfile::AddAlias(const char* field_code, const char* target,
    int fragment_index) const
{
  return gd_add_alias(D, field_code, target, fragment_index);
}

const char** Dirfile::Aliases(const char* field_code) const
{
  return gd_aliases(D, field_code);
}

const char* Dirfile::AliasTarget(const char* field_code) const
{
  return gd_alias_target(D, field_code);
}

int Dirfile::Hide(const char* field_code) const
{
  return gd_hide(D, field_code);
}

int Dirfile::Hidden(const char* field_code) const
{
  return gd_hidden(D, field_code);
}

int Dirfile::IncludeAffix(const char *file, int fragment_index,
    const char* prefix, const char* suffix, unsigned long flags) const
{
  return gd_include_affix(D, file, fragment_index, prefix, suffix, flags);
}

int Dirfile::IncludeNS(const char *file, int fragment_index,
    const char* ns, unsigned long flags) const
{
  return gd_include_ns(D, file, fragment_index, ns, flags);
}

int Dirfile::MAddAlias(const char* parent, const char* name, const char* target)
  const
{
  return gd_madd_alias(D, parent, name, target);
}

int Dirfile::NAliases(const char* field_code) const
{
  return gd_naliases(D, field_code);
}

int Dirfile::Sync(const char* field_code) const
{
  return gd_sync(D, field_code);
}

int Dirfile::RawClose(const char* field_code) const
{
  return gd_raw_close(D, field_code);
}

int Dirfile::UnHide(const char* field_code) const
{
  return gd_unhide(D, field_code);
}

char *Dirfile::StrTok(const char *string)
{
  return gd_strtok(D, string);
}

int Dirfile::DeSync(unsigned int flags)
{
  int desync = gd_desync(D, flags);
  if (desync && flags & GD_DESYNC_REOPEN) {
    free(error_string);
    free(reference_name);

    error_string = NULL;
    reference_name = NULL;
  }

  return desync;
}

unsigned long Dirfile::Flags(unsigned long set, unsigned long reset)
{
  return gd_flags(D, set, reset);
}

int Dirfile::VerbosePrefix(const char *prefix) const
{
  return gd_verbose_prefix(D, prefix);
}

void Dirfile::MplexLookback(int lookback) const
{
  gd_mplex_lookback(D, lookback);
}

unsigned int Dirfile::NEntries(const char *parent, int type, unsigned int flags)
  const
{
  return gd_nentries(D, parent, type, flags);
}

const char** Dirfile::EntryList(const char *parent, int type,
    unsigned int flags) const
{
  return gd_entry_list(D, parent, type, flags);
}

char* Dirfile::LinterpTableName(const char *field_code)
{
  return gd_linterp_tablename(D, field_code);
}

int Dirfile::GetSarray(const char *field_code, const char **data_out,
    unsigned int start, size_t len) const
{
  if (len == 0)
    return gd_get_sarray(D, field_code, data_out);
  else
    return gd_get_sarray_slice(D, field_code, start, len, data_out);
}

const char*** Dirfile::Sarrays() const
{
  return gd_sarrays(D);
}

const char*** Dirfile::MSarrays(const char* parent) const
{
  return gd_msarrays(D, parent);
}

int Dirfile::PutSarray(const char *field_code, const char **data_in,
    unsigned int start, size_t len) const
{
  if (len == 0)
    return gd_put_sarray(D, field_code, data_in);
  else
    return gd_put_sarray_slice(D, field_code, start, len, data_in);
}

unsigned int Dirfile::MatchEntries(const char *regex, int fragment, int type,
    unsigned int flags, const char ***entries)
{
  return gd_match_entries(D, regex, fragment, type, flags, entries);
}
