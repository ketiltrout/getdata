/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#endif

void _GD_Flush(DIRFILE* D, gd_entry_t *E, const char* field_code)
{
  int i;

  dtrace("%p, %p", D, E);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturnvoid();
    return;
  }

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    D->recurse_level--;
    dreturnvoid();
    return;
  }

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      if (E->e->file[0].fp >= 0) {
        if ((D->flags & GD_ACCMODE) == GD_RDWR &&
            ef[E->e->file[0].encoding].sync != NULL &&
            (*ef[E->e->file[0].encoding].sync)(E->e->file))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
        else if ((*ef[E->e->file[0].encoding].close)(E->e->file))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      }
      break;
    case GD_LINCOM_ENTRY:
      for (i = 2; i < GD_MAX_LINCOM; ++i)
        _GD_Flush(D, E->e->entry[i], field_code);
      /* fallthrough */
    case GD_MULTIPLY_ENTRY:
      _GD_Flush(D, E->e->entry[1], field_code);
      /* fallthrough */
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
      _GD_Flush(D, E->e->entry[0], field_code);
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  D->recurse_level--;
  dreturnvoid();
}

static const char* _GD_TypeName(DIRFILE* D, gd_type_t data_type)
{
  dtrace("%p, 0x%x", D, data_type);
  const char* ptr;

  switch(data_type) {
    case GD_UINT8:
      ptr = "UINT8";
      break;
    case GD_INT8:
      ptr = "INT8";
      break;
    case GD_UINT16:
      ptr = "UINT16";
      break;
    case GD_INT16:
      ptr = "INT16";
      break;
    case GD_UINT32:
      ptr = "UINT32";
      break;
    case GD_INT32:
      ptr = "INT32";
      break;
    case GD_UINT64:
      ptr = "UINT64";
      break;
    case GD_INT64:
      ptr = "INT64";
      break;
    case GD_FLOAT32:
      ptr = "FLOAT32";
      break;
    case GD_FLOAT64:
      ptr = "FLOAT64";
      break;
    default:
      _GD_InternalError(D);
      ptr = "";
      break;
  }

  dreturn("\"%s\"", ptr);
  return ptr;
}

static char* _GD_StringEscapeise(char* out, const char* in)
{
  char* ptr = out;
  const char* HexDigit = "0123456789ABCDEF";

  dtrace("%p, \"%s\"", out, in);

  for (; *in != '\0'; ++in)
    if (*in == '"') {
      *(ptr++) = '\\';
      *(ptr++) = '"';
    } else if (*in == '\\') {
      *(ptr++) = '\\';
      *(ptr++) = '\\';
    } else if (*in < 0x20) {
      *(ptr++) = '\\';
      *(ptr++) = 'x';
      *(ptr++) = HexDigit[*in >> 8];
      *(ptr++) = HexDigit[*in & 0xF];
    } else
      *(ptr++) = *in;
  *ptr = '\0';

  dreturn("\"%s\"", out);
  return out;
}

/* Write a field specification line */
static void _GD_FieldSpec(DIRFILE* D, FILE* stream, const gd_entry_t* E,
    int meta)
{
  int i;
  char buffer[GD_MAX_LINE_LENGTH];
  char* ptr;

  dtrace("%p, %p, %p, %i", D, stream, E, meta);
  
  ptr = (meta) ? strchr(E->field, '/') + 1 : E->field;

  if (meta) {
    strcpy(buffer, E->field);
    *strchr(buffer, '/') = '\0';
    fprintf(stream, "META %s ", buffer);
  }

  /* field name */
  if (E->field_type != GD_INDEX_ENTRY)
    fprintf(stream, "%s", _GD_StringEscapeise(buffer, ptr));

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      fprintf(stream, " RAW %s %u\n", _GD_TypeName(D, E->data_type), E->spf);
      break;
    case GD_LINCOM_ENTRY:
      fprintf(stream, " LINCOM %i", E->n_fields);
      for (i = 0; i < E->n_fields; ++i)
        fprintf(stream, " %s %.15g %.15g", E->in_fields[i], E->m[i], E->b[i]);
      fputs("\n", stream);
      break;
    case GD_LINTERP_ENTRY:
      fprintf(stream, " LINTERP %s %s\n", E->in_fields[0], E->table);
      break;
    case GD_BIT_ENTRY:
      fprintf(stream, " BIT %s %i %i\n", E->in_fields[0], E->bitnum,
          E->numbits);
      break;
    case GD_MULTIPLY_ENTRY:
      fprintf(stream, " MULTIPLY %s %s\n", E->in_fields[0], E->in_fields[1]);
      break;
    case GD_PHASE_ENTRY:
      fprintf(stream, " PHASE %s %i\n", E->in_fields[0], E->shift);
      break;
    case GD_CONST_ENTRY:
      if (E->const_type & GD_SIGNED)
        fprintf(stream, " CONST %s %" PRIi64, _GD_TypeName(D, E->const_type),
            E->e->iconst);
      else if (E->const_type & GD_IEEE754)
        fprintf(stream, " CONST %s %.16g", _GD_TypeName(D, E->const_type),
            E->e->dconst);
      else
        fprintf(stream, " CONST %s %" PRIu64, _GD_TypeName(D, E->const_type),
            E->e->uconst);
      break;
    case GD_STRING_ENTRY:
      fprintf(stream, " STRING \"%s\"", _GD_StringEscapeise(buffer,
            E->e->string));
    case GD_INDEX_ENTRY:
      /* INDEX is implicit, and it is an error to define it in the format
       * file */
      break;
    case GD_NO_ENTRY:
      _GD_InternalError(D);
      break;
  }

  dreturnvoid();
}

void _GD_FlushFragment(DIRFILE* D, int i)
{
  int j;
  FILE* stream;
  char buffer[GD_MAX_LINE_LENGTH];
  char temp_file[FILENAME_MAX];
  char* ptr;
  struct tm now;
  time_t t;
  int fd;
  unsigned int u;
  mode_t mode;
  struct stat stat_buf;

  dtrace("%p, %i", D, i);

  /* get the permissions of the old file */
  if (stat(D->fragment[i].cname, &stat_buf))
    mode = 0644;
  else
    mode = stat_buf.st_mode;

  /* open a temporary file */
  snprintf(temp_file, GD_MAX_LINE_LENGTH, "%s/format_XXXXXX", D->name);
  fd = mkstemp(temp_file);
  if (fd == -1) {
    _GD_SetError(D, GD_E_OPEN_INCLUDE, errno, NULL, 0, temp_file);
    dreturnvoid();
    return;
  }
  stream = fdopen(fd, "w");
  if (stream == NULL) {
    _GD_SetError(D, GD_E_OPEN_INCLUDE, errno, NULL, 0, temp_file);
    dreturnvoid();
    return;
  }

  /* Introit */
  t = time(NULL);
  strftime(buffer, GD_MAX_LINE_LENGTH, "%c", gmtime_r(&t, &now));

  fprintf(stream, "# This is a dirfile format file.\n"
      "# It was written using version %s of the GetData Library.\n"
      "# Written on %s UTC", PACKAGE_VERSION, buffer);

  if ((ptr = getenv("LOGNAME")) != NULL) {
    fprintf(stream, " by %s", ptr);
    if ((ptr = getenv("HOSTNAME")) != NULL)
      fprintf(stream, "@%s", ptr);
  }
  fputs(".\n", stream);

  /* Regardless of the version of the input dirfile, we always write
   * the latest version to disk -- this is present in every format file
   * fragment as the first non-comment line for sanity's sake. */
  fprintf(stream, "/VERSION %i\n", DIRFILE_STANDARDS_VERSION);

  /* Byte Sex */
  fprintf(stream, "/ENDIAN %s\n",
#ifdef WORDS_BIGENDIAN
      (D->fragment[i].byte_sex == GD_LITTLE_ENDIAN) ? "little" : "big"
#else
      (D->fragment[i].byte_sex == GD_BIG_ENDIAN) ? "big" : "little"
#endif
      );

  if (D->fragment[i].protection == GD_PROTECT_NONE)
    fputs("/PROTECT none\n", stream);
  else if (D->fragment[i].protection == GD_PROTECT_FORMAT)
    fputs("/PROTECT format\n", stream);
  else if (D->fragment[i].protection == GD_PROTECT_FORMAT)
    fputs("/PROTECT data\n", stream);
  else
    fputs("/PROTECT all\n", stream);

  if (D->fragment[i].frame_offset != 0)
    fprintf(stream, "/FRAMEOFFSET %llu\n",
        (unsigned long long)D->fragment[i].frame_offset);

  /* The encoding -- we only write encodings we know about. */
  switch(D->fragment[i].encoding) {
    case GD_UNENCODED:
      fputs("/ENCODING none\n", stream);
      break;
    case GD_SLIM_ENCODED:
      fputs("/ENCODING slim\n", stream);
      break;
    case GD_TEXT_ENCODED:
      fputs("/ENCODING text\n", stream);
      break;
  }

  /* The includes */
  for (j = 0; j < D->n_fragment; ++j)
    if (D->fragment[j].parent == i)
      fprintf(stream, "/INCLUDE %s\n", D->fragment[j].ename);

  /* The fields */
  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
      _GD_FieldSpec(D, stream, D->entry[u], 0);
      for (j = 0; j < D->entry[u]->e->n_meta; ++j)
        _GD_FieldSpec(D, stream, D->entry[u]->e->meta_entry[j], 1);
    }

  /* REFERENCE is written at the end, because its effect can propagate
   * upwards */
  if (D->fragment[i].ref_name != NULL)
    fprintf(stream, "/REFERENCE %s\n", _GD_StringEscapeise(buffer,
          D->fragment[i].ref_name));

  /* That's all, flush, sync, and close */
  fflush(stream);
  fsync(fd);
  fchmod(fd, mode);
  fclose(stream);

  /* If no error was encountered, move the temporary file over the
   * old format file, otherwise abort */
  if (D->error != GD_E_OK) {
    unlink(temp_file);
    dreturnvoid();
    return;
    /* Only assume we've synced the file if the rename succeeds */
  } else if (rename(temp_file, D->fragment[i].cname)) {
    _GD_SetError(D, GD_E_OPEN_INCLUDE, errno, NULL, 0, D->fragment[i].cname);
    unlink(temp_file);
    dreturnvoid();
    return;
  } else
    D->fragment[i].modified = 0;

  dreturnvoid();
}

void _GD_FlushMeta(DIRFILE* D, int fragment)
{
  int i;

  dtrace("%p, %i", D, fragment);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    /* nothing to do */
    dreturnvoid();
    return;
  }

  if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i)
      if (D->fragment[i].modified)
        _GD_FlushFragment(D, i);
  } else if (D->fragment[fragment].modified)
    _GD_FlushFragment(D, fragment);

  dreturnvoid();
}

int dirfile_metaflush(DIRFILE* D)
{
  dtrace("%p", D);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) /* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
  else
    _GD_FlushMeta(D, GD_ALL_FRAGMENTS);

  dreturn("%i", (D->error == GD_E_OK) ? 0 : -1);
  return (D->error == GD_E_OK) ? 0 : -1;
}

int dirfile_flush(DIRFILE* D, const char* field_code)
{
  unsigned int i;

  dtrace("%p, \"%s\"", D, field_code);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) /* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
  else if (field_code == NULL) {
    _GD_FlushMeta(D, GD_ALL_FRAGMENTS);
    if (!D->error)
      for (i = 0; i < D->n_entries; ++i)
        if (D->entry[i]->field_type == GD_RAW_ENTRY)
          _GD_Flush(D, D->entry[i], NULL);
  } else {
    _GD_Flush(D, _GD_FindField(D, field_code, NULL), field_code);
  }

  dreturn("%i", (D->error == GD_E_OK) ? 0 : -1);
  return (D->error == GD_E_OK) ? 0 : -1;
}
/* vim: ts=2 sw=2 et tw=80
*/
