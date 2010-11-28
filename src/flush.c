/* (C) 2008-2010 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
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
#include <limits.h>
#endif

#define GD_MAX_PRETTY_FIELD_WIDTH 80

void _GD_Flush(DIRFILE* D, gd_entry_t *E)
{
  int i;

  dtrace("%p, %p", D, E);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, E->field);
    D->recurse_level--;
    dreturnvoid();
    return;
  }

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      if (E->e->u.raw.file[0].fp >= 0) {
        if ((D->flags & GD_ACCMODE) == GD_RDWR &&
            _gd_ef[E->e->u.raw.file[0].encoding].sync != NULL &&
            (*_gd_ef[E->e->u.raw.file[0].encoding].sync)(E->e->u.raw.file))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno,
              NULL);
        else if ((*_gd_ef[E->e->u.raw.file[0].encoding].close)(
              E->e->u.raw.file))
        {
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno,
              NULL);
        }
      }
      break;
    case GD_LINCOM_ENTRY:
      for (i = 2; i < E->EN(lincom,n_fields); ++i)
        _GD_Flush(D, E->e->entry[i]);
      /* fallthrough */
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
      _GD_Flush(D, E->e->entry[1]);
      /* fallthrough */
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
      _GD_Flush(D, E->e->entry[0]);
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
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
  const char* ptr;

  dtrace("%p, 0x%x", D, data_type);

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
    case GD_COMPLEX64:
      ptr = "COMPLEX64";
      break;
    case GD_COMPLEX128:
      ptr = "COMPLEX128";
      break;
    default:
      _GD_InternalError(D);
      ptr = "";
      break;
  }

  dreturn("\"%s\"", ptr);
  return ptr;
}

static const char* _GD_OldTypeName(DIRFILE* D, gd_type_t data_type)
{
  const char* ptr;

  dtrace("%p, 0x%x", D, data_type);

  switch(data_type) {
    case GD_UINT8:
      ptr = "c";
      break;
    case GD_UINT16:
      ptr = "u";
      break;
    case GD_INT16:
      ptr = "s";
      break;
    case GD_UINT32:
      ptr = "U";
      break;
    case GD_INT32:
      ptr = "S";
      break;
    case GD_FLOAT32:
      ptr = "f";
      break;
    case GD_FLOAT64:
      ptr = "d";
      break;
    default:
      _GD_InternalError(D);
      ptr = "";
      break;
  }

  dreturn("\"%s\"", ptr);
  return ptr;
}

static size_t _GD_StringEscapeise(FILE* stream, const char* in, int permissive,
    int standards)
{
  const char* HexDigit = "0123456789ABCDEF";
  size_t len = 0;

  dtrace("%p, \"%s\", %i, %i", stream, in, permissive, standards);

  if (!permissive && standards < 6) {
    fputs(in, stream);
    dreturn("%i", strlen(in));
    return strlen(in);
  }

  for (; *in != '\0'; ++in) {
    if (*in == '"') {
      fputs("\\\"", stream);
      len += 2;
      fputc('\\', stream);
    } else if (*in == '\\' || *in == '#' || *in == '"') {
      fputc('\\', stream);
      fputc(*in, stream);
      len += 2;
    } else if (*in < 0x20
#if CHAR_MIN != 0
        && *in >= 0x00
#endif
        ) {
      fputs("\\x", stream);
      fputc(HexDigit[*in >> 8], stream);
      fputc(HexDigit[*in & 0xF], stream);
      len += 4;
    } else {
      fputc(*in, stream);
      len++;
    }
  }

  dreturn("%zu", len);
  return len;
}

static void _GD_PadField(FILE* stream, const char* in, size_t len,
    int permissive, int standards)
{
  size_t i;

  dtrace("%p, \"%s\", %zu, %i, %i", stream, in, len, permissive, standards);

  for (i = _GD_StringEscapeise(stream, in, permissive, standards); i < len; ++i)
    fputc(' ', stream);

  dreturnvoid();
}

/* Write a litteral parameter or CONST name or CARRAY element */
static void _GD_WriteConst(DIRFILE *D, FILE* stream, int type,
    const void* value, const char* scalar, int index, const char* postamble)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i, \"%s\"", D, stream, type, value, scalar,
      index, postamble);

  if (scalar != NULL) {
    if (index == -1)
      fprintf(stream, "%s%s", scalar, postamble);
    else
      fprintf(stream, "%s<%i>%s", scalar, index, postamble);
  } else if (type == GD_UINT16)
    fprintf(stream, "%" PRIu16 "%s", *(uint16_t *)value, postamble);
  else if (type == GD_INT64)
    fprintf(stream, "%" PRIi64 "%s", *(uint64_t *)value, postamble);
  else if (type == GD_INT16)
    fprintf(stream, "%" PRIi16 "%s", *(int16_t *)value, postamble);
  else if (type == GD_FLOAT64)
    fprintf(stream, "%.15g%s", *(double *)value, postamble);
  else if (type == GD_COMPLEX128)
    fprintf(stream, "%.15g;%.15g%s", *(double *)value, ((double *)value)[1],
      postamble);
  else
    _GD_InternalError(D);

  dreturnvoid();
}

/* Write a field specification line */
static void _GD_FieldSpec(DIRFILE* D, FILE* stream, const gd_entry_t* E,
    int meta, size_t max_len, int pretty, int permissive)
{
  int i;
  size_t z;
  char *ptr;

  dtrace("%p, %p, %p, %i, %zi, %i, %i", D, stream, E, meta, max_len, pretty,
      permissive);

  /* INDEX is implicit, and it is an error to define it in the format file */
  if (E->field_type == GD_INDEX_ENTRY) {
    dreturnvoid();
    return;
  }

  ptr = E->field;

  if (meta) {
    fputs("META ", stream);
    for (; *ptr != '/'; ++ptr)
      fputc(*ptr, stream);
    fputc(' ', stream);
    ++ptr;
  }

  /* field name */
  _GD_PadField(stream, ptr, max_len, permissive, D->standards);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      fprintf(stream, " RAW%s %s ", pretty ? "     " : "",
          (permissive || D->standards >= 5) ?  _GD_TypeName(D,
            E->EN(raw,data_type)) : _GD_OldTypeName(D, E->EN(raw,data_type)));
      _GD_WriteConst(D, stream, GD_UINT16, &E->EN(raw,spf), E->scalar[0],
          E->scalar_ind[0], "\n");
      break;
    case GD_LINCOM_ENTRY:
      fprintf(stream, " LINCOM%s %i", pretty ? "  " : "",
          E->EN(lincom,n_fields));
      for (i = 0; i < E->EN(lincom,n_fields); ++i) {
        fprintf(stream, " %s ", E->in_fields[i]);
        if (E->comp_scal) {
          _GD_WriteConst(D, stream, GD_COMPLEX128, &E->EN(lincom,cm)[i],
              E->scalar[i], E->scalar_ind[i], " ");
          _GD_WriteConst(D, stream, GD_COMPLEX128, &E->EN(lincom,cb)[i],
              E->scalar[i + GD_MAX_LINCOM], E->scalar_ind[i + GD_MAX_LINCOM],
              "");
        } else {
          _GD_WriteConst(D, stream, GD_FLOAT64, &E->EN(lincom,m)[i],
              E->scalar[i], E->scalar_ind[i], " ");
          _GD_WriteConst(D, stream, GD_FLOAT64, &E->EN(lincom,b)[i],
              E->scalar[i + GD_MAX_LINCOM], E->scalar_ind[i + GD_MAX_LINCOM],
              "");
        }
      }
      fputs("\n", stream);
      break;
    case GD_LINTERP_ENTRY:
      fprintf(stream, " LINTERP%s %s %s\n", pretty ? " " : "", E->in_fields[0],
          E->EN(linterp,table));
      break;
    case GD_BIT_ENTRY:
      fprintf(stream, " BIT%s %s ", pretty ? "     " : "", E->in_fields[0]);
      _GD_WriteConst(D, stream, GD_INT16, &E->EN(bit,bitnum), E->scalar[0],
          E->scalar_ind[0], " ");
      _GD_WriteConst(D, stream, GD_INT16, &E->EN(bit,numbits), E->scalar[1],
          E->scalar_ind[1], "\n");
      break;
    case GD_DIVIDE_ENTRY:
      fprintf(stream, " DIVIDE%s %s %s", pretty ? "  " : "", E->in_fields[1],
          E->in_fields[0]);
      break;
    case GD_RECIP_ENTRY:
      fprintf(stream, " RECIP%s ", pretty ? "   " : "");
      _GD_WriteConst(D, stream, GD_COMPLEX128, &E->EN(recip,cdividend),
          E->scalar[0], E->scalar_ind[0], "");
      fprintf(stream, " %s", E->in_fields[0]);
      break;
    case GD_MULTIPLY_ENTRY:
      fprintf(stream, " MULTIPLY %s %s\n", E->in_fields[0], E->in_fields[1]);
      break;
    case GD_PHASE_ENTRY:
      fprintf(stream, " PHASE%s %s ", pretty ? "   " : "", E->in_fields[0]);
      _GD_WriteConst(D, stream, GD_INT64, &E->EN(phase,shift), E->scalar[0],
          E->scalar_ind[0], "\n");
      break;
    case GD_POLYNOM_ENTRY:
      fprintf(stream, " POLYNOM%s %s ", pretty ? " " : "", E->in_fields[0]);
      for (i = 0; i <= E->EN(polynom,poly_ord); ++i)
        if (E->comp_scal)
          _GD_WriteConst(D, stream, GD_COMPLEX128, &E->EN(polynom,ca)[i],
              E->scalar[i], E->scalar_ind[i], (i == E->EN(polynom,poly_ord)) ?
              "\n" : " ");
        else
          _GD_WriteConst(D, stream, GD_FLOAT64, &E->EN(polynom,a)[i],
              E->scalar[i], E->scalar_ind[i], (i == E->EN(polynom,poly_ord)) ?
              "\n" : " ");
      break;
    case GD_SBIT_ENTRY:
      fprintf(stream, " SBIT%s %s ", pretty ? "    " : "", E->in_fields[0]);
      _GD_WriteConst(D, stream, GD_INT16, &E->EN(bit,bitnum), E->scalar[0],
          E->scalar_ind[0], " ");
      _GD_WriteConst(D, stream, GD_INT16, &E->EN(bit,numbits), E->scalar[1],
          E->scalar_ind[1], "\n");
      break;
    case GD_CONST_ENTRY:
      fprintf(stream, " CONST%s %s ", pretty ? "   " : "", _GD_TypeName(D,
            E->EN(scalar,const_type)));
      if (E->EN(scalar,const_type) & GD_SIGNED)
        fprintf(stream, "%" PRIi64 "\n", *(int64_t*)E->e->u.scalar.d);
      else if (E->EN(scalar,const_type) & GD_IEEE754)
        fprintf(stream, "%.15g\n", *(double*)E->e->u.scalar.d);
      else if (E->EN(scalar,const_type) & GD_COMPLEX)
        fprintf(stream, "%.15g;%.15g\n", *(double*)E->e->u.scalar.d,
            *((double*)E->e->u.scalar.d + 1));
      else
        fprintf(stream, "%" PRIu64 "\n", *(uint64_t*)E->e->u.scalar.d);
      break;
    case GD_CARRAY_ENTRY:
      fprintf(stream, " CARRAY%s %s", pretty ? "  " : "", _GD_TypeName(D,
            E->EN(scalar,const_type)));
      if (E->EN(scalar,const_type) & GD_SIGNED)
        for (z = 0; z < E->EN(scalar,array_len); ++z)
          fprintf(stream, " %" PRIi64, ((int64_t*)E->e->u.scalar.d)[z]);
      else if (E->EN(scalar,const_type) & GD_IEEE754)
        for (z = 0; z < E->EN(scalar,array_len); ++z)
          fprintf(stream, " %.15g", ((double*)E->e->u.scalar.d)[z]);
      else if (E->EN(scalar,const_type) & GD_COMPLEX)
        for (z = 0; z < E->EN(scalar,array_len); ++z)
          fprintf(stream, " %.15g;%.15g", ((double*)E->e->u.scalar.d)[2 * z],
              ((double*)E->e->u.scalar.d)[2 * z + 1]);
      else
        for (z = 0; z < E->EN(scalar,array_len); ++z)
          fprintf(stream, " %" PRIu64, ((uint64_t*)E->e->u.scalar.d)[z]);
      fputs("\n", stream);
      break;
    case GD_STRING_ENTRY:
      fprintf(stream, " STRING%s \"", pretty ? "  " : "");
      _GD_StringEscapeise(stream, E->e->u.string, permissive, D->standards);
      fputs("\"\n", stream);
      break;
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      _GD_InternalError(D);
      break;
  }

  dreturnvoid();
}

static void _GD_FlushFragment(DIRFILE* D, int i, int permissive)
{
  int j;
  FILE* stream;
  char buffer[GD_MAX_LINE_LENGTH];
  char *temp_file;
  char* ptr;
  struct tm now;
  int fd;
  int pretty = 0;
  size_t max_len = 0;
  const size_t name_len = strlen(D->name);
  unsigned int u;
  mode_t mode;
  struct stat stat_buf;
  time_t t;

  dtrace("%p, %i, %i", D, i, permissive);

  /* get the permissions of the old file */
  if (stat(D->fragment[i].cname, &stat_buf))
    mode = 0644;
  else
    mode = stat_buf.st_mode;

  /* open a temporary file */
  temp_file = (char *)malloc(name_len + 15);
  snprintf(temp_file, name_len + 15, "%s/format_XXXXXX", D->name);
  fd = mkstemp(temp_file);
  if (fd == -1) {
    _GD_SetError(D, GD_E_FLUSH, GD_E_FLUSH_MKTMP, NULL, errno, temp_file);
    free(temp_file);
    dreturnvoid();
    return;
  }
  stream = fdopen(fd, "w+");
  if (stream == NULL) {
    _GD_SetError(D, GD_E_FLUSH, GD_E_FLUSH_OPEN, NULL, errno, temp_file);
    free(temp_file);
    dreturnvoid();
    return;
  }

  if (D->flags & GD_PRETTY_PRINT) {
    size_t t = 0;
    size_t m = 0;
    int n = 0;
    pretty = 1;
    for (u = 0; u < D->n_entries; ++u)
      if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
        size_t l = strlen(D->entry[u]->field);
        if (m < l)
          m = l;
        t += l;
        n++;
      }
    max_len = 2 * t / n;
    if (max_len > m)
      max_len = m;
    if (max_len > GD_MAX_PRETTY_FIELD_WIDTH)
      max_len = GD_MAX_PRETTY_FIELD_WIDTH;
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
  fputs(".\n\n", stream);

  if (permissive)
    fprintf(stream, "# WARNING: This fragment may not conform to any "
        "Dirfile Standards Version.\n");
  else if (D->standards >= 5)
    fprintf(stream, "/VERSION %i\n", D->standards);
  else
    fprintf(stream, "# This fragment conforms to "
        "Dirfile Standards Version %i.\n", D->standards);

  /* Byte Sex */
  if (permissive || D->standards >= 5)
    fprintf(stream, "/ENDIAN %s%s\n",
        (D->fragment[i].byte_sex & GD_LITTLE_ENDIAN) ? "little" : "big",
        ((permissive || D->standards >= 8) &&
         (D->fragment[i].byte_sex & GD_ARM_FLAG) == GD_ARM_ENDIAN) ? " arm" :
        "");

  if (permissive || D->standards >= 6) {
    if (D->fragment[i].protection == GD_PROTECT_NONE)
      fputs("/PROTECT none\n", stream);
    else if (D->fragment[i].protection == GD_PROTECT_FORMAT)
      fputs("/PROTECT format\n", stream);
    else if (D->fragment[i].protection == GD_PROTECT_FORMAT)
      fputs("/PROTECT data\n", stream);
    else
      fputs("/PROTECT all\n", stream);
  }

  if (permissive || D->standards >= 1)
    if (D->fragment[i].frame_offset != 0)
      fprintf(stream, "%sFRAMEOFFSET %llu\n", (D->standards >= 5) ? "/" : "",
          (unsigned long long)D->fragment[i].frame_offset);

  if (permissive || D->standards >= 6) {
    switch(D->fragment[i].encoding) {
      case GD_UNENCODED:
        fputs("/ENCODING none\n", stream);
        break;
      case GD_BZIP2_ENCODED:
        fputs("/ENCODING bzip2\n", stream);
        break;
      case GD_GZIP_ENCODED:
        fputs("/ENCODING gzip\n", stream);
        break;
      case GD_LZMA_ENCODED:
        fputs("/ENCODING lzma\n", stream);
        break;
      case GD_SLIM_ENCODED:
        fputs("/ENCODING slim\n", stream);
        break;
      case GD_TEXT_ENCODED:
        fputs("/ENCODING text\n", stream);
        break;
      case 0:
        break;
      default:
        fprintf(stream, "/ENCODING unknown # (%lx)\n", D->fragment[i].encoding);
        break;
    }
  }

  /* The includes */
  if (permissive || D->standards >= 3)
    for (j = 0; j < D->n_fragment; ++j)
      if (D->fragment[j].parent == i)
        fprintf(stream, "%sINCLUDE %s\n", (D->standards >= 5) ? "/" : "",
            D->fragment[j].ename);

  /* The fields */
  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
      _GD_FieldSpec(D, stream, D->entry[u], 0, max_len, pretty, permissive);
      if (permissive || D->standards >= 6)
        for (j = 0; j < D->entry[u]->e->n_meta; ++j)
          _GD_FieldSpec(D, stream, D->entry[u]->e->p.meta_entry[j], 1, 0,
              pretty, permissive);
    }

  /* REFERENCE is written at the end, because its effect can propagate
   * upwards */
  if (permissive || D->standards >= 6)
    if (D->fragment[i].ref_name != NULL) {
      fputs("/REFERENCE ", stream);
      _GD_StringEscapeise(stream, D->fragment[i].ref_name, permissive,
          D->standards);
      fputc('\n', stream);
    }

  /* That's all; flush, sync, and close */
  fflush(stream);
  fsync(fd);
#ifdef HAVE_FCHMOD  
  fchmod(fd, mode);
#endif
  fclose(stream);

  /* If no error was encountered, move the temporary file over the
   * old format file, otherwise abort */
  if (D->error != GD_E_OK) {
    unlink(temp_file);
    free(temp_file);
    dreturnvoid();
    return;
    /* Only assume we've synced the file if the rename succeeds */
  } else if (_GD_Rename(temp_file, D->fragment[i].cname)) {
    _GD_SetError(D, GD_E_FLUSH, GD_E_FLUSH_RENAME, NULL, errno,
        D->fragment[i].cname);
    unlink(temp_file);
    free(temp_file);
    dreturnvoid();
    return;
  } else
    D->fragment[i].modified = 0;

  free(temp_file);
  dreturnvoid();
}

void _GD_FlushMeta(DIRFILE* D, int fragment, int force)
{
  int i;

  dtrace("%p, %i, %i", D, fragment, force);

  if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i)
      if (force || D->fragment[i].modified)
        _GD_FlushFragment(D, i, D->flags & GD_PERMISSIVE);
  } else if (force || D->fragment[fragment].modified)
    _GD_FlushFragment(D, fragment, D->flags & GD_PERMISSIVE);

  dreturnvoid();
}

int gd_metaflush(DIRFILE* D)
{
  dtrace("%p", D);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_FlushMeta(D, GD_ALL_FRAGMENTS, 0);

  dreturn("%i", (D->error == GD_E_OK) ? 0 : -1);
  return (D->error == GD_E_OK) ? 0 : -1;
}

int gd_rewrite_fragment(DIRFILE* D, int fragment)
{
  dtrace("%p, %i", D, fragment);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 1);
    return -1;
  }

  if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_FlushMeta(D, fragment, 1);

  dreturn("%i", (D->error == GD_E_OK) ? 0 : -1);
  return (D->error == GD_E_OK) ? 0 : -1;
}

int gd_flush(DIRFILE* D, const char* field_code)
{
  unsigned int i;
  int repr;
  char *simple_field_code;
  gd_entry_t *E;

  dtrace("%p, \"%s\"", D, field_code);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) /* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
  else if (field_code == NULL) {
    _GD_FlushMeta(D, GD_ALL_FRAGMENTS, 0);
    if (!D->error)
      for (i = 0; i < D->n_entries; ++i)
        if (D->entry[i]->field_type == GD_RAW_ENTRY)
          _GD_Flush(D, D->entry[i]);
  } else {
    /* discard representation */
    E = _GD_FindFieldAndRepr(D, field_code, &simple_field_code, &repr, NULL, 1);

    if (!D->error)
      _GD_Flush(D, E);

    if (field_code != simple_field_code)
      free(simple_field_code);
  }

  dreturn("%i", (D->error == GD_E_OK) ? 0 : -1);
  return (D->error == GD_E_OK) ? 0 : -1;
}

#define GD_VERS_GE_1  0xFFFFFFFFFFFFFFFELLU
#define GD_VERS_GE_2  0xFFFFFFFFFFFFFFFCLLU
#define GD_VERS_GE_3  0xFFFFFFFFFFFFFFF8LLU
#define GD_VERS_GE_4  0xFFFFFFFFFFFFFFF0LLU
#define GD_VERS_GE_5  0xFFFFFFFFFFFFFFE0LLU
#define GD_VERS_GE_6  0xFFFFFFFFFFFFFFC0LLU
#define GD_VERS_GE_7  0xFFFFFFFFFFFFFF80LLU
#define GD_VERS_GE_8  0xFFFFFFFFFFFFFF00LLU

#define GD_VERS_LE_0  0x0000000000000001LLU
#define GD_VERS_LE_1  0x0000000000000003LLU
#define GD_VERS_LE_2  0x0000000000000007LLU
#define GD_VERS_LE_3  0x000000000000000fLLU
#define GD_VERS_LE_4  0x000000000000001fLLU
#define GD_VERS_LE_5  0x000000000000003fLLU
#define GD_VERS_LE_6  0x000000000000007fLLU
#define GD_VERS_LE_7  0x00000000000000ffLLU
#define GD_VERS_LE_8  0x00000000000001ffLLU

uint64_t _GD_FindVersion(DIRFILE *D)
{
  unsigned int i;
  char* ptr;
  dtrace("%p", D);

  D->av = (1 << (1 + GD_DIRFILE_STANDARDS_VERSION)) - 1;

  if (D->n_fragment > 1)
    D->av &= GD_VERS_GE_3;

  for (i = 0; D->av && i < (unsigned int)D->n_fragment; ++i) {
    /* on an arm-endian platform, the arm flag is set by /ENDIAN directives
     * missing an "arm" token, but it's absense might mean either an "arm"
     * token was present, or else there was no /ENDIAN directive at all */
    if (D->fragment[i].byte_sex & GD_ARM_FLAG)
#ifdef ARM_ENDIAN_DOUBLES
      D->av &= GD_VERS_GE_5;
#else
    D->av &= GD_VERS_GE_8;
    else
#endif
      if ((D->fragment[i].encoding != GD_UNENCODED &&
            D->fragment[i].encoding != GD_AUTO_ENCODED) ||
          D->fragment[i].protection)
        D->av &= GD_VERS_GE_6;
      else if (D->fragment[i].byte_sex &
#ifdef WORDS_BIGENDIAN
          GD_LITTLE_ENDIAN
#else
          GD_BIG_ENDIAN
#endif
          )
        D->av &= GD_VERS_GE_5;
      else if (D->fragment[i].frame_offset > 0)
        D->av &= GD_VERS_GE_1;
  }

  for (i = 0; D->av && i < D->n_entries; ++i) {
    switch (D->entry[i]->field_type) {
      case GD_RAW_ENTRY:
        switch (D->entry[i]->EN(raw,data_type)) {
          case GD_COMPLEX128:
          case GD_COMPLEX64:
            D->av &= GD_VERS_GE_7;
            break;
          case GD_INT8:
          case GD_INT64:
          case GD_UINT64:
            D->av &= GD_VERS_GE_5;
            break;
          default:
            break;
        }
        break;
      case GD_DIVIDE_ENTRY:
      case GD_RECIP_ENTRY:
      case GD_CARRAY_ENTRY:
        D->av &= GD_VERS_GE_8;
        break;
      case GD_MULTIPLY_ENTRY:
        D->av &= GD_VERS_GE_2;
        break;
      case GD_PHASE_ENTRY:
        D->av &= GD_VERS_GE_4;
        break;
      case GD_POLYNOM_ENTRY:
      case GD_SBIT_ENTRY:
        D->av &= GD_VERS_GE_7;
        break;
      case GD_CONST_ENTRY:
        if (D->entry[i]->EN(scalar,const_type) & GD_COMPLEX128)
          D->av &= GD_VERS_GE_7;
        else
          D->av &= GD_VERS_GE_6;
        break;
      case GD_STRING_ENTRY:
        D->av &= GD_VERS_GE_6;
        break;
      case GD_BIT_ENTRY:
        if (D->entry[i]->EN(bit,numbits) > 1)
          D->av &= GD_VERS_GE_1;
        else if (D->entry[i]->EN(bit,bitnum) + D->entry[i]->EN(bit,numbits) - 1
            > 32)
        {
          D->av &= GD_VERS_GE_5;
        }
        break;
      case GD_LINTERP_ENTRY:
      case GD_LINCOM_ENTRY:
      case GD_INDEX_ENTRY:
      case GD_NO_ENTRY:
        break;
    }
    if (D->av & GD_VERS_GE_1 && strcmp(D->entry[i]->field, "FRAMEOFFSET") == 0)
      D->av &= (GD_VERS_LE_0 | GD_VERS_GE_8);
    else if (D->av & GD_VERS_GE_3 && strcmp(D->entry[i]->field, "INCLUDE") == 0)
      D->av &= (GD_VERS_LE_2 | GD_VERS_GE_8);
    else if (D->av & GD_VERS_GE_5 && (strcmp(D->entry[i]->field, "VERSION") == 0
          || strcmp(D->entry[i]->field, "ENDIAN") == 0))
      D->av &= (GD_VERS_LE_4 | GD_VERS_GE_8);
    else if (D->av & GD_VERS_GE_6 &&
        (strcmp(D->entry[i]->field, "ENCODING") == 0
         || strcmp(D->entry[i]->field, "META") == 0
         || strcmp(D->entry[i]->field, "PROTECT") == 0
         || strcmp(D->entry[i]->field, "REFERENCE") == 0))
      D->av &= (GD_VERS_LE_5 | GD_VERS_GE_8);
    else if (D->av & GD_VERS_LE_5 &&
        strcmp(D->entry[i]->field, "FILEFRAM") == 0)
      D->av &= GD_VERS_GE_6;

    for (ptr = D->entry[i]->field; *ptr != 0 && D->av; ++ptr)
      switch(*ptr) {
        case '/': /* a metafield */
        case '#':
        case ' ':
          D->av &= GD_VERS_GE_6;
          break;
        case '.':
          D->av &= GD_VERS_LE_5;
          break;
        case '&':
        case ';':
        case '<':
        case '>':
        case '\\':
        case '|':
          D->av &= GD_VERS_LE_4;
          break;
      }
  }

  D->flags |= GD_HAVE_VERSION;
  dreturn("%04llx", D->av);
  return D->av;
}

int gd_dirfile_standards(DIRFILE *D, int vers) gd_nothrow
{
  /* log2(n) lut */
  static const char ln2[] = { -1,
    0,
    1, 1,
    2, 2, 2, 2,
    3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
  };

  /* some magic numbers */
  static const char earliest_magic[] = {64,  0,  1, 39,  2, 15, 40, 23,  3, 12,
    16, 59, 41, 19, 24, 54,  4,  0, 13, 10, 17, 62, 60, 28, 42, 30, 20, 51, 25,
    44, 55, 47,  5, 32,  0, 38, 14, 22, 11, 58, 18, 53, 63,  9, 61, 27, 29, 50,
    43, 46, 31, 37, 21, 57, 52,  8, 26, 49, 45, 36, 56,  7, 48, 35,  6, 34, 33};

  dtrace("%p, %i", D, vers);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  if (vers == GD_VERSION_CURRENT)
    vers = D->standards;
  else if (vers == GD_VERSION_LATEST) {
    uint32_t a, b;
    if ((b = D->av >> 16))
      vers = (a = b >> 8) ? 24 + ln2[a] : 16 + ln2[b];
    else
      vers = (a = D->av >> 8) ? 8 + ln2[a] : ln2[D->av];
  } else if (vers == GD_VERSION_EARLIEST)
    vers = earliest_magic[(~(D->av - 1) & D->av) % 67];

  if (vers < 0 || vers > GD_DIRFILE_STANDARDS_VERSION ||
      ~D->av & (1ULL << vers))
  {
    _GD_SetError(D, GD_E_BAD_VERSION, (D->av == 0) ? GD_E_VERS_NONE :
        GD_E_VERS_MISSING, NULL, vers, NULL);
    dreturn("%i", -1);
    return -1;
  }

  D->standards = vers;
  dreturn("%i", vers);
  return vers;
}
/* vim: ts=2 sw=2 et tw=80
*/
