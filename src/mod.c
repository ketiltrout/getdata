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
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

static unsigned int max(unsigned int A, unsigned int B)
{
  return (A > B) ? A : B;
}

static int _GD_Change(DIRFILE *D, const char *field_code, const gd_entry_t *N,
    int flags)
{
  int i;
  int field_free = 0;
  int scalar_free = 0;
  int modified = 0;
  void *ptr;
  gd_entry_t *E = NULL;
  gd_entry_t Q;
  struct _gd_private_entry Qe;

  dtrace("%p, \"%s\", %p, %i", D, field_code, N, flags);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if ((E = _GD_FindField(D, field_code, NULL)) == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
  else if (E->field_type != N->field_type)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_MATCH, NULL, 0, field_code);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  memcpy(&Qe, E->e, sizeof(struct _gd_private_entry));
  memcpy(&Q, E, sizeof(gd_entry_t));

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      if (N->spf != 0) {
        Q.spf = N->spf;
        if (N->e != NULL && N->e->scalar[0] != NULL) {
          Qe.scalar[0] = strdup(N->e->scalar[0]);
          scalar_free |= 1;
        }
      }
      Q.data_type = (N->data_type == GD_NULL) ? E->data_type : N->data_type;

      /* nothing to do */
      if (Q.spf == E->spf && Q.data_type == E->data_type)
        break;

      modified = 1;

      if (Q.data_type & 0x40 || (Qe.size = GD_SIZE(Q.data_type)) == 0) {
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, Q.data_type, NULL);
        dreturn("%i", -1);
        return -1;
      }

      if (flags) {
        ssize_t nread, nwrote;
        off64_t ns_out;
        void *buffer1;
        void *buffer2;
        const off64_t nf = BUFFER_SIZE / max(E->e->size, GD_SIZE(Q.data_type)) /
          max(E->spf, Q.spf);


        if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA)
          _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
              D->fragment[E->fragment_index].cname);
        else
          _GD_Supports(D, E, GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK |
              GD_EF_READ | GD_EF_WRITE | GD_EF_SYNC | GD_EF_UNLINK |
              GD_EF_TEMP);

        if (D->error)
          break;

        const struct encoding_t* enc = ef + E->e->file[0].encoding;

        if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0))
          ; /* error already set */
        else if (E->e->file[0].fp == -1 && (*enc->open)(E->e->file, 0, 0))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
        else if ((*enc->seek)(E->e->file, 0, E->data_type, 1) == -1)
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);

        if (D->error)
          break;

        /* Create a temporary file and open it */
        if (_GD_SetEncodedName(D, E->e->file + 1, E->e->filebase, 1))
          ; /* error already set */
        else if ((*enc->temp)(E->e->file, GD_TEMP_OPEN))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
        else if ((*enc->seek)(E->e->file + 1, 0, E->data_type, 1) == -1)
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);

        if (D->error) {
          (*enc->temp)(E->e->file, GD_TEMP_DESTROY);
          break;
        }

        buffer1 = malloc(BUFFER_SIZE);
        buffer2 = malloc(BUFFER_SIZE);
        memset(buffer2, 0, BUFFER_SIZE);

        /* Now copy the old file to the new file */
        for (;;) {
          nread = (*enc->read)(E->e->file, buffer1, E->data_type, nf * E->spf);

          if (nread < 0) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
            break;
          }

          if (nread == 0)
            break;

          ns_out = nread * Q.spf / E->spf;

          /* spf convert -- this is done via AddData */
          if (Q.spf != E->spf)
            _GD_AddData(D, buffer2, Q.spf, buffer1, E->spf, E->data_type,
                ns_out);
          else {
            ptr = buffer1;
            buffer1 = buffer2;
            buffer2 = ptr;
          }

          /* type convert */
          if (Q.data_type != E->data_type)
            _GD_ConvertType(D, buffer2, E->data_type, buffer1, Q.data_type,
                ns_out);
          else {
            ptr = buffer1;
            buffer1 = buffer2;
            buffer2 = ptr;
          }

          nwrote = (*enc->write)(E->e->file + 1, buffer1, Q.data_type, ns_out);

          if (nwrote < ns_out) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
            break;
          }
        }

        free(buffer1);
        free(buffer2);

        /* An error occurred, clean up */
        if (D->error)
          (*enc->temp)(E->e->file, GD_TEMP_DESTROY);
        /* Well, I suppose the copy worked.  Close both files */
        else if ((*enc->close)(E->e->file) || (*enc->sync)(E->e->file + 1) ||
            (*enc->close)(E->e->file + 1))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
        /* Move the temporary file over the old file */
        else if ((*enc->temp)(E->e->file, GD_TEMP_MOVE))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      }

      break;
    case GD_LINCOM_ENTRY:
      Q.n_fields = (N->n_fields == 0) ? E->n_fields : N->n_fields;
      if (Q.n_fields < 1 || Q.n_fields > GD_MAX_LINCOM) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL,
            E->n_fields, NULL);
        break;
      }

      if (Q.n_fields != E->n_fields)
        modified = 1;

      for (i = 0; i < Q.n_fields; ++i) {
        if (flags & 0x1 && N->in_fields[i] != NULL &&
            strcmp(E->in_fields[i], N->in_fields[i]))
        {
          if ((Q.in_fields[i] = strdup(N->in_fields[i])) == NULL) {
            _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
            break;
          }
          modified = 1;
          field_free |= 2 << i;
        }

        if (flags & 0x2 && E->m[i] != N->m[i]) {
          modified = 1;
          Q.m[i] = N->m[i];
          if (N->e != NULL && N->e->scalar[i * 2] != NULL) {
            Qe.scalar[i * 2] = strdup(N->e->scalar[i * 2]);
            scalar_free |= 2 << (i * 2);
          }
        }

        if (flags & 0x4 && E->b[i] != N->b[i]) {
          modified = 1;
          Q.b[i] = N->b[i];
          if (N->e != NULL && N->e->scalar[i * 2 + 1] != NULL) {
            Qe.scalar[i * 2 + 1] = strdup(N->e->scalar[i * 2 + 1]);
            scalar_free |= 2 << (i * 2 + 1);
          }
        }
      }
      break;
    case GD_LINTERP_ENTRY:
      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      if (N->table != NULL && strcmp(E->table, N->table)) {
        if (N->table[0] == '/')
          Q.table = strdup(N->table);
        else {
          Q.table = malloc(FILENAME_MAX);
          if (Q.table != NULL) {
            char temp_buffer[FILENAME_MAX];
            strcpy(temp_buffer, D->fragment[E->fragment_index].cname);
            strcpy(Q.table, dirname(temp_buffer));
            strcat(Q.table, "/");
            strcat(Q.table, N->table);
          }
        }

        if (Q.table == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }

        if (flags) {
          if (rename(E->table, Q.table)) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->table, errno, 0);
            break;
          }
        }

        modified = 1;
        free(E->table);
      }

      break;
    case GD_BIT_ENTRY:
      if (N->numbits >= 1 && E->numbits != N->numbits) {
        modified = 1;
        Q.numbits = N->numbits;
        if (N->e != NULL && N->e->scalar[1] != NULL) {
          Qe.scalar[1] = strdup(N->e->scalar[1]);
          scalar_free |= 2;
        }
      }

      if (N->bitnum >= 0 && E->bitnum != N->bitnum) {
        modified = 1;
        Q.bitnum = N->bitnum;
        if (N->e != NULL && N->e->scalar[0] != NULL) {
          Qe.scalar[0] = strdup(N->e->scalar[0]);
          scalar_free |= 1;
        }
      }

      if (Q.bitnum + Q.numbits - 1 > 63)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITSIZE, NULL, 
            Q.bitnum + Q.numbits - 1, NULL);
      else if (N->in_fields[0] != NULL && strcmp(E->in_fields[0],
            N->in_fields[0]))
      {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      break;
    case GD_MULTIPLY_ENTRY:
      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      if (N->in_fields[1] != NULL && strcmp(E->in_fields[1], N->in_fields[1])) {
        if ((Q.in_fields[1] = strdup(N->in_fields[1])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free |= 2;
      }

      break;
    case GD_PHASE_ENTRY:
      if (E->shift != N->shift) {
        modified = 1;
        Q.shift = N->shift;
        if (N->e != NULL && N->e->scalar[0] != NULL) {
          Qe.scalar[0] = strdup(N->e->scalar[0]);
          scalar_free |= 1;
        }
      }

      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      break;
    case GD_CONST_ENTRY:
      Q.const_type = (N->const_type == GD_NULL) ? E->const_type : N->const_type;

      if (Q.const_type & 0x40 || GD_SIZE(Q.const_type) == 0) {
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, Q.const_type, NULL);
        dreturn("%i", -1);
        return -1;
      }

      if (Q.const_type != E->const_type) {
        modified = 1; 
        /* type convert */
        if (Q.const_type & GD_IEEE754)
          Qe.dconst = (E->const_type & GD_IEEE754) ? E->e->dconst :
            (E->const_type & GD_SIGNED) ? (double)E->e->iconst :
            (double)E->e->uconst;
        else if (Q.const_type & GD_SIGNED)
          Qe.iconst = (E->const_type & GD_IEEE754) ? (int64_t)E->e->dconst :
            (E->const_type & GD_SIGNED) ? E->e->iconst : (int64_t)E->e->uconst;
        else
          Qe.uconst = (E->const_type & GD_IEEE754) ? (uint64_t)E->e->dconst :
            (E->const_type & GD_SIGNED) ? (uint64_t)E->e->iconst : E->e->uconst;
      }

      break;
    case GD_INDEX_ENTRY:
      /* INDEX may not be modified */
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    case GD_NO_ENTRY:
    case GD_STRING_ENTRY:
      break;
  }

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  if (modified) {
    for (i = 0; i < GD_MAX_LINCOM; ++i) {
      if (field_free & (1 << i)) {
        Qe.entry[i] = NULL;
        free(E->in_fields[i]);
      }
    }

    for (i = 0; i < 2 * GD_MAX_LINCOM; ++i) {
      if (scalar_free & (2 << i))
        free(E->e->scalar[i]);
    }

    memcpy(E->e, &Qe, sizeof(struct _gd_private_entry));
    memcpy(E, &Q, sizeof(gd_entry_t));
    E->e->calculated = 0;
    D->fragment[E->fragment_index].modified = 1;
  }

  dreturn("%i", 0);
  return 0;
}

int dirfile_alter_entry(DIRFILE* D, const char* field_code,
    const gd_entry_t *entry, int move)
{
  dtrace("%p, \"%s\", %p, %i", D, field_code, entry, move);

  gd_entry_t N;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* To ensure .e is NULLed */
  memcpy(&N, entry, sizeof(gd_entry_t));
  N.e = NULL;

  if (N.field_type == GD_LINCOM_ENTRY)
    move = 7;

  int ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_raw(DIRFILE *D, const char *field_code, gd_type_t data_type,
    unsigned int spf, int move)
{
  gd_entry_t N;

  dtrace("%p, \"%s\", %u, %x, %i", D, field_code, spf, data_type, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N.field_type = GD_RAW_ENTRY;
  N.spf = spf;
  N.data_type = data_type;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_lincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const double* m, const double* b)
{
  gd_entry_t N;
  int i;
  int move = 0;

  dtrace("%p, \"%s\", %i, %p, %p, %p", D, field_code, n_fields, in_fields, m,
      b);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N.field_type = GD_LINCOM_ENTRY;
  if (n_fields != 0)
    N.n_fields = n_fields;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.n_fields = E->n_fields;
  }
  N.e = NULL;

  for (i = 0; i < N.n_fields; ++i) {
    if (in_fields != NULL) {
      move |= 1;
      N.in_fields[i] = (char*)in_fields[i];
    }

    if (m != NULL) {
      move |= 2;
      N.m[i] = m[i];
    }

    if (b != NULL) {
      move |= 4;
      N.b[i] = b[i];
    }
  }

  int ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_linterp(DIRFILE* D, const char* field_code,
    const char* in_field, const char* table, int move)
{
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field, table,
      move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_LINTERP_ENTRY;
  N.in_fields[0] = (char*)in_field;
  N.table = (char*)table;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_bit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits)
{
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %i, %i", D, field_code, in_field, bitnum,
      numbits);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_BIT_ENTRY;
  N.in_fields[0] = (char*)in_field;
  N.bitnum = bitnum;
  N.numbits = numbits;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_multiply(DIRFILE* D, const char* field_code,
    const char* in_field1, const char* in_field2)
{
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, field_code, in_field1, in_field2);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_MULTIPLY_ENTRY;
  N.in_fields[0] = (char*)in_field1;
  N.in_fields[1] = (char*)in_field2;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_phase(DIRFILE* D, const char* field_code,
    const char* in_field, int shift)
{
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %i", D, field_code, in_field, shift);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_PHASE_ENTRY;
  N.in_fields[0] = (char*)in_field;
  N.shift = shift;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_const(DIRFILE* D, const char* field_code,
    gd_type_t const_type)
{
  gd_entry_t N;

  dtrace("%p, \"%s\", %2x", D, field_code, const_type);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_CONST_ENTRY;
  N.const_type = const_type;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int dirfile_alter_spec(DIRFILE* D, const char* line, int move)
{
  char instring[GD_MAX_LINE_LENGTH];
  char outstring[GD_MAX_LINE_LENGTH];
  const char *in_cols[MAX_IN_COLS];
  int n_cols;
  gd_entry_t *N = NULL;

  dtrace("%p, \"%s\", %i", D, line, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  /* we do this to ensure line is not too long */
  strncpy(instring, line, GD_MAX_LINE_LENGTH - 1);
  instring[GD_MAX_LINE_LENGTH - 2] = '\0';

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols,
      "dirfile_alter_spec()", 0);

  if (D->error) {
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Sanity check */
  if (n_cols == 0) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, "dirfile_alter_spec()", 0,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  N = _GD_FindField(D, in_cols[0], NULL);

  if (N == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, in_cols[0]);
    dreturn("%i", -1);
    return -1;
  }

  /* Let the parser compose the entry */
  N = _GD_ParseFieldSpec(D, n_cols, in_cols, NULL, "dirfile_alter_spec()", 0, 
      N->fragment_index, DIRFILE_STANDARDS_VERSION, 1, 1, 0);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  if (N->field_type == GD_LINCOM_ENTRY)
    move = 7;

  /* Change the entry */
  int ret = _GD_Change(D, N->field, N, move);

  _GD_FreeE(N, 1);

  dreturn("%i", ret);
  return ret;
}

int dirfile_malter_spec(DIRFILE* D, const char* line, const char* parent,
    int move)
{
  char instring[GD_MAX_LINE_LENGTH];
  char outstring[GD_MAX_LINE_LENGTH];
  const char *in_cols[MAX_IN_COLS];
  int n_cols;
  gd_entry_t *N = NULL;

  dtrace("%p, \"%s\", \"%s\", %i", D, line, parent, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N = _GD_FindField(D, parent, NULL);
  if (N == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%i", -1);
    return -1;
  }

  /* we do this to ensure line is not too long */
  strncpy(instring, line, GD_MAX_LINE_LENGTH - 1);
  instring[GD_MAX_LINE_LENGTH - 2] = '\0';

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols,
      "dirfile_alter_spec()", 0);

  if (D->error) {
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Let the parser compose the entry */
  N = _GD_ParseFieldSpec(D, n_cols, in_cols, N, "dirfile_malter_spec()", 0,
      N->fragment_index, DIRFILE_STANDARDS_VERSION, 1, 1, 0);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  if (N->field_type == GD_LINCOM_ENTRY)
    move = 7;

  /* Change the entry */
  int ret = _GD_Change(D, N->field, N, move);

  _GD_FreeE(N, 1);

  dreturn("%i", ret);
  return ret;
}
