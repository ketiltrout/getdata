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
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

/* add an entry */
static int _GD_Add(DIRFILE* D, const gd_entry_t* entry, const char* parent)
{
  char temp_buffer[FILENAME_MAX];
  int i, is_dot;
  int copy_scalar[GD_MAX_POLYORD + 1];
  void* new_list;
  void* new_ref = NULL;
  unsigned int u;
  gd_entry_t* E;
  gd_entry_t* P = NULL;

  dtrace("%p, %p, \"%s\"", D, entry, parent);

  memset(copy_scalar, 0, sizeof(int) * (GD_MAX_POLYORD + 1));

  _GD_ClearError(D);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for include index out of range */
  if (P == NULL && (entry->fragment_index < 0 ||
        entry->fragment_index >= D->n_fragment))
  {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, entry->fragment_index, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check protection */
  if (D->fragment[entry->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[entry->fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  /* check parent */
  if (parent != NULL) {
    /* make sure it's not a meta field already */
    if (strchr(parent, '/') != NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
      dreturn("%i", -1);
      return -1;
    }

    P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);
    if (P == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
      dreturn("%i", -1);
      return -1;
    }

    snprintf(temp_buffer, FILENAME_MAX, "%s/%s", parent, entry->field);
  } else
    snprintf(temp_buffer, FILENAME_MAX, "%s", entry->field);

  /* check for duplicate field */
  E = _GD_FindField(D, temp_buffer, D->entry, D->n_entries, &u);

  if (E != NULL) { /* matched */
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, temp_buffer);
    dreturn("%i", -1);
    return -1;
  }

  /* check for bad field type */
  if (entry->field_type != GD_RAW_ENTRY &&
      entry->field_type != GD_LINCOM_ENTRY &&
      entry->field_type != GD_LINTERP_ENTRY &&
      entry->field_type != GD_BIT_ENTRY &&
      entry->field_type != GD_MULTIPLY_ENTRY &&
      entry->field_type != GD_PHASE_ENTRY &&
      entry->field_type != GD_CONST_ENTRY &&
      entry->field_type != GD_POLYNOM_ENTRY &&
      entry->field_type != GD_SBIT_ENTRY &&
      entry->field_type != GD_DIVIDE_ENTRY &&
      entry->field_type != GD_RECIP_ENTRY &&
      entry->field_type != GD_STRING_ENTRY)
  {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_TYPE, NULL,
        entry->field_type, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* New entry */
  E = (gd_entry_t *)malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }
  memset(E, 0, sizeof(gd_entry_t));
  if (P)
    E->fragment_index = P->fragment_index;
  else
    E->fragment_index = entry->fragment_index;

  E->e = (struct _gd_private_entry *)malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%i", -1);
    return -1;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));
  E->e->calculated = 0;

  /* Validate field code */
  E->field_type = entry->field_type;
  E->field = _GD_ValidateField(P, entry->field, D->standards, 1, &is_dot);

  if (E->field == entry->field) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, entry->field);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%i", -1);
    return -1;
  } else if (E->field == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    dreturn("%i", -1);
    return -1;
  }

  /* Set meta indicies */
  if (parent != NULL)
    E->e->n_meta = -1;

  /* Validate entry and add auxiliary data */
  switch(entry->field_type)
  {
    case GD_RAW_ENTRY:
      /* no METARAW fields allowed */
      if (parent != NULL) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_METARAW, NULL,
            entry->field_type, NULL);
        break;
      }

      /* check protection */
      if (D->fragment[entry->fragment_index].protection & GD_PROTECT_DATA) {
        _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[entry->fragment_index].cname);
        dreturn("%i", -1);
        return -1;
      }

      E->u.raw.type = entry->u.raw.type;
      E->e->u.raw.file[0].fp = E->e->u.raw.file[1].fp = -1;
      E->e->u.raw.file[0].encoding = GD_ENC_UNKNOWN;

      if ((E->e->u.raw.filebase = (char *)malloc(FILENAME_MAX)) == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        break;
      }
 
      if (D->fragment[E->fragment_index].sname)
        snprintf(E->e->u.raw.filebase, FILENAME_MAX, "%s/%s/%s", D->name,
            D->fragment[E->fragment_index].sname, E->field);
      else
        snprintf(E->e->u.raw.filebase, FILENAME_MAX, "%s/%s", D->name,
            E->field);

      if ((E->u.raw.spf = entry->u.raw.spf) == 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_SPF, NULL,
            entry->u.raw.spf, NULL);
      else if (E->u.raw.type & 0x40 || (E->e->u.raw.size =
            GD_SIZE(E->u.raw.type)) == 0)
        _GD_SetError(D, GD_E_BAD_TYPE, entry->u.raw.type, NULL, 0, NULL);
      else if (!_GD_Supports(D, E, GD_EF_TOUCH))
        ; /* error already set */
      else if (_GD_SetEncodedName(D, E->e->u.raw.file, E->e->u.raw.filebase, 0))
        ; /* error already set */
      else if ((*_gd_ef[E->e->u.raw.file[0].encoding].touch)(E->e->u.raw.file))
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      else if (D->fragment[E->fragment_index].ref_name == NULL) {
        /* This is the first raw field in this fragment */
        new_ref = strdup(E->field);
        if (new_ref == NULL)
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      }
      copy_scalar[0] = 1;
      break;
    case GD_LINCOM_ENTRY:
      E->u.lincom.n_fields = entry->u.lincom.n_fields;

      if (E->u.lincom.n_fields < 1 || E->u.lincom.n_fields > GD_MAX_LINCOM)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL,
            E->u.lincom.n_fields, NULL);
      else {
        if (entry->comp_scal) {
          int cs = 0;
          memcpy(E->u.lincom.cm, entry->u.lincom.cm, sizeof(double) * 2 *
              E->u.lincom.n_fields);
          memcpy(E->u.lincom.cb, entry->u.lincom.cb, sizeof(double) * 2 *
              E->u.lincom.n_fields);
          for (i = 0; i < E->u.lincom.n_fields; ++i) {
            E->u.lincom.m[i] = creal(E->u.lincom.cm[i]);
            E->u.lincom.b[i] = creal(E->u.lincom.cb[i]);
            if (cimag(E->u.lincom.cm[i]) || cimag(E->u.lincom.cb[i]))
              cs = 1;
          }
          E->comp_scal = cs;
        } else {
          memcpy(E->u.lincom.m, entry->u.lincom.m, sizeof(double) *
              E->u.lincom.n_fields);
          memcpy(E->u.lincom.b, entry->u.lincom.b, sizeof(double) *
              E->u.lincom.n_fields);
          for (i = 0; i < E->u.lincom.n_fields; ++i) {
            _gd_r2c(E->u.lincom.cm[i], E->u.lincom.m[i]);
            _gd_r2c(E->u.lincom.cb[i], E->u.lincom.b[i]);
          }
          E->comp_scal = 0;
        }

        for (i = 0; i < E->u.lincom.n_fields; ++i) {
          if ((E->in_fields[i] = strdup(entry->in_fields[i])) == NULL)
            _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          copy_scalar[i] = copy_scalar[i + GD_MAX_LINCOM] = 1;
        }
      }
      break;
    case GD_LINTERP_ENTRY:
      E->e->u.linterp.table_len = -1;

      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else if ((E->u.linterp.table = strdup(entry->u.linterp.table)) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else if ((E->in_fields[1] = strdup(entry->in_fields[1])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_RECIP_ENTRY:
      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

      copy_scalar[0] = 1;
      if (entry->comp_scal) {
        _gd_c2c(E->u.recip.cdividend, entry->u.recip.cdividend);
        E->u.recip.dividend = creal(E->u.recip.cdividend);
        E->comp_scal = (cimag(E->u.recip.cdividend) == 0) ? 0 : 1;
      } else {
        E->u.recip.dividend = entry->u.recip.dividend;
        _gd_r2c(E->u.recip.cdividend, E->u.recip.dividend);
        E->comp_scal = 0;
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      E->u.bit.numbits = entry->u.bit.numbits;
      E->u.bit.bitnum = entry->u.bit.bitnum;

      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else if (E->u.bit.numbits < 1)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NUMBITS, NULL, 
            entry->u.bit.numbits, NULL);
      else if (E->u.bit.bitnum < 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITNUM, NULL, 
            entry->u.bit.bitnum, NULL);
      else if (E->u.bit.bitnum + E->u.bit.numbits - 1 > 63)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITSIZE, NULL, 
            E->u.bit.bitnum + E->u.bit.numbits - 1, NULL);
      copy_scalar[0] = copy_scalar[1] = 1;
      break;
    case GD_PHASE_ENTRY:
      E->u.phase.shift = entry->u.phase.shift;

      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      copy_scalar[0] = 1;
      break;
    case GD_CONST_ENTRY:
      E->u.cons.type = entry->u.cons.type;

      if (E->u.cons.type & 0x40 || GD_SIZE(E->u.cons.type) == 0)
        _GD_SetError(D, GD_E_BAD_TYPE, E->u.cons.type, NULL, 0, NULL);
      break;
    case GD_STRING_ENTRY:
      E->e->u.string = strdup("");
      if (E->e->u.string == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_POLYNOM_ENTRY:
      E->u.polynom.poly_ord = entry->u.polynom.poly_ord;

      if (E->u.polynom.poly_ord < 1 || E->u.polynom.poly_ord > GD_MAX_POLYORD)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL,
            E->u.polynom.poly_ord, NULL);
      else {
        if (entry->comp_scal) {
          int cs = 0;
          memcpy(E->u.polynom.ca, entry->u.polynom.ca, sizeof(double) * 2 *
              (E->u.polynom.poly_ord + 1));
          for (i = 0; i <= E->u.polynom.poly_ord; ++i) {
            E->u.polynom.a[i] = creal(E->u.polynom.ca[i]);
            if (cimag(E->u.polynom.ca[i]))
              cs = 1;
          }
          E->comp_scal = cs;
        } else {
          memcpy(E->u.polynom.a, entry->u.polynom.a, sizeof(double) *
              (E->u.polynom.poly_ord + 1));
          for (i = 0; i <= E->u.polynom.poly_ord; ++i)
            _gd_r2c(E->u.polynom.ca[i], E->u.polynom.a[i]);
          E->comp_scal = 0;
        }

        if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      }

      for (i = 0; i < E->u.polynom.poly_ord; ++i)
        copy_scalar[i] = 1;
      break;
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      _GD_InternalError(D); /* We've already verrified field_type is valid */
      break;
  }

  /* copy scalars */
  for (i = 0; i <= GD_MAX_POLYORD; ++i) {
    if (!copy_scalar[i] || entry->scalar[i] == NULL)
      E->scalar[i] = NULL;
    else {
      E->scalar[i] = strdup(entry->scalar[i]);
      if (E->scalar[i] == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    }
  }

  if (D->error != GD_E_OK) {
    free(new_ref);
    _GD_FreeE(E, 1);
    dreturn("%i", -1);
    return -1;
  }

  new_list = realloc(D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));
  if (new_list == NULL) {
    free(new_ref);
    _GD_FreeE(E, 1);
    dreturn("%i", -1);
    return -1;
  }
  D->entry = (gd_entry_t **)new_list;

  if (is_dot) {
    new_list = realloc(D->dot_list, (D->n_dot + 1) * sizeof(gd_entry_t*));
    if (new_list == NULL) {
      free(new_ref);
      _GD_FreeE(E, 1);
      dreturn("%i", -1);
      return -1;
    }
    D->dot_list = (gd_entry_t **)new_list;
  }

  if (P) {
    void *ptr = realloc(P->e->p.meta_entry, (P->e->n_meta + 1) *
        sizeof(gd_entry_t*));
    if (ptr == NULL) {
      free(new_ref);
      _GD_FreeE(E, 1);
      dreturn("%i", -1);
      return -1;
    }

    /* From here on, nothing may fail */

    P->e->p.meta_entry = (gd_entry_t **)ptr;
    P->e->p.meta_entry[P->e->n_meta] = E;
    P->e->n_meta++;
    D->n_meta++;

  }

  if (E->field_type == GD_STRING_ENTRY) {
    if (P)
      P->e->n_meta_string++;
    else
      D->n_string++;
  } else if (E->field_type == GD_CONST_ENTRY) {
    if (P)
      P->e->n_meta_const++;
    else
      D->n_const++;
  } else if (E->field_type == GD_RAW_ENTRY) {
    if (new_ref != NULL) {
      /* This is the first raw field in this fragment; propagate it upwards */
      for (i = E->fragment_index; i != -1; i = D->fragment[i].parent) {
        if (D->fragment[i].ref_name == NULL) {
          D->fragment[i].ref_name = (char *)new_ref;
          D->fragment[i].modified = 1;
        } else
          break;
      }

      if (D->reference_field == NULL)
        D->reference_field = E;
    }
  }

  /* add the entry to the dot list, if needed */
  if (is_dot) {
    D->dot_list[D->n_dot++] = E;
    qsort(D->dot_list, D->n_dot, sizeof(gd_entry_t*), entry_cmp);
  }

  /* add the entry and resort the entry list */
  _GD_InsertSort(D, E, u);
  D->n_entries++;
  D->fragment[E->fragment_index].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  /* Invalidate the field lists */
  D->list_validity = 0;
  D->type_list_validity = 0;

  dreturn("%i", 0);
  return 0;
}

/* add a META field by parsing a field spec */
int gd_madd_spec(DIRFILE* D, const char* line, const char* parent) gd_nothrow
{
  char instring[GD_MAX_LINE_LENGTH];
  char outstring[GD_MAX_LINE_LENGTH];
  char *in_cols[MAX_IN_COLS];
  int n_cols;
  int me;
  gd_entry_t* E = NULL;

  dtrace("%p, \"%s\", \"%s\"", D, line, parent);

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

  E = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);
  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%i", -1);
    return -1;
  }

  me = E->fragment_index;

  /* check protection */
  if (D->fragment[me].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[me].cname);
    dreturn("%i", -1);
    return -1;
  }

  /* we do this to ensure line is not too long */
  strncpy(instring, line, GD_MAX_LINE_LENGTH - 1);
  instring[GD_MAX_LINE_LENGTH - 2] = '\0';

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols, "dirfile_madd_spec()",
      0, D->standards, D->flags & GD_PERMISSIVE);

  if (D->error) {
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Directive parsing is skipped -- The Field Spec parser will add the field */
  _GD_ParseFieldSpec(D, n_cols, in_cols, E, "dirfile_madd_spec()", 0, me,
      D->standards, 1, 1, 1);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  D->fragment[me].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;
  dreturn("%i", 0);
  return 0;
}

/* add a field by parsing a field spec */
int gd_add_spec(DIRFILE* D, const char* line, int fragment_index)
{
  char instring[GD_MAX_LINE_LENGTH];
  char outstring[GD_MAX_LINE_LENGTH];
  char *in_cols[MAX_IN_COLS];
  int n_cols;

  dtrace("%p, \"%s\", %i", D, line, fragment_index);

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

  /* check for include index out of range */
  if (fragment_index < 0 || fragment_index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check protection */
  if (D->fragment[fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  /* we do this to ensure line is not too long */
  strncpy(instring, line, GD_MAX_LINE_LENGTH - 1);
  instring[GD_MAX_LINE_LENGTH - 2] = '\0';

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols, "dirfile_add_spec()",
      0, D->standards, D->flags & GD_PERMISSIVE);

  if (D->error) {
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Directive parsing is skipped -- The Field Spec parser will add the field */
  _GD_ParseFieldSpec(D, n_cols, in_cols, NULL, "dirfile_add_spec()", 0, 
      fragment_index, D->standards, 1, 1, 1);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  D->fragment[fragment_index].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;
  dreturn("%i", 0);
  return 0;
}

int gd_add(DIRFILE* D, const gd_entry_t* entry)
{
  int ret;

  dtrace("%p, %p", D, entry);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Add(D, entry, NULL);

  dreturn("%i", ret);
  return ret;
}

/* add a RAW entry */
int gd_add_raw(DIRFILE* D, const char* field_code, gd_type_t data_type,
    gd_spf_t spf, int fragment_index)
{
  dtrace("%p, \"%s\", %x, %i %i", D, field_code, data_type, spf,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t R;
  memset(&R, 0, sizeof(gd_entry_t));
  R.field = (char *)field_code;
  R.field_type = GD_RAW_ENTRY;
  R.u.raw.spf = spf;
  R.u.raw.type = data_type;
  R.fragment_index = fragment_index;
  int error = _GD_Add(D, &R, NULL);

  dreturn("%i", error);
  return error;
}

/* add a LINCOM entry */
int gd_add_lincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const double* m, const double* b,
    int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", %i, %p, %p, %p, %i", D, field_code, n_fields, in_fields,
      m, b, fragment_index);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  memset(&L, 0, sizeof(gd_entry_t));
  L.field = (char *)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.u.lincom.n_fields = n_fields;
  L.comp_scal = 0;
  L.fragment_index = fragment_index;

  for (i = 0; i < n_fields; ++i) {
    L.in_fields[i] = (char *)in_fields[i];
    L.u.lincom.m[i] = m[i];
    L.u.lincom.b[i] = b[i];
  }
  int error = _GD_Add(D, &L, NULL);

  dreturn("%i", error);
  return error;
}

/* add a LINCOM entry with complex scalars */
int gd_add_clincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const GD_DCOMPLEXP(cm), const GD_DCOMPLEXP(cb),
    int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", %i, %p, %p, %p, %i", D, field_code, n_fields, in_fields,
      cm, cb, fragment_index);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  memset(&L, 0, sizeof(gd_entry_t));
  L.field = (char *)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.u.lincom.n_fields = n_fields;
  L.comp_scal = 1;
  L.fragment_index = fragment_index;

  for (i = 0; i < n_fields; ++i) {
    L.in_fields[i] = (char *)in_fields[i];
    _gd_ca2c(L.u.lincom.cm[i], cm, i);
    _gd_ca2c(L.u.lincom.cb[i], cb, i);
  }
  int error = _GD_Add(D, &L, NULL);

  dreturn("%i", error);
  return error;
}

/* add a LINTERP entry */
int gd_add_linterp(DIRFILE* D, const char* field_code, const char* in_field,
    const char* table, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field, table,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  memset(&L, 0, sizeof(gd_entry_t));
  L.field = (char *)field_code;
  L.field_type = GD_LINTERP_ENTRY;
  L.in_fields[0] = (char *)in_field;
  L.u.linterp.table = (char *)table;
  L.fragment_index = fragment_index;
  int error = _GD_Add(D, &L, NULL);

  dreturn("%i", error);
  return error;
}

/* add a BIT entry */
int gd_add_bit(DIRFILE* D, const char* field_code, const char* in_field,
    gd_bit_t bitnum, gd_bit_t numbits, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i, %i, %i\n", D, field_code, in_field, bitnum,
      numbits, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t B;
  memset(&B, 0, sizeof(gd_entry_t));
  B.field = (char *)field_code;
  B.field_type = GD_BIT_ENTRY;
  B.in_fields[0] = (char *)in_field;
  B.u.bit.bitnum = bitnum;
  B.u.bit.numbits = numbits;
  B.fragment_index = fragment_index;
  int error = _GD_Add(D, &B, NULL);

  dreturn("%i", error);
  return error;
}

/* add a SBIT entry */
int gd_add_sbit(DIRFILE* D, const char* field_code, const char* in_field,
    gd_bit_t bitnum, gd_bit_t numbits, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i, %i, %i\n", D, field_code, in_field, bitnum,
      numbits, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t B;
  memset(&B, 0, sizeof(gd_entry_t));
  B.field = (char *)field_code;
  B.field_type = GD_SBIT_ENTRY;
  B.in_fields[0] = (char *)in_field;
  B.u.bit.bitnum = bitnum;
  B.u.bit.numbits = numbits;
  B.fragment_index = fragment_index;
  int error = _GD_Add(D, &B, NULL);

  dreturn("%i", error);
  return error;
}

/* add a MULTIPLY entry */
int gd_add_multiply(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field1, in_field2,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t M;
  memset(&M, 0, sizeof(gd_entry_t));
  M.field = (char *)field_code;
  M.field_type = GD_MULTIPLY_ENTRY;
  M.in_fields[0] = (char *)in_field1;
  M.in_fields[1] = (char *)in_field2;
  M.fragment_index = fragment_index;
  int error = _GD_Add(D, &M, NULL);

  dreturn("%i", error);
  return error;
}

/* add a DIVIDE entry */
int gd_add_divide(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field1, in_field2,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_DIVIDE_ENTRY;
  E.in_fields[0] = (char *)in_field1;
  E.in_fields[1] = (char *)in_field2;
  E.fragment_index = fragment_index;
  int error = _GD_Add(D, &E, NULL);

  dreturn("%i", error);
  return error;
}

/* add a RECIP entry */
int gd_add_recip(DIRFILE* D, const char* field_code, const char* in_field,
    double dividend, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %g, %i", D, field_code, in_field, dividend,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  E.u.recip.dividend = dividend;
  E.comp_scal = 0;
  E.in_fields[0] = (char *)in_field;
  E.fragment_index = fragment_index;
  int error = _GD_Add(D, &E, NULL);

  dreturn("%i", error);
  return error;
}

#ifndef GD_NO_C99_API
int gd_add_crecip(DIRFILE* D, const char* field_code, const char* in_field,
    double complex cdividend, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %g;%g, %i", D, field_code, in_field,
      creal(cdividend), cimag(cdividend), fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  E.u.recip.cdividend = cdividend;
  E.comp_scal = 1;
  E.in_fields[0] = (char *)in_field;
  E.fragment_index = fragment_index;
  int error = _GD_Add(D, &E, NULL);

  dreturn("%i", error);
  return error;
}
#endif

int gd_add_crecip89(DIRFILE* D, const char* field_code, const char* in_field,
    const double cdividend[2], int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", [%g, %g], %i", D, field_code, in_field,
      cdividend[0], cdividend[1], fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  _gd_a2c(E.u.recip.cdividend, cdividend);
  E.comp_scal = 1;
  E.in_fields[0] = (char *)in_field;
  E.fragment_index = fragment_index;
  int error = _GD_Add(D, &E, NULL);

  dreturn("%i", error);
  return error;
}

/* add a POLYNOM entry */
int gd_add_polynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const double* a, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", %i, \"%s\", %p, %i", D, field_code, poly_ord, in_field,
      a, fragment_index);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (poly_ord < 1 || poly_ord > GD_MAX_LINCOM) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.u.polynom.poly_ord = poly_ord;
  E.fragment_index = fragment_index;
  E.comp_scal = 0;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i)
    E.u.polynom.a[i] = a[i];

  int error = _GD_Add(D, &E, NULL);

  dreturn("%i", error);
  return error;
}

int gd_add_cpolynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const GD_DCOMPLEXP(ca), int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", %i, \"%s\", %p, %i", D, field_code, poly_ord, in_field,
      ca, fragment_index);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (poly_ord < 1 || poly_ord > GD_MAX_LINCOM) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.u.polynom.poly_ord = poly_ord;
  E.fragment_index = fragment_index;
  E.comp_scal = 1;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i)
    _gd_ca2c(E.u.polynom.ca[i], ca, i);

  int error = _GD_Add(D, &E, NULL);

  dreturn("%i", error);
  return error;
}

/* add a PHASE entry */
int gd_add_phase(DIRFILE* D, const char* field_code, const char* in_field,
    gd_shift_t shift, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %lli, %i", D, field_code, in_field,
      (long long)shift, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t P;
  memset(&P, 0, sizeof(gd_entry_t));
  P.field = (char *)field_code;
  P.field_type = GD_PHASE_ENTRY;
  P.in_fields[0] = (char *)in_field;
  P.u.phase.shift = shift;
  P.fragment_index = fragment_index;
  int error = _GD_Add(D, &P, NULL);

  dreturn("%i", error);
  return error;
}

/* add a STRING entry */
int gd_add_string(DIRFILE* D, const char* field_code, const char* value,
    int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i", D, field_code, value, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t S;
  memset(&S, 0, sizeof(gd_entry_t));
  S.field = (char *)field_code;
  S.field_type = GD_STRING_ENTRY;
  S.fragment_index = fragment_index;
  int error = _GD_Add(D, &S, NULL);

  /* Actually store the string, now */
  if (!error) {
    entry = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, 0, 0, 0, GD_NULL, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

/* add a CONST entry */
int gd_add_const(DIRFILE* D, const char* field_code, gd_type_t const_type,
    gd_type_t data_type, const void* value, int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", 0x%x, 0x%x, %p, %i", D, field_code, const_type, data_type,
      value, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t C;
  memset(&C, 0, sizeof(gd_entry_t));
  C.field = (char *)field_code;
  C.field_type = GD_CONST_ENTRY;
  C.u.cons.type = const_type;
  C.fragment_index = fragment_index;
  int error = _GD_Add(D, &C, NULL);

  /* Actually store the constant, now */
  if (!error) {
    entry = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, 0, 0, 0, data_type, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

int gd_madd(DIRFILE* D, const gd_entry_t* entry, const char* parent) gd_nothrow
{
  int ret;

  dtrace("%p, %p, \"%s\"", D, entry, parent);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Add(D, entry, parent);

  dreturn("%i", ret);
  return ret;
}

/* add a META LINCOM entry */
int gd_madd_lincom(DIRFILE* D, const char* parent, const char* field_code,
    int n_fields, const char** in_fields, const double* m, const double* b)
gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i, %p, %p, %p", D, field_code, parent,
      n_fields, in_fields, m, b);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  L.field = (char *)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.u.lincom.n_fields = n_fields;
  L.comp_scal = 0;
  L.fragment_index = 0;

  for (i = 0; i < n_fields; ++i) {
    L.in_fields[i] = (char *)in_fields[i];
    L.u.lincom.m[i] = m[i];
    L.u.lincom.b[i] = b[i];
    L.scalar[i] = NULL;
    L.scalar[i + GD_MAX_LINCOM] = NULL;
  }
  int error = _GD_Add(D, &L, parent);

  dreturn("%i", error);
  return error;
}

/* add a META LINCOM entry, with complex scalaras */
int gd_madd_clincom(DIRFILE* D, const char* parent, const char* field_code,
    int n_fields, const char** in_fields, const GD_DCOMPLEXP(cm),
    const GD_DCOMPLEXP(cb)) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i, %p, %p, %p", D, field_code, parent,
      n_fields, in_fields, cm, cb);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  L.field = (char *)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.u.lincom.n_fields = n_fields;
  L.comp_scal = 1;
  L.fragment_index = 0;

  for (i = 0; i < n_fields; ++i) {
    L.in_fields[i] = (char *)in_fields[i];
    _gd_ca2c(L.u.lincom.cm[i], cm, i);
    _gd_ca2c(L.u.lincom.cb[i], cb, i);
    L.scalar[i] = NULL;
    L.scalar[i + GD_MAX_LINCOM] = NULL;
  }
  int error = _GD_Add(D, &L, parent);

  dreturn("%i", error);
  return error;
}

/* add a META LINTERP entry */
int gd_madd_linterp(DIRFILE* D, const char* parent,
    const char* field_code, const char* in_field, const char* table) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, field_code, parent, in_field,
      table);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  L.field = (char *)field_code;
  L.field_type = GD_LINTERP_ENTRY;
  L.in_fields[0] = (char *)in_field;
  L.u.linterp.table = (char *)table;
  L.fragment_index = 0;
  int error = _GD_Add(D, &L, parent);

  dreturn("%i", error);
  return error;
}

/* add a META BIT entry */
int gd_madd_bit(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, gd_bit_t bitnum, gd_bit_t numbits) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %in", D, field_code, parent, in_field,
      bitnum, numbits);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t B;
  B.field = (char *)field_code;
  B.field_type = GD_BIT_ENTRY;
  B.in_fields[0] = (char *)in_field;
  B.u.bit.bitnum = bitnum;
  B.u.bit.numbits = numbits;
  B.fragment_index = 0;
  B.scalar[0] = B.scalar[1] = NULL;
  int error = _GD_Add(D, &B, parent);

  dreturn("%i", error);
  return error;
}

/* add a META SBIT entry */
int gd_madd_sbit(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, gd_bit_t bitnum, gd_bit_t numbits) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %in", D, field_code, parent, in_field,
      bitnum, numbits);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t B;
  B.field = (char *)field_code;
  B.field_type = GD_SBIT_ENTRY;
  B.in_fields[0] = (char *)in_field;
  B.u.bit.bitnum = bitnum;
  B.u.bit.numbits = numbits;
  B.fragment_index = 0;
  B.scalar[0] = B.scalar[1] = NULL;
  int error = _GD_Add(D, &B, parent);

  dreturn("%i", error);
  return error;
}

/* add a META MULTIPLY entry */
int gd_madd_multiply(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field1, const char* in_field2) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, field_code, parent,
      in_field1, in_field2);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t M;
  M.field = (char *)field_code;
  M.field_type = GD_MULTIPLY_ENTRY;
  M.in_fields[0] = (char *)in_field1;
  M.in_fields[1] = (char *)in_field2;
  M.fragment_index = 0;
  int error = _GD_Add(D, &M, parent);

  dreturn("%i", error);
  return error;
}

/* add a META PHASE entry */
int gd_madd_phase(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, gd_shift_t shift) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %lli", D, field_code, parent, in_field,
      (long long)shift);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t P;
  P.field = (char *)field_code;
  P.field_type = GD_PHASE_ENTRY;
  P.in_fields[0] = (char *)in_field;
  P.u.phase.shift = shift;
  P.fragment_index = 0;
  P.scalar[0] = NULL;
  int error = _GD_Add(D, &P, parent);

  dreturn("%i", error);
  return error;
}

/* add a META POLYNOM entry */
int gd_madd_polynom(DIRFILE* D, const char* parent, const char* field_code,
    int poly_ord, const char* in_field, const double* a) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i, \"%s\", %p", D, field_code, parent, poly_ord,
      in_field, a);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (poly_ord < 1 || poly_ord > GD_MAX_POLYORD) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.u.polynom.poly_ord = poly_ord;
  E.fragment_index = 0;
  E.comp_scal = 0;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i) {
    E.u.polynom.a[i] = a[i];
    E.scalar[i] = NULL;
  }

  int error = _GD_Add(D, &E, parent);

  dreturn("%i", error);
  return error;
}

/* add a META POLYNOM entry */
int gd_madd_cpolynom(DIRFILE* D, const char* parent, const char* field_code,
    int poly_ord, const char* in_field, const GD_DCOMPLEXP(ca)) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i, \"%s\", %p", D, field_code, parent, poly_ord,
      in_field, ca);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (poly_ord < 1 || poly_ord > GD_MAX_POLYORD) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.u.polynom.poly_ord = poly_ord;
  E.fragment_index = 0;
  E.comp_scal = 1;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i) {
    _gd_ca2c(E.u.polynom.ca[i], ca, i);
    E.scalar[i] = NULL;
  }

  int error = _GD_Add(D, &E, parent);

  dreturn("%i", error);
  return error;
}

/* add a META DIVIDE entry */
int gd_madd_divide(DIRFILE* D, const char *parent, const char* field_code,
    const char* in_field1, const char* in_field2) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, parent, field_code, in_field1,
      in_field2);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_DIVIDE_ENTRY;
  E.in_fields[0] = (char *)in_field1;
  E.in_fields[1] = (char *)in_field2;
  int error = _GD_Add(D, &E, parent);

  dreturn("%i", error);
  return error;
}

/* add a RECIP entry */
int gd_madd_recip(DIRFILE* D, const char *parent, const char* field_code,
    const char* in_field, double dividend) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %g", D, parent, field_code, in_field,
      dividend);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  E.u.recip.dividend = dividend;
  E.comp_scal = 0;
  E.in_fields[0] = (char *)in_field;
  int error = _GD_Add(D, &E, parent);

  dreturn("%i", error);
  return error;
}

#ifndef GD_NO_C99_API
int gd_madd_crecip(DIRFILE* D, const char *parent, const char* field_code, const
    char* in_field, double complex cdividend)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %g;%g", D, parent, field_code, in_field,
      creal(cdividend), cimag(cdividend));

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  E.u.recip.cdividend = cdividend;
  E.comp_scal = 1;
  E.in_fields[0] = (char *)in_field;
  int error = _GD_Add(D, &E, parent);

  dreturn("%i", error);
  return error;
}
#endif

int gd_madd_crecip89(DIRFILE* D, const char *parent, const char* field_code,
    const char* in_field, const double cdividend[2]) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", [%g, %g]", D, parent, field_code,
      in_field, cdividend[0], cdividend[1]);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t E;
  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  _gd_a2c(E.u.recip.cdividend, cdividend);
  E.comp_scal = 1;
  E.in_fields[0] = (char *)in_field;
  int error = _GD_Add(D, &E, parent);

  dreturn("%i", error);
  return error;
}

/* add a META STRING entry */
int gd_madd_string(DIRFILE* D, const char* parent,
    const char* field_code, const char* value) gd_nothrow
{
  char buffer[GD_MAX_LINE_LENGTH];
  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, parent, field_code, value);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t S;
  S.field = (char *)field_code;
  S.field_type = GD_STRING_ENTRY;
  S.fragment_index = 0;
  int error = _GD_Add(D, &S, parent);

  /* Actually store the string, now */
  if (!error) {
    snprintf(buffer, GD_MAX_LINE_LENGTH, "%s/%s", parent, field_code);
    entry = _GD_FindField(D, buffer, D->entry, D->n_entries, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, 0, 0, 0, GD_NULL, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

/* add a META CONST entry */
int gd_madd_const(DIRFILE* D, const char* parent, const char* field_code,
    gd_type_t const_type, gd_type_t data_type, const void* value) gd_nothrow
{
  char buffer[GD_MAX_LINE_LENGTH];
  dtrace("%p, \"%s\", \"%s\", 0x%x, 0x%x, %p", D, parent, field_code,
      const_type, data_type, value);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t C;
  C.field = (char *)field_code;
  C.field_type = GD_CONST_ENTRY;
  C.u.cons.type = const_type;
  C.fragment_index = 0;
  int error = _GD_Add(D, &C, parent);

  /* Actually store the constant, now */
  if (!error) {
    snprintf(buffer, GD_MAX_LINE_LENGTH, "%s/%s", parent, field_code);
    entry = _GD_FindField(D, buffer, D->entry, D->n_entries, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, 0, 0, 0, data_type, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}
