/* Copyright (C) 2008-2016 D. V. Wiebe
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

int _GD_InvalidEntype(gd_entype_t t) {
  dtrace("0x%X", t);

  if (((t & GD_SCALAR_ENTRY_BIT) == 0 &&
        (t < GD_RAW_ENTRY || t > GD_SINDIR_ENTRY))
      || ((t & GD_SCALAR_ENTRY_BIT) &&
        (t < GD_CONST_ENTRY || t > GD_SARRAY_ENTRY)))
  {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

int _GD_BadType(int standards, gd_type_t type)
{
  int bad = 1;
  const int size = GD_SIZE(type);
  const int bits = type - size;

  dtrace("%i, 0x%X", standards, type);

  if (bits == 0 && (size == 1 || size == 2 || size == 4 ||
        (standards >= 5 && size == 8)))
  {
    bad = 0;
  } else if (bits == GD_SIGNED && (size == 2 || size == 4 ||
        (standards >= 5 && (size == 1 || size == 8))))
  {
    bad = 0;
  } else if (bits == GD_IEEE754 && (size == 4 || size == 8))
    bad = 0;
  else if (standards >= 8 && bits == GD_COMPLEX && (size == 8 || size == 16))
    bad = 0;

  dreturn("%i", bad);
  return bad;
}

static gd_entry_t *_GD_FixName(DIRFILE *restrict D, char **restrict buffer,
    size_t *restrict len, const char *name, int frag, size_t *restrict offset)
{
  gd_entry_t *P;
  char *ptr;
  struct parser_state p;
  const unsigned flags =
    GD_CO_NAME | GD_CO_ERROR | ((D->standards <= 5) ? GD_CO_EARLY : 0);

  dtrace("%p, %p, %p, \"%s\", %i, %p", D, buffer, len, name, frag, offset);

  /* Check prefix and suffix */
  if (_GD_CheckCodeAffixes(D, name, frag, flags)) {
    dreturn("%p", NULL);
    return NULL;
  }
    
  *buffer = ptr = _GD_Malloc(D, *len + 1);

  if (ptr == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  memcpy(ptr, name, *len + 1);
  _GD_SimpleParserInit(D, NULL, &p);
  P = _GD_CheckParent(D, &p, &ptr, len, -1);

  if (P) {
    char *name_out;

    /* make the new name -- this may be different because P may have been
     * dealiased */
    *offset = P->e->len + 1;
    name_out = _GD_Malloc(D, *offset + *len + 1);
    if (name_out == NULL) {
      free(*buffer);
      dreturn("%p", NULL);
      return NULL;
    }

    memcpy(name_out, P->field, *offset - 1);
    name_out[*offset - 1] = '/';
    memcpy(name_out + *offset, ptr, *len + 1); /* including trailing NUL */
    *len += P->e->len + 1;
    free(*buffer);
    *buffer = name_out;
  } else
    *offset = 0;

  dreturn("%p (\"%s\", %" PRIuSIZE ")", P, *buffer, *offset);
  return P;
}

/* copy scalar entries from the user's entry; returns a mask of
 * initialised scalars */
static unsigned _GD_CopyScalars(DIRFILE *restrict D,
    gd_entry_t *restrict E, const gd_entry_t *restrict entry, unsigned mask)
{
  unsigned mask_out = 0;
  const unsigned flags = GD_CO_ERROR | ((D->standards <= 5) ? GD_CO_EARLY : 0);
  int i;

  dtrace("%p, %p, %p, 0x%X", D, E, entry, mask);

  /* copy scalars */
  for (i = 0; i <= GD_MAX_POLYORD; ++i) {
    if (!(mask & (1 << i)) || entry->scalar[i] == NULL ||
        entry->scalar[i][0] == '\0')
    {
      E->scalar[i] = NULL;
    } else {
      /* check for correct affixes */
      if (_GD_CheckCodeAffixes(D, entry->scalar[i], entry->fragment_index,
            flags))
      {
        break;
      }

      /* when using early Standards, reject ambiguous field codes */
      if (entry->scalar_ind[i] == -1 && !(D->flags & GD_NOSTANDARD) &&
          D->standards <= 7)
      {
        if (_GD_TokToNum(entry->scalar[i], D->standards, 1, NULL, NULL, NULL,
              NULL) != -1)
        {
          _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_AMBIGUOUS, NULL, 0,
              entry->scalar[i]);
          break;
        }
      }

      E->scalar[i] = _GD_Strdup(D, entry->scalar[i]);
      E->scalar_ind[i] = entry->scalar_ind[i];
      mask_out |= (1 << i);
    }
  }

  dreturn("0x%X", mask_out);
  return mask_out;
}

/* add an entry - returns the added entry on success.  If the new entry
 * is scalar and init_scalar is non-zero then the scalar is intialised
 * with the default value (0 or ""). */
static gd_entry_t *_GD_Add(DIRFILE *restrict D,
    const gd_entry_t *restrict entry, const char *restrict parent,
    int init_scalar)
{
  char *name;
  int i;
  void *new_list;
  void *new_ref = NULL;
  size_t z, subfield_offs, len =  strlen(entry->field);
  unsigned int u;
  unsigned mask;
  gd_entry_t *E;
  gd_entry_t *P = NULL;

  /* Flags used by both StripCode and CheckCodeAffixes */
  const unsigned flags = GD_CO_NSALL | GD_CO_ASSERT | GD_CO_ERROR |
    ((D->standards <= 5) ? GD_CO_EARLY : 0);

  dtrace("%p, %p, \"%s\", %i", D, entry, parent, init_scalar);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* check for include index out of range */
  if (parent == NULL && (entry->fragment_index < 0 ||
        entry->fragment_index >= D->n_fragment))
  {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, entry->fragment_index, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* check for bad field type */
  if (_GD_InvalidEntype(entry->field_type)) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, NULL, entry->field_type,
        NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* check parent */
  if (parent != NULL) {
    P = _GD_FindField(D, parent, strlen(parent), D->entry, D->n_entries, 0,
        NULL);
    if (P == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, parent);
      dreturn("%p", NULL);
      return NULL;
    }

    /* make sure it's not a meta field already or an alias */
    if (P->e->n_meta == -1 || P->field_type == GD_ALIAS_ENTRY) {
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);
      dreturn("%p", NULL);
      return NULL;
    }

    /* make name */
    subfield_offs = P->e->len + 1;
    name = _GD_Malloc(D, subfield_offs + len + 1);

    if (name == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }

    memcpy(name, parent, P->e->len);
    name[subfield_offs - 1] = '/';
    memcpy(name + subfield_offs, entry->field, len + 1);
    len += subfield_offs;
  } else {
    /* this will check for affixes and take care of detecting Barth-style
     * metafield definitions */
    P = _GD_FixName(D, &name, &len, entry->field, entry->fragment_index,
        &subfield_offs);

    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }
  }

  /* check for duplicate field */
  if (_GD_FindField(D, name, len, D->entry, D->n_entries, 0, &u)) {
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, name);
    free(name);
    dreturn("%p", NULL);
    return NULL;
  }

  /* New entry */
  E = _GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    free(name);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));
  if (P)
    E->fragment_index = P->fragment_index;
  else
    E->fragment_index = entry->fragment_index;

  /* check protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    free(E);
    free(name);
    dreturn("%p", NULL);
    return NULL;
  }

  E->e = _GD_Malloc(D, sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    free(name);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = entry->field_type;
  E->field = name;
  E->e->len = len;

  /* GD_EN_HIDDEN is the only flag we honour in the input entry */
  E->flags = entry->flags & GD_EN_HIDDEN;

  /* Figure out the length of the attached namespace in the supplied field
   * name.  If P is non-NULL, then there can't be a namespace because
   * we're only checking the subfield name */
  if (D->standards >= 10 && P == NULL) {
    const char *dot, *slash;
    _GD_SlashDot(E->field, strlen(E->field), GD_CO_NAME, &dot, &slash);
    if (dot)
      z = dot - E->field + 1;
    else
      z = 0;
  } else
    z = 0;

  /* Check */
  if (_GD_ValidateField(E->field + subfield_offs, z, D->standards, 1,
        GD_VF_NAME))
  {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, entry->field);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->standards <= 5)
    E->flags |= GD_EN_EARLY;

  /* Set meta indices */
  if (P != NULL)
    E->e->n_meta = -1;

  /* Validate entry and add auxiliary data */
  switch(entry->field_type) {
    case GD_RAW_ENTRY:
      /* no METARAW fields allowed */
      if (P != NULL) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_METARAW, NULL,
            entry->field_type, NULL);
        break;
      }

      /* check protection */
      if (D->fragment[entry->fragment_index].protection & GD_PROTECT_DATA) {
        _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[entry->fragment_index].cname);
        break;
      }

      E->EN(raw,data_type) = entry->EN(raw,data_type);
      E->e->u.raw.file[0].idata = E->e->u.raw.file[1].idata = -1;
      E->e->u.raw.file[0].subenc = GD_ENC_UNKNOWN;

      E->e->u.raw.filebase = _GD_StripCode(D, entry->fragment_index, E->field,
          flags);

      mask = _GD_CopyScalars(D, E, entry, 0x1);

      if (!(mask & 1) && (E->EN(raw,spf) = entry->EN(raw,spf)) == 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_SPF, NULL, 0, NULL);
      else if (_GD_BadType(D->standards, E->EN(raw,data_type)))
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, entry->EN(raw,data_type),
            NULL);
      else if (_GD_InitRawIO(D, E, NULL, -1, NULL, 0,
            GD_FILE_WRITE | GD_FILE_TOUCH, _GD_FileSwapBytes(D, E)))
      {
        ;
      } else if (D->fragment[E->fragment_index].ref_name == NULL) {
        /* This is the first raw field in this fragment */
        new_ref = _GD_Strdup(D, E->field);
      }
      break;
    case GD_LINCOM_ENTRY:
      E->EN(lincom,n_fields) = entry->EN(lincom,n_fields);

      if (E->EN(lincom,n_fields) < 1 || E->EN(lincom,n_fields) > GD_MAX_LINCOM)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL,
            E->EN(lincom,n_fields), NULL);
      
      for (i = 0; i < E->EN(lincom,n_fields); ++i)
        _GD_CheckCodeAffixes(D, entry->in_fields[i], entry->fragment_index,
            flags);

      if (D->error)
        break;

      _GD_CopyScalars(D, E, entry, 9 * ((1 << E->EN(lincom,n_fields)) - 1));

      if (entry->flags & GD_EN_COMPSCAL) {
        unsigned cs = 0;
        memcpy(E->EN(lincom,cm), entry->EN(lincom,cm), sizeof(double) * 2 *
            E->EN(lincom,n_fields));
        memcpy(E->EN(lincom,cb), entry->EN(lincom,cb), sizeof(double) * 2 *
            E->EN(lincom,n_fields));
        for (i = 0; i < E->EN(lincom,n_fields); ++i) {
          E->EN(lincom,m)[i] = creal(E->EN(lincom,cm)[i]);
          E->EN(lincom,b)[i] = creal(E->EN(lincom,cb)[i]);
          if (cimag(E->EN(lincom,cm)[i]) || cimag(E->EN(lincom,cb)[i]))
            cs = GD_EN_COMPSCAL;
        }
        E->flags |= cs;
      } else {
        memcpy(E->EN(lincom,m), entry->EN(lincom,m), sizeof(double) *
            E->EN(lincom,n_fields));
        memcpy(E->EN(lincom,b), entry->EN(lincom,b), sizeof(double) *
            E->EN(lincom,n_fields));
        for (i = 0; i < E->EN(lincom,n_fields); ++i) {
          gd_rs2cs_(E->EN(lincom,cm)[i], E->EN(lincom,m)[i]);
          gd_rs2cs_(E->EN(lincom,cb)[i], E->EN(lincom,b)[i]);
        }
      }

      for (i = 0; i < E->EN(lincom,n_fields); ++i)
        E->in_fields[i] = _GD_Strdup(D, entry->in_fields[i]);
      break;
    case GD_LINTERP_ENTRY:
      E->e->u.linterp.table_len = -1;

      if (_GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags))
      {
        break;
      }

      E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);
      E->EN(linterp,table) = _GD_Strdup(D, entry->EN(linterp,table));
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      if (!_GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags) && !_GD_CheckCodeAffixes(D, entry->in_fields[1],
              entry->fragment_index, flags))
      {
        E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);
        E->in_fields[1] = _GD_Strdup(D, entry->in_fields[1]);
      }
      break;
    case GD_RECIP_ENTRY:
      if (_GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags))
      {
        break;
      }

      E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);

      _GD_CopyScalars(D, E, entry, 0x1);

      if (entry->flags & GD_EN_COMPSCAL) {
        gd_cs2cs_(E->EN(recip,cdividend), entry->EN(recip,cdividend));
        E->EN(recip,dividend) = creal(E->EN(recip,cdividend));
        if (cimag(E->EN(recip,cdividend)) != 0)
          E->flags |= GD_EN_COMPSCAL;
      } else {
        E->EN(recip,dividend) = entry->EN(recip,dividend);
        gd_rs2cs_(E->EN(recip,cdividend), E->EN(recip,dividend));
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      E->EN(bit,numbits) = entry->EN(bit,numbits);
      E->EN(bit,bitnum) = entry->EN(bit,bitnum);

      if (_GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags))
      {
        break;
      }

      E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);

      mask = _GD_CopyScalars(D, E, entry, 0x3);

      if (!(mask & 2) && E->EN(bit,numbits) < 1)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NUMBITS, NULL,
            entry->EN(bit,numbits), NULL);
      else if (!(mask & 1) && E->EN(bit,bitnum) < 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_BITNUM, NULL,
            entry->EN(bit,bitnum), NULL);
      else if (!(mask & 3) && E->EN(bit,bitnum) + E->EN(bit,numbits) - 1 > 63)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_BITSIZE, NULL,
            E->EN(bit,bitnum) + E->EN(bit,numbits) - 1, NULL);
      break;
    case GD_PHASE_ENTRY:
      E->EN(phase,shift) = entry->EN(phase,shift);

      if (_GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags))
      {
        break;
      }

      _GD_CopyScalars(D, E, entry, 0x1);

      E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);
      break;
    case GD_WINDOW_ENTRY:
      E->EN(window,windop) = entry->EN(window,windop);
      E->EN(window,threshold) = entry->EN(window,threshold);

      if (_GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags) || _GD_CheckCodeAffixes(D, entry->in_fields[1],
              entry->fragment_index, flags))
      {
        break;
      }

      if (_GD_BadWindop(E->EN(window,windop))) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_WINDOP, NULL,
            entry->EN(window,windop), NULL);
        break;
      }

      _GD_CopyScalars(D, E, entry, 0x1);

      E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);
      E->in_fields[1] = _GD_Strdup(D, entry->in_fields[1]);
      break;
    case GD_MPLEX_ENTRY:
      E->EN(mplex,count_val) = entry->EN(mplex,count_val);
      E->EN(mplex,period) = entry->EN(mplex,period);

      if (_GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags) || _GD_CheckCodeAffixes(D, entry->in_fields[1],
              entry->fragment_index, flags))
      {
        break;
      }

      E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);
      E->in_fields[1] = _GD_Strdup(D, entry->in_fields[1]);
      E->e->u.mplex.type = GD_NULL;

      mask = _GD_CopyScalars(D, E, entry, 0x3);

      if (!(mask & 2) && entry->EN(mplex,period) < 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_PERIOD, NULL,
            entry->EN(mplex,period), NULL);

      break;
    case GD_CONST_ENTRY:
      E->EN(scalar,const_type) = entry->EN(scalar,const_type);
      E->EN(scalar,array_len) = -1;

      if (_GD_BadType(D->standards, E->EN(scalar,const_type)))
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, E->EN(scalar,const_type), NULL);
      else {
        size_t size = GD_SIZE(_GD_ConstType(D, E->EN(scalar,const_type)));
        E->e->u.scalar.d = _GD_Malloc(D, size);
        if (init_scalar && E->e->u.scalar.d)
          memset(E->e->u.scalar.d, 0, size);
      }
      break;
    case GD_CARRAY_ENTRY:
      E->EN(scalar,const_type) = entry->EN(scalar,const_type);
      E->EN(scalar,array_len) = entry->EN(scalar,array_len);

      if (_GD_BadType(D->standards, E->EN(scalar,const_type)))
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, E->EN(scalar,const_type), NULL);
      else {
        size_t size = GD_SIZE(_GD_ConstType(D, E->EN(scalar,const_type))) *
          E->EN(scalar,array_len);
        E->e->u.scalar.d = _GD_Malloc(D, size);
        if (init_scalar && E->e->u.scalar.d)
          memset(E->e->u.scalar.d, 0, size);
      }
      break;
    case GD_SARRAY_ENTRY:
      E->EN(scalar,array_len) = entry->EN(scalar,array_len);

      if (init_scalar) {
        E->e->u.scalar.d = _GD_Malloc(D,
            sizeof(const char *) * E->EN(scalar,array_len));
        if (E->e->u.scalar.d)
          for (z = 0; z < E->EN(scalar,array_len); ++z)
            ((const char**)E->e->u.scalar.d)[z] = _GD_Strdup(D, "");
      }
      break;
    case GD_STRING_ENTRY:
      if (init_scalar)
        E->e->u.string = _GD_Strdup(D, "");
      break;
    case GD_POLYNOM_ENTRY:
      if (entry->EN(polynom,poly_ord) < 1 || entry->EN(polynom,poly_ord) >
          GD_MAX_POLYORD)
      {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, NULL,
            entry->EN(polynom,poly_ord), NULL);
      } else
        _GD_CheckCodeAffixes(D, entry->in_fields[0], entry->fragment_index,
            flags);

      if (D->error)
        break;

      E->EN(polynom,poly_ord) = entry->EN(polynom,poly_ord);

      _GD_CopyScalars(D, E, entry, (1 << (E->EN(polynom,poly_ord) + 1)) - 1);

      if (entry->flags & GD_EN_COMPSCAL) {
        unsigned cs = 0;
        memcpy(E->EN(polynom,ca), entry->EN(polynom,ca), sizeof(double) * 2 *
            (E->EN(polynom,poly_ord) + 1));
        for (i = 0; i <= E->EN(polynom,poly_ord); ++i) {
          E->EN(polynom,a)[i] = creal(E->EN(polynom,ca)[i]);
          if (cimag(E->EN(polynom,ca)[i]))
            cs = GD_EN_COMPSCAL;
        }
        E->flags |= cs;
      } else {
        memcpy(E->EN(polynom,a), entry->EN(polynom,a), sizeof(double) *
            (E->EN(polynom,poly_ord) + 1));
        for (i = 0; i <= E->EN(polynom,poly_ord); ++i)
          gd_rs2cs_(E->EN(polynom,ca)[i], E->EN(polynom,a)[i]);
      }

      E->in_fields[0] = _GD_Strdup(D, entry->in_fields[0]);
      break;
    case GD_ALIAS_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      _GD_InternalError(D); /* We've already verrified field_type is valid */
      break;
  }

  if (D->error != GD_E_OK) {
    free(new_ref);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  new_list = _GD_Realloc(D, D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));
  if (new_list == NULL) {
    free(new_ref);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }
  D->entry = (gd_entry_t **)new_list;

  if (P) {
    void *ptr = _GD_Realloc(D, P->e->p.meta_entry, (P->e->n_meta + 1) *
        sizeof(gd_entry_t*));
    if (ptr == NULL) {
      free(new_ref);
      _GD_FreeE(D, E, 1);
      dreturn("%p", NULL);
      return NULL;
    }

    /* From here on, nothing may fail */

    P->e->p.meta_entry = (gd_entry_t **)ptr;
    P->e->p.meta_entry[P->e->n_meta] = E;
    P->e->n_meta++;

    P->e->fl.value_list_validity = 0;
    P->e->fl.entry_list_validity = 0;
  } else {
    /* Invalidate the field lists */
    D->fl.value_list_validity = 0;
    D->fl.entry_list_validity = 0;
  }


  if (E->field_type == GD_RAW_ENTRY) {
    if (new_ref != NULL) {
      /* This is the first raw field in this fragment; propagate it upwards */
      for (i = E->fragment_index; i != -1; i = D->fragment[i].parent) {
        if (D->fragment[i].ref_name == NULL) {
          D->fragment[i].ref_name = strdup((const char*)new_ref);
          D->fragment[i].modified = 1;
        } else
          break;
      }

      if (D->reference_field == NULL)
        D->reference_field = E;
      free(new_ref);
    }
  }

  /* add the entry and resort the entry list */
  _GD_InsertSort(D, E, u);
  D->n_entries++;
  D->fragment[E->fragment_index].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  /* Update aliases - no reason to do a reset: all we did was add a field */
  _GD_UpdateAliases(D, 0);

  dreturn("%p", E);
  return E;
}

static int _GD_AddSpec(DIRFILE* D, const char* line, const char* parent,
    int me, const char *name) gd_nothrow
{
  char *outstring = NULL;
  char *in_cols[MAX_IN_COLS];
  const char *tok_pos = NULL;
  int n_cols;
  gd_entry_t* E = NULL;
  struct parser_state p;

  dtrace("%p, \"%s\", \"%s\", %i, \"%s\"", D, line, parent, me, name);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);

  if (parent) {
    /* Find parent -- we don't do code mungeing here because we don't know
     * which fragment this is yet.  */
    E = _GD_FindField(D, parent, strlen(parent), D->entry, D->n_entries, 0,
        NULL);
    if (E == NULL)
      GD_SET_RETURN_ERROR(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, parent);
    else
      me = E->fragment_index;
  } else if (me < 0 || me >= D->n_fragment) /* fragment index out of range */
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, me, NULL);

  /* check protection */
  if (D->fragment[me].protection & GD_PROTECT_FORMAT)
    GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[me].cname);
  
  /* start parsing */
  _GD_SimpleParserInit(D, name, &p);
  n_cols = _GD_Tokenise(D, &p, line, &outstring, &tok_pos, MAX_IN_COLS,
      in_cols);

  if (!D->error) {
    if (n_cols < 2) /* Sanity check */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, name, 0, NULL);
    else /* The Field Spec parser will add the field */
      _GD_ParseFieldSpec(D, &p, n_cols, in_cols, strlen(in_cols[0]), E, me, 1,
          1, &outstring, tok_pos);
  }

  free(outstring);

  if (D->error)
    GD_RETURN_ERROR(D);

  /* Update aliases */
  _GD_UpdateAliases(D, 0);

  D->fragment[me].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;
  dreturn("%i", GD_E_OK);
  return GD_E_OK;
}

int gd_madd_spec(DIRFILE* D, const char* line, const char *parent)
{
  dtrace("%p, \"%s\", \"%s\"", D, line, parent);

  _GD_AddSpec(D, line, parent, 0, "gd_madd_spec()");

  GD_RETURN_ERROR(D);
}

/* add a field by parsing a field spec */
int gd_add_spec(DIRFILE* D, const char* line, int fragment_index)
{
  dtrace("%p, \"%s\", %i", D, line, fragment_index);

  _GD_AddSpec(D, line, NULL, fragment_index, "gd_add_spec()");

  GD_RETURN_ERROR(D);
}

int gd_add(DIRFILE* D, const gd_entry_t* entry)
{
  dtrace("%p, %p", D, entry);

  GD_RETURN_ERR_IF_INVALID(D);

  _GD_Add(D, entry, NULL, 1);

  GD_RETURN_ERROR(D);
}

/* add a RAW entry */
int gd_add_raw(DIRFILE* D, const char* field_code, gd_type_t data_type,
    unsigned int spf, int fragment_index)
{
  gd_entry_t E;

  dtrace("%p, \"%s\", 0x%X, %i, %i", D, field_code, data_type, spf,
      fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RAW_ENTRY;
  E.EN(raw,spf) = spf;
  E.EN(raw,data_type) = data_type;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a LINCOM entry */
int gd_add_lincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const double* m, const double* b,
    int fragment_index) gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", %i, %p, %p, %p, %i", D, field_code, n_fields, in_fields,
      m, b, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL, n_fields,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_LINCOM_ENTRY;
  E.EN(lincom,n_fields) = n_fields;
  E.fragment_index = fragment_index;

  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = (char *)in_fields[i];
    E.EN(lincom,m)[i] = m[i];
    E.EN(lincom,b)[i] = b[i];
  }

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a LINCOM entry with complex scalars */
int gd_add_clincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const GD_DCOMPLEXP(cm), const GD_DCOMPLEXP(cb),
    int fragment_index) gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", %i, %p, %p, %p, %i", D, field_code, n_fields, in_fields,
      cm, cb, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL, n_fields,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_LINCOM_ENTRY;
  E.EN(lincom,n_fields) = n_fields;
  E.flags = GD_EN_COMPSCAL;
  E.fragment_index = fragment_index;

  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = (char *)in_fields[i];
    gd_ca2cs_(E.EN(lincom,cm)[i], cm, i);
    gd_ca2cs_(E.EN(lincom,cb)[i], cb, i);
  }

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a LINTERP entry */
int gd_add_linterp(DIRFILE* D, const char* field_code, const char* in_field,
    const char* table, int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field, table,
      fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_LINTERP_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(linterp,table) = (char *)table;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a BIT entry */
int gd_add_bit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits, int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %i, %i, %i", D, field_code, in_field, bitnum,
      numbits, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_BIT_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(bit,bitnum) = bitnum;
  E.EN(bit,numbits) = numbits;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a SBIT entry */
int gd_add_sbit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits, int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %i, %i, %i", D, field_code, in_field, bitnum,
      numbits, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_SBIT_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(bit,bitnum) = bitnum;
  E.EN(bit,numbits) = numbits;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

static int _GD_AddYoke(DIRFILE* D, gd_entype_t t, const char* field_code,
    const char* in_field1, const char* in_field2, int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, 0x%X, \"%s\", \"%s\", \"%s\", %i", D, t, field_code, in_field1,
      in_field2, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = t;
  E.in_fields[0] = (char *)in_field1;
  E.in_fields[1] = (char *)in_field2;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

int gd_add_multiply(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2, int fragment_index) gd_nothrow
{
  return _GD_AddYoke(D, GD_MULTIPLY_ENTRY, field_code, in_field1, in_field2,
      fragment_index);
}

int gd_add_divide(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2, int fragment_index) gd_nothrow
{
  return _GD_AddYoke(D, GD_DIVIDE_ENTRY, field_code, in_field1, in_field2,
      fragment_index);
}

int gd_add_indir(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2, int fragment_index) gd_nothrow
{
  return _GD_AddYoke(D, GD_INDIR_ENTRY, field_code, in_field1, in_field2,
      fragment_index);
}

int gd_add_sindir(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2, int fragment_index) gd_nothrow
{
  return _GD_AddYoke(D, GD_SINDIR_ENTRY, field_code, in_field1, in_field2,
      fragment_index);
}

int gd_add_recip(DIRFILE* D, const char* field_code, const char* in_field,
    double dividend, int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %g, %i", D, field_code, in_field, dividend,
      fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  E.EN(recip,dividend) = dividend;
  E.in_fields[0] = (char *)in_field;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

#ifndef GD_NO_C99_API
int gd_add_crecip(DIRFILE* D, const char* field_code, const char* in_field,
    double complex cdividend, int fragment_index) gd_nothrow
{
  int error;

  dtrace("%p, \"%s\", \"%s\", %g;%g, %i", D, field_code, in_field,
      creal(cdividend), cimag(cdividend), fragment_index);

  error = gd_add_crecip89(D, field_code, in_field, (const double*)(&cdividend),
      fragment_index);

  dreturn("%i", error);
  return error;
}
#endif

int gd_add_crecip89(DIRFILE* D, const char* field_code, const char* in_field,
    const double cdividend[2], int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", {%g, %g}, %i", D, field_code, in_field,
      cdividend[0], cdividend[1], fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  gd_ra2cs_(E.EN(recip,cdividend), cdividend);
  E.flags = GD_EN_COMPSCAL;
  E.in_fields[0] = (char *)in_field;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a POLYNOM entry */
int gd_add_polynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const double* a, int fragment_index) gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", %i, \"%s\", %p, %i", D, field_code, poly_ord, in_field,
      a, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  if (poly_ord < 1 || poly_ord > GD_MAX_POLYORD)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, NULL, poly_ord,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.EN(polynom,poly_ord) = poly_ord;
  E.fragment_index = fragment_index;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i)
    E.EN(polynom,a)[i] = a[i];

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

int gd_add_cpolynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const GD_DCOMPLEXP(ca), int fragment_index) gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", %i, \"%s\", %p, %i", D, field_code, poly_ord, in_field,
      ca, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  if (poly_ord < 1 || poly_ord > GD_MAX_POLYORD)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, NULL, poly_ord,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.EN(polynom,poly_ord) = poly_ord;
  E.fragment_index = fragment_index;
  E.flags = GD_EN_COMPSCAL;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i)
    gd_ca2cs_(E.EN(polynom,ca)[i], ca, i);

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a PHASE entry */
int gd_add_phase(DIRFILE* D, const char* field_code, const char* in_field,
    gd_int64_t shift, int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %" PRId64 ", %i", D, field_code, in_field,
      (int64_t)shift, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_PHASE_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(phase,shift) = shift;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a WINDOW entry */
int gd_add_window(DIRFILE *D, const char *field_code, const char *in_field,
    const char *check_field, gd_windop_t windop, gd_triplet_t threshold,
    int fragment_index) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, {%g,%" PRIX64 ",%" PRId64 "}, %i", D,
      field_code, in_field, check_field, windop, threshold.r, threshold.u,
      threshold.i, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_WINDOW_ENTRY;
  E.EN(window,threshold) = threshold;
  E.EN(window,windop) = windop;
  E.in_fields[0] = (char *)in_field;
  E.in_fields[1] = (char *)check_field;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a MPLEX entry */
int gd_add_mplex(DIRFILE *D, const char *field_code, const char *in_field,
    const char *count_field, int count_val, int period, int fragment_index)
  gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %i, %i", D, field_code, in_field,
      count_field, count_val, period, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_MPLEX_ENTRY;
  E.EN(mplex,count_val) = count_val;
  E.EN(mplex,period) = period;
  E.in_fields[0] = (char *)in_field;
  E.in_fields[1] = (char *)count_field;
  E.fragment_index = fragment_index;

  _GD_Add(D, &E, NULL, 0);

  GD_RETURN_ERROR(D);
}

/* add a STRING entry */
int gd_add_string(DIRFILE* D, const char* field_code, const char* value,
    int fragment_index) gd_nothrow
{
  gd_entry_t *entry;
  gd_entry_t E;
  char *ptr;

  dtrace("%p, \"%s\", \"%s\", %i", D, field_code, value, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_STRING_ENTRY;
  E.fragment_index = fragment_index;

  /* duplicate early, in case of failure */
  ptr = _GD_Strdup(D, value);
  if (ptr == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  entry = _GD_Add(D, &E, NULL, 0);

  if (D->error)
    free(ptr);
  else
    entry->e->u.string = ptr;

  GD_RETURN_ERROR(D);
}

/* add a CONST entry */
int gd_add_const(DIRFILE* D, const char* field_code, gd_type_t const_type,
    gd_type_t data_type, const void* value, int fragment_index) gd_nothrow
{
  gd_entry_t *entry;
  gd_entry_t E;

  dtrace("%p, \"%s\", 0x%X, 0x%X, %p, %i", D, field_code, const_type, data_type,
      value, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_CONST_ENTRY;
  E.EN(scalar,const_type) = const_type;
  E.fragment_index = fragment_index;
  entry = _GD_Add(D, &E, NULL, 0);

  /* Actually store the constant, now */
  if (entry)
    _GD_DoFieldOut(D, entry, 0, 1, data_type, value);

  GD_RETURN_ERROR(D);
}

/* add a CARRAY entry */
int gd_add_carray(DIRFILE* D, const char* field_code, gd_type_t const_type,
    size_t array_len, gd_type_t data_type, const void* values,
    int fragment_index) gd_nothrow
{
  gd_entry_t *entry;
  gd_entry_t E;

  dtrace("%p, \"%s\", 0x%X, %" PRIuSIZE ", 0x%X, %p, %i", D, field_code,
      const_type, array_len, data_type, values, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_CARRAY_ENTRY;
  E.EN(scalar,const_type) = const_type;
  E.EN(scalar,array_len) = array_len;
  E.fragment_index = fragment_index;

  entry = _GD_Add(D, &E, NULL, 0);

  /* Actually store the carray, now */
  if (entry)
    _GD_DoFieldOut(D, entry, 0, array_len, data_type, values);

  GD_RETURN_ERROR(D);
}

/* add a SARRAY entry */
int gd_add_sarray(DIRFILE* D, const char* field_code, size_t array_len,
    const char **values, int fragment_index) gd_nothrow
{
  size_t i;
  gd_entry_t *entry;
  gd_entry_t E;
  char **data;

  dtrace("%p, \"%s\", %" PRIuSIZE ", %p, %i", D, field_code, array_len, values,
      fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_SARRAY_ENTRY;
  E.EN(scalar,array_len) = array_len;
  E.fragment_index = fragment_index;

  /* duplicate early, in case of failure */
  data = _GD_Malloc(D, array_len * sizeof(char *));
  if (data == NULL)
    GD_RETURN_ERROR(D);

  for (i = 0; i < array_len; ++i) {
    data[i] = _GD_Strdup(D, values[i]);

    if (D->error) {
      while (i-- > 0)
        free(data[i]);
      free(data);
      GD_RETURN_ERROR(D);
    }
  }

  entry = _GD_Add(D, &E, NULL, 0);

  if (D->error) {
    for (i = 0; i < array_len; ++i)
      free(data[i]);
    free(data);
    GD_RETURN_ERROR(D);
  }

  entry->e->u.scalar.d = data;

  dreturn("%i", 0);
  return 0;
}

int gd_madd(DIRFILE* D, const gd_entry_t* entry, const char* parent) gd_nothrow
{
  dtrace("%p, %p, \"%s\"", D, entry, parent);

  GD_RETURN_ERR_IF_INVALID(D);

  _GD_Add(D, entry, parent, 1);

  GD_RETURN_ERROR(D);
}

/* add a META LINCOM entry */
int gd_madd_lincom(DIRFILE* D, const char* parent, const char* field_code,
    int n_fields, const char** in_fields, const double* m, const double* b)
  gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %i, %p, %p, %p", D, field_code, parent,
      n_fields, in_fields, m, b);

  GD_RETURN_ERR_IF_INVALID(D);

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL, n_fields,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_LINCOM_ENTRY;
  E.EN(lincom,n_fields) = n_fields;
  E.fragment_index = 0;

  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = (char *)in_fields[i];
    E.EN(lincom,m)[i] = m[i];
    E.EN(lincom,b)[i] = b[i];
    E.scalar[i] = NULL;
    E.scalar[i + GD_MAX_LINCOM] = NULL;
  }

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META LINCOM entry, with complex scalaras */
int gd_madd_clincom(DIRFILE* D, const char* parent, const char* field_code,
    int n_fields, const char** in_fields, const GD_DCOMPLEXP(cm),
    const GD_DCOMPLEXP(cb)) gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %i, %p, %p, %p", D, field_code, parent,
      n_fields, in_fields, cm, cb);

  GD_RETURN_ERR_IF_INVALID(D);

  if (n_fields < 1 || n_fields > GD_MAX_LINCOM)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL, n_fields,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_LINCOM_ENTRY;
  E.EN(lincom,n_fields) = n_fields;
  E.flags = GD_EN_COMPSCAL;
  E.fragment_index = 0;

  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = (char *)in_fields[i];
    gd_ca2cs_(E.EN(lincom,cm)[i], cm, i);
    gd_ca2cs_(E.EN(lincom,cb)[i], cb, i);
    E.scalar[i] = NULL;
    E.scalar[i + GD_MAX_LINCOM] = NULL;
  }

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META LINTERP entry */
int gd_madd_linterp(DIRFILE* D, const char* parent,
    const char* field_code, const char* in_field, const char* table) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, field_code, parent, in_field,
      table);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_LINTERP_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(linterp,table) = (char *)table;
  E.fragment_index = 0;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META BIT entry */
int gd_madd_bit(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, int bitnum, int numbits) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %in", D, field_code, parent, in_field,
      bitnum, numbits);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_BIT_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(bit,bitnum) = bitnum;
  E.EN(bit,numbits) = numbits;
  E.fragment_index = 0;
  E.scalar[0] = E.scalar[1] = NULL;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META SBIT entry */
int gd_madd_sbit(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, int bitnum, int numbits) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %in", D, field_code, parent, in_field,
      bitnum, numbits);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_SBIT_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(bit,bitnum) = bitnum;
  E.EN(bit,numbits) = numbits;
  E.fragment_index = 0;
  E.scalar[0] = E.scalar[1] = NULL;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

static int _GD_MAddYoke(DIRFILE* D, gd_entype_t t, const char* parent,
    const char* field_code, const char* in_field1, const char* in_field2)
  gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, 0x%X, \"%s\", \"%s\", \"%s\", \"%s\"", D, t, field_code, parent,
      in_field1, in_field2);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = t;
  E.in_fields[0] = (char *)in_field1;
  E.in_fields[1] = (char *)in_field2;
  E.fragment_index = 0;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

int gd_madd_multiply(DIRFILE* D, const char *parent, const char* field_code,
    const char* in_field1, const char* in_field2) gd_nothrow
{
  return _GD_MAddYoke(D, GD_MULTIPLY_ENTRY, parent, field_code, in_field1,
      in_field2);
}

/* add a META PHASE entry */
int gd_madd_phase(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, gd_int64_t shift) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %" PRId64, D, field_code, parent,
      in_field, shift);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_PHASE_ENTRY;
  E.in_fields[0] = (char *)in_field;
  E.EN(phase,shift) = shift;
  E.fragment_index = 0;
  E.scalar[0] = NULL;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META POLYNOM entry */
int gd_madd_polynom(DIRFILE* D, const char* parent, const char* field_code,
    int poly_ord, const char* in_field, const double* a) gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %i, \"%s\", %p", D, field_code, parent, poly_ord,
      in_field, a);

  GD_RETURN_ERR_IF_INVALID(D);

  if (poly_ord < 1 || poly_ord > GD_MAX_POLYORD)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, NULL, poly_ord,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.EN(polynom,poly_ord) = poly_ord;
  E.fragment_index = 0;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i) {
    E.EN(polynom,a)[i] = a[i];
    E.scalar[i] = NULL;
  }

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META POLYNOM entry */
int gd_madd_cpolynom(DIRFILE* D, const char* parent, const char* field_code,
    int poly_ord, const char* in_field, const GD_DCOMPLEXP(ca)) gd_nothrow
{
  int i;
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", %i, \"%s\", %p", D, field_code, parent, poly_ord,
      in_field, ca);

  GD_RETURN_ERR_IF_INVALID(D);

  if (poly_ord < 1 || poly_ord > GD_MAX_POLYORD)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, NULL, poly_ord,
        NULL);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_POLYNOM_ENTRY;
  E.EN(polynom,poly_ord) = poly_ord;
  E.fragment_index = 0;
  E.flags = GD_EN_COMPSCAL;
  E.in_fields[0] = (char *)in_field;

  for (i = 0; i <= poly_ord; ++i) {
    gd_ca2cs_(E.EN(polynom,ca)[i], ca, i);
    E.scalar[i] = NULL;
  }

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

int gd_madd_divide(DIRFILE* D, const char *parent, const char* field_code,
    const char* in_field1, const char* in_field2) gd_nothrow
{
  return _GD_MAddYoke(D, GD_DIVIDE_ENTRY, parent, field_code, in_field1,
      in_field2);
}

int gd_madd_indir(DIRFILE* D, const char *parent, const char* field_code,
        const char* in_field1, const char* in_field2) gd_nothrow
{
  return _GD_MAddYoke(D, GD_INDIR_ENTRY, parent, field_code, in_field1,
      in_field2);
}

int gd_madd_sindir(DIRFILE *D, const char *parent, const char* field_code,
    const char* in_field1, const char* in_field2) gd_nothrow
{
  return _GD_MAddYoke(D, GD_SINDIR_ENTRY, parent, field_code, in_field1,
      in_field2);
}

int gd_madd_recip(DIRFILE* D, const char *parent, const char* field_code,
    const char* in_field, double dividend) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %g", D, parent, field_code, in_field,
      dividend);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  E.EN(recip,dividend) = dividend;
  E.in_fields[0] = (char *)in_field;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

#ifndef GD_NO_C99_API
int gd_madd_crecip(DIRFILE* D, const char *parent, const char* field_code, const
    char* in_field, double complex cdividend)
{
  int error;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %g;%g", D, parent, field_code, in_field,
      creal(cdividend), cimag(cdividend));

  error = gd_madd_crecip89(D, parent, field_code, in_field,
      (const double*)(&cdividend));

  dreturn("%i", error);
  return error;
}
#endif

int gd_madd_crecip89(DIRFILE* D, const char *parent, const char* field_code,
    const char* in_field, const double cdividend[2]) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", {%g, %g}", D, parent, field_code,
      in_field, cdividend[0], cdividend[1]);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_RECIP_ENTRY;
  gd_ra2cs_(E.EN(recip,cdividend), cdividend);
  E.flags = GD_EN_COMPSCAL;
  E.in_fields[0] = (char *)in_field;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META WINDOW entry */
int gd_madd_window(DIRFILE *D, const char *parent, const char *field_code,
    const char *in_field, const char *check_field, gd_windop_t windop,
    gd_triplet_t threshold) gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\", %i, {%g,%" PRIX64 ",%" PRId64 "}",
      D, parent, field_code, in_field, check_field, windop, threshold.r,
      threshold.u, threshold.i);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_WINDOW_ENTRY;
  E.EN(window,threshold) = threshold;
  E.EN(window,windop) = windop;
  E.in_fields[0] = (char *)in_field;
  E.in_fields[1] = (char *)check_field;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META MPLEX entry */
int gd_madd_mplex(DIRFILE *D, const char *parent, const char *field_code,
    const char *in_field, const char *count_field, int count_val, int period)
  gd_nothrow
{
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\", %i, %i", D, parent, field_code,
      in_field, count_field, count_val, period);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_MPLEX_ENTRY;
  E.EN(mplex,count_val) = count_val;
  E.EN(mplex,period) = period;
  E.in_fields[0] = (char *)in_field;
  E.in_fields[1] = (char *)count_field;

  _GD_Add(D, &E, parent, 0);

  GD_RETURN_ERROR(D);
}

/* add a META STRING entry */
int gd_madd_string(DIRFILE* D, const char* parent,
    const char* field_code, const char* value) gd_nothrow
{
  gd_entry_t *entry;
  gd_entry_t E;
  char *ptr;

  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, parent, field_code, value);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_STRING_ENTRY;
  E.fragment_index = 0;

  /* duplicate early, in case of failure */
  ptr = _GD_Strdup(D, value);
  if (ptr == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  entry = _GD_Add(D, &E, parent, 0);

  if (D->error)
    free(ptr);
  else
    entry->e->u.string = ptr;

  GD_RETURN_ERROR(D);
}

/* add a META CONST entry */
int gd_madd_const(DIRFILE* D, const char* parent, const char* field_code,
    gd_type_t const_type, gd_type_t data_type, const void* value) gd_nothrow
{
  gd_entry_t *entry;
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", 0x%X, 0x%X, %p", D, parent, field_code,
      const_type, data_type, value);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_CONST_ENTRY;
  E.EN(scalar,const_type) = const_type;
  E.fragment_index = 0;
  entry = _GD_Add(D, &E, parent, 0);

  /* Actually store the constant, now */
  if (entry)
    _GD_DoFieldOut(D, entry, 0, 1, data_type, value);

  GD_RETURN_ERROR(D);
}

/* add a META CARRAY entry */
int gd_madd_carray(DIRFILE* D, const char* parent, const char* field_code,
    gd_type_t const_type, size_t array_len, gd_type_t data_type,
    const void* values) gd_nothrow
{
  gd_entry_t *entry;
  gd_entry_t E;

  dtrace("%p, \"%s\", \"%s\", 0x%X, %" PRIuSIZE ", 0x%X, %p", D, parent,
      field_code, const_type, array_len, data_type, values);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_CARRAY_ENTRY;
  E.EN(scalar,const_type) = const_type;
  E.EN(scalar,array_len) = array_len;
  E.fragment_index = 0;
  entry = _GD_Add(D, &E, parent, 0);

  /* Actually store the carray, now */
  if (entry)
    _GD_DoFieldOut(D, entry, 0, array_len, data_type, values);

  GD_RETURN_ERROR(D);
}

/* Add META SARRAY */
int gd_madd_sarray(DIRFILE* D, const char *parent, const char *field_code,
    size_t array_len, const char **values) gd_nothrow
{
  size_t i;
  gd_entry_t *entry;
  gd_entry_t E;
  char **data;

  dtrace("%p, \"%s\", \"%s\", %" PRIuSIZE ", %p", D, parent, field_code,
      array_len, values);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&E, 0, sizeof(gd_entry_t));
  E.field = (char *)field_code;
  E.field_type = GD_SARRAY_ENTRY;
  E.EN(scalar,array_len) = array_len;

  /* duplicate early, in case of failure */
  data = _GD_Malloc(D, array_len * sizeof(char *));
  if (data == NULL)
    GD_RETURN_ERROR(D);

  for (i = 0; i < array_len; ++i) {
    data[i] = _GD_Strdup(D, values[i]);

    if (D->error) {
      while (i-- > 0)
        free(data[i]);
      free(data);
      GD_RETURN_ERROR(D);
    }
  }

  entry = _GD_Add(D, &E, parent, 0);

  if (D->error) {
    for (i = 0; i < array_len; ++i)
      free(data[i]);
    free(data);
    GD_RETURN_ERROR(D);
  }

  entry->e->u.scalar.d = data;

  dreturn("%i", 0);
  return 0;
}

/* add an alias */
static int _GD_AddAlias(DIRFILE *restrict D, const char *restrict parent,
    const char *restrict field_code, const char *restrict target,
    int fragment_index)
{
  unsigned u;
  size_t nsl, subfield_offs, len = strlen(field_code);
  char *name = NULL;
  void *ptr;
  gd_entry_t *E = NULL, *P = NULL;
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, parent, field_code, target,
      fragment_index);

  /* Early checks */
  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (fragment_index < 0 || fragment_index >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);
  else if (D->fragment[fragment_index].protection & GD_PROTECT_FORMAT)
    GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment_index].cname);

  if (parent != NULL) {
    /* look for parent */
    P = _GD_FindField(D, parent, strlen(parent), D->entry, D->n_entries, 0,
        NULL);
    if (P == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, parent);
      goto add_alias_error;
    }
    fragment_index = P->fragment_index;

    /* make sure it's not a meta field already or an alias */
    if (P->e->n_meta == -1 || P->field_type == GD_ALIAS_ENTRY) {
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);
      goto add_alias_error;
    }

    subfield_offs = P->e->len + 1;
    name = _GD_Malloc(D, subfield_offs + strlen(field_code) + 1);
    if (name == NULL)
      goto add_alias_error;

    memcpy(name, parent, P->e->len);
    name[subfield_offs - 1] = '/';
    memcpy(name + subfield_offs, field_code, len + 1);
    len += subfield_offs;
  } else {
    /* this will check for affixes and take care of detecting Barth-style
     * metafield definitions */
    P = _GD_FixName(D, &name, &len, field_code, fragment_index, &subfield_offs);

    if (D->error)
      goto add_alias_error;
  }

  /* Figure out the length of the attached namespace in the supplied field
   * name.  If subfield_offs > 0, then there can't be a namespace because
   * we're only checking the subfield name */
  if (D->standards >= 10 && subfield_offs == 0) {
    const char *dot, *slash;
    _GD_SlashDot(name, len, GD_CO_NAME, &dot, &slash);
    if (dot)
      nsl = dot - name + 1;
    else
      nsl = 0;
  } else
    nsl = 0;

  /* check alias name. */
  if (_GD_ValidateField(name + subfield_offs, nsl, D->standards, 1, GD_VF_NAME))
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, field_code);
  else if (_GD_FindField(D, name, len, D->entry, D->n_entries, 0, &u))
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, name);
  else
    /* check target */
    _GD_CheckCodeAffixes(D, target, fragment_index,
        GD_CO_ERROR | ((D->standards <= 5) ? GD_CO_EARLY : 0));

  if (D->error)
    goto add_alias_error;

  ptr = _GD_Realloc(D, D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));
  if (ptr == NULL)
    goto add_alias_error;
  D->entry = (gd_entry_t **)ptr;

  /* create and store */
  E = _GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL)
    goto add_alias_error;

  memset(E, 0, sizeof(gd_entry_t));
  E->e = _GD_Malloc(D, sizeof(struct gd_private_entry_));
  if (E->e == NULL)
    goto add_alias_error;

  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field = name;
  E->e->len = len;
  E->fragment_index = fragment_index;
  E->in_fields[0] = _GD_Strdup(D, target);
  E->field_type = GD_ALIAS_ENTRY;
  E->flags |= GD_EN_CALC;

  if (D->error) {
    _GD_FreeE(D, E, 1);
    GD_RETURN_ERROR(D);
  }

  /* add the entry and resort the entry list */
  _GD_InsertSort(D, E, u);
  D->n_entries++;
  D->fragment[fragment_index].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  /* Invalidate the field lists */
  if (P) {
    P->e->fl.value_list_validity = 0;
    P->e->fl.entry_list_validity = 0;
  } else {
    D->fl.value_list_validity = 0;
    D->fl.entry_list_validity = 0;
  }

  /* Update aliases */
  _GD_UpdateAliases(D, 0);

  dreturn("%i", GD_E_OK);
  return GD_E_OK;

add_alias_error:
  free(E);
  free(name);
  GD_RETURN_ERROR(D);
}

int gd_add_alias(DIRFILE *D, const char *alias_name, const char *target_code,
    int fragment_index) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", %i", D, alias_name, target_code, fragment_index);

  _GD_AddAlias(D, NULL, alias_name, target_code, fragment_index);

  GD_RETURN_ERROR(D);
}

int gd_madd_alias(DIRFILE *D, const char *parent, const char *alias_name,
    const char *target_code) gd_nothrow
{
  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, parent, alias_name, target_code);

  _GD_AddAlias(D, parent, alias_name, target_code, 0);

  GD_RETURN_ERROR(D);
}
