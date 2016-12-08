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

#define GD_MAX_PRETTY_FIELD_WIDTH 80

/* If we're writing to 'file', call the appropriate sync function */
static void _GD_SyncRaw(DIRFILE *D, struct gd_raw_file_ *file)
{
  dtrace("%p, %p", D, file);
  
  if ((D->flags & GD_ACCMODE) == GD_RDWR && file->idata >= 0 && 
      (file->mode & GD_FILE_WRITE) && _GD_ef[file->subenc].sync != NULL &&
      (*_GD_ef[file->subenc].sync)(file))
  {
    _GD_SetEncIOError(D, GD_E_IO_WRITE, file);
  }

  dreturnvoid();
}

void _GD_Flush(DIRFILE *D, gd_entry_t *E, int syn, int clo)
{
  int i;

  dtrace("%p, %p, %i, %i", D, E, syn, clo);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0, E->field);
    D->recurse_level--;
    dreturnvoid();
    return;
  }

  _GD_FindInputs(D, E, 0);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      if (syn) {
        /* We can't get here when a temporary file is open, so if we're dealing
         * with an GD_EF_OOP encoding and file[1] is open, then it's a
         * write-side file which we should sync.
         */
        if (_GD_ef[E->e->u.raw.file[0].subenc].flags & GD_EF_OOP)
          _GD_SyncRaw(D, E->e->u.raw.file + 1);
        else
          _GD_SyncRaw(D, E->e->u.raw.file);
      }
      if (clo && D->error == 0 && (E->e->u.raw.file[0].idata >= 0 ||
            ((_GD_ef[E->e->u.raw.file[0].subenc].flags & GD_EF_OOP) &&
             (E->e->u.raw.file[1].idata >= 0))))
      {
        _GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_KEEP);
      }
      break;
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i)
        _GD_Flush(D, E->e->entry[i], syn, clo);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      _GD_Flush(D, E->e->entry[1], syn, clo);
      /* fallthrough */
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      _GD_Flush(D, E->e->entry[0], syn, clo);
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
      break;
  }

  D->recurse_level--;
  dreturnvoid();
}

static const char* _GD_TypeName(DIRFILE* D, gd_type_t data_type)
{
  const char* ptr;

  dtrace("%p, 0x%X", D, data_type);

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

  dtrace("%p, 0x%X", D, data_type);

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

static const char *_GD_WindopName(DIRFILE *D, gd_windop_t op)
{
  const char *ptr;

  dtrace("%p, %i", D, op);

  switch(op) {
    case GD_WINDOP_EQ:
      ptr = "EQ";
      break;
    case GD_WINDOP_GE:
      ptr = "GE";
      break;
    case GD_WINDOP_GT:
      ptr = "GT";
      break;
    case GD_WINDOP_LE:
      ptr = "LE";
      break;
    case GD_WINDOP_LT:
      ptr = "LT";
      break;
    case GD_WINDOP_NE:
      ptr = "NE";
      break;
    case GD_WINDOP_SET:
      ptr = "SET";
      break;
    case GD_WINDOP_CLR:
      ptr = "CLR";
      break;
    default:
      _GD_InternalError(D);
      ptr = "";
      break;
  }

  dreturn("\"%s\"", ptr);
  return ptr;
}

static ssize_t _GD_StringEscapeise(FILE *stream, const char *in, int meta,
    int permissive, int standards)
{
  const char* HexDigit = "0123456789ABCDEF";
  ssize_t len = 0;

  dtrace("%p, \"%s\", %i, %i, %i", stream, in, meta, permissive, standards);

  if (in == NULL || in[0] == '\0') {
    if (stream) {
      if (fputs("\"\"", stream) == EOF)
        goto WRITE_ERR;
    }

    dreturn("%i", 2);
    return 2;
  }

  if (!permissive && standards < 6) {
    if (stream)
      if (fputs(in, stream) == EOF)
        goto WRITE_ERR;
    dreturn("%" PRIuSIZE, strlen(in));
    return strlen(in);
  }

  for (; *in != '\0'; ++in) {
    if (*in == '\\' || *in == '#' || *in == '"' || *in == ' ') {
      if (stream) {
        if (fputc('\\', stream) == EOF)
          goto WRITE_ERR;
        if (fputc(*in, stream) == EOF)
          goto WRITE_ERR;
      }
      len += 2;
    } else if (*in < 0x20
#if CHAR_MIN != 0
        && *in >= 0x00
#endif
        )
    {
      if (stream) {
        if (fputs("\\x", stream) == EOF)
          goto WRITE_ERR;
        if (fputc(HexDigit[*in >> 4], stream) == EOF)
          goto WRITE_ERR;
        if (fputc(HexDigit[*in & 0xF], stream) == EOF)
          goto WRITE_ERR;
      }
      len += 4;
    } else if (meta && *in == '/')
      break;
    else {
      if (stream)
        if (fputc(*in, stream) == EOF)
          goto WRITE_ERR;
      len++;
    }
  }

  dreturn("%" PRIuSIZE, len);
  return len;

WRITE_ERR:
  dreturn("%i", -1);
  return -1;
}

/* write a field code, taking care of stripping off affixes; returns the length
 * written */
#define GD_WFC_SPACE     0x01 /* Write a trailing space */
#define GD_WFC_SCALAR    0x02 /* This is a scalar field code */
#define GD_WFC_EARLY     0x04 /* This is an early field (no namespaces!) */
#define GD_WFC_NAME      0x08 /* This is a field name (no repr) */
static ssize_t _GD_WriteFieldCode(DIRFILE *D, FILE *stream, int me,
    const char *code, int index, int permissive, int standards, unsigned flags)
{
  ssize_t len;
  char *ptr;
  const unsigned strip_flags = GD_CO_NSROOT | GD_CO_ASSERT
    | ((flags & GD_WFC_EARLY) ? GD_CO_EARLY : 0)
    | ((flags & GD_WFC_NAME) ? 0 : GD_CO_REPR)
    | ((permissive || D->standards >= 10) ? GD_CO_REPRZ : 0);

  dtrace("%p, %p, %i, \"%s\", %i, %i, %i, 0x%X", D, stream, me, code, index,
      permissive, standards, flags);

  ptr = _GD_StripCode(D, me, code, strip_flags);

  len = _GD_StringEscapeise(stream, ptr, 0, permissive, standards);

  /* If a scalar field code could be interpreted as a number, we must force
   * interpretation as a field code by appending a <0> scalar index, which is
   * valid for both CARRAY and CONST fields.  */
  if (len > 0 && (flags & GD_WFC_SCALAR) && index == -1)
    if (_GD_TokToNum(ptr, standards, !permissive, NULL, NULL, NULL, NULL) != -1)
    {
      if (permissive || standards >= 8) {
        if (fputs("<0>", stream) == EOF)
          len = -1;
        else
          len += 3;
      } else 
        _GD_InternalError(D);
    }

  /* append a space */
  if ((flags & GD_WFC_SPACE) && len > 0) {
    if (fputc(' ', stream) == EOF)
      len = -1;
    else
      len++;
  }

  free(ptr);

  dreturn("%" PRIdSIZE, len);
  return len;
}

/* write a field name, padding to the specified length */
static ssize_t _GD_PadField(DIRFILE *D, FILE *stream, int me, const char *in,
    ssize_t len, int early, int permissive, int standards)
{
  ssize_t i;

  dtrace("%p, %p, %i, \"%s\", %" PRIdSIZE ", %i, %i, %i", D, stream, me, in,
      len, early, permissive, standards);

  i = _GD_WriteFieldCode(D, stream, me, in, 0, permissive, standards,
      GD_WFC_NAME | early);

  if (i >= 0)
    for (; i < len; ++i)
      if (fputc(' ', stream) == EOF) {
        i = -1;
        break;
      }

  dreturn("%" PRIdSIZE, i);
  return i;
}

/* Write a litteral parameter or CONST name or CARRAY element */
static int _GD_WriteConst(DIRFILE *D, FILE *stream, int me, int permissive,
    int type, const void* value, const char *scalar, int index, int early,
    const char *postamble)
{
  int e;

  dtrace("%p, %p, %i, %i, 0x%X, %p, \"%s\", %i, %i, \"%s\"", D, stream, me,
      permissive, type, value, scalar, index, early, postamble);

  if (scalar != NULL) {
    if (_GD_WriteFieldCode(D, stream, me, scalar, index, permissive,
          D->standards, GD_WFC_SCALAR | (early ? GD_WFC_EARLY : 0)) < 0)
    {
      dreturn("%i", -1);
      return -1;
    }
    if (index == -1)
      fprintf(stream, "%s", postamble);
    else
      fprintf(stream, "<%i>%s", index, postamble);
  } else if (type == GD_UINT64)
    fprintf(stream, "%" PRIu64 "%s", *(uint64_t *)value, postamble);
  else if (type == GD_INT64)
    fprintf(stream, "%" PRId64 "%s", *(int64_t *)value, postamble);
  else if (type == GD_UINT32)
    fprintf(stream, "%" PRIu32 "%s", *(uint32_t *)value, postamble);
  else if (type == GD_INT32)
    fprintf(stream, "%" PRIi32 "%s", *(int32_t *)value, postamble);
  else if (type == GD_UINT16)
    fprintf(stream, "%" PRIu16 "%s", *(uint16_t *)value, postamble);
  else if (type == GD_INT16)
    fprintf(stream, "%" PRIi16 "%s", *(int16_t *)value, postamble);
  else if (type == GD_FLOAT64)
    fprintf(stream, "%.15g%s", *(double *)value, postamble);
  else if (type == GD_COMPLEX128)
    fprintf(stream, "%.15g;%.15g%s", *(double *)value, ((double *)value)[1],
        postamble);
  else {
    _GD_InternalError(D);
    dreturn("%i", -1);
    return -1;
  }

  e = ferror(stream);
  dreturn("%i", e);
  return e;
}

#define GD_WRITE_INFIELD(n,f) \
  (_GD_WriteFieldCode(D, stream, me, E->in_fields[n], 0, permissive, \
                      D->standards, f | early) < 0)

#define GD_WRITE_CONST(t,l,n,p) \
  (_GD_WriteConst(D, stream, me, permissive, t, &(l), E->scalar[n], \
                  E->scalar_ind[n], early, p) < 0)

/* Write a field specification line */
static int _GD_FieldSpec(DIRFILE* D, FILE* stream, const gd_entry_t* E,
    int me, int meta, size_t max_len, int pretty, int permissive)
{
  int i;
  char *ptr;
  ssize_t len, pos;
  char buffer[1000];
  size_t z;
  const unsigned early = (E->flags & GD_EN_EARLY) ? GD_WFC_EARLY : 0;

  dtrace("%p, %p, %p, %i, %i, %" PRIuSIZE ", %i, %i", D, stream, E, me, meta,
      max_len, pretty, permissive);

  /* INDEX is implicit, and it is an error to define it in the format file */
  if (E->field_type == GD_INDEX_ENTRY) {
    dreturn("%i", 0);
    return 0;
  }

  /* aliases */
  if (E->field_type == GD_ALIAS_ENTRY) {
    if (fputs("/ALIAS ", stream) == EOF)
      goto WRITE_ERR;
    if (_GD_WriteFieldCode(D, stream, me, E->field, 0, permissive, D->standards,
          GD_WFC_SPACE | GD_WFC_NAME | early) < 0)
    {
      goto WRITE_ERR;
    }

    if (GD_WRITE_INFIELD(0, 0))
      goto WRITE_ERR;

    if (fputc('\n', stream) == EOF)
      goto WRITE_ERR;
  } else {
    /* For DSV 6, use the "META" directive.  Later verisons can just use
     * the "parent/subfield" style of metafield definition (AKA "Barth-style")
     */
    if (meta && D->standards < 7) {
      /* Write "META <parent> " for DSV == 6 */
      if (fputs("META ", stream) == EOF)
        goto WRITE_ERR;

      /* There's no need to use WriteFieldCode here because DSV < 7 doesn't
       * support affixes */
      if (_GD_StringEscapeise(stream, E->field, 1, permissive,
            D->standards) < 0)
      {
        goto WRITE_ERR;
      }

      if (fputc(' ', stream) == EOF)
        goto WRITE_ERR;

      /* Advance field name pointer */
      ptr = strchr(E->field, '/') + 1;
    } else
      ptr = E->field;

    /* field name -- with DSV >=7, for metafields, this will be parent/subfield
     */
    if (_GD_PadField(D, stream, me, ptr, max_len, early, permissive,
          D->standards) < 0)
    {
      goto WRITE_ERR;
    }

    switch(E->field_type) {
      case GD_RAW_ENTRY:
        if (fprintf(stream, " RAW%s %s ", pretty ? "     " : "", (permissive ||
                D->standards >= 5) ? _GD_TypeName(D, E->EN(raw,data_type)) :
              _GD_OldTypeName(D, E->EN(raw,data_type))) < 0 ||
            GD_WRITE_CONST(GD_UINT_TYPE, E->EN(raw,spf), 0, ""))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_LINCOM_ENTRY:
        if (fprintf(stream, " LINCOM%s %i ", pretty ? "  " : "",
              E->EN(lincom,n_fields)) < 0)
        {
          goto WRITE_ERR;
        }
        for (i = 0; i < E->EN(lincom,n_fields); ++i) {
          if (GD_WRITE_INFIELD(i, GD_WFC_SPACE))
            goto WRITE_ERR;

          if (E->flags & GD_EN_COMPSCAL) {
            if (GD_WRITE_CONST(GD_COMPLEX128, E->EN(lincom,cm)[i], i, " ") ||
                GD_WRITE_CONST(GD_COMPLEX128, E->EN(lincom,cb)[i], i +
                  GD_MAX_LINCOM, i == E->EN(lincom,n_fields) - 1 ? "" : " "))
            {
              goto WRITE_ERR;
            }
          } else {
            if (GD_WRITE_CONST(GD_FLOAT64, E->EN(lincom,m)[i], i, " ") ||
                GD_WRITE_CONST(GD_FLOAT64, E->EN(lincom,b)[i], i +
                  GD_MAX_LINCOM, i == E->EN(lincom,n_fields) - 1 ? "" : " "))
            {
              goto WRITE_ERR;
            }
          }
        }
        break;
      case GD_LINTERP_ENTRY:
        if (fprintf(stream, " LINTERP%s ", pretty ? " " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) ||
            _GD_StringEscapeise(stream, E->EN(linterp,table), 0, permissive,
              D->standards) < 0)
        {
          goto WRITE_ERR;
        }
        break;
      case GD_BIT_ENTRY:
        if (fprintf(stream, " BIT%s ", pretty ? "     " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) ||
            GD_WRITE_CONST(GD_INT_TYPE, E->EN(bit,bitnum), 0, " ") ||
            GD_WRITE_CONST(GD_INT_TYPE, E->EN(bit,numbits), 1, ""))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_DIVIDE_ENTRY:
        if (fprintf(stream, " DIVIDE%s ", pretty ? "  " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) || GD_WRITE_INFIELD(1, 0))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_RECIP_ENTRY:
        if (fprintf(stream, " RECIP%s ", pretty ? "   " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) ||
            GD_WRITE_CONST(GD_COMPLEX128, E->EN(recip,cdividend), 0, ""))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_MULTIPLY_ENTRY:
        if (fputs(" MULTIPLY ", stream) == EOF ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) || GD_WRITE_INFIELD(1, 0))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_PHASE_ENTRY:
        if (fprintf(stream, " PHASE%s ", pretty ? "   " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) ||
            GD_WRITE_CONST(GD_INT64, E->EN(phase,shift), 0, ""))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_POLYNOM_ENTRY:
        if (fprintf(stream, " POLYNOM%s ", pretty ? " " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE))
        {
          goto WRITE_ERR;
        }
        for (i = 0; i <= E->EN(polynom,poly_ord); ++i)
          if (E->flags & GD_EN_COMPSCAL) {
            if (GD_WRITE_CONST(GD_COMPLEX128, E->EN(polynom,ca)[i], i,
                  (i == E->EN(polynom,poly_ord)) ?  "" : " "))
            {
              goto WRITE_ERR;
            }
          } else {
            if (GD_WRITE_CONST(GD_FLOAT64, E->EN(polynom,a)[i], i,
                  (i == E->EN(polynom,poly_ord)) ?  "" : " "))
            {
              goto WRITE_ERR;
            }
          }
        break;
      case GD_SBIT_ENTRY:
        if (fprintf(stream, " SBIT%s ", pretty ? "    " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) ||
            GD_WRITE_CONST(GD_INT_TYPE, E->EN(bit,bitnum), 0, " ") ||
            GD_WRITE_CONST(GD_INT_TYPE, E->EN(bit,numbits), 1, ""))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_WINDOW_ENTRY:
        if (fprintf(stream, " WINDOW%s ", pretty ? "  " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) ||
            GD_WRITE_INFIELD(1, GD_WFC_SPACE) ||
            fprintf(stream, " %s ", _GD_WindopName(D, E->EN(window,windop)))
            < 0)
        {
          goto WRITE_ERR;
        }
        switch (E->EN(window,windop)) {
          case GD_WINDOP_EQ:
          case GD_WINDOP_NE:
            if (GD_WRITE_CONST(GD_INT64, E->EN(window,threshold.i), 0, ""))
              goto WRITE_ERR;
            break;
          case GD_WINDOP_SET:
          case GD_WINDOP_CLR:
            if (GD_WRITE_CONST(GD_UINT64, E->EN(window,threshold.u), 0, ""))
              goto WRITE_ERR;
            break;
          default:
            if (GD_WRITE_CONST(GD_FLOAT64, E->EN(window,threshold.r), 0, ""))
              goto WRITE_ERR;
            break;
        }
        break;
      case GD_MPLEX_ENTRY:
        if (fprintf(stream, " MPLEX%s ", pretty ? "   " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) ||
            GD_WRITE_INFIELD(1, GD_WFC_SPACE) ||
            GD_WRITE_CONST(GD_INT_TYPE, E->EN(mplex,count_val), 0, " ") ||
            GD_WRITE_CONST(GD_INT_TYPE, E->EN(mplex,period), 1, ""))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_CONST_ENTRY:
        if (fprintf(stream, " CONST%s %s ", pretty ? "   " : "", _GD_TypeName(D,
                E->EN(scalar,const_type))) < 0)
        {
          goto WRITE_ERR;
        }
        if (E->EN(scalar,const_type) & GD_SIGNED) {
          if (fprintf(stream, "%" PRId64, *(int64_t*)E->e->u.scalar.d) < 0)
            goto WRITE_ERR;
        } else if (E->EN(scalar,const_type) & GD_IEEE754) {
          if (fprintf(stream, "%.15g", *(double*)E->e->u.scalar.d) < 0)
            goto WRITE_ERR;
        } else if (E->EN(scalar,const_type) & GD_COMPLEX) {
          if (fprintf(stream, "%.15g;%.15g", *(double*)E->e->u.scalar.d,
                *((double*)E->e->u.scalar.d + 1)) < 0)
          {
            goto WRITE_ERR;
          }
        } else
          if (fprintf(stream, "%" PRIu64, *(uint64_t*)E->e->u.scalar.d) < 0)
            goto WRITE_ERR;
        break;
      case GD_CARRAY_ENTRY:
        pos = fprintf(stream, " CARRAY%s %s", pretty ? "  " : "",
            _GD_TypeName(D, E->EN(scalar,const_type)));
        if (pos < 0)
          goto WRITE_ERR;

        for (z = 0; z < E->EN(scalar,array_len); ++z) {
          if (E->EN(scalar,const_type) & GD_SIGNED)
            len = sprintf(buffer, " %" PRId64, ((int64_t*)E->e->u.scalar.d)[z]);
          else if (E->EN(scalar,const_type) & GD_IEEE754)
            len = sprintf(buffer, " %.15g", ((double*)E->e->u.scalar.d)[z]);
          else if (E->EN(scalar,const_type) & GD_COMPLEX)
            len = sprintf(buffer, " %.15g;%.15g",
                ((double*)E->e->u.scalar.d)[2 * z],
                ((double*)E->e->u.scalar.d)[2 * z + 1]);
          else
            len = sprintf(buffer, " %" PRIu64,
                ((uint64_t*)E->e->u.scalar.d)[z]);

          /* don't write lines that are too long
           * also, add one to length for the trailing '\n' */
          if (GD_SSIZE_T_MAX - (len + 1) <= pos) {
            _GD_SetError(D, GD_E_LINE_TOO_LONG, GD_E_LONG_FLUSH, E->field, 0,
                NULL);
            dreturn("%i", -1);
            return -1;
          }
          if (fputs(buffer, stream) == EOF)
            goto WRITE_ERR;
        }
        break;
      case GD_STRING_ENTRY:
        if (fprintf(stream, " STRING%s ", pretty ? "  " : "") < 0)
          goto WRITE_ERR;
        if (_GD_StringEscapeise(stream, E->e->u.string, 0, permissive,
              D->standards) < 0)
        {
          goto WRITE_ERR;
        }
        break;
      case GD_SARRAY_ENTRY:
        pos = fprintf(stream, " SARRAY%s", pretty ? "  " : "");
        if (pos < 0)
          goto WRITE_ERR;

        for (z = 0; z < E->EN(scalar,array_len); ++z) {
          if (fputc(' ', stream) == EOF)
            goto WRITE_ERR;
          pos++;

          /* compute length */
          len = _GD_StringEscapeise(NULL, ((char**)E->e->u.scalar.d)[z], 0,
              permissive, D->standards);

          /* don't write lines that are too long
           * also, add one to length for the trailing '\n' */
          if (GD_SSIZE_T_MAX - (len + 1) <= pos) {
            _GD_SetError(D, GD_E_LINE_TOO_LONG, GD_E_LONG_FLUSH, E->field, 0,
                NULL);
            dreturn("%i", -1);
            return -1;
          }

          if (_GD_StringEscapeise(stream, ((char**)E->e->u.scalar.d)[z], 0,
                permissive, D->standards) < 0)
          {
            goto WRITE_ERR;
          }
        }
        break;
      case GD_INDIR_ENTRY:
        if (fprintf(stream, " INDIR%s ", pretty ? "   " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) || GD_WRITE_INFIELD(1, 0))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_SINDIR_ENTRY:
        if (fprintf(stream, " SINDIR%s ", pretty ? "  " : "") < 0 ||
            GD_WRITE_INFIELD(0, GD_WFC_SPACE) || GD_WRITE_INFIELD(1, 0))
        {
          goto WRITE_ERR;
        }
        break;
      case GD_INDEX_ENTRY:
      case GD_ALIAS_ENTRY:
      case GD_NO_ENTRY:
        _GD_InternalError(D);
        dreturn("%i", -1);
        return -1;
    }

    if (fputc('\n', stream) == EOF)
      goto WRITE_ERR;
  }

  if (!D->error && (E->flags & GD_EN_HIDDEN) &&
      (permissive || D->standards >= 9))
  {
    if (fputs("/HIDDEN ", stream) == EOF)
      goto WRITE_ERR;
    if (_GD_WriteFieldCode(D, stream, me, E->field, 0, permissive, D->standards,
          GD_WFC_NAME | early) < 0)
    {
      goto WRITE_ERR;
    }
    if (fputc('\n', stream) == EOF)
      goto WRITE_ERR;
  }

  dreturn("%i", 0);
  return 0;

WRITE_ERR:
  if (!D->error)
    _GD_SetError(D, GD_E_IO, GD_E_IO_WRITE, NULL, 0, NULL);
  dreturn("%i", -1);
  return -1;
}

static int WriteInclude(DIRFILE *D, int i, int j, size_t ns_offset,
    int permissive, FILE *stream)
{
  const char *ns = NULL, *px = NULL;
  char *sx = NULL;
  int free_sx = 0;

  dtrace("%p, %i, %i, %" PRIuSIZE ", %i, %p", D, i, j, ns_offset,
      permissive, stream);

  if (D->fragment[j].nsl > D->fragment[i].nsl)
    ns = D->fragment[j].ns + ns_offset;

  if (D->fragment[j].pxl)
    px = D->fragment[j].px + D->fragment[i].pxl;

  if (D->fragment[j].sxl > 0) {
    if (D->fragment[i].sxl == 0)
      sx = D->fragment[j].sx;
    else if (D->fragment[j].sxl == D->fragment[i].sxl)
      sx = NULL;
    else {
      free_sx = 1;
      sx = _GD_Strdup(D, D->fragment[j].sx);
      if (sx)
        sx[D->fragment[j].sxl - D->fragment[i].sxl] = 0;
      else
        goto WRITE_ERR;
    }
  }

  if (fprintf(stream, "%sINCLUDE ", (D->standards >= 5) ? "/" : "") < 0 ||
      _GD_StringEscapeise(stream, D->fragment[j].ename, 0, permissive,
        D->standards) < 0)
  {
    goto WRITE_ERR;
  }

  if (ns || px || sx)
    if (fputc(' ', stream) == EOF)
      goto WRITE_ERR;

  if (ns) {
    if (_GD_StringEscapeise(stream, ns, 0, permissive, D->standards) < 0)
      goto WRITE_ERR;
    if (fputc('.', stream) == EOF)
      goto WRITE_ERR;
  }

  /* An empty prefix must be written if there's a suffix, but no namespace */
  if (px || (sx && !ns)) {
    if (fputc(' ', stream) == EOF || _GD_StringEscapeise(stream, px, 0,
          permissive, D->standards) < 0)
    {
      goto WRITE_ERR;
    }
  }

  if (sx) {
    if (fputc(' ', stream) == EOF || _GD_StringEscapeise(stream, sx, 0,
          permissive, D->standards) < 0)
    {
      goto WRITE_ERR;
    }
  }

  if (fputc('\n', stream) == EOF)
    goto WRITE_ERR;

  if (free_sx)
    free(sx);

  dreturn("%i", 0);
  return 0;

WRITE_ERR:
  if (free_sx)
    free(sx);
  dreturn("%i", 1);
  return 1;
}

static void _GD_FlushFragment(DIRFILE* D, int i, int permissive)
{
  int j;
  FILE* stream;
  char buffer[GD_MAX_LINE_LENGTH];
  char temp_file[] = "format_XXXXXX";
  char* ptr;
  struct tm now;
  int fd;
  int pretty = 0;
  ssize_t max_len = 0;
  unsigned int u;
#ifdef HAVE_FCHMOD
  mode_t mode;
#endif
  struct stat stat_buf;
  time_t t;
  int dirfd = D->fragment[i].dirfd;

  dtrace("%p, %i, %i", D, i, permissive);

#ifdef HAVE_FCHMOD
  /* get the permissions of the old file */
  if (stat(D->fragment[i].cname, &stat_buf))
    mode = 0644;
  else
    mode = stat_buf.st_mode;
#endif

  /* open a temporary file */
  fd = _GD_MakeTempFile(D, dirfd, temp_file);
  if (fd == -1) {
    _GD_SetError(D, GD_E_IO, GD_E_IO_OPEN, NULL, 0, NULL);
    dreturnvoid();
    return;
  }
  stream = fdopen(fd, "wb+");
  if (stream == NULL) {
    _GD_SetError(D, GD_E_IO, GD_E_IO_OPEN, NULL, 0, NULL);
    dreturnvoid();
    return;
  }

  if (D->flags & GD_PRETTY_PRINT) {
    size_t t = 0;
    ssize_t m = 0;
    int n = 0;
    pretty = 1;
    for (u = 0; u < D->n_entries; ++u)
      if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
        ssize_t l = strlen(D->entry[u]->field);
        if (m < l)
          m = l;
        t += l;
        n++;
      }
    max_len = (n == 0) ? 0: 2 * t / n;
    if (max_len > m)
      max_len = m;
    if (max_len > GD_MAX_PRETTY_FIELD_WIDTH)
      max_len = GD_MAX_PRETTY_FIELD_WIDTH;
  }

  /* Introit */
  t = time(NULL);
  strftime(buffer, GD_MAX_LINE_LENGTH, "%c", gmtime_r(&t, &now));

  if (fprintf(stream, "# This is a dirfile format file.\n"
        "# It was written using version %s of the GetData Library.\n"
        "# Written on %s UTC", GD_GETDATA_VERSION, buffer) < 0)
  {
    goto WRITE_ERR;
  }

  ptr = getenv("LOGNAME");
  if (ptr == NULL)
    ptr = getenv("USER");

  if (ptr != NULL) {
    if (fprintf(stream, " by %s", ptr) < 0)
      goto WRITE_ERR;
    if ((ptr = getenv("HOSTNAME")) != NULL)
      if (fprintf(stream, "@%s", ptr) < 0)
        goto WRITE_ERR;
  }
  if (fputs(".\n\n", stream) == EOF)
    goto WRITE_ERR;

  /* version */
  if (permissive) {
    if (fprintf(stream, "# WARNING: This file may not conform to any "
          "Dirfile Standards Version.\n") < 0)
    {
      goto WRITE_ERR;
    }
  } else if (D->standards >= 5) {
    if (fprintf(stream, "/VERSION %i\n", D->standards) < 0)
      goto WRITE_ERR;
  } else
    if (fprintf(stream,
          "# This file conforms to Dirfile Standards Version %i.\n",
          D->standards) < 0)
    {
      goto WRITE_ERR;
    }

  /* Byte Sex */
  if (permissive || D->standards >= 5)
    if (fprintf(stream, "/ENDIAN %s%s\n",
          (D->fragment[i].byte_sex & GD_LITTLE_ENDIAN) ? "little" : "big",
          ((permissive || D->standards >= 8) &&
           (D->fragment[i].byte_sex & GD_ARM_FLAG) == GD_ARM_ENDIAN) ? " arm" :
          "") < 0)
    {
      goto WRITE_ERR;
    }

  /* Protection */
  if (permissive || D->standards >= 6) {
    if (D->fragment[i].protection == GD_PROTECT_NONE) {
      if (fputs("/PROTECT none\n", stream) == EOF)
        goto WRITE_ERR;
    } else if (D->fragment[i].protection == GD_PROTECT_FORMAT) {
      if (fputs("/PROTECT format\n", stream) == EOF)
        goto WRITE_ERR;
    } else if (D->fragment[i].protection == GD_PROTECT_DATA) {
      if (fputs("/PROTECT data\n", stream) == EOF)
        goto WRITE_ERR;
    } else {
      if (fputs("/PROTECT all\n", stream) == EOF)
        goto WRITE_ERR;
    }
  }

  /* Frame offset */
  if (permissive || D->standards >= 1)
    if (D->fragment[i].frame_offset != 0)
      if (fprintf(stream, "%sFRAMEOFFSET %" PRIu64 "\n",
            (D->standards >= 5) ? "/" : "", D->fragment[i].frame_offset) < 0)
      {
        goto WRITE_ERR;
      }

  /* Encoding */
  if (permissive || D->standards >= 6) {
    const char *encoding = NULL;
    int use_encdat = 0;
    switch(D->fragment[i].encoding) {
      case GD_UNENCODED:
        encoding = "none";
        break;
      case GD_BZIP2_ENCODED:
        encoding = "bzip2";
        break;
      case GD_GZIP_ENCODED:
        encoding = "gzip";
        break;
      case GD_LZMA_ENCODED:
        encoding = "lzma";
        break;
      case GD_SLIM_ENCODED:
        encoding = "slim";
        break;
      case GD_TEXT_ENCODED:
        encoding = "text";
        break;
      case GD_SIE_ENCODED:
        encoding = "sie";
        break;
      case GD_ZZIP_ENCODED:
        encoding = "zzip";
        use_encdat = 1;
        break;
      case GD_ZZSLIM_ENCODED:
        encoding = "zzslim";
        use_encdat = 1;
        break;
      case GD_FLAC_ENCODED:
        encoding = "flac";
        break;
      case GD_AUTO_ENCODED: /* an unresolved, auto-encoded fragment */
        break;
      default:
        if (fprintf(stream, "/ENCODING unknown # (%lx)\n",
              D->fragment[i].encoding) < 0)
        {
          goto WRITE_ERR;
        }
        break;
    }

    if (encoding != NULL) {
      if (use_encdat && D->fragment[i].enc_data) {

        if (fprintf(stream, "/ENCODING %s %s\n", encoding,
              (char*)D->fragment[i].enc_data) < 0)
        {
          goto WRITE_ERR;
        }
      } else {
        if (fprintf(stream, "/ENCODING %s\n", encoding) < 0)
          goto WRITE_ERR;
      }
    }
  }

  /* The includes */
  if (permissive || D->standards >= 3) {
    size_t ns_offset = D->fragment[i].nsl;
    if (ns_offset)
      ns_offset++; /* for the dot */

    for (j = 0; j < D->n_fragment; ++j)
      if (D->fragment[j].parent == i)
        if (WriteInclude(D, i, j, ns_offset, permissive, stream))
          goto WRITE_ERR;
  }

  /* The fields */
  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
      if (_GD_FieldSpec(D, stream, D->entry[u], i, 0, max_len, pretty,
            permissive) < 0)
      {
        goto WRITE_ERR;
      }
      if (permissive || D->standards >= 6)
        for (j = 0; j < D->entry[u]->e->n_meta; ++j)
          if (_GD_FieldSpec(D, stream, D->entry[u]->e->p.meta_entry[j], i, 1, 0,
                pretty, permissive) < 0)
          {
            goto WRITE_ERR;
          }
    }

  /* REFERENCE is written at the end, because its effect can propagate
   * upwards.  In the WriteFieldCode call, early is always zero because the
   * REFERENCE directive appeared in DSV 6 */
  if (permissive || D->standards >= 6)
    if (D->fragment[i].ref_name != NULL) {
      if (fputs("/REFERENCE ", stream) == EOF ||
          _GD_WriteFieldCode(D, stream, i, D->fragment[i].ref_name, 0,
            permissive, D->standards, GD_WFC_NAME) < 0 ||
          fputc('\n', stream) == EOF)
      {
        goto WRITE_ERR;
      }
    }

  /* That's all */
#ifdef HAVE_FCHMOD
  if (fchmod(fd, mode))
    goto WRITE_ERR;
#endif

  /* if there's no error, try closing */
  if (ferror(stream)) {
WRITE_ERR:
    if (!D->error)
      _GD_SetError(D, GD_E_IO, GD_E_IO_WRITE, NULL, 0, NULL);
  }

  if (fclose(stream) == EOF) {
    if (!D->error)
      _GD_SetError(D, GD_E_IO, GD_E_IO_WRITE, NULL, 0, NULL);
  }

  /* If no error was encountered, move the temporary file over the
   * old format file, otherwise abort */

  /* Only assume we've synced the file if the rename succeeds */
  if (D->error != GD_E_OK)
    gd_UnlinkAt(D, dirfd, temp_file, 0);
  else if (gd_RenameAt(D, dirfd, temp_file, dirfd, D->fragment[i].bname)) {
    _GD_SetError(D, GD_E_IO, GD_E_IO_WRITE, NULL, 0, NULL);
    gd_UnlinkAt(D, dirfd, temp_file, 0);
  } else
    D->fragment[i].modified = 0;

  /* update mtime, if successful */
  if (!D->error && gd_StatAt(D, dirfd, D->fragment[i].bname, &stat_buf, 0) == 0)
    D->fragment[i].mtime = stat_buf.st_mtime;

  dreturnvoid();
}

void _GD_FlushMeta(DIRFILE* D, int fragment, int force)
{
  int i;

  dtrace("%p, %i, %i", D, fragment, force);

  if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i)
      if (force || D->fragment[i].modified)
        _GD_FlushFragment(D, i, D->flags & GD_NOSTANDARD);
  } else if (force || D->fragment[fragment].modified)
    _GD_FlushFragment(D, fragment, D->flags & GD_NOSTANDARD);

  dreturnvoid();
}

int gd_metaflush(DIRFILE* D)
{
  dtrace("%p", D);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);

  _GD_FlushMeta(D, GD_ALL_FRAGMENTS, 0);

  GD_RETURN_ERROR(D);
}

int gd_rewrite_fragment(DIRFILE* D, int fragment)
{
  dtrace("%p, %i", D, fragment);

  GD_RETURN_ERR_IF_INVALID(D);

  if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment)
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
  else if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else
    _GD_FlushMeta(D, fragment, 1);

  GD_RETURN_ERROR(D);
}

static int _GD_SyncOrClose(DIRFILE* D, const char* field_code, int syn, int clo)
{
  unsigned int i;
  gd_entry_t *E;

  dtrace("%p, \"%s\", %i, %i", D, field_code, syn, clo);

  GD_RETURN_ERR_IF_INVALID(D);

  if (field_code == NULL) {
    _GD_FlushMeta(D, GD_ALL_FRAGMENTS, 0);
    if (!D->error)
      for (i = 0; i < D->n_entries; ++i)
        if (D->entry[i]->field_type == GD_RAW_ENTRY)
          _GD_Flush(D, D->entry[i], syn, clo);
  } else {
    E = _GD_FindEntry(D, field_code);

    if (!D->error)
      _GD_Flush(D, E, syn, clo);
  }

  GD_RETURN_ERROR(D);
}

int gd_sync(DIRFILE *D, const char *field_code)
{
  int ret;

  dtrace("%p, \"%s\"", D, field_code);

  ret = _GD_SyncOrClose(D, field_code, 1, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_raw_close(DIRFILE *D, const char *field_code)
{
  int ret;

  dtrace("%p, \"%s\"", D, field_code);

  ret = _GD_SyncOrClose(D, field_code, 0, 1);

  dreturn("%i", ret);
  return ret;
}

int gd_flush(DIRFILE *D, const char *field_code)
{
  int ret;

  dtrace("%p, \"%s\"", D, field_code);

  ret = _GD_SyncOrClose(D, field_code, 1, 1);

  dreturn("%i", ret);
  return ret;
}

#define GD_VERS_GE_1   0xFFFFFFFEUL
#define GD_VERS_GE_2   0xFFFFFFFCUL
#define GD_VERS_GE_3   0xFFFFFFF8UL
#define GD_VERS_GE_4   0xFFFFFFF0UL
#define GD_VERS_GE_5   0xFFFFFFE0UL
#define GD_VERS_GE_6   0xFFFFFFC0UL
#define GD_VERS_GE_7   0xFFFFFF80UL
#define GD_VERS_GE_8   0xFFFFFF00UL
#define GD_VERS_GE_9   0xFFFFFE00UL
#define GD_VERS_GE_10  0xFFFFFC00UL

#define GD_VERS_LE_0   0x00000001UL
#define GD_VERS_LE_1   0x00000003UL
#define GD_VERS_LE_2   0x00000007UL
#define GD_VERS_LE_3   0x0000000fUL
#define GD_VERS_LE_4   0x0000001fUL
#define GD_VERS_LE_5   0x0000003fUL
#define GD_VERS_LE_6   0x0000007fUL
#define GD_VERS_LE_7   0x000000ffUL
#define GD_VERS_LE_8   0x000001ffUL
#define GD_VERS_LE_9   0x000003ffUL
#define GD_VERS_LE_10  0x000007ffUL

uint64_t _GD_FindVersion(DIRFILE *D)
{
  unsigned int i;
  char* ptr;
  dtrace("%p", D);

  D->av = (1 << (1 + GD_DIRFILE_STANDARDS_VERSION)) - 1;

  if (D->n_fragment > 1)
    D->av &= GD_VERS_GE_3;

  for (i = 0; D->av && i < (unsigned int)D->n_fragment; ++i) {
    if (D->fragment[i].px || D->fragment[i].sx)
      D->av &= GD_VERS_GE_9;
    else if (D->fragment[i].byte_sex & GD_ARM_FLAG)
      /* on an arm-endian platform, the arm flag is set by /ENDIAN directives
       * missing an "arm" token, but it's absense might mean either an "arm"
       * token was present, or else there was no /ENDIAN directive at all */
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
    if (D->entry[i]->flags & GD_EN_HIDDEN)
      D->av &= GD_VERS_GE_9;
    else
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
        case GD_SARRAY_ENTRY:
        case GD_INDIR_ENTRY:
        case GD_SINDIR_ENTRY:
          D->av &= GD_VERS_GE_10;
          break;
        case GD_BIT_ENTRY:
          if (D->entry[i]->EN(bit,numbits) > 1)
            D->av &= GD_VERS_GE_1;
          else if (D->entry[i]->EN(bit,bitnum) + D->entry[i]->EN(bit,numbits)
              - 1 > 32)
          {
            D->av &= GD_VERS_GE_5;
          }
          break;
        case GD_ALIAS_ENTRY:
        case GD_WINDOW_ENTRY:
        case GD_MPLEX_ENTRY:
          D->av &= GD_VERS_GE_9;
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
          if (D->entry[i]->flags & GD_EN_EARLY)
            D->av &= GD_VERS_LE_5;
          else
            D->av &= GD_VERS_GE_10;
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
  dreturn("0x%04" PRIx64, D->av);
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

  GD_RETURN_ERR_IF_INVALID(D);

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
    GD_SET_RETURN_ERROR(D, GD_E_ARGUMENT, (D->av == 0) ? GD_E_ARG_NO_VERS :
        GD_E_ARG_BAD_VERS, NULL, vers, NULL);
  }

  D->standards = vers;
  dreturn("%i", vers);
  return vers;
}
/* vim: ts=2 sw=2 et tw=80
*/
