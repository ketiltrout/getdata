/* (C) 2003-2005 C. Barth Netterfield
 * (C) 2003-2005 Theodore Kisner
 * (C) 2005-2010 D. V. Wiebe
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
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#endif

static size_t _GD_DoRawOut(DIRFILE *D, gd_entry_t *E, off64_t s0,
    size_t ns, gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;
  void *databuffer;

  /* check protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", 0);
    return 0;
  }

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, s0, ns, data_type, data_in);

  if (s0 < D->fragment[E->fragment_index].frame_offset * E->EN(raw,spf)) {
    _GD_SetError(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  s0 -= D->fragment[E->fragment_index].frame_offset * E->EN(raw,spf);

  databuffer = _GD_Alloc(D, E->EN(raw,data_type), ns);

  if (databuffer == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  _GD_ConvertType(D, data_in, data_type, databuffer, E->EN(raw,data_type), ns);

  if (D->error) { /* bad input type */
    free(databuffer);
    dreturn("%i", 0);
    return 0;
  }

  if (!_GD_Supports(D, E, GD_EF_OPEN | GD_EF_SEEK | GD_EF_WRITE)) {
    dreturn("%i", 0);
    return 0;
  }

  if (_gd_ef[E->e->u.raw.file[0].encoding].ecor) {
    /* convert to/from middle-ended doubles */
    if ((E->EN(raw,data_type) == GD_FLOAT64 || E->EN(raw,data_type) ==
          GD_COMPLEX128) &&
        D->fragment[E->fragment_index].byte_sex & GD_ARM_FLAG)
    {
      _GD_ArmEndianise((uint64_t*)databuffer, E->EN(raw,data_type) & GD_COMPLEX,
          ns);
    }

    if (D->fragment[E->fragment_index].byte_sex &
#ifdef WORDS_BIGENDIAN
           GD_LITTLE_ENDIAN
#else
           GD_BIG_ENDIAN
#endif
           )
    {
      if (E->EN(raw,data_type) & GD_COMPLEX)
        _GD_FixEndianness((char *)databuffer, E->e->u.raw.size / 2, ns * 2);
      else
        _GD_FixEndianness((char *)databuffer, E->e->u.raw.size, ns);
    }
  }
  /* write data to file. */

  if (E->e->u.raw.file[0].fp < 0) {
    /* open file for reading / writing if not already opened */

    if (_GD_SetEncodedName(D, E->e->u.raw.file, E->e->u.raw.filebase, 0)) {
      dreturn("%i", 0);
      return 0;
    } else if ((*_gd_ef[E->e->u.raw.file[0].encoding].open)(E->e->u.raw.file,
          D->flags & GD_ACCMODE, 1))
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      dreturn("%i", 0);
      return 0;
    }
  }

  if ((*_gd_ef[E->e->u.raw.file[0].encoding].seek)(E->e->u.raw.file, s0,
        E->EN(raw,data_type), 1) == -1)
  {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      dreturn("%i", 0);
      return 0;
  }

  n_wrote = (*_gd_ef[E->e->u.raw.file[0].encoding].write)(E->e->u.raw.file,
      databuffer, E->EN(raw,data_type), ns);

  free(databuffer);

  dreturn("%zu", n_wrote);
  return n_wrote;
}

static size_t _GD_DoLinterpOut(DIRFILE* D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;
  int dir = -1, i;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, data_type,
      data_in);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  /* if the table is complex valued, we can't invert it */
  if (E->e->u.linterp.complex_table) {
    _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_COMPLEX, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if (E->e->u.linterp.table_len < 0) {
    _GD_ReadLinterpFile(D, E);
    if (D->error != GD_E_OK) {
      dreturn("%i", 0);
      return 0;
    }
  }

  /* Check whether the LUT is monotonic */
  if (E->e->u.linterp.table_monotonic == -1) {
    E->e->u.linterp.table_monotonic = 1;
    for (i = 1; i < E->e->u.linterp.table_len; ++i)
      if (E->e->u.linterp.lut[i].y.r != E->e->u.linterp.lut[i - 1].y.r) {
        if (dir == -1)
          dir = (E->e->u.linterp.lut[i].y.r > E->e->u.linterp.lut[i - 1].y.r);
        else if (dir != (E->e->u.linterp.lut[i].y.r >
              E->e->u.linterp.lut[i - 1].y.r))
        {
          E->e->u.linterp.table_monotonic = 0;
          break;
        }
      }
  }

  /* Can't invert a non-monotonic function */
  if (E->e->u.linterp.table_monotonic == 0) {
    _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_ANTITONIC, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  double *tmpbuf = (double *)_GD_Alloc(D, GD_FLOAT64, num_samp);
  if (tmpbuf == NULL) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ConvertType(D, data_in, data_type, tmpbuf, GD_FLOAT64, num_samp);

  if (D->error) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  /* Make the reverse lut */
  struct _gd_lut *tmp_lut = (struct _gd_lut *)malloc(E->e->u.linterp.table_len
      * sizeof(struct _gd_lut));
  if (tmp_lut == NULL) {
    free(tmpbuf);
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  for (i = 0; i < E->e->u.linterp.table_len; ++i) {
    tmp_lut[i].x = E->e->u.linterp.lut[i].y.r;
    tmp_lut[i].y.r = E->e->u.linterp.lut[i].x;
  }

  _GD_LinterpData(D, tmpbuf, GD_FLOAT64, 0, tmpbuf, num_samp, tmp_lut,
      E->e->u.linterp.table_len);

  free(tmp_lut);
  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->e->repr[0], first_samp,
      num_samp, GD_FLOAT64, tmpbuf);

  free(tmpbuf);

  dreturn("%zu", n_wrote);
  return n_wrote;
}

static size_t _GD_DoLincomOut(DIRFILE* D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;
  void* tmpbuf;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, data_type,
      data_in);

  /* we cannot write to LINCOM fields that are a linear combination */
  /* of more than one raw field (no way to know how to split data). */

  if (E->EN(lincom,n_fields) > 1) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, NULL, 0, E->field);
    dreturn("%i", 0);
    return 0;
  }

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  /* writeable copy */
  tmpbuf = _GD_Alloc(D, data_type, num_samp);

  if (tmpbuf == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  memcpy(tmpbuf, data_in, num_samp * GD_SIZE(data_type));

  if (E->comp_scal) {
#ifdef GD_NO_C99_API
    double cm[1][2];
    double cb[1][2];
    const double d = E->EN(lincom,cm)[0][0] * E->EN(lincom,cm)[0][0]
      + E->EN(lincom,cm)[0][1] * E->EN(lincom,cm)[0][1];

    cm[0][0] = E->EN(lincom,cm)[0][0] / d;
    cm[0][1] = -E->EN(lincom,cm)[0][1] / d;

    cb[0][0] = -(E->EN(lincom,cb)[0][0] * E->EN(lincom,cm)[0][0] +
        E->EN(lincom,cb)[0][1] * E->EN(lincom,cm)[0][1]) / d;
    cb[0][1] = -(E->EN(lincom,cb)[0][1] * E->EN(lincom,cm)[0][0] -
        E->EN(lincom,cb)[0][0] * E->EN(lincom,cm)[0][1]) / d;
    _GD_CLincomData(D, 1, tmpbuf, data_type, NULL, NULL, cm, cb, NULL,
        num_samp);
#else
    double complex cm = 1 / E->EN(lincom,cm)[0];
    double complex cb = -E->EN(lincom,cb)[0] / E->EN(lincom,cm)[0];
    _GD_CLincomData(D, 1, tmpbuf, data_type, NULL, NULL, &cm, &cb, NULL,
        num_samp);
#endif
  } else {
    double m = 1 / E->EN(lincom,m)[0];
    double b = -E->EN(lincom,b)[0] / E->EN(lincom,m)[0];
    _GD_LincomData(D, 1, tmpbuf, data_type, NULL, NULL, &m, &b, NULL, num_samp);
  }

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->e->repr[0], first_samp,
      num_samp, data_type, tmpbuf);
  free(tmpbuf);

  dreturn("%zu", n_wrote);
  return n_wrote;
}

static size_t _GD_DoBitOut(DIRFILE* D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t data_type, const void *data_in)
{
  uint64_t *tmpbuf;
  uint64_t *readbuf;
  size_t i, n_wrote;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, data_type,
      data_in);

  const uint64_t mask = (E->EN(bit,numbits) == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << E->EN(bit,numbits)) - 1;

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  tmpbuf = (uint64_t *)_GD_Alloc(D, GD_UINT64, num_samp);
  readbuf = (uint64_t *)_GD_Alloc(D, GD_UINT64, num_samp);

  if (tmpbuf == NULL || readbuf == NULL) {
    free(tmpbuf);
    free(readbuf);
    dreturn("%i", 0);
    return 0;
  }

  memset(tmpbuf, 0, sizeof(uint64_t) * num_samp);
  memset(readbuf, 0, sizeof(uint64_t) * num_samp);

  _GD_ConvertType(D, data_in, data_type, (void*)tmpbuf, GD_UINT64, num_samp);

  /* first, READ the field in so that we can change the bits    */
  _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp, GD_UINT64,
      readbuf);

  /* error encountered, abort */
  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* now go through and set the correct bits in each field value */
  for (i = 0; i < num_samp; i++)
    readbuf[i] = (readbuf[i] & ~(mask << E->EN(bit,bitnum))) |
      (tmpbuf[i] & mask) << E->EN(bit,bitnum);

  /* write the modified data out */
  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->e->repr[0], first_samp,
      num_samp, GD_UINT64, (void*)readbuf);

  free(readbuf);
  free(tmpbuf);

  dreturn("%zu", n_wrote);
  return n_wrote;
}

static size_t _GD_DoPhaseOut(DIRFILE* D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, data_type,
      data_in);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->e->repr[0], first_samp +
      E->EN(phase,shift), num_samp, data_type, data_in);

  dreturn("%zu", n_wrote);

  return n_wrote;
}

static size_t _GD_DoRecipOut(DIRFILE* D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;
  void* tmpbuf;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, data_type,
      data_in);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  /* writeable copy */
  tmpbuf = _GD_Alloc(D, data_type, num_samp);

  if (tmpbuf == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  memcpy(tmpbuf, data_in, num_samp * GD_SIZE(data_type));

  /* calculate x = a/y instead of y = a/x */
  if (E->comp_scal)
    _GD_CInvertData(D, tmpbuf, data_type, E->EN(recip,cdividend), num_samp);
  else
    _GD_InvertData(D, tmpbuf, data_type, E->EN(recip,dividend), num_samp);

  if (D->error) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->e->repr[0], first_samp,
      num_samp, data_type, tmpbuf);
  free(tmpbuf);

  dreturn("%zu", n_wrote);

  return n_wrote;
}

static size_t _GD_DoPolynomOut(DIRFILE* D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;
  void* tmpbuf;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, data_type,
      data_in);

  /* we cannot write to POLYNOM fields that are quadradic or higher order */

  if (E->EN(polynom,poly_ord) > 1) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, NULL, 0, E->field);
    dreturn("%i", 0);
    return 0;
  }

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  /* do the inverse scaling */
  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* writeable copy */
  tmpbuf = _GD_Alloc(D, data_type, num_samp);

  if (tmpbuf == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  memcpy(tmpbuf, data_in, num_samp * GD_SIZE(data_type));

  if (E->comp_scal) {
#ifdef GD_NO_C99_API
    double cm[1][2];
    double cb[1][2];
    const double d = E->EN(polynom,ca)[1][0] * E->EN(polynom,ca)[1][0]
      + E->EN(polynom,ca)[1][1] * E->EN(polynom,ca)[1][1];

    cm[0][0] = E->EN(polynom,ca)[1][0] / d;
    cm[0][1] = -E->EN(polynom,ca)[1][1] / d;

    cb[0][0] = -(E->EN(polynom,ca)[0][0] * E->EN(polynom,ca)[1][0] +
        E->EN(polynom,ca)[0][1] * E->EN(polynom,ca)[1][1]) / d;
    cb[0][1] = -(E->EN(polynom,ca)[0][1] * E->EN(polynom,ca)[1][0] -
        E->EN(polynom,ca)[0][0] * E->EN(polynom,ca)[1][1]) / d;
    _GD_CLincomData(D, 1, tmpbuf, data_type, NULL, NULL, cm, cb, NULL,
        num_samp);
#else
    double complex cm = 1 / E->EN(polynom,ca)[1];
    double complex cb = -E->EN(polynom,ca)[0] / E->EN(polynom,ca)[1];
    _GD_CLincomData(D, 1, tmpbuf, data_type, NULL, NULL, &cm, &cb, NULL,
        num_samp);
#endif
  } else {
    double m = 1 / E->EN(polynom,a)[1];
    double b = -E->EN(polynom,a)[0] / E->EN(polynom,a)[1];
    _GD_LincomData(D, 1, tmpbuf, data_type, NULL, NULL, &m, &b, NULL, num_samp);
  }

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->e->repr[0], first_samp,
      num_samp, data_type, tmpbuf);
  free(tmpbuf);

  dreturn("%zu", n_wrote);
  return n_wrote;
}

static size_t _GD_DoConstOut(DIRFILE* D, gd_entry_t *E, off64_t first,
    size_t len, gd_type_t data_type, const void *data_in)
{
  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first, len, data_type, data_in);

  /* check protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
  else {
    gd_type_t type = _GD_ConstType(D, E->EN(scalar,const_type));
    _GD_ConvertType(D, data_in, data_type, E->e->u.scalar.d + first *
        GD_SIZE(type), type, len);
  }

  if (D->error) { /* bad input type */
    dreturn("%i", 0);
    return 0;
  }

  D->fragment[E->fragment_index].modified = 1;

  dreturn("%i", 1);
  return 1;
}

static size_t _GD_DoStringOut(DIRFILE* D, gd_entry_t *E, const char *data_in)
{
  dtrace("%p, %p, %p", D, E, data_in);
  char* ptr = E->e->u.string;

  /* check protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", 0);
    return 0;
  }

  E->e->u.string = strdup(data_in);
  if (E->e->u.string == NULL) {
    E->e->u.string = ptr;
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }
  free(ptr);
  D->fragment[E->fragment_index].modified = 1;

  dreturn("%zu", strlen(E->e->u.string) + 1);
  return strlen(E->e->u.string) + 1;
}

size_t _GD_DoFieldOut(DIRFILE *D, gd_entry_t* E, int repr, off64_t first_samp,
    size_t num_samp, gd_type_t data_type, const void *data_in)
{
  size_t n_wrote = 0;

  dtrace("%p, %p, %i, %lli, %zu, 0x%x, %p", D, E, repr, first_samp, num_samp,
      data_type, data_in);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%i", 0);
    return 0;
  }

  if (!E->e->calculated)
    _GD_CalculateEntry(D, E);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* writing to representations is prohibited */
  if (repr != GD_REPR_NONE) {
    const char r[2] = {(char)repr, 0};
    _GD_SetError(D, GD_E_BAD_REPR, GD_E_REPR_PUT, NULL, 0, r);
    dreturn("%i", 0);
    return 0;
  }

  switch (E->field_type) {
    case GD_RAW_ENTRY:
      n_wrote = _GD_DoRawOut(D, E, first_samp, num_samp, data_type, data_in);
      break;
    case GD_LINTERP_ENTRY:
      n_wrote = _GD_DoLinterpOut(D, E, first_samp, num_samp, data_type,
          data_in);
      break;
    case GD_LINCOM_ENTRY:
      n_wrote = _GD_DoLincomOut(D, E, first_samp, num_samp, data_type, data_in);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      n_wrote = _GD_DoBitOut(D, E, first_samp, num_samp, data_type, data_in);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDEX_ENTRY:
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, NULL, 0, E->field);
      break;
    case GD_RECIP_ENTRY:
      n_wrote = _GD_DoRecipOut(D, E, first_samp, num_samp, data_type, data_in);
      break;
    case GD_PHASE_ENTRY:
      n_wrote = _GD_DoPhaseOut(D, E, first_samp, num_samp, data_type, data_in);
      break;
    case GD_POLYNOM_ENTRY:
      n_wrote = _GD_DoPolynomOut(D, E, first_samp, num_samp, data_type,
          data_in);
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
      n_wrote = _GD_DoConstOut(D, E, first_samp, num_samp, data_type, data_in);
      break;
    case GD_STRING_ENTRY:
      n_wrote = _GD_DoStringOut(D, E, (const char *)data_in);
      break;
    case GD_NO_ENTRY:
      _GD_InternalError(D);
      break;
  }

  D->recurse_level--;
  dreturn("%zu", n_wrote);
  return n_wrote;
}

/* this function is little more than a public boilerplate for _GD_DoFieldOut */
size_t gd_putdata64(DIRFILE* D, const char *field_code_in, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp, gd_type_t data_type,
    const void *data_in)
{
  size_t n_wrote = 0;
  gd_entry_t *entry;
  char* field_code;
  int repr;

  dtrace("%p, \"%s\", %lli, %lli, %zu, %zu, 0x%x, %p", D, field_code_in,
      first_frame, first_samp, num_frames, num_samp, data_type, data_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, field_code);

  if (field_code != field_code_in)
    free(field_code);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* get the samples per frame */
  gd_spf_t spf = _GD_GetSPF(D, entry);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  first_samp += spf * first_frame;
  num_samp += spf * num_frames;

  n_wrote = _GD_DoFieldOut(D, entry, repr, first_samp, num_samp, data_type,
      data_in);

  dreturn("%zu", n_wrote);
  return n_wrote;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
size_t gd_putdata(DIRFILE* D, const char *field_code, off_t first_frame,
    off_t first_samp, size_t num_frames, size_t num_samp, gd_type_t data_type,
    const void *data_in)
{
  return gd_putdata64(D, field_code, first_frame, first_samp, num_frames,
      num_samp, data_type, data_in);
}
/* vim: ts=2 sw=2 et
*/
