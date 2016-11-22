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

static unsigned int gd_max_(unsigned int A, unsigned int B)
{
  return (A > B) ? A : B;
}

#define GD_AS_FREE_SCALAR 1 /* Free the old scalar */
#define GD_AS_NEED_RECALC 2 /* Updated entry needs re-calculation */
#define GD_AS_ERROR       4 /* Error occurred */
#define GD_AS_MODIFIED    8 /* Updated entry has been modified */
/* _GD_AlterScalar: modify an entry scalar that can take a field code.
 *
 * There's two types of things to consider here:
 * - the literal (numeric) parameter (e.g. E->shfit for PHASE)
 * - the scalar field code (if any) (i.e. E->scalar and E->scalar_ind)
 *
 * There are bits and pieces of two entry objects here:
 * - Q: the entry object we're updating.  It started off as a copy of the
 *      old entry (E) and it's a mess
 * - N: the entry object containing the update request (from the caller)
 *      which may contain some special "keep-as-is" values
 *
 * Parameters:
 *  sN - scalar name in N (NULL means remove; "" means keep as-is)
 *  iN - scalar index in N
 *  lN - literal value in N
 *
 *  sQ - scalar name in Q
 *  iQ - scalar index in Q
 *  lQ - literal value in Q
 *
 *  calculated: whether E was calculated (GD_EN_CALC) before _GD_Change was
 *                          called.
 *
 *  alter_literal: boolean; true if lN and lQ differ and lN is not a special
 *                          "keep-as-is" value
 *
 *  early: whether E is from DSV <= 5 (GD_EN_EARLY)
 *
 * This function performs one of five actions, indicated by 0 through 4,
 * according to the values of sN, sQ, and alter_literal as given in 
 * the following table, where:
 *
 * - sQ can be 'N' (NULL) or 'a' (a non-NULL field code from the
 *                                                     original entry)
 * - sN can be 'N' (NULL), '-' (the empty string ""), or 'b' (a new
 *                                  field code provided by the caller)
 * - AL can be '0' or '1' indicating the boolean state of alter_literal
 *
 * sQ or AL can also be '*' meaning any permitted value
 *
 * sQ  sN  AL  ->  action
 *  N   N   0  ->  0: no change            (labelled 0a below)
 *  N   N   1  ->  1: set lQ from lN       (labelled 1a below)
 *  N   -   0  ->  0: no change            (labelled 0b below)
 *  N   -   1  ->  1: set lQ from lN       (labelled 1b below)
 *  a   N   0  ->  3: set lQ to the value of the entry specified
 *                    by sQ field and then delete sQ
 *  a   N   1  ->  4: set lQ to lN and delete sQ
 *  a   -   *  ->  0: no change            (labelled 0c below)
 *  *   b   *  ->  2: set sQ,iQ from sN,iN
 *
 * Flags returned by this function are based on action performed:
 *
 * GD_AS_FREE_SCALAR is set when sQ is changed (2) or deleted (3,4)
 * GD_AS_NEED_RECALC is set when sQ is changed (2)
 * GD_AS_MODIFIED is set when any of Q is changed (all but 0)
 *
 * Also note: iN is ignored if sN == "" (keep as-is), even if it is
 * different than iQ!
 */
static int _GD_AlterScalar(DIRFILE* D, int alter_literal, gd_type_t type,
    void *lQ, const void *lN, char **sQ, int *iQ, const char *sN,
    int iN, int calculated, int early, int fragment_index)
{
  int r = 0;
  int set_lQ = 0;

  dtrace("%p, %i, 0x%X, %p, %p, %p, %p, %p, %i, %i, %i, %i", D, alter_literal,
      type, lQ, lN, sQ, iQ, sN, iN, calculated, early, fragment_index);

  if (sN == NULL) {
    if (*sQ != NULL) {
      if (alter_literal) {
        /* 4: replace a CONST field with a literal scalar */
        r = GD_AS_FREE_SCALAR | GD_AS_MODIFIED;
        *sQ = NULL;
        set_lQ = 1;
      } else {
        /* 3: derefence a CONST field to turn it into a literal scalar
         *
         *    if E has been successfully calculated, lQ already has
         *    the right value, and all we need to do is delete sQ.
         *
         *    Otherwise we're in for some tricks:
         */
        if (!calculated) {
          /* We've not tried to fetch this scalar before.  Ideally we'd like
           * to run _GD_Calculate here, but we can't because Q is a mess right
           * now, being in the middle of an update.  It's also in two discon-
           * nected parts on the stack (Q, Qe), which makes even passing it to
           * other parts of the library sketchy at best.  Instead we call
           * _GD_GetScalar (which _GD_Calculate uses) directly.  It returns a
           * GD_E_BAD_SCALAR suberror which we convert into GD_E_BAD_CODE or
           * GD_E_BAD_FIELD_TYPE
           */
          int e = _GD_GetScalar(D, *sQ, iQ, type, lQ, NULL);
          if (e == GD_E_SCALAR_CODE) 
            _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, *sQ);
          else if (e == GD_E_SCALAR_TYPE)
            _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, *sQ);
        }
        r = GD_AS_FREE_SCALAR | GD_AS_MODIFIED;
        *sQ = NULL;
      }
    } else if (alter_literal) {
      /* 1a: set lQ from lN */
      set_lQ = 1;
    }
    /* otherwise 0a: do nothing */
  } else if (sN[0] == '\0') {
    if (*sQ == NULL && alter_literal) {
      /* 1b: set lQ from lN */
      set_lQ = 1;
    }
    /* otherwise 0b or 0c: do nothing */
  } else {
    /* 2: set a new CONST field from sN; if this is a RAW field, and we've
     *    been asked to move the raw file, _GD_Change is going to need to
     *    recalculate the entry; no need to change lQ: it's ignored. */

    if (_GD_CheckCodeAffixes(D, sN, fragment_index,
          GD_CO_ERROR | (early ? GD_CO_EARLY : 0)))
    {
      ; /* reject codes with bad affixes */
    } else if (iN == -1 && !(D->flags & GD_NOSTANDARD) && D->standards <= 7 &&
        _GD_TokToNum(sN, D->standards, 1, NULL, NULL, NULL, NULL) != -1)
    {
      /* when using early Standards, reject ambiguous field codes */
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_AMBIGUOUS, NULL, 0, sN);
    } else {
      r = GD_AS_FREE_SCALAR | GD_AS_NEED_RECALC | GD_AS_MODIFIED;
      *sQ = _GD_Strdup(D, sN);
      *iQ = iN;
    }
  }

  if (!D->error && set_lQ) {
    r |= GD_AS_MODIFIED;
    if (type == GD_INT64)
      *(int64_t *)lQ = *(int64_t *)lN;
    else if (type == GD_COMPLEX128)
      memcpy(lQ, lN, 2 * sizeof(double));
    else if (type == GD_FLOAT64)
      *(double *)lQ = *(double *)lN;
    else if (type == GD_INT16)
      *(int16_t *)lQ = *(int16_t *)lN;
    else if (type == GD_UINT16)
      *(uint16_t *)lQ = *(uint16_t *)lN;
    else if (type == GD_INT32)
      *(int32_t *)lQ = *(int32_t *)lN;
    else if (type == GD_UINT32)
      *(uint32_t *)lQ = *(uint32_t *)lN;
    else if (type == GD_UINT64)
      *(uint64_t *)lQ = *(uint64_t *)lN;
    else
      _GD_InternalError(D);
  }

  if (D->error)
    r |= GD_AS_ERROR;

  dreturn("0x%X", r);
  return r;
}

/* _GD_SPFConvert: this is the no-longer used AddData, cut down for use by
 * _GD_Change.  NB: Don't precompute (spfB / spfA) here: the order of operations
 * is important to get proper integer trucation.
 */
static void _GD_SPFConvert(DIRFILE* D, void *A, unsigned int spfA, void *B,
    unsigned int spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%X, %" PRIuSIZE, D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t *)A)[i] = ((int8_t *)B)[i * spfB / spfA];
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t *)A)[i] = ((uint8_t *)B)[i * spfB / spfA];
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t *)A)[i] = ((int16_t *)B)[i * spfB / spfA];
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t *)A)[i] = ((uint16_t *)B)[i * spfB / spfA];
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t *)A)[i] = ((int32_t *)B)[i * spfB / spfA];
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t *)A)[i] = ((uint32_t *)B)[i * spfB / spfA];
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t *)A)[i] = ((int64_t *)B)[i * spfB / spfA];
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t *)A)[i] = ((uint64_t *)B)[i * spfB / spfA];
      break;
    case GD_FLOAT32:
      for (i = 0; i < n; i++)
        ((float *)A)[i] = ((float *)B)[i * spfB / spfA];
      break;
    case GD_FLOAT64:
      for (i = 0; i < n; i++)
        ((double *)A)[i] = ((double *)B)[i * spfB / spfA];
      break;
    case GD_COMPLEX64:
      for (i = 0; i < n; i++)
        memcpy((float *)A + 2 * i, (float *)B + 2 * (i * spfB / spfA),
            2 * sizeof(float));
      break;
    case GD_COMPLEX128:
      for (i = 0; i < n; i++)
        memcpy((double *)A + 2 * i, (double *)B + 2 * (i * spfB / spfA),
            2 * sizeof(double));
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, type, NULL);
      break;
  }

  dreturnvoid();
}

/* returns -1 on error, 1 if modified, 0 if no change */
static int _GD_AlterInField(DIRFILE *D, int i, char **Q, char *const *N,
    char *const *E, int early, int fragment_index, int force)
{
  dtrace("%p, %i, %p, %p, %p, %i, %i, %i", D, i, Q, N, E, early, fragment_index,
      force);

  if (force || (N[i] != NULL && strcmp(E[i], N[i]))) {
    if (_GD_CheckCodeAffixes(D, N[i], fragment_index,
          GD_CO_ERROR | (early ? GD_CO_EARLY : 0)))
    {
      dreturn("%i", -1);
      return -1;
    } else if ((Q[i] = _GD_Strdup(D, N[i])) == NULL) {
      dreturn("%i", -1); 
      return -1;
    }

    dreturn("%i", 1);
    return 1;
  }

  dreturn("%i", 0);
  return 0;
}

/* N is the new entry, supplied by the user
 * E is the old entry, stored in the database
 * Q is our workspace; in the end, Q is a sanitised N which replaces E */
static int _GD_Change(DIRFILE *D, const char *field_code, const gd_entry_t *N,
    int flags)
{
  int i, j, early, calc;
  int field_free = 0;
  int scalar_free = 0;
  int modified = 0;
  size_t n;
  gd_type_t type;
  void *ptr;
  gd_entry_t *E = NULL;
  gd_entry_t Q;
  struct gd_private_entry_ Qe;

  dtrace("%p, \"%s\", %p, %i", D, field_code, N, flags);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if ((E = _GD_FindEntry(D, field_code)) == NULL)
    ; /* Error already set */
  else if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
  else if (E->field_type != N->field_type)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_MATCH, NULL, 0, field_code);

  if (D->error)
    GD_RETURN_ERROR(D);

  early = E->flags & GD_EN_EARLY ? 1 : 0;
  calc = E->flags & GD_EN_CALC ? 1 : 0;

  memcpy(&Qe, E->e, sizeof(struct gd_private_entry_));
  memcpy(&Q, E, sizeof(gd_entry_t));

  /* hiddenness can be changed here */
  if (N->flags & GD_EN_HIDDEN)
    Q.flags |= GD_EN_HIDDEN;
  else
    Q.flags &= ~GD_EN_HIDDEN;

  if ((Q.flags & GD_EN_HIDDEN) != (E->flags & GD_EN_HIDDEN))
    modified = 1;

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      j = _GD_AlterScalar(D, N->EN(raw,spf) && N->EN(raw,spf) != E->EN(raw,spf),
          GD_UINT_TYPE, &Q.EN(raw,spf), &N->EN(raw,spf), Q.scalar, Q.scalar_ind,
          N->scalar[0], N->scalar_ind[0], calc, early, E->fragment_index);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 1;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;

      Q.EN(raw,data_type) = (N->EN(raw,data_type) == GD_NULL) ?
        E->EN(raw,data_type) : N->EN(raw,data_type);

      /* nothing to do */
      if (Q.EN(raw,spf) == E->EN(raw,spf) && Q.EN(raw,data_type) ==
          E->EN(raw,data_type) && Q.scalar[0] == E->scalar[0])
      {
        break;
      }

      modified = 1;

      if (_GD_BadType(D->standards, Q.EN(raw,data_type)))
        GD_SET_RETURN_ERROR(D, GD_E_BAD_TYPE, 0, NULL, Q.EN(raw,data_type),
            NULL);

      if (flags) {
        ssize_t nread, nwrote;
        off64_t ns_out, nf;
        void *buffer1;
        void *buffer2;
        struct encoding_t *enc;

        if (!(Q.flags & GD_EN_CALC))
          if (gd_get_constant(D, Q.scalar[0], GD_UINT_TYPE, &Q.EN(raw,spf)))
            break;

        if (!calc)
          if (!_GD_CalculateEntry(D, E, 1))
            break;

        nf = GD_BUFFER_SIZE / gd_max_(E->e->u.raw.size,
            GD_SIZE(Q.EN(raw,data_type))) / gd_max_(E->EN(raw,spf),
            Q.EN(raw,spf));

        if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA)
          _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
              D->fragment[E->fragment_index].cname);
        else
          _GD_Supports(D, E, GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK |
              GD_EF_READ | GD_EF_WRITE | GD_EF_SYNC | GD_EF_UNLINK);

        if (D->error)
          break;

        enc = _GD_ef + E->e->u.raw.file[0].subenc;

        /* open the old file */
        if (_GD_InitRawIO(D, E, NULL, -1, NULL, 0, GD_FILE_READ,
              _GD_FileSwapBytes(D, E)))
        {
          break;
        } else if ((*enc->seek)(E->e->u.raw.file, 0, E->EN(raw,data_type),
              GD_FILE_READ) == -1)
        {
          _GD_SetEncIOError(D, GD_E_IO_READ, E->e->u.raw.file + 0);
        }

        if (D->error)
          break;

        /* Create a temporary file and open it */
        if (_GD_InitRawIO(D, E, NULL, -1, enc, 0, GD_FILE_WRITE | GD_FILE_TEMP,
              _GD_FileSwapBytes(D, E)))
          break;
        else if (_GD_DoSeek(D, E, enc, 0, GD_FILE_WRITE | GD_FILE_TEMP) == -1) {
          _GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DISCARD |
              GD_FINIRAW_CLOTEMP);
          break;
        }

        buffer1 = _GD_Malloc(D, GD_BUFFER_SIZE);
        buffer2 = _GD_Malloc(D, GD_BUFFER_SIZE);

        if (D->error) {
          free(buffer1);
          break;
        }

        /* Now copy the old file to the new file */
        for (;;) {
          nread = (*enc->read)(E->e->u.raw.file, buffer1, E->EN(raw,data_type),
              nf * E->EN(raw,spf));

          if (nread < 0) {
            _GD_SetEncIOError(D, GD_E_IO_READ, E->e->u.raw.file + 1);
            break;
          }

          if (nread == 0)
            break;

          ns_out = nread * Q.EN(raw,spf) / E->EN(raw,spf);

          /* spf convert */
          if (Q.EN(raw,spf) != E->EN(raw,spf))
            _GD_SPFConvert(D, buffer2, Q.EN(raw,spf), buffer1, E->EN(raw,spf),
                E->EN(raw,data_type), ns_out);
          else {
            ptr = buffer1;
            buffer1 = buffer2;
            buffer2 = ptr;
          }

          /* type convert */
          if (Q.EN(raw,data_type) != E->EN(raw,data_type))
            _GD_ConvertType(D, buffer2, E->EN(raw,data_type), buffer1,
                Q.EN(raw,data_type), ns_out);
          else {
            ptr = buffer1;
            buffer1 = buffer2;
            buffer2 = ptr;
          }

          nwrote = _GD_WriteOut(E, enc, buffer1, Q.EN(raw,data_type), ns_out,
              1);

          if (nwrote < ns_out) {
            _GD_SetEncIOError(D, GD_E_IO_WRITE, E->e->u.raw.file + 1);
            break;
          }
        }

        free(buffer1);
        free(buffer2);

        if (D->error)
          /* An error occurred, delete the temporary file (the old
           * file can stay open) */
          _GD_FiniRawIO(D, E, E->fragment_index,
              GD_FINIRAW_CLOTEMP | GD_FINIRAW_DISCARD);
        else {
          /* discard the old file and move the temporary file into place */
          if (_GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DISCARD) == 0)
            _GD_FiniRawIO(D, E, E->fragment_index,
                GD_FINIRAW_KEEP | GD_FINIRAW_CLOTEMP);
        }
      }
      memcpy(Qe.u.raw.file, E->e->u.raw.file, sizeof(struct gd_raw_file_));

      break;
    case GD_LINCOM_ENTRY:
      Q.EN(lincom,n_fields) = (N->EN(lincom,n_fields) == 0) ?
        E->EN(lincom,n_fields) : N->EN(lincom,n_fields);
      if (Q.EN(lincom,n_fields) < 1 || Q.EN(lincom,n_fields) > GD_MAX_LINCOM) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL,
            E->EN(lincom,n_fields), NULL);
        break;
      }

      if (Q.EN(lincom,n_fields) != E->EN(lincom,n_fields)) {
        modified = 1;
        if (Q.EN(lincom,n_fields) < E->EN(lincom,n_fields))
          for (i = Q.EN(lincom,n_fields); i < E->EN(lincom,n_fields); ++i) {
            field_free |= 1 << i;
            scalar_free |= 1 << i;
            scalar_free |= 1 << (i + GD_MAX_LINCOM);
          }
      }

      Q.flags &= ~GD_EN_COMPSCAL;

      for (i = 0; i < Q.EN(lincom,n_fields); ++i) {
        if (flags & 0x1) {
          j = _GD_AlterInField(D, i, Q.in_fields, N->in_fields, E->in_fields,
              early, E->fragment_index, E->EN(lincom,n_fields) <= i);
          if (j < 0)
            break;
          else if (j) {
            modified = 1;
            field_free |= 1 << i;
          }
        }

        if (flags & 0x2) {
          if (N->flags & GD_EN_COMPSCAL) {
            j = _GD_AlterScalar(D, !gd_ccmpc_(E->EN(lincom,cm)[i],
                  N->EN(lincom,cm)[i]), GD_COMPLEX128, Q.EN(lincom,cm) + i,
                N->EN(lincom,cm) + i, Q.scalar + i, Q.scalar_ind + i,
                N->scalar[i], N->scalar_ind[i], calc, early, E->fragment_index);
            Q.EN(lincom,m)[i] = creal(Q.EN(lincom,cm)[i]);
          } else {
            j = _GD_AlterScalar(D, E->EN(lincom,m)[i] != N->EN(lincom,m)[i],
                GD_FLOAT64, Q.EN(lincom,m) + i, N->EN(lincom,m) + i,
                Q.scalar + i, Q.scalar_ind + i, N->scalar[i], N->scalar_ind[i],
                calc, early, E->fragment_index);
            gd_rs2cs_(Q.EN(lincom,cm)[i], Q.EN(lincom,m)[i]);
          }

          if (j & GD_AS_ERROR)
            break;
          if (j & GD_AS_FREE_SCALAR)
            scalar_free |= 1 << i;
          if (j & GD_AS_MODIFIED)
            modified = 1;
          if (j & GD_AS_NEED_RECALC)
            Q.flags &= ~GD_EN_CALC;
        }

        if (flags & 0x4) {
          if (N->flags & GD_EN_COMPSCAL) {
            j = _GD_AlterScalar(D, !gd_ccmpc_(E->EN(lincom,cb)[i],
                  N->EN(lincom,cb)[i]), GD_COMPLEX128, Q.EN(lincom,cb) + i,
                N->EN(lincom,cb) + i, Q.scalar + i + GD_MAX_LINCOM,
                Q.scalar_ind + i + GD_MAX_LINCOM, N->scalar[i +  GD_MAX_LINCOM],
                N->scalar_ind[i + GD_MAX_LINCOM], calc, early,
                E->fragment_index);
            Q.EN(lincom,b)[i] = creal(Q.EN(lincom,cb)[i]);
          } else {
            j = _GD_AlterScalar(D, E->EN(lincom,b)[i] != N->EN(lincom,b)[i],
                GD_FLOAT64, Q.EN(lincom,b) + i, N->EN(lincom,b) + i,
                Q.scalar + i + GD_MAX_LINCOM, Q.scalar_ind + i + GD_MAX_LINCOM,
                N->scalar[i + GD_MAX_LINCOM], N->scalar_ind[i + GD_MAX_LINCOM],
                calc, early, E->fragment_index);
            gd_rs2cs_(Q.EN(lincom,cb)[i], Q.EN(lincom,b)[i]);
          }

          if (j & GD_AS_ERROR)
            break;
          if (j & GD_AS_FREE_SCALAR)
            scalar_free |= 1 << (i + GD_MAX_LINCOM);
          if (j & GD_AS_MODIFIED)
            modified = 1;
          if (j & GD_AS_NEED_RECALC)
            Q.flags &= ~GD_EN_CALC;
        }

        if (cimag(Q.EN(lincom,cm)[i]) || cimag(Q.EN(lincom,cb)[i]))
          Q.flags |= GD_EN_COMPSCAL;
      }

      if ((Q.flags & GD_EN_COMPSCAL) != (E->flags & GD_EN_COMPSCAL))
        modified = 1;
      break;
    case GD_LINTERP_ENTRY:
      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free = 1;
      }

      if (N->EN(linterp,table) != NULL && strcmp(E->EN(linterp,table),
            N->EN(linterp,table)))
      {
        Q.EN(linterp,table) = _GD_Strdup(D, N->EN(linterp,table));
        Qe.u.linterp.table_file = NULL;
        Qe.u.linterp.table_len = -1; /* not read yet */

        if (Q.EN(linterp,table) == NULL)
          break;

        if (flags) {
          if (E->e->u.linterp.table_file == NULL)
            if (_GD_SetTablePath(D, E, E->e))
              break;

          if (Qe.u.linterp.table_file == NULL)
            if (_GD_SetTablePath(D, &Q, &Qe))
              break;

          if (gd_RenameAt(D, E->e->u.linterp.table_dirfd,
                E->e->u.linterp.table_file, Qe.u.linterp.table_dirfd,
                Qe.u.linterp.table_file))
          {
            _GD_ReleaseDir(D, Qe.u.linterp.table_dirfd);
            _GD_SetError(D, GD_E_IO, GD_E_IO_RENAME, E->EN(linterp,table), 0,
                NULL);
            break;
          }
        }

        modified = 1;
        free(E->EN(linterp,table));
        free(E->e->u.linterp.table_file);
        if (E->e->u.linterp.table_dirfd > 0)
          _GD_ReleaseDir(D, E->e->u.linterp.table_dirfd);
        free(E->e->u.linterp.lut);
      }

      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      j = _GD_AlterScalar(D, N->EN(bit,numbits) >= 1 && E->EN(bit,numbits) !=
          N->EN(bit,numbits), GD_INT_TYPE, &Q.EN(bit,numbits),
          &N->EN(bit,numbits), Q.scalar + 1, Q.scalar_ind + 1, N->scalar[1],
          N->scalar_ind[1], calc, early, E->fragment_index);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 2;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterScalar(D, N->EN(bit,bitnum) >= 0 && E->EN(bit,bitnum) !=
          N->EN(bit,bitnum), GD_INT_TYPE, &Q.EN(bit,bitnum), &N->EN(bit,bitnum),
          Q.scalar, Q.scalar_ind, N->scalar[0], N->scalar_ind[0], calc, early,
          E->fragment_index);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 1;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free = 1;
      }

      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free = 1;
      }

      j = _GD_AlterInField(D, 1, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free |= 2;
      }

      break;
    case GD_RECIP_ENTRY:
      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free = 1;
      }

      Q.flags &= ~GD_EN_COMPSCAL;
      if (N->flags & GD_EN_COMPSCAL) {
        j = _GD_AlterScalar(D, cabs(N->EN(recip,cdividend)) != 0 &&
            !gd_ccmpc_(E->EN(recip,cdividend), N->EN(recip,cdividend)),
            GD_COMPLEX128, &Q.EN(recip,cdividend), &(N->EN(recip,cdividend)),
            Q.scalar, Q.scalar_ind, N->scalar[0], N->scalar_ind[0], calc, early,
            E->fragment_index);
        Q.EN(recip,dividend) = creal(Q.EN(recip,cdividend));
        if (cimag(Q.EN(recip,cdividend)) != 0)
          Q.flags |= GD_EN_COMPSCAL;
      } else {
        j = _GD_AlterScalar(D, N->EN(recip,dividend) != 0 &&
            E->EN(recip,dividend) != N->EN(recip,dividend), GD_FLOAT64,
            &Q.EN(recip,dividend), &(N->EN(recip,dividend)), Q.scalar,
            Q.scalar_ind, N->scalar[0], N->scalar_ind[0], calc, early,
            E->fragment_index);
        gd_rs2cs_(Q.EN(recip,cdividend), Q.EN(recip,dividend));
      }

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free = 1;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      if ((Q.flags & GD_EN_COMPSCAL) != (E->flags & GD_EN_COMPSCAL))
        modified = 1;

      break;
    case GD_PHASE_ENTRY:
      j = _GD_AlterScalar(D, E->EN(phase,shift) != N->EN(phase,shift), GD_INT64,
          &Q.EN(phase,shift), &N->EN(phase,shift), Q.scalar, Q.scalar_ind,
          N->scalar[0], N->scalar_ind[0], calc, early, E->fragment_index);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free = 1;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free = 1;
      }

      break;
    case GD_POLYNOM_ENTRY:
      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free = 1;
      }

      Q.EN(polynom,poly_ord) = (N->EN(polynom,poly_ord) == 0) ?
        E->EN(polynom,poly_ord) : N->EN(polynom,poly_ord);
      if (Q.EN(polynom,poly_ord) != E->EN(polynom,poly_ord))
        modified = 1;

      if (flags & 0x1) {
        Q.flags &= ~GD_EN_COMPSCAL;

        for (i = 0; i <= Q.EN(polynom,poly_ord); ++i) {
          if (N->flags & GD_EN_COMPSCAL) {
            j = _GD_AlterScalar(D, !gd_ccmpc_(E->EN(polynom,ca)[i],
                  N->EN(polynom,ca)[i]), GD_COMPLEX128, Q.EN(polynom,ca) + i,
                N->EN(polynom,ca) + i, Q.scalar + i, Q.scalar_ind + i,
                N->scalar[i], N->scalar_ind[i], calc, early, E->fragment_index);
            Q.EN(polynom,a)[i] = creal(Q.EN(polynom,ca)[i]);
          } else {
            j = _GD_AlterScalar(D, E->EN(polynom,a)[i] != N->EN(polynom,a)[i],
                GD_FLOAT64, Q.EN(polynom,a) + i, N->EN(polynom,a) + i,
                Q.scalar + i, Q.scalar_ind + i, N->scalar[i], N->scalar_ind[i],
                calc, early, E->fragment_index);
            gd_rs2cs_(Q.EN(polynom,ca)[i], Q.EN(polynom,a)[i]);
          }

          if (j & GD_AS_FREE_SCALAR)
            scalar_free |= 1 << i;
          if (j & GD_AS_NEED_RECALC)
            Q.flags &= ~GD_EN_CALC;
          if (j & GD_AS_MODIFIED)
            modified = 1;

          if (cimag(Q.EN(polynom,ca)[i]))
            Q.flags |= GD_EN_COMPSCAL;
        }

        if ((Q.flags & GD_EN_COMPSCAL) != (E->flags & GD_EN_COMPSCAL))
          modified = 1;
      }

      break;
    case GD_WINDOW_ENTRY:
      if (N->EN(window,windop) == GD_WINDOP_UNK)
        Q.EN(window,windop) = E->EN(window,windop);
      else {
        Q.EN(window,windop) = N->EN(window,windop);
        modified = 1;
      }

      switch (Q.EN(window,windop)) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          j = _GD_AlterScalar(D, E->EN(window,threshold.i) !=
              N->EN(window,threshold.i), GD_INT64, &Q.EN(window,threshold.i),
              &N->EN(window,threshold.i), Q.scalar, Q.scalar_ind, N->scalar[0],
              N->scalar_ind[0], calc, early, E->fragment_index);
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          j = _GD_AlterScalar(D, E->EN(window,threshold.u) !=
              N->EN(window,threshold.u), GD_UINT64, &Q.EN(window,threshold.u),
              &N->EN(window,threshold.u), Q.scalar, Q.scalar_ind, N->scalar[0],
              N->scalar_ind[0], calc, early, E->fragment_index);
          break;
        default:
          j = _GD_AlterScalar(D, E->EN(window,threshold.r) !=
              N->EN(window,threshold.r), GD_FLOAT64, &Q.EN(window,threshold.r),
              &N->EN(window,threshold.r), Q.scalar, Q.scalar_ind, N->scalar[0],
              N->scalar_ind[0], calc, early, E->fragment_index);
          break;
      }

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 1;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free |= 1;
      }

      j = _GD_AlterInField(D, 1, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free |= 2;
      }

      break;
    case GD_MPLEX_ENTRY:
      j = _GD_AlterScalar(D, N->EN(mplex,period) != -1 &&
          E->EN(mplex,period) != N->EN(mplex,period), GD_INT_TYPE,
          &Q.EN(mplex,period), &N->EN(mplex,period), Q.scalar + 1,
          Q.scalar_ind + 1, N->scalar[1], N->scalar_ind[1], calc, early,
          E->fragment_index);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 2;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterScalar(D, E->EN(mplex,count_val) != N->EN(mplex,count_val),
          GD_INT_TYPE, &Q.EN(mplex,count_val), &N->EN(mplex,count_val),
          Q.scalar, Q.scalar_ind, N->scalar[0], N->scalar_ind[0], calc, early,
          E->fragment_index);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 1;
      if (j & GD_AS_NEED_RECALC)
        Q.flags &= ~GD_EN_CALC;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterInField(D, 0, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free |= 1;
      }

      j = _GD_AlterInField(D, 1, Q.in_fields, N->in_fields, E->in_fields,
          early, E->fragment_index, 0);
      if (j < 0)
        break;
      else if (j) {
        modified = 1;
        field_free |= 2;
      }

      break;
    case GD_CONST_ENTRY:
      Q.EN(scalar,const_type) = (N->EN(scalar,const_type) == GD_NULL) ?
        E->EN(scalar,const_type) : N->EN(scalar,const_type);

      if (_GD_BadType(D->standards, Q.EN(scalar,const_type)))
        GD_SET_RETURN_ERROR(D, GD_E_BAD_TYPE, 0, NULL, Q.EN(scalar,const_type),
            NULL);

      type = _GD_ConstType(D, Q.EN(scalar,const_type));
      if (Q.EN(scalar,const_type) != E->EN(scalar,const_type))
        modified = 1;

      if (type == _GD_ConstType(D, E->EN(scalar,const_type)))
        Qe.u.scalar.d = E->e->u.scalar.d;
      else {
        /* type convert */
        Qe.u.scalar.d = _GD_Malloc(D, GD_SIZE(type));
        if (Qe.u.scalar.d == NULL)
          GD_RETURN_ERROR(D);

        if (type == GD_COMPLEX128) {
          *(double*)Qe.u.scalar.d = (E->EN(scalar,const_type) & GD_IEEE754) ?
            *(double*)E->e->u.scalar.d : (E->EN(scalar,const_type) & GD_SIGNED)
            ? (double)*(int64_t*)E->e->u.scalar.d
            : (double)*(uint64_t*)E->e->u.scalar.d;
          ((double*)Qe.u.scalar.d)[1] = 0;
        } else if (type == GD_FLOAT64)
          *(double*)Qe.u.scalar.d = (E->EN(scalar,const_type) & GD_COMPLEX) ?
            *(double*)E->e->u.scalar.d : (E->EN(scalar,const_type) & GD_SIGNED)
            ? (double)*(int64_t*)E->e->u.scalar.d
            : (double)*(uint64_t*)E->e->u.scalar.d;
        else if (type == GD_INT64)
          *(int64_t*)Qe.u.scalar.d = (E->EN(scalar,const_type) & (GD_COMPLEX |
                GD_IEEE754)) ? (int64_t)*(double*)E->e->u.scalar.d :
            (int64_t)*(uint64_t*)E->e->u.scalar.d;
        else
          *(uint64_t*)Qe.u.scalar.d = (E->EN(scalar,const_type) & (GD_COMPLEX |
                GD_IEEE754)) ? (uint64_t)*(double*)E->e->u.scalar.d :
            (uint64_t)*(int64_t*)E->e->u.scalar.d;
        free(E->e->u.scalar.d);
      }

      break;
    case GD_CARRAY_ENTRY:
      Q.EN(scalar,array_len) = (N->EN(scalar,array_len) == 0) ?
        E->EN(scalar,array_len) : N->EN(scalar,array_len);

      Q.EN(scalar,const_type) = (N->EN(scalar,const_type) == GD_NULL) ?
        E->EN(scalar,const_type) : N->EN(scalar,const_type);

      if (Q.EN(scalar,const_type) != E->EN(scalar,const_type) ||
          Q.EN(scalar,array_len) != E->EN(scalar,array_len))
      {
        modified = 1;

        if (_GD_BadType(D->standards, Q.EN(scalar,const_type))) {
          _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, Q.EN(scalar,const_type),
              NULL);
          break;
        }

        type = _GD_ConstType(D, Q.EN(scalar,const_type));
        Qe.u.scalar.d = _GD_Malloc(D, GD_SIZE(type) * Q.EN(scalar,array_len));
        if (Qe.u.scalar.d == NULL)
          break;
        memset(Qe.u.scalar.d, 0, GD_SIZE(type) * Q.EN(scalar,array_len));

        /* copy via type conversion, if array_len has increased, trailing
         * elements are uninitialised. */
        n = E->EN(scalar,array_len);
        if (n > Q.EN(scalar,array_len))
          n = Q.EN(scalar,array_len);

        _GD_ConvertType(D, E->e->u.scalar.d, _GD_ConstType(D,
              E->EN(scalar,const_type)), Qe.u.scalar.d, type, n);

        if (D->error) {
          free(Qe.u.scalar.d);
          break;
        }

        free(E->e->u.scalar.d);
      } else
        Qe.u.scalar.d = E->e->u.scalar.d;
      break;
    case GD_SARRAY_ENTRY:
      if (N->EN(scalar,array_len) == 0) {
        Q.EN(scalar,array_len) = E->EN(scalar,array_len);
        Qe.u.scalar.d = E->e->u.scalar.d;
      } else {
        Q.EN(scalar,array_len) = N->EN(scalar,array_len);

        if (Q.EN(scalar,array_len) != E->EN(scalar,array_len)) {
          modified = 1;

          if (Q.EN(scalar,array_len) < E->EN(scalar,array_len)) {
            /* Free dropped elements */
            size_t i;
            for (i = Q.EN(scalar,array_len); i < E->EN(scalar,array_len); ++i)
              free(((char**)E->e->u.scalar.d)[i]);
          }

          Qe.u.scalar.d = _GD_Realloc(D, E->e->u.scalar.d,
              sizeof(const char *) * Q.EN(scalar,array_len));
          if (Qe.u.scalar.d == NULL)
            break;

          if (Q.EN(scalar,array_len) > E->EN(scalar,array_len)) {
            /* Zero new elements */
            size_t i;
            for (i = E->EN(scalar,array_len); i < Q.EN(scalar,array_len); ++i)
              ((char**)Qe.u.scalar.d)[i] = _GD_Strdup(D, "");
          }
        } else
          Qe.u.scalar.d = E->e->u.scalar.d;
      }
      break;
    case GD_INDEX_ENTRY:
      /* INDEX may not be modified */
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_STRING_ENTRY:
      break;
  }

  if (D->error)
    GD_RETURN_ERROR(D);

  if (modified) {
    for (i = 0; i < GD_MAX_LINCOM; ++i) {
      if (field_free & (1 << i)) {
        Qe.entry[i] = NULL;
        free(E->in_fields[i]);
      }
    }

    for (i = 0; i <= GD_MAX_POLYORD; ++i) {
      if (scalar_free & (1 << i))
        free(E->scalar[i]);
    }

    if (E->field_type == GD_LINTERP_ENTRY && flags)
      _GD_ReleaseDir(D, Qe.u.linterp.table_dirfd);

    memcpy(E->e, &Qe, sizeof(struct gd_private_entry_));
    Q.e = E->e;
    memcpy(E, &Q, sizeof(gd_entry_t));
    D->fragment[E->fragment_index].modified = 1;
    D->flags &= ~GD_HAVE_VERSION;
  }

  dreturn("%i", 0);
  return 0;
}

int gd_alter_entry(DIRFILE* D, const char* field_code,
    const gd_entry_t *entry, int move)
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", %p, %i", D, field_code, entry, move);

  GD_RETURN_ERR_IF_INVALID(D);

  /* To ensure .e is NULLed */
  memcpy(&N, entry, sizeof(gd_entry_t));
  N.e = NULL;

  /* for these field types, move is a set of bitflags; we set them all */
  if (N.field_type == GD_LINCOM_ENTRY || N.field_type == GD_POLYNOM_ENTRY)
    move = 7;

  ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_raw(DIRFILE *D, const char *field_code,
    gd_type_t data_type, unsigned int spf, int move)
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", %u, 0x%X, %i", D, field_code, spf, data_type, move);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_RAW_ENTRY;
  N.EN(raw,spf) = spf;
  N.EN(raw,data_type) = data_type;
  N.scalar[0] = (spf == 0) ? (char *)"" : NULL;

  ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_lincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const double* m, const double* b) gd_nothrow
{
  gd_entry_t N;
  int i, ret;
  int flags = 0;

  dtrace("%p, \"%s\", %i, %p, %p, %p", D, field_code, n_fields, in_fields, m,
      b);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_LINCOM_ENTRY;
  if (n_fields > GD_MAX_LINCOM || n_fields < 0)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
  else if (n_fields != 0)
    N.EN(lincom,n_fields) = n_fields;
  else {
    gd_entry_t *E = _GD_FindEntry(D, field_code);

    if (E == NULL)
      GD_RETURN_ERROR(D);

    N.EN(lincom,n_fields) = E->EN(lincom,n_fields);
  }

  for (i = 0; i < N.EN(lincom,n_fields); ++i) {
    if (in_fields != NULL) {
      flags |= 1;
      N.in_fields[i] = (char *)in_fields[i];
    }

    if (m != NULL) {
      flags |= 2;
      N.EN(lincom,m)[i] = m[i];
      N.scalar[i] = NULL;
    } else
      N.scalar[i] = "";

    if (b != NULL) {
      flags |= 4;
      N.EN(lincom,b)[i] = b[i];
      N.scalar[i + GD_MAX_LINCOM] = NULL;
    } else
      N.scalar[i + GD_MAX_LINCOM] = "";
  }

  ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_clincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const GD_DCOMPLEXP(cm), const GD_DCOMPLEXP(cb))
  gd_nothrow
{
  gd_entry_t N;
  int i, ret;
  int flags = 0;

  dtrace("%p, \"%s\", %i, %p, %p, %p", D, field_code, n_fields, in_fields, cm,
      cb);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_LINCOM_ENTRY;
  N.flags = GD_EN_COMPSCAL;
  if (n_fields > GD_MAX_LINCOM || n_fields < 0)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
  else if (n_fields != 0)
    N.EN(lincom,n_fields) = n_fields;
  else {
    gd_entry_t *E = _GD_FindEntry(D, field_code);

    if (E == NULL)
      GD_RETURN_ERROR(D);

    N.EN(lincom,n_fields) = E->EN(lincom,n_fields);
  }

  for (i = 0; i < N.EN(lincom,n_fields); ++i) {
    if (in_fields != NULL) {
      flags |= 1;
      N.in_fields[i] = (char *)in_fields[i];
    }

    if (cm != NULL) {
      flags |= 2;
      gd_ca2cs_(N.EN(lincom,cm)[i], cm, i);
      N.scalar[i] = NULL;
    } else
      N.scalar[i] = "";

    if (cb != NULL) {
      flags |= 4;
      gd_ca2cs_(N.EN(lincom,cb)[i], cb, i);
      N.scalar[i + GD_MAX_LINCOM] = NULL;
    } else
      N.scalar[i + GD_MAX_LINCOM] = "";
  }

  ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_linterp(DIRFILE* D, const char* field_code, const char* in_field,
    const char* table, int move)
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field, table,
      move);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_LINTERP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(linterp,table) = (char *)table;

  ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_bit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %i, %i", D, field_code, in_field, bitnum,
      numbits);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_BIT_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(bit,bitnum) = bitnum;
  N.EN(bit,numbits) = numbits;
  N.scalar[0] = (bitnum == -1) ? (char *)"" : NULL;
  N.scalar[1] = (numbits == 0) ? (char *)"" : NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_sbit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %i, %i", D, field_code, in_field, bitnum,
      numbits);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_SBIT_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(bit,bitnum) = bitnum;
  N.EN(bit,numbits) = numbits;
  N.scalar[0] = (bitnum == -1) ? (char *)"" : NULL;
  N.scalar[1] = (numbits == 0) ? (char *)"" : NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_recip(DIRFILE* D, const char* field_code, const char* in_field,
    double dividend) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %g", D, field_code, in_field, dividend);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_RECIP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.scalar[0] = (dividend == 0) ? (char *)"" : NULL;
  N.EN(recip,dividend) = dividend;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

#ifndef GD_NO_C99_API
int gd_alter_crecip(DIRFILE* D, const char* field_code, const char* in_field,
    double complex cdividend)
{
  int ret;

  dtrace("%p, \"%s\", \"%s\", %g;%g", D, field_code, in_field, creal(cdividend),
      cimag(cdividend));

  ret = gd_alter_crecip89(D, field_code, in_field, (const double*)(&cdividend));

  dreturn("%i", ret);
  return ret;
}
#endif

int gd_alter_crecip89(DIRFILE* D, const char* field_code, const char* in_field,
    const double cdividend[2]) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %p={%g, %g}", D, field_code, in_field, cdividend,
      (cdividend == NULL) ? 0 : cdividend[0],
      (cdividend == NULL) ? 0 : cdividend[1]);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_RECIP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  if (cdividend == NULL) {
    N.scalar[0] = "";
    gd_li2cs_(N.EN(recip,cdividend), 0, 0);
  } else {
    N.scalar[0] = (cdividend[0] == 0 && cdividend[1] == 0) ? (char *)"" : NULL;
    gd_ra2cs_(N.EN(recip,cdividend), cdividend);
  }
  N.scalar_ind[0] = 0;
  N.flags = GD_EN_COMPSCAL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

static int _GD_AlterYoke(DIRFILE* D, gd_entype_t t, const char* field_code,
    const char* in_field1, const char* in_field2) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, 0x%X, \"%s\", \"%s\", \"%s\"", D, t, field_code, in_field1,
      in_field2);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = t;
  N.in_fields[0] = (char *)in_field1;
  N.in_fields[1] = (char *)in_field2;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_divide(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
  return _GD_AlterYoke(D, GD_DIVIDE_ENTRY, field_code, in_field1, in_field2);
}

int gd_alter_multiply(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
  return _GD_AlterYoke(D, GD_MULTIPLY_ENTRY, field_code, in_field1, in_field2);
}

int gd_alter_indir(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
  return _GD_AlterYoke(D, GD_INDIR_ENTRY, field_code, in_field1, in_field2);
}

int gd_alter_sindir(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
  return _GD_AlterYoke(D, GD_SINDIR_ENTRY, field_code, in_field1, in_field2);
}

int gd_alter_phase(DIRFILE* D, const char* field_code, const char* in_field,
    gd_shift_t shift) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %" PRId64, D, field_code, in_field, shift);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_PHASE_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(phase,shift) = shift;
  N.scalar[0] = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_const(DIRFILE* D, const char* field_code, gd_type_t const_type)
  gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", 0x%X", D, field_code, const_type);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_CONST_ENTRY;
  N.EN(scalar,const_type) = const_type;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_carray(DIRFILE* D, const char* field_code, gd_type_t const_type,
    size_t array_len) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", 0x%X, %" PRIuSIZE, D, field_code, const_type, array_len);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_CARRAY_ENTRY;
  N.EN(scalar,const_type) = const_type;
  N.EN(scalar,array_len) = array_len;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_sarray(DIRFILE* D, const char* field_code, size_t array_len)
  gd_nothrow
{
  gd_entry_t N;

  dtrace("%p, \"%s\", %" PRIuSIZE, D, field_code, array_len);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_SARRAY_ENTRY;
  N.EN(scalar,array_len) = array_len;

  _GD_Change(D, field_code, &N, 0);

  dreturn("%i", D->error);
  return D->error;
}

int gd_alter_polynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const double* a) gd_nothrow
{
  gd_entry_t N;
  int i, ret;
  int flags = 0;

  dtrace("%p, \"%s\", %i, \"%s\", %p", D, field_code, poly_ord, in_field, a);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_POLYNOM_ENTRY;
  if (poly_ord > GD_MAX_POLYORD || poly_ord < 0)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
  else if (poly_ord != 0)
    N.EN(polynom,poly_ord) = poly_ord;
  else {
    gd_entry_t *E = _GD_FindEntry(D, field_code);

    if (E == NULL)
      GD_RETURN_ERROR(D);

    N.EN(polynom,poly_ord) = E->EN(polynom,poly_ord);
  }
  N.in_fields[0] = (char *)in_field;

  if (a != NULL) {
    flags |= 1;
    for (i = 0; i <= N.EN(polynom,poly_ord); ++i) {
      N.EN(polynom,a)[i] = a[i];
      N.scalar[i] = NULL;
    }
  } else
    for (i = 0; i <= N.EN(polynom,poly_ord); ++i)
      N.scalar[i] = "";

  ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_cpolynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const GD_DCOMPLEXP(ca)) gd_nothrow
{
  gd_entry_t N;
  int i, ret;
  int flags = 0;

  dtrace("%p, \"%s\", %i, \"%s\", %p", D, field_code, poly_ord, in_field, ca);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_POLYNOM_ENTRY;
  N.flags = GD_EN_COMPSCAL;
  if (poly_ord > GD_MAX_POLYORD || poly_ord < 0)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
  else if (poly_ord != 0)
    N.EN(polynom,poly_ord) = poly_ord;
  else {
    gd_entry_t *E = _GD_FindEntry(D, field_code);

    if (E == NULL)
      GD_RETURN_ERROR(D);

    N.EN(polynom,poly_ord) = E->EN(polynom,poly_ord);
  }
  N.in_fields[0] = (char *)in_field;

  if (ca != NULL)  {
    flags |= 1;
    for (i = 0; i <= N.EN(polynom,poly_ord); ++i) {
      gd_ca2cs_(N.EN(polynom,ca)[i], ca, i);
      N.scalar[i] = NULL;
    }
  } else
    for (i = 0; i <= N.EN(polynom,poly_ord); ++i)
      N.scalar[i] = "";

  ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_window(DIRFILE* D, const char *field_code, const char *in_field,
    const char *check_field, gd_windop_t windop, gd_triplet_t threshold)
  gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, {%g,%" PRIX64 ",%" PRId64 "}", D,
      field_code, in_field, check_field, windop, threshold.r, threshold.u,
      threshold.i);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_WINDOW_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.in_fields[1] = (char *)check_field;
  N.EN(window,windop) = windop;
  N.EN(window,threshold) = threshold;
  N.scalar[0] = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_mplex(DIRFILE* D, const char *field_code, const char *in_field,
    const char *count_field, int count_val, int period) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %i", D, field_code, in_field,
      count_field, count_val, period);

  GD_RETURN_ERR_IF_INVALID(D);

  memset(&N, 0, sizeof(gd_entry_t));
  N.field_type = GD_MPLEX_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.in_fields[1] = (char *)count_field;
  N.EN(mplex,count_val) = count_val;
  N.EN(mplex,period) = period;
  N.scalar[0] = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

static int _GD_AlterSpec(DIRFILE* D, const char* line, const char* parent,
    const char *name, int move)
{
  char *outstring = NULL, *new_code;
  const char *tok_pos;
  size_t len0;
  char *in_cols[MAX_IN_COLS];
  int n_cols, ret;
  gd_entry_t *N = NULL;
  struct parser_state p;

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, line, parent, name, move);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);

  /* start parsing */
  _GD_SimpleParserInit(D, name, &p);
  n_cols = _GD_Tokenise(D, &p, line, &outstring, &tok_pos, MAX_IN_COLS,
      in_cols);

  /* Sanity check */
  if (!D->error && n_cols == 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, name, 0, NULL);

  if (D->error) {
    free(outstring);
    GD_RETURN_ERROR(D); /* tokeniser threw an error */
  }

  if (parent)
    N = _GD_FindEntry(D, parent);
  else
    N = _GD_FindEntry(D, in_cols[0]);

  if (D->error) {
    free(outstring);
    GD_RETURN_ERROR(D);
  }

  /* the parser will modifiy in_cols[0] if it contains a metafield code */
  len0 = strlen(in_cols[0]);
  if (parent) {
    new_code = _GD_Malloc(D, N->e->len + len0 + 2);

    if (new_code) {
      memcpy(new_code, N->field, N->e->len);
      new_code[N->e->len] = '/';
      memcpy(new_code + N->e->len + 1, in_cols[0], len0 + 1);
    }
    len0 += N->e->len + 1;
  } else {
    new_code = _GD_Malloc(D, len0 + 1);
    if (new_code)
      memcpy(new_code, in_cols[0], len0 + 1);
  }

  if (D->error) { /* malloc error */
    free(outstring);
    GD_RETURN_ERROR(D);
  }

  /* Let the parser compose the entry */
  N = _GD_ParseFieldSpec(D, &p, n_cols, in_cols, len0, parent ? N : NULL,
      N->fragment_index, 0, 0, &outstring, tok_pos);

  free(outstring);

  if (D->error) {
    free(new_code);
    GD_RETURN_ERROR(D); /* field spec parser threw an error */
  }

  /* The parser will have re-applied the prefix and suffix, undo that */
  free(N->field);
  N->field = new_code;
  N->e->len = len0;

  if (N->field_type == GD_LINCOM_ENTRY || N->field_type == GD_POLYNOM_ENTRY)
    move = 7;

  /* Change the entry */
  ret = _GD_Change(D, N->field, N, move);

  _GD_FreeE(D, N, 1);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_spec(DIRFILE* D, const char* line, int move)
{
  int ret;

  dtrace("%p, \"%s\", %i", D, line, move);

  ret = _GD_AlterSpec(D, line, NULL, "gd_alter_spec()", move);

  dreturn("%i", ret);
  return ret;
}

int gd_malter_spec(DIRFILE* D, const char* line, const char* parent, int move)
{
  int ret;

  dtrace("%p, \"%s\", \"%s\", %i", D, line, parent, move);

  ret = _GD_AlterSpec(D, line, parent, "gd_malter_spec()", move);

  dreturn("%i", ret);
  return ret;
}
