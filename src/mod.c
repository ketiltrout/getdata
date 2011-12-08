/* Copyright (C) 2008-2011 D. V. Wiebe
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

static unsigned int _gd_max(unsigned int A, unsigned int B)
{
  return (A > B) ? A : B;
}

#define GD_AS_FREE_SCALAR 1
#define GD_AS_NEED_RECALC 2
#define GD_AS_ERROR       4
#define GD_AS_MODIFIED    8
/*  sold snew alit
 * 0a N    N0   0     -> do nothing         ()
 * 1  N    N0   1     -> set lout           ()
 * 2  Na   a    01    -> set sout           (free scalar; need recalc)
 * 3  a    N    0     -> recalc, set sout   (free scalar)
 * 4  a    N    1     -> set sout, set lout (free scalar)
 * 0b a    0    01    -> do nothing         ()
 */
static int _GD_AlterScalar(DIRFILE* D, int alter_literal, gd_type_t type,
    void *lout, const void *lin, char **sout, int *iout, const char *sin,
    int iin, int calculated)
{
  int r = 0;
  int set_lout = 0;
  int error = 0;

  dtrace("%p, %i, 0x%X, %p, %p, %p, %p, %p, %i, %i", D, alter_literal, type,
      lout, lin, sout, iout, sin, iin, calculated);

  if (sin == NULL) {
    if (*sout != NULL) {
      if (alter_literal) {
        /* 4: replace a CONST field with a literal scalar */
        r = GD_AS_FREE_SCALAR | GD_AS_MODIFIED;
        *sout = NULL;
        set_lout = 1;
      } else {
        /* 3: derefencing a CONST field to turn it into a literal scalar
         *    lout is not set from lin, but kept as-is, after calculation;
         *    this may throw GD_E_BAD_CODE or GD_E_BAD_FIELD_TYPE, via
         *    get_constant. */
        r = GD_AS_FREE_SCALAR | GD_AS_MODIFIED;
        if (!calculated)
          error = gd_get_constant(D, *sout, GD_INT64, lout);
        *sout = NULL;
      }
    } else if (alter_literal) {
      /* 1: set lout from lin */
      set_lout = 1;
    }
    /* otherwise 0a: do nothing */
  } else if (sin[0] == '\0') {
    if (*sout == NULL && alter_literal) {
      /* 1: set lout from lin */
      set_lout = 1;
    }
    /* otherwise 0b: do nothing */
  } else {
    /* 2: set a new CONST field from sout; if this is a RAW field, and we've
     *    been asked to move the raw file, _GD_Change is going to need to
     *    recalculate the entry; no need to change lout: it's ignored. */
    r = GD_AS_FREE_SCALAR | GD_AS_NEED_RECALC | GD_AS_MODIFIED;
    *sout = strdup(sin);
    if (*sout == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    *iout = iin;
  }

  if (!error && set_lout) {
    r |= GD_AS_MODIFIED;
    if (type == GD_INT64)
      *(int64_t *)lout = *(int64_t *)lin;
    else if (type == GD_COMPLEX128)
      memcpy(lout, lin, 2 * sizeof(double));
    else if (type == GD_FLOAT64)
      *(double *)lout = *(double *)lin;
    else if (type == GD_INT16)
      *(int16_t *)lout = *(int16_t *)lin;
    else if (type == GD_UINT16)
      *(uint16_t *)lout = *(uint16_t *)lin;
    else
      _GD_InternalError(D);
  }

  if (error)
    r |= GD_AS_ERROR;

  dreturn("%i", r);
  return r;
}

/* _GD_SPFConvert: this is the no-longer used AddData, cut down for use by
 * _GD_Change.  NB: Don't precompute (spfB / spfA) here: the order of operations
 * is important to get proper integer trucation.
 */
static void _GD_SPFConvert(DIRFILE* D, void *A, gd_spf_t spfA, void *B,
    gd_spf_t spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%X, %zu", D, A, spfA, B, spfB, type, n);

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
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* N is the new entry, supplied by the user
 * E is the old entry, stored in the database
 * Q is our workspace; in the end, Q is a sanitised N which replaces E */
static int _GD_Change(DIRFILE *D, const char *field_code, const gd_entry_t *N,
    int flags)
{
  int i, j;
  int field_free = 0;
  int scalar_free = 0;
  int modified = 0;
  size_t n;
  gd_type_t type;
  void *ptr;
  gd_entry_t *E = NULL;
  gd_entry_t Q;
  struct _gd_private_entry Qe;

  dtrace("%p, \"%s\", %p, %i", D, field_code, N, flags);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if ((E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL))
      == NULL)
  {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  } else if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
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

  /* hiddenness isn't changeable with this interface */
  Q.hidden = E->hidden;

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      j = _GD_AlterScalar(D, N->EN(raw,spf) && N->EN(raw,spf) != E->EN(raw,spf),
          GD_UINT16, &Q.EN(raw,spf), &N->EN(raw,spf), Q.scalar, Q.scalar_ind,
          N->scalar[0], N->scalar_ind[0], E->e->calculated);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 1;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;

      Q.EN(raw,data_type) = (N->EN(raw,data_type) == GD_NULL) ?
        E->EN(raw,data_type) : N->EN(raw,data_type);

      /* nothing to do */
      if (Q.EN(raw,spf) == E->EN(raw,spf) && Q.EN(raw,data_type) ==
          E->EN(raw,data_type) && Q.scalar[0] == E->scalar[0])
        break;

      modified = 1;

      if (Q.EN(raw,data_type) & 0x40 ||
          (Qe.u.raw.size = GD_SIZE(Q.EN(raw,data_type))) == 0)
      {
        _GD_SetError(D, GD_E_BAD_TYPE, Q.EN(raw,data_type), NULL, 0, NULL);
        dreturn("%i", -1);
        return -1;
      }

      if (flags) {
        ssize_t nread, nwrote;
        off64_t ns_out, nf;
        void *buffer1;
        void *buffer2;
        struct encoding_t *enc;

        if (j & GD_AS_NEED_RECALC)
          if (gd_get_constant(D, Q.scalar[0], GD_UINT16, &Q.EN(raw,spf)))
            break;

        nf = BUFFER_SIZE / _gd_max(E->e->u.raw.size,
            GD_SIZE(Q.EN(raw,data_type))) / _gd_max(E->EN(raw,spf),
            Q.EN(raw,spf));

        if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA)
          _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
              D->fragment[E->fragment_index].cname);
        else
          _GD_Supports(D, E, GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK |
              GD_EF_READ | GD_EF_WRITE | GD_EF_SYNC | GD_EF_UNLINK);

        if (D->error)
          break;

        enc = _gd_ef + E->e->u.raw.file[0].subenc;

        /* open the old file */
        if (_GD_InitRawIO(D, E, NULL, 0, NULL, 0, GD_FILE_READ, 0))
          break;
        else if ((*enc->seek)(E->e->u.raw.file, 0, E->EN(raw,data_type),
              GD_FILE_READ) == -1)
        {
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno,
              NULL);
        }

        if (D->error)
          break;

        /* Create a temporary file and open it */
        if (_GD_InitRawIO(D, E, NULL, -1, enc, 0, GD_FILE_WRITE | GD_FILE_TEMP,
              _GD_FileSwapBytes(D, E->fragment_index)))
          break;
        else if (_GD_WriteSeek(D, E, enc, 0, GD_FILE_WRITE | GD_FILE_TEMP)
            == -1)
        {
          _GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DISCARD |
              GD_FINIRAW_CLOTEMP);
          break;
        }

        buffer1 = malloc(BUFFER_SIZE);
        buffer2 = malloc(BUFFER_SIZE);

        /* Now copy the old file to the new file */
        for (;;) {
          nread = (*enc->read)(E->e->u.raw.file, buffer1, E->EN(raw,data_type),
              nf * E->EN(raw,spf));

          if (nread < 0) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
                NULL);
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

          nwrote = _GD_WriteOut(D, E, enc, buffer1, Q.EN(raw,data_type), ns_out,
              1);

          if (nwrote < ns_out) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
                NULL);
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
      memcpy(Qe.u.raw.file, E->e->u.raw.file, sizeof(struct _gd_raw_file));

      break;
    case GD_LINCOM_ENTRY:
      Q.EN(lincom,n_fields) = (N->EN(lincom,n_fields) == 0) ?
        E->EN(lincom,n_fields) : N->EN(lincom,n_fields);
      if (Q.EN(lincom,n_fields) < 1 || Q.EN(lincom,n_fields) > GD_MAX_LINCOM) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL,
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

      Q.comp_scal = 0;

      for (i = 0; i < Q.EN(lincom,n_fields); ++i) {
        if (flags & 0x1)
          if (E->EN(lincom,n_fields) <= i || (N->in_fields[i] != NULL &&
                strcmp(E->in_fields[i], N->in_fields[i])))
          {
            if ((Q.in_fields[i] = strdup(N->in_fields[i])) == NULL) {
              _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
              break;
            }
            modified = 1;
            field_free |= 1 << i;
          }

        if (flags & 0x2) {
          if (N->comp_scal) {
            j = _GD_AlterScalar(D, !_gd_ccmpc(E->EN(lincom,cm)[i],
                  N->EN(lincom,cm)[i]), GD_COMPLEX128, Q.EN(lincom,cm) + i,
                N->EN(lincom,cm) + i, Q.scalar + i, Q.scalar_ind + i,
                N->scalar[i], N->scalar_ind[i], E->e->calculated);
            Q.EN(lincom,m)[i] = creal(Q.EN(lincom,cm)[i]);
          } else {
            j = _GD_AlterScalar(D, E->EN(lincom,m)[i] != N->EN(lincom,m)[i],
                GD_FLOAT64, Q.EN(lincom,m) + i, N->EN(lincom,m) + i,
                Q.scalar + i, Q.scalar_ind + i, N->scalar[i], N->scalar_ind[i],
                E->e->calculated);
            _gd_r2c(Q.EN(lincom,cm)[i], Q.EN(lincom,m)[i]);
          }

          if (j & GD_AS_FREE_SCALAR)
            scalar_free |= 1 << i;
          if (j & GD_AS_MODIFIED)
            modified = 1;
          if (j & GD_AS_NEED_RECALC)
            Qe.calculated = 0;
          if (j & GD_AS_MODIFIED)
            modified = 1;
        }

        if (flags & 0x4) {
          if (N->comp_scal) {
            j = _GD_AlterScalar(D, !_gd_ccmpc(E->EN(lincom,cb)[i],
                  N->EN(lincom,cb)[i]), GD_COMPLEX128, Q.EN(lincom,cb) + i,
                N->EN(lincom,cb) + i, Q.scalar + i + GD_MAX_LINCOM,
                Q.scalar_ind + i + GD_MAX_LINCOM, N->scalar[i +  GD_MAX_LINCOM],
                N->scalar_ind[i + GD_MAX_LINCOM], E->e->calculated);
            Q.EN(lincom,b)[i] = creal(Q.EN(lincom,cb)[i]);
          } else {
            j = _GD_AlterScalar(D, E->EN(lincom,b)[i] != N->EN(lincom,b)[i],
                GD_FLOAT64, Q.EN(lincom,b) + i, N->EN(lincom,b) + i,
                Q.scalar + i + GD_MAX_LINCOM, Q.scalar_ind + i + GD_MAX_LINCOM,
                N->scalar[i + GD_MAX_LINCOM], N->scalar_ind[i + GD_MAX_LINCOM],
                E->e->calculated);
            _gd_r2c(Q.EN(lincom,cb)[i], Q.EN(lincom,b)[i]);
          }

          if (j & GD_AS_FREE_SCALAR)
            scalar_free |= 1 << (i + GD_MAX_LINCOM);
          if (j & GD_AS_MODIFIED)
            modified = 1;
          if (j & GD_AS_NEED_RECALC)
            Qe.calculated = 0;
          if (j & GD_AS_MODIFIED)
            modified = 1;
        }

        if (cimag(Q.EN(lincom,cm)[i]) || cimag(Q.EN(lincom,cb)[i]))
          Q.comp_scal = 1;
      }

      if ((Q.comp_scal && !E->comp_scal) || (!Q.comp_scal && E->comp_scal))
        modified = 1;
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

      if (N->EN(linterp,table) != NULL && strcmp(E->EN(linterp,table),
            N->EN(linterp,table)))
      {
        Q.EN(linterp,table) = strdup(N->EN(linterp,table));
        Qe.u.linterp.table_file = NULL;

        if (Q.EN(linterp,table) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }

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
            _GD_SetError(D, GD_E_RAW_IO, 0, E->EN(linterp,table), errno, 0);
            break;
          }
        }

        modified = 1;
        free(E->EN(linterp,table));
        free(E->e->u.linterp.table_file);
      }

      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      j = _GD_AlterScalar(D, N->EN(bit,numbits) >= 1 && E->EN(bit,numbits) !=
          N->EN(bit,numbits), GD_INT16, &Q.EN(bit,numbits), &N->EN(bit,numbits),
          Q.scalar + 1, Q.scalar_ind + 1, N->scalar[1], N->scalar_ind[1],
          E->e->calculated);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 2;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterScalar(D, N->EN(bit,bitnum) >= 0 && E->EN(bit,bitnum) !=
          N->EN(bit,bitnum), GD_INT16, &Q.EN(bit,bitnum), &N->EN(bit,bitnum),
          Q.scalar, Q.scalar_ind, N->scalar[0], N->scalar_ind[0],
          E->e->calculated);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 1;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
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
    case GD_RECIP_ENTRY:
      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      Q.comp_scal = 0;
      if (N->comp_scal) {
        j = _GD_AlterScalar(D, cabs(N->EN(recip,cdividend)) != 0 &&
              !_gd_ccmpc(E->EN(recip,cdividend), N->EN(recip,cdividend)),
              GD_COMPLEX128, &Q.EN(recip,cdividend), &(N->EN(recip,cdividend)),
              Q.scalar, Q.scalar_ind, N->scalar[0], N->scalar_ind[0],
              E->e->calculated);
        Q.EN(recip,dividend) = creal(Q.EN(recip,cdividend));
        if (cimag(Q.EN(recip,cdividend)) != 0)
          Q.comp_scal = 1;
      } else {
        j = _GD_AlterScalar(D, N->EN(recip,dividend) != 0 &&
            E->EN(recip,dividend) != N->EN(recip,dividend), GD_FLOAT64,
            &Q.EN(recip,dividend), &(N->EN(recip,dividend)), Q.scalar,
            Q.scalar_ind, N->scalar[0], N->scalar_ind[0], E->e->calculated);
        _gd_r2c(Q.EN(recip,cdividend), Q.EN(recip,dividend));
      }

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free = 1;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      if ((Q.comp_scal && !E->comp_scal) || (!Q.comp_scal && E->comp_scal))
        modified = 1;

      break;
    case GD_PHASE_ENTRY:
      j = _GD_AlterScalar(D, E->EN(phase,shift) != N->EN(phase,shift), GD_INT64,
          &Q.EN(phase,shift), &N->EN(phase,shift), Q.scalar, Q.scalar_ind,
          N->scalar[0], N->scalar_ind[0], E->e->calculated);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 2;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      break;
    case GD_POLYNOM_ENTRY:
      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free = 1;
      }

      Q.EN(polynom,poly_ord) = (N->EN(polynom,poly_ord) == 0) ?
        E->EN(polynom,poly_ord) : N->EN(polynom,poly_ord);
      if (Q.EN(polynom,poly_ord) != E->EN(polynom,poly_ord))
        modified = 1;

      Q.comp_scal = 0;

      if (flags & 0x1)
        for (i = 0; i <= Q.EN(polynom,poly_ord); ++i) {
          if (N->comp_scal) {
            j = _GD_AlterScalar(D, !_gd_ccmpc(E->EN(polynom,ca)[i],
                  N->EN(polynom,ca)[i]), GD_COMPLEX128, Q.EN(polynom,ca) + i,
                N->EN(polynom,ca) + i, Q.scalar + i, Q.scalar_ind + i,
                N->scalar[i], N->scalar_ind[i], E->e->calculated);
            Q.EN(polynom,a)[i] = creal(Q.EN(polynom,ca)[i]);
          } else {
            j = _GD_AlterScalar(D, E->EN(polynom,a)[i] != N->EN(polynom,a)[i],
                GD_FLOAT64, Q.EN(polynom,a) + i, N->EN(polynom,a) + i,
                Q.scalar + i, Q.scalar_ind + i, N->scalar[i], N->scalar_ind[i],
                E->e->calculated);
            _gd_r2c(Q.EN(polynom,ca)[i], Q.EN(polynom,a)[i]);
          }

          if (j & GD_AS_FREE_SCALAR)
            scalar_free |= 1 << i;
          if (j & GD_AS_MODIFIED)
            modified = 1;
          if (j & GD_AS_NEED_RECALC)
            Qe.calculated = 0;
          if (j & GD_AS_MODIFIED)
            modified = 1;

          if (cimag(Q.EN(polynom,ca)[i]))
            Q.comp_scal = 1;
        }

      if ((Q.comp_scal && !E->comp_scal) || (!Q.comp_scal && E->comp_scal))
        modified = 1;

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
              N->scalar_ind[0], E->e->calculated);
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          j = _GD_AlterScalar(D, E->EN(window,threshold.u) !=
              N->EN(window,threshold.u), GD_UINT64, &Q.EN(window,threshold.u),
              &N->EN(window,threshold.u), Q.scalar, Q.scalar_ind, N->scalar[0],
              N->scalar_ind[0], E->e->calculated);
          break;
        default:
          j = _GD_AlterScalar(D, E->EN(window,threshold.r) !=
              N->EN(window,threshold.r), GD_FLOAT64, &Q.EN(window,threshold.r),
              &N->EN(window,threshold.r), Q.scalar, Q.scalar_ind, N->scalar[0],
              N->scalar_ind[0], E->e->calculated);
          break;
      }

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 2;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      if (N->in_fields[0] != NULL && strcmp(E->in_fields[0], N->in_fields[0])) {
        if ((Q.in_fields[0] = strdup(N->in_fields[0])) == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }
        modified = 1;
        field_free |= 1;
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
    case GD_CONST_ENTRY:
      Q.EN(scalar,const_type) = (N->EN(scalar,const_type) == GD_NULL) ?
        E->EN(scalar,const_type) : N->EN(scalar,const_type);

      if (Q.EN(scalar,const_type) & 0x40 || GD_SIZE(Q.EN(scalar,const_type))
          == 0)
      {
        _GD_SetError(D, GD_E_BAD_TYPE, Q.EN(scalar,const_type), NULL, 0, NULL);
        dreturn("%i", -1);
        return -1;
      }

      type = _GD_ConstType(D, Q.EN(scalar,const_type));
      if (Q.EN(scalar,const_type) != E->EN(scalar,const_type))
        modified = 1;

      if (type == _GD_ConstType(D, E->EN(scalar,const_type)))
        Qe.u.scalar.d = E->e->u.scalar.d;
      else {
        /* type convert */
        Qe.u.scalar.d = malloc(GD_SIZE(type));
        if (Qe.u.scalar.d == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          dreturn("%i", -1);
          return -1;
        }
        if (type == GD_COMPLEX128) {
          *(double*)Qe.u.scalar.d = (E->EN(scalar,const_type) & GD_IEEE754) ?
            *(double*)E->e->u.scalar.d : (E->EN(scalar,const_type) & GD_SIGNED)
            ? (double)*(int64_t*)E->e->u.scalar.d
            : (double)*(uint64_t*)E->e->u.scalar.d;
          ((double*)Qe.u.scalar.d)[1] = 0;
        } else if (type == GD_IEEE754)
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

      if (Q.EN(scalar,const_type) & 0x40 || GD_SIZE(Q.EN(scalar,const_type)) ==
          0)
      {
        _GD_SetError(D, GD_E_BAD_TYPE, Q.EN(scalar,const_type), NULL, 0, NULL);
        dreturn("%i", -1);
        return -1;
      } else if (E->EN(scalar,array_len) > GD_MAX_CARRAY_LENGTH) {
        _GD_SetError(D, GD_E_BOUNDS, 0, NULL, 0, NULL);
        dreturn("%i", -1);
        return -1;
      }

      if (Q.EN(scalar,const_type) != E->EN(scalar,const_type) ||
          Q.EN(scalar,array_len) != E->EN(scalar,array_len))
      {
        modified = 1;
      }

      type = _GD_ConstType(D, Q.EN(scalar,const_type));
      Qe.u.scalar.d = malloc(GD_SIZE(type) * Q.EN(scalar,array_len));
      if (Qe.u.scalar.d == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        dreturn("%i", -1);
        return -1;
      }

      /* copy via type conversion, if array_len has increased, trailing elements
       * are uninitialised. */
      n = E->EN(scalar,array_len);
      if (n > Q.EN(scalar,array_len))
        n = Q.EN(scalar,array_len);

      _GD_ConvertType(D, E->e->u.scalar.d, _GD_ConstType(D,
            E->EN(scalar,const_type)), Qe.u.scalar.d, type, n);

      if (D->error) {
        free(Qe.u.scalar.d);
        dreturn("%i", -1);
        return -1;
      }

      free(E->e->u.scalar.d);
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

    for (i = 0; i <= GD_MAX_POLYORD; ++i) {
      if (scalar_free & (1 << i))
        free(E->scalar[i]);
    }

    if (E->field_type == GD_LINTERP_ENTRY && flags)
      _GD_ReleaseDir(D, Qe.u.linterp.table_dirfd);

    memcpy(E->e, &Qe, sizeof(struct _gd_private_entry));
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

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

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
    gd_type_t data_type, gd_spf_t spf, int move)
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", %u, 0x%X, %i", D, field_code, spf, data_type, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N.field_type = GD_RAW_ENTRY;
  N.EN(raw,spf) = spf;
  N.EN(raw,data_type) = data_type;
  N.e = NULL;
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

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N.field_type = GD_LINCOM_ENTRY;
  N.comp_scal = 0;
  if (n_fields > GD_MAX_LINCOM || n_fields < 0) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
    dreturn("%i", -1);
    return -1;
  } else if (n_fields != 0)
    N.EN(lincom,n_fields) = n_fields;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.EN(lincom,n_fields) = E->EN(lincom,n_fields);
  }
  N.e = NULL;

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

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N.field_type = GD_LINCOM_ENTRY;
  N.comp_scal = 1;
  if (n_fields > GD_MAX_LINCOM || n_fields < 0) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL, n_fields,
        NULL);
    dreturn("%i", -1);
    return -1;
  } else if (n_fields != 0)
    N.EN(lincom,n_fields) = n_fields;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.EN(lincom,n_fields) = E->EN(lincom,n_fields);
  }
  N.e = NULL;

  for (i = 0; i < N.EN(lincom,n_fields); ++i) {
    if (in_fields != NULL) {
      flags |= 1;
      N.in_fields[i] = (char *)in_fields[i];
    }

    if (cm != NULL) {
      flags |= 2;
      _gd_ca2c(N.EN(lincom,cm)[i], cm, i);
      N.scalar[i] = NULL;
    } else
      N.scalar[i] = "";

    if (cb != NULL) {
      flags |= 4;
      _gd_ca2c(N.EN(lincom,cb)[i], cb, i);
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

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_LINTERP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(linterp,table) = (char *)table;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_bit(DIRFILE* D, const char* field_code, const char* in_field,
    gd_bit_t bitnum, gd_bit_t numbits) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %i, %i", D, field_code, in_field, bitnum,
      numbits);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_BIT_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(bit,bitnum) = bitnum;
  N.EN(bit,numbits) = numbits;
  N.e = NULL;
  N.scalar[0] = (bitnum == -1) ? (char *)"" : NULL;
  N.scalar[1] = (numbits == 0) ? (char *)"" : NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_sbit(DIRFILE* D, const char* field_code, const char* in_field,
    gd_bit_t bitnum, gd_bit_t numbits) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %i, %i", D, field_code, in_field, bitnum,
      numbits);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_SBIT_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(bit,bitnum) = bitnum;
  N.EN(bit,numbits) = numbits;
  N.e = NULL;
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

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_RECIP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.scalar[0] = (dividend == 0) ? (char *)"" : NULL;
  N.EN(recip,dividend) = dividend;
  N.comp_scal = 0;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

#ifndef GD_NO_C99_API
int gd_alter_crecip(DIRFILE* D, const char* field_code, const char* in_field,
    double complex cdividend)
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %g;%g", D, field_code, in_field, creal(cdividend),
      cimag(cdividend));

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_RECIP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.scalar[0] = (cdividend == 0) ? "" : NULL;
  N.EN(recip,cdividend) = cdividend;
  N.comp_scal = 1;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}
#endif

int gd_alter_crecip89(DIRFILE* D, const char* field_code, const char* in_field,
    const double cdividend[2]) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", [%g, %g]", D, field_code, in_field,
      (cdividend == NULL) ? 0 : cdividend[0],
      (cdividend == NULL) ? 0 : cdividend[1]);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_RECIP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  if (cdividend == NULL) {
    N.scalar[0] = "";
    _gd_l2c(N.EN(recip,cdividend), 0, 0);
  } else {
    N.scalar[0] = (cdividend[0] == 0 && cdividend[1] == 0) ? (char *)"" : NULL;
    _gd_a2c(N.EN(recip,cdividend), cdividend);
  }
  N.scalar_ind[0] = 0;
  N.comp_scal = 1;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_divide(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, field_code, in_field1, in_field2);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_DIVIDE_ENTRY;
  N.in_fields[0] = (char *)in_field1;
  N.in_fields[1] = (char *)in_field2;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_multiply(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, field_code, in_field1, in_field2);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_MULTIPLY_ENTRY;
  N.in_fields[0] = (char *)in_field1;
  N.in_fields[1] = (char *)in_field2;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_phase(DIRFILE* D, const char* field_code, const char* in_field,
    gd_shift_t shift) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %lli", D, field_code, in_field, (long long)shift);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_PHASE_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.EN(phase,shift) = shift;
  N.e = NULL;
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

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_CONST_ENTRY;
  N.EN(scalar,const_type) = const_type;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_carray(DIRFILE* D, const char* field_code, gd_type_t const_type,
    size_t array_len) gd_nothrow
{
  int ret;
  gd_entry_t N;

  dtrace("%p, \"%s\", 0x%X, %zu", D, field_code, const_type, array_len);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_CARRAY_ENTRY;
  N.EN(scalar,const_type) = const_type;
  N.EN(scalar,array_len) = array_len;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_polynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const double* a) gd_nothrow
{
  gd_entry_t N;
  int i, ret;
  int flags = 0;

  dtrace("%p, \"%s\", %i, \"%s\", %p", D, field_code, poly_ord, in_field, a);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N.field_type = GD_POLYNOM_ENTRY;
  N.comp_scal = 0;
  if (poly_ord > GD_MAX_POLYORD || poly_ord < 0) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
    dreturn("%i", -1);
    return -1;
  } else if (poly_ord != 0)
    N.EN(polynom,poly_ord) = poly_ord;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.EN(polynom,poly_ord) = E->EN(polynom,poly_ord);
  }
  N.in_fields[0] = (char *)in_field;
  N.e = NULL;

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

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  N.field_type = GD_POLYNOM_ENTRY;
  N.comp_scal = 1;
  if (poly_ord > GD_MAX_POLYORD || poly_ord < 0) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_POLYORD, NULL, poly_ord,
        NULL);
    dreturn("%i", -1);
    return -1;
  } else if (poly_ord != 0)
    N.EN(polynom,poly_ord) = poly_ord;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.EN(polynom,poly_ord) = E->EN(polynom,poly_ord);
  }
  N.in_fields[0] = (char *)in_field;
  N.e = NULL;

  if (ca != NULL)  {
    flags |= 1;
    for (i = 0; i <= N.EN(polynom,poly_ord); ++i) {
      _gd_ca2c(N.EN(polynom,ca)[i], ca, i);
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

  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, {%g,%llx,%lli}", D, field_code,
      in_field, check_field, windop, threshold.r,
      (unsigned long long)threshold.u, (long long)threshold.i);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_WINDOW_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.in_fields[1] = (char *)check_field;
  N.EN(window,windop) = windop;
  N.EN(window,threshold) = threshold;
  N.scalar[0] = NULL;
  N.e = NULL;

  ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_spec(DIRFILE* D, const char* line, int move)
{
  const char *tok_pos = NULL;
  char *outstring = NULL;
  char *in_cols[MAX_IN_COLS];
  int n_cols, ret;
  int standards = GD_DIRFILE_STANDARDS_VERSION;
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

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  if (D->av)
    standards = D->standards;

  /* start parsing */
  n_cols = _GD_Tokenise(D, line, &outstring, &tok_pos, in_cols,
      "dirfile_alter_spec()", 0, standards, D->flags & GD_PERMISSIVE);

  if (D->error) {
    free(outstring);
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Sanity check */
  if (n_cols == 0) {
    free(outstring);
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, "dirfile_alter_spec()", 0,
        NULL);
    dreturn("%i", -1);
    return -1;
  }

  N = _GD_FindField(D, in_cols[0], D->entry, D->n_entries, NULL);

  if (N == NULL) {
    free(outstring);
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, in_cols[0]);
    dreturn("%i", -1);
    return -1;
  }

  /* Let the parser compose the entry */
  N = _GD_ParseFieldSpec(D, n_cols, in_cols, NULL, "dirfile_alter_spec()", 0,
      N->fragment_index, standards, 0, GD_PEDANTIC, 0, &outstring, tok_pos);

  free(outstring);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  if (N->field_type == GD_LINCOM_ENTRY)
    move = 7;

  /* Change the entry */
  ret = _GD_Change(D, N->field, N, move);

  _GD_FreeE(D, N, 1);

  dreturn("%i", ret);
  return ret;
}

int gd_malter_spec(DIRFILE* D, const char* line, const char* parent, int move)
{
  char *outstring = NULL;
  const char *tok_pos;
  char *in_cols[MAX_IN_COLS];
  int n_cols, ret;
  int standards = GD_DIRFILE_STANDARDS_VERSION;
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

  N = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);
  if (N == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%i", -1);
    return -1;
  }

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  if (D->av)
    standards = D->standards;

  /* start parsing */
  n_cols = _GD_Tokenise(D, line, &outstring, &tok_pos, in_cols,
      "dirfile_malter_spec()", 0, standards, D->flags & GD_PERMISSIVE);

  if (!D->error)
    /* Let the parser compose the entry */
    N = _GD_ParseFieldSpec(D, n_cols, in_cols, N, "dirfile_malter_spec()", 0,
        N->fragment_index, standards, 0, GD_PEDANTIC, 0, &outstring, tok_pos);

  free(outstring);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  if (N->field_type == GD_LINCOM_ENTRY)
    move = 7;

  /* Change the entry */
  ret = _GD_Change(D, N->field, N, move);

  _GD_FreeE(D, N, 1);

  dreturn("%i", ret);
  return ret;
}
