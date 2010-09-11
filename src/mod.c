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

static unsigned int max(unsigned int A, unsigned int B)
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
    void* lout, const void* lin, char** sout, const char *sin, int calculated)
{
  int r = 0;
  int set_lout = 0;
  int error = 0;

  dtrace("%p, %i, %x, %p, %p, %p, \"%s\", %i", D, alter_literal, type, lout,
      lin, sout, sin, calculated);

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

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zu", D, A, spfA, B, spfB, type, n);

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

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      j = _GD_AlterScalar(D, N->u.raw.spf && N->u.raw.spf != E->u.raw.spf,
          GD_UINT16, &Q.u.raw.spf, &N->u.raw.spf, Q.scalar + 0, N->scalar[0],
          E->e->calculated);
      
      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 1;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;

      Q.u.raw.type = (N->u.raw.type == GD_NULL) ? E->u.raw.type :
        N->u.raw.type;

      /* nothing to do */
      if (Q.u.raw.spf == E->u.raw.spf && Q.u.raw.type == E->u.raw.type
          && Q.scalar[0] == E->scalar[0])
        break;

      modified = 1;

      if (Q.u.raw.type & 0x40 ||
          (Qe.u.raw.size = GD_SIZE(Q.u.raw.type)) == 0)
      {
        _GD_SetError(D, GD_E_BAD_TYPE, Q.u.raw.type, NULL, 0, NULL);
        dreturn("%i", -1);
        return -1;
      }

      if (flags) {
        ssize_t nread, nwrote;
        off64_t ns_out;
        void *buffer1;
        void *buffer2;

        if (j & GD_AS_NEED_RECALC)
          if (gd_get_constant(D, Q.scalar[0], GD_UINT16, &Q.u.raw.spf))
            break;

        const off64_t nf = BUFFER_SIZE / max(E->e->u.raw.size,
            GD_SIZE(Q.u.raw.type)) / max(E->u.raw.spf, Q.u.raw.spf);

        if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA)
          _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
              D->fragment[E->fragment_index].cname);
        else
          _GD_Supports(D, E, GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK |
              GD_EF_READ | GD_EF_WRITE | GD_EF_SYNC | GD_EF_UNLINK |
              GD_EF_TEMP);

        if (D->error)
          break;

        const struct encoding_t* enc = _gd_ef + E->e->u.raw.file[0].encoding;

        if (_GD_SetEncodedName(D, E->e->u.raw.file, E->e->u.raw.filebase, 0))
          ; /* error already set */
        else if (E->e->u.raw.file[0].fp == -1 && (*enc->open)(E->e->u.raw.file,
              0, 0))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno,
              NULL);
        else if ((*enc->seek)(E->e->u.raw.file, 0, E->u.raw.type, 1) == -1)
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno,
              NULL);

        if (D->error)
          break;

        /* Create a temporary file and open it */
        if (_GD_SetEncodedName(D, E->e->u.raw.file + 1, E->e->u.raw.filebase,
              1))
          ; /* error already set */
        else if ((*enc->temp)(E->e->u.raw.file, GD_TEMP_OPEN))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
              NULL);
        else if ((*enc->seek)(E->e->u.raw.file + 1, 0, E->u.raw.type, 1) ==
            -1)
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
              NULL);

        if (D->error) {
          (*enc->temp)(E->e->u.raw.file, GD_TEMP_DESTROY);
          break;
        }

        buffer1 = malloc(BUFFER_SIZE);
        buffer2 = malloc(BUFFER_SIZE);

        /* Now copy the old file to the new file */
        for (;;) {
          nread = (*enc->read)(E->e->u.raw.file, buffer1, E->u.raw.type,
              nf * E->u.raw.spf);

          if (nread < 0) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
                NULL);
            break;
          }

          if (nread == 0)
            break;

          ns_out = nread * Q.u.raw.spf / E->u.raw.spf;

          /* spf convert */
          if (Q.u.raw.spf != E->u.raw.spf)
            _GD_SPFConvert(D, buffer2, Q.u.raw.spf, buffer1, E->u.raw.spf,
                E->u.raw.type, ns_out);
          else {
            ptr = buffer1;
            buffer1 = buffer2;
            buffer2 = ptr;
          }

          /* type convert */
          if (Q.u.raw.type != E->u.raw.type)
            _GD_ConvertType(D, buffer2, E->u.raw.type, buffer1,
                Q.u.raw.type, ns_out);
          else {
            ptr = buffer1;
            buffer1 = buffer2;
            buffer2 = ptr;
          }

          nwrote = (*enc->write)(E->e->u.raw.file + 1, buffer1,
              Q.u.raw.type, ns_out);

          if (nwrote < ns_out) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
                NULL);
            break;
          }
        }

        free(buffer1);
        free(buffer2);

        /* An error occurred, clean up */
        if (D->error)
          (*enc->temp)(E->e->u.raw.file, GD_TEMP_DESTROY);
        /* Well, I suppose the copy worked.  Close both files */
        else if ((*enc->close)(E->e->u.raw.file) ||
            (*enc->sync)(E->e->u.raw.file + 1) ||
            (*enc->close)(E->e->u.raw.file + 1))
        {
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno,
              NULL);
        /* Move the temporary file over the old file */
        } else if ((*enc->temp)(E->e->u.raw.file, GD_TEMP_MOVE))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno,
              NULL);
      }

      break;
    case GD_LINCOM_ENTRY:
      Q.u.lincom.n_fields = (N->u.lincom.n_fields == 0) ? E->u.lincom.n_fields :
        N->u.lincom.n_fields;
      if (Q.u.lincom.n_fields < 1 || Q.u.lincom.n_fields > GD_MAX_LINCOM) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL,
            E->u.lincom.n_fields, NULL);
        break;
      }

      if (Q.u.lincom.n_fields != E->u.lincom.n_fields) {
        modified = 1;
        if (Q.u.lincom.n_fields < E->u.lincom.n_fields)
          for (i = Q.u.lincom.n_fields; i < E->u.lincom.n_fields; ++i) {
            field_free |= 1 << i;
            scalar_free |= 1 << i;
            scalar_free |= 1 << (i + GD_MAX_LINCOM);
          }
      }

      Q.comp_scal = 0;

      for (i = 0; i < Q.u.lincom.n_fields; ++i) {
        if (flags & 0x1)
          if (E->u.lincom.n_fields <= i || (N->in_fields[i] != NULL && 
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
            j = _GD_AlterScalar(D, !_gd_ccmpc(E->u.lincom.cm[i],
                  N->u.lincom.cm[i]), GD_COMPLEX128, Q.u.lincom.cm + i,
                N->u.lincom.cm + i, Q.scalar + i, N->scalar[i],
                E->e->calculated);
            Q.u.lincom.m[i] = creal(Q.u.lincom.cm[i]);
          } else {
            j = _GD_AlterScalar(D, E->u.lincom.m[i] != N->u.lincom.m[i],
                GD_FLOAT64, Q.u.lincom.m + i, N->u.lincom.m + i, Q.scalar + i,
                N->scalar[i], E->e->calculated);
            _gd_r2c(Q.u.lincom.cm[i], Q.u.lincom.m[i]);
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
            j = _GD_AlterScalar(D, !_gd_ccmpc(E->u.lincom.cb[i],
                  N->u.lincom.cb[i]), GD_COMPLEX128, Q.u.lincom.cb + i,
                N->u.lincom.cb + i, Q.scalar + i + GD_MAX_LINCOM,
                N->scalar[i +  GD_MAX_LINCOM], E->e->calculated);
            Q.u.lincom.b[i] = creal(Q.u.lincom.cb[i]);
          } else {
            j = _GD_AlterScalar(D, E->u.lincom.b[i] != N->u.lincom.b[i],
                GD_FLOAT64, Q.u.lincom.b + i, N->u.lincom.b + i,
                Q.scalar + i + GD_MAX_LINCOM, N->scalar[i + GD_MAX_LINCOM],
                E->e->calculated);
            _gd_r2c(Q.u.lincom.cb[i], Q.u.lincom.b[i]);
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

        if (cimag(Q.u.lincom.cm[i]) || cimag(Q.u.lincom.cb[i]))
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

      if (N->u.linterp.table != NULL && strcmp(E->u.linterp.table,
            N->u.linterp.table))
      {
        Q.u.linterp.table = strdup(N->u.linterp.table);
        Qe.u.linterp.table_path = NULL;

        if (Q.u.linterp.table == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }

        if (flags) {
          if (E->e->u.linterp.table_path == NULL)
            if (_GD_SetTablePath(D, E, E->e))
              break;

          if (Qe.u.linterp.table_path == NULL)
            if (_GD_SetTablePath(D, &Q, &Qe))
              break;

          if (_GD_Rename(E->e->u.linterp.table_path, Qe.u.linterp.table_path)) {
            _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.linterp.table_path, errno,
                0);
            break;
          }
        }

        modified = 1;
        free(E->u.linterp.table);
        free(E->e->u.linterp.table_path);
      }

      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      j = _GD_AlterScalar(D, N->u.bit.numbits >= 1 && E->u.bit.numbits !=
          N->u.bit.numbits, GD_INT16, &Q.u.bit.numbits, &N->u.bit.numbits,
          Q.scalar + 1, N->scalar[1], E->e->calculated);

      if (j & GD_AS_ERROR)
        break;
      if (j & GD_AS_FREE_SCALAR)
        scalar_free |= 2;
      if (j & GD_AS_NEED_RECALC)
        Qe.calculated = 0;
      if (j & GD_AS_MODIFIED)
        modified = 1;

      j = _GD_AlterScalar(D, N->u.bit.bitnum >= 0 && E->u.bit.bitnum !=
          N->u.bit.bitnum, GD_INT16, &Q.u.bit.bitnum, &N->u.bit.bitnum,
          Q.scalar + 0, N->scalar[0], E->e->calculated);

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
        j = _GD_AlterScalar(D, !_gd_ccmpc(E->u.recip.cdividend,
              N->u.recip.cdividend), GD_COMPLEX128, &Q.u.recip.cdividend,
            &(N->u.recip.cdividend), Q.scalar, N->scalar[0],
            E->e->calculated);
        Q.u.recip.dividend = creal(Q.u.recip.cdividend);
        if (cimag(Q.u.recip.cdividend) != 0)
          Q.comp_scal = 1;
      } else {
        j = _GD_AlterScalar(D, E->u.recip.dividend != N->u.recip.dividend,
            GD_FLOAT64, &Q.u.recip.dividend, &(N->u.recip.dividend), Q.scalar,
            N->scalar[0], E->e->calculated);
        _gd_r2c(Q.u.recip.cdividend, Q.u.recip.dividend);
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
      j = _GD_AlterScalar(D, E->u.phase.shift != N->u.phase.shift, GD_INT64,
          &Q.u.phase.shift, &N->u.phase.shift, Q.scalar + 0, N->scalar[0],
          E->e->calculated);

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

      Q.u.polynom.poly_ord = (N->u.polynom.poly_ord == 0) ?
        E->u.polynom.poly_ord : N->u.polynom.poly_ord;
      if (Q.u.polynom.poly_ord != E->u.polynom.poly_ord)
        modified = 1;

      Q.comp_scal = 0;

      if (flags & 0x1)
        for (i = 0; i <= Q.u.polynom.poly_ord; ++i) {
          if (N->comp_scal) {
            j = _GD_AlterScalar(D, !_gd_ccmpc(E->u.polynom.ca[i],
                  N->u.polynom.ca[i]), GD_COMPLEX128, Q.u.polynom.ca + i,
                N->u.polynom.ca + i, Q.scalar + i, N->scalar[i],
                E->e->calculated);
            Q.u.polynom.a[i] = creal(Q.u.polynom.ca[i]);
          } else {
            j = _GD_AlterScalar(D, E->u.polynom.a[i] != N->u.polynom.a[i],
                GD_FLOAT64, Q.u.polynom.a + i, N->u.polynom.a + i, Q.scalar + i,
                N->scalar[i], E->e->calculated);
            _gd_r2c(Q.u.polynom.ca[i], Q.u.polynom.a[i]);
          }

          if (j & GD_AS_FREE_SCALAR)
            scalar_free |= 1 << i;
          if (j & GD_AS_MODIFIED)
            modified = 1;
          if (j & GD_AS_NEED_RECALC)
            Qe.calculated = 0;
          if (j & GD_AS_MODIFIED)
            modified = 1;

          if (cimag(Q.u.polynom.ca[i]))
            Q.comp_scal = 1;
        }

      if ((Q.comp_scal && !E->comp_scal) || (!Q.comp_scal && E->comp_scal))
        modified = 1;

      break;
    case GD_CONST_ENTRY:
      Q.u.cons.type = (N->u.cons.type == GD_NULL) ? E->u.cons.type :
        N->u.cons.type;

      if (Q.u.cons.type & 0x40 || GD_SIZE(Q.u.cons.type) == 0) {
        _GD_SetError(D, GD_E_BAD_TYPE, Q.u.cons.type, NULL, 0, NULL);
        dreturn("%i", -1);
        return -1;
      }

      if (Q.u.cons.type != E->u.cons.type) {
        modified = 1; 
        /* type convert */
        if (Q.u.cons.type & GD_COMPLEX)
#ifdef GD_NO_C99_API
          Qe.u.cons.d.c[0] = (E->u.cons.type & GD_COMPLEX) ?
            E->e->u.cons.d.c[0] : (E->u.cons.type & GD_IEEE754) ?
            E->e->u.cons.d.d : (E->u.cons.type & GD_SIGNED) ?
            (double)E->e->u.cons.d.i : (double)E->e->u.cons.d.u;
          Qe.u.cons.d.c[1] = (E->u.cons.type & GD_COMPLEX) ?
            E->e->u.cons.d.c[1] : 0;
#else
          Qe.u.cons.d.c = (E->u.cons.type & GD_COMPLEX) ? E->e->u.cons.d.c :
            (E->u.cons.type & GD_IEEE754) ? (double complex)E->e->u.cons.d.d :
            (E->u.cons.type & GD_SIGNED) ? (double complex)E->e->u.cons.d.i :
            (double complex)E->e->u.cons.d.u;
#endif
        if (Q.u.cons.type & GD_IEEE754)
          Qe.u.cons.d.d = (E->u.cons.type & GD_IEEE754) ? E->e->u.cons.d.d :
            (E->u.cons.type & GD_COMPLEX) ? creal(E->e->u.cons.d.c):
            (E->u.cons.type & GD_SIGNED) ? (double)E->e->u.cons.d.i :
            (double)E->e->u.cons.d.u;
        else if (Q.u.cons.type & GD_SIGNED)
          Qe.u.cons.d.i = (E->u.cons.type & GD_IEEE754) ?
            (int64_t)E->e->u.cons.d.d : (E->u.cons.type & GD_COMPLEX) ?
            (int64_t)creal(E->e->u.cons.d.c) : (E->u.cons.type & GD_SIGNED) ?
            E->e->u.cons.d.i : (int64_t)E->e->u.cons.d.u;
        else
          Qe.u.cons.d.u = (E->u.cons.type & GD_IEEE754) ?
            (uint64_t)E->e->u.cons.d.d : (E->u.cons.type & GD_COMPLEX) ?
            (uint64_t)creal(E->e->u.cons.d.c) : (E->u.cons.type & GD_SIGNED) ?
            (uint64_t)E->e->u.cons.d.i : E->e->u.cons.d.u;
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

    for (i = 0; i <= GD_MAX_POLYORD; ++i) {
      if (scalar_free & (1 << i))
        free(E->scalar[i]);
    }

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

  /* for these field types, move is a set of bitflags; we set them all */
  if (N.field_type == GD_LINCOM_ENTRY || N.field_type == GD_POLYNOM_ENTRY)
    move = 7;

  int ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_raw(DIRFILE *D, const char *field_code,
    gd_type_t data_type, gd_spf_t spf, int move)
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
  N.u.raw.spf = spf;
  N.u.raw.type = data_type;
  N.e = NULL;
  N.scalar[0] = (spf == 0) ? (char *)"" : NULL;

  int ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_lincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const double* m, const double* b) gd_nothrow
{
  gd_entry_t N;
  int i;
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
    N.u.lincom.n_fields = n_fields;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.u.lincom.n_fields = E->u.lincom.n_fields;
  }
  N.e = NULL;

  for (i = 0; i < N.u.lincom.n_fields; ++i) {
    if (in_fields != NULL) {
      flags |= 1;
      N.in_fields[i] = (char *)in_fields[i];
    }

    if (m != NULL) {
      flags |= 2;
      N.u.lincom.m[i] = m[i];
      N.scalar[i] = NULL;
    } else
      N.scalar[i] = "";

    if (b != NULL) {
      flags |= 4;
      N.u.lincom.b[i] = b[i];
      N.scalar[i + GD_MAX_LINCOM] = NULL;
    } else
      N.scalar[i + GD_MAX_LINCOM] = "";
  }

  int ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_clincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const GD_DCOMPLEXP(cm), const GD_DCOMPLEXP(cb))
  gd_nothrow
{
  gd_entry_t N;
  int i;
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
    N.u.lincom.n_fields = n_fields;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.u.lincom.n_fields = E->u.lincom.n_fields;
  }
  N.e = NULL;

  for (i = 0; i < N.u.lincom.n_fields; ++i) {
    if (in_fields != NULL) {
      flags |= 1;
      N.in_fields[i] = (char *)in_fields[i];
    }

    if (cm != NULL) {
      flags |= 2;
      _gd_ca2c(N.u.lincom.cm[i], cm, i);
      N.scalar[i] = NULL;
    } else
      N.scalar[i] = "";

    if (cb != NULL) {
      flags |= 4;
      _gd_ca2c(N.u.lincom.cb[i], cb, i);
      N.scalar[i + GD_MAX_LINCOM] = NULL;
    } else
      N.scalar[i + GD_MAX_LINCOM] = "";
  }

  int ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_linterp(DIRFILE* D, const char* field_code, const char* in_field,
    const char* table, int move)
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
  N.in_fields[0] = (char *)in_field;
  N.u.linterp.table = (char *)table;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, move);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_bit(DIRFILE* D, const char* field_code, const char* in_field,
    gd_bit_t bitnum, gd_bit_t numbits) gd_nothrow
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
  N.in_fields[0] = (char *)in_field;
  N.u.bit.bitnum = bitnum;
  N.u.bit.numbits = numbits;
  N.e = NULL;
  N.scalar[0] = (bitnum == -1) ? (char *)"" : NULL;
  N.scalar[1] = (numbits == 0) ? (char *)"" : NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_sbit(DIRFILE* D, const char* field_code, const char* in_field,
    gd_bit_t bitnum, gd_bit_t numbits) gd_nothrow
{
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
  N.u.bit.bitnum = bitnum;
  N.u.bit.numbits = numbits;
  N.e = NULL;
  N.scalar[0] = (bitnum == -1) ? (char *)"" : NULL;
  N.scalar[1] = (numbits == 0) ? (char *)"" : NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_recip(DIRFILE* D, const char* field_code, const char* in_field,
    double dividend) gd_nothrow
{
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
  N.u.recip.dividend = dividend;
  N.comp_scal = 0;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

#ifndef GD_NO_C99_API
int gd_alter_crecip(DIRFILE* D, const char* field_code, const char* in_field,
    double complex cdividend)
{
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
  N.u.recip.cdividend = cdividend;
  N.comp_scal = 1;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}
#endif

int gd_alter_crecip89(DIRFILE* D, const char* field_code, const char* in_field,
    const double cdividend[2]) gd_nothrow
{
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", [%g, %g]", D, field_code, in_field, cdividend[0],
      cdividend[1]);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_RECIP_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.scalar[0] = (cdividend[0] == 0 && cdividend[1] == 0) ? (char *)"" : NULL;
  _gd_a2c(N.u.recip.cdividend, cdividend);
  N.comp_scal = 1;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_divide(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
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

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_multiply(DIRFILE* D, const char* field_code, const char* in_field1,
    const char* in_field2) gd_nothrow
{
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

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_phase(DIRFILE* D, const char* field_code, const char* in_field,
    gd_shift_t shift) gd_nothrow
{
  gd_entry_t N;

  dtrace("%p, \"%s\", \"%s\", %lli", D, field_code, in_field, (long long)shift);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_PHASE_ENTRY;
  N.in_fields[0] = (char *)in_field;
  N.u.phase.shift = shift;
  N.e = NULL;
  N.scalar[0] = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_const(DIRFILE* D, const char* field_code, gd_type_t const_type)
  gd_nothrow
{
  gd_entry_t N;

  dtrace("%p, \"%s\", 0x%x", D, field_code, const_type);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  N.field_type = GD_CONST_ENTRY;
  N.u.cons.type = const_type;
  N.e = NULL;

  int ret = _GD_Change(D, field_code, &N, 0);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_polynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const double* a) gd_nothrow
{
  gd_entry_t N;
  int i;
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
    N.u.polynom.poly_ord = poly_ord;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.u.polynom.poly_ord = E->u.polynom.poly_ord;
  }
  N.in_fields[0] = (char *)in_field;
  N.e = NULL;

  if (a != NULL) {
    flags |= 1;
    for (i = 0; i <= N.u.polynom.poly_ord; ++i) {
      N.u.polynom.a[i] = a[i];
      N.scalar[i] = NULL;
    }
  } else
    for (i = 0; i <= N.u.polynom.poly_ord; ++i)
      N.scalar[i] = "";

  int ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_cpolynom(DIRFILE* D, const char* field_code, int poly_ord,
    const char* in_field, const GD_DCOMPLEXP(ca)) gd_nothrow
{
  gd_entry_t N;
  int i;
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
    N.u.polynom.poly_ord = poly_ord;
  else {
    gd_entry_t *E = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

    if (E == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
      dreturn("%i", -1);
      return -1;
    }

    N.u.polynom.poly_ord = E->u.polynom.poly_ord;
  }
  N.in_fields[0] = (char *)in_field;
  N.e = NULL;

  if (ca != NULL)  {
    flags |= 1;
    for (i = 0; i <= N.u.polynom.poly_ord; ++i) {
      _gd_ca2c(N.u.polynom.ca[i], ca, i);
      N.scalar[i] = NULL;
    }
  } else
    for (i = 0; i <= N.u.polynom.poly_ord; ++i)
      N.scalar[i] = "";

  int ret = _GD_Change(D, field_code, &N, flags);

  dreturn("%i", ret);
  return ret;
}

int gd_alter_spec(DIRFILE* D, const char* line, int move)
{
  char instring[GD_MAX_LINE_LENGTH];
  char outstring[GD_MAX_LINE_LENGTH];
  char *in_cols[MAX_IN_COLS];
  int n_cols;
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

  /* we do this to ensure line is not too long */
  strncpy(instring, line, GD_MAX_LINE_LENGTH - 1);
  instring[GD_MAX_LINE_LENGTH - 2] = '\0';

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  if (D->av)
    standards = D->standards;

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols,
      "dirfile_alter_spec()", 0, standards, D->flags & GD_PERMISSIVE);

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

  N = _GD_FindField(D, in_cols[0], D->entry, D->n_entries, NULL);

  if (N == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, in_cols[0]);
    dreturn("%i", -1);
    return -1;
  }

  /* Let the parser compose the entry */
  N = _GD_ParseFieldSpec(D, n_cols, in_cols, NULL, "dirfile_alter_spec()", 0, 
      N->fragment_index, standards, 0, 1, 0);

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

int gd_malter_spec(DIRFILE* D, const char* line, const char* parent, int move)
{
  char instring[GD_MAX_LINE_LENGTH];
  char outstring[GD_MAX_LINE_LENGTH];
  char *in_cols[MAX_IN_COLS];
  int n_cols;
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

  /* we do this to ensure line is not too long */
  strncpy(instring, line, GD_MAX_LINE_LENGTH - 1);
  instring[GD_MAX_LINE_LENGTH - 2] = '\0';

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  if (D->av)
    standards = D->standards;

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols,
      "dirfile_malter_spec()", 0, standards, D->flags & GD_PERMISSIVE);

  if (D->error) {
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Let the parser compose the entry */
  N = _GD_ParseFieldSpec(D, n_cols, in_cols, N, "dirfile_malter_spec()", 0,
      N->fragment_index, standards, 0, 1, 0);

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
