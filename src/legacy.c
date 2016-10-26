/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2003-2005 Theodore Kisner
 * Copyright (C) 2005-2016 D. V. Wiebe
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

static struct {
  unsigned int n;
  DIRFILE** D;
} _GD_Dirfiles = {0, NULL};

/* Error-reporting kludge for deprecated API */
static DIRFILE _GD_GlobalErrors = {
#ifndef GD_NO_C99_API
  .error = 0,
  .suberror = 0,
  .error_string = NULL,
  .error_file = NULL,
  .error_line = 0,
  .stdlib_errno = 0,
  .flags = GD_INVALID,
#else
  0, 0, NULL, NULL, 0, 0, GD_INVALID
#endif
};

/* old error strings */
const char *GD_ERROR_CODES[GD_N_ERROR_CODES] = {
  "Success", /* GD_E_OK */
  "Error in Format file", /* GD_E_FORMAT */
  NULL, /* GD_E_CREAT */
  "Bad field code", /* GD_E_BAD_CODE */
  "Bad data type", /* GD_E_BAD_TYPE */
  "I/O error", /* GD_E_IO */
  "Internal error", /* GD_E_INTERNAL_ERROR */
  "Memory allocation failed", /* GD_E_ALLOC */
  "Request out-of-range", /* GD_E_RANGE */
  "Syntax error in LINTERP table", /* GD_E_LUT */
  "Too many levels of recursion", /* GD_E_RECURSE_LEVEL */
  "Bad dirfile", /* GD_E_BAD_DIRFILE */
  "Bad field type", /* GD_E_BAD_FIELD_TYPE */
  "Read-only dirfile", /* GD_E_ACCMODE */
  "Operation not supported by current encoding scheme", /* GD_E_UNSUPPORTED */
  "Unknown encoding scheme", /* GD_E_UNKNOWN_ENCODING */
  NULL, /* GD_E_BAD_ENTRY */
  NULL, /* GD_E_DUPLICATE */
  "Scalar field found where vector field expected", /* GD_E_DIMENSION */
  NULL, /* GD_E_BAD_INDEX */
  "Scalar field code not found", /* GD_E_BAD_SCALAR */
  "Bad REFERENCE field", /* GD_E_BAD_REFERENCE */
  "Operation prohibited by protection level", /* GD_E_PROTECTED */
  NULL, /* GD_E_DELETE */
  "Bad argument", /* GD_E_ARGUMENT */
  NULL, /* GD_E_CALLBACK */
  NULL, /* GD_E_EXISTS */
  NULL, /* GD_E_UNCLEAN_DB */
  "Improper domain", /* GD_E_DOMAIN */
  NULL, /* GD_E_BOUNDS */
  "Line too long", /* GD_E_LINE_TOO_LONG */
};

static struct FormatType Format;

/* _GD_CopyGlobalError: Copy the last error message to the global error buffer.
*/
static int _GD_CopyGlobalError(DIRFILE* D)
{
  dtrace("%p", D);

  _GD_GlobalErrors.suberror = D->suberror;
  _GD_GlobalErrors.error_line = D->error_line;
  free(_GD_GlobalErrors.error_file);
  _GD_GlobalErrors.error_file = (D->error_file) ? strdup(D->error_file) : NULL;
  free(_GD_GlobalErrors.error_string);
  _GD_GlobalErrors.error_string = (D->error_string) ? strdup(D->error_string) :
    NULL;

  dreturn("%i", D->error);
  return _GD_GlobalErrors.error = D->error;
}

/* legacy wrapper for gd_error_string()
*/
char *GetDataErrorString(char* buffer, size_t buflen) gd_nothrow
{
  return gd_error_string(&_GD_GlobalErrors, buffer, buflen);
}

/* _GD_GetDirfile: Locate the legacy DIRFILE given the filespec.  This started
 * life as GetFormat...
 */
static DIRFILE *_GD_GetDirfile(const char *filename_in, int mode,
    int *error_code)
{
  unsigned int i_dirfile;
  void *ptr;
  char *filedir;

  dtrace("\"%s\", %x", filename_in, mode);

  filedir = strdup(filename_in);

  if (!filedir) {
    *error_code = -GD_E_ALLOC;
    dreturn("%p", NULL);
    return NULL;
  }

  if (filedir[strlen(filedir) - 1] == '/')
    filedir[strlen(filedir) - 1] = '\0';

  /* first check to see if we have already read it */
  for (i_dirfile = 0; i_dirfile < _GD_Dirfiles.n; i_dirfile++) {
    if (strcmp(filedir, _GD_Dirfiles.D[i_dirfile]->name) == 0) {
      /* if the dirfile was previously opened read-only, close it so we can
       * re-open it read-write */
      if ((mode & GD_RDWR) && (_GD_Dirfiles.D[i_dirfile]->flags & GD_ACCMODE) ==
          GD_RDONLY) {
        /* close it */
        gd_discard(_GD_Dirfiles.D[i_dirfile]);

        /* copy the last dirfile in the list over top of this one and decrement
         * the counter -- next realloc will do nothing */
        _GD_Dirfiles.D[i_dirfile] = _GD_Dirfiles.D[--_GD_Dirfiles.n];
      } else {
        free(filedir);
        _GD_ClearError(_GD_Dirfiles.D[i_dirfile]);
        dreturn("%p", _GD_Dirfiles.D[i_dirfile]);
        return _GD_Dirfiles.D[i_dirfile];
      }
    }
  }

  /* if we get here, the file has not yet been read */
  /* Allocate the memory, then fill.  If we have an error, */
  /*  we will have to free the memory... */
  ptr = realloc(_GD_Dirfiles.D, (_GD_Dirfiles.n + 1) * sizeof(*_GD_Dirfiles.D));
  if (ptr == NULL) {
    *error_code = -(_GD_GlobalErrors.error = GD_E_ALLOC);
    free(filedir);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_Dirfiles.D = ptr;

  /* Open a dirfile */
  _GD_Dirfiles.D[_GD_Dirfiles.n] = gd_open(filedir, mode);
  free(filedir);

  /* Error encountered -- clean up */
  if (_GD_Dirfiles.D[_GD_Dirfiles.n]->error != GD_E_OK) {
    *error_code = -_GD_CopyGlobalError(_GD_Dirfiles.D[_GD_Dirfiles.n]);
    gd_discard(_GD_Dirfiles.D[_GD_Dirfiles.n]);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", _GD_Dirfiles.D[_GD_Dirfiles.n]);
  return _GD_Dirfiles.D[_GD_Dirfiles.n++];
}

static void CopyRawEntry(struct RawEntryType* R, gd_entry_t* E)
{
  dtrace("%p, %p", R, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  R->field = E->field;

  switch(E->EN(raw,data_type)) {
    case GD_UINT8:
      R->type = 'c';
      break;
    case GD_UINT16:
      R->type = 'u';
      break;
    case GD_INT16:
      R->type = 's';
      break;
    case GD_UINT32:
      R->type = 'U';
      break;
    case GD_INT32:
      R->type = 'S';
      break;
    case GD_FLOAT32:
      R->type = 'f';
      break;
    case GD_FLOAT64:
      R->type = 'd';
      break;
    default: /* Well, this isn't right, but it's the best we can do. */
      R->type = 'n';
      break;
  }

  R->size = (int)E->e->u.raw.size;
  R->samples_per_frame = (int)E->EN(raw,spf);

  dreturnvoid();
}

/* We operate under the myth that POLYNOMs are actually LINCOMs.  We report them
 * to have one input field, and discard non-linear terms */
static void CopyPolynomEntry(struct LincomEntryType* L, gd_entry_t* E)
{
  dtrace("%p, %p", L, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  L->field = E->field;
  L->n_fields = 1;
  L->in_fields[0] = E->in_fields[0];
  L->m[0] = E->EN(polynom,a)[1];
  L->b[0] = E->EN(polynom,a)[0];

  dreturnvoid();
}

static void CopyLincomEntry(struct LincomEntryType* L, gd_entry_t* E)
{
  int i;

  dtrace("%p, %p", L, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  L->field = E->field;
  L->n_fields = E->EN(lincom,n_fields);
  for (i = 0; i < E->EN(lincom,n_fields); ++i) {
    L->in_fields[i] = E->in_fields[i];
    L->m[i] = E->EN(lincom,m)[i];
    L->b[i] = E->EN(lincom,b)[i];
  }

  dreturnvoid();
}

static void CopyLinterpEntry(struct LinterpEntryType* L, gd_entry_t* E)
{
  dtrace("%p, %p", L, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  L->field = E->field;
  L->raw_field = E->in_fields[0];
  L->linterp_file = E->EN(linterp,table);

  dreturnvoid();
}

static void CopyBitEntry(struct BitEntryType* B, gd_entry_t* E)
{
  dtrace("%p, %p", B, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  B->field = E->field;
  B->raw_field = E->in_fields[0];
  B->bitnum = E->EN(bit,bitnum);
  B->numbits = E->EN(bit,numbits);

  dreturnvoid();
}

static void CopyMultDivEntry(struct MultiplyEntryType* M, gd_entry_t* E)
{
  dtrace("%p, %p", M, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  M->field = E->field;
  M->in_fields[0] = E->in_fields[0];
  M->in_fields[1] = E->in_fields[1];

  dreturnvoid();
}

static void CopyReciprocalEntry(struct LincomEntryType* L, gd_entry_t* E)
{
  dtrace("%p, %p", L, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  L->field = E->field;
  L->n_fields = 1;
  L->in_fields[0] = E->in_fields[0];
  L->m[0] = E->EN(recip,dividend);
  L->b[0] = 0;

  dreturnvoid();
}

static void CopyPhaseEntry(struct PhaseEntryType* P, gd_entry_t* E)
{
  dtrace("%p, %p", P, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  P->field = E->field;
  P->raw_field = E->in_fields[0];
  P->shift = E->EN(phase,shift);

  dreturnvoid();
}

static void CopyWindowEntry(struct MPlexEntryType* M, gd_entry_t* E)
{
  dtrace("%p, %p", M, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  M->field = E->field;
  M->data_field = E->in_fields[0];
  M->cnt_field = E->in_fields[1];
  M->i = (int)E->EN(window,windop);
  M->max_i = (int)E->EN(window,threshold.i);

  dreturnvoid();
}

static void CopyMplexEntry(struct MPlexEntryType* M, gd_entry_t* E)
{
  dtrace("%p, %p", M, E);

  if (E == NULL) {
    dreturnvoid();
    return;
  }

  M->field = E->field;
  M->data_field = E->in_fields[0];
  M->cnt_field = E->in_fields[1];
  M->i = (int)E->EN(mplex,count_val);
  M->max_i = (int)E->EN(mplex,period);

  dreturnvoid();
}

/* Okay, reconstruct the old FormatType.  This is painful. */
struct FormatType *GetFormat(const char *filedir, int *error_code) gd_nothrow
{
  unsigned int i;

  int nraw = 0;
  int nlincom = 0;
  int nlinterp = 0;
  int nmultiply = 0;
  int nbit = 0;
  int nphase = 0;
  int nmplex = 0;
  DIRFILE *D;

  dtrace("\"%s\", %p", filedir, error_code);

  D = _GD_GetDirfile(filedir, GD_RDONLY, error_code);

  if (!D) {
    dreturn("%p", NULL);
    return NULL;
  }

  memset(&Format, 0, sizeof(Format));

  /* fill the structure -- like everything about the legacy API, this is
   * not thread-safe */
  Format.FileDirName = filedir; 
  Format.frame_offset = (int)D->fragment[0].frame_offset;
  CopyRawEntry(&Format.first_field, D->reference_field);

  /* Pass one: run through the entry list and count the number of different
   * types */
  for (i = 0; i < D->n_entries; ++i) 
    switch(D->entry[i]->field_type) {
      case GD_RAW_ENTRY:
        Format.n_raw++;
        break;
      case GD_LINCOM_ENTRY:
      case GD_POLYNOM_ENTRY:
      case GD_RECIP_ENTRY:
        Format.n_lincom++;
        break;
      case GD_LINTERP_ENTRY:
        Format.n_linterp++;
        break;
      case GD_BIT_ENTRY:
      case GD_SBIT_ENTRY:
        Format.n_bit++;
        break;
      case GD_MULTIPLY_ENTRY:
      case GD_DIVIDE_ENTRY:
      case GD_INDIR_ENTRY:
      case GD_SINDIR_ENTRY:
        Format.n_multiply++;
        break;
      case GD_PHASE_ENTRY:
        Format.n_phase++;
        break;
      case GD_WINDOW_ENTRY:
      case GD_MPLEX_ENTRY:
        Format.n_mplex++;
        break;
      case GD_NO_ENTRY:
      case GD_ALIAS_ENTRY:
      case GD_CONST_ENTRY:
      case GD_CARRAY_ENTRY:
      case GD_SARRAY_ENTRY:
      case GD_INDEX_ENTRY:
      case GD_STRING_ENTRY:
        break;
    }

  /* Now reallocate the Entry arrays */
  free(Format.rawEntries);
  free(Format.lincomEntries);
  free(Format.linterpEntries);
  free(Format.mplexEntries);
  free(Format.multiplyEntries);
  free(Format.bitEntries);
  free(Format.phaseEntries);

  Format.rawEntries = malloc(Format.n_raw * sizeof(*Format.rawEntries));
  Format.lincomEntries = malloc(Format.n_lincom *
      sizeof(*Format.lincomEntries));
  Format.linterpEntries = malloc(Format.n_linterp *
      sizeof(*Format.linterpEntries));
  Format.mplexEntries = malloc(Format.n_mplex * sizeof(*Format.mplexEntries));
  Format.multiplyEntries = malloc(Format.n_multiply *
      sizeof(*Format.multiplyEntries));
  Format.bitEntries = malloc(Format.n_bit * sizeof(*Format.bitEntries));
  Format.phaseEntries = malloc(Format.n_phase * sizeof(*Format.phaseEntries));

  if (Format.rawEntries == NULL || Format.lincomEntries == NULL ||
      Format.linterpEntries == NULL || Format.mplexEntries == NULL ||
      Format.multiplyEntries == NULL || Format.bitEntries == NULL ||
      Format.phaseEntries == NULL)
  {
    D->error = GD_E_ALLOC;
    *error_code = -_GD_CopyGlobalError(D);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Pass 2: Fill the Entry structs */
  for (i = 0; i < D->n_entries; ++i)
    switch(D->entry[i]->field_type) {
      case GD_RAW_ENTRY:
        CopyRawEntry(&Format.rawEntries[nraw++], D->entry[i]);
        break;
      case GD_POLYNOM_ENTRY:
        CopyPolynomEntry(&Format.lincomEntries[nlincom++], D->entry[i]);
        break;
      case GD_LINCOM_ENTRY:
        CopyLincomEntry(&Format.lincomEntries[nlincom++], D->entry[i]);
        break;
      case GD_LINTERP_ENTRY:
        CopyLinterpEntry(&Format.linterpEntries[nlinterp++], D->entry[i]);
        break;
      case GD_BIT_ENTRY:
      case GD_SBIT_ENTRY:
        CopyBitEntry(&Format.bitEntries[nbit++], D->entry[i]);
        break;
      case GD_RECIP_ENTRY:
        CopyReciprocalEntry(&Format.lincomEntries[nlincom++], D->entry[i]);
        break;
      case GD_MULTIPLY_ENTRY:
      case GD_DIVIDE_ENTRY:
      case GD_INDIR_ENTRY:
      case GD_SINDIR_ENTRY:
        CopyMultDivEntry(&Format.multiplyEntries[nmultiply++], D->entry[i]);
        break;
      case GD_PHASE_ENTRY:
        CopyPhaseEntry(&Format.phaseEntries[nphase++], D->entry[i]);
        break;
      case GD_WINDOW_ENTRY:
        CopyWindowEntry(&Format.mplexEntries[nmplex++], D->entry[i]);
        break;
      case GD_MPLEX_ENTRY:
        CopyMplexEntry(&Format.mplexEntries[nmplex++], D->entry[i]);
        break;
      case GD_STRING_ENTRY:
      case GD_CONST_ENTRY:
      case GD_CARRAY_ENTRY:
      case GD_SARRAY_ENTRY:
      case GD_INDEX_ENTRY:
      case GD_ALIAS_ENTRY:
      case GD_NO_ENTRY:
        break;
    }

  dreturn("%p", &Format);
  return &Format;
}

/* legacy interface to getdata() */
int GetData(const char *filename, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char return_type, void *data_out, int *error_code)
{
  DIRFILE* D;
  int nread;

  dtrace("\"%s\", \"%s\", %i, %i, %i, %i, '%c', %p, %p", filename, field_code,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out,
      error_code);

  D = _GD_GetDirfile(filename, GD_RDONLY, error_code);

  if (!D) {
    dreturn("%i", 0);
    return 0;
  }

  nread = (int)gd_getdata64(D, field_code, (off64_t)first_frame,
      (off64_t)first_samp, (size_t)num_frames, (size_t)num_samp,
      _GD_LegacyType(return_type), data_out);
  *error_code = -_GD_CopyGlobalError(D);

  dreturn("%i", nread);
  return nread;
}

/* legacy interface to get_nframes() --- the third argument to this function
 * has been ignored since at least 2005 (and why does it come after
 * error_code?)
 */
int GetNFrames(const char *filename, int *error_code,
    const void *unused gd_unused_)
{
  DIRFILE* D;
  int nf;

  dtrace("\"%s\", %p, <unused>", filename, error_code);

  D = _GD_GetDirfile(filename, GD_RDONLY, error_code);

  if (!D) {
    dreturn("%i", 0);
    return 0;
  }

  nf = (int)gd_nframes(D);
  *error_code = -_GD_CopyGlobalError(D);

  dreturn("%i", nf);
  return nf;
}

/* legacy interface to get_spf()
*/
int GetSamplesPerFrame(const char *filename, const char *field_code,
    int *error_code) gd_nothrow
{
  DIRFILE* D;
  int spf;

  dtrace("\"%s\", \"%s\", %p", filename, field_code, error_code);

  D = _GD_GetDirfile(filename, GD_RDONLY, error_code);

  if (!D) {
    dreturn("%i", 0);
    return 0;
  }

  spf = (int)gd_spf(D, field_code);
  *error_code = -_GD_CopyGlobalError(D);

  dreturn("%i", spf);
  return spf;
}

/* legacy interface to putdata()
*/
int PutData(const char *filename, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    char data_type, const void *data_in, int *error_code)
{
  DIRFILE* D;
  int n_write = 0;

  dtrace("\"%s\", \"%s\", %i, %i, %i, %i, '%c', %p, %p", filename, field_code,
      first_frame, first_samp, num_frames, num_samp, data_type, data_in,
      error_code);

  D = _GD_GetDirfile(filename, GD_RDWR | GD_UNENCODED, error_code);

  if (!D) {
    dreturn("%i", 0);
    return 0;
  }

  n_write = (int)gd_putdata64(D, field_code, (off64_t)first_frame,
      (off64_t)first_samp, (size_t)num_frames, (size_t)num_samp,
      _GD_LegacyType(data_type), data_in);
  *error_code = -_GD_CopyGlobalError(D);

  dreturn("%i", n_write);
  return n_write;
}
/* vim: ts=2 sw=2 et
*/
