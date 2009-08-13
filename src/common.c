/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
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
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

/* _GD_GetLine: read non-comment line from format file.  The line is placed in
 *       *line.  Returns 1 if successful, 0 if unsuccessful.
 */
int _GD_GetLine(FILE *fp, char *line, int* linenum)
{
  char *ret_val;
  int first_char;

  dtrace("%p, %p, %p", fp, line, linenum);

  do {
    ret_val = fgets(line, GD_MAX_LINE_LENGTH, fp);
    (*linenum)++;
    first_char = 0;
    while (line[first_char] == ' ' || line[first_char] == '\t')
      ++first_char;
    line += first_char;
  } while (ret_val && (line[0] == '#' || line[0] == 0 || line[1] == 0));


  if (ret_val) {
    dreturn("\"%s\"", line);
    return 1; /* a line was read */
  }

  dreturn("%i", 0);
  return 0;  /* there were no valid lines */
}

/* This function is needed outside the legacy API to handle old format
 * files
 */
gd_type_t _GD_LegacyType(char c)
{
  switch (c) {
    case 'n':
      return GD_NULL;
    case 'c':
      return GD_UINT8;
    case 'u':
      return GD_UINT16;
    case 's':
      return GD_INT16;
    case 'U':
      return GD_UINT32;
    case 'i':
    case 'S':
      return GD_INT32;
    case 'f':
      return GD_FLOAT32;
    case 'd':
      return GD_FLOAT64;
  }

  return GD_UNKNOWN;
}

/* Binary search to find the field */
gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code,
    unsigned int *index)
{
  int c;
  unsigned int i, l = 0, u = D->n_entries;

  dtrace("%p, \"%s\", %p", D, field_code, index);

  while (l < u) {
    i = (l + u) / 2;
    c = strcmp(field_code, D->entry[i]->field);
    if (c < 0)
      u = i;
    else if (c > 0)
      l = i + 1;
    else {
      if (index != NULL) 
        *index = i;

      dreturn("%p", D->entry[i]);
      return D->entry[i];
    }
  }

  if (index != NULL) 
    *index = u;

  dreturn("%p", NULL);
  return NULL;
}

/* Insertion sort the entry list */
void _GD_InsertSort(DIRFILE* D, gd_entry_t* E, int u)
{
  dtrace("%p, %p, %i", D, E, u);

  memmove(&D->entry[u + 1], &D->entry[u], sizeof(gd_entry_t*) *
      (D->n_entries - u));

  D->entry[u] = E;

  dreturnvoid();
}

/* _GD_Alloc: allocate a buffer of the right type & size
*/
void* _GD_Alloc(DIRFILE* D, gd_type_t type, size_t n)
{
  void* ptr = NULL;

  dtrace("%p, 0x%x, %zi", D, type, n);
  if (n == 0)
    _GD_InternalError(D);

  if (type == GD_NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  else if (GD_SIZE(type) == 0) {
    _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  ptr = malloc(n * GD_SIZE(type));
  dreturn("%p", ptr);
  return ptr;
}

/* _GD_ScaleData: Compute data = data * m + b, for scalar m and b.
*/
void _GD_ScaleData(DIRFILE* D, void *data, gd_type_t type, size_t npts,
    double m, double b)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %zi, %g, %g", D, data, type, npts, m, b);

  switch (type) {
    case GD_NULL:
      break;
    case GD_INT8:
      for (i = 0; i < npts; i++)
        ((int8_t*)data)[i] = (int8_t)(((int8_t*)data)[i] * m + b);
      break;
    case GD_UINT8:
      for (i = 0; i < npts; i++)
        ((uint8_t*)data)[i] = (uint8_t)(((uint8_t*)data)[i] * m + b);
      break;
    case GD_INT16:
      for (i = 0; i < npts; i++)
        ((int16_t*)data)[i] = (int16_t)(((int16_t*)data)[i] * m + b);
      break;
    case GD_UINT16:
      for (i = 0; i < npts; i++)
        ((uint16_t*)data)[i] = (uint16_t)(((uint16_t*)data)[i] * m + b);
      break;
    case GD_INT32:
      for (i = 0; i < npts; i++)
        ((int32_t*)data)[i] = (int32_t)(((int32_t*)data)[i] * m + b);
      break;
    case GD_UINT32:
      for (i = 0; i < npts; i++)
        ((uint32_t*)data)[i] = (uint32_t)(((uint32_t*)data)[i] * m + b);
      break;
    case GD_INT64:
      for (i = 0; i < npts; i++)
        ((int64_t*)data)[i] = (int64_t)(((int64_t*)data)[i] * m + b);
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++)
        ((uint64_t*)data)[i] = (uint64_t)(((uint64_t*)data)[i] * m + b);
      break;
    case GD_FLOAT:
      for (i = 0; i < npts; i++)
        ((float*)data)[i] = (float)(((float*)data)[i] * m + b);
      break;
    case GD_DOUBLE:
      for (i = 0; i < npts; i++)
        ((double*)data)[i] = ((double*)data)[i] * m + b;
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* _GD_MakeDummyLinterp: Make an empty linterp
*/
static void _GD_MakeDummyLinterp(DIRFILE* D, struct _gd_private_entry *e)
{
  dtrace("%p, %p", D, e);

  e->table_len = 2;
  e->x = (double *)malloc(2*sizeof(double));
  e->y = (double *)malloc(2*sizeof(double));

  if (e->x == NULL || e->y == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  else {
    e->x[0] = 0;
    e->y[0] = 0;
    e->x[1] = 1;
    e->y[1] = 1;
  }

  dreturnvoid();
}

/* _GD_ReadLinterpFile: Read in the linterp data for this field
*/
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E)
{
  FILE *fp;
  int i;
  char line[GD_MAX_LINE_LENGTH];
  int linenum = 0;

  dtrace("%p, %p", D, E);

  fp = fopen(E->table, "r");
  if (fp == NULL) {
    _GD_MakeDummyLinterp(D, E->e);
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_OPEN, NULL, 0, E->table);
    dreturnvoid();
    return;
  }

  dtrace("[%p]1", E->e->entry[0]);

  /* first read the file to see how big it is */
  i = 0;
  while (_GD_GetLine(fp, line, &linenum))
    i++;

  if (i < 2) {
    _GD_MakeDummyLinterp(D, E->e);
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, NULL, 0, E->table);
    dreturnvoid();
    return;
  }

  dtrace("[%p]2", E->e->entry[0]);

  E->e->table_len = i;
  E->e->x = (double *)malloc(i * sizeof(double));
  E->e->y = (double *)malloc(i * sizeof(double));
  if (E->e->x == NULL || E->e->y == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturnvoid();
    return;
  }

  dtrace("[%p]3", E->e->entry[0]);

  /* now read in the data */
  rewind(fp);
  linenum = 0;
  for (i = 0; i < E->e->table_len; i++) {
    _GD_GetLine(fp, line, &linenum);
    sscanf(line, "%lg %lg", &(E->e->x[i]), &(E->e->y[i]));
  }

  dreturnvoid();
}

/* _GD_GetIndex: get LUT index.
*/
static size_t _GD_GetIndex(double x, double lx[], size_t idx, size_t n)
{
  dtrace("%g, %p, %zi, %zi", x, lx, idx, n);

  /* Just linearly search - we're probably right to start    */
  /* increment until we are bigger */
  while ((idx < n - 2) && (x > lx[idx]))
    idx++;

  /* decrement until we are smaller */
  while ((idx > 0) && (x < lx[idx]))
    idx--;

  dreturn("%zi", idx);
  return idx;
}

/* _GD_AddData: add vector B to vector A.  B is unchanged
*/
void _GD_AddData(DIRFILE* D, void *A, unsigned int spfA, void *B,
    unsigned int spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zi", D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)A)[i] = (int8_t)(((int8_t*)A)[i] + (((int8_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)A)[i] = (uint8_t)(((uint8_t*)A)[i] + (((uint8_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)A)[i] = (int16_t)(((int16_t*)A)[i] + (((int16_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)A)[i] = (uint16_t)(((uint16_t*)A)[i] + (((uint16_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)A)[i] = (int32_t)(((int32_t*)A)[i] + (((int32_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)A)[i] = (uint32_t)(((uint32_t*)A)[i] + (((uint32_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)A)[i] = (int64_t)(((int64_t*)A)[i] + (((int64_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)A)[i] = (uint64_t)(((uint64_t*)A)[i] + (((uint64_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_FLOAT:
      for (i = 0; i < n; i++)
        ((float*)A)[i] += ((float*)B)[i * spfB / spfA];
      break;
    case GD_DOUBLE:
      for (i = 0; i < n; i++)
        ((double*)A)[i] += ((double*)B)[i * spfB / spfA];
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* _GD_LinterpData: calibrate data using lookup table lx and ly
*/
void _GD_LinterpData(DIRFILE* D, const void *data, gd_type_t type, size_t npts,
    double *lx, double *ly, size_t n_ln)
{
  int idx = 0;
  size_t i;
  double x;

  dtrace("%p, %p, 0x%x, %zi, %p, %p, %zi", D, data, type, npts, lx, ly, n_ln);

  switch (type) {
    case GD_NULL:
      break;
    case GD_INT8:
      for (i = 0; i < npts; i++) {
        x = ((int8_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int8_t *)data)[i] = (int8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT8:
      for (i = 0; i < npts; i++) {
        x = ((uint8_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint8_t *)data)[i] = (uint8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT16:
      for (i = 0; i < npts; i++) {
        x = ((int16_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int16_t *)data)[i] = (int16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT16:
      for (i = 0; i < npts; i++) {
        x = ((uint16_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx,n_ln);
        ((uint16_t *)data)[i] = (uint16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT32:
      for (i = 0; i < npts; i++) {
        x = ((int32_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int32_t *)data)[i] = (int32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT32:
      for (i = 0; i < npts; i++) {
        x = ((uint32_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint32_t *)data)[i] = (uint32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT64:
      for (i = 0; i < npts; i++) {
        x = (double)((int64_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int64_t *)data)[i] = (int64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++) {
        x = (double)((uint64_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint64_t *)data)[i] = (uint64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_FLOAT:
      for (i = 0; i < npts; i++) {
        x = ((float *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((float *)data)[i] = (float)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_DOUBLE:
      for (i = 0; i < npts; i++) {
        x = ((double *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((double *)data)[i] = (double)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}
/* vim: ts=2 sw=2 et tw=80
*/
