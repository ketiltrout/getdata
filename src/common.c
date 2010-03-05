/* (C) 2002-2005 C. Barth Netterfield
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
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
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
    gd_entry_t** list, unsigned int u, unsigned int *index)
{
  int c;
  unsigned int i, l = 0;

  dtrace("%p, \"%s\", %p, %u, %p", D, field_code, list, u, index);

  /* handle FILEFRAM */
  if (D->standards < 6 && (D->flags & GD_PEDANTIC) &&
      strcmp(field_code, "FILEFRAM") == 0)
    field_code = "INDEX";

  while (l < u) {
    i = (l + u) / 2;
    c = strcmp(field_code, list[i]->field);
    if (c < 0)
      u = i;
    else if (c > 0)
      l = i + 1;
    else {
      if (index != NULL) 
        *index = i;

      dreturn("%p", list[i]);
      return list[i];
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
  if (n == 0) {
    _GD_InternalError(D);
    dreturn("%p", NULL);
    return NULL;
  }

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

  if (ptr == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  dreturn("%p", ptr);
  return ptr;
}

/* _GD_MakeDummyLinterp: Make an empty linterp
*/
static void _GD_MakeDummyLinterp(DIRFILE* D, struct _gd_private_entry *e)
{
  dtrace("%p, %p", D, e);

  e->table_len = 2;
  e->complex_table = 0;
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

/* compute LUT table path -- this is used by _GD_Change, so e may not be E->e */
int _GD_SetTablePath(DIRFILE *D, gd_entry_t *E, struct _gd_private_entry *e)
{
  dtrace("%p, %p, %p", D, E, e);

  if (E->table[0] == '/') {
    e->table_path = strdup(E->table);
    if (e->table_path == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", 1);
      return 1;
    }
  } else {
    e->table_path = malloc(FILENAME_MAX);
    if (e->table_path == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", 1);
      return 1;
    }
    char temp_buffer[FILENAME_MAX];
    strcpy(temp_buffer, D->fragment[E->fragment_index].cname);
    strcpy(e->table_path, dirname(temp_buffer));
    strcat(e->table_path, "/");
    strcat(e->table_path, E->table);
  }

  dreturn("%i", 0);
  return 0;
}

/* _GD_ReadLinterpFile: Read in the linterp data for this field
*/
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E)
{
  FILE *fp;
  int i;
  char line[GD_MAX_LINE_LENGTH];
  int linenum = 0;
  double yr, yi;

  dtrace("%p, %p", D, E);

  if (E->e->table_path == NULL)
    if (_GD_SetTablePath(D, E, E->e)) {
      dreturnvoid();
      return;
    }

  E->e->complex_table = 0;

  fp = fopen(E->e->table_path, "r");
  if (fp == NULL) {
    _GD_MakeDummyLinterp(D, E->e);
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_OPEN, NULL, 0,
        E->e->table_path);
    dreturnvoid();
    return;
  }

  /* first read the file to see how big it is, also figure out whether the
   * table is complex valued */
  i = 0;
  while (_GD_GetLine(fp, line, &linenum)) {
    if (i == 0) { /* complex check */
      char ystr[50];
      if (sscanf(line, "%lg %49s", &yr, ystr) == 2)
        E->e->complex_table = (strchr(ystr, ';') == NULL) ? 0 : 1;
    }
    i++;
  }

  if (i < 2) {
    _GD_MakeDummyLinterp(D, E->e);
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, NULL, 0,
        E->e->table_path);
    fclose(fp);
    dreturnvoid();
    return;
  }

  E->e->table_len = i;

  E->e->x = (double *)malloc(i * sizeof(double));

  if (E->e->complex_table)
    E->e->cy = (double complex*)malloc(i * sizeof(double complex));
  else
    E->e->y = (double *)malloc(i * sizeof(double));

  if (E->e->x == NULL || E->e->y == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    fclose(fp);
    dreturnvoid();
    return;
  }

  /* now read in the data */
  rewind(fp);
  linenum = 0;
  for (i = 0; i < E->e->table_len; i++) {
    _GD_GetLine(fp, line, &linenum);
    if (E->e->complex_table) {
      sscanf(line, "%lg %lg;%lg", &(E->e->x[i]), &yr, &yi);
      E->e->cy[i] = yr + _Complex_I * yi;
    } else
      sscanf(line, "%lg %lg", &(E->e->x[i]), &(E->e->y[i]));
  }

  fclose(fp);
  dreturnvoid();
}

/* _GD_GetIndex: get LUT index.
*/
static size_t _GD_GetIndex(double x, const double *lx, size_t idx, size_t n)
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
void _GD_AddData(DIRFILE* D, void *A, gd_spf_t spfA, void *B,
    gd_spf_t spfB, gd_type_t type, size_t n)
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
    case GD_FLOAT32:
      for (i = 0; i < n; i++)
        ((float*)A)[i] += ((float*)B)[i * spfB / spfA];
      break;
    case GD_FLOAT64:
      for (i = 0; i < n; i++)
        ((double*)A)[i] += ((double*)B)[i * spfB / spfA];
      break;
    case GD_COMPLEX64:
      for (i = 0; i < n; i++)
        ((float complex*)A)[i] += ((float complex*)B)[i * spfB / spfA];
      break;
    case GD_COMPLEX128:
      for (i = 0; i < n; i++)
        ((double complex*)A)[i] += ((double complex*)B)[i * spfB / spfA];
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* _GD_LinterpData: calibrate data using lookup table lx and ly
*/
void _GD_LinterpData(DIRFILE* D, void *data, gd_type_t type,
    const double *data_in, size_t npts, const double *lx, const double *ly,
    size_t n_ln)
{
  int idx = 0;
  size_t i;
  double x;

  dtrace("%p, %p, 0x%x, %p, %zi, %p, %p, %zi", D, data, type, data_in, npts,
      lx, ly, n_ln);

  switch (type) {
    case GD_NULL:
      break;
    case GD_INT8:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int8_t *)data)[i] = (int8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT8:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint8_t *)data)[i] = (uint8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT16:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int16_t *)data)[i] = (int16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT16:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx,n_ln);
        ((uint16_t *)data)[i] = (uint16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT32:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int32_t *)data)[i] = (int32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT32:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint32_t *)data)[i] = (uint32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int64_t *)data)[i] = (int64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint64_t *)data)[i] = (uint64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_FLOAT32:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((float *)data)[i] = (float)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_FLOAT64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((double *)data)[i] = (double)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_COMPLEX64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((float complex *)data)[i] = (float complex)(ly[idx] + (ly[idx + 1] -
            ly[idx]) / (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_COMPLEX128:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((double complex *)data)[i] = (double complex)(ly[idx] + (ly[idx + 1] -
            ly[idx]) / (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* _GD_CLinterpData: calibrate data using lookup table lx and complex ly
*/
void _GD_CLinterpData(DIRFILE* D, void *data, gd_type_t type,
    const double *data_in, size_t npts, const double *lx,
    const double complex *ly, size_t n_ln)
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
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int8_t *)data)[i] = (int8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT8:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint8_t *)data)[i] = (uint8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT16:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int16_t *)data)[i] = (int16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT16:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx,n_ln);
        ((uint16_t *)data)[i] = (uint16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT32:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int32_t *)data)[i] = (int32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT32:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint32_t *)data)[i] = (uint32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int64_t *)data)[i] = (int64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint64_t *)data)[i] = (uint64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_FLOAT32:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((float *)data)[i] = (float)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_FLOAT64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((double *)data)[i] = (double)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_COMPLEX64:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((float complex *)data)[i] = (float complex)(ly[idx] + (ly[idx + 1] -
            ly[idx]) / (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_COMPLEX128:
      for (i = 0; i < npts; i++) {
        x = data_in[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((double complex *)data)[i] = (double complex)(ly[idx] + (ly[idx + 1] -
            ly[idx]) / (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* macros to reduce tangly code */
#define LINCOM1(t) for (i = 0; i < n_read; i++) \
                            ((t*)data1)[i] = (t)(((t*)data1)[i] * m[0] + b[0])

#define LINCOM2(t) for (i = 0; i < n_read; i++) \
                            ((t*)data1)[i] = (t)(((t*)data1)[i] * m[0] + \
                              (data2[i * spf[1] / spf[0]] * m[1] + b[0] + b[1]))

#define LINCOM3(t) for (i = 0; i < n_read; i++) \
                            ((t*)data1)[i] = (t)(((t*)data1)[i] * m[0] + \
                              (data2[i * spf[1] / spf[0]] * m[1] + \
                               data3[i * spf[2] / spf[0]] * m[2] + \
                               b[0] + b[1] + b[2]))

#define LINCOM(t) \
  switch (n) { \
    case 1: LINCOM1(t); break; \
    case 2: LINCOM2(t); break; \
    case 3: LINCOM3(t); break; \
    default: _GD_InternalError(D); \
  }

/* Compute a lincom, all at once */
void _GD_LincomData(DIRFILE* D, int n, void* data1, gd_type_t return_type,
    double *data2, double *data3, double* m, double *b, gd_spf_t *spf,
    size_t n_read)
{
  size_t i;

  dtrace("%p, %i, %p, 0x%x, %p, %p, %p, %p, %p, %zi", D, n, data1, return_type,
      data2, data3, m, b, spf, n_read);

  switch(return_type) {
    case GD_NULL:
      break;
    case GD_UINT8:
      LINCOM(uint8_t);
      break;
    case GD_INT8:
      LINCOM(int8_t);
      break;
    case GD_UINT16:
      LINCOM(uint16_t);
      break;
    case GD_INT16:
      LINCOM(int16_t);
      break;
    case GD_UINT32:
      LINCOM(uint32_t);
      break;
    case GD_INT32:
      LINCOM(int32_t);
      break;
    case GD_UINT64:
      LINCOM(uint64_t);
      break;
    case GD_INT64:
      LINCOM(int64_t);
      break;
    case GD_FLOAT32:
      LINCOM(float);
      break;
    case GD_FLOAT64:
      LINCOM(double);
      break;
    case GD_COMPLEX64:
      LINCOM(float complex);
      break;
    case GD_COMPLEX128:
      LINCOM(double complex);
      break;
    default:
      _GD_InternalError(D);
  }

  dreturnvoid();
}

/* Compute a complex valued lincom, all at once */
void _GD_CLincomData(DIRFILE* D, int n, void* data1, gd_type_t return_type,
    double complex *data2, double complex *data3, double complex* m,
    double complex *b, gd_spf_t *spf, size_t n_read)
{
  size_t i;

  dtrace("%p, %i, %p, 0x%x, %p, %p, %p, %p, %p, %zi", D, n, data1, return_type,
      data2, data3, m, b, spf, n_read);

  switch(return_type) {
    case GD_NULL:
      break;
    case GD_UINT8:
      LINCOM(uint8_t);
      break;
    case GD_INT8:
      LINCOM(int8_t);
      break;
    case GD_UINT16:
      LINCOM(uint16_t);
      break;
    case GD_INT16:
      LINCOM(int16_t);
      break;
    case GD_UINT32:
      LINCOM(uint32_t);
      break;
    case GD_INT32:
      LINCOM(int32_t);
      break;
    case GD_UINT64:
      LINCOM(uint64_t);
      break;
    case GD_INT64:
      LINCOM(int64_t);
      break;
    case GD_FLOAT32:
      LINCOM(float);
      break;
    case GD_FLOAT64:
      LINCOM(double);
      break;
    case GD_COMPLEX64:
      LINCOM(float complex);
      break;
    case GD_COMPLEX128:
      LINCOM(double complex);
      break;
    default:
      _GD_InternalError(D);
  }

  dreturnvoid();
}

int _GD_GetRepr(DIRFILE* D, const char* field_code_in, char** field_code)
{
  int repr = GD_REPR_NONE;

  dtrace("%p, \"%s\", %p", D, field_code_in, field_code);

  const int field_code_len = strlen(field_code_in);

  *field_code = (char*)field_code_in;
  /* find the representation, if any */
  if (field_code_in[field_code_len - 2] == '.') {
    switch (field_code_in[field_code_len - 1]) {
      case 'r':
        repr = GD_REPR_REAL;
        break;
      case 'i':
        repr = GD_REPR_IMAG;
        break;
      case 'm':
        repr = GD_REPR_MOD;
        break;
      case 'a':
        repr = GD_REPR_ARG;
        break;
      default:
        _GD_SetError(D, GD_E_BAD_REPR, GD_E_REPR_UNKNOWN, NULL, 0,
            field_code_in + field_code_len - 1);
        dreturn("%i", 0);
        return 0;
    }

    /* make a copy of the field code without the representation */
    *field_code = strdup(field_code_in);
    if (*field_code == NULL) 
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    else
      (*field_code)[field_code_len - 2] = '\0';
  }

  dreturn("%i", repr);
  return repr;
}

/* Ensure that an input field has been identified (with error checking) */
int _GD_BadInput(DIRFILE* D, gd_entry_t* E, int i)
{
  char* code;

  dtrace("%p, %p, %i", D, E, i);

  if (E->e->entry[i] == NULL) {
    E->e->entry[i] = _GD_FindFieldAndRepr(D, E->in_fields[i], &code,
        &E->e->repr[i], NULL, 1);

    if (D->error) {
      dreturn("%i", 1);
      return 1;
    }

    if (code != E->in_fields[i])
      free(code);
  }

  /* scalar entries not allowed */
  if (E->e->entry[0]->field_type & GD_SCALAR_ENTRY) {
    _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0, E->e->entry[0]->field);
    dreturn("%zi", 1);
    return 1;
  }

  dreturn("%i", 0);
  return 0;
}

/* Find the entry and the representation */
gd_entry_t* _GD_FindFieldAndRepr(DIRFILE* D, const char* field_code_in,
    char** field_code, int* repr, unsigned int *index, int set)
{
  gd_entry_t* E = NULL;

  dtrace("%p, \"%s\", %p, %p, %p, %i", D, field_code_in, field_code, repr,
      index, set);

  E = _GD_FindField(D, field_code_in, D->dot_list, D->n_dot, NULL);

  if (E == NULL) {
    *repr = _GD_GetRepr(D, field_code_in, field_code);

    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }
  } else {
    *repr = GD_REPR_NONE;
    *field_code = (char*)field_code_in;
  }

  if (E == NULL || index != NULL)
    E = _GD_FindField(D, *field_code, D->entry, D->n_entries, index);

  if (E == NULL && set) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code_in);
    if (field_code_in != *field_code)
      free(*field_code);
  }

  dreturn("%p", E);
  return E;
}
/* vim: ts=2 sw=2 et tw=80
*/
