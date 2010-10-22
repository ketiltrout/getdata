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

#ifdef GETDATA_DEBUG
int gd_col_count = 0;
char gd_debug_col[GD_COL_SIZE + 1] = "";
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

    if (ret_val == NULL)
      break;

    first_char = 0;
    while (line[first_char] == ' ' || line[first_char] == '\t')
      ++first_char;
    line += first_char;
  } while (line[0] == '#' || line[0] == 0 || line[1] == 0);


  if (ret_val) {
    dreturn("\"%s\"", line);
    return 1; /* a line was read */
  }

  dreturn("%i", 0);
  return 0;  /* there were no valid lines */
}

/* This function is needed outside the legacy API to handle old format files
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
void _GD_InsertSort(DIRFILE* D, gd_entry_t* E, int u) gd_nothrow
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

  dtrace("%p, 0x%x, %zu", D, type, n);
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

/* compute LUT table path -- this is used by _GD_Change, so e may not be E->e */
int _GD_SetTablePath(DIRFILE *D, gd_entry_t *E, struct _gd_private_entry *e)
{
  dtrace("%p, %p, %p", D, E, e);

  if (E->EN(linterp,table)[0] == '/') {
    e->EN(linterp,table_path) = strdup(E->EN(linterp,table));
    if (e->EN(linterp,table_path) == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", 1);
      return 1;
    }
  } else {
    e->EN(linterp,table_path) = (char *)malloc(FILENAME_MAX);
    if (e->EN(linterp,table_path) == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", 1);
      return 1;
    }
    char temp_buffer[FILENAME_MAX];
    strcpy(temp_buffer, D->fragment[E->fragment_index].cname);
    strcpy(e->EN(linterp,table_path), dirname(temp_buffer));
    strcat(e->EN(linterp,table_path), "/");
    strcat(e->EN(linterp,table_path), E->EN(linterp,table));
  }

  dreturn("%i", 0);
  return 0;
}

/* LUT comparison function for qsort */
static int lutcmp(const void* a, const void* b)
{
  double dx = ((struct _gd_lut *)a)->x - ((struct _gd_lut *)b)->x;
  return (dx < 0) ? -1 : (dx > 0) ? 1 : 0;
}

/* _GD_ReadLinterpFile: Read in the linterp data for this field
*/
void _GD_ReadLinterpFile(DIRFILE* D, gd_entry_t *E)
{
  FILE *fp;
  struct _gd_lut *ptr;
  int i;
  int dir = -1;
  char line[GD_MAX_LINE_LENGTH];
  int linenum = 0;
  double yr, yi;
  int buf_len = 100;

  dtrace("%p, %p", D, E);

  if (E->e->EN(linterp,table_path) == NULL)
    if (_GD_SetTablePath(D, E, E->e)) {
      dreturnvoid();
      return;
    }

  fp = fopen(E->e->EN(linterp,table_path), "r" FOPEN_TEXT);
  if (fp == NULL) {
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_OPEN, NULL, 0,
        E->e->EN(linterp,table_path));
    dreturnvoid();
    return;
  }

  /* read the first line to see whether the table is complex valued */
  if (_GD_GetLine(fp, line, &linenum)) {
    char ystr[50];
    if (sscanf(line, "%lg %49s", &yr, ystr) == 2)
      E->e->EN(linterp,complex_table) = (strchr(ystr, ';') == NULL) ? 0 : 1;
  }

  E->e->EN(linterp,lut) = (struct _gd_lut *)malloc(buf_len *
      sizeof(struct _gd_lut));

  if (E->e->EN(linterp,lut) == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    fclose(fp);
    dreturnvoid();
    return;
  }

  /* now read in the data -- we've already read line one */
  i = 0;
  linenum = 0;
  do {
    if (E->e->EN(linterp,complex_table)) {
      sscanf(line, "%lg %lg;%lg", &(E->e->EN(linterp,lut)[i].x), &yr, &yi);
      _gd_l2c(E->e->EN(linterp,lut)[i].y.c, yr, yi);
    } else
      sscanf(line, "%lg %lg", &(E->e->EN(linterp,lut)[i].x),
          &(E->e->EN(linterp,lut)[i].y.r));

    if (dir > -2 && i > 0 && E->e->EN(linterp,lut)[i].x !=
        E->e->EN(linterp,lut)[i - 1].x)
    {
      if (dir == -1)
        dir = (E->e->EN(linterp,lut)[i].x > E->e->EN(linterp,lut)[i - 1].x);
      else if (dir != (E->e->EN(linterp,lut)[i].x > E->e->EN(linterp,lut)[i
            - 1].x))
      {
        dir = -2;
      }
    }

    i++;
    if (i >= buf_len) {
      buf_len += 100;
      ptr = (struct _gd_lut *)realloc(E->e->EN(linterp,lut), buf_len *
          sizeof(struct _gd_lut));

      if (ptr == NULL) {
        free(E->e->EN(linterp,lut));
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        fclose(fp);
        dreturnvoid();
        return;
      }

      E->e->EN(linterp,lut) = ptr;
    }
  } while (_GD_GetLine(fp, line, &linenum));

  if (i < 2) {
    free(E->e->EN(linterp,lut));
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, NULL, 0,
        E->e->EN(linterp,table_path));
    fclose(fp);
    dreturnvoid();
    return;
  }

  /* Free unused memory */
  ptr = (struct _gd_lut *)realloc(E->e->EN(linterp,lut), i
      * sizeof(struct _gd_lut));

  if (ptr == NULL) {
    free(E->e->EN(linterp,lut));
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    fclose(fp);
    dreturnvoid();
    return;
  }

  E->e->EN(linterp,table_monotonic) = -1;
  E->e->EN(linterp,lut) = ptr;
  E->e->EN(linterp,table_len) = i;

  /* sort the LUT */
  if (dir == -2)
    qsort(E->e->EN(linterp,lut), i, sizeof(struct _gd_lut), lutcmp);

  fclose(fp);
  dreturnvoid();
}

/* _GD_GetIndex: get LUT index.
*/
static size_t _GD_GetIndex(double x, const struct _gd_lut *lut, size_t idx,
    size_t n)
{
  dtrace("%g, %p, %zu, %zu", x, lut, idx, n);

  /* Just linearly search - we're probably right to start    */
  /* increment until we are bigger */
  while ((idx < n - 2) && (x > lut[idx].x))
    idx++;

  /* decrement until we are smaller */
  while ((idx > 0) && (x < lut[idx].x))
    idx--;

  dreturn("%zu", idx);
  return idx;
}

#ifdef GD_NO_C99_API
#define CLINTERP(t) \
  do { \
    for (i = 0; i < npts; i++) { \
      x = data_in[i]; \
      idx = _GD_GetIndex(x, lut, idx, n_ln); \
      ((t *)data)[i] = (t)(lut[idx].y.c[0] + \
        (lut[idx + 1].y.c[0] - lut[idx].y.c[0]) / \
        (lut[idx + 1].x - lut[idx].x) * (x - lut[idx].x)); \
    } \
  } while (0)

#define LINTERPC(t) \
  if (complex_table) CLINTERPC(t); else RLINTERPC(t)
#else
#define CLINTERP(t) \
  do { \
    for (i = 0; i < npts; i++) { \
      x = data_in[i]; \
      idx = _GD_GetIndex(x, lut, idx, n_ln); \
      ((t *)data)[i] = (t)(lut[idx].y.c + (lut[idx + 1].y.c - lut[idx].y.c) / \
        (lut[idx + 1].x - lut[idx].x) * (x - lut[idx].x)); \
    } \
  } while (0)

#define LINTERPC(t) LINTERP(complex t)
#endif

#define RLINTERP(t) \
  do { \
    for (i = 0; i < npts; i++) { \
      x = data_in[i]; \
      idx = _GD_GetIndex(x, lut, idx, n_ln); \
      ((t *)data)[i] = (t)(lut[idx].y.r + (lut[idx + 1].y.r - lut[idx].y.r) / \
        (lut[idx + 1].x - lut[idx].x) * (x - lut[idx].x)); \
    } \
  } while (0)

#define CLINTERPC(t) \
  do { \
    for (i = 0; i < npts; i++) { \
      x = data_in[i]; \
      idx = _GD_GetIndex(x, lut, idx, n_ln); \
      double tx = lut[idx + 1].x - lut[idx].x; \
      double dx = x - lut[idx].x; \
      ((t *)data)[2 * i] = (t)(lut[idx].y.c[0] + \
        (lut[idx + 1].y.c[0] - lut[idx].y.c[0]) / tx * dx); \
      ((t *)data)[2 * i + 1] = (t)(lut[idx].y.c[1] + \
        (lut[idx + 1].y.c[1] - lut[idx].y.c[1]) / tx * dx); \
    } \
  } while (0)

#define RLINTERPC(t) \
  do { \
    for (i = 0; i < npts; i++) { \
      x = data_in[i]; \
      idx = _GD_GetIndex(x, lut, idx, n_ln); \
      ((t *)data)[2 * i] = (t)(lut[idx].y.r + \
        (lut[idx + 1].y.r - lut[idx].y.r) / \
        (lut[idx + 1].x - lut[idx].x) * (x - lut[idx].x)); \
      ((t *)data)[2 * i + 1] = 0; \
    } \
  } while (0)

#define LINTERP(t) \
  if (complex_table) CLINTERP(t); else RLINTERP(t)

/* _GD_LinterpData: calibrate data using lookup table lut
*/
void _GD_LinterpData(DIRFILE* D, void *data, gd_type_t type, int complex_table,
    const double *data_in, size_t npts, const struct _gd_lut *lut, size_t n_ln)
{
  int idx = 0;
  size_t i;
  double x;

  dtrace("%p, %p, 0x%x, %i, %p, %zu, %p, %zu", D, data, type, complex_table,
      data_in, npts, lut, n_ln);

  switch (type) {
    case GD_NULL:                          break;
    case GD_INT8:       LINTERP(int8_t  ); break;
    case GD_UINT8:      LINTERP(uint8_t ); break;
    case GD_INT16:      LINTERP(int16_t ); break;
    case GD_UINT16:     LINTERP(uint16_t); break;
    case GD_INT32:      LINTERP(int32_t ); break;
    case GD_UINT32:     LINTERP(uint32_t); break;
    case GD_INT64:      LINTERP(int64_t ); break;
    case GD_UINT64:     LINTERP(uint64_t); break;
    case GD_FLOAT32:    LINTERP(float   ); break;
    case GD_FLOAT64:    LINTERP(double  ); break;
    case GD_COMPLEX64:  LINTERPC(float  ); break;
    case GD_COMPLEX128: LINTERPC(double ); break;
    default:            _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
  }

  dreturnvoid();
}

/* macros to reduce tangly code */
#define LINCOM1(t) for (i = 0; i < n_read; i++) \
                            ((t *)data1)[i] = (t)(((t *)data1)[i] * m[0] + b[0])

#define LINCOM2(t) for (i = 0; i < n_read; i++) \
                            ((t *)data1)[i] = (t)(((t *)data1)[i] * m[0] + \
                              (data2[i * spf[1] / spf[0]] * m[1] + b[0] + b[1]))

#define LINCOM3(t) for (i = 0; i < n_read; i++) \
                            ((t *)data1)[i] = (t)(((t *)data1)[i] * m[0] + \
                              (data2[i * spf[1] / spf[0]] * m[1] + \
                               data3[i * spf[2] / spf[0]] * m[2] + \
                               b[0] + b[1] + b[2]))

#ifdef GD_NO_C99_API
#define LINCOMC1(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      ((t *)data1)[2 * i] = (t)(((t *)data1)[i] * m[0] + b[0]); \
      ((t *)data1)[2 * i + 1] = 0; \
    } \
  } while (0)

#define LINCOMC2(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      ((t *)data1)[2 * i] = (t)(((t *)data1)[i] * m[0] + \
        (data2[i * spf[1] / spf[0]] * m[1] + b[0] + b[1])); \
      ((t *)data1)[2 * i + 1] = 0; \
    } \
  } while (0)


#define LINCOMC3(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      ((t *)data1)[2 * i] = (t)(((t *)data1)[i] * m[0] + \
        (data2[i * spf[1] / spf[0]] * m[1] + \
         data3[i * spf[2] / spf[0]] * m[2] + \
         b[0] + b[1] + b[2])); \
      ((t *)data1)[2 * i + 1] = 0; \
    } \
  } while (0)

#define LINCOMC(t) \
  switch (n) { \
    case 1: LINCOMC1(t); break; \
    case 2: LINCOMC2(t); break; \
    case 3: LINCOMC3(t); break; \
    default: _GD_InternalError(D); \
  }
#else
#define LINCOMC(t) LINCOM(complex t)
#endif

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

  dtrace("%p, %i, %p, 0x%x, %p, %p, %p, %p, %p, %zu", D, n, data1, return_type,
      data2, data3, m, b, spf, n_read);

  switch(return_type) {
    case GD_NULL:                         break;
    case GD_UINT8:      LINCOM(uint8_t);  break;
    case GD_INT8:       LINCOM(int8_t);   break;
    case GD_UINT16:     LINCOM(uint16_t); break;
    case GD_INT16:      LINCOM(int16_t);  break;
    case GD_UINT32:     LINCOM(uint32_t); break;
    case GD_INT32:      LINCOM(int32_t);  break;
    case GD_UINT64:     LINCOM(uint64_t); break;
    case GD_INT64:      LINCOM(int64_t);  break;
    case GD_FLOAT32:    LINCOM(float);    break;
    case GD_FLOAT64:    LINCOM(double);   break;
    case GD_COMPLEX64:  LINCOMC(float);   break;
    case GD_COMPLEX128: LINCOMC(double);  break;
    default:            _GD_InternalError(D);
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
/* These must all be redefined in the absense of complex math. */
#undef LINCOM1
#undef LINCOM2
#undef LINCOM3
#undef LINCOMC1
#undef LINCOMC2
#undef LINCOMC3

#define LINCOM1(t) for (i = 0; i < n_read; i++) \
                            ((t *)data1)[i] = (t)(((t *)data1)[i] * m[0][0] + \
                              b[0][0])

#define LINCOM2(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const int i2 = 2 * (i * spf[1] / spf[0]); \
      ((t *)data1)[i] = (t)(((t *)data1)[i] * m[0][0] + \
        (data2[i2] * m[1][0] - data2[i2 + 1] * m[1][1] + b[0][0] + b[1][0])); \
    } \
  } while (0)


#define LINCOM3(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const int i2 = 2 * (i * spf[1] / spf[0]); \
      const int i3 = 2 * (i * spf[2] / spf[0]); \
      ((t *)data1)[i] = (t)(((t *)data1)[i] * m[0][0] + \
        (data2[i2] * m[1][0] - data2[i2 + 1] * m[1][1] + \
         data3[i3] * m[2][0] - data3[i3 + 1] * m[2][1] + \
         b[0][0] + b[1][0] + b[2][0])); \
    } \
  } while (0)

#define LINCOMC1(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const t x = ((t *)data1)[2 * i]; \
      const t y = ((t *)data1)[2 * i + 1]; \
      ((t *)data1)[2 * i] = (t)(x * m[0][0] - y * m[0][1] + b[0][0]); \
      ((t *)data1)[2 * i + 1] = (t)(x * m[0][1] + y * m[0][0] + b[0][1]); \
    } \
  } while (0)

#define LINCOMC2(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const int i2 = 2 * (i * spf[1] / spf[0]); \
      const t x = ((t *)data1)[2 * i]; \
      const t y = ((t *)data1)[2 * i + 1]; \
      ((t *)data1)[2 * i] = (t)(x * m[0][0] - y * m[0][1] + \
        data2[i2] * m[1][0] - data2[i2 + 1] * m[1][1] + b[0][0] + b[1][0]); \
      ((t *)data1)[2 * i + 1] = (t)(x * m[0][1] + y * m[0][0] + \
        data2[i2] * m[1][1] + data2[i2 + 1] * m[1][0] + b[0][1] + b[1][1]); \
    } \
  } while (0)


#define LINCOMC3(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const int i2 = 2 * (i * spf[1] / spf[0]); \
      const int i3 = 2 * (i * spf[2] / spf[0]); \
      const t x = ((t *)data1)[2 * i]; \
      const t y = ((t *)data1)[2 * i + 1]; \
      ((t *)data1)[2 * i] = (t)(x * m[0][0] - y * m[0][1] + \
        data2[i2] * m[1][0] - data2[i2 + 1] * m[1][1] + \
        data3[i3] * m[2][0] - data3[i3 + 1] * m[2][1] + \
        b[0][0] + b[1][0] + b[2][0]); \
      ((t *)data1)[2 * i + 1] = (t)(x * m[0][1] + y * m[0][0] + \
        data2[i2] * m[1][1] + data2[i2 + 1] * m[1][0] + \
        data3[i3] * m[2][1] + data3[i3 + 1] * m[2][0] + \
        b[0][1] + b[1][1] + b[2][1]); \
    } \
  } while (0)
#endif

/* Compute a complex valued lincom, all at once */
void _GD_CLincomData(DIRFILE* D, int n, void* data1, gd_type_t return_type,
    GD_DCOMPLEXP(data2), GD_DCOMPLEXP(data3), GD_DCOMPLEXV(m), GD_DCOMPLEXV(b),
    gd_spf_t *spf, size_t n_read)
{
  size_t i;

  dtrace("%p, %i, %p, 0x%x, %p, %p, %p, %p, %p, %zu", D, n, data1, return_type,
      data2, data3, m, b, spf, n_read);

  switch(return_type) {
    case GD_NULL:                         break;
    case GD_UINT8:      LINCOM(uint8_t);  break;
    case GD_INT8:       LINCOM(int8_t);   break;
    case GD_UINT16:     LINCOM(uint16_t); break;
    case GD_INT16:      LINCOM(int16_t);  break;
    case GD_UINT32:     LINCOM(uint32_t); break;
    case GD_INT32:      LINCOM(int32_t);  break;
    case GD_UINT64:     LINCOM(uint64_t); break;
    case GD_INT64:      LINCOM(int64_t);  break;
    case GD_FLOAT32:    LINCOM(float);    break;
    case GD_FLOAT64:    LINCOM(double);   break;
    case GD_COMPLEX64:  LINCOMC(float);   break;
    case GD_COMPLEX128: LINCOMC(double);  break;
    default:            _GD_InternalError(D);
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
#define INVERTC(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const t d = ((t *)data)[2 * i] * ((t *)data)[2 * i] + \
        ((t *)data)[2 * i + 1] * ((t *)data)[2 * i + 1]; \
      ((t *)data)[2 * i] = (t)(dividend * ((t *)data)[2 * i] / d); \
      ((t *)data)[2 * i + 1] = (t)(-dividend * ((t *)data)[2 * i + 1] / d); \
    } \
  } while (0)
#else
#define INVERTC(t) INVERT(complex t)
#endif

#define INVERT(t) for (i = 0; i < n_read; i++) \
                           ((t *)data)[i] = (t)(dividend / ((t *)data)[i])

/* Invert a vector */
void _GD_InvertData(DIRFILE* D, void* data, gd_type_t return_type,
    double dividend, size_t n_read)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %g, %zu", D, data, return_type, dividend, n_read);

  switch(return_type) {
    case GD_NULL:                         break;
    case GD_UINT8:      INVERT(uint8_t);  break;
    case GD_INT8:       INVERT(int8_t);   break;
    case GD_UINT16:     INVERT(uint16_t); break;
    case GD_INT16:      INVERT(int16_t);  break;
    case GD_UINT32:     INVERT(uint32_t); break;
    case GD_INT32:      INVERT(int32_t);  break;
    case GD_UINT64:     INVERT(uint64_t); break;
    case GD_INT64:      INVERT(int64_t);  break;
    case GD_FLOAT32:    INVERT(float);    break;
    case GD_FLOAT64:    INVERT(double);   break;
    case GD_COMPLEX64:  INVERTC(float);   break;
    case GD_COMPLEX128: INVERTC(double);  break;
    default:            _GD_InternalError(D);
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
#undef INVERTC
#undef INVERT

#define INVERTC(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const t d = ((t *)data)[2 * i] * ((t *)data)[2 * i] + \
        ((t *)data)[2 * i + 1] * ((t *)data)[2 * i + 1]; \
      ((t *)data)[2 * i] = (t)((dividend[0] * ((t *)data)[2 * i] - \
          dividend[1] * ((t *)data)[2 * i + 1]) / d); \
      ((t *)data)[2 * i + 1] = (t)((dividend[1] * ((t *)data)[2 * i] - \
          dividend[0] * ((t *)data)[2 * i]) / d); \
    } \
  } while (0)

#define INVERT(t) \
  do { \
    for (i = 0; i < n_read; i++) { \
      const t d = ((t *)data)[2 * i] * ((t *)data)[2 * i] + \
        ((t *)data)[2 * i + 1] * ((t *)data)[2 * i + 1]; \
      ((t *)data)[2 * i] = (t)((dividend[0] * ((t *)data)[2 * i] - \
          dividend[1] * ((t *)data)[2 * i + 1]) / d); \
    } \
  } while (0)

#endif

/* Invert a vector */
void _GD_CInvertData(DIRFILE* D, void* data, gd_type_t return_type,
    GD_DCOMPLEXA(dividend), size_t n_read)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %g;%g, %zu", D, data, return_type, creal(dividend),
      cimag(dividend), n_read);

  switch(return_type) {
    case GD_NULL:                         break;
    case GD_UINT8:      INVERT(uint8_t);  break;
    case GD_INT8:       INVERT(int8_t);   break;
    case GD_UINT16:     INVERT(uint16_t); break;
    case GD_INT16:      INVERT(int16_t);  break;
    case GD_UINT32:     INVERT(uint32_t); break;
    case GD_INT32:      INVERT(int32_t);  break;
    case GD_UINT64:     INVERT(uint64_t); break;
    case GD_INT64:      INVERT(int64_t);  break;
    case GD_FLOAT32:    INVERT(float);    break;
    case GD_FLOAT64:    INVERT(double);   break;
    case GD_COMPLEX64:  INVERTC(float);   break;
    case GD_COMPLEX128: INVERTC(double);  break;
    default:            _GD_InternalError(D);
  }

  dreturnvoid();
}

int _GD_GetRepr(DIRFILE* D, const char* field_code_in, char** field_code)
{
  int repr = GD_REPR_NONE;

  dtrace("%p, \"%s\", %p", D, field_code_in, field_code);

  const int field_code_len = strlen(field_code_in);

  *field_code = (char *)field_code_in;
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
  if (E->e->entry[i]->field_type & GD_SCALAR_ENTRY) {
    _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_FORMAT, E->field, 0,
        E->e->entry[i]->field);
    dreturn("%i", 1);
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
    *field_code = (char *)field_code_in;
  }

  if (E == NULL || index != NULL)
    E = _GD_FindField(D, *field_code, D->entry, D->n_entries, index);

  if (E == NULL && set) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code_in);
    if (field_code_in != *field_code)
      free(*field_code);
  }

  dreturn("%p %i", E, *repr);
  return E;
}
/* vim: ts=2 sw=2 et tw=80
*/
