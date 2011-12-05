/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2011 D. V. Wiebe
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

/* This is defined on BSD */
#ifndef MAXSYMLINKS
#define MAXSYMLINKS 20
#endif

#ifdef GETDATA_DEBUG
int gd_col_count = 0;
char gd_debug_col[GD_COL_SIZE + 1] = "";
#endif

/* _GD_GetLine: read non-comment line from format file.  The line is newly
 *      malloc'd.  Returns a pointer if successful, NULL if unsuccessful.
 *      The length read is provided in *n.  Increments *linenum as appropriate;
 */
char *_GD_GetLine(FILE *fp, size_t *n, int* linenum)
{
  ssize_t len;

  char *line = NULL;

  dtrace("%p, %p, %p", fp, n, linenum);

  do {
    errno = 0;
    len = getdelim(&line, n, '\n', fp);
    if (len == -1)
      break;

    (*linenum)++;
  } while (line[0] == '#' || line[0] == 0 || line[0] == '\n');


  if (len != -1) {
    dreturn("\"%s\" (%zu)", line, *n);
    return line; /* a line was read */
  }

  free(line);
  dreturn("%p", NULL);
  return NULL;  /* there were no valid lines */
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
  char *temp_buffer;

  dtrace("%p, %p, %p", D, E, e);

  e->u.linterp.table_dirfd = _GD_GrabDir(D,
      D->fragment[E->fragment_index].dirfd, E->EN(linterp,table));

  temp_buffer = strdup(E->EN(linterp,table));
  if (temp_buffer == NULL) {
    _GD_ReleaseDir(D, e->u.linterp.table_dirfd);
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", 1);
    return 1;
  }
  e->u.linterp.table_file = strdup(basename(temp_buffer));
  if (e->u.linterp.table_file == NULL) {
    _GD_ReleaseDir(D, e->u.linterp.table_dirfd);
    free(temp_buffer);
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", 1);
    return 1;
  }
  free(temp_buffer);

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
  int i, fd;
  int dir = -1;
  char *line;
  size_t n = 0;
  int linenum = 0;
  double yr, yi;
  int buf_len = 100;

  dtrace("%p, %p", D, E);

  if (E->e->u.linterp.table_file == NULL)
    if (_GD_SetTablePath(D, E, E->e)) {
      dreturnvoid();
      return;
    }

  fd = gd_OpenAt(D, E->e->u.linterp.table_dirfd, E->e->u.linterp.table_file,
      O_RDONLY, 0666);
  if (fd == -1) {
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_OPEN, NULL, 0,
        E->EN(linterp,table));
    dreturnvoid();
    return;
  }

  fp = fdopen(fd, "r");
  if (fp == NULL) {
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_OPEN, NULL, 0,
        E->EN(linterp,table));
    dreturnvoid();
    return;
  }

  /* read the first line to see whether the table is complex valued */
  if ((line = _GD_GetLine(fp, &n, &linenum))) {
    char ystr[50];
    if (sscanf(line, "%lg %49s", &yr, ystr) == 2)
      E->e->u.linterp.complex_table = (strchr(ystr, ';') == NULL) ? 0 : 1;
  } else {
    if (errno == EOVERFLOW)
      /* line too long */
      _GD_SetError(D, GD_E_LINE_TOO_LONG, 0, E->EN(linterp,table), linenum,
          NULL);
    else 
      /* no data in file! */
      _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, NULL, 0,
          E->EN(linterp,table));
    fclose(fp);
    dreturnvoid();
    return;
  }

  E->e->u.linterp.lut = (struct _gd_lut *)malloc(buf_len *
      sizeof(struct _gd_lut));

  if (E->e->u.linterp.lut == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    fclose(fp);
    dreturnvoid();
    return;
  }

  /* now read in the data -- we've already read line one */
  i = 0;
  do {
    if (E->e->u.linterp.complex_table) {
      sscanf(line, "%lg %lg;%lg", &(E->e->u.linterp.lut[i].x), &yr, &yi);
      _gd_l2c(E->e->u.linterp.lut[i].y.c, yr, yi);
    } else
      sscanf(line, "%lg %lg", &(E->e->u.linterp.lut[i].x),
          &(E->e->u.linterp.lut[i].y.r));

    if (dir > -2 && i > 0 && E->e->u.linterp.lut[i].x !=
        E->e->u.linterp.lut[i - 1].x)
    {
      if (dir == -1)
        dir = (E->e->u.linterp.lut[i].x > E->e->u.linterp.lut[i - 1].x);
      else if (dir != (E->e->u.linterp.lut[i].x > E->e->u.linterp.lut[i - 1].x))
        dir = -2;
    }

    i++;
    if (i >= buf_len) {
      buf_len += 100;
      ptr = (struct _gd_lut *)realloc(E->e->u.linterp.lut, buf_len *
          sizeof(struct _gd_lut));

      if (ptr == NULL) {
        free(E->e->u.linterp.lut);
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        fclose(fp);
        dreturnvoid();
        return;
      }

      E->e->u.linterp.lut = ptr;
    }
    free(line);
  } while ((line = _GD_GetLine(fp, &n, &linenum)));

  if (i < 2) {
    free(E->e->u.linterp.lut);
    _GD_SetError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, NULL, 0,
        E->EN(linterp,table));
    fclose(fp);
    dreturnvoid();
    return;
  }

  /* Free unused memory */
  ptr = (struct _gd_lut *)realloc(E->e->u.linterp.lut, i
      * sizeof(struct _gd_lut));

  if (ptr == NULL) {
    free(E->e->u.linterp.lut);
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    fclose(fp);
    dreturnvoid();
    return;
  }

  E->e->u.linterp.table_monotonic = -1;
  E->e->u.linterp.lut = ptr;
  E->e->u.linterp.table_len = i;

  /* sort the LUT */
  if (dir == -2)
    qsort(E->e->u.linterp.lut, i, sizeof(struct _gd_lut), lutcmp);

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
      double tx, dx; \
      x = data_in[i]; \
      idx = _GD_GetIndex(x, lut, idx, n_ln); \
      tx = lut[idx + 1].x - lut[idx].x; \
      dx = x - lut[idx].x; \
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
  const int field_code_len = strlen(field_code_in);

  dtrace("%p, \"%s\", %p", D, field_code_in, field_code);

  *field_code = (char *)field_code_in;
  /* find the representation, if any */
  if (field_code_len > 2 && field_code_in[field_code_len - 2] == '.') {
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

const char *_GD_DirName(const DIRFILE *D, int dirfd)
{
#ifndef GD_NO_DIR_OPEN
  unsigned int i;
#endif

  dtrace("%p, %i", D, dirfd);

#ifdef GD_NO_DIR_OPEN
  /* in the non-POSIX case, dirfd is just the index in the dir list */
  if (D->ndir > 0) {
    dreturn("\"%s\"", D->dir[dirfd].path);
    return D->dir[dirfd].path;
  }
#else
  for (i = 0; i < D->ndir; ++i)
    if (dirfd == D->dir[i].fd) {
      dreturn("\"%s\"", D->dir[i].path);
      return D->dir[i].path;
    }
#endif

  /* we only get here in the early stages of opening a dirfile */
  dreturn("%p", D->name);
  return D->name;
}

/* This is effectively the POSIX.1 realpath(3) function, but with some
 * optimisation due to the fact that we know that the front part of the
 * path (car) has already been canonicalised */
char *_GD_CanonicalPath(const char *car, const char *cdr)
{
  int last_element = 0, loop_count = 0;
  size_t res_len, res_size, len;
  char *res = NULL, *ptr, *work, *cur, *end;
  dtrace("\"%s\", \"%s\"", car, cdr);

  if (car && !_GD_AbsPath(cdr)) {
    if (car[0] != '/') {
      /* car is not abosulte -- don't bother trying to do anything fancy */
      res = malloc(strlen(car) + strlen(cdr) + 2);
      if (res == NULL) {
        dreturn("%p", NULL);
        return NULL;
      }
      sprintf(res, "%s/%s", car, cdr);
      dreturn("%s", res);
      return res;
    }
    /* car is nonnull and cdr is not absolute: copy car into res and
     * cdr into work */
    res = strdup(car);
    if (res == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }
    res_size = (res_len = strlen(car)) + 1;

    cur = work = strdup(cdr);
    if (work == NULL) {
      free(res);
      dreturn("%p", NULL);
      return NULL;
    }
  } else if (cdr[0] == '/') {
    /* cdr is absolute: make res "/" and copy cdr relative to / into work;
     * ignore car */
    res_len = 1;
    res = (char*)malloc(res_size = PATH_MAX);
    if (res == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }
    res[0] = '/';
    res[1] = '\0';

    if (cdr[0] == '/' && cdr[1] == '\0') {
      dreturn("\"%s\"", res);
      return res;
    }

    cur = work = strdup(cdr + 1);
    if (work == NULL) {
      free(res);
      dreturn("%p", NULL);
      return NULL;
    }
  } else {
    /* car is not present and cdr is relative: try to get the CWD; and make
     * work CWD/cdr, relative to / if getcwd returned an absolute path.  If
     * getcwd fails, just use cdr, and abandon hope of an absolute path.
     */
    res = (char*)malloc(res_size = PATH_MAX);
    work = (char*)malloc(PATH_MAX);
    if (res == NULL || work == NULL) {
      free(res);
      free(res);
      dreturn("%p", NULL);
      return NULL;
    }

    if (gd_getcwd(work, PATH_MAX) == NULL) {
      /* if getcwd fails, we're stuck with a relative path, oh well. */
      free(work);
      cur = work = strdup(cdr);
      if (work == NULL) {
        free(res);
        dreturn("%p", NULL);
        return NULL;
      }
      res[0] = '\0';
      res_len = 0;
    } else {
      if ((len = strlen(work) + 2 + strlen(cdr)) < PATH_MAX) {
        ptr = (char*)realloc(work, len);
        if (ptr == NULL) {
          free(res);
          free(work);
          dreturn("%p", NULL);
          return NULL;
        }
        work = ptr;
      }
      ptr = work + strlen(work);
      *(ptr++) = '/';
      strcpy(ptr, cdr);

      if (work[0] == '/') {
        res[0] = '/';
        res[1] = '\0';
        res_len = 1;
        cur = work + 1;
      } else {
        res[0] = '\0';
        res_len = 0;
        cur = work;
      }
    }
  }

  /* now step through work, building up res as appropriate */
  for (end = cur ; !last_element; cur = end) {
    /* look for the next '/' or NUL */
    for (; *end != '\0' && *end != '/'; ++end)
      ;

    /* end of string */
    if (*end == '\0' && cur == end)
      break;
    else if (*end == '\0')
      last_element = 1;
    else
      *(end++) = '\0';

    if (cur[0] == '\0')
      /* consecutive /s */
      ;
    else if (cur[0] == '.' && cur[1] == '\0') {
      /* discard . */
      continue;
    } else if (cur[0] == '.' && cur[1] == '.' && cur[2] == '\0') {
      /* don't strip the leading '/' */
      if (res_len > 1) {
        /* find the last '/', but don't strip the leading '/' */
        for(ptr = res + res_len - 1; *ptr != '/' && ptr > res + 1; --ptr)
          ;

        /* strip the .. if possible, otherwise append it */
        *ptr = '\0';
        res_len = ptr - res;
      }
    } else {
      /* a path element, copy it to res */
      len = strlen(cur) + 1;
      if (res_len + len >= res_size) {
        ptr = (char*)realloc(res, res_size += (len < 1000) ? 1000 : len);
        if (ptr == NULL) {
          free(res);
          free(work);
          dreturn("%p", NULL);
          return NULL;
        }
        res = ptr;
      }
      if (res_len > 1 && res[res_len - 1] != '/')
        res[res_len++] = '/';
      strcpy(res + res_len, cur);
      res_len += len - 1;
#if defined HAVE_READLINK && defined HAVE_LSTAT64
      {
        gd_stat64_t statbuf;

        /* check if it's a symlink */
        if (lstat64(res, &statbuf)) {
          if (errno == ENOENT && *end == '\0') {
            /* the leaf doesn't exist.  I guess that means we're done. */
            goto _GD_CanonicalPath_DONE;
          }
          /* doesn't exist */
          free(res);
          free(work);
          dreturn("%p", NULL);
          return NULL;
        }

        if (S_ISLNK(statbuf.st_mode)) {
          char target[PATH_MAX], *new;
          ssize_t slen;

          /* check for symlink loop */
          if (loop_count++ > MAXSYMLINKS) {
            errno = ELOOP;
            free(res);
            free(work);
            dreturn("%p", NULL);
            return NULL;
          }

          /* get the link target */
          slen = readlink(res, target, PATH_MAX);
          if (slen == -1) {
            free(res);
            free(work);
            dreturn("%p", NULL);
            return NULL;
          }

          /* now we have to start all over again */
          ptr = target;
          if (target[0] == '/') {
            res[1] = '\0';
            res_len = 1;
            ptr++;
            slen--;
          } else if (res_len > 1) {
            /* strip the symlink name from res */
            for (ptr = res + res_len - 1; *ptr != '/'; --ptr)
              ;
            *(ptr + 1) = '\0';
            res_len = res - ptr + 1;
          }

          /* now make a new work buffer of "target/remaining", asusming
           * remaining is non-null, otherwise, just use target */
          if (*end == '\0') {
            free(work);
            end = work = strdup(target);
            if (work == NULL) {
              free(res);
              dreturn("%p", NULL);
              return NULL;
            }
          } else {
            char slash[2] = "/";
            len = strlen(end) + slen + 2;

            if (*(ptr + slen - 1) == '/') {
              slash[0] = '\0';
              len--;
            }

            new = (char*)malloc(len);
            if (new == NULL) {
              free(res);
              free(work);
              dreturn("%p", NULL);
              return NULL;
            }
            sprintf(new, "%s%s%s", ptr, slash, end);
            free(work);
            end = work = new;
          }
        }
      }
#endif
    }
  }

_GD_CanonicalPath_DONE:
  free(work);

  /* trim */
  ptr = realloc(res, res_len + 1);
  if (ptr)
    res = ptr;

  dreturn("\"%s\"", res);
  return res;
}

char *_GD_MakeFullPath(const DIRFILE *D, int dirfd, const char *name)
{
  const char *dir;
  char *filepath;
  dtrace("%p, %i, \"%s\"", D, dirfd, name);

  if (dirfd >= 0) {
    dir = _GD_DirName(D, dirfd);
    if (dir == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }
  } else
    dir = NULL;

  filepath = _GD_CanonicalPath(dir, name);

  dreturn("\"%s\"", filepath);
  return filepath;
}

int _GD_GrabDir(DIRFILE *D, int dirfd, const char *name)
{
  unsigned int i;
  char *path, *dir = NULL;
  void *ptr;
  int abs = _GD_AbsPath(name);

  dtrace("%p, %i, \"%s\"", D, dirfd, name);

  path = _GD_MakeFullPath(D, dirfd, name);

  if (path == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  dir = dirname(path);

  for (i = 0; i < D->ndir; ++i)
    if (strcmp(dir, D->dir[i].path) == 0) {
      D->dir[i].rc++;
      free(path);
      dreturn("%i", D->dir[i].fd);
      return D->dir[i].fd;
    }

  /* new one */
  ptr = realloc(D->dir, sizeof(struct gd_dir_t) * (D->ndir + 1));

  if (ptr == NULL) {
    free(path);
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  D->dir = ptr;
  D->dir[D->ndir].rc = 1;
  D->dir[D->ndir].path = strdup(dir);

  if (D->dir[D->ndir].path == NULL) {
    free(path);
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

#ifdef GD_NO_DIR_OPEN
  D->dir[D->ndir].fd = D->ndir;
  free(path);
#else
  if (abs) {
    D->dir[D->ndir].fd = open(dir, O_RDONLY);
  } else {
    free(path);
    path = strdup(name);
    if (path == NULL) {
      free(D->dir[D->ndir].path);
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }
    D->dir[D->ndir].fd = gd_OpenAt(D, dirfd, dirname(path), O_RDONLY, 0666);
  }
  free(path);

  if (D->dir[D->ndir].fd == -1) {
    free(D->dir[D->ndir].path);
    dreturn("%i", -1);
    return -1;
  }
#endif
  D->ndir++;

  dreturn("%i", D->dir[D->ndir - 1].fd);
  return D->dir[D->ndir - 1].fd;
}

void _GD_ReleaseDir(DIRFILE *D, int dirfd)
{
#ifndef GD_NO_DIR_OPEN
  unsigned int i;
#endif

  dtrace("%p, %i", D, dirfd);

#ifdef GD_NO_DIR_OPEN
  if (--D->dir[dirfd].rc == 0) {
    free(D->dir[dirfd].path);
    D->dir[dirfd] = D->dir[--D->ndir];
  }
#else
  for (i = 0; i < D->ndir; ++i)
    if (D->dir[i].fd == dirfd) {
      if (--D->dir[i].rc == 0) {
        free(D->dir[i].path);
        close(D->dir[i].fd);
        D->dir[i] = D->dir[--D->ndir];
      }
      break;
    }
#endif

  dreturnvoid();
}

/* vim: ts=2 sw=2 et tw=80
*/
