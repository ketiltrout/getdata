/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "internal.h"

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 3) /* for lincom */

static gd_type_t _GD_RawType(const char* type)
{
  /* for backwards compatibility */
  if (strlen(type) == 1) 
    return _GD_LegacyType(type[0]);

  if (strcmp(type, "NULL") == 0)
    return GD_NULL;
  if (strcmp(type, "INT8") == 0)
    return GD_INT8;
  if (strcmp(type, "UINT8") == 0)
    return GD_UINT8;
  if (strcmp(type, "INT16") == 0)
    return GD_INT16;
  if (strcmp(type, "INT32") == 0)
    return GD_INT32;
  if (strcmp(type, "UINT32") == 0)
    return GD_UINT32;
  if (strcmp(type, "UINT64") == 0)
    return GD_UINT64;
  if (strcmp(type, "INT64") == 0)
    return GD_INT64;
  if (strcmp(type, "UINT16") == 0)
    return GD_UINT16;
  if (strcmp(type, "FLOAT32") == 0)
    return GD_FLOAT32;
  if (strcmp(type, "FLOAT") == 0)
    return GD_FLOAT32;
  if (strcmp(type, "FLOAT64") == 0)
    return GD_FLOAT64;
  if (strcmp(type, "DOUBLE") == 0)
    return GD_FLOAT64;

  return GD_UNKNOWN;
}

/* Check for a valid field name -- returns input on error */
char* _GD_ValidateField(const char* field_code)
{
  size_t len = strlen(field_code);
  size_t i;
  char* ptr;

  dtrace("\"%s\"", field_code);

  for (i = 0; i < len; ++i)
    if (strchr("/\\<>;|&", field_code[i]) != NULL) {
      dreturn("\"%s\"", field_code);
      return (char*)field_code;
    }

  ptr = strdup(field_code);
  dreturn("%p", ptr);
  return ptr;
}

/* _GD_ParseRaw: parse a RAW data type in the format file
*/
static gd_entry_t* _GD_ParseRaw(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* subdir, const char* format_file, int line)
{
  dtrace("%p, %p, %i, \"%s\", \"%s\", %i", D, in_cols, n_cols, subdir,
      format_file, line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* R = malloc(sizeof(gd_entry_t));
  if (R == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  R->field_type = GD_RAW_ENTRY;
  R->file = NULL;
  R->fp = -1; /* file not opened yet */
  R->format_file = D->n_include - 1;

  R->field = _GD_ValidateField(in_cols[0]);
  if (R->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    free(R);
    dreturn("%p", R);
    return R;
  }

  R->file = malloc(FILENAME_MAX);
  if (R->file == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(R);
    free(R);
    dreturn("%p", NULL);
    return NULL;
  }

  snprintf((char*)R->file, FILENAME_MAX, "%s/%s", subdir, in_cols[0]);
  R->data_type = _GD_RawType(in_cols[2]);
  R->size = GD_SIZE(R->data_type);

  if (R->size == 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
  else if ((R->spf = atoi(in_cols[3])) <= 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_SPF, format_file, line,
        in_cols[3]);

  if (D->error != GD_E_OK) {
    dirfile_free_entry_strings(R);
    free(R);
    R = NULL;
  }

  dreturn("%p", R);
  return R;
}

/* _GD_ParseLincom: parse a LINCOM data type in the format file.
*/
static gd_entry_t* _GD_ParseLincom(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  int i;

  dtrace("%p, %p, %i, \"%s\", %i", D, in_cols, n_cols, format_file, line);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* L = malloc(sizeof(gd_entry_t));
  if (L == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  L->field_type = GD_LINCOM_ENTRY;
  for (i = 0; i < GD_MAX_LINCOM; ++i)
    L->in_fields[i] = NULL;
  L->format_file = D->n_include - 1;

  L->field = _GD_ValidateField(in_cols[0]);
  if (L->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    free(L);
    dreturn("%p", NULL);
    return NULL;
  }

  L->n_fields = atoi(in_cols[2]);

  if (L->field == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  else if ((L->n_fields < 1) || (L->n_fields > GD_MAX_LINCOM))
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_FIELDS, format_file, line,
        in_cols[2]);
  else if (n_cols < L->n_fields * 3 + 3)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
  else
    for (i = 0; i < L->n_fields; i++) {
      L->in_fields[i] = strdup(in_cols[i * 3 + 3]);
      if (L->in_fields[i] == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      L->m[i] = atof(in_cols[i * 3 + 4]);
      L->b[i] = atof(in_cols[i * 3 + 5]);
    }

  if (D->error != GD_E_OK) {
    dirfile_free_entry_strings(L);
    free(L);
    L = NULL;
  }

  dreturn("%p", L);
  return L;
}

/* _GD_ParseLinterp: parse a LINTERP data type in the format file.
*/
static gd_entry_t* _GD_ParseLinterp(DIRFILE* D,
    const char* in_cols[MAX_IN_COLS], int n_cols, const char* format_file,
    int line)
{
  char temp_buffer[FILENAME_MAX];

  dtrace("%p, %p, %i, \"%s\", %i", D, in_cols, n_cols, format_file, line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* L = malloc(sizeof(gd_entry_t));
  if (L == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  L->field_type = GD_LINTERP_ENTRY;
  L->in_fields[0] = NULL;
  L->table = NULL;
  L->format_file = D->n_include - 1;

  L->field = _GD_ValidateField(in_cols[0]);
  if (L->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    free(L);
    dreturn("%p", NULL);
    return NULL;
  }

  L->in_fields[0] = strdup(in_cols[2]);
  L->table_len = -1; /* linterp file not read yet */


  if (in_cols[3][0] == '/')
    L->table = strdup(in_cols[3]);
  else {
    /* non-absolute paths are relative to the format file's directory */
    L->table = malloc(FILENAME_MAX);
    if (L->table != NULL) {
      strcpy(temp_buffer, format_file);
      strcpy(L->table, dirname(temp_buffer));
      strcat(L->table, "/");
      strcat(L->table, in_cols[3]);
    }
  }

  if (L->field == NULL || L->in_fields[0] == NULL || L->table == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(L);
    free(L);
    L = NULL;
  }

  dreturn("%p", L);
  return L;
}

/* _GD_ParseMultiply: parse MULTIPLY data type entry in format file.
*/
static gd_entry_t* _GD_ParseMultiply(DIRFILE* D,
    const char* in_cols[MAX_IN_COLS], int n_cols, const char* format_file,
    int line)
{
  dtrace("%p, %p, %i, \"%s\", %i", D, in_cols, n_cols, format_file, line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* M = malloc(sizeof(gd_entry_t));
  if (M == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  M->field_type = GD_MULTIPLY_ENTRY;
  M->in_fields[0] = M->in_fields[1] = NULL;
  M->format_file = D->n_include - 1;

  M->field = _GD_ValidateField(in_cols[0]);
  if (M->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    free(M);
    dreturn("%p", NULL);
    return NULL;
  }

  M->in_fields[0] = strdup(in_cols[2]);
  M->in_fields[1] = strdup(in_cols[3]);

  if (M->field == NULL || M->in_fields[0] == NULL || M->in_fields[1] == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(M);
    free(M);
    M = NULL;
  }

  dreturn("%p", M);
  return M;
}

/* _GD_ParseBit: parse BIT data type entry in format file.
*/
static gd_entry_t* _GD_ParseBit(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  dtrace("%p, %p, %i, \"%s\", %i", D, in_cols, n_cols, format_file, line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* B = malloc(sizeof(gd_entry_t));
  if (B == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  B->field_type = GD_BIT_ENTRY;
  B->in_fields[0] = NULL;
  B->format_file = D->n_include - 1;

  B->field = _GD_ValidateField(in_cols[0]);
  if (B->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    B->field = NULL;
    dreturn("%p", B);
    return B;
  }

  B->in_fields[0] = strdup(in_cols[2]);

  if (B->field == NULL || B->in_fields[0] == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  B->bitnum = atoi(in_cols[3]);
  if (n_cols > 4)
    B->numbits = atoi(in_cols[4]);
  else
    B->numbits = 1;

  if (B->numbits < 1)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NUMBITS, format_file, line, NULL);
  else if (B->bitnum < 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITNUM, format_file, line, NULL);
  else if (B->bitnum + B->numbits - 1 > 63)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITSIZE, format_file, line, NULL);

  if (D->error != GD_E_OK) {
    dirfile_free_entry_strings(B);
    free(B);
    B = NULL;
  }

  dreturn("%p", B);
  return B;
}

/* _GD_ParsePhase: parse PHASE data type entry in formats file.
*/
static gd_entry_t* _GD_ParsePhase(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  dtrace("%p, %p, %i, \"%s\", %i", D, in_cols, n_cols, format_file, line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* P = malloc(sizeof(gd_entry_t));
  if (P == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  P->field_type = GD_PHASE_ENTRY;
  P->in_fields[0] = NULL;
  P->format_file = D->n_include - 1;

  P->field = _GD_ValidateField(in_cols[0]);
  if (P->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    free(P);
    dreturn("%p", NULL);
    return NULL;
  }

  P->in_fields[0] = strdup(in_cols[2]); /* field */
  P->shift = atoi(in_cols[3]); /*shift*/

  if (P->field == NULL || P->in_fields[0] == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(P);
    free(P);
    P = NULL;
  }

  dreturn("%p", P);
  return P;
}

/* _GD_EntryCmp: Comparison functions for the entries
*/
int _GD_EntryCmp(const void *A, const void *B)
{
  return (strcmp((*(gd_entry_t **)A)->field,
        (*(gd_entry_t **)B)->field));
}

/* _GD_ParseFormatFile: Perform the actual parsing of the format file.  This
 *       function is called from GetFormat once for the main format file and
 *       once for each included file.
 *
 *       Returns 0 unless this format file contains the first raw field.
 */
static int _GD_ParseFormatFile(FILE* fp, DIRFILE *D, const char* filedir,
    const char* subdir, const char* format_file, int format_parent,
    int standards)
{
  char instring[MAX_LINE_LENGTH];
  const char* in_cols[MAX_IN_COLS];
  char* ptr;
  int n_cols = 0;
  int linenum = 0;
  int ws = 1;
  int me = D->n_include - 1;
  int have_first = 0;

  dtrace("%p, %p, \"%s\", \"%s\", \"%s\", %i", fp, D, filedir, subdir,
      format_file, standards);

  /* start parsing */
  while (_GD_GetLine(fp, instring, &linenum)) {
    n_cols = 0;
    /* tokenise the line */
    for (ptr = instring; *ptr != '\0'; ++ptr) {
      if (isspace(*ptr)) {
        if (!ws)
          *ptr = '\0';
        ws = 1;
      } else {
        if (ws) {
          if (n_cols >= MAX_IN_COLS)
            break; /* Ignore trailing data on the line */
          in_cols[n_cols++] = ptr;
        }
        ws = 0;
      }
    }

    /* set up for possibly slashed reserved words */
    ptr = (char*)in_cols[0] + ((in_cols[0][0] == '/') ? 1 : 0);

    if (n_cols < 2) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, linenum,
          NULL);

      /* Directives */

    } else if (strcmp(ptr, "FRAMEOFFSET") == 0) {
      D->frame_offset = atoll(in_cols[1]);
    } else if (strcmp(ptr, "INCLUDE") == 0) {
      int i, found = 0;
      char temp_buffer[FILENAME_MAX];
      char new_format_file[FILENAME_MAX];
      char new_subdir[FILENAME_MAX];
      FILE* new_fp = NULL;

      /* create the format filename */
      snprintf(new_format_file, FILENAME_MAX, "%s/%s/%s", filedir,
          subdir, in_cols[1]);

      /* Run through the include list to see if we've already included this
       * file */
      for (i = 0; i < D->n_include; ++i)
        if (strcmp(new_format_file, D->include_list[i].name) == 0) {
          found = 1;
          break;
        }

      /* If we found the file, we won't reopen it.  Continue parsing. */
      if (found)
        continue;

      /* Otherwise, try to open the file */
      new_fp = fopen(new_format_file, "r");

      /* If opening the file failed, set the error code and abort parsing. */
      if (new_fp == NULL) {
        _GD_SetError(D, GD_E_OPEN_INCLUDE, errno, format_file, linenum,
            new_format_file);
        break;
      }

      /* If we got here, we managed to open the included file; parse it */
      D->include_list = realloc(D->include_list, ++(D->n_include) *
          sizeof(struct gd_include_t));
      if (D->include_list == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        dreturn("%i", have_first);
        return have_first;
      }
      D->include_list[D->n_include - 1].name = strdup(new_format_file);
      D->include_list[D->n_include - 1].modified = 0;
      D->include_list[D->n_include - 1].parent = format_parent;
      D->include_list[D->n_include - 1].first = 0;

      if (D->include_list[D->n_include - 1].name == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        dreturn("%i", have_first);
        return have_first;
      }

      /* extract the subdirectory name - dirname both returns a volatile string
       * and modifies its argument, ergo strcpy */
      strcpy(temp_buffer, in_cols[1]);
      if (strcmp(subdir, ".") == 0)
        strcpy(new_subdir, dirname(temp_buffer));
      else
        snprintf(new_subdir, FILENAME_MAX, "%s/%s", subdir,
            dirname(temp_buffer));

      if (_GD_ParseFormatFile(new_fp, D, filedir, new_subdir, new_format_file,
            me, standards))
        D->include_list[me].first = 1;
      fclose(new_fp);
    } else if (strcmp(ptr, "ENDIAN") == 0) {
      if (!(D->flags & GD_FORCE_ENDIAN)) {
        if (strcmp(in_cols[1], "big") == 0) {
          D->flags |= GD_BIG_ENDIAN;
          D->flags &= ~GD_LITTLE_ENDIAN;
        } else if (strcmp(in_cols[1], "little") == 0) {
          D->flags |= GD_LITTLE_ENDIAN;
          D->flags &= ~GD_BIG_ENDIAN;
        } else 
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN, format_file, linenum,
              NULL);
      }
    } else if (strcmp(ptr, "VERSION") == 0) {
      standards = atoi(in_cols[1]);

      /* Field Types -- here we go back to in_cols */

    } else if ((strcmp(in_cols[0], "INDEX") == 0) ||
        (strcmp(in_cols[0], "FILEFRAM") == 0))
    { /* reserved field names */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_RES_NAME, format_file, linenum,
          NULL);
    } else if (strcmp(in_cols[1], "RAW") == 0) {
      D->n_entries++;
      D->entry = realloc(D->entry, D->n_entries * sizeof(gd_entry_t**));
      D->entry[D->n_entries - 1] = _GD_ParseRaw(D, in_cols, n_cols, subdir,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
      else if (D->first_field == NULL) {
        /* set the first field */
        D->first_field = malloc(sizeof(gd_entry_t));
        if (D->first_field == NULL)
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        else {
          memcpy(D->first_field, D->entry[D->n_entries - 1],
              sizeof(gd_entry_t));
          have_first = 1;
          D->include_list[me].first = 1;
        }
      }
    } else if (strcmp(in_cols[1], "LINCOM") == 0) {
      D->n_entries++;
      D->entry = realloc(D->entry, D->n_entries * sizeof(gd_entry_t**));
      D->entry[D->n_entries - 1] = _GD_ParseLincom(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "LINTERP") == 0) {
      D->n_entries++;
      D->entry = realloc(D->entry, D->n_entries * sizeof(gd_entry_t**));
      D->entry[D->n_entries - 1] = _GD_ParseLinterp(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "MULTIPLY") == 0) {
      D->n_entries++;
      D->entry = realloc(D->entry, D->n_entries * sizeof(gd_entry_t**));
      D->entry[D->n_entries - 1] = _GD_ParseMultiply(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "BIT") == 0) {
      D->n_entries++;
      D->entry = realloc(D->entry, D->n_entries * sizeof(gd_entry_t**));
      D->entry[D->n_entries - 1] = _GD_ParseBit(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "PHASE") == 0) {
      D->n_entries++;
      D->entry = realloc(D->entry, D->n_entries * sizeof(gd_entry_t**));
      D->entry[D->n_entries - 1] = _GD_ParsePhase(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (standards <= DIRFILE_STANDARDS_VERSION ||
        (D->flags & GD_PEDANTIC))
    {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, format_file, linenum,
          NULL);
    }

    /* break out of loop (so we can return) if we've encountered an error */
    if (D->error != GD_E_OK)
      break;
  }

  dreturn("%i", have_first);
  return have_first;
}

/* attempt to open or create a new dirfile - set error appropriately */
static FILE* _GD_CreateDirfile(DIRFILE* D, const char* format_file,
    const char* filedir, unsigned int flags)
{
  struct stat statbuf;
  char fullname[FILENAME_MAX];
  DIR* dir;
  char* dirfile_end;
  struct dirent* lamb;
  int dir_error = 0;
  int format_error = 0;
  FILE* fp = NULL;

  dtrace("%p, \"%s\", \"%s\", 0%o", D, format_file, filedir, flags);

  /* naively try to open the format file */
  if ((fp = fopen(format_file, "r")) == NULL) {
    format_error = errno;

    /* open failed, try to stat the directory itself */
    if (stat(filedir, &statbuf))
      dir_error = errno;
    else if (!S_ISDIR(statbuf.st_mode))
      dir_error = ENOTDIR;
  }

  /* First, cast out our four failure modes */

  /* unable to read the format file */
  if (format_error == EACCES || dir_error == EACCES) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NO_ACCESS, format_file, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* the directory exists, but it's not a dirfile, do nothing else -- even if we
   * were asked to truncate it */
  if (!dir_error && format_error) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_DIRFILE, format_file, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Couldn't open the file, and we weren't asked to create it */
  if (format_error && !(flags & GD_CREAT)) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_EXIST, format_file, format_error,
        NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* It does exist, but we were asked to exclusively create it */
  if (!format_error && (flags & GD_CREAT) && (flags & GD_EXCL)) {
    _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_EXCL, filedir, 0, NULL);
    fclose(fp);
    dreturn("%p", NULL);
    return NULL;
  }

  /* If we made it here we either:
   * 1) have no such directory, but plan to create it, or
   * 2) have a dirfile, which means the directory supplied contains a readable
   *   file called format */

  /* Truncate, if needed -- dangerous!  Truncating a dirfile deletes every
   * regular file in the specified directory.  It does not touch subdirectories.
   * Note that the rather lame definition of a dirfile at this point
   * (specifically, we haven't bothered to see if the format file is parsable)
   * could be problematic if users use GD_TRUNC cavalierly. */
  if (flags & GD_TRUNC && !format_error) {
    /* This file isn't going to be around much longer */
    fclose(fp);

    /* can't truncate a read-only dirfile */
    if ((flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* This code is from defile */
    if ((dir = opendir(filedir)) == NULL) {
      _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, filedir, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    strcpy(fullname, filedir);
    dirfile_end = fullname + strlen(fullname);
    if (*(dirfile_end - 1) != '/') {
      strcat(fullname, "/");
      dirfile_end++;
    }

    while ((lamb = readdir(dir)) != NULL) {
      strcpy(dirfile_end, lamb->d_name);

      if (stat(fullname, &statbuf)) {
        _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_STAT, fullname, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }

      /* only delete regular files */
      if (S_ISREG(statbuf.st_mode)) {
        if (unlink(fullname)) {
          _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_UNLINK, fullname, errno, NULL);
          dreturn("%p", NULL);
          return NULL;
        }
      }
    }
  }

  /* Create, if needed */
  if ((flags & GD_CREAT && format_error) || (flags & GD_TRUNC))
  {
    /* can't create a read-only dirfile */
    if ((flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* attempt to create the dirfile directory, if not present */
    if (dir_error) 
      if (mkdir(filedir, 00777) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_DIR, filedir, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }

    /* create a new, empty format file */
    if ((fp = fopen(format_file, "wt")) == NULL) {
      _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }
  }

  /* open succeeds */
  dreturn("%p", fp);
  return fp;
}

/* dirfile_open: open (or, perhaps, create) and parse the specified dirfile
*/
DIRFILE* dirfile_open(const char* filedir, unsigned int flags)
{
  FILE *fp;
  DIRFILE* D;
  char format_file[FILENAME_MAX];

  dtrace("\"%s\", 0%o", filedir, flags);

  D = malloc(sizeof(DIRFILE));
  _GD_ClearError(D);
  D->recurse_level = 0;

  D->entry = NULL;
  D->error_string = malloc(FILENAME_MAX);
  D->error_file = malloc(FILENAME_MAX);
  D->name = strdup(filedir);
  D->frame_offset = 0;
  D->n_entries = 0;
  D->first_field = NULL;
  D->field_list = NULL;
  D->flags = flags | GD_INVALID;
  D->n_include = 0;

  snprintf(format_file, FILENAME_MAX, "%s%sformat", filedir,
      (filedir[strlen(filedir) - 1] == '/') ? "" : "/");

  /* open the format file (or create it) */
  if ((fp = _GD_CreateDirfile(D, format_file, filedir, flags)) == NULL) {
    dreturn("%p", D);
    return D; /* errors have already been set */
  }

  /* Parse the file.  This will take care of any necessary inclusions */
  D->n_include = 1;

  D->include_list = malloc(sizeof(struct gd_include_t));
  if (D->include_list == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  D->include_list[0].name = "format";
  D->include_list[0].modified = 0;
  D->include_list[0].parent = -1;

  _GD_ParseFormatFile(fp, D, filedir, ".", format_file, 0,
      DIRFILE_STANDARDS_VERSION);
  fclose(fp);

  if (D->error != GD_E_OK) {
    dreturn("%p", D);
    return D;
  }

  /** Now sort the entries */
  if (D->n_entries > 1)
    qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);

  /* Success! Clear invalid bit */
  if (D->error == GD_E_OK)
    D->flags &= ~GD_INVALID;

  dreturn("%p", D);
  return D;
}
/* vim: ts=2 sw=2 et tw=80
*/
