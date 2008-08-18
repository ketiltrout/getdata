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
 * You should have received a copy of the GNU General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
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

#include "getdata_internal.h"

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 3) /* for lincom */

/* _GD_GetLine: read non-comment line from format file.  The line is placed in
 *       *line.  Returns 1 if successful, 0 if unsuccessful.
 */
int __nonnull((1, 2, 3)) _GD_GetLine(FILE *fp, char *line, int* linenum)
{
  char *ret_val;
  int first_char;
  int i, len;

  do {
    ret_val = fgets(line, MAX_LINE_LENGTH, fp);
    (*linenum)++;
    first_char = 0;
    while (line[first_char] == ' ' || line[first_char] == '\t') ++first_char;
    line += first_char;
  } while (ret_val && (line[0] == '#' || line[0] == 0 || line[1] == 0));


  if (ret_val) {
    /* truncate comments from end of lines */
    len = strlen(line);
    for (i = 0; i < len; i++) {
      if (line[i] == '#')
        line[i] = '\0';
    }

    return 1; /* a line was read */
  }
  return 0;  /* there were no valid lines */
}

/* _GD_FreeD: free the DIRFILE and its subordinates
*/
static void _GD_FreeD(DIRFILE* D)
{
  int i, j;

  for (i = 0; i < D->n_entries; ++i) 
    if (D->entries[i] != NULL) {
      free((char*)D->entries[i]->field); /* cast away bogus constness */
      switch(D->entries[i]->field_type) {
        case GD_RAW_ENTRY:
          free(ENTRY(Raw, D->entries[i])->file);
          break;
        case GD_LINCOM_ENTRY:
          for (j = 0; j < ENTRY(Lincom, D->entries[i])->n_infields; ++j)
            free(ENTRY(Lincom, D->entries[i])->in_fields[j]);
          break;
        case GD_LINTERP_ENTRY:
          free(ENTRY(Linterp, D->entries[i])->raw_field);
          free(ENTRY(Linterp, D->entries[i])->linterp_file);
          if (ENTRY(Linterp, D->entries[i])->n_interp > 0) {
            free(ENTRY(Linterp, D->entries[i])->x);
            free(ENTRY(Linterp, D->entries[i])->y);
          }
          break;
        case GD_MULTIPLY_ENTRY:
          free(ENTRY(Multiply, D->entries[i])->in_fields[0]);
          free(ENTRY(Multiply, D->entries[i])->in_fields[1]);
          break;
        case GD_BIT_ENTRY:
          free(ENTRY(Bit, D->entries[i])->raw_field);
          break;
        case GD_PHASE_ENTRY:
          free(ENTRY(Phase, D->entries[i])->raw_field);
          break;
      }
    }

  free(D->entries);
  free(D->error_string);
  free(D->error_file);
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

/* _GD_ParseRaw: parse a RAW data type in the format file
*/
static void* _GD_ParseRaw(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* subdir, const char* format_file, int line)
{
  _GD_ClearGetDataError(D);

  if (n_cols < 4) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
        line, NULL);
    return NULL;
  }

  struct RawEntryType* R = malloc(sizeof(struct RawEntryType));
  if (R == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return NULL;
  }

  R->field = strdup(in_cols[0]);
  R->field_type = GD_RAW_ENTRY;

  R->file = malloc(FILENAME_MAX);
  if (R->file == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return R;
  }

  snprintf((char*)R->file, FILENAME_MAX, "%s/%s", subdir, in_cols[0]);
  R->fp = -1; /* file not opened yet */
  R->data_type = _GD_RawType(in_cols[2]);
  R->size = GD_SIZE(R->data_type);

  if (R->size == 0) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file,
        line, in_cols[2]);
    return R;
  }

  R->samples_per_frame = atoi(in_cols[3]);

  if (R->samples_per_frame <= 0)
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_SPF, format_file,
        line, in_cols[3]);

  return R;
}

/* _GD_ParseLincom: parse a LINCOM data type in the format file.
*/
static void* _GD_ParseLincom(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  int i;

  _GD_ClearGetDataError(D);

  if (n_cols < 3) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
        line, NULL);
    return NULL;
  }

  struct LincomEntryType* L = malloc(sizeof(struct LincomEntryType));
  if (L == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return NULL;
  }

  L->field = strdup(in_cols[0]);
  L->field_type = GD_LINCOM_ENTRY;
  L->n_infields = atoi(in_cols[2]);

  if (L->field == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return L;
  }

  if ((L->n_infields < 1) || (L->n_infields > GD_MAX_LINCOM)) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_FIELDS, format_file,
        line, in_cols[2]);
    return L;
  }

  if (n_cols < L->n_infields * 3 + 3) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
        line, NULL);
    return L;
  }

  for (i = 0; i < L->n_infields; i++) {
    L->in_fields[i] = strdup(in_cols[i * 3 + 3]);
    if (L->in_fields[i] == NULL)
      _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    L->m[i] = atof(in_cols[i * 3 + 4]);
    L->b[i] = atof(in_cols[i * 3 + 5]);
  }

  return L;
}

/* _GD_ParseLinterp: parse a LINTERP data type in the format file.
*/
static void* _GD_ParseLinterp(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  char temp_buffer[FILENAME_MAX];

  _GD_ClearGetDataError(D);

  if (n_cols < 4) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
        line, NULL);
    return NULL;
  }

  struct LinterpEntryType* L = malloc(sizeof(struct LinterpEntryType));
  if (L == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return NULL;
  }

  L->field = strdup(in_cols[0]);
  L->field_type = GD_LINTERP_ENTRY;
  L->raw_field = strdup(in_cols[2]);
  L->n_interp = -1; /* linterp file not read yet */


  if (in_cols[3][0] == '/')
    L->linterp_file = strdup(in_cols[3]);
  else {
    /* non-absolute paths are relative to the format file's directory */
    L->linterp_file = malloc(FILENAME_MAX);
    strcpy(temp_buffer, format_file);
    strcpy(L->linterp_file, dirname(temp_buffer));
    strcat(L->linterp_file, "/");
    strcat(L->linterp_file, in_cols[3]);
  }

  if (L->field == NULL || L->raw_field == NULL || L->linterp_file == NULL)
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  return L;
}

/* _GD_ParseMultiply: parse MULTIPLY data type entry in format file.
*/
static void* _GD_ParseMultiply(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  _GD_ClearGetDataError(D);

  if (n_cols < 4) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
        line, NULL);
    return NULL;
  }

  struct MultiplyEntryType* M = malloc(sizeof(struct MultiplyEntryType));
  if (M == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return NULL;
  }

  M->field = strdup(in_cols[0]);
  M->field_type = GD_MULTIPLY_ENTRY;
  M->in_fields[0] = strdup(in_cols[2]);
  M->in_fields[1] = strdup(in_cols[3]);

  if (M->field == NULL || M->in_fields[0] == NULL || M->in_fields[1] == NULL)
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  return M;
}

/* _GD_ParseBit: parse BIT data type entry in format file.
*/
static void* _GD_ParseBit(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  _GD_ClearGetDataError(D);

  if (n_cols < 4) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
        line, NULL);
    return NULL;
  }

  struct BitEntryType* B = malloc(sizeof(struct BitEntryType));
  if (B == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return NULL;
  }

  B->field = strdup(in_cols[0]);
  B->field_type = GD_BIT_ENTRY;
  B->raw_field = strdup(in_cols[2]);

  if (B->field == NULL || B->raw_field == NULL)
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  B->bitnum = atoi(in_cols[3]);
  if (n_cols > 4)
    B->numbits = atoi(in_cols[4]);
  else
    B->numbits = 1;

  if (B->numbits < 1)
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_NUMBITS, format_file,
        line, NULL);
  else if (B->bitnum < 0)
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_BITNUM, format_file,
        line, NULL);
  else if (B->bitnum + B->numbits - 1 > 63)
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_BITSIZE, format_file,
        line, NULL);

  return B;
}

/* _GD_ParsePhase: parse PHASE data type entry in formats file.
*/
static void* _GD_ParsePhase(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const char* format_file, int line)
{
  _GD_ClearGetDataError(D);

  if (n_cols < 4) {
    _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
        line, NULL);
    return NULL;
  }

  struct PhaseEntryType* P = malloc(sizeof(struct PhaseEntryType));
  if (P == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return NULL;
  }

  P->field = strdup(in_cols[0]);
  P->field_type = GD_PHASE_ENTRY;
  P->raw_field = strdup(in_cols[2]); /* field */
  P->shift = atoi(in_cols[3]); /*shift*/

  if (P->field == NULL || P->raw_field == NULL)
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  /*FIXME make sure the shift is within the range of the raw field...*/

  return P;
}

/* _GD_EntryCmp: Comparison functions for the entries
*/
static int _GD_EntryCmp(const void *A, const void *B)
{
  return (strcmp((*(struct gd_entry_t **)A)->field,
        (*(struct gd_entry_t **)B)->field));
}

/* _GD_ParseFormatFile: Perform the actual parsing of the format file.  This
 *       function is called from GetFormat once for the main format file and
 *       once for each included file.
 */
static void _GD_ParseFormatFile(FILE* fp, DIRFILE *D, const char* filedir,
    const char* subdir, const char* format_file, int standards,
    char*** IncludeList, int *i_include)
{
  char instring[MAX_LINE_LENGTH];
  const char* in_cols[MAX_IN_COLS];
  char* ptr;
  int n_cols = 0;
  int linenum = 0;
  int ws = 1;

#ifdef GETDATA_DEBUG
  printf("_GD_ParseFormatFile(%p, %p, %s, %s, %s, %i, %p, %i)\n", fp, D,
      filedir, subdir, format_file, standards, IncludeList, *i_include);
#endif

  _GD_ClearGetDataError(D);

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

    if (n_cols < 2) {
      _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file,
          linenum, NULL);

      /* Directives */

    } else if (strcmp(in_cols[0], "FRAMEOFFSET") == 0) {
      D->frame_offset = atoi(in_cols[1]);
    } else if (strcmp(in_cols[0], "INCLUDE") == 0) {
      int i, found = 0;
      char temp_buffer[FILENAME_MAX];
      char new_format_file[FILENAME_MAX];
      char new_subdir[FILENAME_MAX];
      FILE* new_fp = NULL;

      /* Run through the include list to see if we've already included this
       * file */
      for (i = 0; i < *i_include; ++i)
        if (strcmp(in_cols[1], (*IncludeList)[i]) == 0) {
          found = 1;
          break;
        }

      /* If we found the file, we won't reopen it.  Continue parsing. */
      if (found)
        continue;

      /* Otherwise, try to open the file */
      snprintf(new_format_file, FILENAME_MAX, "%s/%s/%s", filedir,
          subdir, in_cols[1]);
      new_fp = fopen(new_format_file, "r");

      /* If opening the file failed, set the error code and abort parsing. */
      if (new_fp == NULL) {
        _GD_SetGetDataError(D, GD_E_OPEN_INCLUDE, 0, format_file, linenum,
            new_format_file);
        break;
      }

      /* If we got here, we managed to open the included file; parse it */
      *IncludeList = realloc(*IncludeList, ++(*i_include) * sizeof(char*));
      (*IncludeList)[*i_include - 1] = strdup(in_cols[1]);

      if ((*IncludeList)[*i_include - 1] == NULL) {
        _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        return;
      }

      /* extract the subdirectory name - dirname both returns a volatile string
       * and modifies its argument, ergo strcpy */
      strcpy(temp_buffer, in_cols[1]);
      if (strcmp(subdir, ".") == 0)
        strcpy(new_subdir, dirname(temp_buffer));
      else
        snprintf(new_subdir, FILENAME_MAX, "%s/%s", subdir,
            dirname(temp_buffer));

      _GD_ParseFormatFile(new_fp, D, filedir, new_subdir, new_format_file,
          standards, IncludeList, i_include);
      fclose(new_fp);
    } else if (strcmp(in_cols[0], "ENDIAN") == 0) {
      if (!(D->flags & GD_FORCE_ENDIAN)) {
        if (strcmp(in_cols[1], "big") == 0) {
          D->flags |= GD_BIG_ENDIAN;
          D->flags &= ~GD_LITTLE_ENDIAN;
        } else if (strcmp(in_cols[1], "little") == 0) {
          D->flags |= GD_LITTLE_ENDIAN;
          D->flags &= ~GD_BIG_ENDIAN;
        } else 
          _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN, format_file,
              linenum, NULL);
      }
    } else if (strcmp(in_cols[0], "VERSION") == 0) {
      standards = atoi(in_cols[1]);

      /* Field Types */

    } else if ((strcmp(in_cols[0], "INDEX") == 0) ||
        (strcmp(in_cols[0], "FILEFRAM") == 0))
    { /* reserved field names */
      _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_RES_NAME, format_file,
          linenum, NULL);
    } else if (strcmp(in_cols[1], "RAW") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseRaw(D, in_cols, n_cols, subdir,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "LINCOM") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseLincom(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "LINTERP") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseLinterp(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "MULTIPLY") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseMultiply(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "BIT") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseBit(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (strcmp(in_cols[1], "PHASE") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParsePhase(D, in_cols, n_cols,
          format_file, linenum);
      if (D->error != GD_E_OK)
        D->n_entries--;
    } else if (standards <= DIRFILE_STANDARDS_VERSION ||
        (D->flags & GD_PEDANTIC))
    {
      _GD_SetGetDataError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, format_file,
          linenum, NULL);
    }

    /* break out of loop (so we can return) if we've encountered an error */
    if (D->error != GD_E_OK)
      break;
  }

  return;
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
    _GD_SetGetDataError(D, GD_E_OPEN, GD_E_OPEN_NO_ACCESS, format_file,
        0, NULL);
    return NULL;
  }

  /* the directory exists, but it's not a dirfile, do nothing else -- even if we
   * were asked to truncate it */
  if (!dir_error && format_error) {
    _GD_SetGetDataError(D, GD_E_OPEN, GD_E_OPEN_NOT_DIRFILE, format_file,
        0, NULL);
    return NULL;
  }

  /* Couldn't open the file, and we weren't asked to create it */
  if (format_error && !(flags & GD_CREAT)) {
    _GD_SetGetDataError(D, GD_E_OPEN, GD_E_OPEN_NOT_EXIST, format_file,
        format_error, NULL);
    return NULL;
  }

  /* It does exist, but we were asked to exclusively create it */
  if (!format_error && (flags & GD_CREAT) && (flags & GD_EXCL)) {
    _GD_SetGetDataError(D, GD_E_CREAT, GD_E_CREAT_EXCL, filedir, 0, NULL);
    fclose(fp);
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
      _GD_SetGetDataError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      return NULL;
    }

    /* This code is from defile */
    if ((dir = opendir(filedir)) == NULL) {
      _GD_SetGetDataError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, filedir, errno, 
          NULL);
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
        _GD_SetGetDataError(D, GD_E_TRUNC, GD_E_TRUNC_STAT, fullname, errno, 
            NULL);
        return NULL;
      }

      /* only delete regular files */
      if (S_ISREG(statbuf.st_mode)) {
        if (unlink(fullname)) {
          _GD_SetGetDataError(D, GD_E_TRUNC, GD_E_TRUNC_UNLINK, fullname, errno,
              NULL);
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
      _GD_SetGetDataError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      return NULL;
    }

    /* attempt to create the dirfile directory, if not present */
    if (dir_error) 
      if (mkdir(filedir, 00777) < 0) {
        _GD_SetGetDataError(D, GD_E_CREAT, GD_E_CREAT_DIR, filedir, errno,
            NULL);
        return NULL;
      }

    /* create a new, empty format file */
    if ((fp = fopen(format_file, "wt")) == NULL) {
      _GD_SetGetDataError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno,
          NULL);
      return NULL;
    }
  }

  /* open succeeds */
  return fp;
}

/* dirfile_open: open (or, perhaps, create) and parse the specified dirfile
*/
DIRFILE* dirfile_open(const char* filedir, unsigned int flags)
{
  int i;
  struct stat statbuf;
  FILE *fp;
  DIRFILE* D;
  char format_file[FILENAME_MAX];
  char raw_data_filename[FILENAME_MAX];
  char **IncludeList = NULL;
  int i_include;

#ifdef GETDATA_DEBUG
  printf("dirfile_open(%s, %x)\n", filedir, flags);
#endif

  D = malloc(sizeof(DIRFILE));
  _GD_ClearGetDataError(D);
  D->recurse_level = 0;

  D->entries = NULL;
  D->error_string = malloc(FILENAME_MAX);
  D->error_file = malloc(FILENAME_MAX);
  D->name = strdup(filedir);
  D->frame_offset = 0;
  D->n_entries = 0;
  D->first_field = NULL;
  D->flags = flags | GD_INVALID;

  snprintf(format_file, FILENAME_MAX, "%s%sformat", filedir,
      (filedir[strlen(filedir) - 1] == '/') ? "" : "/");

  /* open the format file (or create it) */
  if ((fp = _GD_CreateDirfile(D, format_file, filedir, flags)) == NULL)
    return D; /* errors have already been set */

  /* Parse the file.  This will take care of any necessary inclusions */
  i_include = 1;

  IncludeList = malloc(sizeof(char*));
  if (IncludeList == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return D;
  }

  IncludeList[0] = strdup("format");

  if (IncludeList[0] == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(IncludeList);
    return D;
  }

  _GD_ParseFormatFile(fp, D, filedir, ".", format_file,
      DIRFILE_STANDARDS_VERSION, &IncludeList, &i_include);
  fclose(fp);

  /* Clean up IncludeList.  We don't need it anymore */
  for (i = 0; i < i_include; ++i)
    free(IncludeList[i]);
  free(IncludeList);

  if (D->error != GD_E_OK)
    return D;

  /* find the first raw field */
  for (i = 0; i < D->n_entries; i++) {
    if (D->entries[i]->field_type == GD_RAW_ENTRY) {
      snprintf(raw_data_filename, FILENAME_MAX, "%s/%s", filedir,
          ENTRY(Raw, D->entries[i])->file);
      if (stat(raw_data_filename, &statbuf) >= 0) {
        /* This must be a deep copy because we don't know where this particular
         * entry will end up after the qsort */
        D->first_field = malloc(sizeof(struct RawEntryType));
        if (D->first_field == NULL)
          _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        else
          memcpy(D->first_field, D->entries[i], sizeof(struct RawEntryType));
        break;
      }
    }
  }

  /** Now sort the entries */
  if (D->n_entries > 1)
    qsort(D->entries, D->n_entries, sizeof(struct gd_entry_t*), _GD_EntryCmp);

  /* Success! Clear invalid bit */
  if (D->error == GD_E_OK)
    D->flags &= ~GD_INVALID;

  return D;
}

/* dirfile_close: Close the specified dirfile and free memory
*/
void dirfile_close(DIRFILE* D)
{
  int i;

  for(i = 0; i < D->n_entries; ++i)
    if (D->entries[i]->field_type == GD_RAW_ENTRY)
      close(ENTRY(Raw, &D->entries[i])->fp);

  _GD_ClearGetDataError(D);
  _GD_FreeD(D);
}
/* vim: ts=2 sw=2 et tw=80
*/
