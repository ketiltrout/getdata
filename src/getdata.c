/*                           (C) 2002 C. Barth Netterfield */
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#ifndef NDEBUG
#include <assert.h>
#else
#define assert(...)
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#include "getdata_internal.h"

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 3) /* for lincom */

/* The following has been extracted from internal.cpp from kjs */

/*
 ** For systems without NAN, this is a NAN in IEEE double format.
 */

#if !defined(NAN)
static double __NAN()
{
  /* work around some strict alignment requirements
     for double variables on some architectures (e.g. PA-RISC) */
  typedef union { unsigned char b[8]; double d; } nan_t;
#ifdef WORDS_BIGENDIAN
  static const nan_t NaN_Bytes = { { 0x7f, 0xf8, 0, 0, 0, 0, 0, 0 } };
#elif defined(arm)
  static const nan_t NaN_Bytes = { { 0, 0, 0xf8, 0x7f, 0, 0, 0, 0 } };
#else
  static const nan_t NaN_Bytes = { { 0, 0, 0, 0, 0, 0, 0xf8, 0x7f } };
#endif

  const double NaN = NaN_Bytes.d;
  return NaN;
}
#define NAN __NAN()
#endif /* !defined(NAN) */

const char *GD_ERROR_CODES[GD_N_ERROR_CODES] = {"Success",
  "Error opening dirfile",
  "Error in Format file",
  "Error truncating dirfile",
  "Error creating dirfile",
  "Field code not found in File Format",
  "Unrecognized return type",
  "Could not open field file",
  "Could not open included Format file",
  "Internal error",
  "No RAW fields available",
  "Memory allocation failed",
  "No RAW fields defined",
  "Could not open interpolation file",
  "Too many levels of recursion",
  "Bad DIRFILE",
  "Cannot write non-RAW data",
  "Could not open field file for writing",
  "Could not close read-only field file",
  "Could not write to field file (already in use)",
  "Could not allocate file lock struct",
  "Cannot write to LINCOM with multiple raw fields",
  "Error in Endian conversion"
};

/* _GD_GetLine: read non-comment line from format file.  The line is placed in
 *       *line.  Returns 1 if successful, 0 if unsuccessful.
 */
static int _GD_GetLine(FILE *fp, char *line, int* linenum)
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

/* _GD_SetGetDataError: Sets the global error variables for a library error
*/
void _GD_SetGetDataError(DIRFILE* D, int error, int suberror,
    const char* format_file, int line, const char* token)
{
  D->error = error;
  D->suberror = suberror;
  D->error_line = line;
  if (format_file != NULL)
    strncpy(D->error_file, format_file, FILENAME_MAX);
  if (token != NULL)
    strncpy(D->error_string, token, FILENAME_MAX);
}

/* _GD_ClearGetDataError: Everything's A-OK; clear the last error.
*/
void _GD_ClearGetDataError(DIRFILE* D)
{
  D->error = GD_E_OK;
}

/* GetDataErrorString: Write a descriptive message in the supplied buffer
 *       describing the last library error.  The message may be truncated but
 *       will be null terminated.  Returns buffer, or NULL if buflen < 1.
 */
char* getdata_error_string(DIRFILE* D, char* buffer, size_t buflen)
{
  char* ptr;

  /* Sanity check */
  if (buffer == NULL || buflen < 1)
    return NULL;

  /* Copy the default error message into the buffer and make sure
   * the result is null terminated */
  strncpy(buffer, GD_ERROR_CODES[D->error], buflen - 1);
  buffer[buflen - 1] = 0;

  /* point to the end of the string and reduce buflen appropriately */
  ptr = buffer + strlen(buffer);
  buflen -= strlen(buffer);

  /* add the anciliary data - we use snprintfs here to ensure the resultant
   * string is properly null terminated (while not overflowing the buffer) */
  switch (D->error) {
    case GD_E_INTERNAL_ERROR: /* internal error: report line and source file
                                 where it happened */
      snprintf(ptr, buflen, "  [%s,%i]", D->error_file, D->error_line);
      break;
    case GD_E_OPEN: /* main format file couldn't be opened -- report filename
                       and then the suberror */
      snprintf(ptr, buflen, " %s: ", D->error_file);
      buflen -= strlen(ptr);
      ptr += strlen(ptr);

      switch (D->suberror) {
        case GD_E_OPEN_NOT_DIRFILE:
          snprintf(ptr, buflen, "not a dirfile");
          break;
        case GD_E_OPEN_NOT_EXIST: /* report the libc error encountered */
        case GD_E_OPEN_NO_ACCESS:
          strerror_r(D->error_line, ptr, buflen);
          break;
      }
      break;
    case GD_E_CREAT: /* couldn't create the dirifle */
      snprintf(ptr, buflen, " %s: ", D->error_file);
      buflen -= strlen(ptr);
      ptr += strlen(ptr);

      switch (D->suberror) {
        case GD_E_CREAT_DIR:
          snprintf(ptr, buflen, "unable to make directory");
          break;
        case GD_E_CREAT_FORMAT:
          snprintf(ptr, buflen, "unable to create format file");
          break;
        case GD_E_CREAT_EXCL: /* GD_EXCL create failed */
          snprintf(ptr, buflen, "already exists");
          break;
      }
      break;
    case GD_E_TRUNC: /* couldn't truncate the dirfile */
      snprintf(ptr, buflen, " %s: ", D->error_file);
      buflen -= strlen(ptr);
      ptr += strlen(ptr);

      switch (D->suberror) {
        case GD_E_TRUNC_DIR:
        case GD_E_TRUNC_STAT:
        case GD_E_TRUNC_UNLINK:
          strerror_r(D->error_line, ptr, buflen);
          break;
      }
      break;
    case GD_E_FORMAT: /* syntax errors in the format file -- lots of
                         suberror types here */

      /* No RAW fields specified -- this isn't tied to a particular line */
      if (D->suberror == GD_E_FORMAT_N_RAW) {
        snprintf(ptr, buflen, ": no raw fields defined");
        break;
      }

      /* otherwise, add the format filename and line number where the
       * syntax error was found */
      snprintf(ptr, buflen, " on line %i of %s: ", D->error_line,
          D->error_file);
      buflen -= strlen(ptr);
      ptr += strlen(ptr);

      switch (D->suberror) {
        case GD_E_FORMAT_BAD_TYPE: /* bad field type; include the thing
                                         we thought was the type specifier */
          snprintf(ptr, buflen, "bad raw field type: %s", D->error_string);
          break;
        case GD_E_FORMAT_BAD_SPF: /* SPF < 0 -- print the column we expected
                                        to hold the SPF */
          snprintf(ptr, buflen, "samples per frame out of range: %s",
              D->error_string);
          break;
        case GD_E_FORMAT_N_FIELDS: /* number of fields in the LINCOM and
                                         the number of columns in the format
                                         file don't match */
          snprintf(ptr, buflen, "lincom field count out of range: %s",
              D->error_string);
          break;
        case GD_E_FORMAT_N_COLS: /* missing data we expected to find on this
                                       line */
          snprintf(ptr, buflen, "missing column");
          break;
        case GD_E_FORMAT_MAX_I: /* max_i out of range (what is an MPLEX?) */
          snprintf(ptr, buflen, "max_i out of range: %s", D->error_string);
          break;
        case GD_E_FORMAT_NUMBITS: /* bitfield numbits is less than 1 */
          snprintf(ptr, buflen, "numbits out of range");
          break;
        case GD_E_FORMAT_BITNUM: /* bitnum is less than 0 */
          snprintf(ptr, buflen, "starting bit out of range");
          break;
        case GD_E_FORMAT_BITSIZE: /* bitfield extends past 32 bits */
          snprintf(ptr, buflen, "end of bitfield is out of bounds");
          break;
        case GD_E_FORMAT_BAD_LINE: /* couldn't make heads nor tails of the
                                         line -- ie. a mistyped keyword &c. */
          snprintf(ptr, buflen, "line indecypherable");
          break;
        case GD_E_FORMAT_RES_NAME: /* field name reserved */
          snprintf(ptr, buflen, "field name is reserved");
          break;
        case GD_E_FORMAT_ENDIAN: /* unknown endianness */
          snprintf(ptr, buflen, "unrecognised endianness");
          break;
      }
      break;
    case GD_E_OPEN_INCLUDE: /* Couldn't open an INCLUDEd file -- report the
                               included filename as well as the line and name
                               of the format file where it was encountered */
      snprintf(ptr, buflen, " %s on line %i of %s", D->error_string,
          D->error_line, D->error_file);
      break;
    case GD_E_BAD_RETURN_TYPE: /* unsupported data return type passed to
                                  GetData */
      snprintf(ptr, buflen, ": %02x", D->suberror);
      break;
    case GD_E_RECURSE_LEVEL: /* recursion too deep -- report field name for
                                which this happened */
      snprintf(ptr, buflen, " while resolving field %s", D->error_string);
      break;
    case GD_E_BAD_CODE: /* A required field name wasn't defined */
    case GD_E_OPEN_RAWFIELD: /* A raw field file wasn't found on disk */
      snprintf(ptr, buflen, ": %s", D->error_string);
      break;
    case GD_E_OPEN_LINFILE: /* problems with LINTERPs: report the linterp
                               filename with the error message */
      snprintf(ptr, buflen, " %s: %s", D->error_string,
          (D->suberror == GD_E_LINFILE_OPEN) ? "open failed"
          : "file too short");
      break;
    case GD_E_NO_RAW_FIELDS: /* couldn't find the first RAW file */
      snprintf(ptr, buflen, ": %s", "no RAW fields available");
      break;
  }

  return buffer;
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

size_t _GD_TypeSize(gd_type_t type)
{
  switch (type) {
    case GD_INT8:
    case GD_UINT8:
      return 1;
    case GD_INT16:
    case GD_UINT16:
      return 2;
    case GD_INT32:
    case GD_UINT32:
    case GD_FLOAT32:
      return 4;
    case GD_INT64:
    case GD_UINT64:
    case GD_FLOAT64:
      return 8;
  }

  return 0;
}

static gd_type_t _GD_RawType(const char* type)
{
  /* for backwards compatibility */
  if (strlen(type) == 1) {
    if (type[0] == GD_INT32_ALT)
      return GD_INT32;
    else
      return type[0];
  }

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
  R->size = _GD_TypeSize(R->data_type);

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

  if ((L->n_infields < 1) || (L->n_infields > GD_MAX_LINCOM) ||
      (n_cols < L->n_infields * 3 + 3))
  {
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
  L->linterp_file = strdup(in_cols[3]);
  L->n_interp = -1; /* linterp file not read yet */

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
    const char* subdir, const char* format_file, char*** IncludeList,
    int *i_include)
{
  char instring[MAX_LINE_LENGTH];
  const char* in_cols[MAX_IN_COLS];
  char* ptr;
  int n_cols = 0;
  int linenum = 0;
  int standards = DIRFILE_STANDARDS_VERSION;
  int ws = 1;

  _GD_ClearGetDataError(D);

  /***** start parsing ****/
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

      /* If we got here, we managed to open the inlcuded file; parse it */
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
          IncludeList, i_include);
      fclose(new_fp);
    } else if (strcmp(in_cols[0], "ENDIAN") == 0) {
      if (!(D->flags & GD_FORCE_ENDIAN)) {
        if (strcmp(in_cols[1], "big")) {
          D->flags |= GD_BIG_ENDIAN;
          D->flags &= ~GD_LITTLE_ENDIAN;
        } else if (strcmp(in_cols[1], "litte")) {
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
    } else if (strcmp(in_cols[1], "LINCOM") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseLincom(D, in_cols, n_cols,
          format_file, linenum);
    } else if (strcmp(in_cols[1], "LINTERP") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseLinterp(D, in_cols, n_cols,
          format_file, linenum);
    } else if (strcmp(in_cols[1], "MULTIPLY") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseMultiply(D, in_cols, n_cols,
          format_file, linenum);
    } else if (strcmp(in_cols[1], "BIT") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParseBit(D, in_cols, n_cols,
          format_file, linenum);
    } else if (strcmp(in_cols[1], "PHASE") == 0) {
      D->n_entries++;
      D->entries = realloc(D->entries, D->n_entries *
          sizeof(struct gt_entry_t**));
      D->entries[D->n_entries - 1] = _GD_ParsePhase(D, in_cols, n_cols,
          format_file, linenum);
    } else if (standards <= DIRFILE_STANDARDS_VERSION ||
        (D->flags && GD_PEDANTIC))
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
    _GD_SetGetDataError(D, GD_E_OPEN_FORMAT, GD_E_OPEN_NO_ACCESS, format_file,
        0, NULL);
    return NULL;
  }

  /* the directory exists, but it's not a dirfile, do nothing else -- even if we
   * were asked to truncate it */
  if (!dir_error && format_error) {
    _GD_SetGetDataError(D, GD_E_OPEN_FORMAT, GD_E_OPEN_NOT_DIRFILE, format_file,
        0, NULL);
    return NULL;
  }

  /* Couldn't open the file, and we weren't asked to create it */
  if (format_error && !(flags & GD_CREAT)) {
    _GD_SetGetDataError(D, GD_E_OPEN_FORMAT, GD_E_OPEN_NOT_EXIST, format_file,
        format_error, NULL);
    return NULL;
  }

  /* It does exist, but we were asked to exclusively create it */
  if (!format_error && (flags & GD_CREAT) && (flags & GD_EXCL)) {
    _GD_SetGetDataError(D, GD_E_CREAT, GD_E_CREAT_EXCL, filedir, 0, NULL);
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

  D = malloc(sizeof(DIRFILE));
  _GD_ClearGetDataError(D);
  D->recurse_level = 0;

  /***** open the format file (or create it) ******/
  if ((fp = _GD_CreateDirfile(D, format_file, filedir, flags)) == NULL)
    return D; /* errors have already been set */

  D->entries = NULL;
  D->error_string = malloc(FILENAME_MAX);
  D->error_file = malloc(FILENAME_MAX);
  D->name = strdup(filedir);
  D->frame_offset = 0;
  D->n_entries = 0;
  D->first_field = NULL;
  D->flags = flags;

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

  _GD_ParseFormatFile(fp, D, filedir, ".", format_file, &IncludeList,
      &i_include);
  fclose(fp);

  /* Clean up IncludeList.  We don't need it anymore */
  for (i = 0; i < i_include; ++i)
    free(IncludeList[i]);
  free(IncludeList);

  if (D->error != GD_E_OK)
    return D;

  /* find the first raw field */
  for (i = 0; i < D->n_entries; i++)
    if (D->entries[i]->field_type == GD_RAW_ENTRY) {
      snprintf(raw_data_filename, FILENAME_MAX, "%s/%s", filedir,
          ENTRY(Raw, &D->entries[i])->file);
      if (stat(raw_data_filename, &statbuf) >= 0) {
        /* This must be a deep copy because we don't know where this particular
         * entry will end up after the qsort */
        D->first_field = malloc(sizeof(struct RawEntryType));
        if (D->first_field == NULL)
          _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        else
          memcpy(D->first_field, &D->entries[i], sizeof(struct RawEntryType));
        break;
      }
    }

  /** Now sort the entries */
  if (D->n_entries > 1)
    qsort(D->entries, D->n_entries, sizeof(struct gd_entry_t*), _GD_EntryCmp);

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

/* _GD_FillFileFrame: fill dataout with frame indicies
*/
static void _GD_FillFileFrame(void *dataout, char rtype, uint32_t s0,
    uint32_t n)
{
  uint32_t i;

  switch (rtype) {
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)dataout)[i] = (int8_t)(i + s0);
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)dataout)[i] = (uint8_t)(i + s0);
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)dataout)[i] = (int16_t)(i + s0);
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)dataout)[i] = (uint16_t)(i + s0);
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)dataout)[i] = (int32_t)(i + s0);
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)dataout)[i] = (i + s0);
      break;
    case GD_FLOAT:
      for (i = 0; i < n; i++)
        ((float*)dataout)[i] = (float)(i + s0);
      break;
    case GD_DOUBLE:
      for (i = 0; i < n; i++)
        ((double*)dataout)[i] = (double)(i + s0);
      break;
  }
}

/* _GD_ConvertType: copy data to output buffer while converting type.
*/
void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, int n)
{
  int i;

  _GD_ClearGetDataError(D);

  if (out_type == GD_NULL) /* null return type: don't return data */
    return;

  switch (in_type) {
    case GD_INT8:
      switch (out_type) {
        case GD_INT8:
          memcpy(data_out, data_in, n * sizeof(int8_t));
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
      }
      break;
    case GD_UINT8:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT8:
          memcpy(data_out, data_in, n * sizeof(uint8_t));
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
      }
      break;
    case GD_INT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_INT16:
          memcpy(data_out, data_in, n * sizeof(int16_t));
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
      }
      break;
    case GD_UINT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT16:
          memcpy(data_out, data_in, n * sizeof(uint16_t));
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
      }
      break;
    case GD_INT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_INT32:
          memcpy(data_out, data_in, n * sizeof(int32_t));
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        default:
          _GD_SetGetDataError(D, GD_E_BAD_RETURN_TYPE, out_type, NULL, 0, NULL);
          return;
      }
      break;
    case GD_UINT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT32:
          memcpy(data_out, data_in, n * sizeof(uint32_t));
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
      }
      break;
    case GD_INT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_INT64:
          memcpy(data_out, data_in, n * sizeof(int64_t));
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]=((int64_t*)data_in)[i];
          return;
      }
      break;
    case GD_UINT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT64:
          memcpy(data_out, data_in, n * sizeof(uint64_t));
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
      }
      break;
    case GD_FLOAT:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((float*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_FLOAT:
          memcpy(data_out, data_in, n * sizeof(float));
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]=((float*)data_in)[i];
          return;
      }
      break;
    case GD_DOUBLE:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((double*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_DOUBLE:
          memcpy(data_out, data_in, n * sizeof(double));
          return;
      }
      break;
  }

  _GD_SetGetDataError(D, GD_E_BAD_RETURN_TYPE, in_type, NULL, 0, NULL);
}


/* _GD_FillZero: fill data buffer with zero/NaN of the appropriate type.  Used
 *       if s0 < 0.  Fills up to position 0 or ns + s0, whichever is less
 */
static int _GD_FillZero(void *databuffer, char type, int s0, int ns)
{
  int i, nz = ns;
  const double NaN = NAN;

  if (s0 >= 0)
    return 0;

  if (s0 + ns > 0)
    nz = -s0;

  switch (type) {
    case GD_INT8:
    case GD_UINT8:
      memset(databuffer, 0, nz);
      break;
    case GD_INT16:
    case GD_UINT16:
      memset(databuffer, 0, nz * 2);
      break;
    case GD_INT32:
    case GD_UINT32:
      memset(databuffer, 0, nz * 4);
      break;
    case GD_INT64:
    case GD_UINT64:
      memset(databuffer, 0, nz * 8);
      break;
    case GD_FLOAT:
      for (i = 0; i < nz; ++i)
        *((float*)databuffer + i) = (float)NaN;
      break;
    case GD_DOUBLE:
      for (i = 0; i < nz; ++i)
        *((double*)databuffer + i) = (double)NaN;
      break;
  }

  return (nz);
}

/* Binary search to find the field */
struct gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code)
{
  int i, c;
  int l = 0;
  int u = D->n_entries;

  while (l < u) {
    i = (l + u) / 2;
    c = strcmp(field_code, D->entries[i]->field);
    if (c < 0)
      u = i;
    else if (c > 0)
      l = i + 1;
    else
      return D->entries[i];
  }

  return NULL;
}

/* _GD_GetSPF: Get samples per frame for field
*/
unsigned int _GD_GetSPF(const char *field_code, DIRFILE* D)
{
  struct gd_entry_t* entry;
  int spf = 0;

  if (D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetGetDataError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    return 0;
  }

  if ((strcmp(field_code, "FILEFRAM") == 0) ||
      (strcmp(field_code, "INDEX") == 0))
    return 1;

  /* Find the field */
  entry = _GD_FindField(D, field_code);

  if (entry == NULL) {
    _GD_SetGetDataError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    return 0;
  }

  D->recurse_level++;
  switch(entry->field_type) {
    case GD_RAW_ENTRY:
      spf = ENTRY(Raw, entry)->samples_per_frame;
      break;
    case GD_LINCOM_ENTRY:
      spf = _GD_GetSPF(ENTRY(Lincom, entry)->in_fields[0], D);
      break;
    case GD_MULTIPLY_ENTRY:
      spf = _GD_GetSPF(ENTRY(Multiply, entry)->in_fields[0], D);
      break;
    case GD_BIT_ENTRY:
      spf = _GD_GetSPF(ENTRY(Bit, entry)->raw_field, D);
      break;
    case GD_PHASE_ENTRY:
      spf = _GD_GetSPF(ENTRY(Phase, entry)->raw_field, D);
      break;
    case GD_LINTERP_ENTRY:
      spf = _GD_GetSPF(ENTRY(Linterp, entry)->raw_field, D);
      break;
    default:
      _GD_SetGetDataError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL);
  }
  D->recurse_level--;
  return spf;
}

static void _GD_FixEndianness(DIRFILE* D, char* databuffer, size_t size,
    int n_read)
{
  int i, j;
  char b;

  if (size == 1)
    return;

  for (i = 0; i < n_read; ++i)
    for (j = 0; j < size; ++j) {
      b = databuffer[size * (i + 1) - j - 1];
      databuffer[size * (i + 1) - j - 1] = databuffer[size * i + j];
      databuffer[size * i + j] = b;
    }
}

/* _GD_DoRaw:  Read from a raw.  Returns number of samples read.
*/
static int _GD_DoRaw(DIRFILE *D, struct RawEntryType *R,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t return_type, void *data_out)
{
  int s0, ns, samples_read, n_read = 0;
  char datafilename[FILENAME_MAX];
  void *databuffer;

  s0 = first_samp + first_frame*R->samples_per_frame;
  ns = num_samp + num_frames*R->samples_per_frame;

  /** open the file (and cache the fp) if it hasn't been opened yet. */
  if (R->fp < 0) {
    snprintf(datafilename, FILENAME_MAX, "%s/%s", D->name, R->file);
    R->fp = open(datafilename, O_RDONLY);
    if (R->fp < 0) {
      _GD_SetGetDataError(D, GD_E_OPEN_RAWFIELD, 0, NULL, 0, datafilename);
      return 0;
    }
  }

  databuffer = malloc(ns * R->size);
  if (databuffer == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return 0;
  }

  if (s0 < 0) {
    n_read = _GD_FillZero(databuffer, R->data_type, s0, ns);
    ns -= n_read;
    s0 = 0;
  }

  if (ns > 0) {
    lseek(R->fp, s0 * R->size, SEEK_SET);
    samples_read = read(R->fp, databuffer + n_read * R->size, ns * R->size) /
      R->size;

  if (D->flags &
#ifdef WORDS_BIGENDIAN
      GD_LITTLE_ENDIAN
#else
      GD_BIG_ENDIAN
#endif
     )
    _GD_FixEndianness(D, databuffer + n_read * R->size, R->size,
        samples_read);

    n_read += samples_read;
  }
  _GD_ConvertType(D, databuffer, R->data_type, data_out, return_type, n_read);

  free(databuffer);

  return n_read;
}


/* _GD_Alloc: allocate a buffer of the right type & size
*/
void* _GD_Alloc(DIRFILE* D, gd_type_t type, int n)
{
  void *buff = NULL;

  assert(n > 0);

  _GD_ClearGetDataError(D);

  switch (type) {
    case GD_NULL:
      buff = NULL;
      break;
    case GD_INT8:
    case GD_UINT8:
      buff = malloc(n);
      break;
    case GD_INT16:
    case GD_UINT16:
      buff = malloc(n * 2);
      break;
    case GD_INT32:
    case GD_UINT32:
    case GD_FLOAT32:
      buff = malloc(n * 4);
      break;
    case GD_INT64:
    case GD_UINT64:
    case GD_FLOAT64:
      buff = malloc(n * 8);
      break;
    default:
      _GD_SetGetDataError(D, GD_E_BAD_RETURN_TYPE, type, NULL, 0, NULL);
      return NULL;
  }

  if ((type != GD_NULL) && (buff == NULL))
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  return buff;
}

/* _GD_ScaleData: Compute data = data * m + b, for scalar m and b.
*/
void _GD_ScaleData(DIRFILE* D, void *data, gd_type_t type, int npts, double m,
    double b)
{
  int i;

  switch (type) {
    case GD_NULL:
      break;
    case GD_INT8:
      for (i = 0; i < npts; i++)
        ((int8_t*)data)[i] = ((double)((int8_t*)data)[i] * m + b);
      break;
    case GD_UINT8:
      for (i = 0; i < npts; i++)
        ((uint8_t*)data)[i] = ((double)((uint8_t*)data)[i] * m + b);
      break;
    case GD_INT16:
      for (i = 0; i < npts; i++)
        ((int16_t*)data)[i] = ((double)((int16_t*)data)[i] * m + b);
      break;
    case GD_UINT16:
      for (i = 0; i < npts; i++)
        ((uint16_t*)data)[i] = ((double)((uint16_t*)data)[i] * m + b);
      break;
    case GD_INT32:
      for (i = 0; i < npts; i++)
        ((int32_t*)data)[i] = ((double)((int32_t*)data)[i] * m + b);
      break;
    case GD_UINT32:
      for (i = 0; i < npts; i++)
        ((uint32_t*)data)[i] = ((double)((uint32_t*)data)[i] * m + b);
      break;
    case GD_INT64:
      for (i = 0; i < npts; i++)
        ((int64_t*)data)[i] = ((double)((int64_t*)data)[i] * m + b);
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++)
        ((uint64_t*)data)[i] = ((double)((uint64_t*)data)[i] * m + b);
      break;
    case GD_FLOAT:
      for (i = 0; i < npts; i++)
        ((float*)data)[i] = ((double)((float*)data)[i] * m + b);
      break;
    case GD_DOUBLE:
      for (i = 0; i < npts; i++)
        ((double*)data)[i] = ((double)((double*)data)[i] * m + b);
      break;
    default:
      _GD_SetGetDataError(D, GD_E_BAD_RETURN_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
}

/* _GD_AddData: add vector B to vector A.  B is unchanged
*/
static void _GD_AddData(DIRFILE* D, void *A, int spfA, void *B, int spfB,
    gd_type_t type, int n)
{
  int i;

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)A)[i] += ((int8_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)A)[i] += ((uint8_t*)B)[i * spfB / spfA];
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)A)[i] += ((int16_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)A)[i] += ((uint16_t*)B)[i * spfB / spfA];
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)A)[i] += ((int32_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)A)[i] += ((uint32_t*)B)[i * spfB / spfA];
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)A)[i] += ((int64_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)A)[i] += ((uint64_t*)B)[i * spfB / spfA];
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
      _GD_SetGetDataError(D, GD_E_BAD_RETURN_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
}

/* MultiplyData: Multiply A by B.  B is unchanged.
*/
static void _GD_MultiplyData(DIRFILE* D, void *A, int spfA, void *B, int spfB,
    gd_type_t type, int n)
{
  int i;

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)A)[i] *= ((int8_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)A)[i] *= ((uint8_t*)B)[i * spfB / spfA];
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)A)[i] *= ((int16_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)A)[i] *= ((uint16_t*)B)[i * spfB / spfA];
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)A)[i] *= ((int32_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)A)[i] *= ((uint32_t*)B)[i * spfB / spfA];
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)A)[i] *= ((int64_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)A)[i] *= ((uint64_t*)B)[i * spfB / spfA];
      break;
    case GD_FLOAT:
      for (i = 0; i < n; i++)
        ((float*)A)[i] *= ((float*)B)[i * spfB / spfA];
      break;
    case GD_DOUBLE:
      for (i = 0; i < n; i++)
        ((double*)A)[i] *= ((double*)B)[i * spfB / spfA];
      break;
    default:
      _GD_SetGetDataError(D, GD_E_BAD_RETURN_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
}


/* _GD_DoLincom:  Read from a lincom.  Returns number of samples read.
*/
static int _GD_DoLincom(DIRFILE *D, struct LincomEntryType *L,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int i;
  int spf1, spf2, num_samp2, first_samp2;
  int n_read, n_read2;

  D->recurse_level++;
  spf1 = _GD_GetSPF(L->in_fields[0], D);
  if (D->error != GD_E_OK)
    return 0;

  /* read and scale the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, L->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, return_type, data_out);

  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  /* Nothing to lincomise */
  if (n_read == 0)
    return 0;

  _GD_ScaleData(D, data_out, return_type, n_read, L->m[0], L->b[0]);

  if (L->n_infields > 1) {
    for (i = 1; i < L->n_infields; i++) {
      D->recurse_level++;

      /* find the samples per frame of the next field */
      spf2 = _GD_GetSPF(L->in_fields[i], D);
      if (D->error != GD_E_OK)
        return 1;

      /* calculate the first sample and number of samples to read of the
       * next field */
      num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
      first_samp2 = (first_frame * spf2 + first_samp * spf2 / spf1);

      /* Allocate a temporary buffer for the next field */
      tmpbuf = _GD_Alloc(D, return_type, num_samp2);

      if (D->error != GD_E_OK)
        return 0;

      /* read the next field */
      n_read2 = _GD_DoField(D, L->in_fields[i], 0, first_samp2, 0, num_samp2,
          return_type, tmpbuf);
      D->recurse_level--;

      if (D->error != GD_E_OK) {
        free(tmpbuf);
        return 0;
      }

      _GD_ScaleData(D, tmpbuf, return_type, n_read2, L->m[i], L->b[i]);

      if (D->error != GD_E_OK) {
        free(tmpbuf);
        return 0;
      }

      if (n_read2 > 0 && n_read2 * spf1 != n_read * spf2)
        n_read = n_read2 * spf1 / spf2;

      _GD_AddData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);

      free(tmpbuf);
    }
  }

  return n_read;
}

/* _GD_DoMultiply:  Read from a multiply.  Returns number of samples read.
*/
static int _GD_DoMultiply(DIRFILE *D, struct MultiplyEntryType* M,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int spf1, spf2, num_samp2, first_samp2;
  int n_read, n_read2;

  D->recurse_level++;

  /* find the samples per frame of the first field */
  spf1 = _GD_GetSPF(M->in_fields[0], D);
  if (D->error != GD_E_OK)
    return 0;

  /* read the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, M->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, return_type, data_out);

  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  /* Nothing to multiply */
  if (n_read == 0)
    return 0;

  D->recurse_level++;

  /* find the samples per frame of the second field */
  spf2 = _GD_GetSPF(M->in_fields[1], D);
  if (D->error != GD_E_OK)
    return 0;

  /* calculate the first sample and number of samples to read of the
   * second field */
  num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
  first_samp2 = (first_frame * spf2 + first_samp * spf2 / spf1);

  /* Allocate a temporary buffer for the second field */
  tmpbuf = _GD_Alloc(D, return_type, num_samp2);

  if (D->error != GD_E_OK)
    return 0;

  /* read the second field */
  n_read2 = _GD_DoField(D, M->in_fields[1], 0, first_samp2, 0, num_samp2,
      return_type, tmpbuf);
  D->recurse_level--;
  if (D->error != GD_E_OK) {
    free(tmpbuf);
    return 0;
  }

  if (n_read2 > 0 && n_read2 * spf1 < n_read * spf2)
    n_read = n_read2 * spf1 / spf2;

  _GD_MultiplyData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);

  free(tmpbuf);

  return n_read;
}

/* _GD_DoBit:  Read from a bitfield.  Returns number of samples read.
*/
static int _GD_DoBit(DIRFILE *D, struct BitEntryType *B,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t return_type, void *data_out)
{
  uint64_t *tmpbuf;
  int i;
  int spf;
  int ns;
  int n_read;

  const uint64_t mask = (B->numbits == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << B->numbits) - 1;

  /*****************************************/
  /** if we got here, we found the field! **/
  D->recurse_level++;
  spf = _GD_GetSPF(B->raw_field, D);
  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  ns = num_samp + num_frames * spf;
  tmpbuf = (uint64_t *)malloc(ns * sizeof(uint64_t));
  if (tmpbuf == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return 0;
  }

  D->recurse_level++;
  n_read = _GD_DoField(D, B->raw_field, first_frame, first_samp,
      num_frames, num_samp, GD_UINT64, tmpbuf);
  D->recurse_level--;

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    return 0;
  }

  for (i = 0; i < n_read; i++)
    tmpbuf[i] = (tmpbuf[i] >> B->bitnum) & mask;

  _GD_ConvertType(D, tmpbuf, GD_UINT64, data_out, return_type, n_read);
  free(tmpbuf);

  return n_read;
}

/* _GD_DoPhase:  Read from a phase.  Returns number of samples read.
*/
static int _GD_DoPhase(DIRFILE *D, struct PhaseEntryType *P,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t return_type, void *data_out)
{
  int n_read;

  D->recurse_level++;
  n_read = _GD_DoField(D, P->raw_field, first_frame, first_samp + P->shift,
      num_frames, num_samp, return_type, data_out);
  D->recurse_level--;

  return n_read;
}

/* _GD_MakeDummyLinterp: Make an empty linterp
*/
static void _GD_MakeDummyLinterp(DIRFILE* D, struct LinterpEntryType *E)
{
  E->n_interp = 2;
  E->x = (double *)malloc(2*sizeof(double));
  E->y = (double *)malloc(2*sizeof(double));

  if (E->x == NULL || E->y == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return;
  }

  E->x[0] = 0;
  E->y[0] = 0;
  E->x[1] = 1;
  E->y[1] = 1;
}

/* _GD_ReadLinterpFile: Read in the linterp data for this field
*/
void _GD_ReadLinterpFile(DIRFILE* D, struct LinterpEntryType *E)
{
  FILE *fp;
  int i;
  char line[255];
  int linenum = 0;

  fp = fopen(E->linterp_file, "r");
  if (fp == NULL) {
    _GD_MakeDummyLinterp(D, E);
    _GD_SetGetDataError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_OPEN, NULL, 0,
        E->linterp_file);
    return;
  }

  /* first read the file to see how big it is */
  i = 0;
  while (_GD_GetLine(fp, line, &linenum))
    i++;

  if (i < 2) {
    _GD_MakeDummyLinterp(D, E);
    _GD_SetGetDataError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, NULL, 0,
        E->linterp_file);
    return;
  }

  E->n_interp = i;
  E->x = (double *)malloc(i * sizeof(double));
  E->y = (double *)malloc(i * sizeof(double));
  if (E->x == NULL || E->y == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return;
  }

  /* now read in the data */
  rewind(fp);
  linenum = 0;
  for (i = 0; i < E->n_interp; i++) {
    _GD_GetLine(fp, line, &linenum);
    sscanf(line, "%lg %lg",&(E->x[i]), &(E->y[i]));
  }

  _GD_ClearGetDataError(D);
}

/* _GD_GetIndex: get LUT index.
*/
static int _GD_GetIndex(double x, double lx[], int idx, int n)
{
  /* Just linearly search - we're probably right to start    */
  /* increment until we are bigger */
  while ((idx < n - 2) && (x > lx[idx]))
    idx++;

  /* decrement until we are smaller */
  while ((idx > 0) && (x < lx[idx]))
    idx--;

  return idx;
}

/* _GD_LinterpData: calibrate data using lookup table lx and ly
*/
void _GD_LinterpData(DIRFILE* D, const void *data, gd_type_t type, int npts,
    double *lx, double *ly, int n_ln)
{
  int i, idx = 0;
  double x;

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
        x = ((int64_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int64_t *)data)[i] = (int64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++) {
        x = ((uint64_t *)data)[i];
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
      _GD_SetGetDataError(D, GD_E_BAD_RETURN_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
}

/* _GD_DoLinterp:  Read from a linterp.  Returns number of samples read.
*/
static int _GD_DoLinterp(DIRFILE *D, struct LinterpEntryType* I,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t return_type, void *data_out)
{
  int n_read = 0;

  if (I->n_interp < 0) {
    _GD_ReadLinterpFile(D, I);
    if (D->error != GD_E_OK)
      return 0;
  }

  D->recurse_level++;
  n_read = _GD_DoField(D, I->raw_field, first_frame, first_samp,
      num_frames, num_samp, return_type, data_out);
  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  _GD_LinterpData(D, data_out, return_type, n_read, I->x, I->y, I->n_interp);

  return n_read;
}

/* _GD_DoField: Locate the field in the database and read it.
*/
int _GD_DoField(DIRFILE *D, const char *field_code, int first_frame,
    int first_samp, int num_frames, int num_samp, gd_type_t return_type,
    void *data_out)
{
  int n_read = 0;
  struct gd_entry_t* entry;

  if (D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetGetDataError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    return 0;
  }

  /********************************************/
  /* if Asking for "FILEFRAM" or "INDEX", just return it */
  if ((strcmp(field_code,"FILEFRAM") == 0) ||
      (strcmp(field_code,"INDEX") == 0)) {
    n_read = num_frames + num_samp;
    if (data_out != NULL) {
      _GD_FillFileFrame(data_out, return_type, first_frame + first_samp +
          D->frame_offset, n_read);
    }
    _GD_ClearGetDataError(D);
    return n_read;
  }

  /* Find the field */
  entry = _GD_FindField(D, field_code);

  if (entry == NULL) { /* No match */
    _GD_SetGetDataError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    return 0;
  }

  switch (entry->field_type) {
    case GD_RAW_ENTRY:
      return _GD_DoRaw(D, ENTRY(Raw, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_LINTERP_ENTRY:
      return _GD_DoLinterp(D, ENTRY(Linterp, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_LINCOM_ENTRY:
      return _GD_DoLincom(D, ENTRY(Lincom, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_BIT_ENTRY:
      return _GD_DoBit(D, ENTRY(Bit, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_MULTIPLY_ENTRY:
      return _GD_DoMultiply(D, ENTRY(Multiply, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_PHASE_ENTRY:
      return _GD_DoPhase(D, ENTRY(Phase, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
  }

  /* Can't get here */
  _GD_SetGetDataError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL);
  return 0;
}

/***************************************************************************/
/*                                                                         */
/*  getdata: read BLAST format files.  This function is little more than   */
/*    a public wrapper for _GD_DoField                                     */
/*                                                                         */
/*    filename_in: the name of the file directory (raw files are in here)  */
/*    field_code: the name of the field you want to read                   */
/*    first_frame, first_samp: the first sample read is                    */
/*              first_samp + samples_per_frame*first_frame                 */
/*    num_frames, num_samps: the number of samples read is                 */
/*              num_samps + samples_per_frame*num_frames                   */
/*    return_type: data type of *data_out:                                 */
/*           GD_INT8,  GD_INT16,   GD_INT32,   GD_INT64                    */
/*          GD_UINT8, GD_UINT16,  GD_UINT32,  GD_UINT64                    */
/*                               GD_FLOAT32, GD_FLOAT64                    */
/*    void *data_out: array to put the data                                */
/*    *error_code: error code is returned here.                            */
/*                                                                         */
/*    return value: returns number of samples actually read into data_out  */
/*                                                                         */
/***************************************************************************/
int getdata(DIRFILE* D, const char *field_code, int first_frame, int first_samp,
    int num_frames, int num_samp, gd_type_t return_type, void *data_out)
{
  if (!D || (D->flags & GD_INVALID)) {/* don't crash */
    _GD_SetGetDataError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    return 0;
  }

  _GD_ClearGetDataError(D);

  first_frame -= D->frame_offset;

  /* Get rid of GD_INT32_ALT right away to avoid having to carry it around */
  if (return_type == GD_INT32_ALT)
    return_type = GD_INT32;

  return _GD_DoField(D, field_code, first_frame, first_samp, num_frames,
      num_samp, return_type, data_out);
}

/***************************************************************************/
/*                                                                         */
/*    Get the number of frames available                                   */
/*                                                                         */
/***************************************************************************/
unsigned long get_n_frames(DIRFILE* D)
{
  char raw_data_filename[FILENAME_MAX];
  struct stat statbuf;
  unsigned long nf;

  if (!D || (D->flags & GD_INVALID)) {/* don't crash */
    _GD_SetGetDataError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    return 0;
  }

  if (D->first_field == NULL) {
    _GD_SetGetDataError(D, GD_E_NO_RAWFIELDS, 0, NULL, 0, NULL);
    return 0;
  }

  /* load the first valid raw field */
  snprintf(raw_data_filename, FILENAME_MAX, "%s/%s", D->name,
      ENTRY(Raw, D->first_field)->file);
  if (stat(raw_data_filename, &statbuf) < 0) {
    _GD_SetGetDataError(D, GD_E_OPEN_RAWFIELD, 0, NULL, 0, NULL);
    return 0;
  }

  nf = statbuf.st_size / (ENTRY(Raw, D->first_field)->size *
      ENTRY(Raw, D->first_field)->samples_per_frame);
  nf += D->frame_offset;

  return nf;
}

/***************************************************************************/
/*                                                                         */
/*    Get the number of samples for each frame for the given field         */
/*                                                                         */
/***************************************************************************/
unsigned int get_samples_per_frame(DIRFILE* D, const char *field_code)
{
  if (!D || (D->flags & GD_INVALID)) {/* don't crash */
    _GD_SetGetDataError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    return 0;
  }

  _GD_ClearGetDataError(D);

  return _GD_GetSPF(field_code, D);
}
/* vim: ts=2 sw=2 et tw=80
*/
