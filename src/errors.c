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
#include <string.h>
#endif

#include "getdata_internal.h"

const char const*GD_ERROR_CODES[GD_N_ERROR_CODES] = {
  "Success",
  "Error opening dirfile",
  "Error in Format file",
  "Error truncating dirfile",
  "Error creating dirfile",
  "Field code not found in File Format",
  "Unrecognized data type",
  "I/O error accessing field file",
  "Could not open included Format file",
  "Internal error",
  "Memory allocation failed",
  "No RAW fields defined",
  "Could not open interpolation file",
  "Too many levels of recursion",
  "Bad DIRFILE",
  "Cannot write to specified field",
  "Read-only dirfile",
  "Request out-of-range"
};

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
char* getdata_error_string(const DIRFILE* D, char* buffer, size_t buflen)
{
  char* ptr;

  /* Sanity check */
  if (buffer == NULL || D == NULL || buflen < 1)
    return NULL;

  /* Copy the default error message into the buffer and make sure
   * the result is null terminated */
  strncpy(buffer, GD_ERROR_CODES[D->error], buflen - 1);
  buffer[buflen - 1] = 0;

  /* point to the end of the string and reduce buflen appropriately */
  ptr = buffer + strlen(buffer);
  buflen -= strlen(buffer);

  /* add the ancillary data - we use snprintfs here to ensure the resultant
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
    case GD_E_CREAT: /* couldn't create the dirfile */
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
          snprintf(ptr, buflen, "line indecipherable");
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
    case GD_E_BAD_TYPE: /* unsupported data type */
      snprintf(ptr, buflen, ": 0x%02x", D->suberror);
      break;
    case GD_E_RECURSE_LEVEL: /* recursion too deep -- report field name for
                                which this happened */
      snprintf(ptr, buflen, " while resolving field %s", D->error_string);
      break;
    case GD_E_BAD_CODE: /* A required field name wasn't defined */
    case GD_E_RAW_IO:   /* A raw field file couldn't be opened */
      snprintf(ptr, buflen, ": %s", D->error_string);
      break;
    case GD_E_OPEN_LINFILE: /* problems with LINTERPs: report the linterp
                               filename with the error message */
      snprintf(ptr, buflen, " %s: %s", D->error_string,
          (D->suberror == GD_E_LINFILE_OPEN) ? "open failed"
          : "file too short");
      break;
    case GD_E_EMPTY: /* couldn't find the first RAW file */
      snprintf(ptr, buflen, ": %s", "no RAW fields available");
      break;
  }

  return buffer;
}
/* vim: ts=2 sw=2 et tw=80
 */
