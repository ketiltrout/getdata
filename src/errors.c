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
#include "internal.h"

#ifdef STDC_HEADERS
#include <string.h>
#endif

/* Error strings. */
static const struct {
  int error;
  int suberror; /* 0 = any */
  const char* format; /* 1 = suberror, 2 = file, 3 = line, 4 = string */
  int adderr; /* 1 = append strerror(line), 2 = append sterror(suberror) */
} error_string[] = {
  /* GD_E_OPEN: 1 = suberror, 2 = filename */
  { GD_E_OPEN, GD_E_OPEN_NOT_DIRFILE, "Not a dirfile: {2}", 0 },
  { GD_E_OPEN, GD_E_OPEN_NO_ACCESS,
    "Cannot open dirfile {2}: permission denied", 0 },
  { GD_E_OPEN, GD_E_OPEN_NOT_EXIST, "Dirfile does not exist: {2}", 0 },
  /* GD_E_FORMAT: 1 = suberror, 2 = formatfile, 3 = line number, 4 = token */
  { GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE,
    "Bad RAW field type on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BAD_SPF,
    "Samples per frame out of range on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_N_FIELDS,
    "LINCOM field count out of range on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_N_COLS, "Missing column on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_NUMBITS,
    "Numbits out of range on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BITNUM,
    "Starting bit out of range on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BITSIZE,
    "End of bitfield out of bounds on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_CHARACTER,
    "Invalid character on line {3} or {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, "Line {3} of {2} indecipherable", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_RES_NAME,
    "Field name is reserved on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_ENDIAN,
    "Unrecognised endianness on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BAD_NAME,
    "Bad field name on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_UNTERM,
    "Unterminated token on line {3} of {2}", 0 },
  /* GD_E_CREAT: 1 = suberror, 2 = filename. 3 = errno */
  { GD_E_TRUNC, 0, "Error truncating {2}: ", 1 },
  /* GD_E_CREAT: 1 = suberror, 2 = filename, 3 = errno */
  { GD_E_CREAT, GD_E_CREAT_DIR, "Unable to create directory {2}: ", 1 },
  { GD_E_CREAT, GD_E_CREAT_FORMAT, "Unable to create format file {2}: ", 1 },
  { GD_E_CREAT, GD_E_CREAT_EXCL,
    "Unable to create dirfile {2}: already exists", 1 },
  /* GD_E_BAD_CODE: 4 = field code */
  { GD_E_BAD_CODE, 0, "Field not found: {4}", 0 },
  /* GD_E_BAD_TYPE: 1 = data type */
  { GD_E_BAD_TYPE, 0, "Unsupported data type: {1}", 0 },
  /* GD_E_RAW_IO: 1 = suberror, 3 = errno */
  { GD_E_RAW_IO, 0, "Error accessing {2}: ", 1 },
  /* GD_E_OPEN_INCLUDE: 1 = errno, 2 = format file, 3 = line, 4 = includefile */
  { GD_E_OPEN_INCLUDE, 0,
    "Unable to open INCLUDEd file {4} on line {3} of {2}: ", 2 },
  /* GD_E_INTERNAL_ERROR: 2 = source file, 3 = line */
  { GD_E_INTERNAL_ERROR, 0, "Internal error at [{2},{3}]; "
    "please report to " PACKAGE_BUGREPORT , 0 },
  /* GD_E_EMPTY: (nothing) */
  { GD_E_EMPTY, 0, "Unable to query dirfile: no RAW field available", 0 },
  /* GD_E_ALLOC: (nothing) */
  { GD_E_ALLOC, 0, "Memory allocation error", 0 },
  /* GD_E_RANGE: (nothing) */
  { GD_E_RANGE, 0, "Request out of range", 0 },
  /* GD_E_OPEN_LINFILE: 1 = suberror, 2 = errno, 4 = lutfile */
  { GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, "LINTERP table {4} too short", 0 },
  { GD_E_OPEN_LINFILE, 0, "Error opening LINTERP table {4}: ", 2 },
  /* GD_E_RECURSE_LEVEL: 4 = fieldcode */
  { GD_E_RECURSE_LEVEL, 0, "Recursion too deep resolving field {4}", 0 },
  /* GD_E_BAD_DIRFILE: (nothing) */
  { GD_E_BAD_DIRFILE, 0, "Invalid dirfile", 0 },
  /* GD_E_BAD_FIELD_TYPE: 4 = fieldcode */
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, "No method to write field {4}", 0 },
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_GET, "Bad field type for field {4}", 0 },
  /* GD_E_ACCMODE: (nothing) */
  { GD_E_ACCMODE, 0, "Dirfile has been opened read-only", 0 },
  /* GD_E_BAD_ENTRY: 3 = parameter */
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_TYPE, "Invalid entry type: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_FORMAT, "Invalid format file index: {3}",
    0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_SPF, "Samples per frame out of range: {3}",
    0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS,
    "LINCOM field count out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITNUM, "Starting bit out of range: {3}",
    0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NUMBITS, "Numbits out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITSIZE,
    "End of bitfield out of range: {3}", 0 },
  /* GD_E_DUPLICATE 4 = name */
  { GD_E_DUPLICATE, 0, "Field code already present: {4}", 0 },
  /* GD_E_OK: (nothing) */
  { 0, 0, "Success", 0} /* this must be the last error string defined */
};

/* _GD_SetError: Sets the global error variables for a library error
*/
void _GD_SetError(DIRFILE* D, int error, int suberror,
    const char* format_file, int line, const char* token)
{
  dtrace("%p, %i, %i, \"%s\", %i, \"%s\"", D, error, suberror, format_file,
      line, token);

  *D->user_error = D->error = error;
  D->suberror = suberror;
  D->error_line = line;
  if (format_file != NULL)
    strncpy(D->error_file, format_file, FILENAME_MAX);
  if (token != NULL)
    strncpy(D->error_string, token, FILENAME_MAX);

  dreturnvoid();
}

/* Return the error */
int get_error(const DIRFILE* D)
{
  dtrace("%p", get_error);

  dreturn("%i", D->error);

  return D->error;
}

/* Set the user's error location */
void dirfile_user_error(DIRFILE* D, int* ptr)
{
  dtrace("%p, %p", D, ptr);

  if (ptr == NULL)
    D->user_error = &(D->ierror);
  else
    D->user_error = ptr;

  dreturnvoid();
}


/* Write a descriptive message in the supplied buffer describing the last
 * library error.  The message may be truncated but will be null terminated.
 * Returns buffer, or NULL if buflen < 1.
 */
char* get_error_string(const DIRFILE* D, char* buffer, size_t buflen)
{
  const char* ip;
  char* op = buffer;
  char* bufend = buffer + buflen;
  int i, s = -1;

  dtrace("%p, %p, %zi", D, buffer, buflen);

  /* Sanity check */
  if (buffer == NULL || D == NULL || buflen < 1) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* Find the error message */
  for (i = 0; s == -1; ++i) {
    if ((error_string[i].error == D->error) && ((error_string[i].suberror == 0)
          || (error_string[i].suberror == D->suberror)))
      s = i;
    else if (error_string[i].error == 0)
      break;
  }

  if (s == -1) /* Unhandled error */
    snprintf(buffer, buflen, "Unknown error %i:%i", D->error, D->suberror);
  else {
    for (ip = error_string[s].format; *ip != '\0' && op < bufend - 1; ++ip) {
      if (*ip == '{') {
        ip++;
        if (*ip == '1')
          op += snprintf(op, bufend - op, "%02x", D->suberror);
        else if (*ip == '2')
          op += snprintf(op, bufend - op, "%s", D->error_file);
        else if (*ip == '3')
          op += snprintf(op, bufend - op, "%i", D->error_line);
        else if (*ip == '4')
          op += snprintf(op, bufend - op, "%s", D->error_string);
        ip++;
      } else
        *(op++) = *ip;
    }

    *op = '\0';
    if (op < bufend - 1 && error_string[s].adderr)
      strerror_r((error_string[s].adderr == 2) ? D->suberror : D->error_line,
          op, bufend - op);
  }

  dreturn("\"%s\"", buffer);
  return buffer;
}
/* vim: ts=2 sw=2 et tw=80
*/
