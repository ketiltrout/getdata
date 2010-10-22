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
  { GD_E_FORMAT, GD_E_FORMAT_N_TOK, "Missing token on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_NUMBITS,
    "Numbits out of range on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BITNUM,
    "Starting bit out of range on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BITSIZE,
    "End of bitfield out of bounds on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_CHARACTER,
    "Invalid character on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, "Line {3} of {2} indecipherable", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_RES_NAME,
    "Field name is reserved on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_ENDIAN,
    "Unrecognised endianness on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_BAD_NAME,
    "Bad field name on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_UNTERM,
    "Unterminated token on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_METARAW,
    "Invalid metafield type on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_NO_PARENT,
    "Meta field defined before parent ({4}) on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_DUPLICATE,
    "Field code on line {3} of {2} already defined: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_LOCATION,
    "META in a different file than parent ({4}) on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_PROTECT,
    "Bad protection level on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_LITERAL,
    "Unexpected characters in scalar literal ({4}) on line {3} of {2}", 0 },
  /* GD_E_TRUNC: 1 = suberror, 2 = filename. 3 = errno */
  { GD_E_TRUNC, 0, "Error truncating {2}: ", 1 },
  /* GD_E_CREAT: 1 = suberror, 2 = filename, 3 = errno */
  { GD_E_CREAT, GD_E_CREAT_DIR, "Unable to create directory {2}: ", 1 },
  { GD_E_CREAT, GD_E_CREAT_FORMAT, "Unable to create format file {2}: ", 1 },
  { GD_E_CREAT, GD_E_CREAT_EXCL, "Unable to create dirfile {2}: already exists",
    1 },
  /* GD_E_BAD_CODE: 4 = field code */
  { GD_E_BAD_CODE, 0, "Field not found: {4}", 0 },
  /* GD_E_BAD_TYPE: 1 = data type */
  { GD_E_BAD_TYPE, 0, "Unsupported data type: {1}", 0 },
  /* GD_E_RAW_IO: 1 = suberror, 2 = filename, 3 = errno */
  { GD_E_RAW_IO, 0, "Error accessing {2}: ", 1 },
  /* GD_E_OPEN_FRAGMENT: 1 = errno, 2 = format file, 3 = line, 4 = includefile*/
  { GD_E_OPEN_FRAGMENT, 0,
    "Unable to open fragment {4} on line {3} of {2}: ", 2 },
  /* GD_E_INTERNAL_ERROR: 2 = source file, 3 = line */
  { GD_E_INTERNAL_ERROR, 0, "Internal error at [{2},{3}]; "
    "please report to " PACKAGE_BUGREPORT , 0 },
  /* GD_E_ALLOC: (nothing) */
  { GD_E_ALLOC, 0, "Memory allocation error", 0 },
  /* GD_E_RANGE: (nothing) */
  { GD_E_RANGE, GD_E_OUT_OF_RANGE, "Request out of range", 0 },
  { GD_E_RANGE, GD_E_SINGULAR_RANGE, "Singular range", 0 },
  /* GD_E_OPEN_LINFILE: 1 = suberror, 2 = errno, 4 = lutfile */
  { GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, "LINTERP table {4} too short", 0 },
  { GD_E_OPEN_LINFILE, 0, "Error opening LINTERP table {4}: ", 2 },
  /* GD_E_RECURSE_LEVEL: 4 = fieldcode */
  { GD_E_RECURSE_LEVEL, 0, "Recursion too deep resolving field {4}", 0 },
  /* GD_E_BAD_DIRFILE: (nothing) */
  { GD_E_BAD_DIRFILE, 0, "Invalid dirfile", 0 },
  /* GD_E_BAD_FIELD_TYPE: 4 = fieldcode */
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, "No method to write field {4}", 0 },
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, "Invalid field type for {4}", 0 },
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_MATCH, "Field type mismatch for {4}", 0 },
  /* GD_E_ACCMODE: (nothing) */
  { GD_E_ACCMODE, 0, "Dirfile has been opened read-only", 0 },
  /* GD_E_UNSUPPORTED: (nothing) */
  { GD_E_UNSUPPORTED, 0, "Operation not supported by current encoding scheme",
    0 },
  /* GD_E_UNKNOWN_ENCODING: (nothing) */
  { GD_E_UNKNOWN_ENCODING, 0, "Unable to determine encoding scheme", 0 },
  /* GD_E_BAD_ENTRY: 3 = parameter */
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_TYPE, "Invalid entry type: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_SPF, "Samples per frame out of range: {3}",
    0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS,
    "LINCOM field count out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITNUM, "Starting bit out of range: {3}",
    0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NUMBITS, "Numbits out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITSIZE,
    "End of bitfield out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_METARAW, "Invalid metafield type: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_POLYORD, "POLYNOM order out of range: {3}",
    0 },
  /* GD_E_DUPLICATE: 4 = name */
  { GD_E_DUPLICATE, 0, "Field code already present: {4}", 0 },
  /* GD_E_DIMENSION: 2 = parent field (if any), 4 = field code */
  { GD_E_DIMENSION, GD_E_DIM_FORMAT, "Scalar field {4} found where vector "
    "field expected in definition of {2}", 0 },
  { GD_E_DIMENSION, GD_E_DIM_CALLER,
    "Vector field expected, but scalar field given: {4}", 0 },
  /* GD_E_BAD_INDEX: 3 = index */
  { GD_E_BAD_INDEX, 0, "Invalid format file index: {3}", 0 },
  /* GD_E_BAD_SCALAR: 2 = parent field, 4 = scalar field */
  { GD_E_BAD_SCALAR, GD_E_SCALAR_CODE,
    "Scalar field {4} not found in definition of {2}", 0 },
  { GD_E_BAD_SCALAR, GD_E_SCALAR_TYPE,
    "Scalar field {4} has wrong type in definition of {2}", 0 },
  /* GD_E_BAD_REFERENCE: 4 = field name */
  { GD_E_BAD_REFERENCE, GD_E_REFERENCE_CODE,
    "REFERENCE field code not found: {4}", 0 },
  { GD_E_BAD_REFERENCE, GD_E_REFERENCE_TYPE,
    "Bad field type for REFERENCE field: {4}", 0 },
  /* GD_E_PROTECTED: 4 = fragment name */
  { GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, "Fragment is protected: {4}", 0 },
  { GD_E_PROTECTED, GD_E_PROTECTED_DATA, "Data for fragment is protected: {4}",
    0 },
  /* GD_E_DELETE: 2 = client field, 4 = deleted field */
  { GD_E_DELETE, GD_E_DEL_META, "Cannot delete field {4} with metafields", 0 },
  { GD_E_DELETE, GD_E_DEL_CONST,
    "Cannot delete field {4} used in definiton of field {2}", 0 },
  { GD_E_DELETE, GD_E_DEL_DERIVED,
    "Cannot delete field {4} used as input to field {2}", 0 },
  /* GD_E_BAD_ENDIANNESS: (nothing) */
  { GD_E_BAD_ENDIANNESS, 0, "Invalid endianness specified", 0 },
  /* GD_E_CALLBACK: 3 = response */
  { GD_E_CALLBACK, 0, "Unrecognised response from callback function: {3}", 0 },
  /* GD_E_BAD_PROTECTION: (nothing) */
  { GD_E_BAD_PROTECTION, 0, "Invalid protection level specified", 0 },
  /* GD_E_UNCLEAN_DB: 3 = fragment */
  { GD_E_UNCLEAN_DB, 0,
    "Unexpected system error processing {3}; database unclean", 0 },
  /* GD_E_DOMAIN: (nothing) */
  { GD_E_DOMAIN, GD_E_DOMAIN_COMPLEX, "Improper domain: complex valued", 0 },
  { GD_E_DOMAIN, GD_E_DOMAIN_EMPTY, "Improper domain: empty set", 0 },
  { GD_E_DOMAIN, GD_E_DOMAIN_ANTITONIC, "Improper domain: not monotonic", 0 },
  /* GD_E_UNCLEAN_DB: 4 = repr */
  { GD_E_BAD_REPR, GD_E_REPR_UNKNOWN, "Unknown field representation: .{4}", 0 },
  { GD_E_BAD_REPR, GD_E_REPR_PUT, "Unable to write to field reprentation: .{4}",
    0 },
  /* GD_E_BAD_VERSION 3 = version */
  { GD_E_BAD_VERSION, GD_E_VERS_NONE,
    "Dirfile conforms to no Standards Version", 0 },
  { GD_E_BAD_VERSION, GD_E_VERS_MISSING,
    "Dirfile does not conform to Standards Version {3}", 0 },
  /* GD_E_FLUSH: 3 = suberror, 4 = filename */
  { GD_E_FLUSH, GD_E_FLUSH_MKTMP, "I/O error creating temporary file: ", 1 },
  { GD_E_FLUSH, GD_E_FLUSH_OPEN, "I/O error opening temporary file: ", 1 },
  { GD_E_FLUSH, GD_E_FLUSH_RENAME, "I/O error replacing format metadata: ", 1 },
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

  D->error = error;
  D->suberror = suberror;
  D->error_line = line;
  if (format_file != NULL)
    strncpy(D->error_file, format_file, FILENAME_MAX);
  if (token != NULL)
    strncpy(D->error_string, token, FILENAME_MAX);

  if (D->flags & GD_VERBOSE) {
    char buffer[GD_MAX_LINE_LENGTH];
    fprintf(stderr, PACKAGE_NAME ": %s\n", gd_error_string(D, buffer,
          GD_MAX_LINE_LENGTH));
  }

  dreturnvoid();
}

/* Return the error */
int gd_error(const DIRFILE* D) gd_nothrow
{
  dtrace("%p", D);

  dreturn("%i", D->error);

  return D->error;
}

/* Write a descriptive message in the supplied buffer describing the last
 * library error.  The message may be truncated but will be null terminated.
 * Returns buffer, or NULL if buflen < 1.
 */
char* gd_error_string(const DIRFILE* D, char* buffer, size_t buflen) gd_nothrow
{
  const char* ip;
  char* op = buffer;
  char* bufend = buffer + buflen;
  int i, s = -1;

  dtrace("%p, %p, %zu", D, buffer, buflen);

  /* Sanity check */
  if (buffer == NULL || D == NULL || buflen < 1) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* Find the error message */
  for (i = 0; error_string[i].error; ++i)
    if ((error_string[i].error == D->error) && ((error_string[i].suberror == 0)
          || (error_string[i].suberror == D->suberror))) {
      s = i;
      break;
    }

  if (D->error == 0)
    s = i;

  if (s == -1) /* Unhandled error */
    snprintf(buffer, buflen, "Unknown error %i:%i. Please report to "
        PACKAGE_BUGREPORT, D->error, D->suberror);
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
    if (op < bufend - 1 && error_string[s].adderr) {
#ifdef STRERROR_R_CHAR_P
      char *ptr = strerror_r((error_string[s].adderr == 2) ? D->suberror :
          D->error_line, op, bufend - op);
      if (ptr != op)
        strncpy(op, ptr, bufend - op);
#else
      strerror_r((error_string[s].adderr == 2) ? D->suberror : D->error_line,
          op, bufend - op);
#endif
    }
  }

  dreturn("\"%s\"", buffer);
  return buffer;
}
/* vim: ts=2 sw=2 et tw=80
*/
