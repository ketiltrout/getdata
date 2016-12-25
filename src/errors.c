/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2016 D. V. Wiebe
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

/* Error strings. */
static const struct {
  int error;
  int suberror; /* 0 = any */
  const char* format; /* 1 = suberror, 2 = file, 3 = line, 4 = string */
  int adderr; /* 1 = append strerror(errno) */
} error_string[] = {
  /* GD_E_FORMAT: 1 = suberror, 2 = formatfile, 3 = line number, 4 = token */
  { GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, "Bad data type on line {3} of {2}: {4}",
    0 },
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
  { GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, "Bad name on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_UNTERM,
    "Unterminated token on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_METARAW,
    "Invalid metafield type on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_NO_FIELD,
    "Field code not found on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_DUPLICATE,
    "Field code on line {3} of {2} already defined: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_LOCATION,
    "META in a different file than parent ({4}) on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_PROTECT,
    "Bad protection level on line {3} of {2}: {4}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_LITERAL,
    "Unexpected characters in scalar literal ({4}) on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_WINDOP,
    "Unrecognised operator ({4}) on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_META_META,
    "Cannot attach meta field to meta field {4} on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_ALIAS,
    "Cannot use alias {4} as parent to a meta field on line {3} of {2}", 0 },
  { GD_E_FORMAT, GD_E_FORMAT_MPLEXVAL,
    "Bad MPLEX period ({4}) on line {3} of {2}", 0 },
  /* GD_E_CREAT: 1 = suberror, 2 = filename */
  { GD_E_CREAT, GD_E_CREAT_DIR, "Unable to create directory {2}: ", 1 },
  { GD_E_CREAT, GD_E_CREAT_OPEN, "Unable to open directory {2}: ", 1 },
  { GD_E_CREAT, GD_E_CREAT_FORMAT, "Unable to create format file {2}: ", 1 },
  /* GD_E_BAD_CODE: 4 = field code */
  { GD_E_BAD_CODE, GD_E_CODE_MISSING, "Field not found: {4}", 0 },
  { GD_E_BAD_CODE, GD_E_CODE_INVALID, "Bad field code: {4}", 0 },
  { GD_E_BAD_CODE, GD_E_CODE_AMBIGUOUS, "Ambiguous field code: {4}", 0 },
  { GD_E_BAD_CODE, GD_E_CODE_INVALID_NS, "Bad namespace: {4}", 0 },
  { GD_E_BAD_CODE, GD_E_CODE_REPR, "Invalid representation suffix in: {4}", 0 },
  /* GD_E_BAD_TYPE: 1 = suberror, 3 = data type */
  { GD_E_BAD_TYPE, GD_E_TYPE_NULL, "Bad data type: GD_NULL not allowed", 0 },
  { GD_E_BAD_TYPE, 0, "Bad data type: {3}", 0 },
  /* GD_E_IO: 2 = filename; 3 = line; 4 = included file/encoding error */
  { GD_E_IO, GD_E_IO_OPEN, "Error opening {2}: ", 1 },
  { GD_E_IO, GD_E_IO_READ, "Error reading {2}: ", 1 },
  { GD_E_IO, GD_E_IO_WRITE, "Error writing {2}: ", 1 },
  { GD_E_IO, GD_E_IO_CLOSE, "Error closing {2}: ", 1 },
  { GD_E_IO, GD_E_IO_UNLINK, "Error unlinking {2}: ", 1 },
  { GD_E_IO, GD_E_IO_RENAME, "Error renaming {2}: ", 1 },
  { GD_E_IO, GD_E_IO_INCL, "Error opening {4} included on line {3} of {2}: ", 
    1 },
  { GD_E_IO, GD_E_IO_ENC_OPEN, "Error opening {2}: {4}", 0 },
  { GD_E_IO, GD_E_IO_ENC_READ, "Error reading {2}: {4}", 0 },
  { GD_E_IO, GD_E_IO_ENC_WRITE, "Error writing {2}: {4}", 0 },
  { GD_E_IO, GD_E_IO_ENC_CLOSE, "Error closing {2}: {4}", 0 },
  { GD_E_IO, GD_E_IO_ENC_UNLINK, "Error unlinking {2}: {4} ", 0 },
  { GD_E_IO, GD_E_IO_ENC_RENAME, "Error renaming {2}: {4} ", 0 },
  { GD_E_IO, 0, "Error accessing {2}: ", 1 },
  /* GD_E_INTERNAL_ERROR: 2 = source file, 3 = line */
  { GD_E_INTERNAL_ERROR, 0, "Internal error at [{2},{3}]; "
    "please report to <" PACKAGE_BUGREPORT ">", 0 },
  /* GD_E_ALLOC: (nothing) */
  { GD_E_ALLOC, 0, "Memory allocation error", 0 },
  /* GD_E_RANGE: (nothing) */
  { GD_E_RANGE, GD_E_OUT_OF_RANGE, "Request out of range", 0 },
  { GD_E_RANGE, GD_E_SINGULAR_RANGE, "Singular range", 0 },
  /* GD_E_LUT: 1 = suberror, 2 = lutfile, 3 = line */
  { GD_E_LUT, GD_E_LUT_LENGTH, "LINTERP table {2} too short", 0 },
  { GD_E_LUT, GD_E_LUT_SYNTAX, "Malfomed data on line {3} of LINTERP table {2}",
    0 },
  /* GD_E_RECURSE_LEVEL: 2 = file; 3 = line; 4 = name */
  { GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE,
    "Recursion too deep resolving field {4}", 0 },
  { GD_E_RECURSE_LEVEL, GD_E_RECURSE_INCLUDE,
    "Recursion too deep including {4} on line {3} of {2}", 0 },
  /* GD_E_BAD_DIRFILE: (nothing) */
  { GD_E_BAD_DIRFILE, 0, "Invalid dirfile", 0 },
  /* GD_E_BAD_FIELD_TYPE: 2 = parent field (if any) 4 = fieldcode */
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, "No method to write field {4}", 0 },
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, "Invalid field type for {4}", 0 },
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_MATCH, "Field type mismatch for {4}", 0 },
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_FORMAT,
    "Bad field type for {4} in definition of {2}", 0 },
  { GD_E_BAD_FIELD_TYPE, GD_E_FIELD_STR, "Non-numeric data in {4}", 0 },
  /* GD_E_ACCMODE: (nothing) */
  { GD_E_ACCMODE, 0, "Dirfile has been opened read-only", 0 },
  /* GD_E_UNSUPPORTED: 4 = regex type */
  { GD_E_UNSUPPORTED, GD_E_SUPPORT_REGEX,
    "Specified regular expression grammar not supported by library: {4}", 0 },
  { GD_E_UNSUPPORTED, 0, "Operation not supported by current encoding scheme",
    0 },
  /* GD_E_UNKNOWN_ENCODING: (nothing) */
  { GD_E_UNKNOWN_ENCODING, GD_E_UNENC_UNDET,
    "Unable to determine encoding scheme", 0 },
  { GD_E_UNKNOWN_ENCODING, GD_E_UNENC_TARGET, "Unknown ouput encoding scheme",
    0 },
  /* GD_E_BAD_ENTRY: 3 = parameter */
  { GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, "Invalid entry type: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_SPF, "Samples per frame out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_NFIELDS, "LINCOM field count out of range: {3}",
    0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_BITNUM, "Starting bit out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_NUMBITS, "Numbits out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_BITSIZE, "End of bitfield out of range: {3}", 0},
  { GD_E_BAD_ENTRY, GD_E_ENTRY_METARAW, "Invalid metafield type: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_POLYORD, "POLYNOM order out of range: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_WINDOP, "Unrecognised WINDOW operator: {3}", 0 },
  { GD_E_BAD_ENTRY, GD_E_ENTRY_PERIOD, "MPLEX period out of range: {3}", 0 },
  /* GD_E_DUPLICATE: 4 = name */
  { GD_E_DUPLICATE, 0, "Field code already present: {4}", 0 },
  /* GD_E_DIMENSION: 2 = parent field (if any), 4 = field code */
  { GD_E_DIMENSION, GD_E_DIM_FORMAT, "Scalar field {4} found where vector "
    "field expected in definition of {2}", 0 },
  { GD_E_DIMENSION, GD_E_DIM_CALLER,
    "Vector field expected, but scalar field given: {4}", 0 },
  /* GD_E_BAD_INDEX: 3 = index */
  { GD_E_BAD_INDEX, 0, "Invalid fragment index: {3}", 0 },
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
  { GD_E_DELETE, GD_E_DEL_ALIAS, "Cannot delete field {4} with aliases", 0 },
  /* GD_E_ARGUMENT: 3 = offset; 4 = string */
  { GD_E_ARGUMENT, GD_E_ARG_WHENCE, "Invalid origin specified", 0 },
  { GD_E_ARGUMENT, GD_E_ARG_ENDIANNESS, "Invalid endianness specified", 0 },
  { GD_E_ARGUMENT, GD_E_ARG_PROTECTION, "Invalid protection level specified",
    0 },
  { GD_E_ARGUMENT, GD_E_ARG_NODATA, "No data", 0 },
  { GD_E_ARGUMENT, GD_E_ARG_NO_VERS, "Dirfile conforms to no Standards Version",
    0 },
  { GD_E_ARGUMENT, GD_E_ARG_BAD_VERS,
    "Dirfile does not conform to specified Standards Version", 0 },
  { GD_E_ARGUMENT, GD_E_ARG_REGEX, "Bad regular expression: {4}", 0},
  { GD_E_ARGUMENT, GD_E_ARG_PCRE, "Bad regular expression at offset {3}: {4}",
    0},
  { GD_E_ARGUMENT, 0, "Bad argument", 0 },
  /* GD_E_CALLBACK: 3 = response */
  { GD_E_CALLBACK, 0, "Unrecognised response from callback function: {3}", 0 },
  /* GD_E_ExISTS: (nothing) */
  { GD_E_EXISTS, 0, "Dirfile exists", 0 },
  /* GD_E_UNCLEAN_DB: 2 = fragment, 4 = call */
  { GD_E_UNCLEAN_DB, GD_E_UNCLEAN_CALL,
    "Unexpected system error processing {2}; database unclean: {4}: ", 1 },
  { GD_E_UNCLEAN_DB, 0,
    "Unexpected system error processing {2}; database unclean", 0 },
  /* GD_E_DOMAIN: (nothing) */
  { GD_E_DOMAIN, GD_E_DOMAIN_COMPLEX, "Improper domain: complex valued", 0 },
  { GD_E_DOMAIN, GD_E_DOMAIN_EMPTY, "Improper domain: empty set", 0 },
  { GD_E_DOMAIN, GD_E_DOMAIN_ANTITONIC, "Improper domain: not monotonic", 0 },
  { GD_E_DOMAIN, GD_E_DOMAIN_MULTIPOS, "I/O position mismatch in inputs", 0 },
  /* GD_E_BOUNDS: (nothing) */
  { GD_E_BOUNDS, 0, "Array length out of bounds", 0 },
  /* GD_E_LINE_TOO_LONG */
  { GD_E_LINE_TOO_LONG, GD_E_LONG_FLUSH, "Line too long writing {2}", 0 },
  { GD_E_LINE_TOO_LONG, 0, "Line {3} of {2} too long", 0 },
  /* GD_E_OK: (nothing) */
  { 0, 0, "Success", 0} /* this must be the last error string defined */
};

/* _GD_SetError: Sets the global error variables for a library error */
void _GD_SetError2(DIRFILE* D, int error, int suberror,
    const char* format_file, int line, const char* token, int stdlib_errno)
{
  dtrace("%p, %i, %i, \"%s\", %i, \"%s\", %i", D, error, suberror, format_file,
      line, token, stdlib_errno);

  D->error = error;
  if (error != GD_E_OK)
    D->n_error++;
  D->suberror = suberror;
  D->stdlib_errno = stdlib_errno;
  D->error_line = line;
  if (format_file != NULL) {
    free(D->error_file);
    D->error_file = strdup(format_file);
  }
  if (token != NULL) {
    free(D->error_string);
    D->error_string = strdup(token);
  }

  if (D->flags & GD_VERBOSE) {
    char *error_string = gd_error_string(D, NULL, 0);
    fprintf(stderr, "%slibgetdata: %s\n",
        D->error_prefix ? D->error_prefix : "", error_string);
    _GD_CFree(error_string);
  }

  dreturnvoid();
}

void _GD_SetError(DIRFILE* D, int error, int suberror, const char* format_file,
    int line, const char* token)
{
  dtrace("%p, %i, %i, \"%s\", %i, \"%s\"", D, error, suberror, format_file,
      line, token);

  _GD_SetError2(D, error, suberror, format_file, line, token, errno);

  dreturnvoid();
}

/* Set an appropriate error message for encoding framework errors */
void _GD_SetEncIOError(DIRFILE *D, int suberror, const struct gd_raw_file_ *f)
{
  dtrace("%p, %i, %p", D, suberror, f);

  /* If the encoding has no strerr function, fallback on strerr */
  if (_GD_MissingFramework(f->subenc, GD_EF_STRERR))
    _GD_SetError(D, GD_E_IO, suberror, f->name, 0, NULL);
  else {
    char buffer[GD_MAX_LINE_LENGTH];
    if ((*_GD_ef[f->subenc].strerr)(f, buffer, GD_MAX_LINE_LENGTH))
      strcpy(buffer, "Unknown error");
    _GD_SetError2(D, GD_E_IO, GD_E_IO_ENC_OFFSET + suberror, f->name, 0,
        buffer, 0);
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
#define UNKNOWN "Unknown error %i:%i. Please report to <" PACKAGE_BUGREPORT ">"
char* gd_error_string(const DIRFILE* D, char* buffer, size_t buflen) gd_nothrow
{
  const char* ip;
  char* op;
  char* bufend;
  int i, s = -1;

  dtrace("%p, %p, %" PRIuSIZE, D, buffer, buflen);

  /* Sanity check */
  if (buffer && buflen < 1) {
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

  if (buffer == NULL) {
    /* computer buffer length */
    if (s == -1) {
      /* a 64-bit int is 20 decimal digits, the %i takes up two characters,
       * so at most 18 more are needed for each integer */
      buflen = sizeof(UNKNOWN) + 2 * 18 + 1;
    } else {
      /* again, space for two 64-bit ints - "{n}" = 17 */
      buflen = strlen(error_string[s].format) + 2 * 17 + 1;
      if (D->error_file)
        buflen += strlen(D->error_file);
      if (D->error_string)
        buflen += strlen(D->error_string);
      if (error_string[s].adderr)
        /* hmmm... how long is our strerror string?  Really, the only way to
         * find out is to run strerror_r with increasingly long buffers until
         * we don't get ERANGE anymore.  Um, let's just guess. */
        buflen += GD_MAX_LINE_LENGTH;
    }

    buffer = _GD_CMalloc(buflen);
    if (buffer == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }
  }
  op = buffer;
  bufend = buffer + buflen;

  if (s == -1) /* Unhandled error */
    snprintf(buffer, buflen, UNKNOWN, -D->error, D->suberror);
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
      _GD_StrError(D->stdlib_errno, op, bufend - op);
  }

  dreturn("\"%s\"", buffer);
  return buffer;
}

int gd_error_count(DIRFILE *D) gd_nothrow
{
  int count;

  dtrace("%p", D);
  count = D->n_error;
  D->n_error = 0;

  dreturn("%i", count);

  return count;
}
/* vim: ts=2 sw=2 et tw=80
*/
