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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "getdata_internal.h"

static int _GD_DoFieldOut(DIRFILE* D, const char *field_code,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t data_type, const void *data_in);

/***************************************************************************/
/*                                                                         */
/* Look to see if the output field code belongs to a raw.  If so, parse it.*/
/*                                                                         */
/***************************************************************************/
static int _GD_DoRawOut(DIRFILE *D, struct RawEntryType *R, int first_frame,
    int first_samp, int num_frames, int num_samp, gd_type_t data_type,
    const void *data_in)
{
  int s0, ns, n_wrote;
  char datafilename[FILENAME_MAX];
  void *databuffer;
  struct stat statbuf;

  s0 = first_samp + first_frame * (int)(R->samples_per_frame);
  ns = num_samp + num_frames * (int)(R->samples_per_frame);

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoRawOut:  file pointer for field %s = %d\n", field_code,
      R->fp);
#endif

  if (R->fp < 0) {
    /* open file for reading / writing if not already opened */

    sprintf(datafilename, "%s/%s", D->name, R->file);

#ifdef GETDATA_DEBUG
    fprintf(stdout,"DoRawOut:  stat(%s) = %d\n", datafilename,
        stat(datafilename, &statbuf));
#endif

    if(stat(datafilename, &statbuf) == 0){
      R->fp = open(datafilename, O_RDWR);
      if (R->fp < 0) {
        _GD_SetGetDataError(D, GD_E_PUT_RAWFIELD, 0, NULL, 0, NULL);
        return 0;
      }
    } else {
      R->fp = open(datafilename, O_RDWR | O_CREAT, 0644);
      if (R->fp < 0) {
        _GD_SetGetDataError(D, GD_E_PUT_RAWFIELD, 0, NULL, 0, NULL);
        return 0;
      }
    }

#ifdef GETDATA_DEBUG
    fprintf(stdout, "DoRawOut:  opening file %s for writing\n",
        datafilename);
#endif
  } else {
    /* make sure that file is in read / write mode        */
    /* if not, close file and reopen in read / write mode */

#ifdef GETDATA_DEBUG
    fprintf(stdout, "DoRawOut:  file is already open\n");
#endif

    sprintf(datafilename, "%s/%s", D->name, R->file);
    if (close(R->fp) < 0) {
      _GD_SetGetDataError(D, GD_E_PUT_RAWFIELD, 0, NULL, 0, NULL);
      return 0;
    } else {
      R->fp = open(datafilename, O_RDWR);
    }
    if (R->fp < 0) {
      _GD_SetGetDataError(D, GD_E_PUT_RAWFIELD, 0, NULL, 0, NULL);
      return 0;
    }
  }

  databuffer = _GD_Alloc(D, R->data_type, ns);

  _GD_ConvertType(D, data_in, data_type, databuffer, R->data_type, ns);

  /* write data to file.  Note that if the first sample is beyond     */
  /* the current end of file, the gap will be filled with zero bytes. */

  lseek(R->fp, s0 * (int)(R->size), SEEK_SET);
  n_wrote = ((int)write(R->fp, databuffer, (size_t)(R->size) * (size_t)ns)) /
    (R->size);

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoRawOut:  %d samples\n", (int)*n_write);
#endif

  free(databuffer);

  return n_wrote;
}

/***************************************************************************/
/*                                                                         */
/*   Look to see if output field belongs to a linterp.  If so, parse it.   */
/*                                                                         */
/***************************************************************************/
static int _GD_DoLinterpOut(DIRFILE* D, struct LinterpEntryType *I,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t data_type, const void *data_in)
{
  int spf;
  int ns;
  int n_wrote;

  if (I->n_interp < 0) {
    _GD_ReadLinterpFile(D, I);
    if (D->error != GD_E_OK)
      return 0;
  }

  /* Interpolate X(y) instead of Y(x) */

  D->recurse_level++;
  spf = _GD_GetSPF(I->raw_field, D);
  D->recurse_level--;
  ns = num_samp + num_frames * (int)spf;

  _GD_LinterpData(D, data_in, data_type, ns, I->y, I->x, I->n_interp);

  if (D->error != GD_E_OK)
    return 0;

  D->recurse_level++;
  n_wrote = _GD_DoFieldOut(D, I->raw_field, first_frame, first_samp,
      num_frames, num_samp, data_type, data_in);
  D->recurse_level--;

  return n_wrote;
}

/***************************************************************************/
/*                                                                         */
/*   Look to see if output field belongs to a lincom.  If so, parse it.    */
/*                                                                         */
/***************************************************************************/
static int _GD_DoLincomOut(DIRFILE* D, struct LincomEntryType *L,
    int first_frame, int first_samp, int num_frames, int num_samp,
    gd_type_t data_type, const void *data_in)
{
  int spf;
  int ns, n_wrote;
  void* tmpbuf;

  /* we cannot write to LINCOM fields that are a linear combination */
  /* of more than one raw field (no way to know how to split data). */

  if (L->n_infields > 1) {
    _GD_SetGetDataError(D, GD_E_MULT_LINCOM, 0, NULL, 0, L->field);
    return 0;
  }

  D->recurse_level++;

  /* do the inverse scaling */

  D->recurse_level++;
  spf = _GD_GetSPF(L->in_fields[0], D);
  D->recurse_level--;
  ns = num_samp + num_frames * (int)spf;

  /* writeable copy */
  tmpbuf = _GD_Alloc(D, data_type, ns);
  memcpy(tmpbuf, data_in, ns * _GD_TypeSize(data_type));

  _GD_ScaleData(D, tmpbuf, data_type, ns, 1 / L->m[0], -L->b[0] / L->m[0]);

  if (D->error != GD_E_OK)
    return 0;

  n_wrote = _GD_DoFieldOut(D, L->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, data_type, tmpbuf);
  free(tmpbuf);

  D->recurse_level--;

  return n_wrote;
}

/***************************************************************************/
/*                                                                         */
/*   Look to see if output field belongs to a bitfield.  If so, parse it.  */
/*                                                                         */
/***************************************************************************/
static int _GD_DoBitOut(DIRFILE* D, struct BitEntryType *B, int first_frame,
    int first_samp, int num_frames, int num_samp, gd_type_t data_type,
    const void *data_in)
{
  uint64_t *tmpbuf;
  uint64_t *readbuf;
  int i, n_wrote;
  int spf;
  int ns;
  int n_read;
  unsigned highmask;
  unsigned lowmask;

  D->recurse_level++;
  spf = _GD_GetSPF(B->raw_field, D);
  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  ns = num_samp + num_frames * (int)spf;

  tmpbuf = _GD_Alloc(D, GD_UINT64, ns);
  memset(tmpbuf, 0, sizeof(uint64_t) * ns);
  readbuf = _GD_Alloc(D, GD_UINT64, ns);
  memset(readbuf, 0, sizeof(uint64_t) * ns);

  _GD_ConvertType(D, data_in, data_type, (void*)tmpbuf, GD_UINT64, ns);

  /* first, READ the field in so that we can change the bits    */
  /* do not check error code, since the field may not exist yet */

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoBitOut:  reading in bitfield %s\n",B->raw_field);
#endif

  D->recurse_level++;

  n_read = _GD_DoField(D, B->raw_field, first_frame, first_samp, num_frames,
      num_samp, GD_UINT64, readbuf);

  D->recurse_level--;

  _GD_ClearGetDataError(D);

  /* now go through and set the correct bit in each field value */

  highmask = 1 << (B->bitnum);
  lowmask = ~highmask;
#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoBitOut:  bitnum = %d highmask = %u lowmask = %u\n",
      B->bitnum, highmask, lowmask);
#endif

  /* FIXME: Multibit */
  for (i = 0; i < ns; i++) {
    if (tmpbuf[i]) /*set the bit to 1*/
      readbuf[i] = readbuf[i] | highmask;
    else /*set the bit to 0*/
      readbuf[i] = readbuf[i] & lowmask;
  }

  /* write the modified data out */
  n_wrote = _GD_DoFieldOut(D, B->raw_field, first_frame, first_samp,
      num_frames, num_samp, GD_UINT64, (void*)readbuf);

  free(readbuf);
  free(tmpbuf);
  return n_wrote;
}

/***************************************************************************/
/*                                                                         */
/*  _GD_DoFieldOut: Do one output field once F has been identified         */
/*                                                                         */
/***************************************************************************/
static int _GD_DoFieldOut(DIRFILE *D, const char *field_code, int first_frame,
    int first_samp, int num_frames, int num_samp, gd_type_t data_type,
    const void *data_in)
{
  struct gd_entry_t* entry;

  if (D->recurse_level > 10) {
    _GD_SetGetDataError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    return 0;
  }

  /* Find the field */
  entry = _GD_FindField(D, field_code);

  if (entry == NULL) { /* No match */
    _GD_SetGetDataError(D, GD_E_BAD_PUT_CODE, 0, NULL, 0, field_code);
    return 0;
  }

  switch (entry->field_type) {
    case GD_RAW_ENTRY:
      return _GD_DoRawOut(D, ENTRY(Raw, entry), first_frame, first_samp,
          num_frames, num_samp, data_type, data_in);
    case GD_LINTERP_ENTRY:
      return _GD_DoLinterpOut(D, ENTRY(Linterp, entry), first_frame, first_samp,
          num_frames, num_samp, data_type, data_in);
    case GD_LINCOM_ENTRY:
      return _GD_DoLincomOut(D, ENTRY(Lincom, entry), first_frame, first_samp,
          num_frames, num_samp, data_type, data_in);
    case GD_BIT_ENTRY:
      return _GD_DoBitOut(D, ENTRY(Bit, entry), first_frame, first_samp,
          num_frames, num_samp, data_type, data_in);
    case GD_MULTIPLY_ENTRY:
      _GD_SetGetDataError(D, GD_E_BAD_PUT_CODE, 0, NULL, 0, field_code);
      return 0;
    case GD_PHASE_ENTRY:
      _GD_SetGetDataError(D, GD_E_BAD_PUT_CODE, 0, NULL, 0, field_code);
      return 0;
  }

  _GD_SetGetDataError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL);
  return 0;
}

/***************************************************************************/
/*                                                                         */
/*  PutData: write BLAST format RAW files.                                 */
/*    filename_in: the name of the file directory (raw files are in here)  */
/*    field_code: the name of the field you want to write                  */
/*    first_frame, first_samp: the first sample written is                 */
/*              first_samp + samples_per_frame*first_frame                 */
/*    num_frames, num_samps: the number of samples written is              */
/*              num_samps + samples_per_frame*num_frames                   */
/*    data_type: data type of *data_in.  's': 16 bit signed                */
/*              'u' 16bit unsigned 'S' 32bit signed 'U' 32bit unsigned     */
/*              'c' 8 bit unsigned 'f' 32bit float 'd' 64bit double        */
/*    void *data_in: array containing the data                             */
/*    *error_code: error code is returned here. If error_code == NULL,     */
/*               PutData prints the error message and exits                */
/*                                                                         */
/*    return value: returns # of samples actually written to file          */
/*                                                                         */
/***************************************************************************/
int putdata(DIRFILE* D, const char *field_code, int first_frame, int first_samp,
    int num_frames, int num_samp, gd_type_t data_type, void *data_in)
{
  _GD_ClearGetDataError(D);

  if (data_type == GD_INT32_ALT)
    data_type = GD_INT32;

  return _GD_DoFieldOut(D, field_code, first_frame, first_samp, num_frames,
      num_samp, data_type, data_in);
}
/* vim: ts=2 sw=2 et
*/
