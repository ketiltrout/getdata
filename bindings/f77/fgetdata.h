/* (C) 2008 D. V. Wiebe
 *
 *************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef FGETDATA_H
#define FGETDATA_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "getdata.h"
#include "../../src/internal.h"

/* If F77_FUNC isn't defined, we have no knowledge of the F77 mangling scheme */
#ifndef F77_FUNC
# error The F77_FUNC must be defined to build the F77 bindings
#endif

#define GDF_N_DIRFILES 1024

#endif

/* Forward declarations to keep icc happy */
void F77_FUNC(gdfopn, GDFOPN) (int* dirfile, const char* dirfilename,
    const int* dirfilename_l, const int* flags);

void F77_FUNC(gdfcls, GDFCLS) (const int* dirfile);

void F77_FUNC(gdffls, GDFFLS) (const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdfget, GDFGET) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l,
    const int* first_frame, const int* first_sample,
    const int* num_frames, const int* num_samples, const int* return_type,
    void* data_out);

void F77_FUNC(gdffnx, GDFFNX) (int* max, const int* dirfile);

void F77_FUNC(gdffdn, GDFFDN) (char* name, int* name_l, const int* dirfile,
    const int* field_num);

void F77_FUNC(gdfnfd, GDFNFD) (int* nfields, const int* dirfile);

void F77_FUNC(gdfnfr, GDFNFR) (int* nframes, const int* dirfile);

void F77_FUNC(gdfspf, GDFSPF) (int* spf, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdfput, GDFPUT) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* first_frame,
    const int* first_sample, const int* num_frames, const int* num_samples,
    const int* data_type, const void* data_in);

void F77_FUNC(gdferr, GDFERR) (int* error, const int* dirfile);

void F77_FUNC(gdfstr, GDFSTR) (const int* dirfile, char* buffer,
    const int* len);

void F77_FUNC(gdffdt, GDFFDT) (int* type, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdferw, GDFERW) (int* spf, int* dtype, int* format_file,
    const int* dirfile, const char* field_code, const int* field_code_l);

void F77_FUNC(gdfelc, GDFELC) (int* nfields,
    char* infield1, int* infield1_l, double* m1, double* b1,
    char* infield2, int* infield2_l, double* m2, double* b2,
    char* infield3, int* infield3_l, double* m3, double* b3, int* format_file,
    const int* dirfile, const char* field_code, const int* field_code_l);

void F77_FUNC(gdfelt, GDFELT) (char* in_field, int* in_field_l, char* table,
    int* table_l, int* format_file, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdfebt, GDFEBT) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, int* format_file, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdfemt, GDFEMT) (char* in_field1, int* in_field1_l,
    char* in_field2, int* in_field2_l, int* format_file, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdfeph, GDFEPH) (char* in_field, int* in_field_l, int* shift,
    int* format_file, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdfarw, GDFARW) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* data_type, const int* spf,
    const int* format_file);

void F77_FUNC(gdfalc, GDFALC) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const double* m1, const double* b1,
    const char* in_field2, const int* in_field2_l, const double* m2,
    const double* b2, const char* in_field3, const int* in_field3_l,
    const double* m3, const double* b3, const int* format_file);

void F77_FUNC(gdfalt, GDFALT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const char* table, const int* table_l, const int* format_file);

void F77_FUNC(gdfabt, GDFABT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* bitnum, const int* numbits, const int* format_file);

void F77_FUNC(gdfamt, GDFAMT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l, const int* format_file);

void F77_FUNC(gdfaph, GDFAPH) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* shift, const int* format_file);

void F77_FUNC(gdfffn, GDFFFN) (char* filename, int* filename_l,
    const int* dirfile, const int* index);

void F77_FUNC(gdfnft, GDFNFT) (int* nformats, const int* dirfile);

void F77_FUNC(gdfflm, GDFFLM) (const int* dirfile);
