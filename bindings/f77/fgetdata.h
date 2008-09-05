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

void F77_FUNC(gdferw, GDFERW) (int* spf, int* dtype, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdfelc, GDFELC) (int* nfields,
    char* infield1, int* infield1_l, double* m1, double* b1,
    char* infield2, int* infield2_l, double* m2, double* b2,
    char* infield3, int* infield3_l, double* m3, double* b3,
    const int* dirfile, const char* field_code, const int* field_code_l);

void F77_FUNC(gdfelt, GDFELT) (char* in_field, int* in_field_l, char* table,
    int* table_l, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdfebt, GDFEBT) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdfemt, GDFEMT) (char* in_field1, int* in_field1_l,
    char* in_field2, int* in_field2_l, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdfeph, GDFEPH) (char* in_field, int* in_field_l, int* shift,
    const int* dirfile, const char* field_code, const int* field_code_l);
