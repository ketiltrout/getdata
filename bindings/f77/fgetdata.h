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
# error The F77_FUNC macro must be defined to build the F77 bindings
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

void F77_FUNC(gdffgn, GDFFGN) (char* filename, int* filename_l,
    const int* dirfile, const int* index);

void F77_FUNC(gdfnfg, GDFNFG) (int* nformats, const int* dirfile);

void F77_FUNC(gdfflm, GDFFLM) (const int* dirfile);

void F77_FUNC(gdfinc, GDFINC) (const int* dirfile, const char* file,
    const int* file_l, const int* format_file, const int* flags);

void F77_FUNC(gdfnft, GDFNFT) (int* nfields, const int* dirfile,
    const int* type);

void F77_FUNC(gdfnve, GDFNVE) (int* nvectors, const int* dirfile);

void F77_FUNC(gdffnt, GDFFNT) (char* name, int* name_l, const int* dirfile,
    const int* type, const int* field_num);

void F77_FUNC(gdfven, GDFVEN) (char* name, int* name_l, const int* dirfile,
    const int* field_num);

void F77_FUNC(gdfmlc, GDFMLC) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* n_fields, const char* in_field1, const int* in_field1_l,
    const double* m1, const double* b1, const char* in_field2,
    const int* in_field2_l, const double* m2, const double* b2,
    const char* in_field3, const int* in_field3_l, const double* m3,
    const double* b3);

void F77_FUNC(gdfmlt, GDFMLT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const char* table,
    const int* table_l);

void F77_FUNC(gdfmbt, GDFMBT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* bitnum,
    const int* numbits);

void F77_FUNC(gdfmmt, GDFMMT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field1, const int* in_field1_l, const char* in_field2,
    const int* in_field2_l);

void F77_FUNC(gdfmph, GDFMPH) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* shift);

void F77_FUNC(gdfaco, GDFACO) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* const_type, const int* data_type,
    const void* value, const int* format_file);

void F77_FUNC(gdfmco, GDFMCO) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* const_type, const int* data_type, const void* value);

void F77_FUNC(gdfast, GDFAST) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* value, const int* value_l,
    const int* format_file);

void F77_FUNC(gdfmst, GDFMST) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* value, const int* value_l);

void F77_FUNC(gdfasp, GDFASP) (const int* dirfile, const char* spec,
    const int* spec_l, const int* format_file);

void F77_FUNC(gdfmsp, GDFMSP) (const int* dirfile, const char* spec,
    const int* spec_l, const char *parent, const int* parent_l);

void F77_FUNC(gdfffi, GDFFFI) (int* format_file, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdfgco, GDFGCO) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* return_type,
    void* data_out);

void F77_FUNC(gdfgst, GDFGST) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    char* data_out);

void F77_FUNC(gdfpco, GDFPCO) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* return_type,
    const void* data_out);

void F77_FUNC(gdfpst, GDFPST) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    const char* data_out);

void F77_FUNC(gdfnmf, GDFNMF) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l);

void F77_FUNC(gdfnmt, GDFNMT) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l, const int* type);

void F77_FUNC(gdfnmv, GDFNMV) (int* nvectors, const int* dirfile,
    const char* parent, const int* parent_l);

void F77_FUNC(gdfeco, GDFECO) (int* data_type, int* format_file,
    const int* dirfile, const char* field_code, const int* field_code_l);

void F77_FUNC(gdfmfx, GDFMFX) (int* max, const int* dirfile, const char* parent,
    const int* parent_l);

void F77_FUNC(gdfmfn, GDFMFN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num);

void F77_FUNC(gdfmft, GDFMFT) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* type,
    const int* field_num);

void F77_FUNC(gdfmvn, GDFMVN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num);
