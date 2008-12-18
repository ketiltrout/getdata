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

/* disable the "unspecified order" remark in ICC */
#ifdef __INTEL_COMPILER
#  pragma warning (disable : 981)
#endif

#define GDF_N_DIRFILES 1024

#endif

/* Forward declarations to keep icc happy */
void F77_FUNC(gdopen, GDOPEN) (int* dirfile, const char* dirfilename,
    const int* dirfilename_l, const int* flags);

void F77_FUNC(gdclos, GDCLOS) (const int* dirfile);

void F77_FUNC(gdflsh, GDFLSH) (const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdgetd, GDGETD) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l,
    const int* first_frame, const int* first_sample,
    const int* num_frames, const int* num_samples, const int* return_type,
    void* data_out);

void F77_FUNC(gdmdnx, GDFDNX) (int* max, const int* dirfile);

void F77_FUNC(gdfldn, GDFLDN) (char* name, int* name_l, const int* dirfile,
    const int* field_num);

void F77_FUNC(gdnfld, GDNFLD) (int* nfields, const int* dirfile);

void F77_FUNC(gdnfrm, GDNFRM) (int* nframes, const int* dirfile);

void F77_FUNC(gdgspf, GDGSPF) (int* spf, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdputd, GDPUTD) (int* n_wrote, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* first_frame,
    const int* first_sample, const int* num_frames, const int* num_samples,
    const int* data_type, const void* data_in);

void F77_FUNC(gderor, GDEROR) (int* error, const int* dirfile);

void F77_FUNC(gdestr, GDESTR) (const int* dirfile, char* buffer,
    const int* len);

void F77_FUNC(gdfldt, GDFLDT) (int* type, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgerw, GDGERW) (int* spf, int* dtype, int* fragment_index,
    const int* dirfile, const char* field_code, const int* field_code_l);

void F77_FUNC(gdgelc, GDGELC) (int* nfields,
    char* infield1, int* infield1_l, double* m1, double* b1,
    char* infield2, int* infield2_l, double* m2, double* b2,
    char* infield3, int* infield3_l, double* m3, double* b3,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdgelt, GDGELT) (char* in_field, int* in_field_l, char* table,
    int* table_l, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgebt, GDGEBT) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgemt, GDGEMT) (char* in_field1, int* in_field1_l,
    char* in_field2, int* in_field2_l, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgeph, GDGEPH) (char* in_field, int* in_field_l, int* shift,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdadrw, GDADRW) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* data_type, const int* spf,
    const int* fragment_index);

void F77_FUNC(gdadlc, GDADLC) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const double* m1, const double* b1,
    const char* in_field2, const int* in_field2_l, const double* m2,
    const double* b2, const char* in_field3, const int* in_field3_l,
    const double* m3, const double* b3, const int* fragment_index);

void F77_FUNC(gdadlt, GDADLT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const char* table, const int* table_l, const int* fragment_index);

void F77_FUNC(gdadbt, GDADBT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* bitnum, const int* numbits, const int* fragment_index);

void F77_FUNC(gdadmt, GDADMT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l, const int* fragment_index);

void F77_FUNC(gdadph, GDADPH) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* shift, const int* fragment_index);

void F77_FUNC(gdfrgn, GDFRGN) (char* filename, int* filename_l,
    const int* dirfile, const int* index);

void F77_FUNC(gdnfrg, GDNFRG) (int* nformats, const int* dirfile);

void F77_FUNC(gdmfls, GDMFLS) (const int* dirfile);

void F77_FUNC(gdincl, GDINCL) (const int* dirfile, const char* file,
    const int* file_l, const int* fragment_index, const int* flags);

void F77_FUNC(gdnfdt, GDNFDT) (int* nfields, const int* dirfile,
    const int* type);

void F77_FUNC(gdnvec, GDNVEC) (int* nvectors, const int* dirfile);

void F77_FUNC(gdfdnt, GDFDNT) (char* name, int* name_l, const int* dirfile,
    const int* type, const int* field_num);

void F77_FUNC(gdvecn, GDVECN) (char* name, int* name_l, const int* dirfile,
    const int* field_num);

void F77_FUNC(gdmdlc, GDMDLC) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* n_fields, const char* in_field1, const int* in_field1_l,
    const double* m1, const double* b1, const char* in_field2,
    const int* in_field2_l, const double* m2, const double* b2,
    const char* in_field3, const int* in_field3_l, const double* m3,
    const double* b3);

void F77_FUNC(gdmdlt, GDMDLT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const char* table,
    const int* table_l);

void F77_FUNC(gdmdbt, GDMDBT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* bitnum,
    const int* numbits);

void F77_FUNC(gdmdmt, GDMDMT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field1, const int* in_field1_l, const char* in_field2,
    const int* in_field2_l);

void F77_FUNC(gdmdph, GDMDPH) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* shift);

void F77_FUNC(gdadco, GDADCO) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* const_type, const int* data_type,
    const void* value, const int* fragment_index);

void F77_FUNC(gdmdco, GDMDCO) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* const_type, const int* data_type, const void* value);

void F77_FUNC(gdadst, GDADST) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* value, const int* value_l,
    const int* fragment_index);

void F77_FUNC(gdmdst, GDMDST) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* value, const int* value_l);

void F77_FUNC(gdadsp, GDADSP) (const int* dirfile, const char* spec,
    const int* spec_l, const int* fragment_index);

void F77_FUNC(gdmdsp, GDMDSP) (const int* dirfile, const char* spec,
    const int* spec_l, const char *parent, const int* parent_l);

void F77_FUNC(gdfrgi, GDFRGI) (int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgtco, GDGTCO) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* return_type,
    void* data_out);

void F77_FUNC(gdgtst, GDGTST) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    char* data_out);

void F77_FUNC(gdptco, GDPTCO) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* return_type,
    const void* data_out);

void F77_FUNC(gdptst, GDPTST) (int* n_read, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* len,
    const char* data_out);

void F77_FUNC(gdnmfd, GDNMFD) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l);

void F77_FUNC(gdnmft, GDNMFT) (int* nfields, const int* dirfile,
    const char* parent, const int* parent_l, const int* type);

void F77_FUNC(gdnmve, GDNMVE) (int* nvectors, const int* dirfile,
    const char* parent, const int* parent_l);

void F77_FUNC(gdgeco, GDGECO) (int* data_type, int* fragment_index,
    const int* dirfile, const char* field_code, const int* field_code_l);

void F77_FUNC(gdmfnx, GDMFNX) (int* max, const int* dirfile, const char* parent,
    const int* parent_l);

void F77_FUNC(gdmfdn, GDMFDN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num);

void F77_FUNC(gdmfdt, GDMFDT) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* type,
    const int* field_num);

void F77_FUNC(gdmven, GDMVEN) (char* name, int* name_l, const int* dirfile,
    const char* parent, const int* parent_l, const int* field_num);

void F77_FUNC(gddscd, GDDSCD) (const int* dirfile);

void F77_FUNC(gdcopn, GDCOPN) (int* dirfile, const char* dirfilename,
    const int* dirfilename_l, const int* flags, void* callback);
