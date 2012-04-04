/* Copyright (C) 2008-2012 D. V. Wiebe
 *
 *************************************************************************
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
#ifndef FGETDATA_H
#define FGETDATA_H

#include "internal.h"

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

#ifdef __cplusplus
extern "C" {
#endif
typedef void(*_GDF_callback_t)(int*, const int*, const int*, char*, const int*,
               const char*);

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

void F77_FUNC(gdfdnx, GDFDNX) (int* max, const int* dirfile);

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

void F77_FUNC(gdenty, GDENTY) (int* type, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgecl, GDGECL) (int* nfields,
    char* infield1, int* infield1_l, GD_DCOMPLEXP(m1), GD_DCOMPLEXP(b1),
    char* infield2, int* infield2_l, GD_DCOMPLEXP(m2), GD_DCOMPLEXP(b2),
    char* infield3, int* infield3_l, GD_DCOMPLEXP(m3), GD_DCOMPLEXP(b3),
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdgecp, GDGECP) (int* poly_ord,
    char* infield, int* infield_l, GD_DCOMPLEXP(a0), GD_DCOMPLEXP(a1),
    GD_DCOMPLEXP(a2), GD_DCOMPLEXP(a3), GD_DCOMPLEXP(a4),
    GD_DCOMPLEXP(a5), int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgerw, GDGERW) (int* spf, int* dtype, int* fragment_index,
    const int* dirfile, const char* field_code, const int* field_code_l);

void F77_FUNC(gdgelc, GDGELC) (int* nfields,
    char* infield1, int* infield1_l, double* m1, double* b1,
    char* infield2, int* infield2_l, double* m2, double* b2,
    char* infield3, int* infield3_l, double* m3, double* b3,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdgepn, GDGEPN) (int* nfields, char* infield, int* infield_l,
    double* a0, double* a1, double* a2, double* a3, double* a4, double* a5,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdgelt, GDGELT) (char* in_field, int* in_field_l, char* table,
    int* table_l, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgebt, GDGEBT) (char* in_field, int* in_field_l, int* bitnum,
    int* numbits, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgesb, GDGESB) (char* in_field, int* in_field_l, int* bitnum,
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

void F77_FUNC(gdadpn, GDADPN) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const double* a0, const double* a1, const double* a2,
    const double* a3, const double* a4, const double* a5,
    const int* fragment_index);

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

void F77_FUNC(gdmdpn, GDMDPN) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* poly_ord, const char* in_field, const int* in_field_l,
    const double* a0, const double* a1, const double* a2, const double* a3,
    const double* a4, const double* a5);

void F77_FUNC(gdmdlt, GDMDLT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const char* table,
    const int* table_l);

void F77_FUNC(gdmdbt, GDMDBT) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const int* bitnum,
    const int* numbits);

void F77_FUNC(gdmdsb, GDMDSB) (const int* dirfile, const char* parent,
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

void F77_FUNC(gdgtco, GDGTCO) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *return_type, void *data_out);

void F77_FUNC(gdgtca, GDGTCA) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *return_type, void *data_out);

void F77_FUNC(gdcaln, GDCALN) (int *len, const int *dirfile,
    const char *field_code, const int *field_code_l);

void F77_FUNC(gdgcas, GDGCAS) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *start, const int *n,
    const int *return_type, void *data_out);

void F77_FUNC(gdgtst, GDGTST) (int *size, const int *dirfile,
    const char *field_code, const int *field_code_l, const int *len,
    char *data_out);

void F77_FUNC(gdptco, GDPTCO) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *data_type, const void *data_in);

void F77_FUNC(gdptca, GDPTCA) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *data_type, const void *data_in);

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
    const int* dirfilename_l, const int* flags, const _GDF_callback_t callback);

void F77_FUNC(gdclbk, GDCLBK) (const int* dirfile,
    const _GDF_callback_t callback);

void F77_FUNC(gdalbt, GDALBT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    int* bitnum, int* numbits);

void F77_FUNC(gdalco, GDALCO) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* const_type);

void F77_FUNC(gdalca, GDALCA) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* const_type, const int *array_len);

void F77_FUNC(gdallc, GDALLC) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const double* m1, const double* b1,
    const char* in_field2, const int* in_field2_l, const double* m2,
    const double* b2, const char* in_field3, const int* in_field3_l,
    const double* m3, const double* b3);

void F77_FUNC(gdalpn, GDALPN) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const double* a0, const double* a1, const double* a2,
    const double* a3, const double* a4, const double* a5);

void F77_FUNC(gdalmt, GDALMT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l);

void F77_FUNC(gdalph, GDALPH) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* shift);

void F77_FUNC(gdgenc, GDGENC) (int* encoding, const int* dirfile,
    const int* fragment);

void F77_FUNC(gdgend, GDGEND) (int* endianness, const int* dirfile,
    const int* fragment);

void F77_FUNC(gdname, GDNAME) (char* name, int* name_l, const int* dirfile);

void F77_FUNC(gdpfrg, GDPFRG) (int* parent, const int* dirfile,
    const int* fragment);

void F77_FUNC(gdprot, GDPROT) (const int* dirfile, const int* fragment, 
    const int* protection_level);

void F77_FUNC(gdgprt, GDGPRT) (int* protection_level, const int* dirfile,
    const int* fragment);

void F77_FUNC(gdrwfn, GDRWFN) (char* name, int* name_l, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdrefe, GDREFE) (char* name, int* name_l, const int* dirfile,
    const char* field_code, const int *field_code_l);

void F77_FUNC(gdaenc, GDAENC) (const int* dirfile, const int* encoding,
    const int* fragment, const int* recode);

void F77_FUNC(gdaend, GDAEND) (const int* dirfile, const int* endianness,
    const int* fragment, const int* recode);

void F77_FUNC(gdallt, GDALLT) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const char* table, const int* table_l, const int* recode);

void F77_FUNC(gdalrw, GDALRW) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* data_type, const int* spf,
    const int* recode);

void F77_FUNC(gdalsp, GDALSP) (const int* dirfile, const char* spec,
    const int* spec_l, const int* fragment_index);

void F77_FUNC(gddele, GDDELE) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* flags);

void F77_FUNC(gdmlsp, GDMLSP) (const int* dirfile, const char* spec,
    const int* spec_l, const char* parent, const int* parent_l,
    const int* recode);

void F77_FUNC(gdmove, GDMOVE) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* new_fragment, const int* move_data);

void F77_FUNC(gdrenm, GDRENM) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* new_name, const int* new_name_l,
    const int* move_data);

void F77_FUNC(gduinc, GDUINC) (const int* dirfile, const int* fragment,
    const int* del);

void F77_FUNC(gdafof, GDAFOF) (const int* dirfile, const int* offset,
    const int* fragment, const int* recode);

void F77_FUNC(gdgfof, GDGFOF) (int* offset, const int* dirfile,
    const int* fragment);

void F77_FUNC(gdadcl, GDADCL) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char* in_field2, const int* in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char* in_field3, const int* in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3),
    const int* fragment_index);

void F77_FUNC(gdadcp, GDADCP) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5),
    const int* fragment_index);

void F77_FUNC(gdmdcl, GDMDCL) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* n_fields, const char* in_field1, const int* in_field1_l,
    const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1), const char* in_field2,
    const int* in_field2_l, const GD_DCOMPLEXP(m2), const GD_DCOMPLEXP(b2),
    const char* in_field3, const int* in_field3_l, const GD_DCOMPLEXP(m3),
    const GD_DCOMPLEXP(b3));

void F77_FUNC(gdmdcp, GDMDCP) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const int* poly_ord, const char* in_field, const int* in_field_l,
    const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5));

void F77_FUNC(gdntyp, GDNTYP) (int* type, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdalcl, GDALCL) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* n_fields, const char* in_field1,
    const int* in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char* in_field2, const int* in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char* in_field3, const int* in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3));

void F77_FUNC(gdalcp, GDALCP) (const int* dirfile, const char* field_code,
    const int* field_code_l, const int* poly_ord, const char* in_field,
    const int* in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5));

void F77_FUNC(gdcscl, GDCSCL) (int *comp_scal, const int *dirfile,
    const char *field_code, const int *field_code_l);

void F77_FUNC(gdvldt, GDVLDT) (int *valid, const int *dirfile,
    const char *field_code, const int *field_code_l);

void F77_FUNC(gdfnum, GDFNUM) (double *framenum, const int *dirfile,
    const char *field_code, const int *field_code_l, const double *value);

void F77_FUNC(gdfnss, GDFNSS) (double *framenum, const int *dirfile,
    const char *field_code, const int *field_code_l, const double *value,
    const int *start, const int *end);

void F77_FUNC(gdgsca, GDGSCA) (char* scalar, int* scalar_l, int *scalar_index,
    const int* dirfile, const char* field_code, const int *field_code_l,
    const int* index);

void F77_FUNC(gdasca, GDASCA) (const int* dirfile, const char* field_code,
    const int *field_code_l, const int *index, const char *scalar,
    const int *scalar_l, int *scalar_index, int* recode);

void F77_FUNC(gdalsb, GDALSB) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    int* bitnum, int* numbits);

void F77_FUNC(gdadsb, GDADSB) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const int* bitnum, const int* numbits, const int* fragment_index);

void F77_FUNC(gdinvd, GDINVD) (int *dirfile);

void F77_FUNC(gdstdv, GDSTDV) (int *vers, const int *dirfile);

void F77_FUNC(gdgbof, GDGBOF) (int* bof, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgeof, GDGEOF) (int* eof, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgedv, GDGEDV) (char* in_field1, int* in_field1_l,
    char* in_field2, int* in_field2_l, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgerc, GDGERC) (char* in_field, int* in_field_l,
    double* dividend, int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdgecr, GDGECR) (char* in_field, int* in_field_l,
    GD_DCOMPLEXP(cdividend), int* fragment_index, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdaddv, GDADDV) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l, const int* fragment_index);

void F77_FUNC(gdadrc, GDADRC) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const double* dividend, const int* fragment_index);

void F77_FUNC(gdadcr, GDADCR) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field, const int* in_field_l,
    const GD_DCOMPLEXP(cdividend), const int* fragment_index);

void F77_FUNC(gdmddv, GDMDDV) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field1, const int* in_field1_l, const char* in_field2,
    const int* in_field2_l);

void F77_FUNC(gdrfrg, GDRFRG) (const int* dirfile, const int* fragment);

void F77_FUNC(gdmdrc, GDMDRC) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l, const double* dividend);

void F77_FUNC(gdmdcr, GDMDCR) (const int* dirfile, const char* parent,
    const int* parent_l, const char* field_code, const int* field_code_l,
    const char* in_field, const int* in_field_l,
    const GD_DCOMPLEXP(cdividend));

void F77_FUNC(gdaldv, GDALDV) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const char* in_field2, const int* in_field2_l);

void F77_FUNC(gdalrc, GDALRC) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const double* dividend);

void F77_FUNC(gdalcr, GDALCR) (const int* dirfile, const char* field_code,
    const int* field_code_l, const char* in_field1, const int* in_field1_l,
    const GD_DCOMPLEXP(cdividend));

void F77_FUNC(gdaprt, GDAPRT) (const int* dirfile, const int* protection_level,
    const int* fragment);

void F77_FUNC(gdgeca, GDGECA) (int* data_type, int *array_len,
    int* fragment_index, const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdadca, GDADCA) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *const_type, int *array_len,
    const int *data_type, const void *value, const int *fragment_index);

void F77_FUNC(gdmdca, GDMDCA) (const int *dirfile, const char *parent,
    const int *parent_l, const char *field_code, const int *field_code_l,
    const int *const_type, const int *array_len, const int *data_type,
    const void *value);

void F77_FUNC(gdpcas, GDPCAS) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *start, const int *n,
    const int *data_type, const void *data_in);

void F77_FUNC(gdseek, GDSEEK) (int* pos, const int* dirfile,
    const char* field_code, const int* field_code_l, const int* frame_num,
    const int* sample_num, const int* flags);

void F77_FUNC(gdtell, GDTELL) (int* pos, const int* dirfile,
    const char* field_code, const int* field_code_l);

void F77_FUNC(gdcons, GDCONS) (void *value, const int *dirfile,
    const int *return_type, const int *field_num);

void F77_FUNC(gdmcos, GDMCOS) (void *value, const int *dirfile,
    const char *parent, const int *parent_l, const int *return_type,
    const int *field_num);

void F77_FUNC(gdstrs, GDSTRS) (char *value, int *value_l, const int *dirfile,
    const int *field_num);

void F77_FUNC(gdmsts, GDMSTS) (void *value, int *value_l, const int *dirfile,
    const char *parent, const int *parent_l, const int *field_num);

void F77_FUNC(gdstrx, GDSTRX) (int *max, const int *dirfile);

void F77_FUNC(gdmstx, GDMSTX) (int *max, const int *dirfile, const char *parent,
    const int *parent_l);

void F77_FUNC(gdadwd, GDADWD) (const int *dirfile, const char *field_code,
    const int *field_code_l, const char *in_field, const int *in_field_l,
    const char *check_field, const int *check_field_l, const int *windop,
    const void *threshold, const int *fragment_index);

void F77_FUNC(gdmdwd, GDMDWD) (const int *dirfile, const char *parent,
    const int *parent_l, const char *field_code, const int *field_code_l, 
    const char *in_field, const int *in_field_l, const char *check_field,
    const int *check_field_l, const int *windop, const void *threshold);

void F77_FUNC(gdalwd, GDALWD) (const int *dirfile, const char *field_code,
    const int *field_code_l, const char *in_field, const int *in_field_l,
    const char *check_field, const int *check_field_l, const int *windop,
    const void *threshold);

void F77_FUNC(gdadmx, GDADMX) (const int *dirfile, const char *field_code,
    const int *field_code_l, const char *in_field, const int *in_field_l,
    const char *count_field, const int *count_field_l, const int *val,
    const int *max, const int *fragment_index);

void F77_FUNC(gdmdmx, GDMDMX) (const int *dirfile, const char *parent,
    const int *parent_l, const char *field_code, const int *field_code_l, 
    const char *in_field, const int *in_field_l, const char *count_field,
    const int *count_field_l, const int *val, const int *max);

void F77_FUNC(gdalmx, GDALMX) (const int *dirfile, const char *field_code,
    const int *field_code_l, const char *in_field, const int *in_field_l,
    const char *count_field, const int *count_field_l, const int *val,
    const int *max);

void F77_FUNC(gdsync, GDSYNC) (const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdrclo, GDRCLO) (const int* dirfile, const char* field_code,
    const int* field_code_l);

void F77_FUNC(gdinca, GDINCA) (const int* dirfile, const char* file,
    const int* file_l, const int* fragment_index, const char* prefix,
    const int* prefix_l, const char* suffix, const int* suffix_l,
    const int* flags);

void F77_FUNC(gdmova, GDMOVA) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *new_fragment);

void F77_FUNC(gdmdal, GDMDAL) (const int *dirfile, const char *parent,
    const int *parent_l, const char *field_code, const int *field_code_l,
    const char *target, const int *target_l);

void F77_FUNC(gduhid, GDUHID) (const int *dirfile, const char *field_code,
    const int *field_code_l);

void F77_FUNC(gdhide, GDHIDE) (const int *dirfile, const char *field_code,
    const int *field_code_l);

void F77_FUNC(gdhidn, GDHIDN) (int *result, const int *dirfile,
    const char *field_code, const int *field_code_l);

void F77_FUNC(gdfraf, GDFRAF) (char *prefix, int *prefix_l, char *suffix,
    int *suffix_l, const int *dirfile, const int *index);

void F77_FUNC(gddela, GDDELA) (const int *dirfile, const char *field_code,
    const int *field_code_l, const int *flags);

void F77_FUNC(gdaafx, GDAAFX) (const int *dirfile, const int *index,
    const char *prefix, const int *prefix_l, const char *suffix,
    const int *suffix_l);

void F77_FUNC(gdalss, GDALSS) (char *alias, int *alias_l, const int *dirfile,
    const char *field_code, const int *field_code_l, const int *num);

void F77_FUNC(gdnals, GDNALS) (int *nalias, const int *dirfile,
    const char *field_code, const int *field_code_l);

void F77_FUNC(gdalsx, GDALSX) (int* max, const int* dirfile,
    const char *field_code, const int *field_code_l);

void F77_FUNC(gdatrg, GDATRG) (char *target, int *target_l, const int *dirfile, 
    const char *field_code, const int *field_code_l);

void F77_FUNC(gdadal, GDADAL) (const int *dirfile, const char *field_code,
    const int *field_code_l, const char *target, const int *target_l,
    const int *fragment_index);

void F77_FUNC(gdnocb, GDNOCB) (const int* dirfile);

void F77_FUNC(gddsyn, GDDSYN) (int *desync, const int *dirfile,
    const int *flags);

void F77_FUNC(gdflag, GDFLAG) (int *flags, const int *dirfile, const int *set,
    const int *reset);

void F77_FUNC(gdvbpx, GDVBPX) (const int *dirfile, const char *prefix,
    const int *prefix_l);

void F77_FUNC(gdmxlb, GDMXLB) (const int *dirfile, const int *lookback);

void F77_FUNC(gdnent, GDNENT) (int *nentries, const int *dirfile,
    const char *parent, const int *parent_l, const int *type, const int *flags);

void F77_FUNC(gdentx, GDENTX) (int* max, const int* dirfile, const char *parent,
    const int *parent_l, const int *type, const int *flags);

void F77_FUNC(gdentn, GDENTN) (char *name, int *name_l, const int *dirfile,
    const char *parent, const int *parent_l, const int *type, const int *flags,
    const int *field_num);
#ifdef __cplusplus
}
#endif
