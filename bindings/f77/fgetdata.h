/* Copyright (C) 2008, 2010, 2012-2014 D. V. Wiebe
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
typedef void(*_GDF_callback_t)(int32_t*, const int32_t*, const int32_t*, char*,
    const int32_t*, const char*);

/* Forward declarations to keep icc happy */
void F77_FUNC(gdopen, GDOPEN) (int32_t *dirfile, const char *dirfilename,
    const int32_t *dirfilename_l, const int32_t *flags);

void F77_FUNC(gdclos, GDCLOS) (const int32_t *dirfile);

void F77_FUNC(gdflsh, GDFLSH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgetd, GDGETD) (int32_t *n_read, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample,
    const int32_t *num_frames, const int32_t *num_samples,
    const int32_t *return_type, void *data_out);

void F77_FUNC(gdfdnx, GDFDNX) (int32_t *max, const int32_t *dirfile);

void F77_FUNC(gdfldn, GDFLDN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const int32_t *field_num);

void F77_FUNC(gdnfld, GDNFLD) (int32_t *nfields, const int32_t *dirfile);

void F77_FUNC(gdnfrm, GDNFRM) (int32_t *nframes, const int32_t *dirfile);

void F77_FUNC(gdgspf, GDGSPF) (int32_t *spf, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdputd, GDPUTD) (int32_t *n_wrote, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample,
    const int32_t *num_frames, const int32_t *num_samples,
    const int32_t *data_type, const void *data_in);

void F77_FUNC(gderor, GDEROR) (int32_t *error, const int32_t *dirfile);

void F77_FUNC(gdestr, GDESTR) (const int32_t *dirfile, char *buffer,
    const int32_t *len);

void F77_FUNC(gdenty, GDENTY) (int32_t *type, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgecl, GDGECL) (int32_t *nfields,
    char *infield1, int32_t *infield1_l, GD_DCOMPLEXP(m1), GD_DCOMPLEXP(b1),
    char *infield2, int32_t *infield2_l, GD_DCOMPLEXP(m2), GD_DCOMPLEXP(b2),
    char *infield3, int32_t *infield3_l, GD_DCOMPLEXP(m3), GD_DCOMPLEXP(b3),
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgecp, GDGECP) (int32_t *poly_ord,
    char *infield, int32_t *infield_l, GD_DCOMPLEXP(a0), GD_DCOMPLEXP(a1),
    GD_DCOMPLEXP(a2), GD_DCOMPLEXP(a3), GD_DCOMPLEXP(a4),
    GD_DCOMPLEXP(a5), int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgerw, GDGERW) (int32_t *spf, int32_t *dtype,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgelc, GDGELC) (int32_t *nfields,
    char *infield1, int32_t *infield1_l, double *m1, double *b1,
    char *infield2, int32_t *infield2_l, double *m2, double *b2,
    char *infield3, int32_t *infield3_l, double *m3, double *b3,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgepn, GDGEPN) (int32_t *nfields, char *infield,
    int32_t *infield_l, double *a0, double *a1, double *a2, double *a3,
    double *a4, double *a5, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgelt, GDGELT) (char *in_field, int32_t *in_field_l, char *table,
    int32_t *table_l, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgebt, GDGEBT) (char *in_field, int32_t *in_field_l,
    int32_t *bitnum, int32_t *numbits, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgesb, GDGESB) (char *in_field, int32_t *in_field_l,
    int32_t *bitnum, int32_t *numbits, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgemt, GDGEMT) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgeph, GDGEPH) (char *in_field, int32_t *in_field_l,
    int32_t *shift, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdadrw, GDADRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const int32_t *fragment_index);

void F77_FUNC(gdadlc, GDADLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const double *b1,
    const char *in_field2, const int32_t *in_field2_l, const double *m2,
    const double *b2, const char *in_field3, const int32_t *in_field3_l,
    const double *m3, const double *b3, const int32_t *fragment_index);

void F77_FUNC(gdadpn, GDADPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const double *a1,
    const double *a2, const double *a3, const double *a4, const double *a5,
    const int32_t *fragment_index);

void F77_FUNC(gdadlt, GDADLT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *table, const int32_t *table_l,
    const int32_t *fragment_index);

void F77_FUNC(gdadbt, GDADBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits,
    const int32_t *fragment_index);

void F77_FUNC(gdadmt, GDADMT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index);

void F77_FUNC(gdadph, GDADPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift,
    const int32_t *fragment_index);

void F77_FUNC(gdfrgn, GDFRGN) (char *filename, int32_t *filename_l,
    const int32_t *dirfile, const int32_t *index);

void F77_FUNC(gdnfrg, GDNFRG) (int32_t *nformats, const int32_t *dirfile);

void F77_FUNC(gdmfls, GDMFLS) (const int32_t *dirfile);

void F77_FUNC(gdincl, GDINCL) (const int32_t *dirfile, const char *file,
    const int32_t *file_l, const int32_t *fragment_index, const int32_t *flags);

void F77_FUNC(gdnfdt, GDNFDT) (int32_t *nfields, const int32_t *dirfile,
    const int32_t *type);

void F77_FUNC(gdnvec, GDNVEC) (int32_t *nvectors, const int32_t *dirfile);

void F77_FUNC(gdfdnt, GDFDNT) (char *name, int32_t *name_l,
    const int32_t *dirfile, const int32_t *type, const int32_t *field_num);

void F77_FUNC(gdvecn, GDVECN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const int32_t *field_num);

void F77_FUNC(gdmdlc, GDMDLC) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const double *b1,
    const char *in_field2, const int32_t *in_field2_l, const double *m2,
    const double *b2, const char *in_field3, const int32_t *in_field3_l,
    const double *m3, const double *b3);

void F77_FUNC(gdmdpn, GDMDPN) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const double *a1,
    const double *a2, const double *a3, const double *a4, const double *a5);

void F77_FUNC(gdmdlt, GDMDLT) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *table, const int32_t *table_l);

void F77_FUNC(gdmdbt, GDMDBT) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits);

void F77_FUNC(gdmdsb, GDMDSB) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits);

void F77_FUNC(gdmdmt, GDMDMT) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdmdph, GDMDPH) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift);

void F77_FUNC(gdadco, GDADCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *data_type, const void *value, const int32_t *fragment_index);

void F77_FUNC(gdmdco, GDMDCO) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *data_type, const void *value);

void F77_FUNC(gdadst, GDADST) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *value, const int32_t *value_l,
    const int32_t *fragment_index);

void F77_FUNC(gdmdst, GDMDST) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *value, const int32_t *value_l);

void F77_FUNC(gdadsp, GDADSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const int32_t *fragment_index);

void F77_FUNC(gdmdsp, GDMDSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const char *parent, const int32_t *parent_l);

void F77_FUNC(gdfrgi, GDFRGI) (int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgtco, GDGTCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *return_type, void *data_out);

void F77_FUNC(gdgtca, GDGTCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *return_type, void *data_out);

void F77_FUNC(gdcaln, GDCALN) (int32_t *len, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgcas, GDGCAS) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *start, const int32_t *n,
    const int32_t *return_type, void *data_out);

void F77_FUNC(gdgtst, GDGTST) (int32_t *size, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l, const int32_t *len,
    char *data_out);

void F77_FUNC(gdptco, GDPTCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const void *data_in);

void F77_FUNC(gdptca, GDPTCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const void *data_in);

void F77_FUNC(gdptst, GDPTST) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *len, const char *data_out);

void F77_FUNC(gdnmfd, GDNMFD) (int32_t *nfields, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l);

void F77_FUNC(gdnmft, GDNMFT) (int32_t *nfields, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *type);

void F77_FUNC(gdnmve, GDNMVE) (int32_t *nvectors, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l);

void F77_FUNC(gdgeco, GDGECO) (int32_t *data_type, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdmfnx, GDMFNX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l);

void F77_FUNC(gdmfdn, GDMFDN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *field_num);

void F77_FUNC(gdmfdt, GDMFDT) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *type, const int32_t *field_num);

void F77_FUNC(gdmven, GDMVEN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *field_num);

void F77_FUNC(gddscd, GDDSCD) (const int32_t *dirfile);

void F77_FUNC(gdcopn, GDCOPN) (int32_t *dirfile, const char *dirfilename,
    const int32_t *dirfilename_l, const int32_t *flags,
    const _GDF_callback_t callback);

void F77_FUNC(gdclbk, GDCLBK) (const int32_t *dirfile,
    const _GDF_callback_t callback);

void F77_FUNC(gdalbt, GDALBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits);

void F77_FUNC(gdalco, GDALCO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type);

void F77_FUNC(gdalca, GDALCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *array_len);

void F77_FUNC(gdallc, GDALLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const double *b1,
    const char *in_field2, const int32_t *in_field2_l, const double *m2,
    const double *b2, const char *in_field3, const int32_t *in_field3_l,
    const double *m3, const double *b3);

void F77_FUNC(gdalpn, GDALPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const double *a1,
    const double *a2, const double *a3, const double *a4, const double *a5);

void F77_FUNC(gdalmt, GDALMT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdalph, GDALPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift);

void F77_FUNC(gdgenc, GDGENC) (int32_t *encoding, const int32_t *dirfile,
    const int32_t *fragment);

void F77_FUNC(gdgend, GDGEND) (int32_t *endianness, const int32_t *dirfile,
    const int32_t *fragment);

void F77_FUNC(gdname, GDNAME) (char *name, int32_t *name_l,
    const int32_t *dirfile);

void F77_FUNC(gdpfrg, GDPFRG) (int32_t *parent, const int32_t *dirfile,
    const int32_t *fragment);

void F77_FUNC(gdprot, GDPROT) (const int32_t *dirfile, const int32_t *fragment, 
    const int32_t *protection_level);

void F77_FUNC(gdgprt, GDGPRT) (int32_t *protection_level,
    const int32_t *dirfile, const int32_t *fragment);

void F77_FUNC(gdrwfn, GDRWFN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdrefe, GDREFE) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdaenc, GDAENC) (const int32_t *dirfile, const int32_t *encoding,
    const int32_t *fragment, const int32_t *recode);

void F77_FUNC(gdaend, GDAEND) (const int32_t *dirfile,
    const int32_t *endianness, const int32_t *fragment, const int32_t *recode);

void F77_FUNC(gdallt, GDALLT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *table, const int32_t *table_l,
    const int32_t *recode);

void F77_FUNC(gdalrw, GDALRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const int32_t *recode);

void F77_FUNC(gdalsp, GDALSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const int32_t *fragment_index);

void F77_FUNC(gddele, GDDELE) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *flags);

void F77_FUNC(gdmlsp, GDMLSP) (const int32_t *dirfile, const char *spec,
    const int32_t *spec_l, const char *parent, const int32_t *parent_l,
    const int32_t *recode);

void F77_FUNC(gdmove, GDMOVE) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *new_fragment,
    const int32_t *move_data);

void F77_FUNC(gdrenm, GDRENM) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *new_name,
    const int32_t *new_name_l, const int32_t *move_data);

void F77_FUNC(gduinc, GDUINC) (const int32_t *dirfile, const int32_t *fragment,
    const int32_t *del);

void F77_FUNC(gdafof, GDAFOF) (const int32_t *dirfile, const int32_t *offset,
    const int32_t *fragment, const int32_t *recode);

void F77_FUNC(gdgfof, GDGFOF) (int32_t *offset, const int32_t *dirfile,
    const int32_t *fragment);

void F77_FUNC(gdadcl, GDADCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char *in_field2, const int32_t *in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char *in_field3, const int32_t *in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3),
    const int32_t *fragment_index);

void F77_FUNC(gdadcp, GDADCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5),
    const int32_t *fragment_index);

void F77_FUNC(gdmdcl, GDMDCL) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char *in_field2, const int32_t *in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char *in_field3, const int32_t *in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3));

void F77_FUNC(gdmdcp, GDMDCP) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5));

void F77_FUNC(gdntyp, GDNTYP) (int32_t *type, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdalcl, GDALCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const GD_DCOMPLEXP(b1),
    const char *in_field2, const int32_t *in_field2_l, const GD_DCOMPLEXP(m2),
    const GD_DCOMPLEXP(b2), const char *in_field3, const int32_t *in_field3_l,
    const GD_DCOMPLEXP(m3), const GD_DCOMPLEXP(b3));

void F77_FUNC(gdalcp, GDALCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const GD_DCOMPLEXP(a1),
    const GD_DCOMPLEXP(a2), const GD_DCOMPLEXP(a3),
    const GD_DCOMPLEXP(a4), const GD_DCOMPLEXP(a5));

void F77_FUNC(gdenfl, GDENFL) (int32_t *flags, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdvldt, GDVLDT) (int32_t *valid, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdfnum, GDFNUM) (double *framenum, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l, const double *value);

void F77_FUNC(gdfnss, GDFNSS) (double *framenum, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l, const double *value,
    const int32_t *start, const int32_t *end);

void F77_FUNC(gdgsca, GDGSCA) (char *scalar, int32_t *scalar_l,
    int32_t *scalar_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *index);

void F77_FUNC(gdasca, GDASCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *index, const char *scalar,
    const int32_t *scalar_l, int32_t *scalar_index, int32_t *recode);

void F77_FUNC(gdalsb, GDALSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits);

void F77_FUNC(gdadsb, GDADSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const int32_t *numbits,
    const int32_t *fragment_index);

void F77_FUNC(gdinvd, GDINVD) (int32_t *dirfile);

void F77_FUNC(gdstdv, GDSTDV) (int32_t *vers, const int32_t *dirfile);

void F77_FUNC(gdgbof, GDGBOF) (int32_t *bof, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgeof, GDGEOF) (int32_t *eof, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgedv, GDGEDV) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgerc, GDGERC) (char *in_field, int32_t *in_field_l,
    double *dividend, int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdgecr, GDGECR) (char *in_field, int32_t *in_field_l,
    GD_DCOMPLEXP(cdividend), int32_t *fragment_index, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdaddv, GDADDV) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index);

void F77_FUNC(gdadrc, GDADRC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const int32_t *fragment_index);

void F77_FUNC(gdadcr, GDADCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *cdividend,
    const int32_t *fragment_index);

void F77_FUNC(gdmddv, GDMDDV) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdrfrg, GDRFRG) (const int32_t *dirfile, const int32_t *fragment);

void F77_FUNC(gdmdrc, GDMDRC) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend);

void F77_FUNC(gdmdcr, GDMDCR) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(cdividend));

void F77_FUNC(gdaldv, GDALDV) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdalrc, GDALRC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const double *dividend);

void F77_FUNC(gdalcr, GDALCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(cdividend));

void F77_FUNC(gdaprt, GDAPRT) (const int32_t *dirfile,
    const int32_t *protection_level, const int32_t *fragment);

void F77_FUNC(gdgeca, GDGECA) (int32_t *data_type, int32_t *array_len,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdadca, GDADCA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type, int32_t *array_len,
    const int32_t *data_type, const void *value, const int32_t *fragment_index);

void F77_FUNC(gdmdca, GDMDCA) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *const_type,
    const int32_t *array_len, const int32_t *data_type, const void *value);

void F77_FUNC(gdpcas, GDPCAS) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *start, const int32_t *n,
    const int32_t *data_type, const void *data_in);

void F77_FUNC(gdseek, GDSEEK) (int32_t *pos, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l,
    const int32_t *frame_num, const int32_t *sample_num, const int32_t *flags);

void F77_FUNC(gdtell, GDTELL) (int32_t *pos, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdcons, GDCONS) (void *value, const int32_t *dirfile,
    const int32_t *return_type, const int32_t *field_num);

void F77_FUNC(gdmcos, GDMCOS) (void *value, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *return_type,
    const int32_t *field_num);

void F77_FUNC(gdstrs, GDSTRS) (char *value, int32_t *value_l,
    const int32_t *dirfile, const int32_t *field_num);

void F77_FUNC(gdmsts, GDMSTS) (void *value, int32_t *value_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *field_num);

void F77_FUNC(gdstrx, GDSTRX) (int32_t *max, const int32_t *dirfile);

void F77_FUNC(gdmstx, GDMSTX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l);

void F77_FUNC(gdadwd, GDADWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop,
    const void *threshold, const int32_t *fragment_index);

void F77_FUNC(gdmdwd, GDMDWD) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold);

void F77_FUNC(gdalwd, GDALWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold);

void F77_FUNC(gdadmx, GDADMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const int32_t *period,
    const int32_t *fragment_index);

void F77_FUNC(gdmdmx, GDMDMX) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const int32_t *period);

void F77_FUNC(gdalmx, GDALMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const int32_t *period);

void F77_FUNC(gdsync, GDSYNC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdrclo, GDRCLO) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdinca, GDINCA) (const int32_t *dirfile, const char *file,
    const int32_t *file_l, const int32_t *fragment_index, const char *prefix,
    const int32_t *prefix_l, const char *suffix, const int32_t *suffix_l,
    const int32_t *flags);

void F77_FUNC(gdincn, GDINCN) (const int32_t *dirfile, const char *file,
    const int32_t *file_l, const int32_t *fragment_index, const char *nsin,
    const int32_t *ns_l, const int32_t *flags);

void F77_FUNC(gdmova, GDMOVA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *new_fragment);

void F77_FUNC(gdmdal, GDMDAL) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *target, const int32_t *target_l);

void F77_FUNC(gduhid, GDUHID) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdhide, GDHIDE) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdhidn, GDHIDN) (int32_t *result, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdfraf, GDFRAF) (char *prefix, int32_t *prefix_l, char *suffix,
    int32_t *suffix_l, const int32_t *dirfile, const int32_t *index);

void F77_FUNC(gdfrns, GDFRNS) (char *nsout, int32_t *nsout_l,
    const int32_t *dirfile, const int32_t *index, const char *nsin,
    const int32_t *nsin_l);

void F77_FUNC(gdaafx, GDAAFX) (const int32_t *dirfile, const int32_t *index,
    const char *prefix, const int32_t *prefix_l, const char *suffix,
    const int32_t *suffix_l);

void F77_FUNC(gdalss, GDALSS) (char *alias, int32_t *alias_l,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *num);

void F77_FUNC(gdnals, GDNALS) (int32_t *nalias, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdalsx, GDALSX) (int32_t *max, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdatrg, GDATRG) (char *target, int32_t *target_l,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdadal, GDADAL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *target, const int32_t *target_l,
    const int32_t *fragment_index);

void F77_FUNC(gdnocb, GDNOCB) (const int32_t *dirfile);

void F77_FUNC(gddsyn, GDDSYN) (int32_t *desync, const int32_t *dirfile,
    const int32_t *flags);

void F77_FUNC(gdflag, GDFLAG) (int32_t *flags, const int32_t *dirfile,
    const int32_t *set, const int32_t *reset);

void F77_FUNC(gdvbpx, GDVBPX) (const int32_t *dirfile, const char *prefix,
    const int32_t *prefix_l);

void F77_FUNC(gdmxlb, GDMXLB) (const int32_t *dirfile, const int32_t *lookback);

void F77_FUNC(gdnent, GDNENT) (int32_t *nentries, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *type,
    const int32_t *flags);

void F77_FUNC(gdentx, GDENTX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l, const int32_t *type,
    const int32_t *flags);

void F77_FUNC(gdentn, GDENTN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *parent, const int32_t *parent_l,
    const int32_t *type, const int32_t *flags, const int32_t *field_num);

void F77_FUNC(gdgewd, GDGEWD) (char *in_field, int32_t *in_field_l,
    char *check_field, int32_t *check_field_l, int32_t *windop,
    int32_t *ithreshold, double *rthreshold, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdecnt, GDECNT) (int32_t *error_count, const int32_t *dirfile);

void F77_FUNC(gdtoke, GDTOKE) (char *toke, int32_t *toke_l,
    const int32_t *dirfile, const char *string, const int32_t *string_l);

void F77_FUNC(gdgemx, GDGEMX) (char *in_field, int32_t *in_field_l,
    char *count_field, int32_t *count_field_l, int32_t *val, int32_t *period,
    int32_t *fragment_index, const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdlttn, GDLTTN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdasrw, GDASRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const char *spf_scalar, const int32_t *spf_scalar_l,
    const int32_t *spf_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdasbt, GDASBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind,
    const int32_t *fragment_index);

void F77_FUNC(gdaslc, GDASLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind, const double *b1,
    const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const double *m2, const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind, const double *b2,
    const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const double *m3, const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind, const double *b3,
    const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdascl, GDASCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind,
    const GD_DCOMPLEXP(b1), const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const GD_DCOMPLEXP(m2), const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind,
    const GD_DCOMPLEXP(b2), const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const GD_DCOMPLEXP(m3), const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind,
    const GD_DCOMPLEXP(b3), const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdascl, GDASCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind,
    const GD_DCOMPLEXP(b1), const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const GD_DCOMPLEXP(m2), const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind,
    const GD_DCOMPLEXP(b2), const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const GD_DCOMPLEXP(m3), const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind,
    const GD_DCOMPLEXP(b3), const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdasph, GDASPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift,
    const char *shift_scalar, const int32_t *shift_scalar_l,
    const int32_t *shift_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdaswd, GDASWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold,
    const char *threshold_scalar, const int32_t *threshold_scalar_l,
    const int32_t *threshold_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdasmx, GDASMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const char *val_scalar,
    const int32_t *val_scalar_l, const int32_t *val_scalar_ind,
    const int32_t *period, const char *period_scalar,
    const int32_t *period_scalar_l, const int32_t *period_scalar_ind,
    const int32_t *fragment_index);

void F77_FUNC(gdasrc, GDASCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdascr, GDASCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdlsrw, GDLSRW) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *data_type, const int32_t *spf,
    const char *spf_scalar, const int32_t *spf_scalar_l,
    const int32_t *spf_scalar_ind, const int32_t *recode);

void F77_FUNC(gdlsbt, GDLSBT) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind);

void F77_FUNC(gdlslc, GDLSLC) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const double *m1, const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind, const double *b1,
    const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const double *m2, const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind, const double *b2,
    const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const double *m3, const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind, const double *b3,
    const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind);

void F77_FUNC(gdlscl, GDLSCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind,
    const GD_DCOMPLEXP(b1), const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const GD_DCOMPLEXP(m2), const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind,
    const GD_DCOMPLEXP(b2), const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const GD_DCOMPLEXP(m3), const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind,
    const GD_DCOMPLEXP(b3), const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind);

void F77_FUNC(gdlscl, GDLSCL) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *n_fields, const char *in_field1,
    const int32_t *in_field1_l, const GD_DCOMPLEXP(m1), const char *m1_scalar,
    const int32_t *m1_scalar_l, const int32_t *m1_scalar_ind,
    const GD_DCOMPLEXP(b1), const char *b1_scalar, const int32_t *b1_scalar_l,
    const int32_t *b1_scalar_ind, const char *in_field2,
    const int32_t *in_field2_l, const GD_DCOMPLEXP(m2), const char *m2_scalar,
    const int32_t *m2_scalar_l, const int32_t *m2_scalar_ind,
    const GD_DCOMPLEXP(b2), const char *b2_scalar, const int32_t *b2_scalar_l,
    const int32_t *b2_scalar_ind, const char *in_field3,
    const int32_t *in_field3_l, const GD_DCOMPLEXP(m3), const char *m3_scalar,
    const int32_t *m3_scalar_l, const int32_t *m3_scalar_ind,
    const GD_DCOMPLEXP(b3), const char *b3_scalar, const int32_t *b3_scalar_l,
    const int32_t *b3_scalar_ind);

void F77_FUNC(gdlsph, GDLSPH) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *shift,
    const char *shift_scalar, const int32_t *shift_scalar_l,
    const int32_t *shift_scalar_ind);

void F77_FUNC(gdlswd, GDLSWD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *check_field,
    const int32_t *check_field_l, const int32_t *windop, const void *threshold,
    const char *threshold_scalar, const int32_t *threshold_scalar_l,
    const int32_t *threshold_scalar_ind);

void F77_FUNC(gdlsmx, GDLSMX) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const char *count_field,
    const int32_t *count_field_l, const int32_t *val, const char *val_scalar,
    const int32_t *val_scalar_l, const int32_t *val_scalar_ind,
    const int32_t *period, const char *period_scalar,
    const int32_t *period_scalar_l, const int32_t *period_scalar_ind);

void F77_FUNC(gdlsrc, GDLSCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind);

void F77_FUNC(gdlscr, GDLSCR) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const double *dividend,
    const char *dividend_scalar, const int32_t *dividend_scalar_l,
    const int32_t *dividend_scalar_ind);

void F77_FUNC(gdencs,GDENCS) (int32_t *ret, const int32_t *encoding);

void F77_FUNC(gdsarx, GDSARX) (int32_t *max, const int32_t *dirfile);

void F77_FUNC(gdmsax, GDMSAX) (int32_t *max, const int32_t *dirfile,
    const char *parent, const int32_t *parent_l);

void F77_FUNC(gdgesa, GDGESA) (int32_t *array_len, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgtsa, GDGTSA) (char *value, int32_t *value_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    const int32_t *index);

void F77_FUNC(gdarln, GDARLN) (int32_t *len, const int32_t *dirfile,
    const char *field_code, const int32_t *field_code_l);

void F77_FUNC(gdptsa, GDPTSA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *index, const char *value,
    const int32_t *value_l);

void F77_FUNC(gdadsa, GDADSA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, int32_t *array_len,
    const int32_t *fragment_index);

void F77_FUNC(gdmdsa, GDMDSA) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const int32_t *array_len);

void F77_FUNC(gdalsa, GDALSA) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *array_len);

void F77_FUNC(gdgeid, GDGEID) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdgesd, GDGESD) (char *in_field1, int32_t *in_field1_l,
    char *in_field2, int32_t *in_field2_l, int32_t *fragment_index,
    const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l);

void F77_FUNC(gdadid, GDADID) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index);

void F77_FUNC(gdadsd, GDADSD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l, const int32_t *fragment_index);

void F77_FUNC(gdmdid, GDMDID) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdmdsd, GDMDSD) (const int32_t *dirfile, const char *parent,
    const int32_t *parent_l, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdalid, GDALID) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdalsd, GDALSD) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field1,
    const int32_t *in_field1_l, const char *in_field2,
    const int32_t *in_field2_l);

void F77_FUNC(gdgstd, GDGSTD) (char *value, int32_t *value_l,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample);

void F77_FUNC(gdgstp, GDGSTP) (int32_t *n_read, char *data,
    const int32_t *dirfile, const char *field_code, const int32_t *field_code_l,
    const int32_t *first_frame, const int32_t *first_sample,
    const int32_t *num_frames, const int32_t *num_samples);

void F77_FUNC(gdaspn, GDASPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind, const double *a1,
    const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const double *a2, const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind, const double *a3,
    const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const double *a4, const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind, const double *a5,
    const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdascp, GDASCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind,
    const GD_DCOMPLEXP(a1), const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const GD_DCOMPLEXP(a2), const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind,
    const GD_DCOMPLEXP(a3), const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const GD_DCOMPLEXP(a4), const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind,
    const GD_DCOMPLEXP(a5), const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind, const int32_t *fragment_index);

void F77_FUNC(gdassb, GDASSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind,
    const int32_t *fragment_index);

void F77_FUNC(gdlspn, GDLSPN) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const double *a0, const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind,
    const double *a1, const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const double *a2, const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind,
    const double *a3, const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const double *a4, const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind,
    const double *a5, const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind);

void F77_FUNC(gdlscp, GDLSCP) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const int32_t *poly_ord, const char *in_field,
    const int32_t *in_field_l, const GD_DCOMPLEXP(a0), const char *a0_scalar,
    const int32_t *a0_scalar_l, const int32_t *a0_scalar_ind,
    const GD_DCOMPLEXP(a1), const char *a1_scalar, const int32_t *a1_scalar_l,
    const int32_t *a1_scalar_ind, const GD_DCOMPLEXP(a2), const char *a2_scalar,
    const int32_t *a2_scalar_l, const int32_t *a2_scalar_ind,
    const GD_DCOMPLEXP(a3), const char *a3_scalar, const int32_t *a3_scalar_l,
    const int32_t *a3_scalar_ind, const GD_DCOMPLEXP(a4), const char *a4_scalar,
    const int32_t *a4_scalar_l, const int32_t *a4_scalar_ind,
    const GD_DCOMPLEXP(a5), const char *a5_scalar, const int32_t *a5_scalar_l,
    const int32_t *a5_scalar_ind);

void F77_FUNC(gdlssb, GDLSSB) (const int32_t *dirfile, const char *field_code,
    const int32_t *field_code_l, const char *in_field,
    const int32_t *in_field_l, const int32_t *bitnum, const char *bitnum_scalar,
    const int32_t *bitnum_scalar_l, const int32_t *bitnum_scalar_ind,
    const int32_t *numbits, const char *numbits_scalar,
    const int32_t *numbits_scalar_l, const int32_t *numbits_scalar_ind);

void F77_FUNC(gdxstp, GDXSTP) (char *value, int32_t *value_l, const char *data,
    const int32_t *index);

void F77_FUNC(gddstp, GDDSTP) (char *data);

void F77_FUNC(gdmatx, GDMATX) (int32_t *max, const int32_t *dirfile,
    const char *regex, const int32_t *regex_l, const int32_t *fragment,
    const int32_t *type, const int32_t *flags);

void F77_FUNC(gdmatn, GDMATN) (char *name, int32_t *name_l,
    const int32_t *dirfile, const char *regex, const int32_t *regex_l,
    const int32_t *fragment, const int32_t *type, const int32_t *flags,
    const int32_t *field_num);

void F77_FUNC(gdnmat, GDNMAT) (int32_t *nentries, const int32_t *dirfile,
    const char *regex, const int32_t *regex_l, const int32_t *fragment,
    const int32_t *type, const int32_t *flags);

#ifdef __cplusplus
}
#endif
