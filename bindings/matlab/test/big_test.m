% Copyright (C) 2013 D. V. Wiebe
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This file is part of the GetData project.
%
% GetData is free software; you can redistribute it and/or modify it under
% the terms of the GNU Lesser General Public License as published by the
% Free Software Foundation; either version 2.1 of the License, or (at your
% option) any later version.
%
% GetData is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
% License for more details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with GetData; if not, write to the Free Software Foundation, Inc.,
% 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
function big_test
try
  ne = 0;

  filedir='dirfile';
  format=strcat(filedir, '/format');
  format1=strcat(filedir, '/format1');
  form2=strcat(filedir, '/form2');
  data=strcat(filedir, '/data');
  new1=strcat(filedir, '/new1');
  format_data =[...
  '/ENDIAN little\n'...
  'data RAW INT8 8\n'...
  'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const\n'...
  '/META data mstr STRING "This is a string constant."\n'...
  '/META data mconst CONST COMPLEX128 3.3;4.4\n'...
  '/META data mlut LINTERP DATA ./lut\n'...
  'const CONST FLOAT64 5.5\n'...
  'carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6\n'...
  'linterp LINTERP data ./lut\n'...
  'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const\n'...
  'bit BIT data 3 4\n'...
  'sbit SBIT data 5 6\n'...
  'mplex MPLEX data sbit 1 10\n'...
  'mult MULTIPLY data sbit\n'...
  'div DIVIDE mult bit\n'...
  'recip RECIP div 6.5;4.3\n'...
  'phase PHASE data 11\n'...
  'window WINDOW linterp mult LT 4.1\n'...
  '/ALIAS alias data\n'...
  'string STRING "Zaphod Beeblebrox"\n'];
  form2_data = 'const2 CONST INT8 -19\n';

  fields = {'INDEX'; 'alias'; 'bit'; 'carray'; 'const'; 'data'; 'div'; ...
  'lincom'; 'linterp'; 'mplex'; 'mult'; 'phase'; 'polynom'; 'recip'; 'sbit'; ...
  'string'; 'window'};
  nfields = 17;

  % Create the dirfile
  mkdir(filedir);

  fid=fopen(format, 'w');
  fprintf(fid, format_data);
  fclose(fid);

  fid=fopen(form2, 'w');
  fprintf(fid, form2_data);
  fclose(fid);

  data_data=0:1:80;
  fid=fopen(data, 'w');
  fwrite(fid, data_data);
  fclose(fid);

  % 140: getdata_constants check
  GD = getdata_constants();
  ne = ne + check_num(140, GD.RDWR, 1);

  % 0: gd_error check
  try
    D = gd_open('x');
  catch exc
    ne = ne + check_exc(exc, 0, 'Open');
  end

  % 1: gd_open check
  try
    D = gd_open(filedir, GD.RDWR);
  catch exc
    ne = ne + check_ok(exc, 1);
  end

  % 2: getdata check
  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0);
    ne = ne + check_array(2, d, [40:47]);
  catch exc
    ne = ne + check_ok(exc, 2);
  end

  % 108: getdata check (complex128)
  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0, GD.COMPLEX128);
    ne = ne + check_array(2, d, [ complex(40,0), complex(41,0), ...
    complex(42,0), complex(43,0), complex(44,0), complex(45,0), ...
    complex(46,0), complex(47,0) ]);
  catch exc
    ne = ne + check_ok(exc, 108);
  end

  % 3: gd_get_constant check
  try
    d = gd_get_constant(D, 'const');
    ne = ne + check_num(3, d, 5.5);
  catch exc
    ne = ne + check_ok(exc, 3);
  end

  % 6: gd_nfields check
  try
    d = gd_nfields(D);
    ne = ne + check_num(6, d, nfields);
  catch exc
    ne = ne + check_ok(exc, 6);
  end

  % 8: gd_field_list check
  try
    d = gd_field_list(D);
    ne = ne + check_sarray(8, d, fields);
  catch exc
    ne = ne + check_ok(exc, 8);
  end

  % 9: gd_nmfields check
  try
    d = gd_nmfields(D, 'data');
    ne = ne + check_num(8, d, 3);
  catch exc
    ne = ne + check_ok(exc, 9);
  end

  % 10: gd_mfield_list check
  try
    d = gd_mfield_list(D, 'data');
    ne = ne + check_sarray(10, d, { 'mstr'; 'mconst'; 'mlut' });
  catch exc
    ne = ne + check_ok(exc, 10);
  end

  % 11: gd_nframes check
  try
    d = gd_nframes(D);
    ne = ne + check_num(11, d, 10);
  catch exc
    ne = ne + check_ok(exc, 11);
  end

  % 12: gd_spf check
  try
    d = gd_spf(D, 'data');
    ne = ne + check_num(12, d, 8);
  catch exc
    ne = ne + check_ok(exc, 12);
  end

  % 13: gd_putdata check
  p = [ 13, 14, 15, 16 ];
  try
    d = gd_putdata(D, 'data', 5, 1, p);
    ne = ne + check_num(13, d, 4);
  catch exc
    ne = ne + check_ok2(exc, 13, 1);
  end

  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0);
  catch exc
    ne = ne + check_ok2(exc, 13, 2);
  end

  % 124: gd_putdata check (complex)
  p = [ complex(33,0), complex(34,0), complex(35,0), complex(36,0) ];
  try
    d = gd_putdata(D, 'data', 5, 1, p);
    ne = ne + check_num(124, d, 4);
  catch exc
    ne = ne + check_ok2(exc, 124, 1);
  end

  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0);
    ne = ne + check_array(124, d, [40, 33, 34, 35, 36, 45, 46, 47]);
  catch exc
    ne = ne + check_ok2(exc, 124, 2);
  end

  % 14: error_string
  try
    gd_getdata(D, 'x', 1, 0, 1, 0);
  catch exc
    ne = ne + check_exc(exc, 14, 'BadCode');
  end
  ne = ne + check_num(14, gd_error(D), GD.E_BAD_CODE);
  ne = ne + check_string(14, gd_error_string(D), 'Field not found: x');

  % 15: entry_type
  try
    d = gd_entry_type(D, 'data');
    ne = ne + check_num(15, d, GD.RAW_ENTRY);
  catch exc
    ne = ne + check_ok(exc, 15);
  end

  % 16: entry (raw) check
  try
    d = gd_entry(D, 'data');
    ne = ne + check_string2(16, 1, d.field, 'data');
    ne = ne + check_num2(16, 2, d.field_type, GD.RAW_ENTRY);
    ne = ne + check_num2(16, 3, d.fragment_index, 0);
    ne = ne + check_num2(16, 4, d.data_type, GD.INT8);
  catch exc
    ne = ne + check_ok(exc, 16);
  end

  % 18: entry (lincom) check
  try
    d = gd_entry(D, 'lincom');
    ne = ne + check_string2(18, 1, d.field, 'lincom');
    ne = ne + check_num2(18, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(18, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(18, 4, d.in_fields, { 'data'; 'INDEX'; 'linterp' });
    ne = ne + check_array2(18, 5, d.m, [ 1.1, 2.2, 5.5 ]);
    ne = ne + check_array2(18, 6, d.b, [ 2.2, complex(3.3,4.4), 5.5 ]);
    ne = ne + check_sarray2(18, 7, d.scalar, {0; 0; 'const'; 0; 0; 'const'});
    ne = ne + check_array2(18, 8, d.scalar_ind, [0, 0, -1, 0, 0, -1]);
  catch exc
    ne = ne + check_ok(exc, 18);
  end

  % 20: entry (polynom) check
  try
    d = gd_entry(D, 'polynom');
    ne = ne + check_string2(20, 1, d.field, 'polynom');
    ne = ne + check_num2(20, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(20, 3, d.fragment_index, 0);
    ne = ne + check_string2(20, 4, d.in_fields, 'data');
    ne = ne + check_array2(20, 5, d.a, [ 1.1, 2.2, 2.2, complex(3.3, 4.4), ...
    5.5, 5.5]);
    ne = ne + check_sarray2(20, 6, d.scalar, {0; 0; 0; 0; 'const'; 'const'});
    ne = ne + check_array2(20, 7, d.scalar_ind, [0, 0, 0, 0, -1, -1]);
  catch exc
    ne = ne + check_ok(exc, 20);
  end

  % 21: entry (linterp) check
  try
    d = gd_entry(D, 'linterp');
    ne = ne + check_string2(21, 1, d.field, 'linterp');
    ne = ne + check_num2(21, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(21, 3, d.fragment_index, 0);
    ne = ne + check_string2(21, 4, d.in_fields, 'data');
    ne = ne + check_string2(21, 5, d.table, './lut');
  catch exc
    ne = ne + check_ok(exc, 21);
  end

  % 22: entry (bit) check
  try
    d = gd_entry(D, 'bit');
    ne = ne + check_string2(22, 1, d.field, 'bit');
    ne = ne + check_num2(22, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(22, 3, d.fragment_index, 0);
    ne = ne + check_string2(22, 4, d.in_fields, 'data');
    ne = ne + check_num2(22, 5, d.bitnum, 3);
    ne = ne + check_num2(22, 6, d.numbits, 4);
  catch exc
    ne = ne + check_ok(exc, 22);
  end

  % 23: entry (sbit) check
  try
    d = gd_entry(D, 'sbit');
    ne = ne + check_string2(23, 1, d.field, 'sbit');
    ne = ne + check_num2(23, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(23, 3, d.fragment_index, 0);
    ne = ne + check_string2(23, 4, d.in_fields, 'data');
    ne = ne + check_num2(23, 5, d.bitnum, 5);
    ne = ne + check_num2(23, 6, d.numbits, 6);
  catch exc
    ne = ne + check_ok(exc, 23);
  end

  % 24: entry (mult) check
  try
    d = gd_entry(D, 'mult');
    ne = ne + check_string2(24, 1, d.field, 'mult');
    ne = ne + check_num2(24, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(24, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(24, 4, d.in_fields, { 'data'; 'sbit'; });
  catch exc
    ne = ne + check_ok(exc, 24);
  end

  % 25: entry (phase) check
  try
    d = gd_entry(D, 'phase');
    ne = ne + check_string2(25, 1, d.field, 'phase');
    ne = ne + check_num2(25, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(25, 3, d.fragment_index, 0);
    ne = ne + check_string2(25, 4, d.in_fields, 'data');
    ne = ne + check_num2(25, 5, d.shift, 11);
  catch exc
    ne = ne + check_ok(exc, 25);
  end

  % 26: entry (const) check
  try
    d = gd_entry(D, 'const');
    ne = ne + check_string2(26, 1, d.field, 'const');
    ne = ne + check_num2(26, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(26, 3, d.fragment_index, 0);
    ne = ne + check_num2(26, 5, d.const_type, GD.FLOAT64);
  catch exc
    ne = ne + check_ok(exc, 26);
  end

  % 134: entry (string) check
  try
    d = gd_entry(D, 'string');
    ne = ne + check_string2(134, 1, d.field, 'string');
    ne = ne + check_num2(134, 2, d.field_type, GD.STRING_ENTRY);
    ne = ne + check_num2(134, 3, d.fragment_index, 0);
  catch exc
    ne = ne + check_ok(exc, 134);
  end

  % 27: gd_fragment_index check
  try
    d = gd_fragment_index(D, 'data');
  catch exc
    ne = ne + check_ok(exc, 27);
  end
  ne = ne + check_num(27, d, 0);

  % 28: add_raw
  try
    gd_add_raw(D, 'new1', GD.FLOAT64, 3, 0);
  catch exc
    ne = ne + check_ok2(exc, 28, 1);
  end

  try
    d = gd_entry(D, 'new1');
    ne = ne + check_string2(28, 1, d.field, 'new1');
    ne = ne + check_num2(28, 2, d.field_type, GD.RAW_ENTRY);
    ne = ne + check_num2(28, 3, d.fragment_index, 0);
    ne = ne + check_num2(28, 4, d.data_type, GD.FLOAT64);
    ne = ne + check_num2(28, 4, d.spf, 3);
  catch exc
    ne = ne + check_ok2(exc, 28, 2);
  end

  % 29: add entry (lincom) check
  try
    gd_add_lincom(D, 'new2', { 'in1'; 'in2' }, [9.9, 7.7], [8.8, 6.6], 0);
  catch exc
    ne = ne + check_ok2(exc, 29, 1);
  end

  try
    d = gd_entry(D, 'new2');
    ne = ne + check_string2(29, 1, d.field, 'new2');
    ne = ne + check_num2(29, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(29, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(29, 4, d.in_fields, { 'in1'; 'in2' });
    ne = ne + check_array2(29, 5, d.m, [ 9.9, 7.7 ]);
    ne = ne + check_array2(29, 6, d.b, [ 8.8, 6.6 ]);
  catch exc
    ne = ne + check_ok2(exc, 29, 2);
  end

  % 32: add entry (polynom) check
  try
    gd_add_polynom(D, 'new5', 'in1', [3.9, 4.8, 5.7, complex(6.6,7.5)], 0);
  catch exc
    ne = ne + check_ok2(exc, 32, 1);
  end

  try
    d = gd_entry(D, 'new5');
    ne = ne + check_string2(32, 1, d.field, 'new5');
    ne = ne + check_num2(32, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(32, 3, d.fragment_index, 0);
    ne = ne + check_string2(32, 4, d.in_fields, 'in1');
    ne = ne + check_array2(32, 5, d.a, [3.9, 4.8, 5.7, complex(6.6,7.5)]);
  catch exc
    ne = ne + check_ok2(exc, 32, 2);
  end

  % 33: add entry (linterp) check
  try
    gd_add_linterp(D, 'new6', 'in', './some/table', 0);
  catch exc
    ne = ne + check_ok2(exc, 33, 1);
  end

  try
    d = gd_entry(D, 'new6');
    ne = ne + check_string2(33, 1, d.field, 'new6');
    ne = ne + check_num2(33, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(33, 3, d.fragment_index, 0);
    ne = ne + check_string2(33, 4, d.in_fields, 'in');
    ne = ne + check_string2(33, 5, d.table, './some/table');
  catch exc
    ne = ne + check_ok2(exc, 33, 2);
  end

  % 34: add entry (bit) check
  try
    gd_add_bit(D, 'new7', 'in', 13, 12, 0);
  catch exc
    ne = ne + check_ok2(exc, 34, 1);
  end

  try
    d = gd_entry(D, 'new7');
    ne = ne + check_string2(34, 1, d.field, 'new7');
    ne = ne + check_num2(34, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(34, 3, d.fragment_index, 0);
    ne = ne + check_string2(34, 4, d.in_fields, 'in');
    ne = ne + check_num2(34, 5, d.bitnum, 13);
    ne = ne + check_num2(34, 6, d.numbits, 12);
  catch exc
    ne = ne + check_ok2(exc, 34, 2);
  end

  % 35: add entry (sbit) check
  try
    gd_add_sbit(D, 'new8', 'in', 14, 15, 0);
  catch exc
    ne = ne + check_ok2(exc, 35, 1);
  end

  try
    d = gd_entry(D, 'new8');
    ne = ne + check_string2(35, 1, d.field, 'new8');
    ne = ne + check_num2(35, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(35, 3, d.fragment_index, 0);
    ne = ne + check_string2(35, 4, d.in_fields, 'in');
    ne = ne + check_num2(35, 5, d.bitnum, 14);
    ne = ne + check_num2(35, 6, d.numbits, 15);
  catch exc
    ne = ne + check_ok2(exc, 35, 2);
  end

  % 36: add entry (mult) check
  try
    gd_add_multiply(D, 'new9', 'in1', 'in2', 0);
  catch exc
    ne = ne + check_ok2(exc, 36, 1);
  end

  try
    d = gd_entry(D, 'new9');
    ne = ne + check_string2(36, 1, d.field, 'new9');
    ne = ne + check_num2(36, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(36, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(36, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 36, 2);
  end

  % 37: add entry (phase) check
  try
    gd_add_phase(D, 'new10', 'in1', 22, 0);
  catch exc
    ne = ne + check_ok2(exc, 37, 1);
  end

  try
    d = gd_entry(D, 'new10');
    ne = ne + check_string2(37, 1, d.field, 'new10');
    ne = ne + check_num2(37, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(37, 3, d.fragment_index, 0);
    ne = ne + check_string2(37, 4, d.in_fields, 'in1');
    ne = ne + check_num2(37, 5, d.shift, 22);
  catch exc
    ne = ne + check_ok2(exc, 37, 2);
  end

  % 38: add entry (const) check
  try
    gd_add_const(D, 'new11', GD.COMPLEX128, 2.6, 0);
  catch exc
    ne = ne + check_ok2(exc, 38, 1);
  end

  try
    d = gd_entry(D, 'new11');
    ne = ne + check_string2(38, 1, d.field, 'new11');
    ne = ne + check_num2(38, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(38, 3, d.fragment_index, 0);
    ne = ne + check_num2(38, 5, d.const_type, GD.COMPLEX128);
  catch exc
    ne = ne + check_ok2(exc, 38, 2);
  end

  try
    d = gd_get_constant(D, 'new11', GD.FLOAT64);
    ne = ne + check_num2(38, 6, d, 2.6);
  catch exc
    ne = ne + check_ok2(exc, 38, 3);
  end

  % 39: fragmentname
  try
    d = gd_fragmentname(D, 0);
    ne = ne + check_eostring(39, d, 'dirfile/format');
  catch exc
    ne = ne + check_ok(exc, 39);
  end

  % 40: nfragments
  try
    d = gd_nfragments(D);
    ne = ne + check_num(40, d, 1);
  catch exc
    ne = ne + check_ok(exc, 40);
  end

  % 41: include
  try
    d = gd_include(D, 'form2', 0, 0);
    ne = ne + check_num2(41, 1, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 41, 1);
  end

  try
    d = gd_get_constant(D, 'const2');
    ne = ne + check_num2(41, 2, d, -19);
  catch exc
    ne = ne + check_ok2(exc, 41, 2);
  end

  % 42: nfields_by_type
  try
    d = gd_nfields_by_type(D, GD.LINCOM_ENTRY);
    ne = ne + check_num(42, d, 2);
  catch exc
    ne = ne + check_ok(exc, 42);
  end

  % 43: field_list_by_type
  try
    d = gd_field_list_by_type(D, GD.LINCOM_ENTRY);
    ne = ne + check_sarray(43, d, { 'lincom', 'new2' });
  catch exc
    ne = ne + check_ok(exc, 43);
  end

  % 44: nfields_by_type
  try
    d = gd_nvectors(D);
    ne = ne + check_num(44, d, 22);
  catch exc
    ne = ne + check_ok(exc, 44);
  end

  % 45: vector_list check
  try
    d = gd_vector_list(D);
    ne = ne + check_sarray(45, d, ...
    {'INDEX'; 'alias'; 'bit'; 'data'; 'div'; 'lincom'; 'linterp'; 'mplex'; ...
    'mult'; 'new1'; 'new10'; 'new2'; 'new5'; 'new6'; 'new7'; 'new8'; 'new9'; ...
    'phase'; 'polynom'; 'recip'; 'sbit'; 'window'});
  catch exc
    ne = ne + check_ok(exc, 45);
  end

  % 46: madd entry (lincom) check
  try
    gd_madd_lincom(D, 'data', 'mnew1', { 'in1'; 'in2' }, [9.9, 7.7], ...
    [8.8, 6.6]);
  catch exc
    ne = ne + check_ok2(exc, 46, 1);
  end

  try
    d = gd_entry(D, 'data/mnew1');
    ne = ne + check_string2(46, 1, d.field, 'data/mnew1');
    ne = ne + check_num2(46, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(46, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(46, 4, d.in_fields, { 'in1'; 'in2' });
    ne = ne + check_array2(46, 5, d.m, [ 9.9, 7.7 ]);
    ne = ne + check_array2(46, 6, d.b, [ 8.8, 6.6 ]);
  catch exc
    ne = ne + check_ok2(exc, 46, 2);
  end

  % 49: madd polynom check
  try
    gd_madd_polynom(D, 'data', 'mnew4', 'in1', [3.9, 4.8, 5.7, ...
    complex(6.6,7.5)]);
  catch exc
    ne = ne + check_ok2(exc, 49, 1);
  end

  try
    d = gd_entry(D, 'data/mnew4');
    ne = ne + check_string2(49, 1, d.field, 'data/mnew4');
    ne = ne + check_num2(49, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(49, 3, d.fragment_index, 0);
    ne = ne + check_string2(49, 4, d.in_fields, 'in1');
    ne = ne + check_array2(49, 5, d.a, [3.9, 4.8, 5.7, complex(6.6,7.5)]);
  catch exc
    ne = ne + check_ok2(exc, 49, 2);
  end

  % 50: add entry (linterp) check
  try
    gd_madd_linterp(D, 'data', 'mnew6', 'in', './some/table');
  catch exc
    ne = ne + check_ok2(exc, 50, 1);
  end

  try
    d = gd_entry(D, 'data/mnew6');
    ne = ne + check_string2(50, 1, d.field, 'data/mnew6');
    ne = ne + check_num2(50, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(50, 3, d.fragment_index, 0);
    ne = ne + check_string2(50, 4, d.in_fields, 'in');
    ne = ne + check_string2(50, 5, d.table, './some/table');
  catch exc
    ne = ne + check_ok2(exc, 50, 2);
  end

  % 51: add entry (bit) check
  try
    gd_madd_bit(D, 'data', 'mnew7', 'in', 13, 12);
  catch exc
    ne = ne + check_ok2(exc, 51, 1);
  end

  try
    d = gd_entry(D, 'data/mnew7');
    ne = ne + check_string2(51, 1, d.field, 'data/mnew7');
    ne = ne + check_num2(51, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(51, 3, d.fragment_index, 0);
    ne = ne + check_string2(51, 4, d.in_fields, 'in');
    ne = ne + check_num2(51, 5, d.bitnum, 13);
    ne = ne + check_num2(51, 6, d.numbits, 12);
  catch exc
    ne = ne + check_ok2(exc, 34, 2);
  end

  % 52: add entry (sbit) check
  try
    gd_madd_sbit(D, 'data', 'mnew8', 'in', 14, 15);
  catch exc
    ne = ne + check_ok2(exc, 52, 1);
  end

  try
    d = gd_entry(D, 'data/mnew8');
    ne = ne + check_string2(52, 1, d.field, 'data/mnew8');
    ne = ne + check_num2(52, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(52, 3, d.fragment_index, 0);
    ne = ne + check_string2(52, 4, d.in_fields, 'in');
    ne = ne + check_num2(52, 5, d.bitnum, 14);
    ne = ne + check_num2(52, 6, d.numbits, 15);
  catch exc
    ne = ne + check_ok2(exc, 52, 2);
  end

  % 53: add entry (mult) check
  try
    gd_madd_multiply(D, 'data', 'mnew9', 'in1', 'in2');
  catch exc
    ne = ne + check_ok2(exc, 53, 1);
  end

  try
    d = gd_entry(D, 'data/mnew9');
    ne = ne + check_string2(53, 1, d.field, 'data/mnew9');
    ne = ne + check_num2(53, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(53, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(53, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 53, 2);
  end

  % 54: add entry (phase) check
  try
    gd_madd_phase(D, 'data', 'mnew10', 'in1', 22);
  catch exc
    ne = ne + check_ok2(exc, 54, 1);
  end

  try
    d = gd_entry(D, 'data/mnew10');
    ne = ne + check_string2(54, 1, d.field, 'data/mnew10');
    ne = ne + check_num2(54, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(54, 3, d.fragment_index, 0);
    ne = ne + check_string2(54, 4, d.in_fields, 'in1');
    ne = ne + check_num2(54, 5, d.shift, 22);
  catch exc
    ne = ne + check_ok2(exc, 54, 2);
  end

  % 55: madd entry (const) check
  try
    gd_madd_const(D, 'data', 'mnew11', GD.FLOAT64, 2.6);
  catch exc
    ne = ne + check_ok2(exc, 55, 1);
  end

  try
    d = gd_entry(D, 'data/mnew11');
    ne = ne + check_string2(55, 1, d.field, 'data/mnew11');
    ne = ne + check_num2(55, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(55, 3, d.fragment_index, 0);
    ne = ne + check_num2(55, 5, d.const_type, GD.FLOAT64);
  catch exc
    ne = ne + check_ok2(exc, 55, 2);
  end

  try
    d = gd_get_constant(D, 'data/mnew11');
    ne = ne + check_num2(55, 6, d, 2.6);
  catch exc
    ne = ne + check_ok2(exc, 55, 3);
  end

  % 56: get_string
  try
    d = gd_get_string(D, 'string');
    ne = ne + check_string(56, d, 'Zaphod Beeblebrox');
  catch exc
    ne = ne + check_ok(exc, 56);
  end

  % 57: add string
  try
    gd_add_string(D, 'new12', '---string---', 0);
  catch exc
    ne = ne + check_ok2(exc, 57, 1);
  end

  try
    d = gd_entry(D, 'new12');
    ne = ne + check_string2(57, 1, d.field, 'new12');
    ne = ne + check_num2(57, 2, d.field_type, GD.STRING_ENTRY);
    ne = ne + check_num2(57, 3, d.fragment_index, 0);
  catch exc
    ne = ne + check_ok2(exc, 57, 2);
  end

  try
    d = gd_get_string(D, 'new12');
    ne = ne + check_string(57, d, '---string---');
  catch exc
    ne = ne + check_ok(exc, 57);
  end

  % 58: madd string
  try
    gd_madd_string(D, 'data', 'mnew12', '---mstring---');
  catch exc
    ne = ne + check_ok2(exc, 58, 1);
  end

  try
    d = gd_entry(D, 'data/mnew12');
    ne = ne + check_string2(58, 1, d.field, 'data/mnew12');
    ne = ne + check_num2(58, 2, d.field_type, GD.STRING_ENTRY);
    ne = ne + check_num2(58, 3, d.fragment_index, 0);
  catch exc
    ne = ne + check_ok2(exc, 58, 2);
  end

  try
    d = gd_get_string(D, 'data/mnew12');
    ne = ne + check_string(58, d, '---mstring---');
  catch exc
    ne = ne + check_ok(exc, 58);
  end

  % 59: add_spec
  try
    gd_add_spec(D, 'lorem STRING "Lorem ipsum"', 0);
  catch exc
    ne = ne + check_ok2(exc, 59, 1);
  end

  try
    d = gd_get_string(D, 'lorem');
    ne = ne + check_string(59, d, 'Lorem ipsum');
  catch exc
    ne = ne + check_ok2(exc, 59, 2);
  end

  % 60: madd_spec
  try
    gd_madd_spec(D, 'ipsum STRING "dolor sit amet."', 'lorem');
  catch exc
    ne = ne + check_ok2(exc, 60, 1);
  end

  try
    d = gd_get_string(D, 'lorem/ipsum');
    ne = ne + check_string(60, d, 'dolor sit amet.');
  catch exc
    ne = ne + check_ok2(exc, 60, 2);
  end

  % 61: put_constant
  try
    gd_put_constant(D, 'const', 61);
  catch exc
    ne = ne + check_ok2(exc, 61, 1);
  end

  try
    d = gd_get_constant(D, 'const');
    ne = ne + check_num2(61, 6, d, 61);
  catch exc
    ne = ne + check_ok2(exc, 61, 2);
  end

  % 133: put_constant
  try
    gd_put_constant(D, 'new11', complex(133,134));
  catch exc
    ne = ne + check_ok2(exc, 133, 1);
  end

  try
    d = gd_get_constant(D, 'new11');
    ne = ne + check_num2(133, 6, d, complex(133,134));
  catch exc
    ne = ne + check_ok2(exc, 133, 2);
  end

  % 62: put_string
  try
    gd_put_string(D, 'string', 'Arthur Dent');
  catch exc
    ne = ne + check_ok2(exc, 62, 1);
  end

  try
    d = gd_get_string(D, 'string');
    ne = ne + check_string(62, d, 'Arthur Dent');
  catch exc
    ne = ne + check_ok2(exc, 62, 2);
  end

  % 63: nmfields_by_type
  try
    d = gd_nmfields_by_type(D, 'data', GD.LINCOM_ENTRY);
    ne = ne + check_num(63, d, 1);
  catch exc
    ne = ne + check_ok(exc, 63);
  end

  % 64: mfield_list_by_type
  try
    d = gd_mfield_list_by_type(D, 'data', GD.LINCOM_ENTRY);
    ne = ne + check_sarray(64, d, {'mnew1'});
  catch exc
    ne = ne + check_ok(exc, 64);
  end

  % 65: nmvectors
  try
    d = gd_nmvectors(D, 'data');
    ne = ne + check_num(65, d, 8);
  catch exc
    ne = ne + check_ok(exc, 65);
  end

  % 66: mvector_list
  try
    d = gd_mvector_list(D, 'data');
    ne = ne + check_sarray(66, d, {'mlut', 'mnew1', 'mnew4', 'mnew6', ...
    'mnew7', 'mnew8', 'mnew9', 'mnew10'});
  catch exc
    ne = ne + check_ok(exc, 66);
  end

  % 67: gd_alter_raw
  try
    gd_alter_raw(D, 'new1', GD.INT32, 4, 0);
  catch exc
    ne = ne + check_ok2(exc, 67, 1);
  end

  try
    d = gd_entry(D, 'new1');
    ne = ne + check_string2(67, 1, d.field, 'new1');
    ne = ne + check_num2(67, 2, d.field_type, GD.RAW_ENTRY);
    ne = ne + check_num2(67, 3, d.fragment_index, 0);
    ne = ne + check_num2(67, 4, d.data_type, GD.INT32);
    ne = ne + check_num2(67, 4, d.spf, 4);
  catch exc
    ne = ne + check_ok2(exc, 67, 2);
  end

  % 68: gd_alter_lincom
  try
    gd_alter_lincom(D, 'new2', {'in4'; 0; 'in6'}, [ 0.99, 11, 1.96 ], ...
    [ 7.8, 0.022, 0 ]);
  catch exc
    ne = ne + check_ok2(exc, 68, 1);
  end

  try
    d = gd_entry(D, 'new2');
    ne = ne + check_string2(68, 1, d.field, 'new2');
    ne = ne + check_num2(68, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(68, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(68, 4, d.in_fields, { 'in4'; 'in2'; 'in6' });
    ne = ne + check_array2(68, 5, d.m, [ 0.99, 11, 1.96 ]);
    ne = ne + check_array2(68, 6, d.b, [ 7.8, 0.022, 0 ]);
  catch exc
    ne = ne + check_ok2(exc, 68, 2);
  end

  % 70: gd_alter_polynom
  try
    gd_alter_polynom(D, 'new5', 0, [ 1.1, 1.2, 1.3, 1.4, 1.5 ]);
  catch exc
    ne = ne + check_ok2(exc, 70, 1);
  end

  try
    d = gd_entry(D, 'new5');
    ne = ne + check_string2(70, 1, d.field, 'new5');
    ne = ne + check_num2(70, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(70, 3, d.fragment_index, 0);
    ne = ne + check_string2(70, 4, d.in_fields, 'in1');
    ne = ne + check_array2(70, 5, d.a, [1.1, 1.2, 1.3, 1.4, 1.5]);
  catch exc
    ne = ne + check_ok2(exc, 70, 2);
  end

  % 72: gd_alter_linterp
  try
    gd_alter_linterp(D, 'new6', 'in3', 0, 0);
  catch exc
    ne = ne + check_ok2(exc, 72, 1);
  end

  try
    d = gd_entry(D, 'new6');
    ne = ne + check_string2(72, 1, d.field, 'new6');
    ne = ne + check_num2(72, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(72, 3, d.fragment_index, 0);
    ne = ne + check_string2(72, 4, d.in_fields, 'in3');
    ne = ne + check_string2(72, 5, d.table, './some/table');
  catch exc
    ne = ne + check_ok2(exc, 72, 2);
  end

  % 73: gd_alter_bit
  try
    gd_alter_bit(D, 'new7', 'in3', 3, 0);
  catch exc
    ne = ne + check_ok2(exc, 73, 1);
  end

  try
    d = gd_entry(D, 'new7');
    ne = ne + check_string2(73, 1, d.field, 'new7');
    ne = ne + check_num2(73, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(73, 3, d.fragment_index, 0);
    ne = ne + check_string2(73, 4, d.in_fields, 'in3');
    ne = ne + check_num2(73, 5, d.bitnum, 3);
    ne = ne + check_num2(73, 6, d.numbits, 12);
  catch exc
    ne = ne + check_ok2(exc, 73, 2);
  end

  % 74: gd_alter_sbit
  try
    gd_alter_sbit(D, 'new8', 'in3', 3, 9);
  catch exc
    ne = ne + check_ok2(exc, 74, 1);
  end

  try
    d = gd_entry(D, 'new8');
    ne = ne + check_string2(74, 1, d.field, 'new8');
    ne = ne + check_num2(74, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(74, 3, d.fragment_index, 0);
    ne = ne + check_string2(74, 4, d.in_fields, 'in3');
    ne = ne + check_num2(74, 5, d.bitnum, 3);
    ne = ne + check_num2(74, 6, d.numbits, 9);
  catch exc
    ne = ne + check_ok2(exc, 74, 2);
  end

  % 75: gd_alter_multiply
  try
    gd_alter_multiply(D, 'new9', 'in6', 0);
  catch exc
    ne = ne + check_ok2(exc, 75, 1);
  end

  try
    d = gd_entry(D, 'new9');
    ne = ne + check_string2(75, 1, d.field, 'new9');
    ne = ne + check_num2(75, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(75, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(75, 4, d.in_fields, { 'in6'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 75, 2);
  end

  % 76: alter phase
  try
    gd_alter_phase(D, 'new10', 'in2', 23);
  catch exc
    ne = ne + check_ok2(exc, 76, 1);
  end

  try
    d = gd_entry(D, 'new10');
    ne = ne + check_string2(76, 1, d.field, 'new10');
    ne = ne + check_num2(76, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(76, 3, d.fragment_index, 0);
    ne = ne + check_string2(76, 4, d.in_fields, 'in2');
    ne = ne + check_num2(76, 5, d.shift, 23);
  catch exc
    ne = ne + check_ok2(exc, 76, 2);
  end

  % 77: atler const
  try
    gd_alter_const(D, 'new11', GD.FLOAT64);
  catch exc
    ne = ne + check_ok2(exc, 77, 1);
  end

  try
    d = gd_entry(D, 'new11');
    ne = ne + check_string2(77, 1, d.field, 'new11');
    ne = ne + check_num2(77, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(77, 3, d.fragment_index, 0);
    ne = ne + check_num2(77, 5, d.const_type, GD.FLOAT64);
  catch exc
    ne = ne + check_ok2(exc, 77, 2);
  end

  % 78: gd_encoding
  try
    d = gd_encoding(D, 0);
    ne = ne + check_num(78, d, GD.UNENCODED);
  catch exc
    ne = ne + check_ok(exc, 78);
  end

  % 79: gd_encoding
  try
    d = gd_endianness(D, 0);
    ne = ne + check_num(79, d, GD.LITTLE_ENDIAN + GD.NOT_ARM_ENDIAN);
  catch exc
    ne = ne + check_ok(exc, 79);
  end

  % 80: dirfilename
  try
    d = gd_dirfilename(D);
    ne = ne + check_eostring(80, d, 'dirfile');
  catch exc
    ne = ne + check_ok(exc, 80);
  end

  % 81: gd_parent_fragment
  try
    d = gd_parent_fragment(D, 1);
    ne = ne + check_num(81, d, 0);
  catch exc
    ne = ne + check_ok(exc, 81);
  end

  % 82: gd_alter_protection
  try
    gd_alter_protection(D, 0, GD.PROTECT_DATA);
  catch exc
    ne = ne + check_ok(exc, 82);
  end

  % 83: gd_protection
  try
    d = gd_protection(D, 0);
    ne = ne + check_num(83, d, GD.PROTECT_DATA);
  catch exc
    ne = ne + check_ok(exc, 83);
  end

  % 84: gd_raw_filename
  try
    d = gd_raw_filename(D, 'data');
    ne = ne + check_eostring(84, d, 'dirfile/data');
  catch exc
    ne = ne + check_ok(exc, 84);
  end

  % 85: gd_reference
  try
    d = gd_reference(D);
    ne = ne + check_string2(85, 1, d, 'data');
  catch exc
    ne = ne + check_ok2(exc, 85, 1);
  end

  try
    d = gd_reference(D, 'new1');
    ne = ne + check_string2(85, 2, d, 'new1');
  catch exc
    ne = ne + check_ok2(exc, 85, 2);
  end

  % 87: gd_alter_encoding
  try
    gd_alter_encoding(D, GD.SLIM_ENCODED, 1, 0);
  catch exc
    ne = ne + check_ok(exc, 87);
  end

  % 88: gd_alter_endiannness
  try
    gd_alter_endianness(D, GD.BIG_ENDIAN, 1, 0);
  catch exc
    ne = ne + check_ok(exc, 88);
  end

  % 89: gd_alter_spec
  try
    gd_alter_spec(D, 'new10 PHASE in5 3', 0);
  catch exc
    ne = ne + check_ok2(exc, 89, 1);
  end

  try
    d = gd_entry(D, 'new10');
    ne = ne + check_string2(89, 1, d.field, 'new10');
    ne = ne + check_num2(89, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(89, 3, d.fragment_index, 0);
    ne = ne + check_string2(89, 4, d.in_fields, 'in5');
    ne = ne + check_num2(89, 5, d.shift, 3);
  catch exc
    ne = ne + check_ok2(exc, 89, 2);
  end

  % 90: gd_delete
  try
    gd_delete(D, 'new10', 0);
  catch exc
    ne = ne + check_ok2(exc, 90, 1);
  end

  try
    d = gd_entry(D, 'new10');
  catch exc
    ne = ne + check_exc2(exc, 90, 2, 'BadCode');
  end

  % 91: gd_malter_spec
  try
    gd_malter_spec(D, 'mnew10 PHASE in4 11', 'data', 0);
  catch exc
    ne = ne + check_ok2(exc, 91, 1);
  end

  try
    d = gd_entry(D, 'data/mnew10');
    ne = ne + check_string2(91, 1, d.field, 'data/mnew10');
    ne = ne + check_num2(91, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(91, 3, d.fragment_index, 0);
    ne = ne + check_string2(91, 4, d.in_fields, 'in4');
    ne = ne + check_num2(91, 5, d.shift, 11);
  catch exc
    ne = ne + check_ok2(exc, 91, 2);
  end

  % 92: move
  try
    gd_move(D, 'new9', 1, 0);
  catch exc
    ne = ne + check_ok2(exc, 92, 1);
  end

  try
    d = gd_fragment_index(D, 'new9');
    ne = ne + check_num(92, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 92, 2);
  end

  % 93: rename
  try
    gd_rename(D, 'new9', 'newer', 0);
  catch exc
    ne = ne + check_ok2(exc, 93, 1);
  end

  try
    d = gd_fragment_index(D, 'newer');
    ne = ne + check_num(93, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 93, 2);
  end

  % 94: unclude
  try
    gd_uninclude(D, 1, 0);
  catch exc
    ne = ne + check_ok2(exc, 94, 1);
  end

  try
    d = gd_nfragments(D);
    ne = ne + check_num(94, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 94, 2);
  end

  % 95: frameoffset
  try
    d = gd_frameoffset(D, 0);
    ne = ne + check_num(95, d, 0);
  catch exc
    ne = ne + check_ok(exc, 95);
  end

  % 96: alter_frameoffset
  try
    gd_alter_frameoffset(D, 33, 0, 0);
  catch exc
    ne = ne + check_ok2(exc, 96, 1);
  end

  try
    d = gd_frameoffset(D, 0);
    ne = ne + check_num(96, d, 33);
  catch exc
    ne = ne + check_ok2(exc, 96, 2);
  end

  % 97: native_type
  try
    d = gd_native_type(D, 'data');
    ne = ne + check_num(97, d, GD.INT8);
  catch exc
    ne = ne + check_ok(exc, 97);
  end

  % 99: validate
  try
    d = gd_validate(D, 'new7');
  catch exc
    ne = ne + check_exc(exc, 99, 'BadCode');
  end

  % 100: framenum
  try
    gd_reference(D, 'data');
    d = gd_framenum(D, 'data', 33.3);
    ne = ne + check_num(100, d, 37.1625);
  catch exc
    ne = ne + check_ok(exc, 100);
  end

  % 101: framenum_subset
  try
    d = gd_framenum(D, 'data', 33.3, 6);
    ne = ne + check_num(101, d, 37.1625);
  catch exc
    ne = ne + check_ok(exc, 101);
  end

  % 86: gd_eof
  try
    d = gd_eof(D, 'lincom');
    ne = ne + check_num(86, d, 345);
  catch exc
    ne = ne + check_ok(exc, 86);
  end

  % 142: gd_bof
  try
    d = gd_bof(D, 'lincom');
    ne = ne + check_num(142, d, 264);
  catch exc
    ne = ne + check_ok(exc, 142);
  end

  % 143: entry (div) check
  try
    d = gd_entry(D, 'div');
    ne = ne + check_string2(143, 1, d.field, 'div');
    ne = ne + check_num2(143, 2, d.field_type, GD.DIVIDE_ENTRY);
    ne = ne + check_num2(143, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(143, 4, d.in_fields, { 'mult'; 'bit'; });
  catch exc
    ne = ne + check_ok(exc, 143);
  end

  % 145: entry (recip) check
  try
    d = gd_entry(D, 'recip');
    ne = ne + check_string2(145, 1, d.field, 'recip');
    ne = ne + check_num2(145, 2, d.field_type, GD.RECIP_ENTRY);
    ne = ne + check_num2(145, 3, d.fragment_index, 0);
    ne = ne + check_num2(145, 4, d.dividend, complex(6.5,4.3));
  catch exc
    ne = ne + check_ok(exc, 145);
  end

  % 146: add entry (mult) check
  try
    gd_add_divide(D, 'new14', 'in1', 'in2', 0);
  catch exc
    ne = ne + check_ok2(exc, 146, 1);
  end

  try
    d = gd_entry(D, 'new14');
    ne = ne + check_string2(146, 1, d.field, 'new14');
    ne = ne + check_num2(146, 2, d.field_type, GD.DIVIDE_ENTRY);
    ne = ne + check_num2(146, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(146, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 146, 2);
  end

  % 148: add entry (recip) check
  try
    gd_add_recip(D, 'new16', 'in3', 33.3, 0);
  catch exc
    ne = ne + check_ok2(exc, 148, 1);
  end

  try
    d = gd_entry(D, 'new16');
    ne = ne + check_string2(148, 1, d.field, 'new16');
    ne = ne + check_num2(148, 2, d.field_type, GD.RECIP_ENTRY);
    ne = ne + check_num2(148, 3, d.fragment_index, 0);
    ne = ne + check_string2(148, 4, d.in_fields, 'in3');
    ne = ne + check_num2(148, 3, d.dividend, 33.3);
  catch exc
    ne = ne + check_ok2(exc, 148, 2);
  end

  % 149: madd entry (div) check
  try
    gd_madd_divide(D, 'data', 'mnew14', 'in1', 'in2');
  catch exc
    ne = ne + check_ok2(exc, 149, 1);
  end

  try
    d = gd_entry(D, 'data/mnew14');
    ne = ne + check_string2(149, 1, d.field, 'data/mnew14');
    ne = ne + check_num2(149, 2, d.field_type, GD.DIVIDE_ENTRY);
    ne = ne + check_num2(149, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(149, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 149, 2);
  end

  % 151: madd entry (recip) check
  try
    gd_madd_recip(D, 'data', 'mnew16', 'in1', complex(5.5,6.6));
  catch exc
    ne = ne + check_ok2(exc, 151, 1);
  end

  try
    d = gd_entry(D, 'data/mnew16');
    ne = ne + check_string2(151, 1, d.field, 'data/mnew16');
    ne = ne + check_num2(151, 2, d.field_type, GD.RECIP_ENTRY);
    ne = ne + check_num2(151, 3, d.fragment_index, 0);
    ne = ne + check_string2(151, 4, d.in_fields, 'in1');
    ne = ne + check_num2(151, 5, d.dividend, complex(5.5,6.6));
  catch exc
    ne = ne + check_ok2(exc, 151, 2);
  end

  % 155: rewrite_fragment
  try
    gd_rewrite_fragment(D, 0);
  catch exc
    ne = ne + check_ok(exc, 155);
  end

  % 156: invalid_dirfile
  try
    d = gd_invalid_dirfile();
  catch exc
    ne = ne + check_ok(exc, 156);
  end

  try
    gd_discard(d);
  catch
    [];
  end

  % 157: standards
  try
    d = gd_dirfile_standards(D);
    ne = ne + check_num(157, d, GD.DIRFILE_STANDARDS_VERSION);
  catch exc
    ne = ne + check_ok2(exc, 157, 1);
  end

  try
    d = gd_dirfile_standards(D, 0);
  catch exc
    ne = ne + check_exc2(exc, 157, 2, 'Argument');
  end

  % 158: get_carray
  try
    d = gd_get_carray(D, 'carray');
    ne = ne + check_array(158, d, [1.1, 2.2, 3.3, 4.4, 5.5, 6.6]);
  catch exc
    ne = ne + check_ok(exc, 158);
  end

  % 159: gd_carray_slice
  try
    d = gd_get_carray_slice(D, 'carray', 2, 2);
    ne = ne + check_array(159, d, [3.3, 4.4]);
  catch exc
    ne = ne + check_ok(exc, 159);
  end

  % 167: gd_carrays
  try
    d = gd_carrays(D);
    ne = ne + check_num(167, length(d), 1);
    ne = ne + check_array(167, d{1}, [1.1, 2.2, 3.3, 4.4, 5.5, 6.6]);
  catch exc
    ne = ne + check_ok(exc, 167);
  end

  % 168: gd_put_carray
  try
    gd_put_carray(D, 'carray', [9,8,7,6,5,4]);
  catch exc
    ne = ne + check_ok2(exc, 168, 1);
  end

  try
    d = gd_get_carray(D, 'carray');
    ne = ne + check_array(168, d, [9,8,7,6,5,4]);
  catch exc
    ne = ne + check_ok2(exc, 168, 2);
  end

  % 169: gd_put_carray
  try
    gd_put_carray_slice(D, 'carray', 2, [169,169]);
  catch exc
    ne = ne + check_ok2(exc, 169, 1);
  end

  try
    d = gd_get_carray(D, 'carray');
    ne = ne + check_array(169, d, [9,8,169,169,5,4]);
  catch exc
    ne = ne + check_ok2(exc, 169, 2);
  end

  % 177: gd_carray_len
  try
    d = gd_carray_len(D, 'carray');
    ne = ne + check_num(177, d, 6);
  catch exc
    ne = ne + check_ok(exc, 177);
  end

  % 178: gd_entry (carray)
  try
    d = gd_entry(D, 'carray');
    ne = ne + check_string2(178, 1, d.field, 'carray');
    ne = ne + check_num2(178, 2, d.field_type, GD.CARRAY_ENTRY);
    ne = ne + check_num2(178, 3, d.fragment_index, 0);
    ne = ne + check_num2(178, 4, d.const_type, GD.FLOAT64);
    ne = ne + check_num2(178, 4, d.array_len, 6);
  catch exc
    ne = ne + check_ok(exc, 178);
  end

  % 179: add_carray
  try
    gd_add_carray(D, 'new17', GD.FLOAT64, [1.1,2.2,3.3,4.4], 0);
  catch exc
    ne = ne + check_ok2(exc, 179, 1);
  end

  try
    d = gd_entry(D, 'new17');
    ne = ne + check_string2(179, 1, d.field, 'new17');
    ne = ne + check_num2(179, 2, d.field_type, GD.CARRAY_ENTRY);
    ne = ne + check_num2(179, 3, d.fragment_index, 0);
    ne = ne + check_num2(179, 4, d.const_type, GD.FLOAT64);
    ne = ne + check_num2(179, 4, d.array_len, 4);
  catch exc
    ne = ne + check_ok2(exc, 179, 2);
  end

  try
    d = gd_get_carray(D, 'new17');
    ne = ne + check_array(179, d, [1.1,2.2,3.3,4.4]);
  catch exc
    ne = ne + check_ok2(exc, 179, 3);
  end

  % 180: add_carray
  try
    gd_madd_carray(D, 'data', 'mnew17', GD.FLOAT64, [1.1,2.2,3.3,4.4]);
  catch exc
    ne = ne + check_ok2(exc, 180, 1);
  end

  try
    d = gd_entry(D, 'data/mnew17');
    ne = ne + check_string2(180, 1, d.field, 'data/mnew17');
    ne = ne + check_num2(180, 2, d.field_type, GD.CARRAY_ENTRY);
    ne = ne + check_num2(180, 3, d.fragment_index, 0);
    ne = ne + check_num2(180, 4, d.const_type, GD.FLOAT64);
    ne = ne + check_num2(180, 4, d.array_len, 4);
  catch exc
    ne = ne + check_ok2(exc, 180, 2);
  end

  try
    d = gd_get_carray(D, 'data/mnew17');
    ne = ne + check_array(180, d, [1.1,2.2,3.3,4.4]);
  catch exc
    ne = ne + check_ok2(exc, 180, 3);
  end

  % 181: add_carray
  try
    gd_alter_carray(D, 'new17', GD.INT32, 5);
  catch exc
    ne = ne + check_ok2(exc, 181, 1);
  end

  try
    d = gd_entry(D, 'new17');
    ne = ne + check_string2(181, 1, d.field, 'new17');
    ne = ne + check_num2(181, 2, d.field_type, GD.CARRAY_ENTRY);
    ne = ne + check_num2(181, 3, d.fragment_index, 0);
    ne = ne + check_num2(181, 4, d.const_type, GD.INT32);
    ne = ne + check_num2(181, 4, d.array_len, 5);
  catch exc
    ne = ne + check_ok2(exc, 181, 2);
  end

  try
    d = gd_get_carray(D, 'new17');
    ne = ne + check_array(181, d, [1,2,3,4,0]);
  catch exc
    ne = ne + check_ok2(exc, 181, 3);
  end

  % 183: gd_constants
  try
    d = gd_constants(D);
    ne = ne + check_array(183, d, [61, 133]);
  catch exc
    ne = ne + check_ok(exc, 183);
  end

  % 191: gd_mconstants
  try
    d = gd_mconstants(D, 'data');
    ne = ne + check_array(191, d, [3.3, 2.6]);
  catch exc
    ne = ne + check_ok(exc, 191);
  end

  % 199: gd_strings
  try
    d = gd_strings(D);
    ne = ne + check_sarray(199, d, {'Lorem ipsum'; '---string---'; ...
    'Arthur Dent'});
  catch exc
    ne = ne + check_ok(exc, 199);
  end

  % 200: gd_mstrings
  try
    d = gd_mstrings(D, 'data');
    ne = ne + check_sarray(200, d, {'This is a string constant.'; ...
    '---mstring---'});
  catch exc
    ne = ne + check_ok(exc, 200);
  end

  % 203: seek
  try
    d = gd_seek(D, 'data', 35, 0, GD.SEEK_SET);
    ne = ne + check_num(203, d, 280);
  catch exc
    ne = ne + check_ok2(exc, 203, 1);
  end

  try
    d = gd_getdata(D, 'data', GD.HERE, 0, 1, 0);
    ne = ne + check_array(203, d, [16:1:23]);
  catch exc
    ne = ne + check_ok2(exc, 203, 2);
  end

  % 204: tell
  try
    d = gd_tell(D, 'data');
    ne = ne + check_num(204, d, 288);
  catch exc
    ne = ne + check_ok(exc, 204);
  end

  % 205: hide
  try
    gd_hide(D, 'data');
  catch exc
    ne = ne + check_ok(exc, 205);
  end

  % 206: hidden
  try
    d = gd_hidden(D, 'data');
    ne = ne + check_num(206, d, 1);
  catch exc
    ne = ne + check_ok(exc, 206);
  end

  % 207: unhide
  try
    gd_unhide(D, 'data');
  catch exc
    ne = ne + check_ok2(exc, 207, 1);
  end

  try
    d = gd_hidden(D, 'data');
    ne = ne + check_num(207, d, 0);
  catch exc
    ne = ne + check_ok2(exc, 207, 2);
  end

  % 208: sync
  try
    gd_sync(D, 'data');
  catch exc
    ne = ne + check_ok(exc, 208);
  end

  % 209: sync
  try
    gd_flush(D, 'data');
  catch exc
    ne = ne + check_ok(exc, 209);
  end

  % 210: metaflush
  try
    gd_metaflush(D);
  catch exc
    ne = ne + check_ok(exc, 210);
  end

  % 211: gd_entry (window)
  try
    d = gd_entry(D, 'window');
    ne = ne + check_string2(211, 1, d.field, 'window');
    ne = ne + check_num2(211, 2, d.field_type, GD.WINDOW_ENTRY);
    ne = ne + check_num2(211, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(211, 4, d.in_fields, { 'linterp'; 'mult' });
    ne = ne + check_num2(211, 5, d.windop, GD.WINDOP_LT);
    ne = ne + check_num2(211, 6, d.threshold, 4.1);
  catch exc
    ne = ne + check_ok(exc, 211);
  end

  % 212: gd_add_window
  try
    gd_add_window(D, 'new18', 'in1', 'in2', GD.WINDOP_NE, 32, 0);
  catch exc
    ne = ne + check_ok2(exc, 212, 1);
  end

  try
    d = gd_entry(D, 'new18');
    ne = ne + check_string2(212, 1, d.field, 'new18');
    ne = ne + check_num2(212, 2, d.field_type, GD.WINDOW_ENTRY);
    ne = ne + check_num2(212, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(212, 4, d.in_fields, { 'in1'; 'in2' });
    ne = ne + check_num2(212, 5, d.windop, GD.WINDOP_NE);
    ne = ne + check_num2(212, 6, d.threshold, 32);
  catch exc
    ne = ne + check_ok2(exc, 212, 2);
  end

  % 214: gd_add_window
  try
    gd_madd_window(D, 'data', 'mnew18', 'in1', 'in2', GD.WINDOP_EQ, 214);
  catch exc
    ne = ne + check_ok2(exc, 214, 1);
  end

  try
    d = gd_entry(D, 'data/mnew18');
    ne = ne + check_string2(214, 1, d.field, 'data/mnew18');
    ne = ne + check_num2(214, 2, d.field_type, GD.WINDOW_ENTRY);
    ne = ne + check_num2(214, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(214, 4, d.in_fields, { 'in1'; 'in2' });
    ne = ne + check_num2(214, 5, d.windop, GD.WINDOP_EQ);
    ne = ne + check_num2(214, 6, d.threshold, 214);
  catch exc
    ne = ne + check_ok2(exc, 214, 2);
  end

  % 217: gd_alter_window
  try
    gd_alter_window(D, 'new18', 'in3', 0, GD.WINDOP_GE, 32e2);
  catch exc
    ne = ne + check_ok2(exc, 217, 1);
  end

  try
    d = gd_entry(D, 'new18');
    ne = ne + check_string2(217, 1, d.field, 'new18');
    ne = ne + check_num2(217, 2, d.field_type, GD.WINDOW_ENTRY);
    ne = ne + check_num2(217, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(217, 4, d.in_fields, { 'in3'; 'in2' });
    ne = ne + check_num2(217, 5, d.windop, GD.WINDOP_GE);
    ne = ne + check_num2(217, 6, d.threshold, 32e2);
  catch exc
    ne = ne + check_ok2(exc, 217, 2);
  end

  % 218: gd_alias_target
  try
    d = gd_alias_target(D, 'alias');
    ne = ne + check_string(218, d, 'data');
  catch exc
    ne = ne + check_ok(exc, 218);
  end

  % 219: gd_add_alias
  try
    gd_add_alias(D, 'new20', 'data', 0);
  catch exc
    ne = ne + check_ok2(exc, 219, 1);
  end

  try
    d = gd_alias_target(D, 'new20');
    ne = ne + check_string(219, d, 'data');
  catch exc
    ne = ne + check_ok2(exc, 219, 2);
  end

  % 220: madd_alias
  try
    gd_madd_alias(D, 'data', 'mnew20', 'data');
  catch exc
    ne = ne + check_ok2(exc, 219, 1);
  end

  try
    d = gd_alias_target(D, 'data/mnew20');
    ne = ne + check_string(219, d, 'data');
  catch exc
    ne = ne + check_ok2(exc, 219, 2);
  end

  % 221: naliases
  try
    d = gd_naliases(D, 'data');
    ne = ne + check_num(221, d, 4);
  catch exc
    ne = ne + check_ok(exc, 221);
  end

  % 222: aliases
  try
    d = gd_aliases(D, 'data');
    ne = ne + check_sarray(222, d, {'data'; 'alias'; 'data/mnew20'; 'new20'});
  catch exc
    ne = ne + check_ok(exc, 222);
  end

  % 223: include_affix
  try
    gd_include_affix(D, 'format1', 0, GD.CREAT + GD.EXCL, 'A', 'Z');
  catch exc
    ne = ne + check_ok2(exc, 41, 1);
  end

  % 224: move_alias
  try
    gd_move_alias(D, 'new20', 1);
  catch exc
    ne = ne + check_ok2(exc, 224, 1);
  end

  try
    d = gd_fragment_index(D, 'Anew20Z');
    ne = ne + check_num(224, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 224, 2);
  end

  % 225: delete_alias
  try
    gd_delete_alias(D, 'Anew20Z');
  catch exc
    ne = ne + check_ok2(exc, 225, 1);
  end

  try
    d = gd_fragment_index(D, 'Anew20Z');
  catch exc
    ne = ne + check_exc2(exc, 225, 2, 'BadCode');
  end

  % 226: fragment_affixes
  try
    d = gd_fragment_affixes(D, 1);
    ne = ne + check_sarray(226, d, {'A'; 'Z'});
  catch exc
    ne = ne + check_ok(exc, 226);
  end

  % 227: alter_affixes
  try
    gd_alter_affixes(D, 1, 'B', '');
  catch exc
    ne = ne + check_ok2(exc, 227, 1);
  end

  try
    d = gd_fragment_affixes(D, 1);
    ne = ne + check_sarray(226, d, {'B'; ''});
  catch exc
    ne = ne + check_ok2(exc, 227, 2);
  end

  % 228: gd_entry (mplex)
  try
    d = gd_entry(D, 'mplex');
    ne = ne + check_string2(228, 1, d.field, 'mplex');
    ne = ne + check_num2(228, 2, d.field_type, GD.MPLEX_ENTRY);
    ne = ne + check_num2(228, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(228, 4, d.in_fields, { 'data'; 'sbit' });
    ne = ne + check_num2(228, 5, d.count_val, 1);
    ne = ne + check_num2(228, 6, d.count_max, 10);
  catch exc
    ne = ne + check_ok(exc, 228);
  end

  % 229: gd_add_mplex
  try
    gd_add_mplex(D, 'new21', 'in1', 'in2', 5, 6, 0);
  catch exc
    ne = ne + check_ok2(exc, 229, 1);
  end

  try
    d = gd_entry(D, 'new21');
    ne = ne + check_string2(229, 1, d.field, 'new21');
    ne = ne + check_num2(229, 2, d.field_type, GD.MPLEX_ENTRY);
    ne = ne + check_num2(229, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(229, 4, d.in_fields, { 'in1'; 'in2' });
    ne = ne + check_num2(229, 5, d.count_val, 5);
    ne = ne + check_num2(229, 6, d.count_max, 6);
  catch exc
    ne = ne + check_ok2(exc, 229, 2);
  end

  % 230: gd_add_mplex
  try
    gd_madd_mplex(D, 'data', 'mnew21', 'in3', 'in2', 0, 12);
  catch exc
    ne = ne + check_ok2(exc, 230, 1);
  end

  try
    d = gd_entry(D, 'data/mnew21');
    ne = ne + check_string2(230, 1, d.field, 'data/mnew21');
    ne = ne + check_num2(230, 2, d.field_type, GD.MPLEX_ENTRY);
    ne = ne + check_num2(230, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(230, 4, d.in_fields, { 'in3'; 'in2' });
    ne = ne + check_num2(230, 5, d.count_val, 0);
    ne = ne + check_num2(230, 6, d.count_max, 12);
  catch exc
    ne = ne + check_ok2(exc, 230, 2);
  end

  % 231: gd_alter_mplex
  try
    gd_alter_mplex(D, 'new21', 'in4', 0, 3, 7);
  catch exc
    ne = ne + check_ok2(exc, 231, 1);
  end

  try
    d = gd_entry(D, 'new21');
    ne = ne + check_string2(231, 1, d.field, 'new21');
    ne = ne + check_num2(231, 2, d.field_type, GD.MPLEX_ENTRY);
    ne = ne + check_num2(231, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(231, 4, d.in_fields, { 'in4'; 'in2' });
    ne = ne + check_num2(231, 5, d.count_val, 3);
    ne = ne + check_num2(231, 6, d.count_max, 7);
  catch exc
    ne = ne + check_ok2(exc, 231, 2);
  end

  % 232: gd_strtok
  try
    d = gd_strtok(D, '"test1 test2" test3\ test4');
    ne = ne + check_sarray(232, d, {'test1 test2'; 'test3 test4'});
  catch exc
    ne = ne + check_ok(exc, 232);
  end

  % 233: raw_close
  try
    gd_raw_close(D, 'data');
  catch exc
    ne = ne + check_ok(exc, 233);
  end

  % 234: desync
  try
    d = gd_desync(D);
    ne = ne + check_num(234, d, 0);
  catch exc
    ne = ne + check_ok(exc, 234);
  end

  % 235: gd_flags
  try
    d = gd_flags(D, GD.PRETTY_PRINT, 0);
    ne = ne + check_num(235, d, GD.PRETTY_PRINT);
  catch exc
    ne = ne + check_ok(exc, 235);
  end

  % 236: gd_verbose_prefix
  try
    gd_verbose_prefix(D, 'big_test: ');
  catch exc
    ne = ne + check_ok(exc, 236);
  end

  % 237: gd_nentries
  try
    d = gd_nentries(D, 'data', GD.SCALAR_ENTRIES, ...
    GD.ENTRIES_HIDDEN + GD.ENTRIES_NOALIAS);
    ne = ne + check_num(237, d, 5);
  catch exc
    ne = ne + check_ok(exc, 237);
  end

  % 239: gd_nentries
  try
    d = gd_entry_list(D, 'data', GD.SCALAR_ENTRIES, ...
    GD.ENTRIES_HIDDEN + GD.ENTRIES_NOALIAS);
    ne = ne + check_sarray(239, d, {'mstr', 'mconst', 'mnew11', 'mnew12', ...
    'mnew17'});
  catch exc
    ne = ne + check_ok(exc, 239);
  end

  % 240: gd_mplex_lookback
  try
    gd_mplex_lookback(D, GD.LOOKBACK_ALL);
  catch exc
    ne = ne + check_ok(exc, 240);
  end

  % 241: gd_linterp_tablename
  try
    d = gd_linterp_tablename(D, 'linterp');
    ne = ne + check_eostring(241, d, 'dirfile/lut');
  catch exc
    ne = ne + check_ok(exc, 241);
  end





























  gd_discard(D);
  rmdir(filedir, 's');
  if ne > 0
    ne
    fail;
  end

catch exc
  disp(exc.getReport);
  disp('Stack:');
  disp(struct2cell(transpose(exc.stack)));
  fail
end
end

% hackery
function fail
  fid=fopen('test_failed', 'w');
  fclose(fid);
  exit force
end

% pretty-print an array
function str = pp_array(a)
  str = ['[ ', num2str(a), ' ]'];
end

function str = pp_sarray(a)
  str = '[';
  for i = 1:length(a)
    if (isnumeric(a{i}))
      str = strcat(str, sprintf(' %s', num2str(a{i})));
    else
      str = strcat(str, sprintf(' %s', a{i}));
    end
  end
  str = strcat(str, ' ]');
end

% Check functions

function ne = check_ok(exc, t)
  if (isempty(strmatch('GetData:', exc.identifier)))
    rethrow(exc);
  end
  fprintf(2, 'e[%i] = %s\n', t, exc.identifier);
  disp(exc.getReport);
  ne = 1;
end

function ne = check_ok2(exc, t, m)
  if (isempty(strmatch('GetData:', exc.identifier)))
    rethrow(exc);
  end
  fprintf(2, 'e[%i,%i] = %s\n', t, m, exc.identifier);
  disp(exc.getReport);
  ne = 1;
end

function ne = check_exc(exc, t, v)
  if (isempty(strmatch('GetData:', exc.identifier)))
    rethrow(exc);
  end
  ne = check_string(t,exc.identifier,strcat('GetData:Lib:', v));
end

function ne = check_exc2(exc, t, m, v)
  if (isempty(strmatch('GetData:', exc.identifier)))
    rethrow(exc);
  end
  ne = check_string2(t,m,exc.identifier,strcat('GetData:Lib:', v));
end

function ne = check_eostring(t,v,g)
  l = length(v);
  f = l - length(g) + 1;
  if (strcmp(v(f:l), g) == 0)
    fprintf(2, 's[%i] = [...]''%s'', expected [...]''%s''\n', t, v(f:l), g);
    ne = 1;
  else
    ne = 0;
  end
end
function ne = check_string(t,v,g)
  if (strcmp(v, g) == 0)
    fprintf(2, 's[%i] = ''%s'', expected ''%s''\n', t, v, g);
    ne = 1;
  else
    ne = 0;
  end
end

function ne = check_string2(t,m,v,g)
  if (strcmp(v, g) == 0)
    fprintf(2, 's[%i,%i] = ''%s'', expected ''%s''\n', t, m, v, g);
    ne = 1;
  else
    ne = 0;
  end
end

function ne = check_sarray(t,v,g)
  same = 1;
  if (length(v) ~= length(g))
    same = 0;
  else
    for i = 1:length(v)
      if (isnumeric(v{i}) ~= isnumeric(g{i}))
        same = 0;
      elseif (isnumeric(v{i}))
        if (v{i} ~= g{i})
          same = 0;
        end
      elseif (strcmp(v{i},g{i}) == 0)
        same = 0;
      end
    end
  end
  if (~same)
    fprintf(2, 's[%i] = %s, expected %s\n', t, pp_sarray(v), pp_sarray(g));
    ne = 1;
  else
    ne = 0;
  end
end

function ne = check_sarray2(t,m,v,g)
  same = 1;
  if (length(v) ~= length(g))
    same = 0;
  else
    for i = 1:length(v)
      if (isnumeric(v{i}) ~= isnumeric(g{i}))
        same = 0;
      elseif (isnumeric(v{i}))
        if (v{i} ~= g{i})
          same = 0;
        end
      elseif (strcmp(v{i},g{i}) == 0)
        same = 0;
      end
    end
  end
  if (~same)
    fprintf(2, 's[%i,%i] = %s, expected %s\n', t, m, pp_sarray(v), ...
      pp_sarray(g));
    ne = 1;
  else
    ne = 0;
  end
end

function ne = check_array(t,v,g)
  if (~isequal(v,g))
    fprintf(2, 'a[%i] = %s, expected %s\n', t, pp_array(v), pp_array(g));
    ne = 1;
  else
    ne = 0;
  end
end

function ne = check_array2(t,m,v,g)
  if (~isequal(v,g))
    fprintf(2, 'a[%i,%i] = %s, expected %s\n', t, m, pp_array(v), pp_array(g));
    ne = 1;
  else
    ne = 0;
  end
end

function ne = check_num(t,v,g)
  if (v ~= g)
    fprintf(2, 'n[%i] = %s, expected %s\n', t, num2str(v), num2str(g));
    ne = 1;
  else
    ne = 0;
  end
end

function ne = check_num2(t,m,v,g)
  if (v ~= g)
    fprintf(2, 'n[%i,%i] = %s, expected %s\n', t, m, num2str(v), num2str(g));
    ne = 1;
  else
    ne = 0;
  end
end
