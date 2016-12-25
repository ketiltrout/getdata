% Copyright (C) 2013, 2016 D. V. Wiebe
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
  '/META data mcarray CARRAY FLOAT64 1.9 2.8 3.7 4.6 5.5\n'...
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
  'string STRING "Zaphod Beeblebrox"\n'...
  'sarray SARRAY one two three four five six seven\n'...
  'data/msarray SARRAY eight nine ten eleven twelve\n'...
  'indir INDIR data carray\n'...
  'sindir SINDIR data sarray\n'...
  ];
  form2_data = 'const2 CONST INT8 -19\n';

  fields = { 'bit'; 'div'; 'data'; 'mult'; 'sbit'; 'INDEX'; 'alias'; ...
  'const'; 'indir'; 'mplex'; 'phase'; 'recip'; 'carray'; 'lincom'; 'sarray'; ...
  'sindir'; 'string'; 'window'; 'linterp'; 'polynom'; };
  nfields = 20;

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

  % 0: getdata_constants check
  GD = getdata_constants();
  ne = ne + check_num(0, GD.RDWR, 1);

  % 1: gd_error check
  try
    D = gd_open('x');
  catch exc
    ne = ne + check_exc(exc, 1, 'IO');
  end

  % 2: gd_open check
  try
    D = gd_open(filedir, GD.RDWR);
  catch exc
    ne = ne + check_ok(exc, 2);
  end

  % 3: getdata check
  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0);
    ne = ne + check_array(3, d, [40:47]);
  catch exc
    ne = ne + check_ok(exc, 3);
  end

  % 10: getdata check (complex128)
  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0, GD.COMPLEX128);
    ne = ne + check_array(2, d, [ complex(40,0), complex(41,0), ...
    complex(42,0), complex(43,0), complex(44,0), complex(45,0), ...
    complex(46,0), complex(47,0) ]);
  catch exc
    ne = ne + check_ok(exc, 10);
  end

  % 11: getdata check (NULL)
  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0, GD.NULL);
    ne = ne + check_num(11, d, 8);
  catch exc
    ne = ne + check_ok(exc, 11);
  end

  % 12: gd_get_constant check
  try
    d = gd_get_constant(D, 'const');
    ne = ne + check_num(12, d, 5.5);
  catch exc
    ne = ne + check_ok(exc, 12);
  end

  % 23: gd_nfields check
  try
    d = gd_nfields(D);
    ne = ne + check_num(23, d, nfields);
  catch exc
    ne = ne + check_ok(exc, 23);
  end

  % 25: gd_field_list check
  try
    d = gd_field_list(D);
    ne = ne + check_sarray(25, d, fields);
  catch exc
    ne = ne + check_ok(exc, 25);
  end

  % 26: gd_nmfields check
  try
    d = gd_nmfields(D, 'data');
    ne = ne + check_num(26, d, 5);
  catch exc
    ne = ne + check_ok(exc, 26);
  end

  % 27: gd_mfield_list check
  try
    d = gd_mfield_list(D, 'data');
    ne = ne + check_sarray(27, d, ...
    { 'mstr'; 'mconst'; 'mcarray'; 'mlut'; 'msarray' });
  catch exc
    ne = ne + check_ok(exc, 27);
  end

  % 28: gd_nframes check
  try
    d = gd_nframes(D);
    ne = ne + check_num(28, d, 10);
  catch exc
    ne = ne + check_ok(exc, 28);
  end

  % 29: gd_spf check
  try
    d = gd_spf(D, 'data');
    ne = ne + check_num(29, d, 8);
  catch exc
    ne = ne + check_ok(exc, 29);
  end

  % 30: gd_putdata check
  p = [ 13, 14, 15, 16 ];
  try
    d = gd_putdata(D, 'data', 5, 1, p);
    ne = ne + check_num(30, d, 4);
  catch exc
    ne = ne + check_ok2(exc, 30, 1);
  end

  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0);
  catch exc
    ne = ne + check_ok2(exc, 30, 2);
  end

  % 37: gd_putdata check (complex)
  p = [ complex(33,0), complex(34,0), complex(35,0), complex(36,0) ];
  try
    d = gd_putdata(D, 'data', 5, 1, p);
    ne = ne + check_num(37, d, 4);
  catch exc
    ne = ne + check_ok2(exc, 37, 1);
  end

  try
    d = gd_getdata(D, 'data', 5, 0, 1, 0);
    ne = ne + check_array(37, d, [40, 33, 34, 35, 36, 45, 46, 47]);
  catch exc
    ne = ne + check_ok2(exc, 37, 2);
  end

  % 38: error_string
  try
    gd_getdata(D, 'x', 1, 0, 1, 0);
  catch exc
    ne = ne + check_exc(exc, 38, 'BadCode');
  end
  ne = ne + check_num(38, gd_error(D), GD.E_BAD_CODE);
  ne = ne + check_string(38, gd_error_string(D), 'Field not found: x');

  % 39: entry_type
  try
    d = gd_entry_type(D, 'data');
    ne = ne + check_num(39, d, GD.RAW_ENTRY);
  catch exc
    ne = ne + check_ok(exc, 39);
  end

  % 40: entry (raw) check
  try
    d = gd_entry(D, 'data');
    ne = ne + check_string2(40, 1, d.field, 'data');
    ne = ne + check_num2(40, 2, d.field_type, GD.RAW_ENTRY);
    ne = ne + check_num2(40, 3, d.fragment_index, 0);
    ne = ne + check_num2(40, 4, d.data_type, GD.INT8);
  catch exc
    ne = ne + check_ok(exc, 40);
  end

  % 42: entry (lincom) check
  try
    d = gd_entry(D, 'lincom');
    ne = ne + check_string2(42, 1, d.field, 'lincom');
    ne = ne + check_num2(42, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(42, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(42, 4, d.in_fields, { 'data'; 'INDEX'; 'linterp' });
    ne = ne + check_array2(42, 5, d.m, [ 1.1, 2.2, 5.5 ]);
    ne = ne + check_array2(42, 6, d.b, [ 2.2, complex(3.3,4.4), 5.5 ]);
    ne = ne + check_sarray2(42, 7, d.scalar, {0; 0; 'const'; 0; 0; 'const'});
    ne = ne + check_array2(42, 8, d.scalar_ind, [0, 0, -1, 0, 0, -1]);
  catch exc
    ne = ne + check_ok(exc, 42);
  end

  % 44: entry (polynom) check
  try
    d = gd_entry(D, 'polynom');
    ne = ne + check_string2(44, 1, d.field, 'polynom');
    ne = ne + check_num2(44, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(44, 3, d.fragment_index, 0);
    ne = ne + check_string2(44, 4, d.in_fields, 'data');
    ne = ne + check_array2(44, 5, d.a, [ 1.1, 2.2, 2.2, complex(3.3, 4.4), ...
    5.5, 5.5]);
    ne = ne + check_sarray2(44, 6, d.scalar, {0; 0; 0; 0; 'const'; 'const'});
    ne = ne + check_array2(44, 7, d.scalar_ind, [0, 0, 0, 0, -1, -1]);
  catch exc
    ne = ne + check_ok(exc, 44);
  end

  % 45: entry (linterp) check
  try
    d = gd_entry(D, 'linterp');
    ne = ne + check_string2(45, 1, d.field, 'linterp');
    ne = ne + check_num2(45, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(45, 3, d.fragment_index, 0);
    ne = ne + check_string2(45, 4, d.in_fields, 'data');
    ne = ne + check_string2(45, 5, d.table, './lut');
  catch exc
    ne = ne + check_ok(exc, 45);
  end

  % 46: entry (bit) check
  try
    d = gd_entry(D, 'bit');
    ne = ne + check_string2(46, 1, d.field, 'bit');
    ne = ne + check_num2(46, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(46, 3, d.fragment_index, 0);
    ne = ne + check_string2(46, 4, d.in_fields, 'data');
    ne = ne + check_num2(46, 5, d.bitnum, 3);
    ne = ne + check_num2(46, 6, d.numbits, 4);
  catch exc
    ne = ne + check_ok(exc, 46);
  end

  % 47: entry (sbit) check
  try
    d = gd_entry(D, 'sbit');
    ne = ne + check_string2(47, 1, d.field, 'sbit');
    ne = ne + check_num2(47, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(47, 3, d.fragment_index, 0);
    ne = ne + check_string2(47, 4, d.in_fields, 'data');
    ne = ne + check_num2(47, 5, d.bitnum, 5);
    ne = ne + check_num2(47, 6, d.numbits, 6);
  catch exc
    ne = ne + check_ok(exc, 47);
  end

  % 48: entry (mult) check
  try
    d = gd_entry(D, 'mult');
    ne = ne + check_string2(48, 1, d.field, 'mult');
    ne = ne + check_num2(48, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(48, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(48, 4, d.in_fields, { 'data'; 'sbit'; });
  catch exc
    ne = ne + check_ok(exc, 48);
  end

  % 49: entry (phase) check
  try
    d = gd_entry(D, 'phase');
    ne = ne + check_string2(49, 1, d.field, 'phase');
    ne = ne + check_num2(49, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(49, 3, d.fragment_index, 0);
    ne = ne + check_string2(49, 4, d.in_fields, 'data');
    ne = ne + check_num2(49, 5, d.shift, 11);
  catch exc
    ne = ne + check_ok(exc, 49);
  end

  % 50: entry (const) check
  try
    d = gd_entry(D, 'const');
    ne = ne + check_string2(50, 1, d.field, 'const');
    ne = ne + check_num2(50, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(50, 3, d.fragment_index, 0);
    ne = ne + check_num2(50, 5, d.const_type, GD.FLOAT64);
  catch exc
    ne = ne + check_ok(exc, 50);
  end

  % 51: entry (string) check
  try
    d = gd_entry(D, 'string');
    ne = ne + check_string2(51, 1, d.field, 'string');
    ne = ne + check_num2(51, 2, d.field_type, GD.STRING_ENTRY);
    ne = ne + check_num2(51, 3, d.fragment_index, 0);
  catch exc
    ne = ne + check_ok(exc, 51);
  end

  % 52: gd_fragment_index check
  try
    d = gd_fragment_index(D, 'data');
  catch exc
    ne = ne + check_ok(exc, 52);
  end
  ne = ne + check_num(52, d, 0);

  % 53: add_raw
  try
    gd_add_raw(D, 'new1', GD.FLOAT64, 3, 0);
  catch exc
    ne = ne + check_ok2(exc, 53, 1);
  end

  try
    d = gd_entry(D, 'new1');
    ne = ne + check_string2(53, 1, d.field, 'new1');
    ne = ne + check_num2(53, 2, d.field_type, GD.RAW_ENTRY);
    ne = ne + check_num2(53, 3, d.fragment_index, 0);
    ne = ne + check_num2(53, 4, d.data_type, GD.FLOAT64);
    ne = ne + check_num2(53, 4, d.spf, 3);
  catch exc
    ne = ne + check_ok2(exc, 53, 2);
  end

  % 54: add entry (lincom) check
  try
    gd_add_lincom(D, 'new2', { 'in1'; 'in2' }, [9.9, 7.7], [8.8, 6.6], 0);
  catch exc
    ne = ne + check_ok2(exc, 54, 1);
  end

  try
    d = gd_entry(D, 'new2');
    ne = ne + check_string2(54, 1, d.field, 'new2');
    ne = ne + check_num2(54, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(54, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(54, 4, d.in_fields, { 'in1'; 'in2' });
    ne = ne + check_array2(54, 5, d.m, [ 9.9, 7.7 ]);
    ne = ne + check_array2(54, 6, d.b, [ 8.8, 6.6 ]);
  catch exc
    ne = ne + check_ok2(exc, 54, 2);
  end

  % 57: add entry (polynom) check
  try
    gd_add_polynom(D, 'new5', 'in1', [3.9, 4.8, 5.7, complex(6.6,7.5)], 0);
  catch exc
    ne = ne + check_ok2(exc, 57, 1);
  end

  try
    d = gd_entry(D, 'new5');
    ne = ne + check_string2(57, 1, d.field, 'new5');
    ne = ne + check_num2(57, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(57, 3, d.fragment_index, 0);
    ne = ne + check_string2(57, 4, d.in_fields, 'in1');
    ne = ne + check_array2(57, 5, d.a, [3.9, 4.8, 5.7, complex(6.6,7.5)]);
  catch exc
    ne = ne + check_ok2(exc, 57, 2);
  end

  % 58: add entry (linterp) check
  try
    gd_add_linterp(D, 'new6', 'in', './some/table', 0);
  catch exc
    ne = ne + check_ok2(exc, 58, 1);
  end

  try
    d = gd_entry(D, 'new6');
    ne = ne + check_string2(58, 1, d.field, 'new6');
    ne = ne + check_num2(58, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(58, 3, d.fragment_index, 0);
    ne = ne + check_string2(58, 4, d.in_fields, 'in');
    ne = ne + check_string2(58, 5, d.table, './some/table');
  catch exc
    ne = ne + check_ok2(exc, 58, 2);
  end

  % 59: add entry (bit) check
  try
    gd_add_bit(D, 'new7', 'in', 13, 12, 0);
  catch exc
    ne = ne + check_ok2(exc, 59, 1);
  end

  try
    d = gd_entry(D, 'new7');
    ne = ne + check_string2(59, 1, d.field, 'new7');
    ne = ne + check_num2(59, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(59, 3, d.fragment_index, 0);
    ne = ne + check_string2(59, 4, d.in_fields, 'in');
    ne = ne + check_num2(59, 5, d.bitnum, 13);
    ne = ne + check_num2(59, 6, d.numbits, 12);
  catch exc
    ne = ne + check_ok2(exc, 59, 2);
  end

  % 60: add entry (sbit) check
  try
    gd_add_sbit(D, 'new8', 'in', 14, 15, 0);
  catch exc
    ne = ne + check_ok2(exc, 60, 1);
  end

  try
    d = gd_entry(D, 'new8');
    ne = ne + check_string2(60, 1, d.field, 'new8');
    ne = ne + check_num2(60, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(60, 3, d.fragment_index, 0);
    ne = ne + check_string2(60, 4, d.in_fields, 'in');
    ne = ne + check_num2(60, 5, d.bitnum, 14);
    ne = ne + check_num2(60, 6, d.numbits, 15);
  catch exc
    ne = ne + check_ok2(exc, 60, 2);
  end

  % 61: add entry (mult) check
  try
    gd_add_multiply(D, 'new9', 'in1', 'in2', 0);
  catch exc
    ne = ne + check_ok2(exc, 61, 1);
  end

  try
    d = gd_entry(D, 'new9');
    ne = ne + check_string2(61, 1, d.field, 'new9');
    ne = ne + check_num2(61, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(61, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(61, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 61, 2);
  end

  % 62: add entry (phase) check
  try
    gd_add_phase(D, 'new10', 'in1', 22, 0);
  catch exc
    ne = ne + check_ok2(exc, 62, 1);
  end

  try
    d = gd_entry(D, 'new10');
    ne = ne + check_string2(62, 1, d.field, 'new10');
    ne = ne + check_num2(62, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(62, 3, d.fragment_index, 0);
    ne = ne + check_string2(62, 4, d.in_fields, 'in1');
    ne = ne + check_num2(62, 5, d.shift, 22);
  catch exc
    ne = ne + check_ok2(exc, 62, 2);
  end

  % 63: add entry (const) check
  try
    gd_add_const(D, 'new11', GD.COMPLEX128, 2.6, 0);
  catch exc
    ne = ne + check_ok2(exc, 63, 1);
  end

  try
    d = gd_entry(D, 'new11');
    ne = ne + check_string2(63, 1, d.field, 'new11');
    ne = ne + check_num2(63, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(63, 3, d.fragment_index, 0);
    ne = ne + check_num2(63, 5, d.const_type, GD.COMPLEX128);
  catch exc
    ne = ne + check_ok2(exc, 63, 2);
  end

  try
    d = gd_get_constant(D, 'new11', GD.FLOAT64);
    ne = ne + check_num2(63, 6, d, 2.6);
  catch exc
    ne = ne + check_ok2(exc, 63, 3);
  end

  % 64: fragmentname
  try
    d = gd_fragmentname(D, 0);
    ne = ne + check_eostring(64, d, 'dirfile/format');
  catch exc
    ne = ne + check_ok(exc, 64);
  end

  % 65: nfragments
  try
    d = gd_nfragments(D);
    ne = ne + check_num(65, d, 1);
  catch exc
    ne = ne + check_ok(exc, 65);
  end

  % 66: include
  try
    d = gd_include(D, 'form2', 0, 0);
    ne = ne + check_num2(66, 1, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 66, 1);
  end

  try
    d = gd_get_constant(D, 'const2');
    ne = ne + check_num2(66, 2, d, -19);
  catch exc
    ne = ne + check_ok2(exc, 66, 2);
  end

  % 67: nfields_by_type
  try
    d = gd_nfields_by_type(D, GD.LINCOM_ENTRY);
    ne = ne + check_num(67, d, 2);
  catch exc
    ne = ne + check_ok(exc, 67);
  end

  % 68: field_list_by_type
  try
    d = gd_field_list_by_type(D, GD.LINCOM_ENTRY);
    ne = ne + check_sarray(68, d, { 'new2', 'lincom' });
  catch exc
    ne = ne + check_ok(exc, 68);
  end

  % 69: nfields_by_type
  try
    d = gd_nvectors(D);
    ne = ne + check_num(69, d, 23);
  catch exc
    ne = ne + check_ok(exc, 69);
  end

  % 70: vector_list check
  try
    d = gd_vector_list(D);
    ne = ne + check_sarray(70, d, { 'bit'; 'div'; 'data'; 'mult'; 'new1'; ...
    'new2'; 'new5'; 'new6'; 'new7'; 'new8'; 'new9'; 'sbit'; 'INDEX'; ...
    'alias'; 'indir'; 'mplex'; 'new10'; 'phase'; 'recip'; 'lincom'; ...
    'window'; 'linterp'; 'polynom' });
  catch exc
    ne = ne + check_ok(exc, 70);
  end

  % 71: madd entry (lincom) check
  try
    gd_madd_lincom(D, 'data', 'mnew1', { 'in1'; 'in2' }, [9.9, 7.7], ...
    [8.8, 6.6]);
  catch exc
    ne = ne + check_ok2(exc, 71, 1);
  end

  try
    d = gd_entry(D, 'data/mnew1');
    ne = ne + check_string2(71, 1, d.field, 'data/mnew1');
    ne = ne + check_num2(71, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(71, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(71, 4, d.in_fields, { 'in1'; 'in2' });
    ne = ne + check_array2(71, 5, d.m, [ 9.9, 7.7 ]);
    ne = ne + check_array2(71, 6, d.b, [ 8.8, 6.6 ]);
  catch exc
    ne = ne + check_ok2(exc, 71, 2);
  end

  % 74: madd polynom check
  try
    gd_madd_polynom(D, 'data', 'mnew4', 'in1', [3.9, 4.8, 5.7, ...
    complex(6.6,7.5)]);
  catch exc
    ne = ne + check_ok2(exc, 74, 1);
  end

  try
    d = gd_entry(D, 'data/mnew4');
    ne = ne + check_string2(74, 1, d.field, 'data/mnew4');
    ne = ne + check_num2(74, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(74, 3, d.fragment_index, 0);
    ne = ne + check_string2(74, 4, d.in_fields, 'in1');
    ne = ne + check_array2(74, 5, d.a, [3.9, 4.8, 5.7, complex(6.6,7.5)]);
  catch exc
    ne = ne + check_ok2(exc, 74, 2);
  end

  % 75: add entry (linterp) check
  try
    gd_madd_linterp(D, 'data', 'mnew6', 'in', './some/table');
  catch exc
    ne = ne + check_ok2(exc, 75, 1);
  end

  try
    d = gd_entry(D, 'data/mnew6');
    ne = ne + check_string2(75, 1, d.field, 'data/mnew6');
    ne = ne + check_num2(75, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(75, 3, d.fragment_index, 0);
    ne = ne + check_string2(75, 4, d.in_fields, 'in');
    ne = ne + check_string2(75, 5, d.table, './some/table');
  catch exc
    ne = ne + check_ok2(exc, 75, 2);
  end

  % 76: add entry (bit) check
  try
    gd_madd_bit(D, 'data', 'mnew7', 'in', 13, 12);
  catch exc
    ne = ne + check_ok2(exc, 76, 1);
  end

  try
    d = gd_entry(D, 'data/mnew7');
    ne = ne + check_string2(76, 1, d.field, 'data/mnew7');
    ne = ne + check_num2(76, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(76, 3, d.fragment_index, 0);
    ne = ne + check_string2(76, 4, d.in_fields, 'in');
    ne = ne + check_num2(76, 5, d.bitnum, 13);
    ne = ne + check_num2(76, 6, d.numbits, 12);
  catch exc
    ne = ne + check_ok2(exc, 76, 2);
  end

  % 77: add entry (sbit) check
  try
    gd_madd_sbit(D, 'data', 'mnew8', 'in', 14, 15);
  catch exc
    ne = ne + check_ok2(exc, 77, 1);
  end

  try
    d = gd_entry(D, 'data/mnew8');
    ne = ne + check_string2(77, 1, d.field, 'data/mnew8');
    ne = ne + check_num2(77, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(77, 3, d.fragment_index, 0);
    ne = ne + check_string2(77, 4, d.in_fields, 'in');
    ne = ne + check_num2(77, 5, d.bitnum, 14);
    ne = ne + check_num2(77, 6, d.numbits, 15);
  catch exc
    ne = ne + check_ok2(exc, 77, 2);
  end

  % 78: add entry (mult) check
  try
    gd_madd_multiply(D, 'data', 'mnew9', 'in1', 'in2');
  catch exc
    ne = ne + check_ok2(exc, 78, 1);
  end

  try
    d = gd_entry(D, 'data/mnew9');
    ne = ne + check_string2(78, 1, d.field, 'data/mnew9');
    ne = ne + check_num2(78, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(78, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(78, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 78, 2);
  end

  % 79: add entry (phase) check
  try
    gd_madd_phase(D, 'data', 'mnew10', 'in1', 22);
  catch exc
    ne = ne + check_ok2(exc, 79, 1);
  end

  try
    d = gd_entry(D, 'data/mnew10');
    ne = ne + check_string2(79, 1, d.field, 'data/mnew10');
    ne = ne + check_num2(79, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(79, 3, d.fragment_index, 0);
    ne = ne + check_string2(79, 4, d.in_fields, 'in1');
    ne = ne + check_num2(79, 5, d.shift, 22);
  catch exc
    ne = ne + check_ok2(exc, 79, 2);
  end

  % 80: madd entry (const) check
  try
    gd_madd_const(D, 'data', 'mnew11', GD.FLOAT64, 2.6);
  catch exc
    ne = ne + check_ok2(exc, 80, 1);
  end

  try
    d = gd_entry(D, 'data/mnew11');
    ne = ne + check_string2(80, 1, d.field, 'data/mnew11');
    ne = ne + check_num2(80, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(80, 3, d.fragment_index, 0);
    ne = ne + check_num2(80, 5, d.const_type, GD.FLOAT64);
  catch exc
    ne = ne + check_ok2(exc, 80, 2);
  end

  try
    d = gd_get_constant(D, 'data/mnew11');
    ne = ne + check_num2(80, 6, d, 2.6);
  catch exc
    ne = ne + check_ok2(exc, 80, 3);
  end

  % 81: get_string
  try
    d = gd_get_string(D, 'string');
    ne = ne + check_string(81, d, 'Zaphod Beeblebrox');
  catch exc
    ne = ne + check_ok(exc, 81);
  end

  % 82: add string
  try
    gd_add_string(D, 'new12', '---string---', 0);
  catch exc
    ne = ne + check_ok2(exc, 82, 1);
  end

  try
    d = gd_entry(D, 'new12');
    ne = ne + check_string2(82, 1, d.field, 'new12');
    ne = ne + check_num2(82, 2, d.field_type, GD.STRING_ENTRY);
    ne = ne + check_num2(82, 3, d.fragment_index, 0);
  catch exc
    ne = ne + check_ok2(exc, 82, 2);
  end

  try
    d = gd_get_string(D, 'new12');
    ne = ne + check_string(82, d, '---string---');
  catch exc
    ne = ne + check_ok(exc, 82);
  end

  % 83: madd string
  try
    gd_madd_string(D, 'data', 'mnew12', '---mstring---');
  catch exc
    ne = ne + check_ok2(exc, 83, 1);
  end

  try
    d = gd_entry(D, 'data/mnew12');
    ne = ne + check_string2(83, 1, d.field, 'data/mnew12');
    ne = ne + check_num2(83, 2, d.field_type, GD.STRING_ENTRY);
    ne = ne + check_num2(83, 3, d.fragment_index, 0);
  catch exc
    ne = ne + check_ok2(exc, 83, 2);
  end

  try
    d = gd_get_string(D, 'data/mnew12');
    ne = ne + check_string(83, d, '---mstring---');
  catch exc
    ne = ne + check_ok(exc, 83);
  end

  % 84: add_spec
  try
    gd_add_spec(D, 'lorem STRING "Lorem ipsum"', 0);
  catch exc
    ne = ne + check_ok2(exc, 84, 1);
  end

  try
    d = gd_get_string(D, 'lorem');
    ne = ne + check_string(84, d, 'Lorem ipsum');
  catch exc
    ne = ne + check_ok2(exc, 84, 2);
  end

  % 85: madd_spec
  try
    gd_madd_spec(D, 'ipsum STRING "dolor sit amet."', 'lorem');
  catch exc
    ne = ne + check_ok2(exc, 85, 1);
  end

  try
    d = gd_get_string(D, 'lorem/ipsum');
    ne = ne + check_string(85, d, 'dolor sit amet.');
  catch exc
    ne = ne + check_ok2(exc, 85, 2);
  end

  % 86: put_constant
  try
    gd_put_constant(D, 'const', 86);
  catch exc
    ne = ne + check_ok2(exc, 86, 1);
  end

  try
    d = gd_get_constant(D, 'const');
    ne = ne + check_num2(86, 6, d, 86);
  catch exc
    ne = ne + check_ok2(exc, 86, 2);
  end

  % 93: put_constant
  try
    gd_put_constant(D, 'new11', complex(93,134));
  catch exc
    ne = ne + check_ok2(exc, 93, 1);
  end

  try
    d = gd_get_constant(D, 'new11');
    ne = ne + check_num2(93, 6, d, complex(93,134));
  catch exc
    ne = ne + check_ok2(exc, 93, 2);
  end

  % 94: put_string
  try
    gd_put_string(D, 'string', 'Arthur Dent');
  catch exc
    ne = ne + check_ok2(exc, 94, 1);
  end

  try
    d = gd_get_string(D, 'string');
    ne = ne + check_string(94, d, 'Arthur Dent');
  catch exc
    ne = ne + check_ok2(exc, 94, 2);
  end

  % 95: nmfields_by_type
  try
    d = gd_nmfields_by_type(D, 'data', GD.LINCOM_ENTRY);
    ne = ne + check_num(95, d, 1);
  catch exc
    ne = ne + check_ok(exc, 95);
  end

  % 96: mfield_list_by_type
  try
    d = gd_mfield_list_by_type(D, 'data', GD.LINCOM_ENTRY);
    ne = ne + check_sarray(96, d, {'mnew1'});
  catch exc
    ne = ne + check_ok(exc, 96);
  end

  % 97: nmvectors
  try
    d = gd_nmvectors(D, 'data');
    ne = ne + check_num(97, d, 8);
  catch exc
    ne = ne + check_ok(exc, 97);
  end

  % 98: mvector_list
  try
    d = gd_mvector_list(D, 'data');
    ne = ne + check_sarray(98, d, {'mlut', 'mnew1', 'mnew4', 'mnew6', ...
    'mnew7', 'mnew8', 'mnew9', 'mnew10'});
  catch exc
    ne = ne + check_ok(exc, 98);
  end

  % 99: gd_alter_raw
  try
    gd_alter_raw(D, 'new1', GD.INT32, 4, 0);
  catch exc
    ne = ne + check_ok2(exc, 99, 1);
  end

  try
    d = gd_entry(D, 'new1');
    ne = ne + check_string2(99, 1, d.field, 'new1');
    ne = ne + check_num2(99, 2, d.field_type, GD.RAW_ENTRY);
    ne = ne + check_num2(99, 3, d.fragment_index, 0);
    ne = ne + check_num2(99, 4, d.data_type, GD.INT32);
    ne = ne + check_num2(99, 4, d.spf, 4);
  catch exc
    ne = ne + check_ok2(exc, 99, 2);
  end

  % 100: gd_alter_lincom
  try
    gd_alter_lincom(D, 'new2', {'in4'; 0; 'in6'}, [ 0.99, 11, 1.96 ], ...
    [ 7.8, 0.022, 0 ]);
  catch exc
    ne = ne + check_ok2(exc, 100, 1);
  end

  try
    d = gd_entry(D, 'new2');
    ne = ne + check_string2(100, 1, d.field, 'new2');
    ne = ne + check_num2(100, 2, d.field_type, GD.LINCOM_ENTRY);
    ne = ne + check_num2(100, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(100, 4, d.in_fields, { 'in4'; 'in2'; 'in6' });
    ne = ne + check_array2(100, 5, d.m, [ 0.99, 11, 1.96 ]);
    ne = ne + check_array2(100, 6, d.b, [ 7.8, 0.022, 0 ]);
  catch exc
    ne = ne + check_ok2(exc, 100, 2);
  end

  % 102: gd_alter_polynom
  try
    gd_alter_polynom(D, 'new5', 0, [ 1.1, 1.2, 1.3, 1.4, 1.5 ]);
  catch exc
    ne = ne + check_ok2(exc, 102, 1);
  end

  try
    d = gd_entry(D, 'new5');
    ne = ne + check_string2(102, 1, d.field, 'new5');
    ne = ne + check_num2(102, 2, d.field_type, GD.POLYNOM_ENTRY);
    ne = ne + check_num2(102, 3, d.fragment_index, 0);
    ne = ne + check_string2(102, 4, d.in_fields, 'in1');
    ne = ne + check_array2(102, 5, d.a, [1.1, 1.2, 1.3, 1.4, 1.5]);
  catch exc
    ne = ne + check_ok2(exc, 102, 2);
  end

  % 104: gd_alter_linterp
  try
    gd_alter_linterp(D, 'new6', 'in3', 0, 0);
  catch exc
    ne = ne + check_ok2(exc, 104, 1);
  end

  try
    d = gd_entry(D, 'new6');
    ne = ne + check_string2(104, 1, d.field, 'new6');
    ne = ne + check_num2(104, 2, d.field_type, GD.LINTERP_ENTRY);
    ne = ne + check_num2(104, 3, d.fragment_index, 0);
    ne = ne + check_string2(104, 4, d.in_fields, 'in3');
    ne = ne + check_string2(104, 5, d.table, './some/table');
  catch exc
    ne = ne + check_ok2(exc, 104, 2);
  end

  % 105: gd_alter_bit
  try
    gd_alter_bit(D, 'new7', 'in3', 3, 0);
  catch exc
    ne = ne + check_ok2(exc, 105, 1);
  end

  try
    d = gd_entry(D, 'new7');
    ne = ne + check_string2(105, 1, d.field, 'new7');
    ne = ne + check_num2(105, 2, d.field_type, GD.BIT_ENTRY);
    ne = ne + check_num2(105, 3, d.fragment_index, 0);
    ne = ne + check_string2(105, 4, d.in_fields, 'in3');
    ne = ne + check_num2(105, 5, d.bitnum, 3);
    ne = ne + check_num2(105, 6, d.numbits, 12);
  catch exc
    ne = ne + check_ok2(exc, 105, 2);
  end

  % 106: gd_alter_sbit
  try
    gd_alter_sbit(D, 'new8', 'in3', 3, 9);
  catch exc
    ne = ne + check_ok2(exc, 106, 1);
  end

  try
    d = gd_entry(D, 'new8');
    ne = ne + check_string2(106, 1, d.field, 'new8');
    ne = ne + check_num2(106, 2, d.field_type, GD.SBIT_ENTRY);
    ne = ne + check_num2(106, 3, d.fragment_index, 0);
    ne = ne + check_string2(106, 4, d.in_fields, 'in3');
    ne = ne + check_num2(106, 5, d.bitnum, 3);
    ne = ne + check_num2(106, 6, d.numbits, 9);
  catch exc
    ne = ne + check_ok2(exc, 106, 2);
  end

  % 107: gd_alter_multiply
  try
    gd_alter_multiply(D, 'new9', 'in6', 0);
  catch exc
    ne = ne + check_ok2(exc, 107, 1);
  end

  try
    d = gd_entry(D, 'new9');
    ne = ne + check_string2(107, 1, d.field, 'new9');
    ne = ne + check_num2(107, 2, d.field_type, GD.MULTIPLY_ENTRY);
    ne = ne + check_num2(107, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(107, 4, d.in_fields, { 'in6'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 107, 2);
  end

  % 108: alter phase
  try
    gd_alter_phase(D, 'new10', 'in2', 23);
  catch exc
    ne = ne + check_ok2(exc, 108, 1);
  end

  try
    d = gd_entry(D, 'new10');
    ne = ne + check_string2(108, 1, d.field, 'new10');
    ne = ne + check_num2(108, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(108, 3, d.fragment_index, 0);
    ne = ne + check_string2(108, 4, d.in_fields, 'in2');
    ne = ne + check_num2(108, 5, d.shift, 23);
  catch exc
    ne = ne + check_ok2(exc, 108, 2);
  end

  % 109: atler const
  try
    gd_alter_const(D, 'new11', GD.FLOAT64);
  catch exc
    ne = ne + check_ok2(exc, 109, 1);
  end

  try
    d = gd_entry(D, 'new11');
    ne = ne + check_string2(109, 1, d.field, 'new11');
    ne = ne + check_num2(109, 2, d.field_type, GD.CONST_ENTRY);
    ne = ne + check_num2(109, 3, d.fragment_index, 0);
    ne = ne + check_num2(109, 5, d.const_type, GD.FLOAT64);
  catch exc
    ne = ne + check_ok2(exc, 109, 2);
  end

  % 110: gd_encoding
  try
    d = gd_encoding(D, 0);
    ne = ne + check_num(110, d, GD.UNENCODED);
  catch exc
    ne = ne + check_ok(exc, 110);
  end

  % 111: gd_encoding
  try
    d = gd_endianness(D, 0);
    ne = ne + check_num(111, d, GD.LITTLE_ENDIAN + GD.NOT_ARM_ENDIAN);
  catch exc
    ne = ne + check_ok(exc, 111);
  end

  % 112: dirfilename
  try
    d = gd_dirfilename(D);
    ne = ne + check_eostring(112, d, 'dirfile');
  catch exc
    ne = ne + check_ok(exc, 112);
  end

  % 113: gd_parent_fragment
  try
    d = gd_parent_fragment(D, 1);
    ne = ne + check_num(113, d, 0);
  catch exc
    ne = ne + check_ok(exc, 113);
  end

  % 114: gd_alter_protection
  try
    gd_alter_protection(D, 0, GD.PROTECT_DATA);
  catch exc
    ne = ne + check_ok(exc, 114);
  end

  % 115: gd_protection
  try
    d = gd_protection(D, 0);
    ne = ne + check_num(115, d, GD.PROTECT_DATA);
  catch exc
    ne = ne + check_ok(exc, 115);
  end

  % 116: gd_raw_filename
  try
    d = gd_raw_filename(D, 'data');
    ne = ne + check_eostring(116, d, 'dirfile/data');
  catch exc
    ne = ne + check_ok(exc, 116);
  end

  % 117: gd_reference
  try
    d = gd_reference(D);
    ne = ne + check_string2(117, 1, d, 'data');
  catch exc
    ne = ne + check_ok2(exc, 117, 1);
  end

  try
    d = gd_reference(D, 'new1');
    ne = ne + check_string2(117, 2, d, 'new1');
  catch exc
    ne = ne + check_ok2(exc, 117, 2);
  end

  % 118: gd_eof
  try
    d = gd_eof(D, 'lincom');
    ne = ne + check_num(118, d, 81);
  catch exc
    ne = ne + check_ok(exc, 118);
  end

  % 119: gd_alter_encoding
  try
    gd_alter_encoding(D, GD.SLIM_ENCODED, 1, 0);
  catch exc
    ne = ne + check_ok(exc, 119);
  end

  % 120: gd_alter_endiannness
  try
    gd_alter_endianness(D, GD.BIG_ENDIAN, 1, 0);
  catch exc
    ne = ne + check_ok(exc, 120);
  end

  % 121: gd_alter_spec
  try
    gd_alter_spec(D, 'new10 PHASE in5 3', 0);
  catch exc
    ne = ne + check_ok2(exc, 121, 1);
  end

  try
    d = gd_entry(D, 'new10');
    ne = ne + check_string2(121, 1, d.field, 'new10');
    ne = ne + check_num2(121, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(121, 3, d.fragment_index, 0);
    ne = ne + check_string2(121, 4, d.in_fields, 'in5');
    ne = ne + check_num2(121, 5, d.shift, 3);
  catch exc
    ne = ne + check_ok2(exc, 121, 2);
  end

  % 122: gd_delete
  try
    gd_delete(D, 'new10', 0);
  catch exc
    ne = ne + check_ok2(exc, 122, 1);
  end

  try
    d = gd_entry(D, 'new10');
  catch exc
    ne = ne + check_exc2(exc, 122, 2, 'BadCode');
  end

  % 123: gd_malter_spec
  try
    gd_malter_spec(D, 'mnew10 PHASE in4 11', 'data', 0);
  catch exc
    ne = ne + check_ok2(exc, 123, 1);
  end

  try
    d = gd_entry(D, 'data/mnew10');
    ne = ne + check_string2(123, 1, d.field, 'data/mnew10');
    ne = ne + check_num2(123, 2, d.field_type, GD.PHASE_ENTRY);
    ne = ne + check_num2(123, 3, d.fragment_index, 0);
    ne = ne + check_string2(123, 4, d.in_fields, 'in4');
    ne = ne + check_num2(123, 5, d.shift, 11);
  catch exc
    ne = ne + check_ok2(exc, 123, 2);
  end

  % 124: move
  try
    gd_move(D, 'new9', 1, 0);
  catch exc
    ne = ne + check_ok2(exc, 124, 1);
  end

  try
    d = gd_fragment_index(D, 'new9');
    ne = ne + check_num(124, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 124, 2);
  end

  % 125: rename
  try
    gd_rename(D, 'new9', 'newer', 0);
  catch exc
    ne = ne + check_ok2(exc, 125, 1);
  end

  try
    d = gd_fragment_index(D, 'newer');
    ne = ne + check_num(125, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 125, 2);
  end

  % 126: unclude
  try
    gd_uninclude(D, 1, 0);
  catch exc
    ne = ne + check_ok2(exc, 126, 1);
  end

  try
    d = gd_nfragments(D);
    ne = ne + check_num(126, d, 1);
  catch exc
    ne = ne + check_ok2(exc, 126, 2);
  end

  % 127: frameoffset
  try
    d = gd_frameoffset(D, 0);
    ne = ne + check_num(127, d, 0);
  catch exc
    ne = ne + check_ok(exc, 127);
  end

  % 128: alter_frameoffset
  try
    gd_alter_frameoffset(D, 33, 0, 0);
  catch exc
    ne = ne + check_ok2(exc, 128, 1);
  end

  try
    d = gd_frameoffset(D, 0);
    ne = ne + check_num(128, d, 33);
  catch exc
    ne = ne + check_ok2(exc, 128, 2);
  end

  % 129: native_type
  try
    d = gd_native_type(D, 'data');
    ne = ne + check_num(129, d, GD.INT8);
  catch exc
    ne = ne + check_ok(exc, 129);
  end

  % 131: validate
  try
    d = gd_validate(D, 'new7');
  catch exc
    ne = ne + check_exc(exc, 131, 'BadCode');
  end

  % 132: framenum
  try
    gd_reference(D, 'data');
    d = gd_framenum(D, 'data', 33.3);
    ne = ne + check_num(132, d, 37.1625);
  catch exc
    ne = ne + check_ok(exc, 132);
  end

  % 133: framenum_subset
  try
    d = gd_framenum(D, 'data', 33.3, 6);
    ne = ne + check_num(133, d, 37.1625);
  catch exc
    ne = ne + check_ok(exc, 133);
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

  % 146: add entry (DIV) check
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

  % 177: gd_array_len
  try
    d = gd_array_len(D, 'carray');
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
    ne = ne + check_num2(178, 5, d.array_len, 6);
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
    ne = ne + check_num2(179, 5, d.array_len, 4);
  catch exc
    ne = ne + check_ok2(exc, 179, 2);
  end

  try
    d = gd_get_carray(D, 'new17');
    ne = ne + check_array(179, d, [1.1,2.2,3.3,4.4]);
  catch exc
    ne = ne + check_ok2(exc, 179, 3);
  end

  % 180: madd_carray
  try
    gd_madd_carray(D, 'data', 'mnew17', GD.FLOAT64, [1.8, 18.0]);
  catch exc
    ne = ne + check_ok2(exc, 180, 1);
  end

  try
    d = gd_entry(D, 'data/mnew17');
    ne = ne + check_string2(180, 1, d.field, 'data/mnew17');
    ne = ne + check_num2(180, 2, d.field_type, GD.CARRAY_ENTRY);
    ne = ne + check_num2(180, 3, d.fragment_index, 0);
    ne = ne + check_num2(180, 4, d.const_type, GD.FLOAT64);
    ne = ne + check_num2(180, 5, d.array_len, 2);
  catch exc
    ne = ne + check_ok2(exc, 180, 2);
  end

  try
    d = gd_get_carray(D, 'data/mnew17');
    ne = ne + check_array(180, d, [1.8, 18.0]);
  catch exc
    ne = ne + check_ok2(exc, 180, 3);
  end

  % 181: alter_carray
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
    ne = ne + check_num2(181, 5, d.array_len, 5);
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
    ne = ne + check_array(183, d, [86, 93]);
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
    ne = ne + check_sarray(222, d, { 'data'; 'alias'; 'new20'; 'data/mnew20' });
  catch exc
    ne = ne + check_ok(exc, 222);
  end

  % 223: include_affix
  try
    gd_include_affix(D, 'format1', 0, 'A', 'Z', GD.CREAT + GD.EXCL);
  catch exc
    ne = ne + check_ok2(exc, 223, 1);
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
    ne = ne + check_num2(228, 6, d.period, 10);
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
    ne = ne + check_num2(229, 6, d.period, 6);
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
    ne = ne + check_num2(230, 6, d.period, 12);
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
    ne = ne + check_num2(231, 6, d.period, 7);
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
    ne = ne + check_num(237, d, 7);
  catch exc
    ne = ne + check_ok(exc, 237);
  end

  % 239: gd_entry_list
  try
    d = gd_entry_list(D, 'data', GD.SCALAR_ENTRIES, ...
    GD.ENTRIES_HIDDEN + GD.ENTRIES_NOALIAS);
    ne = ne + check_sarray(239, d, {'mstr', 'mconst', 'mcarray', 'msarray', ...
    'mnew11', 'mnew12', 'mnew17'});
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

  % 242: gd_mcarrays
  try
    d = gd_mcarrays(D, 'data');
    ne = ne + check_num2(242, 1, length(d), 2);
    ne = ne + check_array2(242, 2, d{1}, [1.9, 2.8, 3.7, 4.6, 5.5]);
    ne = ne + check_array2(242, 3, d{2}, [1.8, 18.0]);
  catch exc
    ne = ne + check_ok(exc, 242);
  end

  % 271: gd_encoding_support
  d = gd_encoding_support(GD.SIE_ENCODED);
  ne = ne + check_num(271, d, GD.RDWR);

  % 272: NULL return from gd_reference
  try
    d = gd_open('dirfile/empty', GD.RDWR + GD.CREAT + GD.EXCL);
  catch exc
    ne = ne + check_ok2(exc, 272, 1);
  end

  try
    d = gd_reference(d);
    ne = ne + check_string(272, d, '');
  catch exc
    ne = ne + check_ok2(exc, 272, 1);
  end

  % 273: get_carray (NULL)
  try
    d = gd_get_carray(D, 'carray', GD.NULL);
    ne = ne + check_num(273, d, 0);
  catch exc
    ne = ne + check_ok(exc, 273);
  end

  % 274: gd_carray_slice (NULL)
  try
    d = gd_get_carray_slice(D, 'carray', 2, 2, GD.NULL);
    ne = ne + check_array(274, d, 0);
  catch exc
    ne = ne + check_ok(exc, 274);
  end

  % 277: gd_entry (SARRAY)
  try
    d = gd_entry(D, 'sarray');
    ne = ne + check_string2(277, 1, d.field, 'sarray');
    ne = ne + check_num2(277, 2, d.field_type, GD.SARRAY_ENTRY);
    ne = ne + check_num2(277, 3, d.fragment_index, 0);
    ne = ne + check_num2(277, 4, d.array_len, 7);
  catch exc
    ne = ne + check_ok(exc, 277);
  end

  % 278: get_sarray
  try
    d = gd_get_sarray(D, 'sarray');
    ne = ne + check_sarray(278, d, {'one'; 'two'; 'three'; 'four'; 'five'; ...
    'six'; 'seven'});
  catch exc
    ne = ne + check_ok(exc, 278);
  end

  % 279: gd_sarray_slice
  try
    d = gd_get_sarray_slice(D, 'sarray', 2, 2);
    ne = ne + check_sarray(279, d, {'three'; 'four'});
  catch exc
    ne = ne + check_ok(exc, 279);
  end

  % 280: gd_sarrays
  try
    d = gd_sarrays(D);
    ne = ne + check_num(280, length(d), 1);
    ne = ne + check_sarray(280, d{1}, {'one'; 'two'; 'three'; 'four'; ...
    'five'; 'six'; 'seven'});
  catch exc
    ne = ne + check_ok(exc, 280);
  end

  % 281: gd_put_sarray
  try
    gd_put_sarray(D, 'sarray', ...
    {'eka'; 'dvi'; 'tri'; 'catur'; 'panca'; 'sas'; 'sapta'});
  catch exc
    ne = ne + check_ok2(exc, 281, 0);
  end

  try
    d = gd_get_sarray(D, 'sarray');
    ne = ne + check_sarray(281, d, ...
    {'eka'; 'dvi'; 'tri'; 'catur'; 'panca'; 'sas'; 'sapta'});
  catch exc
    ne = ne + check_ok2(exc, 281, 1);
  end

  % 282: gd_put_sarray
  try
    gd_put_sarray_slice(D, 'sarray', 2, {'asta'; 'nava'});
  catch exc
    ne = ne + check_ok2(exc, 282, 0);
  end

  try
    d = gd_get_sarray(D, 'sarray');
    ne = ne + check_sarray(282, d, ...
    {'eka'; 'dvi'; 'asta'; 'nava'; 'panca'; 'sas'; 'sapta'});
  catch exc
    ne = ne + check_ok2(exc, 282, 1);
  end

  % 283: add_sarray
  try
    gd_add_sarray(D, 'new283', {'eins'; 'zwei'; 'drei'}, 0);
  catch exc
    ne = ne + check_ok2(exc, 283, 1);
  end

  try
    d = gd_entry(D, 'new283');
    ne = ne + check_string2(283, 1, d.field, 'new283');
    ne = ne + check_num2(283, 2, d.field_type, GD.SARRAY_ENTRY);
    ne = ne + check_num2(283, 3, d.fragment_index, 0);
    ne = ne + check_num2(283, 4, d.array_len, 3);
  catch exc
    ne = ne + check_ok2(exc, 283, 2);
  end

  try
    d = gd_get_sarray(D, 'new283');
    ne = ne + check_sarray(283, d, {'eins'; 'zwei'; 'drei'});
  catch exc
    ne = ne + check_ok2(exc, 283, 3);
  end

  % 285: madd_sarray
  try
    gd_madd_sarray(D, 'data', 'mnew285', {'un'; 'deux'; 'trois'});
  catch exc
    ne = ne + check_ok2(exc, 285, 1);
  end

  try
    d = gd_entry(D, 'data/mnew285');
    ne = ne + check_string2(285, 1, d.field, 'data/mnew285');
    ne = ne + check_num2(285, 2, d.field_type, GD.SARRAY_ENTRY);
    ne = ne + check_num2(285, 3, d.fragment_index, 0);
    ne = ne + check_num2(285, 4, d.array_len, 3);
  catch exc
    ne = ne + check_ok2(exc, 285, 2);
  end

  try
    d = gd_get_sarray(D, 'data/mnew285');
    ne = ne + check_sarray(285, d, {'un'; 'deux'; 'trois'});
  catch exc
    ne = ne + check_ok2(exc, 285, 3);
  end

  % 286: alter_sarray
  try
    gd_alter_sarray(D, 'new283', 2);
  catch exc
    ne = ne + check_ok2(exc, 286, 1);
  end

  try
    d = gd_entry(D, 'new283');
    ne = ne + check_string2(286, 1, d.field, 'new283');
    ne = ne + check_num2(286, 2, d.field_type, GD.SARRAY_ENTRY);
    ne = ne + check_num2(286, 3, d.fragment_index, 0);
    ne = ne + check_num2(286, 4, d.array_len, 2);
  catch exc
    ne = ne + check_ok2(exc, 286, 2);
  end

  % 287: gd_msarrays
  try
    d = gd_msarrays(D, 'data');
    ne = ne + check_num(287, length(d), 2);
    ne = ne + check_sarray2(287, 1, d{1}, ...
    {'eight'; 'nine'; 'ten'; 'eleven'; 'twelve'});
    ne = ne + check_sarray2(287, 2, d{2}, {'un'; 'deux'; 'trois'});
  catch exc
    ne = ne + check_ok(exc, 287);
  end

  % 288: entry (INDIR) check
  try
    d = gd_entry(D, 'indir');
    ne = ne + check_string2(288, 1, d.field, 'indir');
    ne = ne + check_num2(288, 2, d.field_type, GD.INDIR_ENTRY);
    ne = ne + check_num2(288, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(288, 4, d.in_fields, { 'data'; 'carray'; });
  catch exc
    ne = ne + check_ok(exc, 288);
  end

  % 289: add entry (INDIR) check
  try
    gd_add_indir(D, 'new289', 'in1', 'in2', 0);
  catch exc
    ne = ne + check_ok2(exc, 289, 1);
  end

  try
    d = gd_entry(D, 'new289');
    ne = ne + check_string2(289, 1, d.field, 'new289');
    ne = ne + check_num2(289, 2, d.field_type, GD.INDIR_ENTRY);
    ne = ne + check_num2(289, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(289, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 289, 2);
  end

  % 290: madd entry (INDIR) check
  try
    gd_madd_indir(D, 'data', 'mnew290', 'in1', 'in2');
  catch exc
    ne = ne + check_ok2(exc, 290, 1);
  end

  try
    d = gd_entry(D, 'data/mnew290');
    ne = ne + check_string2(290, 1, d.field, 'data/mnew290');
    ne = ne + check_num2(290, 2, d.field_type, GD.INDIR_ENTRY);
    ne = ne + check_num2(290, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(290, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 290, 2);
  end

  % 291: gd_alter_indir
  try
    gd_alter_indir(D, 'new289', 0, 'in6');
  catch exc
    ne = ne + check_ok2(exc, 291, 1);
  end

  try
    d = gd_entry(D, 'new289');
    ne = ne + check_string2(291, 1, d.field, 'new289');
    ne = ne + check_num2(291, 2, d.field_type, GD.INDIR_ENTRY);
    ne = ne + check_num2(291, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(291, 4, d.in_fields, { 'in1'; 'in6'; });
  catch exc
    ne = ne + check_ok2(exc, 291, 2);
  end

  % 292: entry (SINDIR) check
  try
    d = gd_entry(D, 'sindir');
    ne = ne + check_string2(292, 1, d.field, 'sindir');
    ne = ne + check_num2(292, 2, d.field_type, GD.SINDIR_ENTRY);
    ne = ne + check_num2(292, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(292, 4, d.in_fields, { 'data'; 'sarray'; });
  catch exc
    ne = ne + check_ok(exc, 292);
  end

  % 293: add entry (SINDIR) check
  try
    gd_add_sindir(D, 'new293', 'in1', 'in2', 0);
  catch exc
    ne = ne + check_ok2(exc, 293, 1);
  end

  try
    d = gd_entry(D, 'new293');
    ne = ne + check_string2(293, 1, d.field, 'new293');
    ne = ne + check_num2(293, 2, d.field_type, GD.SINDIR_ENTRY);
    ne = ne + check_num2(293, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(293, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 293, 2);
  end

  % 294: madd entry (SINDIR) check
  try
    gd_madd_sindir(D, 'data', 'mnew294', 'in1', 'in2');
  catch exc
    ne = ne + check_ok2(exc, 294, 1);
  end

  try
    d = gd_entry(D, 'data/mnew294');
    ne = ne + check_string2(294, 1, d.field, 'data/mnew294');
    ne = ne + check_num2(294, 2, d.field_type, GD.SINDIR_ENTRY);
    ne = ne + check_num2(294, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(294, 4, d.in_fields, { 'in1'; 'in2'; });
  catch exc
    ne = ne + check_ok2(exc, 294, 2);
  end

  % 295: gd_alter_sindir
  try
    gd_alter_sindir(D, 'new293', 0, 'in6');
  catch exc
    ne = ne + check_ok2(exc, 295, 1);
  end

  try
    d = gd_entry(D, 'new293');
    ne = ne + check_string2(295, 1, d.field, 'new293');
    ne = ne + check_num2(295, 2, d.field_type, GD.SINDIR_ENTRY);
    ne = ne + check_num2(295, 3, d.fragment_index, 0);
    ne = ne + check_sarray2(295, 4, d.in_fields, { 'in1'; 'in6'; });
  catch exc
    ne = ne + check_ok2(exc, 295, 2);
  end

  % 296: gd_getdata (SINDIR)
  try
    d = gd_getdata(D, 'sindir', 0, 0, 1, 0);
    ne = ne + check_sarray(296, d, ...
    {'eka'; 'eka'; 'eka'; 'eka'; 'eka'; 'eka'; 'eka'; 'eka';});
  catch exc
    ne = ne + check_ok(exc, 296);
  end

  % 302: gd_include_ns
  try
    gd_include(D, 'format2', 0, 'ns', GD.CREAT + GD.EXCL);
  catch exc
    ne = ne + check_ok2(exc, 302, 1);
  end

  % 303: gd_fragment_namespace (read)
  try
    d = gd_fragment_namespace(D, 2);
    ne = ne + check_string(303, d, 'ns');
  catch exc
    ne = ne + check_ok(exc, 303);
  end

  % 304: gd_fragment_namespace (alter)
  try
    d = gd_fragment_namespace(D, 2, 'ns2');
    ne = ne + check_string(304, d, 'ns2');
  catch exc
    ne = ne + check_ok(exc, 304);
  end

  % 305: gd_match_entries
  try
    d = gd_match_entries(D, '^lin', 0);
    ne = ne + check_sarray(305, d, {'lincom'; 'linterp'});
  catch exc
    ne = ne + check_ok(exc, 305);
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
