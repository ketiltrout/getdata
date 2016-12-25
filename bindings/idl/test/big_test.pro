; vim: ft=idlang
;
; Copyright (C) 2009-2015 D. V. Wiebe
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file is part of the GetData project.
;
; GetData is free software; you can redistribute it and/or modify it under
; the terms of the GNU Lesser General Public License as published by the
; Free Software Foundation; either version 2.1 of the License, or (at your
; option) any later version.
;
; GetData is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
; License for more details.
;
; You should have received a copy of the GNU Lesser General Public License
; along with GetData; if not, write to the Free Software Foundation, Inc.,
; 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; Load the helper routines
.r getdata_idl_test_routines

; If this IDL is running in timed demo mode, we can't perform this test,
; having neither DLM nor file functionality.
if (LMGR(/DEMO)) then timed_demo_mode

; General test

filedir = "test_dirfile"
format  = "test_dirfile/format"
form2   = "test_dirfile/form2"
data    = "test_dirfile/data"

flen    = 11
nfields = 20
nume    = 0

spawn, "rm -rf " + filedir
file_mkdir, filedir

datadata = bindgen(80) + 1

fields = [ 'bit', 'div', 'data', 'mult', 'sbit', 'INDEX', 'alias', 'const', $
  'indir', 'mplex', 'phase', 'recip', 'carray', 'lincom', 'sarray', 'sindir', $
  'string', 'window', 'linterp', 'polynom' ]

; Write the test dirfile
openw,1,format
printf,1,'/ENDIAN little'
printf,1,'data RAW INT8 8'
printf,1,'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const'
printf,1,'/META data mstr STRING "This is a string constant."'
printf,1,'/META data mconst CONST COMPLEX128 3.3;4.4'
printf,1,'/META data mlut LINTERP DATA ./lut'
printf,1,'const CONST FLOAT64 5.5'
printf,1,'carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6'
printf,1,'linterp LINTERP data ./lut'
printf,1,'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const'
printf,1,'bit BIT data 3 4'
printf,1,'sbit SBIT data 5 6'
printf,1,'mplex MPLEX data sbit 1 10'
printf,1,'mult MULTIPLY data sbit'
printf,1,'div DIVIDE mult bit'
printf,1,'recip RECIP div 6.5;4.3'
printf,1,'phase PHASE data 11'
printf,1,'window WINDOW linterp mult LT 4.1'
printf,1,'/ALIAS alias data'
printf,1,'string STRING "Zaphod Beeblebrox"'
printf,1,'sarray SARRAY one two three four five six seven'
printf,1,'data/msarray SARRAY eight nine ten eleven twelve'
printf,1,'indir INDIR data carray'
printf,1,'sindir SINDIR data sarray'
close,1

openw,1,form2
printf,1,'const2 CONST INT8 -19'
close,1

openw,1,data
writeu,1,byte(datadata)
close,1

;  0: getdata_constants check
defsysv, "!GD", getdata_constants()
nume += check_simple(0, !GD.E_OK, 0)

;  1: gd_error check
d = gd_open("x",error=error)
nume += check_error(1, d, !GD.E_IO)
nume += check_simple(1, error, !GD.E_IO)
gd_close, d, /DISCARD

;  2: gd_open check
d = gd_open(filedir, /RDWR)
nume += check_ok(2, d)
nume += check_simple(2, d, 1)

;  3: gd_getdata check
n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok(3, d)
nume += check_simple(3, n, INDGEN(8) + 41)

; 11: gd_getdata check (NULL)
n = gd_getdata(d, "data", type=!GD.NULL, first_frame=5, num_frames=1)
nume += check_ok(11, d)
nume += check_simple(11, n, 8)

;  12: gd_get_constant check
n = gd_get_constant(d, "const", type=!GD.FLOAT64)
nume += check_ok(12, d)
nume += check_simple(12, n, 5.5)

;  20: gd_get_constant check (NULL)
n = gd_get_constant(d, "const", type=!GD.NULL)
nume += check_ok(20, d)
nume += check_simple(20, n, 0)

;  23: gd_nfields check
n = gd_nfields(d)
nume += check_ok(23, d)
nume += check_simple(23, n, nfields)

;  25: gd_field_list check
n = gd_field_list(d)
nume += check_ok(25, d)
nume += check_simple(25, n, fields)

;  26: gd_nmfields_check
n = gd_nfields(d, parent="data")
nume += check_ok(26, d)
nume += check_simple(26, n, 4)

;  27: gd_mfield_list check
n = gd_field_list(d, parent="data")
nume += check_ok(27, d)
nume += check_simple(27, n, [ "mstr", "mconst", "mlut" ])

;  28: gd_nframes check
n = gd_nframes(d)
nume += check_ok(28, d)
nume += check_simple(28, n, 10)

;  29: gd_spf check
n = gd_spf(d, "data")
nume += check_ok(29, d)
nume += check_simple(29, n, 8)

;  30: gd_putdata check
gd_putdata, d, "data", [13, 14, 15, 16], first_frame=5, first_sample=1
nume += check_ok2(30, 1, d)

n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(30, 2, d)
nume += check_simple(30, n, [ 41, 13, 14, 15, 16, 46, 47, 48])

;  35: gd_putdata (float) check
gd_putdata, d, "data", [23., 24., 25., 26.], first_frame=5, first_sample=1
nume += check_ok2(35, 1, d)

n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(35, 2, d)
nume += check_simple(35, n, [ 41, 23, 24, 25, 26, 46, 47, 48])

;  37: gd_putdata (complex) check
gd_putdata, d, "data", [COMPLEX(33,0), COMPLEX(34,0), COMPLEX(35,0), $
  COMPLEX(36,0)], first_frame=5, first_sample=1
nume += check_ok2(37, 1, d)

n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(37, 2, d)
nume += check_simple(37, n, [ 41, 33, 34, 35, 36, 46, 47, 48])

;  38: gd_error_string check
n = gd_getdata(d, "x", num_frames=1, estring=estring)
nume += check_error(38, d, !GD.E_BAD_CODE)
nume += check_simple2(38, 1, gd_error_string(d), "Field not found: x")
nume += check_simple2(38, 2, estring, "Field not found: x")

;  39: gd_entry_type check
n = gd_entry_type(d, "data")
nume += check_ok(39, d)
nume += check_simple(39, n, !GD.RAW_ENTRY)

;  40: gd_entry (raw)
n = gd_entry(d, "data")
nume += check_ok(40, d)
nume += check_simple2(40, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(40, 2, n.field, "data")
nume += check_simple2(40, 3, n.fragment, 0)
nume += check_simple2(40, 4, n.data_type, !GD.INT8)
nume += check_simple2(40, 5, n.spf, 8)

;  42: gd_entry (lincom)
n = gd_entry(d, "lincom")
nume += check_ok(42, d)
nume += check_simple2(42, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(42, 2, n.field, "lincom")
nume += check_simple2(42, 3, n.fragment, 0)
nume += check_simple2(42, 4, n.n_fields, 3)
nume += check_simple2(42, 5, n.in_fields, [ "data", "INDEX", "linterp" ])
nume += check_simple2(42, 6, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(42, 7, n.cm, [ DCOMPLEX(1.1D,0), DCOMPLEX(2.2D,0), $
  DCOMPLEX(5.5D,0) ])
nume += check_simple2(42, 8, n.cb, [ DCOMPLEX(2.2D,0), DCOMPLEX(3.3D,4.4D), $
  DCOMPLEX(5.5D,0) ])
nume += check_simple2(42, 9, n.scalar, [ "", "", "const", "", "", "const" ])

;  44: gd_entry (polynom)
n = gd_entry(d, "polynom")
nume += check_ok(44, d)
nume += check_simple2(44, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(44, 2, n.field, "polynom")
nume += check_simple2(44, 3, n.fragment, 0)
nume += check_simple2(44, 4, n.poly_ord, 5)
nume += check_simple2(44, 5, n.in_fields, [ "data" ])
nume += check_simple2(44, 6, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(44, 7, n.ca, [ DCOMPLEX(1.1D,0), DCOMPLEX(2.2D,0), $
  DCOMPLEX(2.2D,0), DCOMPLEX(3.3D,4.4D), DCOMPLEX(5.5D,0), DCOMPLEX(5.5D,0) ])
nume += check_simple2(44, 8, n.scalar, [ "", "", "", "", "const", "const" ])

;  45: gd_entry (linterp)
n = gd_entry(d, "linterp")
nume += check_ok(45, d)
nume += check_simple2(45, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(45, 2, n.field, "linterp")
nume += check_simple2(45, 3, n.fragment, 0)
nume += check_simple2(45, 4, n.in_fields, [ "data" ])
nume += check_simple2(45, 5, n.table, "./lut")

;  46: gd_entry (bit)
n = gd_entry(d, "bit")
nume += check_ok(46, d)
nume += check_simple2(46, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(46, 2, n.field, "bit")
nume += check_simple2(46, 3, n.fragment, 0)
nume += check_simple2(46, 4, n.in_fields, [ "data" ])
nume += check_simple2(46, 5, n.numbits, 4)
nume += check_simple2(46, 6, n.bitnum, 3)

;  47: gd_entry (sbit)
n = gd_entry(d, "sbit")
nume += check_ok(47, d)
nume += check_simple2(47, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(47, 2, n.field, "sbit")
nume += check_simple2(47, 3, n.fragment, 0)
nume += check_simple2(47, 4, n.in_fields, [ "data" ])
nume += check_simple2(47, 5, n.numbits, 6)
nume += check_simple2(47, 6, n.bitnum, 5)

;  48: gd_entry (multiply)
n = gd_entry(d, "mult")
nume += check_ok(48, d)
nume += check_simple2(48, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(48, 2, n.field, "mult")
nume += check_simple2(48, 3, n.fragment, 0)
nume += check_simple2(48, 4, n.in_fields, [ "data", "sbit" ])

;  49: gd_entry (phase)
n = gd_entry(d, "phase")
nume += check_ok(49, d)
nume += check_simple2(49, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(49, 2, n.field, "phase")
nume += check_simple2(49, 3, n.fragment, 0)
nume += check_simple2(49, 4, n.in_fields, [ "data" ])
nume += check_simple2(49, 5, n.shift, 11)

;  50: gd_entry (const)
n = gd_entry(d, "const")
nume += check_ok(50, d)
nume += check_simple2(50, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(50, 2, n.field, "const")
nume += check_simple2(50, 3, n.fragment, 0)
nume += check_simple2(50, 4, n.data_type, !GD.FLOAT64)

;  51: gd_entry (string)
n = gd_entry(d, "string")
nume += check_ok(51, d)
nume += check_simple2(51, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(51, 2, n.field, "string")
nume += check_simple2(51, 3, n.fragment, 0)

;  52: gd_fragment_index check
n = gd_fragment_index(d, "data")
nume += check_ok(52, d)
nume += check_simple(52, n, 0)

;  53: gd_add_raw check
gd_add_raw, d, "new1", !GD.FLOAT64, spf=3
nume += check_ok2(53, 1, d)

n = gd_entry(d, "new1")
nume += check_ok2(53, 2, d)
nume += check_simple2(53, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(53, 2, n.field, "new1")
nume += check_simple2(53, 3, n.fragment, 0)
nume += check_simple2(53, 4, n.data_type, !GD.FLOAT64)
nume += check_simple2(53, 5, n.spf, 3)

;  55: gd_add_lincom
gd_add_lincom, d, "new2", "in1", COMPLEX(1.1, 1.2), COMPLEX(1.3, 1.4), $
  "in2", COMPLEX(1.4, 1.5), COMPLEX(1.6, 1.7)
nume += check_ok2(55, 1, d)

n = gd_entry(d, "new2")
nume += check_ok2(55, 2, d)
nume += check_simple2(55, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(55, 2, n.field, "new2")
nume += check_simple2(55, 3, n.fragment, 0)
nume += check_simple2(55, 4, n.n_fields, 2)
nume += check_simple2(55, 5, n.in_fields, [ "in1", "in2" ])
nume += check_simple2(55, 6, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(55, 7, n.cm, [ DCOMPLEX(1.1,1.2), DCOMPLEX(1.4,1.5) ])
nume += check_simple2(55, 8, n.cb, [ DCOMPLEX(1.3,1.4), DCOMPLEX(1.6,1.7) ])

;  57: gd_add_polynom
gd_add_polynom, d, "new4", "in1", DCOMPLEX(3.1,7.0D), DCOMPLEX(4.2,8.0), $
  DCOMPLEX(5.2,9.0), DCOMPLEX(6.3,4.4)
nume += check_ok2(57, 1, d)

n = gd_entry(d, "new4")
nume += check_ok2(57, 2, d)
nume += check_simple2(57, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(57, 2, n.field, "new4")
nume += check_simple2(57, 3, n.fragment, 0)
nume += check_simple2(57, 4, n.poly_ord, 3)
nume += check_simple2(57, 5, n.in_fields, [ "in1" ])
nume += check_simple2(57, 6, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(57, 7, n.ca, [ DCOMPLEX(3.1,7.0D), DCOMPLEX(4.2,8.0), $
  DCOMPLEX(5.2,9.0), DCOMPLEX(6.3,4.4) ])

;  58: gd_add_linterp
gd_add_linterp, d, "new6", "in", "./some/table"
nume += check_ok2(58, 1, d)

n = gd_entry(d, "new6")
nume += check_ok2(58, 2, d)
nume += check_simple2(58, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(58, 2, n.field, "new6")
nume += check_simple2(58, 3, n.fragment, 0)
nume += check_simple2(58, 4, n.in_fields, [ "in" ])
nume += check_simple2(58, 5, n.table, "./some/table")

;  59: gd_add_bit
gd_add_bit, d, "new7", "in1", bitnum=11, numbits=22
nume += check_ok2(59, 1, d)

n = gd_entry(d, "new7")
nume += check_ok2(59, 2, d)
nume += check_simple2(59, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(59, 2, n.field, "new7")
nume += check_simple2(59, 3, n.fragment, 0)
nume += check_simple2(59, 4, n.in_fields, [ "in1" ])
nume += check_simple2(59, 5, n.numbits, 22)
nume += check_simple2(59, 6, n.bitnum, 11)

;  60: gd_add_sbit
gd_add_sbit, d, "new8", "in2", bitnum=5, numbits=10
nume += check_ok2(60, 1, d)

n = gd_entry(d, "new8")
nume += check_ok2(60, 2, d)
nume += check_simple2(60, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(60, 2, n.field, "new8")
nume += check_simple2(60, 3, n.fragment, 0)
nume += check_simple2(60, 4, n.in_fields, [ "in2" ])
nume += check_simple2(60, 5, n.numbits, 10)
nume += check_simple2(60, 6, n.bitnum, 5)

;  61: gd_add_multiply
gd_add_multiply, d, "new9", "in2", "in3"
nume += check_ok2(61, 1, d)

n = gd_entry(d, "new9")
nume += check_ok(61, d)
nume += check_simple2(61, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(61, 2, n.field, "new9")
nume += check_simple2(61, 3, n.fragment, 0)
nume += check_simple2(61, 4, n.in_fields, [ "in2", "in3" ])

;  62: gd_add_phase
gd_add_phase, d, "new10", "in6", 42
nume += check_ok2(62, 1, d)

n = gd_entry(d, "new10")
nume += check_ok2(62, 2, d)
nume += check_simple2(62, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(62, 2, n.field, "new10")
nume += check_simple2(62, 3, n.fragment, 0)
nume += check_simple2(62, 4, n.in_fields, [ "in6" ])
nume += check_simple2(62, 5, n.shift, 42)

;  63: gd_add_const
gd_add_const, d, "new11", type=!GD.FLOAT64, value=4.3D
nume += check_ok2(63, 1, d)

n = gd_entry(d, "new11")
nume += check_ok2(63, 2, d)
nume += check_simple2(63, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(63, 2, n.field, "new11")
nume += check_simple2(63, 3, n.fragment, 0)
nume += check_simple2(63, 4, n.data_type, !GD.FLOAT64)

n = gd_get_constant(d, "new11")
nume += check_ok2(63, 3, d)
nume += check_simple2(63, 5, n, 4.3D)

;  64: gd_fragmentname
n = gd_fragmentname(d, 0)
nume += check_ok(64, d)
nume += check_eostring(64, n, "test_dirfile/format")

;  65: gd_nfragments
n = gd_nfragments(d)
nume += check_ok(65, d)
nume += check_simple(65, n, 1)

;  66: gd_include
gd_include, d, "form2"
nume += check_ok2(66, 1, d)

n = gd_get_constant(d, "const2", type=!GD.INT16)
nume += check_ok2(66, 2, d)
nume += check_simple(66, n, -19)

;  67: gd_nfields_by_type check
n = gd_nfields(d,type=!GD.LINCOM_ENTRY)
nume += check_ok(67, d)
nume += check_simple(67, n, 2)

;  68: gd_field_list_by_type check
n = gd_field_list(d, type=!GD.LINCOM_ENTRY)
nume += check_ok(68, d)
nume += check_simple(68, n, [ 'new2', 'lincom' ])

;  69: gd_nvectors check
n = gd_nvectors(d)
nume += check_ok(69, d)
nume += check_simple(69, n, 23)

;  70: gd_vector_list check
n = gd_vector_list(d)
nume += check_ok(70, d)
nume += check_simple(70, n, [ 'bit', 'div', 'data', 'mult', 'new1', 'new2', $
  'new4', 'new6', 'new7', 'new8', 'new9', 'sbit', 'INDEX', 'alias', 'indir', $
  'mplex', 'new10', 'phase', 'recip', 'lincom', 'window', 'linterp', $
  'polynom' ])

;  71: gd_madd_lincom
gd_add_lincom, d, "mnew2", "in1", 9.9D, 8.8D, "in2", 7.7D, 6.6D, $
  parent="data"
nume += check_ok2(71, 1, d)

n = gd_entry(d, "data/mnew2")
nume += check_ok2(71, 2, d)
nume += check_simple2(71, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(71, 2, n.field, "data/mnew2")
nume += check_simple2(71, 3, n.fragment, 0)
nume += check_simple2(71, 4, n.n_fields, 2)
nume += check_simple2(71, 5, n.in_fields, [ "in1", "in2" ])
nume += check_simple2(71, 6, n.flags, !GD.EN_CALC)
nume += check_simple2(71, 7, n.m, [ 9.9D, 7.7D ])
nume += check_simple2(71, 8, n.b, [ 8.8D, 6.6D ])

;  73: gd_madd_polynom
gd_add_polynom, d, "mnew4", "in1", [ 3.3D, 4.4D, 5.5D, 6.6D ], $
  parent="data"
nume += check_ok2(73, 1, d)

n = gd_entry(d, "data/mnew4")
nume += check_ok2(73, 2, d)
nume += check_simple2(73, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(73, 2, n.field, "data/mnew4")
nume += check_simple2(73, 3, n.fragment, 0)
nume += check_simple2(73, 4, n.poly_ord, 3)
nume += check_simple2(73, 5, n.in_fields, [ "in1" ])
nume += check_simple2(73, 6, n.flags, !GD.EN_CALC)
nume += check_simple2(73, 7, n.a, [ 3.3D, 4.4D, 5.5D, 6.6D ])

;  75: gd_madd_linterp
gd_add_linterp, d, "mnew6", "in", "./more/table", parent="data"
nume += check_ok2(75, 1, d)

n = gd_entry(d, "data/mnew6")
nume += check_ok2(75, 2, d)
nume += check_simple2(75, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(75, 2, n.field, "data/mnew6")
nume += check_simple2(75, 3, n.fragment, 0)
nume += check_simple2(75, 4, n.in_fields, [ "in" ])
nume += check_simple2(75, 5, n.table, "./more/table")

;  76: gd_madd_bit
gd_add_bit, d, "mnew7", "in1", bitnum=21, numbits=12, parent="data"
nume += check_ok2(76, 1, d)

n = gd_entry(d, "data/mnew7")
nume += check_ok2(76, 2, d)
nume += check_simple2(76, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(76, 2, n.field, "data/mnew7")
nume += check_simple2(76, 3, n.fragment, 0)
nume += check_simple2(76, 4, n.in_fields, [ "in1" ])
nume += check_simple2(76, 5, n.numbits, 12)
nume += check_simple2(76, 6, n.bitnum, 21)

;  77: gd_madd_sbit
gd_add_sbit, d, "mnew8", "in3", bitnum=2, numbits=14, parent="data"
nume += check_ok2(77, 1, d)

n = gd_entry(d, "data/mnew8")
nume += check_ok2(77, 2, d)
nume += check_simple2(77, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(77, 2, n.field, "data/mnew8")
nume += check_simple2(77, 3, n.fragment, 0)
nume += check_simple2(77, 4, n.in_fields, [ "in3" ])
nume += check_simple2(77, 5, n.numbits, 14)
nume += check_simple2(77, 6, n.bitnum, 2)

;  78: gd_madd_multiply
gd_add_multiply, d, "mnew9", "in4", "in1", parent="data"
nume += check_ok2(78, 1, d)

n = gd_entry(d, "data/mnew9")
nume += check_ok2(78, 2, d)
nume += check_simple2(78, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(78, 2, n.field, "data/mnew9")
nume += check_simple2(78, 3, n.fragment, 0)
nume += check_simple2(78, 4, n.in_fields, [ "in4", "in1" ])

;  79: gd_madd_phase
gd_add_phase, d, "mnew10", "in1", -4, parent="data"
nume += check_ok2(79, 1, d)

n = gd_entry(d, "data/mnew10")
nume += check_ok2(79, 2, d)
nume += check_simple2(79, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(79, 2, n.field, "data/mnew10")
nume += check_simple2(79, 3, n.fragment, 0)
nume += check_simple2(79, 4, n.in_fields, [ "in1" ])
nume += check_simple2(79, 5, n.shift, -4)

;  80: gd_madd_const
gd_add_const, d, "mnew11", type=!GD.UINT64, parent="data"
nume += check_ok2(80, 1, d)

n = gd_entry(d, "data/mnew11")
nume += check_ok2(80, 2, d)
nume += check_simple2(80, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(80, 2, n.field, "data/mnew11")
nume += check_simple2(80, 3, n.fragment, 0)
nume += check_simple2(80, 4, n.data_type, !GD.UINT64)

;  81: gd_get_string
n = gd_get_string(d, "string")
nume += check_ok(81, d)
nume += check_simple(81, n, "Zaphod Beeblebrox")

;  82: gd_add_string
gd_add_string, d, "new12", value="a string"
nume += check_ok2(82, 1, d)

n = gd_entry(d, "new12")
nume += check_ok2(82, 2, d)
nume += check_simple2(82, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(82, 2, n.field, "new12")
nume += check_simple2(82, 3, n.fragment, 0)

n = gd_get_string(d, "new12")
nume += check_ok2(82, 3, d)
nume += check_simple2(82, 4, n, "a string")

;  83: gd_madd_string
gd_add_string, d, "mnew12", value="another string", parent="data"
nume += check_ok2(83, 1, d)

n = gd_entry(d, "data/mnew12")
nume += check_ok2(83, 2, d)
nume += check_simple2(83, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(83, 2, n.field, "data/mnew12")
nume += check_simple2(83, 3, n.fragment, 0)

n = gd_get_string(d, "data/mnew12")
nume += check_ok2(83, 3, d)
nume += check_simple2(83, 4, n, "another string")

;  84: gd_add_spec
gd_add_spec, d, 'lorem STRING "Lorem ipsum"'
nume += check_ok2(84, 1, d)

n = gd_entry(d, "lorem")
nume += check_ok2(84, 2, d)
nume += check_simple2(84, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(84, 2, n.field, "lorem")
nume += check_simple2(84, 3, n.fragment, 0)

n = gd_get_string(d, "lorem")
nume += check_ok2(84, 3, d)
nume += check_simple2(84, 4, n, "Lorem ipsum")

;  85: gd_madd_string
gd_add_spec, d, 'ipsum STRING "dolor sit amet."', parent="lorem"
nume += check_ok2(85, 1, d)

n = gd_entry(d, "lorem/ipsum")
nume += check_ok2(85, 2, d)
nume += check_simple2(85, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(85, 2, n.field, "lorem/ipsum")
nume += check_simple2(85, 3, n.fragment, 0)

n = gd_get_string(d, "lorem/ipsum")
nume += check_ok2(85, 3, d)
nume += check_simple2(85, 4, n, "dolor sit amet.")

;  86: gd_put_constant
gd_put_constant, d, "const", 86
nume += check_ok2(86, 1, d)

n = gd_get_constant(d, "const", type=!GD.INT32)
nume += check_ok2(86, 2, d)
nume += check_simple(86, n, 86)

;  94: gd_put_string
gd_put_string, d, "string", "Arthur Dent"
nume += check_ok2(94, 1, d)

n = gd_get_string(d, "string")
nume += check_ok2(94, 2, d)
nume += check_simple(94, n, "Arthur Dent")

;  95: gd_nmfields_by_type
n = gd_nfields(d, parent="data", type=!GD.LINCOM_ENTRY)
nume += check_ok(95, d)
nume += check_simple(95, n, 1)

;  96: gd_mfield_list_by_type
n = gd_field_list(d, parent="data", type=!GD.LINCOM_ENTRY)
nume += check_ok(96, d)
nume += check_simple(96, n, [ "mnew2" ])

;  97: gd_nmvectors
n = gd_nvectors(d, parent="data")
nume += check_ok(97, d)
nume += check_simple(97, n, 8)

;  98: gd_mvector_list check
n = gd_vector_list(d, parent="data")
nume += check_ok(98, d)
nume += check_simple(98, n, [ 'mlut', 'mnew2', 'mnew4', 'mnew6', $
  'mnew7', 'mnew8', 'mnew9', 'mnew10' ])

;  99: gd_alter_raw check
gd_alter_raw, d, "new1", type=!GD.INT32
nume += check_ok2(99, 1, d)

n = gd_entry(d, "new1")
nume += check_ok2(99, 2, d)
nume += check_simple2(99, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(99, 2, n.field, "new1")
nume += check_simple2(99, 3, n.fragment, 0)
nume += check_simple2(99, 4, n.data_type, !GD.INT32)
nume += check_simple2(99, 5, n.spf, 3)

;  101: gd_alter_lincom
gd_alter_lincom, d, "new2", in_fields=[ "in3", "in4" ], $
  m=[ COMPLEX(2.3, 4.5), COMPLEX(6.7, 8.9) ]
nume += check_ok2(101, 1, d)

n = gd_entry(d, "new2")
nume += check_ok2(101, 2, d)
nume += check_simple2(101, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(101, 2, n.field, "new2")
nume += check_simple2(101, 3, n.fragment, 0)
nume += check_simple2(101, 4, n.n_fields, 2)
nume += check_simple2(101, 5, n.in_fields, [ "in3", "in4" ])
nume += check_simple2(101, 6, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(101, 7, n.cm, [ DCOMPLEX(2.3,4.5), DCOMPLEX(6.7,8.9) ])
nume += check_simple2(101, 8, n.cb, [ DCOMPLEX(1.3,1.4), DCOMPLEX(1.6,1.7) ])

;  103: gd_alter_polynom
gd_alter_polynom, d, "new4", poly_ord=4, a=[ DCOMPLEX(1.2,3.4), $
  DCOMPLEX(5.6,7.8), DCOMPLEX(9.0,1.2), DCOMPLEX(3.4,5.6), DCOMPLEX(7.8,9.0) ]
nume += check_ok2(103, 1, d)

n = gd_entry(d, "new4")
nume += check_ok2(103, 2, d)
nume += check_simple2(103, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(103, 2, n.field, "new4")
nume += check_simple2(103, 3, n.fragment, 0)
nume += check_simple2(103, 4, n.poly_ord, 4)
nume += check_simple2(103, 5, n.in_fields, [ "in1" ])
nume += check_simple2(103, 6, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(103, 7, n.ca, [ DCOMPLEX(1.2,3.4), DCOMPLEX(5.6,7.8), $
  DCOMPLEX(9.0,1.2), DCOMPLEX(3.4,5.6), DCOMPLEX(7.8,9.0) ])

;  104: gd_alter_linterp
gd_alter_linterp, d, "new6", table="./other/table"
nume += check_ok2(104, 1, d)

n = gd_entry(d, "new6")
nume += check_ok2(104, 2, d)
nume += check_simple2(104, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(104, 2, n.field, "new6")
nume += check_simple2(104, 3, n.fragment, 0)
nume += check_simple2(104, 4, n.in_fields, [ "in" ])
nume += check_simple2(104, 5, n.table, "./other/table")

;  105: gd_alter_bit
gd_alter_bit, d, "new7", in_field="in3",  numbits=8
nume += check_ok2(105, 1, d)

n = gd_entry(d, "new7")
nume += check_ok2(105, 2, d)
nume += check_simple2(105, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(105, 2, n.field, "new7")
nume += check_simple2(105, 3, n.fragment, 0)
nume += check_simple2(105, 4, n.in_fields, [ "in3" ])
nume += check_simple2(105, 5, n.numbits, 8)
nume += check_simple2(105, 6, n.bitnum, 11)

;  106: gd_alter_sbit
gd_alter_sbit, d, "new8", bitnum=15, numbits=1
nume += check_ok2(106, 1, d)

n = gd_entry(d, "new8")
nume += check_ok2(106, 2, d)
nume += check_simple2(106, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(106, 2, n.field, "new8")
nume += check_simple2(106, 3, n.fragment, 0)
nume += check_simple2(106, 4, n.in_fields, [ "in2" ])
nume += check_simple2(106, 5, n.numbits, 1)
nume += check_simple2(106, 6, n.bitnum, 15)

;  107: gd_alter_multiply
gd_alter_multiply, d, "new9", in_field1="in6"
nume += check_ok2(107, 1, d)

n = gd_entry(d, "new9")
nume += check_ok(107, d)
nume += check_simple2(107, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(107, 2, n.field, "new9")
nume += check_simple2(107, 3, n.fragment, 0)
nume += check_simple2(107, 4, n.in_fields, [ "in6", "in3" ])

;  108: gd_alter_phase
gd_alter_phase, d, "new10", shift=108
nume += check_ok2(108, 1, d)

n = gd_entry(d, "new10")
nume += check_ok2(108, 2, d)
nume += check_simple2(108, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(108, 2, n.field, "new10")
nume += check_simple2(108, 3, n.fragment, 0)
nume += check_simple2(108, 4, n.in_fields, [ "in6" ])
nume += check_simple2(108, 5, n.shift, 108)

;  109: gd_alter_const
gd_alter_const, d, "new11", type=!GD.FLOAT32
nume += check_ok2(109, 1, d)

n = gd_entry(d, "new11")
nume += check_ok2(109, 2, d)
nume += check_simple2(109, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(109, 2, n.field, "new11")
nume += check_simple2(109, 3, n.fragment, 0)
nume += check_simple2(109, 4, n.data_type, !GD.FLOAT32)

;  110: gd_encoding
n = gd_encoding(d, fragment=0)
nume += check_ok(110, d)
nume += check_simple(110, n, !GD.UNENCODED)

;  111: gd_endianness
n = gd_endianness(d, fragment=0)
nume += check_ok(111, d)
nume += check_simple(111, n, (!GD.LITTLE_ENDIAN + !GD.NOT_ARM_ENDIAN))

;  112: dirfilename
n = gd_dirfilename(d)
nume += check_ok(112, d)
nume += check_eostring(112, n, "test_dirfile")

;  113: gd_parent_fragment
n = gd_parent_fragment(d, fragment=1)
nume += check_ok(113, d)
nume += check_simple(113, n, 0)

;  114: gd_alter_protection
gd_alter_protection, d, !GD.PROTECT_DATA, fragment=1
nume += check_ok(114, d)

;  115: gd_protection
n = gd_protection(d, fragment=1)
nume += check_ok(115, d)
nume += check_simple(115, n, !GD.PROTECT_DATA)

;  116: gd_raw_filename
n = gd_raw_filename(d, 'data')
nume += check_ok(116, d)
nume += check_eostring(116, n, "test_dirfile/data")

;  117: gd_reference
n = gd_reference(d, "new1")
nume += check_ok(117, d)
nume += check_simple(117, n, "new1");

;  118: gd_eof
n = gd_eof(d, "lincom")
nume += check_ok(118,d)
nume += check_simple(118,n,80)

;  119: gd_alter_encoding
gd_alter_encoding, d, !GD.SLIM_ENCODED, fragment=1
nume += check_ok2(119, 1, d)

n = gd_encoding(d, fragment=1)
nume += check_ok2(119, 2, d)
nume += check_simple(119, n, !GD.SLIM_ENCODED)

;  120: gd_alter_endianness
gd_alter_endianness, d, /big_endian, fragment=1
nume += check_ok2(120, 1, d)

n = gd_endianness(d, fragment=1)
nume += check_ok2(120, 2, d)
nume += check_simple(120, n, !GD.BIG_ENDIAN)

;  121: gd_alter_spec
gd_alter_spec, d, "new10 PHASE in 3"
nume += check_ok2(121, 1, d)

n = gd_entry(d, "new10")
nume += check_ok2(121, 2, d)
nume += check_simple2(121, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(121, 2, n.field, "new10")
nume += check_simple2(121, 3, n.fragment, 0)
nume += check_simple2(121, 4, n.in_fields, [ "in" ])
nume += check_simple2(121, 5, n.shift, 3)

;  122: gd_delete
gd_delete, d, "new10"
nume += check_ok2(122, 1, d)

n = gd_entry(d, "new10")
nume += check_error2(122, 2, d, !GD.E_BAD_CODE)

;  123: gd_malter_spec
gd_alter_spec, d, "mnew10 PHASE in4 11", parent="data"
nume += check_ok2(123, 1, d)

n = gd_entry(d, "data/mnew10")
nume += check_ok2(123, 2, d)
nume += check_simple2(123, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(123, 2, n.field, "data/mnew10")
nume += check_simple2(123, 3, n.fragment, 0)
nume += check_simple2(123, 4, n.in_fields, [ "in4" ])
nume += check_simple2(123, 5, n.shift, 11)

;  124: gd_move
gd_move, d, "new9", 1
nume += check_ok2(124, 1, d)

n = gd_entry(d, "new9")
nume += check_ok2(124, 2, d)
nume += check_simple2(124, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(124, 2, n.field, "new9")
nume += check_simple2(124, 3, n.fragment, 1)

;  125: gd_rename
gd_rename, d, "new9", "newer"
nume += check_ok2(125, 1, d)

n = gd_entry(d, "new9")
nume += check_error2(125, 2, d, !GD.E_BAD_CODE)

n = gd_entry(d, "newer")
nume += check_ok2(125, 3, d)
nume += check_simple2(125, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(125, 2, n.field, "newer")
nume += check_simple2(125, 3, n.fragment, 1)

;  126: gd_uninclude
gd_uninclude, d, 1
nume += check_ok2(126, 1, d)

n = gd_entry(d, "newer")
nume += check_error2(126, 2, d, !GD.E_BAD_CODE)

;  127: gd_frameoffset
n = gd_frameoffset(d,fragment=0)
nume += check_ok(127,d)
nume += check_simple(127,n,0)

;  128: gd_alter_frameoffset
gd_alter_frameoffset, d, 33, fragment=0
nume += check_ok2(128,1,d)

n = gd_frameoffset(d,fragment=0)
nume += check_ok(128,d)
nume += check_simple(128,n,33)

;  129: gd_native_type
n = gd_native_type(d, "data")
nume += check_ok(129, d)
nume += check_simple(129,n,!GD.INT8)

;  131: gd_validate
n =  gd_validate(d, "new7")
nume += check_error(131,d,!GD.E_BAD_CODE)
nume += check_simple(131,n,!GD.E_BAD_CODE)

;  133: gd_framenum
n = gd_framenum(d, "data", 33.3, field_start=6)
nume += check_ok(133,d)
nume += check_float(133, n, 37.037500D)

;  135: gd_add
n = {field: "new135", field_type: !GD.RAW_ENTRY, fragment: 0, $
  spf: 5U, data_type: !GD.FLOAT32}
gd_add, d, n
nume += check_ok2(135, 1, d)

n = gd_entry(d, "new135")
nume += check_ok2(135, 2, d)
nume += check_simple2(135, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(135, 2, n.field, "new135")
nume += check_simple2(135, 3, n.fragment, 0)
nume += check_simple2(135, 4, n.data_type, !GD.FLOAT32)
nume += check_simple2(135, 5, n.spf, 5)

;  136: gd_madd
n = {field: "mnew136", field_type: !GD.PHASE_ENTRY, $
  shift: 2L, in_fields: [ "in1" ], scalar: [ "" ], scalar_ind: [ 0 ]}
gd_add, d, n, parent="data"
nume += check_ok2(136, 1, d)

n = gd_entry(d, "data/mnew136")
nume += check_ok2(136, 2, d)
nume += check_simple2(136, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(136, 2, n.field, "data/mnew136")
nume += check_simple2(136, 3, n.fragment, 0)
nume += check_simple2(136, 4, n.in_fields, [ "in1" ])
nume += check_simple2(136, 5, n.shift, 2)

;  141: gd_alter
n = {field_type: !GD.RAW_ENTRY, data_type: !GD.FLOAT64}
gd_alter_entry, d, "new135", n
nume += check_ok2(141, 1, d)

n = gd_entry(d, "new135")
nume += check_ok2(141, 2, d)
nume += check_simple2(141, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(141, 2, n.field, "new135")
nume += check_simple2(141, 3, n.fragment, 0)
nume += check_simple2(141, 4, n.data_type, !GD.FLOAT64)
nume += check_simple2(141, 5, n.spf, 5)

;  142: gd_bof
n = gd_bof(d, "lincom")
nume += check_ok(142,d)
nume += check_simple(142,n,264)

;  143: gd_entry (divide)
n = gd_entry(d, "div")
nume += check_ok(143, d)
nume += check_simple2(143, 1, n.field_type, !GD.DIVIDE_ENTRY)
nume += check_simple2(143, 2, n.field, "div")
nume += check_simple2(143, 3, n.fragment, 0)
nume += check_simple2(143, 4, n.in_fields, [ "mult", "bit" ])

;  145: gd_entry (recip)
n = gd_entry(d, "recip")
nume += check_ok(145, d)
nume += check_simple2(145, 1, n.field_type, !GD.RECIP_ENTRY)
nume += check_simple2(145, 2, n.field, "recip")
nume += check_simple2(145, 3, n.fragment, 0)
nume += check_simple2(145, 4, n.in_fields, [ "div" ])
nume += check_simple2(145, 5, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(145, 6, n.cdividend, DCOMPLEX(6.5D,4.3D))

;  146: gd_add_divide
gd_add_divide, d, "new14", "in2", "in3"
nume += check_ok2(146, 1, d)

n = gd_entry(d, "new14")
nume += check_ok(146, d)
nume += check_simple2(146, 1, n.field_type, !GD.DIVIDE_ENTRY)
nume += check_simple2(146, 2, n.field, "new14")
nume += check_simple2(146, 3, n.fragment, 0)
nume += check_simple2(146, 4, n.in_fields, [ "in2", "in3" ])

;  148: gd_add_recip
gd_add_recip, d, "new16", "in2", dividend=COMPLEX(33.3, 44.4)
nume += check_ok2(148, 1, d)

n = gd_entry(d, "new16")
nume += check_ok(148, d)
nume += check_simple2(148, 1, n.field_type, !GD.RECIP_ENTRY)
nume += check_simple2(148, 2, n.field, "new16")
nume += check_simple2(148, 3, n.fragment, 0)
nume += check_simple2(148, 4, n.in_fields, [ "in2" ])
nume += check_simple2(148, 5, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(148, 6, n.cdividend, DCOMPLEX(33.3, 44.4))

;  152: gd_alter_divide
gd_alter_divide, d, "new14", in_field1="in6"
nume += check_ok2(152, 1, d)

n = gd_entry(d, "new14")
nume += check_ok(152, d)
nume += check_simple2(152, 1, n.field_type, !GD.DIVIDE_ENTRY)
nume += check_simple2(152, 2, n.field, "new14")
nume += check_simple2(152, 3, n.fragment, 0)
nume += check_simple2(152, 4, n.in_fields, [ "in6", "in3" ])

;  153: gd_alter_recip
gd_alter_recip, d, "new16", dividend=1.01
nume += check_ok2(153, 1, d)

n = gd_entry(d, "new16")
nume += check_ok(153, d)
nume += check_simple2(153, 1, n.field_type, !GD.RECIP_ENTRY)
nume += check_simple2(153, 2, n.field, "new16")
nume += check_simple2(153, 3, n.fragment, 0)
nume += check_simple2(153, 5, n.flags, !GD.EN_CALC)
nume += check_simple2(153, 4, n.in_fields, [ "in2" ])
nume += check_simple2(153, 6, n.dividend, 1.01)

;  155: gd_rewrite_fragment
gd_rewrite_fragment, d, fragment=0
nume += check_ok(155, d)

;  156: gd_invalid_dirfile
m = gd_invalid_dirfile()
nume += check_ok2(156, 1, m)
n = gd_nfragments(m)
nume += check_error2(156, 2, m, !GD.E_BAD_DIRFILE)
gd_close, m

;  157: gd_dirfile_standards
n = gd_dirfile_standards(d, /CURRENT)
nume += check_ok2(157, 1, d)
nume += check_simple(157, n, !GD.DIRFILE_STANDARDS_VERSION)
n = gd_dirfile_standards(d, 0)
nume += check_error2(157, 2, d, !GD.E_ARGUMENT)

;  158: gd_get_carray
n = gd_get_carray(d, "carray", type=!GD.FLOAT32)
nume += check_ok(158, d)
nume += check_simple(158, n, [ 1.1, 2.2, 3.3, 4.4, 5.5, 6.6 ])

;  159: gd_get_carray_slice
n = gd_get_carray(d, "carray", type=!GD.FLOAT32, len=2, start=2)
nume += check_ok(159, d)
nume += check_simple(159, n, [ 3.3, 4.4 ])

;  168: gd_put_carray
m = [ 9.8, 8.7, 7.6, 6.5, 5.4, 4.3 ]
gd_put_carray, d, "carray", m
nume += check_ok(168, d)

n = gd_get_carray(d, "carray", type=!GD.FLOAT32)
nume += check_ok(168, d)
nume += check_simple(168, n, m)

;  169: gd_put_carray_slice
gd_put_carray, d, "carray", [ 33, 34 ], start=2
nume += check_ok(169, d)

n = gd_get_carray(d, "carray", type=!GD.FLOAT32)
nume += check_ok(169, d)
nume += check_simple(169, n, [ 9.8, 8.7, 33., 34., 5.4, 4.3 ])

;  177: gd_array_len
n = gd_array_len(d, "carray")
nume += check_ok(177, d)
nume += check_simple(177, n, 6)

;  178: gd_entry (CARRAY)
n = gd_entry(d, "carray")
nume += check_ok(178, d)
nume += check_simple2(178, 1, n.field_type, !GD.CARRAY_ENTRY)
nume += check_simple2(178, 2, n.field, "carray")
nume += check_simple2(178, 3, n.fragment, 0)
nume += check_simple2(178, 4, n.data_type, !GD.FLOAT64)
nume += check_simple2(178, 5, n.array_len, 6)

;  179: gd_add_carray
gd_add_carray, d, "new17", type=!GD.FLOAT64, value=[3.3D, 4.3D]
nume += check_ok2(179, 1, d)

n = gd_entry(d, "new17")
nume += check_ok2(179, 2, d)
nume += check_simple2(179, 1, n.field_type, !GD.CARRAY_ENTRY)
nume += check_simple2(179, 2, n.field, "new17")
nume += check_simple2(179, 3, n.fragment, 0)
nume += check_simple2(179, 4, n.data_type, !GD.FLOAT64)
nume += check_simple2(179, 5, n.array_len, 2)

n = gd_get_carray(d, "new17")
nume += check_ok2(179, 3, d)
nume += check_simple2(179, 5, n, [3.3D, 4.3D])

;  180: gd_madd_carray
gd_add_carray, d, "mnew17", type=!GD.INT16, parent="data", value=[33, 43]
nume += check_ok2(180, 1, d)

n = gd_entry(d, "data/mnew17")
nume += check_ok2(180, 2, d)
nume += check_simple2(180, 1, n.field_type, !GD.CARRAY_ENTRY)
nume += check_simple2(180, 2, n.field, "data/mnew17")
nume += check_simple2(180, 3, n.fragment, 0)
nume += check_simple2(180, 4, n.data_type, !GD.INT16)
nume += check_simple2(180, 5, n.array_len, 2)

n = gd_get_carray(d, "data/mnew17", type=!GD.INT16)
nume += check_ok2(180, 3, d)
nume += check_simple2(180, 6, n, [33, 43])

;  181: gd_alter_carray
gd_alter_carray, d, "new17", type=!GD.FLOAT32, len=3
nume += check_ok2(181, 1, d)

n = gd_entry(d, "new17")
nume += check_ok2(181, 2, d)
nume += check_simple2(181, 1, n.field_type, !GD.CARRAY_ENTRY)
nume += check_simple2(181, 2, n.field, "new17")
nume += check_simple2(181, 3, n.fragment, 0)
nume += check_simple2(181, 4, n.data_type, !GD.FLOAT32)
nume += check_simple2(181, 5, n.array_len, 3)

;  183: gd_constants
n = gd_constants(d, type=!GD.FLOAT32)
nume += check_ok(183, d)

;  191: gd_mconstants
n = gd_constants(d, parent="data", type=!GD.FLOAT32)
nume += check_ok(191, d)
nume += check_simple(191, n, [ 3.3, 0. ])

; 199: gd_strings
n = gd_strings(d)
nume += check_ok(199, d)
nume += check_simple(199, n, [ "Lorem ipsum", "a string", "Arthur Dent" ])

; 200: gd_strings
n = gd_strings(d, parent="data")
nume += check_ok(200, d)
nume += check_simple(200, n, [ "This is a string constant.", "another string" ])

; 203: gd_seek
n = gd_seek(d, "data", frame_num=35)
nume += check_ok2(203, 0, d)
m = gd_getdata(d, "data", type=!GD.INT16, num_frames=1)
nume += check_ok2(203, 1, d)
nume += check_simple2(203, 0, n, 280)
nume += check_simple2(203, 1, m, INDGEN(8) + 17)

; 204: gd_tell
n = gd_tell(d, "data")
nume += check_ok(204, d)
nume += check_simple(204, n, 288)

; 205: gd_hide check
gd_hide, d, 'data'
nume += check_ok(205, d)

; 206: gd_hidden check
n = gd_hidden(d, 'data')
nume += check_ok2(206, 1, d)
nume += check_simple2(206, 1, n, 1)

n = gd_hidden(d, 'lincom')
nume += check_ok2(206, 2, d)
nume += check_simple2(206, 2, n, 0)

; 207: gd_unhide check
gd_unhide, d, 'data'
nume += check_ok2(206, 1, d)
n = gd_hidden(d, 'data')
nume += check_ok2(206, 2, d)
nume += check_simple(206, n, 0)

; 208: gd_sync check
gd_flush, d, field_code='data', /noclose
nume += check_ok(208, d)

; 209: gd_flush check
gd_flush, d, field_code='data'
nume += check_ok(209, d)

; 210: gd_metaflush check
gd_metaflush, d
nume += check_ok(210, d)

; 211: gd_entry (WINDOW) check
n = gd_entry(d, 'window')
nume += check_ok(211, d)
nume += check_simple2(211, 1, n.field_type, !GD.WINDOW_ENTRY)
nume += check_simple2(211, 2, n.fragment, 0)
nume += check_simple2(211, 3, n.windop, !GD.WINDOP_LT)
nume += check_simple2(211, 4, n.in_fields, [ 'linterp', 'mult' ])
nume += check_simple2(211, 6, n.rthreshold, 4.1D0)

; 212: gd_add_window check
gd_add_window, d, 'new18', 'in1', 'in2', /NE, 32, fragment=0
nume += check_ok2(212, 1, d)

n = gd_entry(d, 'new18')
nume += check_ok2(212, 2, d)
nume += check_simple2(212, 1, n.field_type, !GD.WINDOW_ENTRY)
nume += check_simple2(212, 2, n.fragment, 0)
nume += check_simple2(212, 3, n.windop, !GD.WINDOP_NE)
nume += check_simple2(212, 4, n.in_fields, [ 'in1', 'in2' ])
nume += check_simple2(212, 6, n.ithreshold, 32)

; 214: gd_madd_window check
gd_add_window, d, parent='data', 'mnew18', 'in2', 'in3', /SET, 128
nume += check_ok2(214, 1, d)

n = gd_entry(d, 'data/mnew18')
nume += check_ok2(214, 2, d)
nume += check_simple2(214, 1, n.field_type, !GD.WINDOW_ENTRY)
nume += check_simple2(214, 2, n.fragment, 0)
nume += check_simple2(214, 3, n.windop, !GD.WINDOP_SET)
nume += check_simple2(214, 4, n.in_fields, [ 'in2', 'in3' ])
nume += check_simple2(214, 6, n.uthreshold, 128)

; 217: gd_alter_window check
gd_alter_window, d, 'new18', in_field='in3', check_field='in4', /GE, $
  threshold=32e3
nume += check_ok2(217, 1, d)

n = gd_entry(d, 'new18')
nume += check_ok2(217, 2, d)
nume += check_simple2(217, 1, n.field_type, !GD.WINDOW_ENTRY)
nume += check_simple2(217, 2, n.fragment, 0)
nume += check_simple2(217, 3, n.windop, !GD.WINDOP_GE)
nume += check_simple2(217, 4, n.in_fields, [ 'in3', 'in4' ])
nume += check_simple2(217, 6, n.rthreshold, 32d3)

; 218: gd_alias_target check
str = gd_alias_target(d, 'alias')
nume += check_ok(218, d)
nume += check_simple(218, str, 'data')

; 219: gd_add_alias check
gd_add_alias, d, 'new20', 'data', fragment=0
nume += check_ok2(219, 1, d)

str = gd_alias_target(d, 'new20')
nume += check_ok2(219, 2, d)
nume += check_simple(219, str, 'data')

; 220: gd_madd_alias check
gd_add_alias, d, parent='data', 'mnew20', 'data'
nume += check_ok2(220, 1, d)

str = gd_alias_target(d, 'data/mnew20')
nume += check_ok2(220, 2, d)
nume += check_simple(220, str, 'data')

; 221: gd_naliases check
n = gd_naliases(d, 'data')
nume += check_ok(221, d)
nume += check_simple(221, n, 4)

; 222: gd_alias check
n = gd_aliases(d, 'data')
nume += check_ok(222, d)
nume += check_simple(222, n, [ 'data', 'alias', 'new20', 'data/mnew20' ])

; 223: gd_include_affix check
gd_include, d, 'format1', prefix='A', suffix='Z', /CREAT, /EXCL
nume += check_ok(223, d)

; 226: gd_fragment_affixes check
n = gd_fragment_affixes(d, fragment=1)
nume += check_ok(226, d)
nume += check_simple(226, n, [ "A", "Z" ])

; 227: gd_alter_affixes check
gd_alter_affixes, d, fragment=1, prefix='B', suffix=''
nume += check_ok2(227, 1, d)

n = gd_fragment_affixes(d, fragment=1)
nume += check_ok2(227, 2, d)
nume += check_simple(227, n, [ "B", "" ])

; 228: gd_entry (MPLEX) check
n = gd_entry(d, 'mplex')
nume += check_ok(228, d)
nume += check_simple2(228, 1, n.field_type, !GD.MPLEX_ENTRY)
nume += check_simple2(228, 2, n.fragment, 0)
nume += check_simple2(228, 3, n.count_val, 1)
nume += check_simple2(228, 4, n.in_fields, [ 'data', 'sbit' ])
nume += check_simple2(228, 5, n.period, 10)

; 229: gd_add_mplex check
gd_add_mplex, d, 'new21', 'in1', 'in2', 5, max=6
nume += check_ok2(229, 1, d)

n = gd_entry(d, 'new21')
nume += check_ok2(229, 2, d)
nume += check_simple2(229, 1, n.field_type, !GD.MPLEX_ENTRY)
nume += check_simple2(229, 2, n.fragment, 0)
nume += check_simple2(229, 3, n.count_val, 5)
nume += check_simple2(229, 4, n.in_fields, [ 'in1', 'in2' ])
nume += check_simple2(229, 5, n.period, 6)

; 230: gd_madd_mplex check
gd_add_mplex, d, parent='data', 'mnew21', 'in2', 'in3', 0, max=12
nume += check_ok2(230, 1, d)

n = gd_entry(d, 'data/mnew21')
nume += check_ok2(230, 2, d)
nume += check_simple2(230, 1, n.field_type, !GD.MPLEX_ENTRY)
nume += check_simple2(230, 2, n.fragment, 0)
nume += check_simple2(230, 3, n.count_val, 0)
nume += check_simple2(230, 4, n.in_fields, [ 'in2', 'in3' ])
nume += check_simple2(230, 5, n.period, 12)

; 231: gd_alter_mplex check
gd_alter_mplex, d, 'new21', in_field='in3', count_field='in4', count_val=2, $
  period=7
nume += check_ok2(231, 1, d)

n = gd_entry(d, 'new21')
nume += check_ok2(231, 2, d)
nume += check_simple2(231, 1, n.field_type, !GD.MPLEX_ENTRY)
nume += check_simple2(231, 2, n.fragment, 0)
nume += check_simple2(231, 3, n.count_val, 2)
nume += check_simple2(231, 4, n.in_fields, [ 'in3', 'in4' ])
nume += check_simple2(231, 5, n.period, 7)

; 232: gd_strtok check
str = gd_strtok(d, STRING='"test1 test2" test3\ test4')
nume += check_ok2(232, 1 ,d)
nume += check_simple2(232, 2, str, "test1 test2")

str = gd_strtok(d)
nume += check_ok2(232, 3 ,d)
nume += check_simple2(232, 4, str, "test3 test4")

; 233: gd_raw_close check
gd_flush, d, field_code='data', /nosync
nume += check_ok(233, d)

; 234: gd_desync check
n = gd_desync(d)
nume += check_ok(234, d)
nume += check_simple(234, n, 0)

; 235: gd_flags check
n = gd_flags(d, /PRETTY_PRINT)
nume += check_ok(235, d)
nume += check_simple(235, n, !GD.PRETTY_PRINT)

; 236: gd_verbose_prefix
gd_verbose_prefix, d, prefix="big_test"
nume += check_ok(236, d)

; 237: gd_nentries check
n = gd_nentries(d, parent="data", /SCALARS, /HIDDEN, /NOALIAS)
nume += check_ok2(237, 1, d)
nume += check_simple2(237, 1, n, 6)
n = gd_nentries(d, /VECTORS, /HIDDEN, /NOALIAS)
nume += check_ok2(237, 2, d)
nume += check_simple2(237, 2, n, 25)
 
; 239: gd_entry_list check
n = gd_entry_list(d, /VECTORS, /HIDDEN, /NOALIAS)
nume += check_ok(239, d)
nume += check_simple(239, n, [ 'bit', 'div', 'data', 'mult', 'new1', 'new2', $
  'new4', 'new6', 'new7', 'new8', 'sbit', 'INDEX', 'indir', 'mplex', 'new14', $
  'new16', 'new18', 'new21', 'phase', 'recip', 'lincom', 'new135', 'window', $
  'linterp', 'polynom' ])

; 240: gd_mplex_lookback check
gd_mplex_lookback, d, /ALL
nume += check_ok(240, d)

; 241: gd_linterp_tablename
n = gd_linterp_tablename(d, 'linterp')
nume += check_ok(241, d)
nume += check_eostring(241, n, "test_dirfile/lut")

; 243: add lincom
n = {field: 'new243', field_type: !GD.LINCOM_ENTRY, in_fields: [ "in1", "in2", $
  "in3" ], m: [ DCOMPLEX(1.1D,0), DCOMPLEX(0,0), DCOMPLEX(1.4D,0) ], $
  scalar: [ '', 'const', '', 'carray', 'carray', 'carray' ], $
  scalar_ind: [ 0, -1, 0, 3, 4, 5 ], fragment: 0}
gd_add, d, n
nume += check_ok2(243, 1, d)

n = gd_entry(d, "new243")
nume += check_ok2(243, 2, d)
nume += check_simple2(243, 3, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(243, 4, n.field, "new243")
nume += check_simple2(243, 5, n.fragment, 0)
nume += check_simple2(243, 6, n.n_fields, 3)
nume += check_simple2(243, 7, n.in_fields, [ "in1", "in2", "in3" ])
nume += check_simple2(243, 8, n.flags, !GD.EN_CALC)
nume += check_simple2(243, 9, n.cm, [ DCOMPLEX(1.1D,0), DCOMPLEX(86D,0), $
  DCOMPLEX(1.4D,0) ])
nume += check_simple2(243, 10, n.m, [ 1.1D, 86D, 1.4D ])
nume += check_simple2(243, 11, n.cb, [ DCOMPLEX(34D,0), DCOMPLEX(5.4,0), $
  DCOMPLEX(4.3,0) ])
nume += check_simple2(243, 12, n.b, [ 34D, 5.4, 4.3 ])
nume += check_simple2(243, 13, n.scalar, [ "", "const", "", "carray", $
  "carray", "carray" ])
nume += check_simple2(243, 14, n.scalar_ind, [ 0, -1, 0, 3, 4, 5 ])

;  244: gd_add polynom
n = {field: 'new244', field_type: !GD.POLYNOM_ENTRY, in_fields: 'in2', $
  a: [ DCOMPLEX(33D, 0), DCOMPLEX(44D, 55D), DCOMPLEX(66D, 0) ], $
  scalar: [ "", "", "", "carray" ], scalar_ind: [ 0, 0, 0 ], fragment: 0}
gd_add, d, n
nume += check_ok2(244, 1, d)

n = gd_entry(d, "new244")
nume += check_ok2(244, 2, d)
nume += check_simple2(244, 3, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(244, 4, n.field, "new244")
nume += check_simple2(244, 5, n.fragment, 0)
nume += check_simple2(244, 6, n.poly_ord, 3)
nume += check_simple2(244, 7, n.in_fields, [ "in2" ])
nume += check_simple2(244, 8, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(244, 9, n.ca, [ DCOMPLEX(33D, 0), DCOMPLEX(44D, 55D), $
  DCOMPLEX(66D, 0) ])
nume += check_simple2(244, 10, n.a, [ 33D, 44D, 66D ])
nume += check_simple2(244, 11, n.scalar, [ "", "", "", "carray" ])
nume += check_simple2(244, 12, n.scalar_ind, [ 0, 0, 0, 0 ])

;  245: gd_add linterp
n = {field: "new245", field_type: !GD.LINTERP_ENTRY, fragment: 0, $
  in_fields: "in", table: "./some/table"};
gd_add, d, n
nume += check_ok2(245, 1, d)

n = gd_entry(d, "new245")
nume += check_ok2(21, 2, d)
nume += check_simple2(21, 3, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(21, 4, n.field, "new245")
nume += check_simple2(21, 5, n.fragment, 0)
nume += check_simple2(21, 6, n.in_fields, [ "in" ])
nume += check_simple2(21, 7, n.table, "./some/table")

;  246: gd_add bit
n = {field: "new246", field_type: !GD.BIT_ENTRY, fragment: 0, bitnum: 11, $
  in_fields: [ "in1" ]}
gd_add, d, n
nume += check_ok2(245, 1, d)

n = gd_entry(d, "new246")
nume += check_ok2(22, 2, d)
nume += check_simple2(22, 3, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(22, 4, n.field, "new246")
nume += check_simple2(22, 5, n.fragment, 0)
nume += check_simple2(22, 6, n.in_fields, [ "in1" ])
nume += check_simple2(22, 7, n.numbits, 1)
nume += check_simple2(22, 8, n.bitnum, 11)

;  247: gd_add multiply
n = {field: 'new247', field_type: !GD.MULTIPLY_ENTRY, fragment: 0, $
  in_fields: [ "in2", "in3"]};
gd_add, d, n
nume += check_ok2(247, 1, d)

n = gd_entry(d, "new247")
nume += check_ok2(247, 2, d)
nume += check_simple2(247, 3, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(247, 4, n.field, "new247")
nume += check_simple2(247, 5, n.fragment, 0)
nume += check_simple2(247, 6, n.in_fields, [ "in2", "in3" ])

;  248: gd_add phase
n = {field: "new248", field_type: !GD.PHASE_ENTRY, fragment: 0, $
  shift: -88L, in_fields: [ "new9" ], scalar: [ "" ], scalar_ind: [ 0 ]}
gd_add, d, n
nume += check_ok2(248, 1, d)

n = gd_entry(d, "new248")
nume += check_ok2(248, 2, d)
nume += check_simple2(248, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(248, 2, n.field, "new248")
nume += check_simple2(248, 3, n.fragment, 0)
nume += check_simple2(248, 4, n.in_fields, [ "new9" ])
nume += check_simple2(248, 5, n.shift, -88)

;  249: gd_add const
n = {field: 'new249', field_type: !GD.CONST_ENTRY, fragment: 0, $
  data_type: !GD.FLOAT32}
gd_add, d, n
nume += check_ok2(249, 1, d)

n = gd_entry(d, "new249")
nume += check_ok2(249, 2, d)
nume += check_simple2(249, 3, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(249, 4, n.field, "new249")
nume += check_simple2(249, 5, n.fragment, 0)
nume += check_simple2(249, 6, n.data_type, !GD.FLOAT32)

n = gd_get_constant(d, "new249")
nume += check_ok2(249, 7, d)
nume += check_simple2(249, 8, n, 0)

;  250: gd_add string
n = {field: 'new250', field_type: !GD.STRING_ENTRY, fragment: 0, $
  data_type: !GD.FLOAT32}
gd_add, d, n
nume += check_ok2(250, 1, d)

n = gd_entry(d, "new250")
nume += check_ok2(250, 2, d)
nume += check_simple2(250, 3, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(250, 4, n.field, "new250")
nume += check_simple2(250, 5, n.fragment, 0)

n = gd_get_string(d, "new250")
nume += check_ok2(250, 6, d)
nume += check_simple2(250, 7, n, 0)

;  251: gd_add recip
n = {field: 'Bnew251', field_type: !GD.RECIP_ENTRY, fragment: 1, $
  in_fields: 'Bin1', dividend: COMPLEX(33.3, 44.4)}
gd_add, d, n
nume += check_ok2(251, 1, d)

n = gd_entry(d, "Bnew251")
nume += check_ok2(145, 2, d)
nume += check_simple2(145, 3, n.field_type, !GD.RECIP_ENTRY)
nume += check_simple2(145, 4, n.field, "Bnew251")
nume += check_simple2(145, 5, n.fragment, 1)
nume += check_simple2(145, 6, n.in_fields, [ "Bin1" ])
nume += check_simple2(145, 7, n.flags, !GD.EN_COMPSCAL + !GD.EN_CALC)
nume += check_simple2(145, 8, n.cdividend, DCOMPLEX(33.3, 44.4))
nume += check_simple2(145, 9, n.dividend, 33.3)

;  252: gd_add carray
n = {field: 'new252', field_type: !GD.CARRAY_ENTRY, fragment: 0, $
  data_type: !GD.FLOAT32, array_len: 5}
gd_add, d, n
nume += check_ok2(252, 1, d)

n = gd_entry(d, "new252")
nume += check_ok2(252, 2, d)
nume += check_simple2(252, 3, n.field_type, !GD.CARRAY_ENTRY)
nume += check_simple2(252, 4, n.field, "new252")
nume += check_simple2(252, 5, n.fragment, 0)
nume += check_simple2(252, 6, n.data_type, !GD.FLOAT32)
nume += check_simple2(252, 7, n.array_len, 5)

n = gd_get_constant(d, "new252")
nume += check_ok2(252, 8, d)
nume += check_simple2(252, 9, n, [0, 0, 0, 0, 0])

;  253: gd_add window
n = {field: 'new253', field_type: !GD.WINDOW_ENTRY, fragment: 0, $
  windop: !GD.WINDOP_NE, threshold: 32, in_fields: [ "in1", "in2" ]}
gd_add, d, n
nume += check_ok2(253, 1, d)

n = gd_entry(d, 'new253')
nume += check_ok2(253, 2, d)
nume += check_simple2(253, 3, n.field, "new253")
nume += check_simple2(253, 4, n.field_type, !GD.WINDOW_ENTRY)
nume += check_simple2(253, 5, n.fragment, 0)
nume += check_simple2(253, 6, n.windop, !GD.WINDOP_NE)
nume += check_simple2(253, 7, n.in_fields, [ 'in1', 'in2' ])
nume += check_simple2(253, 8, n.ithreshold, 32)

;  254: gd_add mplex
n = {field: 'new254', field_type: !GD.MPLEX_ENTRY, fragment: 0, $
  count_val: 5, in_fields: [ "in1", "in2" ]}
gd_add, d, n
nume += check_ok2(254, 1, d)

n = gd_entry(d, 'new254')
nume += check_ok2(229, 2, d)
nume += check_simple2(253, 3, n.field, "new254")
nume += check_simple2(229, 4, n.field_type, !GD.MPLEX_ENTRY)
nume += check_simple2(229, 5, n.fragment, 0)
nume += check_simple2(229, 6, n.count_val, 5)
nume += check_simple2(229, 7, n.in_fields, [ 'in1', 'in2' ])
nume += check_simple2(229, 8, n.period, 0)

;  259: gd_alter_entry with scalar
n = {field_type: !GD.LINCOM_ENTRY, scalar: [ "", "const", "const", "carray", $
  "", "const" ], scalar_ind: [ 0, 0, 0, 4, 0, -1 ]}
gd_alter_entry, d, 'new243', n

n = gd_entry(d, "new243")
nume += check_ok2(259, 2, d)
nume += check_simple2(259, 3, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(259, 4, n.field, "new243")
nume += check_simple2(259, 5, n.fragment, 0)
nume += check_simple2(259, 6, n.n_fields, 3)
nume += check_simple2(259, 7, n.in_fields, [ "in1", "in2", "in3" ])
nume += check_simple2(259, 8, n.flags, !GD.EN_CALC)
nume += check_simple2(259, 9, n.cm, [ DCOMPLEX(1.1D,0), DCOMPLEX(86D,0), $
  DCOMPLEX(86D,0) ])
nume += check_simple2(259, 10, n.m, [ 1.1D, 86D, 86D ])
nume += check_simple2(259, 11, n.cb, [ DCOMPLEX(5.4,0), DCOMPLEX(5.4,0), $
  DCOMPLEX(86D,0) ])
nume += check_simple2(259, 12, n.b, [ 5.4, 5.4, 86D ])
nume += check_simple2(259, 13, n.scalar, [ "", "const", "const", "carray", $
  "", "const" ])

;  271: gd_encoding_support
n = gd_encoding_support(!GD.SIE_ENCODED)
nume += check_simple(271, n, !GD.RDWR)

;  273: gd_get_carray
n = gd_get_carray(d, "carray", type=!GD.NULL)
nume += check_ok(273, d)
nume += check_simple(273, n, 0)

;  274: gd_get_carray_slice
n = gd_get_carray(d, "carray", type=!GD.NULL, len=2, start=2)
nume += check_ok(274, d)
nume += check_simple(274, n, 0)

;  277: gd_entry (SARRAY)
n = gd_entry(d, "sarray")
nume += check_ok(277, d)
nume += check_simple2(277, 1, n.field_type, !GD.SARRAY_ENTRY)
nume += check_simple2(277, 2, n.field, "sarray")
nume += check_simple2(277, 3, n.fragment, 0)
nume += check_simple2(277, 4, n.array_len, 7)

;  278: gd_get_sarray
n = gd_get_sarray(d, "sarray")
nume += check_ok(278, d)
nume += check_simple(278, n, $
  [ 'one', 'two', 'three', 'four', 'five', 'six', 'seven' ])

;  279: gd_get_sarray_slice
n = gd_get_sarray(d, "sarray", len=2, start=2)
nume += check_ok(279, d)
nume += check_simple(279, n, [ 'three', 'four' ])

;  281: gd_put_sarray
m = [ 'eka', 'dvi', 'tri', 'catur', 'panca', 'sas', 'sapta' ]
gd_put_sarray, d, "sarray", m
nume += check_ok(281, d)

n = gd_get_sarray(d, "sarray")
nume += check_ok(281, d)
nume += check_simple(281, n, m)

;  282: gd_put_sarray_slice
gd_put_sarray, d, "sarray", [ 'asta', 'nava' ], start=2
nume += check_ok(282, d)

n = gd_get_sarray(d, "sarray")
nume += check_ok(282, d)
nume += check_simple(282, n, $
  [ 'eka', 'dvi', 'asta', 'nava', 'panca', 'sas', 'sapta' ])

;  283: gd_add_sarray
gd_add_sarray, d, "new283", value=['un', 'deux']
nume += check_ok2(283, 1, d)

n = gd_entry(d, "new283")
nume += check_ok2(283, 2, d)
nume += check_simple2(283, 1, n.field_type, !GD.SARRAY_ENTRY)
nume += check_simple2(283, 2, n.field, "new283")
nume += check_simple2(283, 3, n.fragment, 0)
nume += check_simple2(283, 4, n.array_len, 2)

n = gd_get_sarray(d, "new283")
nume += check_ok2(283, 3, d)
nume += check_simple2(283, 5, n, ['un', 'deux'])

;  285: gd_madd_sarray
gd_add_sarray, d, "mnew285", parent="data", value=['eins', 'zwei']
nume += check_ok2(285, 1, d)

n = gd_entry(d, "data/mnew285")
nume += check_ok2(285, 2, d)
nume += check_simple2(285, 1, n.field_type, !GD.SARRAY_ENTRY)
nume += check_simple2(285, 2, n.field, "data/mnew285")
nume += check_simple2(285, 3, n.fragment, 0)
nume += check_simple2(285, 4, n.array_len, 2)

n = gd_get_sarray(d, "data/mnew285")
nume += check_ok2(285, 3, d)
nume += check_simple2(285, 6, n, ['eins', 'zwei'])

;  286: gd_alter_sarray
gd_alter_sarray, d, "new283", len=3
nume += check_ok2(286, 1, d)

n = gd_entry(d, "new283")
nume += check_ok2(286, 2, d)
nume += check_simple2(286, 1, n.field_type, !GD.SARRAY_ENTRY)
nume += check_simple2(286, 2, n.field, "new283")
nume += check_simple2(286, 3, n.fragment, 0)
nume += check_simple2(286, 4, n.array_len, 3)

;  288: gd_entry (indir)
n = gd_entry(d, "indir")
nume += check_ok(288, d)
nume += check_simple2(288, 1, n.field_type, !GD.INDIR_ENTRY)
nume += check_simple2(288, 2, n.field, "indir")
nume += check_simple2(288, 3, n.fragment, 0)
nume += check_simple2(288, 4, n.in_fields, [ "data", "carray" ])

;  289: gd_add_indir
gd_add_indir, d, "new289", "in2", "in3"
nume += check_ok2(289, 1, d)

n = gd_entry(d, "new289")
nume += check_ok(289, d)
nume += check_simple2(289, 1, n.field_type, !GD.INDIR_ENTRY)
nume += check_simple2(289, 2, n.field, "new289")
nume += check_simple2(289, 3, n.fragment, 0)
nume += check_simple2(289, 4, n.in_fields, [ "in2", "in3" ])

;  291: gd_alter_indir
gd_alter_indir, d, "new289", in_field1="in6"
nume += check_ok2(291, 1, d)

n = gd_entry(d, "new289")
nume += check_ok(291, d)
nume += check_simple2(291, 1, n.field_type, !GD.INDIR_ENTRY)
nume += check_simple2(291, 2, n.field, "new289")
nume += check_simple2(291, 3, n.fragment, 0)

;  292: gd_entry (sindir)
n = gd_entry(d, "sindir")
nume += check_ok(292, d)
nume += check_simple2(292, 1, n.field_type, !GD.SINDIR_ENTRY)
nume += check_simple2(292, 2, n.field, "sindir")
nume += check_simple2(292, 3, n.fragment, 0)
nume += check_simple2(292, 4, n.in_fields, [ "data", "sarray" ])

;  293: gd_add_sindir
gd_add_sindir, d, "new293", "in2", "in3"
nume += check_ok2(293, 1, d)

n = gd_entry(d, "new293")
nume += check_ok(293, d)
nume += check_simple2(293, 1, n.field_type, !GD.SINDIR_ENTRY)
nume += check_simple2(293, 2, n.field, "new293")
nume += check_simple2(293, 3, n.fragment, 0)
nume += check_simple2(293, 4, n.in_fields, [ "in2", "in3" ])

;  295: gd_alter_sindir
gd_alter_sindir, d, "new293", in_field1="in6"
nume += check_ok2(295, 1, d)

n = gd_entry(d, "new293")
nume += check_ok(295, d)
nume += check_simple2(295, 1, n.field_type, !GD.SINDIR_ENTRY)
nume += check_simple2(295, 2, n.field, "new293")
nume += check_simple2(295, 3, n.fragment, 0)
nume += check_simple2(295, 4, n.in_fields, [ "in6", "in3" ])

;  296: gd_getstrdata
n = gd_getdata(d, "sindir", first_frame=0, num_frames=1)
nume += check_ok(296, d)
nume += check_simple(296, n, $
  ['eka', 'eka', 'eka', 'eka', 'eka', 'eka', 'eka', 'eka'])

;  300: gd_add (INDIR)
n = {field: 'new300', field_type: !GD.INDIR_ENTRY, fragment: 0, $
  in_fields: [ "in3", "in0"]};
gd_add, d, n
nume += check_ok2(300, 1, d)

n = gd_entry(d, "new300")
nume += check_ok2(300, 2, d)
nume += check_simple2(300, 3, n.field_type, !GD.INDIR_ENTRY)
nume += check_simple2(300, 4, n.field, "new300")
nume += check_simple2(300, 5, n.fragment, 0)
nume += check_simple2(300, 6, n.in_fields, [ "in3", "in0" ])

;  301: gd_add (SINDIR)
n = {field: 'new301', field_type: !GD.SINDIR_ENTRY, fragment: 0, $
  in_fields: [ "in3", "in1"]};
gd_add, d, n
nume += check_ok2(301, 1, d)

n = gd_entry(d, "new301")
nume += check_ok2(301, 2, d)
nume += check_simple2(301, 3, n.field_type, !GD.SINDIR_ENTRY)
nume += check_simple2(301, 4, n.field, "new301")
nume += check_simple2(301, 5, n.fragment, 0)
nume += check_simple2(301, 6, n.in_fields, [ "in3", "in1" ])

;  302: gd_include_ns
gd_include, d, 'format2', namespace='ns', /CREAT, /EXCL
nume += check_ok(302, d)

;  303: gd_fragment_namespace (read)
n = gd_fragment_namespace(d, fragment=2)
nume += check_ok(303, d)
nume += check_simple(303, n, 'ns')

;  304: gd_fragment_namespace (alter)
n = gd_fragment_namespace(d, fragment=2, namespace='ns2')
nume += check_ok(304, d)
nume += check_simple(304, n, 'ns2')

;  305: gd_match_entries
n = gd_match_entries(d, fragment=0, regex="^lin")
nume += check_ok(305, d)
nume += check_simple(305, n, [ "lincom", "linterp" ])




; ===============================================================
; Cleanup
gd_close, d, /DISCARD

spawn, "rm -rf " + filedir

if (nume gt 0) then print, "nume=", nume
if (nume gt 0) then exit, /status

exit,status=0
