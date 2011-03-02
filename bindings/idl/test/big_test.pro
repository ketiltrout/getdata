; Copyright (C) 2009-2010 D. V. Wiebe
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

flen    = 7
nfields = 14
nume    = 0

spawn, "rm -rf " + filedir
file_mkdir, filedir

datadata = bindgen(80) + 1

fields = [ 'INDEX', 'bit', 'carray', 'const', 'data', 'div', 'lincom', $
  'linterp', 'mult', 'phase', 'polynom', 'recip', 'sbit', 'string' ]

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
printf,1,'linterp LINTERP data /look/up/file'
printf,1,'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const'
printf,1,'bit BIT data 3 4'
printf,1,'sbit SBIT data 5 6'
printf,1,'mult MULTIPLY data sbit'
printf,1,'div DIVIDE mult bit'
printf,1,'recip RECIP div 6.5;4.3'
printf,1,'phase PHASE data 11'
printf,1,'string STRING "Zaphod Beeblebrox"'
close,1

openw,1,form2
printf,1,'const2 CONST INT8 -19'
close,1

openw,1,data
writeu,1,byte(datadata)
close,1

;  140: getdata_constants check
defsysv, "!GD", getdata_constants()
nume += check_simple(140, !GD.E_OPEN, 1)

;  0: gd_error check
d = gd_open("x",error=error)
nume += check_error(0, d, !GD.E_OPEN)
nume += check_simple(0, error, !GD.E_OPEN)
gd_close, d, /DISCARD

;  1: gd_open check
d = gd_open(filedir, /RDWR)
nume += check_ok(1, d)
nume += check_simple(1, d, 1)

;  2: gd_getdata check
n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok(2, d)
nume += check_simple(2, n, INDGEN(8) + 41)

;  3: gd_get_constant check
n = gd_get_constant(d, "const", type=!GD.FLOAT64)
nume += check_ok(3, d)
nume += check_simple(3, n, 5.5)

;  6: gd_nfields check
n = gd_nfields(d)
nume += check_ok(6, d)
nume += check_simple(6, n, nfields)

;  8: gd_field_list check
n = gd_field_list(d)
nume += check_ok(8, d)
nume += check_simple(8, n, fields)

;  9: gd_nmfields_check
n = gd_nfields(d, parent="data")
nume += check_ok(9, d)
nume += check_simple(9, n, 3)

;  10: gd_mfield_list check
n = gd_field_list(d, parent="data")
nume += check_ok(10, d)
nume += check_simple(10, n, [ "mstr", "mconst", "mlut" ])

;  11: gd_nframes check
n = gd_nframes(d)
nume += check_ok(11, d)
nume += check_simple(11, n, 10)

;  12: gd_spf check
n = gd_spf(d, "data")
nume += check_ok(12, d)
nume += check_simple(12, n, 8)

;  13: gd_putdata check
gd_putdata, d, "data", [13, 14, 15, 16], first_frame=5, first_sample=1
nume += check_ok2(13, 1, d)

n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(13, 2, d)
nume += check_simple(13, n, [ 41, 13, 14, 15, 16, 46, 47, 48])

;  122: gd_putdata (float) check
gd_putdata, d, "data", [23., 24., 25., 26.], first_frame=5, first_sample=1
nume += check_ok2(122, 1, d)

n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(122, 2, d)
nume += check_simple(122, n, [ 41, 23, 24, 25, 26, 46, 47, 48])

;  124: gd_putdata (complex) check
gd_putdata, d, "data", [COMPLEX(33,0), COMPLEX(34,0), COMPLEX(35,0), $
  COMPLEX(36,0)], first_frame=5, first_sample=1
nume += check_ok2(124, 1, d)

n = gd_getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(124, 2, d)
nume += check_simple(124, n, [ 41, 33, 34, 35, 36, 46, 47, 48])

;  14: gd_error_string check
n = gd_getdata(d, "x", num_frames=1, estring=estring)
nume += check_error(14, d, !GD.E_BAD_CODE)
nume += check_simple2(14, 1, gd_error_string(d), "Field not found: x")
nume += check_simple2(14, 2, estring, "Field not found: x")

;  15: gd_entry_type check
n = gd_entry_type(d, "data")
nume += check_ok(15, d)
nume += check_simple(15, n, !GD.RAW_ENTRY)

;  16: gd_entry (raw)
n = gd_entry(d, "data")
nume += check_ok(16, d)
nume += check_simple2(16, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(16, 2, n.field, "data")
nume += check_simple2(16, 3, n.fragment, 0)
nume += check_simple2(16, 4, n.data_type, !GD.INT8)
nume += check_simple2(16, 5, n.spf, 8)

;  18: gd_entry (lincom)
n = gd_entry(d, "lincom")
nume += check_ok(18, d)
nume += check_simple2(18, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(18, 2, n.field, "lincom")
nume += check_simple2(18, 3, n.fragment, 0)
nume += check_simple2(18, 4, n.n_fields, 3)
nume += check_simple2(18, 5, n.in_fields, [ "data", "INDEX", "linterp" ])
nume += check_simple2(18, 6, n.comp_scal, 1)
nume += check_simple2(18, 7, n.cm, [ DCOMPLEX(1.1D,0), DCOMPLEX(2.2D,0), $
  DCOMPLEX(5.5D,0) ])
nume += check_simple2(18, 8, n.cb, [ DCOMPLEX(2.2D,0), DCOMPLEX(3.3D,4.4D), $
  DCOMPLEX(5.5D,0) ])
nume += check_simple2(18, 9, n.scalar, [ "", "", "const", "", "", "const" ])

;  20: gd_entry (polynom)
n = gd_entry(d, "polynom")
nume += check_ok(20, d)
nume += check_simple2(20, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(20, 2, n.field, "polynom")
nume += check_simple2(20, 3, n.fragment, 0)
nume += check_simple2(20, 4, n.poly_ord, 5)
nume += check_simple2(20, 5, n.in_fields, [ "data" ])
nume += check_simple2(20, 6, n.comp_scal, 1)
nume += check_simple2(20, 7, n.ca, [ DCOMPLEX(1.1D,0), DCOMPLEX(2.2D,0), $
  DCOMPLEX(2.2D,0), DCOMPLEX(3.3D,4.4D), DCOMPLEX(5.5D,0), DCOMPLEX(5.5D,0) ])
nume += check_simple2(20, 8, n.scalar, [ "", "", "", "", "const", "const" ])

;  21: gd_entry (linterp)
n = gd_entry(d, "linterp")
nume += check_ok(21, d)
nume += check_simple2(21, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(21, 2, n.field, "linterp")
nume += check_simple2(21, 3, n.fragment, 0)
nume += check_simple2(21, 4, n.in_fields, [ "data" ])
nume += check_simple2(21, 5, n.table, "/look/up/file")

;  22: gd_entry (bit)
n = gd_entry(d, "bit")
nume += check_ok(22, d)
nume += check_simple2(22, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(22, 2, n.field, "bit")
nume += check_simple2(22, 3, n.fragment, 0)
nume += check_simple2(22, 4, n.in_fields, [ "data" ])
nume += check_simple2(22, 5, n.numbits, 4)
nume += check_simple2(22, 6, n.bitnum, 3)

;  23: gd_entry (sbit)
n = gd_entry(d, "sbit")
nume += check_ok(23, d)
nume += check_simple2(23, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(23, 2, n.field, "sbit")
nume += check_simple2(23, 3, n.fragment, 0)
nume += check_simple2(23, 4, n.in_fields, [ "data" ])
nume += check_simple2(23, 5, n.numbits, 6)
nume += check_simple2(23, 6, n.bitnum, 5)

;  24: gd_entry (multiply)
n = gd_entry(d, "mult")
nume += check_ok(24, d)
nume += check_simple2(24, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(24, 2, n.field, "mult")
nume += check_simple2(24, 3, n.fragment, 0)
nume += check_simple2(24, 4, n.in_fields, [ "data", "sbit" ])

;  25: gd_entry (phase)
n = gd_entry(d, "phase")
nume += check_ok(25, d)
nume += check_simple2(25, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(25, 2, n.field, "phase")
nume += check_simple2(25, 3, n.fragment, 0)
nume += check_simple2(25, 4, n.in_fields, [ "data" ])
nume += check_simple2(25, 5, n.shift, 11)

;  26: gd_entry (const)
n = gd_entry(d, "const")
nume += check_ok(26, d)
nume += check_simple2(26, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(26, 2, n.field, "const")
nume += check_simple2(26, 3, n.fragment, 0)
nume += check_simple2(26, 4, n.data_type, !GD.FLOAT64)

;  134: gd_entry (string)
n = gd_entry(d, "string")
nume += check_ok(134, d)
nume += check_simple2(134, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(134, 2, n.field, "string")
nume += check_simple2(134, 3, n.fragment, 0)

;  27: gd_fragment_index check
n = gd_fragment_index(d, "data")
nume += check_ok(27, d)
nume += check_simple(27, n, 0)

;  28: gd_add_raw check
gd_add_raw, d, "new1", !GD.FLOAT64, spf=3
nume += check_ok2(28, 1, d)

n = gd_entry(d, "new1")
nume += check_ok2(28, 2, d)
nume += check_simple2(28, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(28, 2, n.field, "new1")
nume += check_simple2(28, 3, n.fragment, 0)
nume += check_simple2(28, 4, n.data_type, !GD.FLOAT64)
nume += check_simple2(28, 5, n.spf, 3)

;  30: gd_add_lincom
gd_add_lincom, d, "new2", "in1", COMPLEX(1.1, 1.2), COMPLEX(1.3, 1.4), $
  "in2", COMPLEX(1.4, 1.5), COMPLEX(1.6, 1.7)
nume += check_ok2(30, 1, d)

n = gd_entry(d, "new2")
nume += check_ok2(30, 2, d)
nume += check_simple2(30, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(30, 2, n.field, "new2")
nume += check_simple2(30, 3, n.fragment, 0)
nume += check_simple2(30, 4, n.n_fields, 2)
nume += check_simple2(30, 5, n.in_fields, [ "in1", "in2" ])
nume += check_simple2(30, 6, n.comp_scal, 1)
nume += check_simple2(30, 7, n.cm, [ DCOMPLEX(1.1,1.2), DCOMPLEX(1.4,1.5) ])
nume += check_simple2(30, 8, n.cb, [ DCOMPLEX(1.3,1.4), DCOMPLEX(1.6,1.7) ])

;  32: gd_add_polynom
gd_add_polynom, d, "new4", "in1", DCOMPLEX(3.1,7.0D), DCOMPLEX(4.2,8.0), $
  DCOMPLEX(5.2,9.0), DCOMPLEX(6.3,4.4)
nume += check_ok2(32, 1, d)

n = gd_entry(d, "new4")
nume += check_ok2(32, 2, d)
nume += check_simple2(32, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(32, 2, n.field, "new4")
nume += check_simple2(32, 3, n.fragment, 0)
nume += check_simple2(32, 4, n.poly_ord, 3)
nume += check_simple2(32, 5, n.in_fields, [ "in1" ])
nume += check_simple2(32, 6, n.comp_scal, 1)
nume += check_simple2(32, 7, n.ca, [ DCOMPLEX(3.1,7.0D), DCOMPLEX(4.2,8.0), $
  DCOMPLEX(5.2,9.0), DCOMPLEX(6.3,4.4) ])

;  33: gd_add_linterp
gd_add_linterp, d, "new6", "in", "./some/table"
nume += check_ok2(33, 1, d)

n = gd_entry(d, "new6")
nume += check_ok2(33, 2, d)
nume += check_simple2(33, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(33, 2, n.field, "new6")
nume += check_simple2(33, 3, n.fragment, 0)
nume += check_simple2(33, 4, n.in_fields, [ "in" ])
nume += check_simple2(33, 5, n.table, "./some/table")

;  34: gd_add_bit
gd_add_bit, d, "new7", "in1", bitnum=11, numbits=22
nume += check_ok2(34, 1, d)

n = gd_entry(d, "new7")
nume += check_ok2(34, 2, d)
nume += check_simple2(34, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(34, 2, n.field, "new7")
nume += check_simple2(34, 3, n.fragment, 0)
nume += check_simple2(34, 4, n.in_fields, [ "in1" ])
nume += check_simple2(34, 5, n.numbits, 22)
nume += check_simple2(34, 6, n.bitnum, 11)

;  35: gd_add_sbit
gd_add_sbit, d, "new8", "in2", bitnum=5, numbits=10
nume += check_ok2(35, 1, d)

n = gd_entry(d, "new8")
nume += check_ok2(35, 2, d)
nume += check_simple2(35, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(35, 2, n.field, "new8")
nume += check_simple2(35, 3, n.fragment, 0)
nume += check_simple2(35, 4, n.in_fields, [ "in2" ])
nume += check_simple2(35, 5, n.numbits, 10)
nume += check_simple2(35, 6, n.bitnum, 5)

;  36: gd_add_multiply
gd_add_multiply, d, "new9", "in2", "in3"
nume += check_ok2(36, 1, d)

n = gd_entry(d, "new9")
nume += check_ok(36, d)
nume += check_simple2(36, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(36, 2, n.field, "new9")
nume += check_simple2(36, 3, n.fragment, 0)
nume += check_simple2(36, 4, n.in_fields, [ "in2", "in3" ])

;  37: gd_add_phase
gd_add_phase, d, "new10", "in6", 42
nume += check_ok2(37, 1, d)

n = gd_entry(d, "new10")
nume += check_ok2(37, 2, d)
nume += check_simple2(37, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(37, 2, n.field, "new10")
nume += check_simple2(37, 3, n.fragment, 0)
nume += check_simple2(37, 4, n.in_fields, [ "in6" ])
nume += check_simple2(37, 5, n.shift, 42)

;  38: gd_add_const
gd_add_const, d, "new11", type=!GD.FLOAT64, value=4.3D
nume += check_ok2(38, 1, d)

n = gd_entry(d, "new11")
nume += check_ok2(38, 2, d)
nume += check_simple2(38, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(38, 2, n.field, "new11")
nume += check_simple2(38, 3, n.fragment, 0)
nume += check_simple2(38, 4, n.data_type, !GD.FLOAT64)

n = gd_get_constant(d, "new11")
nume += check_ok2(38, 3, d)
nume += check_simple2(38, 5, n, 4.3D)

;  125: gd_add
n = {field: "new13", field_type: !GD.PHASE_ENTRY, fragment: 0, $
  shift: -88L, in_fields: [ "new9" ], scalar: [ "" ], scalar_ind: [ 0 ]}
gd_add, d, n
nume += check_ok2(125, 1, d)

n = gd_entry(d, "new13")
nume += check_ok2(125, 2, d)
nume += check_simple2(125, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(125, 2, n.field, "new13")
nume += check_simple2(125, 3, n.fragment, 0)
nume += check_simple2(125, 4, n.in_fields, [ "new9" ])
nume += check_simple2(125, 5, n.shift, -88)

;  39: gd_fragmentname
n = gd_fragmentname(d, 0)
nume += check_ok(39, d)
nume += check_simple(39, n, "test_dirfile/format")

;  40: gd_nfragments
n = gd_nfragments(d)
nume += check_ok(40, d)
nume += check_simple(40, n, 1)

;  41: gd_include
gd_include, d, "form2"
nume += check_ok2(41, 1, d)

n = gd_get_constant(d, "const2", type=!GD.INT16)
nume += check_ok2(41, 2, d)
nume += check_simple(41, n, -19)

;  42: gd_nfields_by_type check
n = gd_nfields(d,type=!GD.LINCOM_ENTRY)
nume += check_ok(42, d)
nume += check_simple(42, n, 2)

;  43: gd_field_list_by_type check
n = gd_field_list(d, type=!GD.LINCOM_ENTRY)
nume += check_ok(43, d)
nume += check_simple(43, n, [ "lincom", "new2"  ])

;  44: gd_nvectors check
n = gd_nvectors(d)
nume += check_ok(44, d)
nume += check_simple(44, n, 20)

;  45: gd_vector_list check
n = gd_vector_list(d)
nume += check_ok(45, d)
nume += check_simple(45, n, [ 'INDEX', 'bit', 'data', 'div', 'lincom', $
  'linterp', 'mult', 'new1', 'new10', 'new13', 'new2', 'new4', 'new6', 'new7', $
  'new8', 'new9', 'phase', 'polynom', 'recip', 'sbit' ])

;  46: gd_madd_lincom
gd_add_lincom, d, "mnew2", "in1", 9.9D, 8.8D, "in2", 7.7D, 6.6D, $
  parent="data"
nume += check_ok2(46, 1, d)

n = gd_entry(d, "data/mnew2")
nume += check_ok2(46, 2, d)
nume += check_simple2(46, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(46, 2, n.field, "data/mnew2")
nume += check_simple2(46, 3, n.fragment, 0)
nume += check_simple2(46, 4, n.n_fields, 2)
nume += check_simple2(46, 5, n.in_fields, [ "in1", "in2" ])
nume += check_simple2(46, 6, n.comp_scal, 0)
nume += check_simple2(46, 7, n.m, [ 9.9D, 7.7D ])
nume += check_simple2(46, 8, n.b, [ 8.8D, 6.6D ])

;  48: gd_madd_polynom
gd_add_polynom, d, "mnew4", "in1", [ 3.3D, 4.4D, 5.5D, 6.6D ], $
  parent="data"
nume += check_ok2(48, 1, d)

n = gd_entry(d, "data/mnew4")
nume += check_ok2(48, 2, d)
nume += check_simple2(48, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(48, 2, n.field, "data/mnew4")
nume += check_simple2(48, 3, n.fragment, 0)
nume += check_simple2(48, 4, n.poly_ord, 3)
nume += check_simple2(48, 5, n.in_fields, [ "in1" ])
nume += check_simple2(48, 6, n.comp_scal, 0)
nume += check_simple2(48, 7, n.a, [ 3.3D, 4.4D, 5.5D, 6.6D ])

;  50: gd_madd_linterp
gd_add_linterp, d, "mnew6", "in", "./more/table", parent="data"
nume += check_ok2(50, 1, d)

n = gd_entry(d, "data/mnew6")
nume += check_ok2(50, 2, d)
nume += check_simple2(50, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(50, 2, n.field, "data/mnew6")
nume += check_simple2(50, 3, n.fragment, 0)
nume += check_simple2(50, 4, n.in_fields, [ "in" ])
nume += check_simple2(50, 5, n.table, "./more/table")

;  51: gd_madd_bit
gd_add_bit, d, "mnew7", "in1", bitnum=21, numbits=12, parent="data"
nume += check_ok2(51, 1, d)

n = gd_entry(d, "data/mnew7")
nume += check_ok2(51, 2, d)
nume += check_simple2(51, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(51, 2, n.field, "data/mnew7")
nume += check_simple2(51, 3, n.fragment, 0)
nume += check_simple2(51, 4, n.in_fields, [ "in1" ])
nume += check_simple2(51, 5, n.numbits, 12)
nume += check_simple2(51, 6, n.bitnum, 21)

;  52: gd_madd_sbit
gd_add_sbit, d, "mnew8", "in3", bitnum=2, numbits=14, parent="data"
nume += check_ok2(52, 1, d)

n = gd_entry(d, "data/mnew8")
nume += check_ok2(52, 2, d)
nume += check_simple2(52, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(52, 2, n.field, "data/mnew8")
nume += check_simple2(52, 3, n.fragment, 0)
nume += check_simple2(52, 4, n.in_fields, [ "in3" ])
nume += check_simple2(52, 5, n.numbits, 14)
nume += check_simple2(52, 6, n.bitnum, 2)

;  53: gd_madd_multiply
gd_add_multiply, d, "mnew9", "in4", "in1", parent="data"
nume += check_ok2(53, 1, d)

n = gd_entry(d, "data/mnew9")
nume += check_ok2(53, 2, d)
nume += check_simple2(53, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(53, 2, n.field, "data/mnew9")
nume += check_simple2(53, 3, n.fragment, 0)
nume += check_simple2(53, 4, n.in_fields, [ "in4", "in1" ])

;  54: gd_madd_phase
gd_add_phase, d, "mnew10", "in1", -4, parent="data"
nume += check_ok2(54, 1, d)

n = gd_entry(d, "data/mnew10")
nume += check_ok2(54, 2, d)
nume += check_simple2(54, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(54, 2, n.field, "data/mnew10")
nume += check_simple2(54, 3, n.fragment, 0)
nume += check_simple2(54, 4, n.in_fields, [ "in1" ])
nume += check_simple2(54, 5, n.shift, -4)

;  55: gd_madd_const
gd_add_const, d, "mnew11", type=!GD.UINT64, parent="data"
nume += check_ok2(55, 1, d)

n = gd_entry(d, "data/mnew11")
nume += check_ok2(55, 2, d)
nume += check_simple2(55, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(55, 2, n.field, "data/mnew11")
nume += check_simple2(55, 3, n.fragment, 0)
nume += check_simple2(55, 4, n.data_type, !GD.UINT64)

;  126: gd_madd
n = {field: "mnew13", field_type: !GD.PHASE_ENTRY, fragment: 0, $
  shift: 2L, in_fields: [ "in1" ], scalar: [ "" ], scalar_ind: [ 0 ]}
gd_add, d, n, parent="data"
nume += check_ok2(126, 1, d)

n = gd_entry(d, "data/mnew13")
nume += check_ok2(126, 2, d)
nume += check_simple2(126, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(126, 2, n.field, "data/mnew13")
nume += check_simple2(126, 3, n.fragment, 0)
nume += check_simple2(126, 4, n.in_fields, [ "in1" ])
nume += check_simple2(126, 5, n.shift, 2)

;  56: gd_get_string
n = gd_get_string(d, "string")
nume += check_ok(56, d)
nume += check_simple(56, n, "Zaphod Beeblebrox")

;  57: gd_add_string
gd_add_string, d, "new12", value="a string"
nume += check_ok2(57, 1, d)

n = gd_entry(d, "new12")
nume += check_ok2(57, 2, d)
nume += check_simple2(57, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(57, 2, n.field, "new12")
nume += check_simple2(57, 3, n.fragment, 0)

n = gd_get_string(d, "new12")
nume += check_ok2(57, 3, d)
nume += check_simple2(57, 4, n, "a string")

;  58: gd_madd_string
gd_add_string, d, "mnew12", value="another string", parent="data"
nume += check_ok2(58, 1, d)

n = gd_entry(d, "data/mnew12")
nume += check_ok2(58, 2, d)
nume += check_simple2(58, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(58, 2, n.field, "data/mnew12")
nume += check_simple2(58, 3, n.fragment, 0)

n = gd_get_string(d, "data/mnew12")
nume += check_ok2(58, 3, d)
nume += check_simple2(58, 4, n, "another string")

;  59: gd_add_spec
gd_add_spec, d, 'lorem STRING "Lorem ipsum"'
nume += check_ok2(59, 1, d)

n = gd_entry(d, "lorem")
nume += check_ok2(59, 2, d)
nume += check_simple2(59, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(59, 2, n.field, "lorem")
nume += check_simple2(59, 3, n.fragment, 0)

n = gd_get_string(d, "lorem")
nume += check_ok2(59, 3, d)
nume += check_simple2(59, 4, n, "Lorem ipsum")

;  60: gd_madd_string
gd_add_spec, d, 'ipsum STRING "dolor sit amet."', parent="lorem"
nume += check_ok2(60, 1, d)

n = gd_entry(d, "lorem/ipsum")
nume += check_ok2(60, 2, d)
nume += check_simple2(60, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(60, 2, n.field, "lorem/ipsum")
nume += check_simple2(60, 3, n.fragment, 0)

n = gd_get_string(d, "lorem/ipsum")
nume += check_ok2(60, 3, d)
nume += check_simple2(60, 4, n, "dolor sit amet.")

;  61: gd_put_constant
gd_put_constant, d, "const", 61
nume += check_ok2(61, 1, d)

n = gd_get_constant(d, "const", type=!GD.INT32)
nume += check_ok2(61, 2, d)
nume += check_simple(61, n, 61)

;  62: gd_put_string
gd_put_string, d, "string", "Arthur Dent"
nume += check_ok2(62, 1, d)

n = gd_get_string(d, "string")
nume += check_ok2(62, 2, d)
nume += check_simple(62, n, "Arthur Dent")

;  63: gd_nmfields_by_type
n = gd_nfields(d, parent="data", type=!GD.LINCOM_ENTRY)
nume += check_ok(63, d)
nume += check_simple(63, n, 1)

;  64: gd_mfield_list_by_type
n = gd_field_list(d, parent="data", type=!GD.LINCOM_ENTRY)
nume += check_ok(64, d)
nume += check_simple(64, n, [ "mnew2" ])

;  65: gd_nmvectors
n = gd_nvectors(d, parent="data")
nume += check_ok(65, d)
nume += check_simple(65, n, 9)

;  66: gd_mvector_list check
n = gd_vector_list(d, parent="data")
nume += check_ok(66, d)
nume += check_simple(66, n, [ 'mlut', 'mnew2', 'mnew4', 'mnew6', $
  'mnew7', 'mnew8', 'mnew9', 'mnew10', 'mnew13' ])

;  67: gd_alter_raw check
gd_alter_raw, d, "new1", type=!GD.INT32
nume += check_ok2(67, 1, d)

n = gd_entry(d, "new1")
nume += check_ok2(67, 2, d)
nume += check_simple2(67, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(67, 2, n.field, "new1")
nume += check_simple2(67, 3, n.fragment, 0)
nume += check_simple2(67, 4, n.data_type, !GD.INT32)
nume += check_simple2(67, 5, n.spf, 3)

;  69: gd_alter_lincom
gd_alter_lincom, d, "new2", in_fields=[ "in3", "in4" ], $
  m=[ COMPLEX(2.3, 4.5), COMPLEX(6.7, 8.9) ]
nume += check_ok2(69, 1, d)

n = gd_entry(d, "new2")
nume += check_ok2(69, 2, d)
nume += check_simple2(69, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(69, 2, n.field, "new2")
nume += check_simple2(69, 3, n.fragment, 0)
nume += check_simple2(69, 4, n.n_fields, 2)
nume += check_simple2(69, 5, n.in_fields, [ "in3", "in4" ])
nume += check_simple2(69, 6, n.comp_scal, 1)
nume += check_simple2(69, 7, n.cm, [ DCOMPLEX(2.3,4.5), DCOMPLEX(6.7,8.9) ])
nume += check_simple2(69, 8, n.cb, [ DCOMPLEX(1.3,1.4), DCOMPLEX(1.6,1.7) ])

;  71: gd_alter_polynom
gd_alter_polynom, d, "new4", poly_ord=4, a=[ DCOMPLEX(1.2,3.4), $
  DCOMPLEX(5.6,7.8), DCOMPLEX(9.0,1.2), DCOMPLEX(3.4,5.6), DCOMPLEX(7.8,9.0) ]
nume += check_ok2(71, 1, d)

n = gd_entry(d, "new4")
nume += check_ok2(71, 2, d)
nume += check_simple2(71, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(71, 2, n.field, "new4")
nume += check_simple2(71, 3, n.fragment, 0)
nume += check_simple2(71, 4, n.poly_ord, 4)
nume += check_simple2(71, 5, n.in_fields, [ "in1" ])
nume += check_simple2(71, 6, n.comp_scal, 1)
nume += check_simple2(71, 7, n.ca, [ DCOMPLEX(1.2,3.4), DCOMPLEX(5.6,7.8), $
  DCOMPLEX(9.0,1.2), DCOMPLEX(3.4,5.6), DCOMPLEX(7.8,9.0) ])

;  72: gd_alter_linterp
gd_alter_linterp, d, "new6", table="./other/table"
nume += check_ok2(72, 1, d)

n = gd_entry(d, "new6")
nume += check_ok2(72, 2, d)
nume += check_simple2(72, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(72, 2, n.field, "new6")
nume += check_simple2(72, 3, n.fragment, 0)
nume += check_simple2(72, 4, n.in_fields, [ "in" ])
nume += check_simple2(72, 5, n.table, "./other/table")

;  73: gd_alter_bit
gd_alter_bit, d, "new7", in_field="in3",  numbits=8
nume += check_ok2(73, 1, d)

n = gd_entry(d, "new7")
nume += check_ok2(73, 2, d)
nume += check_simple2(73, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(73, 2, n.field, "new7")
nume += check_simple2(73, 3, n.fragment, 0)
nume += check_simple2(73, 4, n.in_fields, [ "in3" ])
nume += check_simple2(73, 5, n.numbits, 8)
nume += check_simple2(73, 6, n.bitnum, 11)

;  74: gd_alter_sbit
gd_alter_sbit, d, "new8", bitnum=15, numbits=1
nume += check_ok2(74, 1, d)

n = gd_entry(d, "new8")
nume += check_ok2(74, 2, d)
nume += check_simple2(74, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(74, 2, n.field, "new8")
nume += check_simple2(74, 3, n.fragment, 0)
nume += check_simple2(74, 4, n.in_fields, [ "in2" ])
nume += check_simple2(74, 5, n.numbits, 1)
nume += check_simple2(74, 6, n.bitnum, 15)

;  75: gd_alter_multiply
gd_alter_multiply, d, "new9", in_field1="in6"
nume += check_ok2(75, 1, d)

n = gd_entry(d, "new9")
nume += check_ok(75, d)
nume += check_simple2(75, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(75, 2, n.field, "new9")
nume += check_simple2(75, 3, n.fragment, 0)
nume += check_simple2(75, 4, n.in_fields, [ "in6", "in3" ])

;  76: gd_alter_phase
gd_alter_phase, d, "new10", shift=76
nume += check_ok2(76, 1, d)

n = gd_entry(d, "new10")
nume += check_ok2(76, 2, d)
nume += check_simple2(76, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(76, 2, n.field, "new10")
nume += check_simple2(76, 3, n.fragment, 0)
nume += check_simple2(76, 4, n.in_fields, [ "in6" ])
nume += check_simple2(76, 5, n.shift, 76)

;  77: gd_alter_const
gd_alter_const, d, "new11", type=!GD.FLOAT32
nume += check_ok2(77, 1, d)

n = gd_entry(d, "new11")
nume += check_ok2(77, 2, d)
nume += check_simple2(77, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(77, 2, n.field, "new11")
nume += check_simple2(77, 3, n.fragment, 0)
nume += check_simple2(77, 4, n.data_type, !GD.FLOAT32)

;  141: gd_alter
n = {field_type: !GD.PHASE_ENTRY, shift: -8L}
gd_alter_entry, d, "new13", n
nume += check_ok2(141, 1, d)

n = gd_entry(d, "new13")
nume += check_ok2(141, 2, d)
nume += check_simple2(141, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(141, 2, n.field, "new13")
nume += check_simple2(141, 3, n.fragment, 0)
nume += check_simple2(141, 4, n.in_fields, [ "new9" ])
nume += check_simple2(141, 5, n.shift, -8)

;  78: gd_encoding
n = gd_encoding(d, fragment=0)
nume += check_ok(78, d)
nume += check_simple(78, n, !GD.UNENCODED)

;  79: gd_endianness
n = gd_endianness(d, fragment=0)
nume += check_ok(79, d)
nume += check_simple(79, n, (!GD.LITTLE_ENDIAN + !GD.NOT_ARM_ENDIAN))

;  80: dirfilename
n = gd_dirfilename(d)
nume += check_ok(80, d)
nume += check_simple(80, n, "test_dirfile")

;  81: gd_parent_fragment
n = gd_parent_fragment(d, fragment=1)
nume += check_ok(81, d)
nume += check_simple(80, n, 0)

;  82: gd_alter_protection
gd_alter_protection, d, !GD.PROTECT_DATA, fragment=1
nume += check_ok(82, d)

;  83: gd_protection
n = gd_protection(d, fragment=1)
nume += check_ok(83, d)
nume += check_simple(83, n, !GD.PROTECT_DATA)

;  84: gd_raw_filename
n = gd_raw_filename(d, 'data')
nume += check_ok(84, d)
nume += check_simple(84, n, "test_dirfile/data")

;  85: gd_reference
gd_reference, d, "new1"
nume += check_ok(85, d)

;  87: gd_alter_encoding
gd_alter_encoding, d, !GD.SLIM_ENCODED, fragment=1
nume += check_ok2(87, 1, d)

n = gd_encoding(d, fragment=1)
nume += check_ok2(87, 2, d)
nume += check_simple(87, n, !GD.SLIM_ENCODED)

;  88: gd_alter_endianness
gd_alter_endianness, d, /big_endian, fragment=1
nume += check_ok2(88, 1, d)

n = gd_endianness(d, fragment=1)
nume += check_ok2(88, 2, d)
nume += check_simple(88, n, !GD.BIG_ENDIAN)

;  89: gd_alter_spec
gd_alter_spec, d, "new10 PHASE in 3"
nume += check_ok2(89, 1, d)

n = gd_entry(d, "new10")
nume += check_ok2(89, 2, d)
nume += check_simple2(89, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(89, 2, n.field, "new10")
nume += check_simple2(89, 3, n.fragment, 0)
nume += check_simple2(89, 4, n.in_fields, [ "in" ])
nume += check_simple2(89, 5, n.shift, 3)

;  90: gd_delete
gd_delete, d, "new10"
nume += check_ok2(90, 1, d)

n = gd_entry(d, "new10")
nume += check_error2(90, 2, d, !GD.E_BAD_CODE)

;  91: gd_malter_spec
gd_alter_spec, d, "mnew10 PHASE in4 11", parent="data"
nume += check_ok2(91, 1, d)

n = gd_entry(d, "data/mnew10")
nume += check_ok2(91, 2, d)
nume += check_simple2(91, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(91, 2, n.field, "data/mnew10")
nume += check_simple2(91, 3, n.fragment, 0)
nume += check_simple2(91, 4, n.in_fields, [ "in4" ])
nume += check_simple2(91, 5, n.shift, 11)

;  92: gd_move
gd_move, d, "new9", 1
nume += check_ok2(92, 1, d)

n = gd_entry(d, "new9")
nume += check_ok2(92, 2, d)
nume += check_simple2(92, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(92, 2, n.field, "new9")
nume += check_simple2(92, 3, n.fragment, 1)

;  93: gd_rename
gd_rename, d, "new9", "newer"
nume += check_ok2(93, 1, d)

n = gd_entry(d, "new9")
nume += check_error2(93, 2, d, !GD.E_BAD_CODE)

n = gd_entry(d, "newer")
nume += check_ok2(93, 3, d)
nume += check_simple2(93, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(93, 2, n.field, "newer")
nume += check_simple2(93, 3, n.fragment, 1)

;  94: gd_uninclude
gd_uninclude, d, 1
nume += check_ok2(94, 1, d)

n = gd_entry(d, "newer")
nume += check_error2(94, 2, d, !GD.E_BAD_CODE)

;  95: gd_frameoffset
n = gd_frameoffset(d,fragment=0)
nume += check_ok(95,d)
nume += check_simple(95,n,0)

;  96: gd_alter_frameoffset
gd_alter_frameoffset, d, 33, fragment=0
nume += check_ok2(96,1,d)

n = gd_frameoffset(d,fragment=0)
nume += check_ok(96,d)
nume += check_simple(96,n,33)

;  97: gd_native_type
n = gd_native_type(d, "data")
nume += check_ok(97, d)
nume += check_simple(97,n,!GD.INT8)

;  99: gd_validate
n =  gd_validate(d, "new7")
nume += check_error(99,d,!GD.E_BAD_CODE)
nume += check_simple(99,n,-1)

;  101: gd_framenum
n = gd_framenum(d, "data", 33.3, field_start=6)
nume += check_ok(101,d)
nume += check_float(101, n, 37.037500D)

;  86: gd_eof
n = gd_eof(d, "lincom")
nume += check_ok(86,d)
nume += check_simple(86,n,344)

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
nume += check_simple2(145, 5, n.comp_scal, 1)
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
nume += check_simple2(148, 5, n.comp_scal, 1)
nume += check_simple2(148, 6, n.cdividend, DCOMPLEX(33.3, 44.4))

;  152: gd_alter_multiply
gd_alter_divide, d, "new14", in_field1="in6"
nume += check_ok2(152, 1, d)

n = gd_entry(d, "new14")
nume += check_ok(152, d)
nume += check_simple2(152, 1, n.field_type, !GD.DIVIDE_ENTRY)
nume += check_simple2(152, 2, n.field, "new14")
nume += check_simple2(152, 3, n.fragment, 0)
nume += check_simple2(152, 4, n.in_fields, [ "in6", "in3" ])

;  153: gd_alter_multiply
gd_alter_recip, d, "new16", dividend=1.01
nume += check_ok2(153, 1, d)

n = gd_entry(d, "new16")
nume += check_ok(153, d)
nume += check_simple2(153, 1, n.field_type, !GD.RECIP_ENTRY)
nume += check_simple2(153, 2, n.field, "new16")
nume += check_simple2(153, 3, n.fragment, 0)
nume += check_simple2(153, 5, n.comp_scal, 0)
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
nume += check_simple(157, n, 8)
n = gd_dirfile_standards(d, 0)
nume += check_error2(157, 2, d, !GD.E_BAD_VERSION)

;  158: gd_get_carray
n = gd_get_carray(d, "carray", type=!GD.FLOAT32)
nume += check_ok(158, d)
nume += check_simple(158, n, [ 1.1, 2.2, 3.3, 4.4, 5.5, 6.6 ])

;  159: gd_get_carray_slice
n = gd_get_carray(d, "carray", type=!GD.FLOAT32, len=2, start=2)
nume += check_ok(159, d)
nume += check_simple(159, n, [ 3.3, 4.4 ])

;  167: gd_carrays
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

;  177: gd_carray_len
n = gd_carray_len(d, "carray")
nume += check_ok(170, d)
nume += check_simple(170, n, 6)

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




; ===============================================================
; Cleanup
gd_close, d, /DISCARD

spawn, "rm -rf " + filedir

if (nume gt 0) then print, "nume=", nume
if (nume gt 0) then exit, /status

exit,status=0
