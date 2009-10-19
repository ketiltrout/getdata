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
nfields = 11
nume    = 0

spawn, "rm -rf " + filedir
file_mkdir, filedir

datadata = bindgen(80) + 1

fields = [ 'INDEX', 'bit', 'const', 'data', 'lincom', 'linterp', 'mult', $
  'phase', 'polynom', 'sbit', 'string' ]

; Write the test dirfile
openw,1,format
printf,1,'/ENDIAN little'
printf,1,'data RAW INT8 8'
printf,1,'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const'
printf,1,'/META data mstr STRING "This is a string constant."'
printf,1,'/META data mconst CONST COMPLEX128 3.3;4.4'
printf,1,'/META data mlut LINTERP DATA ./lut'
printf,1,'const CONST FLOAT64 5.5'
printf,1,'linterp LINTERP data /look/up/file'
printf,1,'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const'
printf,1,'bit BIT data 3 4'
printf,1,'sbit SBIT data 5 6'
printf,1,'mult MULTIPLY data sbit'
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

;  0: get_error check
d = dirfile_open("x",error=error)
nume += check_error(0, d, !GD.E_OPEN)
nume += check_simple(0, error, !GD.E_OPEN)
dirfile_close, d, /DISCARD

;  1: dirfile_open check
d = dirfile_open(filedir, /RDWR)
nume += check_ok(1, d)
nume += check_simple(1, d, 1)

;  2: getdata check
n = getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok(2, d)
nume += check_simple(2, n, INDGEN(8) + 41)

;  3: get_constant check
n = get_constant(d, "const", type=!GD.FLOAT64)
nume += check_ok(3, d)
nume += check_simple(3, n, 5.5)

;  6: get_nfields check
n = get_nfields(d)
nume += check_ok(6, d)
nume += check_simple(6, n, nfields)

;  8: get_field_list check
n = get_field_list(d)
nume += check_ok(8, d)
nume += check_simple(8, n, fields)

;  9: get_nmfields_check
n = get_nfields(d, parent="data")
nume += check_ok(9, d)
nume += check_simple(9, n, 3)

;  10: get_mfield_list check
n = get_field_list(d, parent="data")
nume += check_ok(10, d)
nume += check_simple(10, n, [ "mstr", "mconst", "mlut" ])

;  11: get_nframes check
n = get_nframes(d)
nume += check_ok(11, d)
nume += check_simple(11, n, 10)

;  12: get_spf check
n = get_spf(d, "data")
nume += check_ok(12, d)
nume += check_simple(12, n, 8)

;  13: putdata check
putdata, d, "data", [13, 14, 15, 16], first_frame=5, first_sample=1
nume += check_ok2(13, 1, d)

n = getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(13, 2, d)
nume += check_simple(13, n, [ 41, 13, 14, 15, 16, 46, 47, 48])

;  122: putdata (float) check
putdata, d, "data", [23., 24., 25., 26.], first_frame=5, first_sample=1
nume += check_ok2(122, 1, d)

n = getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(122, 2, d)
nume += check_simple(122, n, [ 41, 23, 24, 25, 26, 46, 47, 48])

;  124: putdata (complex) check
putdata, d, "data", [COMPLEX(33,0), COMPLEX(34,0), COMPLEX(35,0), $
  COMPLEX(36,0)], first_frame=5, first_sample=1
nume += check_ok2(124, 1, d)

n = getdata(d, "data", type=!GD.INT16, first_frame=5, num_frames=1)
nume += check_ok2(124, 2, d)
nume += check_simple(124, n, [ 41, 33, 34, 35, 36, 46, 47, 48])

;  14: get_error_string check
n = getdata(d, "x", num_frames=1, estring=estring)
nume += check_error(14, d, !GD.E_BAD_CODE)
nume += check_simple2(14, 1, get_error_string(d), "Field not found: x")
nume += check_simple2(14, 2, estring, "Field not found: x")

;  15: get_entry_type check
n = get_entry_type(d, "data")
nume += check_ok(15, d)
nume += check_simple(15, n, !GD.RAW_ENTRY)

;  16: get_entry (raw)
n = get_entry(d, "data")
nume += check_ok(16, d)
nume += check_simple2(16, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(16, 2, n.field, "data")
nume += check_simple2(16, 3, n.fragment, 0)
nume += check_simple2(16, 4, n.data_type, !GD.INT8)
nume += check_simple2(16, 5, n.spf, 8)

;  18: get_entry (lincom)
n = get_entry(d, "lincom")
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

;  20: get_entry (polynom)
n = get_entry(d, "polynom")
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

;  21: get_entry (linterp)
n = get_entry(d, "linterp")
nume += check_ok(21, d)
nume += check_simple2(21, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(21, 2, n.field, "linterp")
nume += check_simple2(21, 3, n.fragment, 0)
nume += check_simple2(21, 4, n.in_fields, [ "data" ])
nume += check_simple2(21, 5, n.table, "/look/up/file")

;  22: get_entry (bit)
n = get_entry(d, "bit")
nume += check_ok(22, d)
nume += check_simple2(22, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(22, 2, n.field, "bit")
nume += check_simple2(22, 3, n.fragment, 0)
nume += check_simple2(22, 4, n.in_fields, [ "data" ])
nume += check_simple2(22, 5, n.numbits, 4)
nume += check_simple2(22, 6, n.bitnum, 3)

;  23: get_entry (sbit)
n = get_entry(d, "sbit")
nume += check_ok(23, d)
nume += check_simple2(23, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(23, 2, n.field, "sbit")
nume += check_simple2(23, 3, n.fragment, 0)
nume += check_simple2(23, 4, n.in_fields, [ "data" ])
nume += check_simple2(23, 5, n.numbits, 6)
nume += check_simple2(23, 6, n.bitnum, 5)

;  24: get_entry (multiply)
n = get_entry(d, "mult")
nume += check_ok(24, d)
nume += check_simple2(24, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(24, 2, n.field, "mult")
nume += check_simple2(24, 3, n.fragment, 0)
nume += check_simple2(24, 4, n.in_fields, [ "data", "sbit" ])

;  25: get_entry (phase)
n = get_entry(d, "phase")
nume += check_ok(25, d)
nume += check_simple2(25, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(25, 2, n.field, "phase")
nume += check_simple2(25, 3, n.fragment, 0)
nume += check_simple2(25, 4, n.in_fields, [ "data" ])
nume += check_simple2(25, 5, n.shift, 11)

;  26: get_entry (const)
n = get_entry(d, "const")
nume += check_ok(26, d)
nume += check_simple2(26, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(26, 2, n.field, "const")
nume += check_simple2(26, 3, n.fragment, 0)
nume += check_simple2(26, 4, n.data_type, !GD.FLOAT64)

;  134: get_entry (string)
n = get_entry(d, "string")
nume += check_ok(134, d)
nume += check_simple2(134, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(134, 2, n.field, "string")
nume += check_simple2(134, 3, n.fragment, 0)

;  27: get_fragment_index check
n = get_fragment_index(d, "data")
nume += check_ok(27, d)
nume += check_simple(27, n, 0)

;  28: getdata_add_raw check
dirfile_add_raw, d, "new1", !GD.FLOAT64, spf=3
nume += check_ok2(28, 1, d)

n = get_entry(d, "new1")
nume += check_ok2(28, 2, d)
nume += check_simple2(28, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(28, 2, n.field, "new1")
nume += check_simple2(28, 3, n.fragment, 0)
nume += check_simple2(28, 4, n.data_type, !GD.FLOAT64)
nume += check_simple2(28, 5, n.spf, 3)

;  30: getdata_add_lincom
dirfile_add_lincom, d, "new2", "in1", COMPLEX(1.1, 1.2), COMPLEX(1.3, 1.4), $
  "in2", COMPLEX(1.4, 1.5), COMPLEX(1.6, 1.7)
nume += check_ok2(30, 1, d)

n = get_entry(d, "new2")
nume += check_ok2(30, 2, d)
nume += check_simple2(30, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(30, 2, n.field, "new2")
nume += check_simple2(30, 3, n.fragment, 0)
nume += check_simple2(30, 4, n.n_fields, 2)
nume += check_simple2(30, 5, n.in_fields, [ "in1", "in2" ])
nume += check_simple2(30, 6, n.comp_scal, 1)
nume += check_simple2(30, 7, n.cm, [ DCOMPLEX(1.1,1.2), DCOMPLEX(1.4,1.5) ])
nume += check_simple2(30, 8, n.cb, [ DCOMPLEX(1.3,1.4), DCOMPLEX(1.6,1.7) ])

;  32: dirfile_add_polynom
dirfile_add_polynom, d, "new4", "in1", DCOMPLEX(3.1,7.0D), DCOMPLEX(4.2,8.0), $
  DCOMPLEX(5.2,9.0), DCOMPLEX(6.3,4.4)
nume += check_ok2(32, 1, d)

n = get_entry(d, "new4")
nume += check_ok2(32, 2, d)
nume += check_simple2(32, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(32, 2, n.field, "new4")
nume += check_simple2(32, 3, n.fragment, 0)
nume += check_simple2(32, 4, n.poly_ord, 3)
nume += check_simple2(32, 5, n.in_fields, [ "in1" ])
nume += check_simple2(32, 6, n.comp_scal, 1)
nume += check_simple2(32, 7, n.ca, [ DCOMPLEX(3.1,7.0D), DCOMPLEX(4.2,8.0), $
  DCOMPLEX(5.2,9.0), DCOMPLEX(6.3,4.4) ])

;  33: dirfile_add_linterp
dirfile_add_linterp, d, "new6", "in", "./some/table"
nume += check_ok2(33, 1, d)

n = get_entry(d, "new6")
nume += check_ok2(33, 2, d)
nume += check_simple2(33, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(33, 2, n.field, "new6")
nume += check_simple2(33, 3, n.fragment, 0)
nume += check_simple2(33, 4, n.in_fields, [ "in" ])
nume += check_simple2(33, 5, n.table, "./some/table")

;  34: dirfile_add_bit
dirfile_add_bit, d, "new7", "in1", bitnum=11, numbits=22
nume += check_ok2(34, 1, d)

n = get_entry(d, "new7")
nume += check_ok2(34, 2, d)
nume += check_simple2(34, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(34, 2, n.field, "new7")
nume += check_simple2(34, 3, n.fragment, 0)
nume += check_simple2(34, 4, n.in_fields, [ "in1" ])
nume += check_simple2(34, 5, n.numbits, 22)
nume += check_simple2(34, 6, n.bitnum, 11)

;  35: dirfile_add_sbit
dirfile_add_sbit, d, "new8", "in2", bitnum=5, numbits=10
nume += check_ok2(35, 1, d)

n = get_entry(d, "new8")
nume += check_ok2(35, 2, d)
nume += check_simple2(35, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(35, 2, n.field, "new8")
nume += check_simple2(35, 3, n.fragment, 0)
nume += check_simple2(35, 4, n.in_fields, [ "in2" ])
nume += check_simple2(35, 5, n.numbits, 10)
nume += check_simple2(35, 6, n.bitnum, 5)

;  36: dirfile_add_multiply
dirfile_add_multiply, d, "new9", "in2", "in3"
nume += check_ok2(36, 1, d)

n = get_entry(d, "new9")
nume += check_ok(36, d)
nume += check_simple2(36, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(36, 2, n.field, "new9")
nume += check_simple2(36, 3, n.fragment, 0)
nume += check_simple2(36, 4, n.in_fields, [ "in2", "in3" ])

;  37: dirfile_add_phase
dirfile_add_phase, d, "new10", "in6", 42
nume += check_ok2(37, 1, d)

n = get_entry(d, "new10")
nume += check_ok2(37, 2, d)
nume += check_simple2(37, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(37, 2, n.field, "new10")
nume += check_simple2(37, 3, n.fragment, 0)
nume += check_simple2(37, 4, n.in_fields, [ "in6" ])
nume += check_simple2(37, 5, n.shift, 42)

;  38: dirfile_add_const
dirfile_add_const, d, "new11", type=!GD.FLOAT64, value=4.3D
nume += check_ok2(38, 1, d)

n = get_entry(d, "new11")
nume += check_ok2(38, 2, d)
nume += check_simple2(38, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(38, 2, n.field, "new11")
nume += check_simple2(38, 3, n.fragment, 0)
nume += check_simple2(38, 4, n.data_type, !GD.FLOAT64)

n = get_constant(d, "new11")
nume += check_ok2(38, 3, d)
nume += check_simple2(38, 5, n, 4.3D)

;  125: dirfile_add
n = {field: "new13", field_type: !GD.PHASE_ENTRY, fragment: 0, $
  shift: -88L, in_fields: [ "new9" ], scalar: [ "" ]}
dirfile_add, d, n
nume += check_ok2(125, 1, d)

n = get_entry(d, "new13")
nume += check_ok2(125, 2, d)
nume += check_simple2(125, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(125, 2, n.field, "new13")
nume += check_simple2(125, 3, n.fragment, 0)
nume += check_simple2(125, 4, n.in_fields, [ "new9" ])
nume += check_simple2(125, 5, n.shift, -88)

;  39: get_fragmentname
n = get_fragmentname(d, 0)
nume += check_ok(39, d)
nume += check_simple(39, n, "test_dirfile/format")

;  40: get_nfragments
n = get_nfragments(d)
nume += check_ok(40, d)
nume += check_simple(40, n, 1)

;  41: dirfile_include
dirfile_include, d, "form2"
nume += check_ok2(41, 1, d)

n = get_constant(d, "const2", type=!GD.INT16)
nume += check_ok2(41, 2, d)
nume += check_simple(41, n, -19)

;  42: get_nfields_by_type check
n = get_nfields(d,type=!GD.LINCOM_ENTRY)
nume += check_ok(42, d)
nume += check_simple(42, n, 2)

;  43: get_field_list_by_type check
n = get_field_list(d, type=!GD.LINCOM_ENTRY)
nume += check_ok(43, d)
nume += check_simple(43, n, [ "lincom", "new2"  ])

;  44: get_nvectors check
n = get_nvectors(d)
nume += check_ok(44, d)
nume += check_simple(44, n, 18)

;  45: get_vector_list check
n = get_vector_list(d)
nume += check_ok(45, d)
nume += check_simple(45, n, [ 'INDEX', 'bit', 'data', 'lincom', 'linterp', $
  'mult', 'new1', 'new10', 'new13', 'new2', 'new4', 'new6', 'new7', 'new8', $
  'new9', 'phase', 'polynom', 'sbit', 'string' ])

;  46: getdata_madd_lincom
dirfile_add_lincom, d, "mnew2", "in1", 9.9D, 8.8D, "in2", 7.7D, 6.6D, $
  parent="data"
nume += check_ok2(46, 1, d)

n = get_entry(d, "data/mnew2")
nume += check_ok2(46, 2, d)
nume += check_simple2(46, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(46, 2, n.field, "data/mnew2")
nume += check_simple2(46, 3, n.fragment, 0)
nume += check_simple2(46, 4, n.n_fields, 2)
nume += check_simple2(46, 5, n.in_fields, [ "in1", "in2" ])
nume += check_simple2(46, 6, n.comp_scal, 0)
nume += check_simple2(46, 7, n.m, [ 9.9D, 7.7D ])
nume += check_simple2(46, 8, n.b, [ 8.8D, 6.6D ])

;  48: dirfile_madd_polynom
dirfile_add_polynom, d, "mnew4", "in1", [ 3.3D, 4.4D, 5.5D, 6.6D ], $
  parent="data"
nume += check_ok2(48, 1, d)

n = get_entry(d, "data/mnew4")
nume += check_ok2(48, 2, d)
nume += check_simple2(48, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(48, 2, n.field, "data/mnew4")
nume += check_simple2(48, 3, n.fragment, 0)
nume += check_simple2(48, 4, n.poly_ord, 3)
nume += check_simple2(48, 5, n.in_fields, [ "in1" ])
nume += check_simple2(48, 6, n.comp_scal, 0)
nume += check_simple2(48, 7, n.a, [ 3.3D, 4.4D, 5.5D, 6.6D ])

;  50: dirfile_madd_linterp
dirfile_add_linterp, d, "mnew6", "in", "./more/table", parent="data"
nume += check_ok2(50, 1, d)

n = get_entry(d, "data/mnew6")
nume += check_ok2(50, 2, d)
nume += check_simple2(50, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(50, 2, n.field, "data/mnew6")
nume += check_simple2(50, 3, n.fragment, 0)
nume += check_simple2(50, 4, n.in_fields, [ "in" ])
nume += check_simple2(50, 5, n.table, "./more/table")

;  51: dirfile_madd_bit
dirfile_add_bit, d, "mnew7", "in1", bitnum=21, numbits=12, parent="data"
nume += check_ok2(51, 1, d)

n = get_entry(d, "data/mnew7")
nume += check_ok2(51, 2, d)
nume += check_simple2(51, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(51, 2, n.field, "data/mnew7")
nume += check_simple2(51, 3, n.fragment, 0)
nume += check_simple2(51, 4, n.in_fields, [ "in1" ])
nume += check_simple2(51, 5, n.numbits, 12)
nume += check_simple2(51, 6, n.bitnum, 21)

;  52: dirfile_madd_sbit
dirfile_add_sbit, d, "mnew8", "in3", bitnum=2, numbits=14, parent="data"
nume += check_ok2(52, 1, d)

n = get_entry(d, "data/mnew8")
nume += check_ok2(52, 2, d)
nume += check_simple2(52, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(52, 2, n.field, "data/mnew8")
nume += check_simple2(52, 3, n.fragment, 0)
nume += check_simple2(52, 4, n.in_fields, [ "in3" ])
nume += check_simple2(52, 5, n.numbits, 14)
nume += check_simple2(52, 6, n.bitnum, 2)

;  53: dirfile_madd_multiply
dirfile_add_multiply, d, "mnew9", "in4", "in1", parent="data"
nume += check_ok2(53, 1, d)

n = get_entry(d, "data/mnew9")
nume += check_ok2(53, 2, d)
nume += check_simple2(53, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(53, 2, n.field, "data/mnew9")
nume += check_simple2(53, 3, n.fragment, 0)
nume += check_simple2(53, 4, n.in_fields, [ "in4", "in1" ])

;  54: dirfile_madd_phase
dirfile_add_phase, d, "mnew10", "in1", -4, parent="data"
nume += check_ok2(54, 1, d)

n = get_entry(d, "data/mnew10")
nume += check_ok2(54, 2, d)
nume += check_simple2(54, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(54, 2, n.field, "data/mnew10")
nume += check_simple2(54, 3, n.fragment, 0)
nume += check_simple2(54, 4, n.in_fields, [ "in1" ])
nume += check_simple2(54, 5, n.shift, -4)

;  55: dirfile_madd_const
dirfile_add_const, d, "mnew11", type=!GD.UINT64, parent="data"
nume += check_ok2(55, 1, d)

n = get_entry(d, "data/mnew11")
nume += check_ok2(55, 2, d)
nume += check_simple2(55, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(55, 2, n.field, "data/mnew11")
nume += check_simple2(55, 3, n.fragment, 0)
nume += check_simple2(55, 4, n.data_type, !GD.UINT64)

;  126: dirfile_madd
n = {field: "mnew13", field_type: !GD.PHASE_ENTRY, fragment: 0, $
  shift: 2L, in_fields: [ "in1" ], scalar: [ "" ]}
dirfile_add, d, n, parent="data"
nume += check_ok2(126, 1, d)

n = get_entry(d, "data/mnew13")
nume += check_ok2(126, 2, d)
nume += check_simple2(126, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(126, 2, n.field, "data/mnew13")
nume += check_simple2(126, 3, n.fragment, 0)
nume += check_simple2(126, 4, n.in_fields, [ "in1" ])
nume += check_simple2(126, 5, n.shift, 2)

;  56: get_string
n = get_string(d, "string")
nume += check_ok(56, d)
nume += check_simple(56, n, "Zaphod Beeblebrox")

;  57: dirfile_add_string
dirfile_add_string, d, "new12", value="a string"
nume += check_ok2(57, 1, d)

n = get_entry(d, "new12")
nume += check_ok2(57, 2, d)
nume += check_simple2(57, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(57, 2, n.field, "new12")
nume += check_simple2(57, 3, n.fragment, 0)

n = get_string(d, "new12")
nume += check_ok2(57, 3, d)
nume += check_simple2(57, 4, n, "a string")

;  58: dirfile_madd_string
dirfile_add_string, d, "mnew12", value="another string", parent="data"
nume += check_ok2(58, 1, d)

n = get_entry(d, "data/mnew12")
nume += check_ok2(58, 2, d)
nume += check_simple2(58, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(58, 2, n.field, "data/mnew12")
nume += check_simple2(58, 3, n.fragment, 0)

n = get_string(d, "data/mnew12")
nume += check_ok2(58, 3, d)
nume += check_simple2(58, 4, n, "another string")

;  59: dirfile_add_spec
dirfile_add_spec, d, 'lorem STRING "Lorem ipsum"'
nume += check_ok2(59, 1, d)

n = get_entry(d, "lorem")
nume += check_ok2(59, 2, d)
nume += check_simple2(59, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(59, 2, n.field, "lorem")
nume += check_simple2(59, 3, n.fragment, 0)

n = get_string(d, "lorem")
nume += check_ok2(59, 3, d)
nume += check_simple2(59, 4, n, "Lorem ipsum")

;  60: dirfile_madd_string
dirfile_add_spec, d, 'ipsum STRING "dolor sit amet."', parent="lorem"
nume += check_ok2(60, 1, d)

n = get_entry(d, "lorem/ipsum")
nume += check_ok2(60, 2, d)
nume += check_simple2(60, 1, n.field_type, !GD.STRING_ENTRY)
nume += check_simple2(60, 2, n.field, "lorem/ipsum")
nume += check_simple2(60, 3, n.fragment, 0)

n = get_string(d, "lorem/ipsum")
nume += check_ok2(60, 3, d)
nume += check_simple2(60, 4, n, "dolor sit amet.")

;  61: put_constant
put_constant, d, "const", 61
nume += check_ok2(61, 1, d)

n = get_constant(d, "const", type=!GD.INT32)
nume += check_ok2(61, 2, d)
nume += check_simple(61, n, 61)

;  62: put_string
put_string, d, "string", "Arthur Dent"
nume += check_ok2(62, 1, d)

n = get_string(d, "string")
nume += check_ok2(62, 2, d)
nume += check_simple(62, n, "Arthur Dent")

;  63: get_nmfields_by_type
n = get_nfields(d, parent="data", type=!GD.LINCOM_ENTRY)
nume += check_ok(63, d)
nume += check_simple(63, n, 1)

;  64: get_mfield_list_by_type
n = get_field_list(d, parent="data", type=!GD.LINCOM_ENTRY)
nume += check_ok(64, d)
nume += check_simple(64, n, [ "mnew2" ])

;  65: get_nmvectors
n = get_nvectors(d, parent="data")
nume += check_ok(65, d)
nume += check_simple(65, n, 9)

;  66: get_mvector_list check
n = get_vector_list(d, parent="data")
nume += check_ok(66, d)
nume += check_simple(66, n, [ 'mlut', 'mnew2', 'mnew4', 'mnew6', $
  'mnew7', 'mnew8', 'mnew9', 'mnew10', 'mnew13' ])

;  67: getdata_alter_raw check
dirfile_alter_raw, d, "new1", type=!GD.INT32
nume += check_ok2(67, 1, d)

n = get_entry(d, "new1")
nume += check_ok2(67, 2, d)
nume += check_simple2(67, 1, n.field_type, !GD.RAW_ENTRY)
nume += check_simple2(67, 2, n.field, "new1")
nume += check_simple2(67, 3, n.fragment, 0)
nume += check_simple2(67, 4, n.data_type, !GD.INT32)
nume += check_simple2(67, 5, n.spf, 3)

;  69: getdata_alter_lincom
dirfile_alter_lincom, d, "new2", in_fields=[ "in3", "in4" ], $
  m=[ COMPLEX(2.3, 4.5), COMPLEX(6.7, 8.9) ]
nume += check_ok2(69, 1, d)

n = get_entry(d, "new2")
nume += check_ok2(69, 2, d)
nume += check_simple2(69, 1, n.field_type, !GD.LINCOM_ENTRY)
nume += check_simple2(69, 2, n.field, "new2")
nume += check_simple2(69, 3, n.fragment, 0)
nume += check_simple2(69, 4, n.n_fields, 2)
nume += check_simple2(69, 5, n.in_fields, [ "in3", "in4" ])
nume += check_simple2(69, 6, n.comp_scal, 1)
nume += check_simple2(69, 7, n.cm, [ DCOMPLEX(2.3,4.5), DCOMPLEX(6.7,8.9) ])
nume += check_simple2(69, 8, n.cb, [ DCOMPLEX(1.3,1.4), DCOMPLEX(1.6,1.7) ])

;  71: dirfile_alter_polynom
dirfile_alter_polynom, d, "new4", poly_ord=4, a=[ DCOMPLEX(1.2,3.4), $
  DCOMPLEX(5.6,7.8), DCOMPLEX(9.0,1.2), DCOMPLEX(3.4,5.6), DCOMPLEX(7.8,9.0) ]
nume += check_ok2(71, 1, d)

n = get_entry(d, "new4")
nume += check_ok2(71, 2, d)
nume += check_simple2(71, 1, n.field_type, !GD.POLYNOM_ENTRY)
nume += check_simple2(71, 2, n.field, "new4")
nume += check_simple2(71, 3, n.fragment, 0)
nume += check_simple2(71, 4, n.poly_ord, 4)
nume += check_simple2(71, 5, n.in_fields, [ "in1" ])
nume += check_simple2(71, 6, n.comp_scal, 1)
nume += check_simple2(71, 7, n.ca, [ DCOMPLEX(1.2,3.4), DCOMPLEX(5.6,7.8), $
  DCOMPLEX(9.0,1.2), DCOMPLEX(3.4,5.6), DCOMPLEX(7.8,9.0) ])

;  72: dirfile_alter_linterp
dirfile_alter_linterp, d, "new6", table="./other/table"
nume += check_ok2(72, 1, d)

n = get_entry(d, "new6")
nume += check_ok2(72, 2, d)
nume += check_simple2(72, 1, n.field_type, !GD.LINTERP_ENTRY)
nume += check_simple2(72, 2, n.field, "new6")
nume += check_simple2(72, 3, n.fragment, 0)
nume += check_simple2(72, 4, n.in_fields, [ "in" ])
nume += check_simple2(72, 5, n.table, "./other/table")

;  73: dirfile_alter_bit
dirfile_alter_bit, d, "new7", in_field="in3",  numbits=8
nume += check_ok2(73, 1, d)

n = get_entry(d, "new7")
nume += check_ok2(73, 2, d)
nume += check_simple2(73, 1, n.field_type, !GD.BIT_ENTRY)
nume += check_simple2(73, 2, n.field, "new7")
nume += check_simple2(73, 3, n.fragment, 0)
nume += check_simple2(73, 4, n.in_fields, [ "in3" ])
nume += check_simple2(73, 5, n.numbits, 8)
nume += check_simple2(73, 6, n.bitnum, 11)

;  74: dirfile_alter_sbit
dirfile_alter_sbit, d, "new8", bitnum=15, numbits=1
nume += check_ok2(74, 1, d)

n = get_entry(d, "new8")
nume += check_ok2(74, 2, d)
nume += check_simple2(74, 1, n.field_type, !GD.SBIT_ENTRY)
nume += check_simple2(74, 2, n.field, "new8")
nume += check_simple2(74, 3, n.fragment, 0)
nume += check_simple2(74, 4, n.in_fields, [ "in2" ])
nume += check_simple2(74, 5, n.numbits, 1)
nume += check_simple2(74, 6, n.bitnum, 15)

;  75: dirfile_alter_multiply
dirfile_alter_multiply, d, "new9", in_field1="in6"
nume += check_ok2(75, 1, d)

n = get_entry(d, "new9")
nume += check_ok(75, d)
nume += check_simple2(75, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(75, 2, n.field, "new9")
nume += check_simple2(75, 3, n.fragment, 0)
nume += check_simple2(75, 4, n.in_fields, [ "in6", "in3" ])

;  76: dirfile_alter_phase
dirfile_alter_phase, d, "new10", shift=76
nume += check_ok2(76, 1, d)

n = get_entry(d, "new10")
nume += check_ok2(76, 2, d)
nume += check_simple2(76, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(76, 2, n.field, "new10")
nume += check_simple2(76, 3, n.fragment, 0)
nume += check_simple2(76, 4, n.in_fields, [ "in6" ])
nume += check_simple2(76, 5, n.shift, 76)

;  77: dirfile_alter_const
dirfile_alter_const, d, "new11", type=!GD.FLOAT32
nume += check_ok2(77, 1, d)

n = get_entry(d, "new11")
nume += check_ok2(77, 2, d)
nume += check_simple2(77, 1, n.field_type, !GD.CONST_ENTRY)
nume += check_simple2(77, 2, n.field, "new11")
nume += check_simple2(77, 3, n.fragment, 0)
nume += check_simple2(77, 4, n.data_type, !GD.FLOAT32)

;  141: dirfile_alter
n = {field_type: !GD.PHASE_ENTRY, shift: -8L}
dirfile_alter_entry, d, "new13", n
nume += check_ok2(141, 1, d)

n = get_entry(d, "new13")
nume += check_ok2(141, 2, d)
nume += check_simple2(141, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(141, 2, n.field, "new13")
nume += check_simple2(141, 3, n.fragment, 0)
nume += check_simple2(141, 4, n.in_fields, [ "new9" ])
nume += check_simple2(141, 5, n.shift, -8)

;  78: get_encoding
n = get_encoding(d, fragment=0)
nume += check_ok(78, d)
nume += check_simple(78, n, !GD.UNENCODED)

;  79: get_endianness
n = get_endianness(d, fragment=0)
nume += check_ok(79, d)
nume += check_simple(79, n, !GD.LITTLE_ENDIAN)

;  80: dirfilename
n = dirfilename(d)
nume += check_ok(80, d)
nume += check_simple(80, n, "test_dirfile")

;  81: get_parent_fragment
n = get_parent_fragment(d, fragment=1)
nume += check_ok(81, d)
nume += check_simple(80, n, 0)

;  82: dirfile_protect
dirfile_protect, d, !GD.PROTECT_DATA, fragment=1
nume += check_ok(82, d)

;  83: get_protection
n = get_protection(d, fragment=1)
nume += check_ok(83, d)
nume += check_simple(83, n, !GD.PROTECT_DATA)

;  84: get_raw_filename
n = get_raw_filename(d, 'data')
nume += check_ok(84, d)
nume += check_simple(84, n, "test_dirfile/data")

;  85: dirfile_reference
dirfile_reference, d, "new1"
nume += check_ok(85, d)

;  86: get_reference
n = get_reference(d)
nume += check_ok(86, d)
nume += check_simple(86, n, "new1")

;  87: dirfile_alter_encoding
dirfile_alter_encoding, d, !GD.SLIM_ENCODED, fragment=1
nume += check_ok2(87, 1, d)

n = get_encoding(d, fragment=1)
nume += check_ok2(87, 2, d)
nume += check_simple(87, n, !GD.SLIM_ENCODED)

;  88: dirfile_alter_endianness
dirfile_alter_endianness, d, /big_endian, fragment=1
nume += check_ok2(88, 1, d)

n = get_endianness(d, fragment=1)
nume += check_ok2(88, 2, d)
nume += check_simple(88, n, !GD.BIG_ENDIAN)

;  89: dirfile_alter_spec
dirfile_alter_spec, d, "new10 PHASE in 3"
nume += check_ok2(89, 1, d)

n = get_entry(d, "new10")
nume += check_ok2(89, 2, d)
nume += check_simple2(89, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(89, 2, n.field, "new10")
nume += check_simple2(89, 3, n.fragment, 0)
nume += check_simple2(89, 4, n.in_fields, [ "in" ])
nume += check_simple2(89, 5, n.shift, 3)

;  90: dirfile_delete
dirfile_delete, d, "new10"
nume += check_ok2(90, 1, d)

n = get_entry(d, "new10")
nume += check_error2(90, 2, d, !GD.E_BAD_CODE)

;  91: dirfile_malter_spec
dirfile_alter_spec, d, "mnew10 PHASE in4 11", parent="data"
nume += check_ok2(91, 1, d)

n = get_entry(d, "data/mnew10")
nume += check_ok2(91, 2, d)
nume += check_simple2(91, 1, n.field_type, !GD.PHASE_ENTRY)
nume += check_simple2(91, 2, n.field, "data/mnew10")
nume += check_simple2(91, 3, n.fragment, 0)
nume += check_simple2(91, 4, n.in_fields, [ "in4" ])
nume += check_simple2(91, 5, n.shift, 11)

;  92: dirfile_move
dirfile_move, d, "new9", 1
nume += check_ok2(92, 1, d)

n = get_entry(d, "new9")
nume += check_ok2(92, 2, d)
nume += check_simple2(92, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(92, 2, n.field, "new9")
nume += check_simple2(92, 3, n.fragment, 1)

;  93: dirfile_rename
dirfile_rename, d, "new9", "newer"
nume += check_ok2(93, 1, d)

n = get_entry(d, "new9")
nume += check_error2(93, 2, d, !GD.E_BAD_CODE)

n = get_entry(d, "newer")
nume += check_ok2(93, 3, d)
nume += check_simple2(93, 1, n.field_type, !GD.MULTIPLY_ENTRY)
nume += check_simple2(93, 2, n.field, "newer")
nume += check_simple2(93, 3, n.fragment, 1)

;  94: dirfile_uninclude
dirfile_uninclude, d, 1
nume += check_ok2(94, 1, d)

n = get_entry(d, "newer")
nume += check_error2(94, 2, d, !GD.E_BAD_CODE)

;  95: get_frameoffset
n = get_frameoffset(d,fragment=0)
nume += check_ok(95,d)
nume += check_simple(95,n,0)

;  96: dirfile_alter_frameoffset
dirfile_alter_frameoffset, d, 33, fragment=0
nume += check_ok2(96,1,d)

n = get_frameoffset(d,fragment=0)
nume += check_ok(96,d)
nume += check_simple(96,n,33)

;  97: get_native_type
n = get_native_type(d, "data")
nume += check_ok(97, d)
nume += check_simple(97,n,!GD.INT8)

;  99: dirfile_validate
n =  dirfile_validate(d, "new7")
nume += check_error(99,d,!GD.E_BAD_CODE)
nume += check_simple(99,n,-1)

;  101: get_framenum
n = get_framenum(d, "data", 33.3, field_start=6)
nume += check_ok(101,d)
nume += check_float(101, n, 37.037500D)

spawn, "rm -rf " + filedir

if (nume gt 0) then print, "nume=", nume
if (nume gt 0) then exit, /status

exit,status=0
