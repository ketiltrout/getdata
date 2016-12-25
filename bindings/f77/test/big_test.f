C     Copyright (C) 2009-2015 D. V. Wiebe
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This file is part of the GetData project.
C
C     GetData is free software; you can redistribute it and/or modify it under
C     the terms of the GNU Lesser General Public License as published by the
C     Free Software Foundation; either version 2.1 of the License, or (at your
C     option) any later version.
C
C     GetData is distributed in the hope that it will be useful, but WITHOUT
C     ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
C     FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
C     License for more details.
C
C     You should have received a copy of the GNU Lesser General Public License
C     along with GetData; if not, write to the Free Software Foundation, Inc.,
C     51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
C

C     General test
C
C     This very large test checks almost every procedure defined by the
C     F77 bindings.  Procedures not tested include: GDCOPN GDMFLS GDCLOS
C     GDCLBK GDDSCD (although this last one is used)

C     Check functions
      SUBROUTINE CHKERR(NE, T, D, V)
      INCLUDE "getdata.f"
      INTEGER NE, T, D, V, E
      CALL GDEROR(E, D)

      IF (E .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9001) T, E, V
      ENDIF
 9001 FORMAT('e[', i6, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKINT(NE, T, N, V)
      INTEGER NE, T, N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9002) T, N, V
      ENDIF
 9002 FORMAT('n[', i6, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKER2(NE, T, I, D, V)
      INCLUDE "getdata.f"
      INTEGER NE, T, I, D, V, E
      CALL GDEROR(E, D)

      IF (E .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9006) I, T, E, V
      ENDIF
 9006 FORMAT('e(', i6, ')[', i6, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKIN2(NE, T, I, N, V)
      INTEGER NE, T, I, N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9007) I, T, N, V
      ENDIF
 9007 FORMAT('n(', i6, ')[', i6, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKST2(NE, T, I, N, V)
      INTEGER NE, T, I
      CHARACTER*(*) N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9008) I, T, N, V
      ENDIF
 9008 FORMAT('s(', i6, ')[', i6, '] = "', a, '", expected "', a, '"')
      END SUBROUTINE

      SUBROUTINE CHKSTR(NE, T, N, V)
      INTEGER NE, T
      CHARACTER*(*) N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9009) T, N, V
      ENDIF
 9009 FORMAT('s[', i6, '] = "', a, '", expected "', a, '"')
      END SUBROUTINE

      SUBROUTINE CHKDB2(NE, T, I, N, V)
      INTEGER NE, T, I
      REAL*8 N, V

C     This is good to single precision
      IF (ABS(N - V) .GT. 1E-7) THEN
        NE = NE + 1
        WRITE(*, 9010) I, T, N, V
      ENDIF
 9010 FORMAT('d(', i6, ')[', i6, '] = ', d16.10, ', expected ', d16.10)
      END SUBROUTINE

      SUBROUTINE CHKCP2(NE, T, I, N, V)
      INTEGER NE, T, I
      COMPLEX*16 N, V

      IF (ABS(N - V) .GT. 1E-7) THEN
        NE = NE + 1
        WRITE(*, 9011) I, T, REAL(REAL(N)), REAL(AIMAG(N)),
     +REAL(REAL(V)), REAL(AIMAG(V))
      ENDIF
 9011 FORMAT('x(', i6, ')[', i6, '] = ', d16.10, ';', d16.10,
     +', expected ', d16.10, ';', d16.10)
      END SUBROUTINE

      SUBROUTINE CHKDBL(NE, T, N, V)
      INTEGER NE, T
      REAL*8 N, V

      IF (ABS(N - V) .GT. 1E-7) THEN
        NE = NE + 1
        WRITE(*, 9012) T, N, V
      ENDIF
 9012 FORMAT('d[', i6, '] = ', d16.10, ', expected ', d16.10)
      END SUBROUTINE

      SUBROUTINE CHKCPX(NE, T, N, V)
      INTEGER NE, T
      COMPLEX*16 N, V

      IF (ABS(N - V) .GT. 1E-7) THEN
        NE = NE + 1
        WRITE(*, 9013) T, REAL(REAL(N)), REAL(AIMAG(N)),
     +REAL(REAL(V)), REAL(AIMAG(V))
      ENDIF
 9013 FORMAT('x[', i6, '] = ', d16.10, ';', d16.10,
     +', expected ', d16.10, ';', d16.10)
      END SUBROUTINE

      SUBROUTINE CHKEOS(NE, T, N, V)
      INTEGER NE, T, F
      CHARACTER*(*) N, V

      F = INDEX(N, V)
      IF (F .EQ. 0) THEN
        F = 1
      ENDIF

      CALL CHKSTR(NE, T, N(F:), V)
      END SUBROUTINE

      SUBROUTINE CHKEOK(NE, T, D)
      INTEGER NE, T, D
      INCLUDE "getdata.f"
      CALL CHKERR(NE, T, D, GD_EOK)
      END SUBROUTINE

      SUBROUTINE CHKOK2(NE, T, I, D)
      INTEGER NE, T, I, D
      INCLUDE "getdata.f"
      CALL CHKER2(NE, T, I, D, GD_EOK)
      END SUBROUTINE




      PROGRAM BIGTST
      INCLUDE "getdata.f"
      INCLUDE "test_getdata.f"

      CHARACTER*12 fildir
      PARAMETER (fildir = 'test_dirfile')
      CHARACTER*19 frmat
      PARAMETER (frmat = 'test_dirfile/format')
      CHARACTER*18 frm2
      PARAMETER (frm2 = 'test_dirfile/form2')
      CHARACTER*17 dat
      PARAMETER (dat = 'test_dirfile/data')
      INTEGER flen
      PARAMETER (flen = 11)
      INTEGER nfields
      PARAMETER (nfields = 20)
      INTEGER slen
      PARAMETER (slen = 26)
      INTEGER plen
      PARAMETER (plen = 4096)

C     An ISO-1539 conforming Fortran-77 compiler will either consider
C     this a single character, if it treats backslash as an escape
C     character, or else silently truncate it to a single '\', if it
C     doesn't.
      CHARACTER*1 backslash
      PARAMETER (backslash = '\\')

      CHARACTER*26 strings(3)
      CHARACTER*11 fields(nfields + 9)
      CHARACTER*11 fn
      CHARACTER*26 str
      CHARACTER*4096 path
      INTEGER*1 c(8)
      INTEGER*1 datdat(80)
      INTEGER*1 k
      INTEGER i
      INTEGER d
      INTEGER m
      INTEGER n
      INTEGER l
      INTEGER j
      INTEGER ne
      REAL fl
      REAL*8 dp
      REAL*8 p(6), q(6)
      COMPLEX*16 dc
      COMPLEX*16 cp(6), cq(6)

      CALL SYSTEM ( 'rm -rf ' // fildir )
      CALL SYSTEM ( 'mkdir ' // fildir )

      DO 10 k = 1, 80
      datdat(k) = k
   10 CONTINUE
 
      fields( 1) = 'bit    '
      fields( 2) = 'div    '
      fields( 3) = 'data   '
      fields( 4) = 'mult   '
      fields( 5) = 'sbit   '
      fields( 6) = 'INDEX  '
      fields( 7) = 'alias  '
      fields( 8) = 'const  '
      fields( 9) = 'indir  '
      fields(10) = 'mplex  '
      fields(11) = 'phase  '
      fields(12) = 'recip  '
      fields(13) = 'carray '
      fields(14) = 'lincom '
      fields(15) = 'sarray '
      fields(16) = 'sindir '
      fields(17) = 'string '
      fields(18) = 'window '
      fields(19) = 'linterp'
      fields(20) = 'polynom'

C     Write the test dirfile
      OPEN(1, FILE=frmat, STATUS='NEW')
      WRITE(1, *) '/ENDIAN little'
      WRITE(1, *) 'data RAW INT8 8'
      WRITE(1, *) 'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp
     + const const'
      WRITE(1, *) '/META data mstr STRING "This is a string constant."'
      WRITE(1, *) '/META data mconst CONST COMPLEX128 3.3;4.4'
      WRITE(1, *) '/META data mlut LINTERP DATA ./lut'
      WRITE(1, *) 'const CONST FLOAT64 5.5'
      WRITE(1, *) 'carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6'
      WRITE(1, *) 'linterp LINTERP data ./lut'
      WRITE(1, *) 'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4
     + const const'
      WRITE(1, *) 'bit BIT data 3 4'
      WRITE(1, *) 'sbit SBIT data 5 6'
      WRITE(1, *) 'mplex MPLEX data sbit 1 10'
      WRITE(1, *) 'mult MULTIPLY data sbit'
      WRITE(1, *) 'phase PHASE data 11'
      WRITE(1, *) 'div DIVIDE mult bit'
      WRITE(1, *) 'recip RECIP div 6.5;4.3'
      WRITE(1, *) 'window WINDOW linterp mult LT 4.1'
      WRITE(1, *) '/ALIAS alias data'
      WRITE(1, *) 'string STRING "Zaphod Beeblebrox"'
      WRITE(1, *) 'sarray SARRAY one two three four five six seven'
      WRITE(1, *) 'data/msarray SARRAY eight nine ten eleven twelve'
      WRITE(1, *) 'indir INDIR data carray'
      WRITE(1, *) 'sindir SINDIR data sarray'
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=frm2, STATUS='NEW')
      WRITE(1, *) 'const2 CONST INT8 -19'
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=dat, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=80,
     +STATUS='NEW')
      WRITE(1, REC=1) datdat
      CLOSE(1, STATUS='KEEP')

      ne = 0
C     1: GDEROR check
      CALL GDOPEN(d, "x", 1, GD_RO)
      CALL CHKERR(ne, 1, d, GD_EIO)

C     2: GDOPEN check
      CALL GDOPEN(d, fildir, 12, GD_RW)
      CALL CHKEOK(ne, 2, d)

C     3: GDGETD check
      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c)
      CALL CHKEOK(ne, 3, d)
      CALL CHKINT(ne, 3, n, 8)

      DO 30 i = 1, 8
      CALL CHKIN2(ne, 3, i, INT(c(i)), 40 + i)
   30 CONTINUE

C     12: GDGTCO check
      CALL GDGTCO(d, 'const', 5, GD_F32, fl)
      CALL CHKEOK(ne, 12, d)
      CALL CHKDBL(ne, 12, 1d0 * fl, 5.5d0)

C     21: GDFDNX check
      CALL GDFDNX(i, d)
      CALL CHKEOK(ne, 21, d)
      CALL CHKINT(ne, 21, i, 7)

C     22: GDMFNX check
      CALL GDMFNX(i, d, 'data', 4)
      CALL CHKEOK(ne, 22, d)
      CALL CHKINT(ne, 22, i, 7)

C     23: GDNFLD check
      CALL GDNFLD(n, d)
      CALL CHKEOK(ne, 23, d)
      CALL CHKINT(ne, 23, n, nfields)

C     24: This is a check of (one of many instances of) _GDF_FString
      l = 2
      CALL GDFLDN(fn, l, d, 1)
      CALL CHKEOK(ne, 24, d)
      CALL CHKINT(ne, 24, l, 3)

C     25: GDFLDN check
      DO 250 i = 1, n
      l = flen
      CALL GDFLDN(fn, l, d, i)
      CALL CHKOK2(ne, 25, i, d)
      CALL CHKIN2(ne, 25, i, l, flen)
      CALL CHKST2(ne, 25, i, fn, fields(i))
  250 CONTINUE

C     26: GDNMFD check
      CALL GDNMFD(n, d, 'data', 4)
      CALL CHKEOK(ne, 26, d)
      CALL CHKINT(ne, 26, n, 4)

C     27: GDMFDN check
      fields(1) = 'mstr'
      fields(2) = 'mconst'
      fields(3) = 'mlut'
      fields(4) = 'msarray'
      DO 270 i = 1, n
      l = flen
      CALL GDMFDN(fn, l, d, 'data', 4, i)
      CALL CHKOK2(ne, 27, i, d)
      CALL CHKIN2(ne, 27, i, l, flen)
      CALL CHKST2(ne, 27, i, fn, fields(i))
  270 CONTINUE

C     28: GDNFRM check
      CALL GDNFRM(n, d)
      CALL CHKEOK(ne, 28, d)
      CALL CHKINT(ne, 28, n, 10)

C     29: GDGSPF check
      CALL GDGSPF(n, d, 'data', 4)
      CALL CHKEOK(ne, 29, d)
      CALL CHKINT(ne, 29, n, 8)

C     30: GDPUTD check
      c(1) = 13
      c(2) = 14
      c(3) = 15
      c(4) = 16
      c(5) = 17
      c(6) = 18
      c(7) = 19
      c(8) = 20
      CALL GDPUTD(n, d, 'data', 4, 5, 1, 0, 4, GD_I8, c)
      CALL CHKEOK(ne, 30, d)
      CALL CHKINT(ne, 30, n, 4)

      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c)

      DO 300 i = 1, 8
      IF (i .EQ. 1 .OR. i .GT. 5) THEN
        n = 40 + i
      ELSE
        n = 11 + i
      ENDIF
      CALL CHKIN2(ne, 30, i, INT(c(i)), n)
  300 CONTINUE

C     38: GDESTR check
      CALL GDGETD(n, d, 'x', 1, 5, 0, 1, 0, GD_I8, c)
      CALL GDESTR(d, str, slen)
      CALL CHKSTR(ne, 38, str, 'Field not found: x')

C     39: GDENTY check
      CALL GDENTY(n, d, 'data', 4)
      CALL CHKEOK(ne, 39, d)
      CALL CHKINT(ne, 39, n, GD_RWE)

C     40: GDGERW check
      CALL GDGERW(l, i, n, d, 'data', 4)
      CALL CHKEOK(ne, 40, d)
      CALL CHKIN2(ne, 40, 1, n, 0)
      CALL CHKIN2(ne, 40, 2, l, 8)
      CALL CHKIN2(ne, 40, 3, i, GD_I8)

C     41: GDGELC check
      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'lincom', 6)
      CALL CHKEOK(ne, 41, d)
      CALL CHKIN2(ne, 41, 1, l, flen)
      CALL CHKIN2(ne, 41, 2, i, 3)
      CALL CHKIN2(ne, 41, 3, n, 0)
      CALL CHKST2(ne, 41, 4, fields(1), 'data')
      CALL CHKST2(ne, 41, 5, fields(2), 'INDEX')
      CALL CHKST2(ne, 41, 6, fields(3), 'linterp')

      q(1) = 1.1
      q(2) = 2.2
      q(3) = 2.2
      q(4) = 3.3
      q(5) = 5.5
      q(6) = 5.5
      DO 410 i=1,6
      CALL CHKDB2(ne, 41, i, p(i), q(i))
  410 CONTINUE

C     42: GDGECL check
      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'lincom', 6)
      CALL CHKEOK(ne, 42, d)
      CALL CHKIN2(ne, 42, 1, l, flen)
      CALL CHKIN2(ne, 42, 2, i, 3)
      CALL CHKIN2(ne, 42, 3, n, 0)
      CALL CHKST2(ne, 42, 4, fields(1), 'data')
      CALL CHKST2(ne, 42, 5, fields(2), 'INDEX')
      CALL CHKST2(ne, 42, 6, fields(3), 'linterp')

      cq(1) = dcmplx(1.1d0, 0.0d0)
      cq(2) = dcmplx(2.2d0, 0.0d0)
      cq(3) = dcmplx(2.2d0, 0.0d0)
      cq(4) = dcmplx(3.3d0, 4.4d0)
      cq(5) = dcmplx(5.5d0, 0.0d0)
      cq(6) = dcmplx(5.5d0, 0.0d0)
      DO 420 i=1,6
      CALL CHKCP2(ne, 42, 6 + i, cp(i), cq(i))
  420 CONTINUE

C     43: GDGEPN check
      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'polynom', 7)
      CALL CHKEOK(ne, 43, d)
      CALL CHKIN2(ne, 43, 1, l, flen)
      CALL CHKIN2(ne, 43, 2, i, 5)
      CALL CHKIN2(ne, 43, 3, n, 0)
      CALL CHKST2(ne, 43, 4, fn, 'data')

      q(1) = 1.1
      q(2) = 2.2
      q(3) = 2.2
      q(4) = 3.3
      q(5) = 5.5
      q(6) = 5.5
      DO 430 i=1,6
      CALL CHKDB2(ne, 43, i, p(i), q(i))
  430 CONTINUE

C     44: GDGECP check
      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'polynom', 7)
      CALL CHKEOK(ne, 44, d)
      CALL CHKIN2(ne, 44, 1, l, flen)
      CALL CHKIN2(ne, 44, 2, i, 5)
      CALL CHKIN2(ne, 44, 3, n, 0)
      CALL CHKST2(ne, 44, 4, fn, 'data')

      cq(1) = dcmplx(1.1d0, 0.0)
      cq(2) = dcmplx(2.2d0, 0.0)
      cq(3) = dcmplx(2.2d0, 0.0)
      cq(4) = dcmplx(3.3d0, 4.4d0)
      cq(5) = dcmplx(5.5d0, 0.0)
      cq(6) = dcmplx(5.5d0, 0.0)
      DO 440 i=1,6
      CALL CHKCP2(ne, 44, i, cp(i), cq(i))
  440 CONTINUE

C     45: GDGELT check
      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'linterp', 7)
      CALL CHKEOK(ne, 45, d)
      CALL CHKIN2(ne, 45, 1, l, flen)
      CALL CHKIN2(ne, 45, 2, n, 0)
      CALL CHKST2(ne, 45, 3, fn, 'data')
      CALL CHKST2(ne, 45, 4, str, './lut')

C     46: GDGEBT check
      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'bit', 3)
      CALL CHKEOK(ne, 46, d)
      CALL CHKIN2(ne, 46, 1, l, flen)
      CALL CHKIN2(ne, 46, 2, n, 0)
      CALL CHKIN2(ne, 46, 3, i, 4)
      CALL CHKIN2(ne, 46, 4, m, 3)
      CALL CHKST2(ne, 46, 5, fn, 'data')

C     47: GDGESB check
      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'sbit', 4)
      CALL CHKEOK(ne, 47, d)
      CALL CHKIN2(ne, 47, 1, l, flen)
      CALL CHKIN2(ne, 47, 2, n, 0)
      CALL CHKIN2(ne, 47, 3, i, 6)
      CALL CHKIN2(ne, 47, 4, m, 5)
      CALL CHKST2(ne, 47, 5, fn, 'data')

C     48: GDGEMT check
      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'mult', 4)
      CALL CHKEOK(ne, 48, d)
      CALL CHKIN2(ne, 48, 1, l, flen)
      CALL CHKIN2(ne, 48, 2, n, 0)
      CALL CHKST2(ne, 48, 3, fields(1), 'data')
      CALL CHKST2(ne, 48, 4, fields(2), 'sbit')

C     49: GDGEPH check
      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'phase', 5)
      CALL CHKEOK(ne, 49, d)
      CALL CHKIN2(ne, 49, 1, l, flen)
      CALL CHKIN2(ne, 49, 2, n, 0)
      CALL CHKIN2(ne, 49, 3, i, 11)
      CALL CHKST2(ne, 49, 4, fn, 'data')

C     50: GDGECO check
      CALL GDGECO(i, n, d, 'const', 5)
      CALL CHKEOK(ne, 50, d)
      CALL CHKIN2(ne, 50, 1, n, 0)
      CALL CHKIN2(ne, 50, 2, i, GD_F64)

C     52: GDFRGI check
      CALL GDFRGI(n, d, 'const', 5)
      CALL CHKEOK(ne, 52, d)
      CALL CHKINT(ne, 52, n, 0)

C     53: GDADRW check
      CALL GDADRW(d, 'new1', 4, GD_F64, 3, 0)
      CALL CHKOK2(ne, 53, 1, d)

      CALL GDGERW(l, i, n, d, 'new1', 4)
      CALL CHKOK2(ne, 53, 2, d)
      CALL CHKIN2(ne, 53, 3, n, 0)
      CALL CHKIN2(ne, 53, 4, l, 3)
      CALL CHKIN2(ne, 53, 5, i, GD_F64)

C     54: GDADLC check
      CALL GDADLC(d, 'new2', 4, 2, 'in1', 3, 9.9d0, 8.8d0, 'in2', 3,
     +7.7d0, 6.6d0, '', 0, 0d0, 0d0, 0)
      CALL CHKOK2(ne, 54, 1, d)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL CHKOK2(ne, 54, 2, d)
      CALL CHKIN2(ne, 54, 3, l, flen)
      CALL CHKIN2(ne, 54, 4, i, 2)
      CALL CHKIN2(ne, 54, 5, n, 0)
      CALL CHKST2(ne, 54, 6, fields(1), 'in1')
      CALL CHKST2(ne, 54, 7, fields(2), 'in2')

      q(1) = 9.9d0
      q(2) = 8.8d0
      q(3) = 7.7d0
      q(4) = 6.6d0
      q(5) = 5.5
      q(6) = 5.5
      DO 540 i=1,4
      CALL CHKDB2(ne, 54, i, p(i), q(i))
  540 CONTINUE

C     55: GDADCL check
      cq(1) = dcmplx(1.1d0, 1.2d0)
      cq(2) = dcmplx(1.3d0, 1.4d0)
      cq(3) = dcmplx(1.4d0, 1.5d0)
      cq(4) = dcmplx(1.6d0, 1.7d0)
      CALL GDADCL(d, 'new3', 4, 2, 'in1', 3, cq(1), cq(2), 'in2', 3,
     +cq(3), cq(4), '', 0, cq(5), cq(6), 0)
      CALL CHKOK2(ne, 55, 1, d)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new3', 4)
      CALL CHKOK2(ne, 55, 2, d)
      CALL CHKIN2(ne, 55, 3, l, flen)
      CALL CHKIN2(ne, 55, 4, i, 2)
      CALL CHKIN2(ne, 55, 5, n, 0)
      CALL CHKST2(ne, 55, 6, fields(1), 'in1')
      CALL CHKST2(ne, 55, 7, fields(2), 'in2')

      cq(1) = dcmplx(1.1d0, 1.2d0)
      cq(2) = dcmplx(1.3d0, 1.4d0)
      cq(3) = dcmplx(1.4d0, 1.5d0)
      cq(4) = dcmplx(1.6d0, 1.7d0)
      DO 550 i=1,4
      CALL CHKCP2(ne, 55, i + 7, cp(i), cq(i))
  550 CONTINUE

C     56: GDADPN check
      CALL GDADPN(d, 'new4', 4, 3, 'in1', 3, 3d3, 4d4, 5d5, 6d6, 0d0,
     +0d0, 0)
      CALL CHKOK2(ne, 56, 1, d)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL CHKOK2(ne, 56, 2, d)
      CALL CHKIN2(ne, 56, 1, l, flen)
      CALL CHKIN2(ne, 56, 2, i, 3)
      CALL CHKIN2(ne, 56, 3, n, 0)
      CALL CHKST2(ne, 56, 4, fn, 'in1')

      q(1) = 3d3
      q(2) = 4d4
      q(3) = 5d5
      q(4) = 6d6
      q(5) = 5.5d0
      q(6) = 5.5d0

      DO 560 i=1,4
      CALL CHKDB2(ne, 56, i, p(i), q(i))
  560 CONTINUE

C     57: GDADCP check
      cq(1) = dcmplx(3.1d0, 7.0d0)
      cq(2) = dcmplx(4.2d0, 8.0d0)
      cq(3) = dcmplx(5.2d0, 9.0d0)
      cq(4) = dcmplx(6.3d0, 4.4d0)
      CALL GDADCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6), 0)
      CALL CHKOK2(ne, 57, 1, d)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new5', 4)
      CALL CHKOK2(ne, 57, 2, d)
      CALL CHKIN2(ne, 57, 1, l, flen)
      CALL CHKIN2(ne, 57, 2, i, 3)
      CALL CHKIN2(ne, 57, 3, n, 0)
      CALL CHKST2(ne, 57, 4, fn, 'in1')

      cq(1) = dcmplx(3.1d0, 7.0d0)
      cq(2) = dcmplx(4.2d0, 8.0d0)
      cq(3) = dcmplx(5.2d0, 9.0d0)
      cq(4) = dcmplx(6.3d0, 4.4d0)
      DO 570 i=1,4
      CALL CHKCP2(ne, 57, i, cp(i), cq(i))
  570 CONTINUE

C     58: GDADLT check
      CALL GDADLT(d, "new6", 4, "in", 2, "./some/table", 12, 0)
      CALL CHKOK2(ne, 58, 1, d)

      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'new6', 4)
      CALL CHKOK2(ne, 58, 2, d)
      CALL CHKIN2(ne, 58, 1, l, flen)
      CALL CHKIN2(ne, 58, 2, n, 0)
      CALL CHKST2(ne, 58, 3, fn, 'in')
      CALL CHKST2(ne, 58, 4, str, './some/table')

C     59: GDADBT check
      CALL GDADBT(d, "new7", 4, "in", 2, 13, 12, 0)
      CALL CHKOK2(ne, 59, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL CHKOK2(ne, 59, 2, d)
      CALL CHKIN2(ne, 59, 1, l, flen)
      CALL CHKIN2(ne, 59, 2, n, 0)
      CALL CHKIN2(ne, 59, 3, i, 12)
      CALL CHKIN2(ne, 59, 4, m, 13)
      CALL CHKST2(ne, 59, 5, fn, 'in')

C     60: GDADSB check
      CALL GDADSB(d, "new8", 4, "in", 2, 13, 12, 0)
      CALL CHKOK2(ne, 60, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL CHKOK2(ne, 60, 2, d)
      CALL CHKIN2(ne, 60, 1, l, flen)
      CALL CHKIN2(ne, 60, 2, n, 0)
      CALL CHKIN2(ne, 60, 3, i, 12)
      CALL CHKIN2(ne, 60, 4, m, 13)
      CALL CHKST2(ne, 60, 5, fn, 'in')

C     61: GDADMT check
      CALL GDADMT(d, 'new9', 4, 'in1', 3, 'in2', 3, 0)
      CALL CHKOK2(ne, 61, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKOK2(ne, 61, 2, d)
      CALL CHKIN2(ne, 61, 1, l, flen)
      CALL CHKIN2(ne, 61, 2, n, 0)
      CALL CHKST2(ne, 61, 3, fields(1), 'in1')
      CALL CHKST2(ne, 61, 4, fields(2), 'in2')

C     62: GDADPH check
      CALL GDADPH(d, 'new10', 5, 'in1', 3, 22, 0)
      CALL CHKOK2(ne, 62, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKOK2(ne, 62, 2, d)
      CALL CHKIN2(ne, 62, 1, l, flen)
      CALL CHKIN2(ne, 62, 2, n, 0)
      CALL CHKIN2(ne, 62, 3, i, 22)
      CALL CHKST2(ne, 62, 4, fn, 'in1')

C     63: GDADCO check
      CALL GDADCO(d, 'new11', 5, GD_F64, GD_F32, -8.1, 0)
      CALL CHKOK2(ne, 63, 1, d)

      CALL GDGECO(i, n, d, 'new11', 5)
      CALL CHKOK2(ne, 63, 2, d)
      CALL CHKIN2(ne, 63, 1, n, 0)
      CALL CHKIN2(ne, 63, 2, i, GD_F64)

      CALL GDGTCO(d, 'new11', 5, GD_F32, fl)
      CALL CHKOK2(ne, 63, 3, d)
      CALL CHKDBL(ne, 63, 1d0 * fl, 1d0 * (-8.1))

C     64: GDFRGN check
      l = plen
      CALL GDFRGN(path, l, d, 0)

      CALL CHKEOK(ne, 64, d)
      CALL CHKINT(ne, 64, l, plen)
      CALL CHKEOS(ne, 64, path, fildir//DIRSEP//'format')

C     65: GDNFRG check
      CALL GDNFRG(n, d)
      CALL CHKEOK(ne, 65, d)
      CALL CHKINT(ne, 65, n, 1)

C     66: GDINCL check
      CALL GDINCL(d, 'form2', 5, 0, 0)
      CALL CHKOK2(ne, 66, 1, d)

      CALL GDGTCO(d, 'const2', 6, GD_I8, c(1))
      CALL CHKOK2(ne, 66, 2, d)
      CALL CHKINT(ne, 66, INT(c(1)), -19)

C     67: GDNFDT check
      CALL GDNFDT(n, d, GD_LCE)
      CALL CHKEOK(ne, 67, d)
      CALL CHKINT(ne, 67, n, 3)

C     68: GDFDNT check
      fields(1) = 'new2'
      fields(2) = 'new3'
      fields(3) = 'lincom'
      DO 680 i = 1, n
      l = flen
      CALL GDFDNT(fn, l, d, GD_LCE, i)
      CALL CHKOK2(ne, 68, i, d)
      CALL CHKIN2(ne, 68, i, l, flen)
      CALL CHKST2(ne, 68, i, fn, fields(i))
  680 CONTINUE

C     69: GDNVEC check
      CALL GDNVEC(n, d)
      CALL CHKEOK(ne, 69, d)
      CALL CHKINT(ne, 69, n, 25)

C     70: GDVECN check
      fields( 1) = 'bit    '
      fields( 2) = 'div    '
      fields( 3) = 'data   '
      fields( 4) = 'mult   '
      fields( 5) = 'new1   '
      fields( 6) = 'new2   '
      fields( 7) = 'new3   '
      fields( 8) = 'new4   '
      fields( 9) = 'new5   '
      fields(10) = 'new6   '
      fields(11) = 'new7   '
      fields(12) = 'new8   '
      fields(13) = 'new9   '
      fields(14) = 'sbit   '
      fields(15) = 'INDEX  '
      fields(16) = 'alias  '
      fields(17) = 'indir  '
      fields(18) = 'mplex  '
      fields(19) = 'new10  '
      fields(20) = 'phase  '
      fields(21) = 'recip  '
      fields(22) = 'lincom '
      fields(23) = 'window '
      fields(24) = 'linterp'
      fields(25) = 'polynom'

      DO 700 i = 1, n
      l = flen
      CALL GDVECN(fn, l, d, i)
      CALL CHKOK2(ne, 70, i, d)
      CALL CHKIN2(ne, 70, i, l, flen)
      CALL CHKST2(ne, 70, i, fn, fields(i))
  700 CONTINUE

C     71: GDMDLC check
      CALL GDMDLC(d, 'data', 4, 'mnew1', 5, 2, 'in1', 3, 9.9d0, 8.8d0,
     +'in2', 3, 7.7d0, 6.6d0, '', 0, 0d0, 0d0)
      CALL CHKOK2(ne, 71, 1, d)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'data/mnew1', 10)
      CALL CHKOK2(ne, 71, 2, d)
      CALL CHKIN2(ne, 71, 3, l, flen)
      CALL CHKIN2(ne, 71, 4, i, 2)
      CALL CHKIN2(ne, 71, 5, n, 0)
      CALL CHKST2(ne, 71, 6, fields(1), 'in1')
      CALL CHKST2(ne, 71, 7, fields(2), 'in2')

      q(1) = 9.9d0
      q(2) = 8.8d0
      q(3) = 7.7d0
      q(4) = 6.6d0
      q(5) = 5.5
      q(6) = 5.5
      DO 710 i=1,4
      CALL CHKDB2(ne, 71, i, p(i), q(i))
  710 CONTINUE

C     72: GDMDCL check
      cq(1) = dcmplx(1.1d0, 1.2d0)
      cq(2) = dcmplx(1.3d0, 1.4d0)
      cq(3) = dcmplx(1.4d0, 1.5d0)
      cq(4) = dcmplx(1.6d0, 1.7d0)
      CALL GDMDCL(d, 'data', 4, 'mnew2', 5, 2, 'in1', 3, cq(1), cq(2),
     +'in2', 3, cq(3), cq(4), '', 0, cq(5), cq(6))
      CALL CHKOK2(ne, 72, 1, d)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'data/mnew2', 10)
      CALL CHKOK2(ne, 72, 2, d)
      CALL CHKIN2(ne, 72, 1, l, flen)
      CALL CHKIN2(ne, 72, 2, i, 2)
      CALL CHKIN2(ne, 72, 3, n, 0)
      CALL CHKST2(ne, 72, 4, fields(1), 'in1')
      CALL CHKST2(ne, 72, 5, fields(2), 'in2')

      cq(1) = dcmplx(1.1d0, 1.2d0)
      cq(2) = dcmplx(1.3d0, 1.4d0)
      cq(3) = dcmplx(1.4d0, 1.5d0)
      cq(4) = dcmplx(1.6d0, 1.7d0)
      DO 720 i=1,4
      CALL CHKCP2(ne, 72, i, cp(i), cq(i))
  720 CONTINUE

C     73: GDMDPN check
      CALL GDMDPN(d, 'data', 4, 'mnew3', 5, 3, 'in1', 3, 3d3, 4d4, 5d5,
     +6d6, 0d0, 0d0)
      CALL CHKOK2(ne, 73, 1, d)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'data/mnew3', 10)
      CALL CHKOK2(ne, 73, 2, d)
      CALL CHKIN2(ne, 73, 1, l, flen)
      CALL CHKIN2(ne, 73, 2, i, 3)
      CALL CHKIN2(ne, 73, 3, n, 0)
      CALL CHKST2(ne, 73, 4, fn, 'in1')

      q(1) = 3d3
      q(2) = 4d4
      q(3) = 5d5
      q(4) = 6d6
      q(5) = 5.5d0
      q(6) = 5.5d0
      DO 730 i=1,4
      CALL CHKDB2(ne, 73, i, p(i), q(i))
  730 CONTINUE

C     74: GDMDCP check
      cq(1) = dcmplx(1.1d0, 0.0)
      cq(2) = dcmplx(2.2d0, 0.0)
      cq(3) = dcmplx(2.2d0, 0.0)
      cq(4) = dcmplx(3.3d0, 4.4d0)
      CALL GDMDCP(d, 'data', 4, 'mnew5', 5, 3, 'in1', 3, cq(1), cq(2),
     +cq(3), cq(4), cq(5), cq(6))
      CALL CHKOK2(ne, 74, 1, d)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'data/mnew5', 10)
      CALL CHKOK2(ne, 74, 2, d)
      CALL CHKIN2(ne, 74, 1, l, flen)
      CALL CHKIN2(ne, 74, 2, i, 3)
      CALL CHKIN2(ne, 74, 3, n, 0)
      CALL CHKST2(ne, 74, 4, fn, 'in1')

      cq(1) = dcmplx(1.1d0, 0.0)
      cq(2) = dcmplx(2.2d0, 0.0)
      cq(3) = dcmplx(2.2d0, 0.0)
      cq(4) = dcmplx(3.3d0, 4.4d0)
      DO 740 i=1,4
      CALL CHKCP2(ne, 74, i, cp(i), cq(i))
  740 CONTINUE

C     75: GDMDLT check
      CALL GDMDLT(d, "data", 4, "mnew6", 5, "in", 2, "./more/table", 12)
      CALL CHKOK2(ne, 75, 1, d)

      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'data/mnew6', 10)
      CALL CHKOK2(ne, 75, 2, d)
      CALL CHKIN2(ne, 75, 1, l, flen)
      CALL CHKIN2(ne, 75, 2, n, 0)
      CALL CHKST2(ne, 75, 3, fn, 'in')
      CALL CHKST2(ne, 75, 4, str, './more/table')

C     76: GDMDBT check
      CALL GDMDBT(d, "data", 4, "mnew7", 5, "in", 2, 13, 12)
      CALL CHKOK2(ne, 76, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'data/mnew7', 10)
      CALL CHKOK2(ne, 76, 2, d)
      CALL CHKIN2(ne, 76, 1, l, flen)
      CALL CHKIN2(ne, 76, 2, n, 0)
      CALL CHKIN2(ne, 76, 3, i, 12)
      CALL CHKIN2(ne, 76, 4, m, 13)
      CALL CHKST2(ne, 76, 5, fn, 'in')

C     77: GDMDSB check
      CALL GDMDSB(d, "data", 4, "mnew8", 5, "in", 2, 13, 12)
      CALL CHKOK2(ne, 77, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'data/mnew8', 10)
      CALL CHKOK2(ne, 77, 2, d)
      CALL CHKIN2(ne, 77, 1, l, flen)
      CALL CHKIN2(ne, 77, 2, n, 0)
      CALL CHKIN2(ne, 77, 3, i, 12)
      CALL CHKIN2(ne, 77, 4, m, 13)
      CALL CHKST2(ne, 77, 5, fn, 'in')

C     78: GDMDMT check
      CALL GDMDMT(d, 'data', 4, 'mnew9', 5, 'in1', 3, 'in2', 3)
      CALL CHKOK2(ne, 78, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'data/mnew9', 10)
      CALL CHKOK2(ne, 78, 2, d)
      CALL CHKIN2(ne, 78, 1, l, flen)
      CALL CHKIN2(ne, 78, 2, n, 0)
      CALL CHKST2(ne, 78, 3, fields(1), 'in1')
      CALL CHKST2(ne, 78, 4, fields(2), 'in2')

C     79: GDMDPH check
      CALL GDMDPH(d, 'data', 4, 'mnew10', 6, 'in1', 3, 22)
      CALL CHKOK2(ne, 79, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'data/mnew10', 11)
      CALL CHKOK2(ne, 79, 2, d)
      CALL CHKIN2(ne, 79, 1, l, flen)
      CALL CHKIN2(ne, 79, 2, n, 0)
      CALL CHKIN2(ne, 79, 3, i, 22)
      CALL CHKST2(ne, 79, 4, fn, 'in1')

C     80: GDMDCO check
      CALL GDMDCO(d, 'data', 4, 'mnew11', 6, GD_F64, GD_F32, -8.1)
      CALL CHKOK2(ne, 80, 1, d)

      CALL GDGECO(i, n, d, 'data/mnew11', 11)
      CALL CHKOK2(ne, 80, 2, d)
      CALL CHKIN2(ne, 80, 1, l, flen)
      CALL CHKIN2(ne, 80, 2, n, 0)
      CALL CHKIN2(ne, 80, 3, i, GD_F64)

      CALL GDGTCO(d, 'data/mnew11', 11, GD_F32, fl)
      CALL CHKOK2(ne, 80, 3, d)
      CALL CHKDBL(ne, 80, 1d0 * fl, 1d0 * (-8.1))

C     81: GDGTST check
      CALL GDGTST(n, d, 'string', 6, slen, str)
      CALL CHKEOK(ne, 81, d)
      CALL CHKINT(ne, 81, n, 17)
      CALL CHKSTR(ne, 81, str, "Zaphod Beeblebrox")

C     82: GDADST check
      CALL GDADST(d, 'new12', 5, "---string---", 12, 0)
      CALL CHKOK2(ne, 82, 1, d)

      CALL GDGTST(n, d, 'new12', 5, slen, str)
      CALL CHKOK2(ne, 82, 2, d)
      CALL CHKSTR(ne, 82, str, "---string---")

C     83: GDMDST check
      CALL GDMDST(d, "data", 4, 'mnew12', 6, "kai su, technon;", 16)
      CALL CHKOK2(ne, 83, 1, d)

      CALL GDGTST(n, d, 'data/mnew12', 11, slen, str)
      CALL CHKOK2(ne, 83, 2, d)
      CALL CHKSTR(ne, 83, str, "kai su, technon;")

C     84: GDADSP check
      CALL GDADSP(d, 'lorem STRING "Lorem ipsum"', 26, 0)
      CALL CHKOK2(ne, 84, 1, d)

      CALL GDGTST(n, d, 'lorem', 5, slen, str)
      CALL CHKOK2(ne, 84, 2, d)
      CALL CHKSTR(ne, 84, str, "Lorem ipsum")

C     85: GDMDSP check
      CALL GDMDSP(d, 'ipsum STRING "dolor sit amet."', 30, 'lorem', 5)
      CALL CHKOK2(ne, 85, 1, d)

      CALL GDGTST(n, d, 'lorem/ipsum', 11, slen, str)
      CALL CHKOK2(ne, 85, 2, d)
      CALL CHKSTR(ne, 85, str, "dolor sit amet.")

C     86: GDPTCO check
      CALL GDPTCO(d, 'const', 5, GD_I32, 10)
      CALL CHKOK2(ne, 86, 1, d)

      CALL GDGTCO(d, 'const', 5, GD_F32, fl)
      CALL CHKOK2(ne, 86, 2, d)
      CALL CHKDBL(ne, 86, 1d0 * fl, 10.0d0)

C     94: GDPTST check
      CALL GDPTST(d, 'string', 6, 11, "Arthur Dent")
      CALL CHKOK2(ne, 94, 1, d)

      CALL GDGTST(n, d, 'string', 6, slen, str)
      CALL CHKOK2(ne, 94, 2, d)
      CALL CHKSTR(ne, 94, str, "Arthur Dent")

C     95: GDNMFT check
      CALL GDNMFT(n, d, "data", 4, GD_LCE)
      CALL CHKEOK(ne, 95, d)
      CALL CHKINT(ne, 95, n, 2)

C     96: GDMFDT check
      fields(1) = 'mnew1'
      fields(2) = 'mnew2'
      DO 960 i = 1, n
      l = flen
      CALL GDMFDT(fn, l, d, "data", 4, GD_LCE, i)
      CALL CHKOK2(ne, 96, i, d)
      CALL CHKIN2(ne, 96, i, l, flen)
      CALL CHKST2(ne, 96, i, fn, fields(i))
  960 CONTINUE

C     97: GDNMVE check
      CALL GDNMVE(n, d, "data", 4)
      CALL CHKEOK(ne, 97, d)
      CALL CHKINT(ne, 97, n, 10)

C     98: GDMVEN check
      fields(1) = 'mlut  '
      fields(2) = 'mnew1 '
      fields(3) = 'mnew2 '
      fields(4) = 'mnew3 '
      fields(5) = 'mnew5 '
      fields(6) = 'mnew6 '
      fields(7) = 'mnew7 '
      fields(8) = 'mnew8 '
      fields(9) = 'mnew9 '
      fields(10) = 'mnew10'
      DO 980 i = 1, n
      l = flen
      CALL GDMVEN(fn, l, d, "data", 4, i)
      CALL CHKOK2(ne, 98, i, d)
      CALL CHKIN2(ne, 98, i, l, flen)
      CALL CHKST2(ne, 98, i, fn, fields(i))
  980 CONTINUE

C     99: GDALRW check
      CALL GDALRW(d, 'new1', 4, GD_I32, 4, 0)
      CALL CHKOK2(ne, 99, 1, d)

      CALL GDGERW(l, i, n, d, 'new1', 4)
      CALL CHKOK2(ne, 99, 2, d)
      CALL CHKIN2(ne, 99, 3, n, 0)
      CALL CHKIN2(ne, 99, 4, l, 4)
      CALL CHKIN2(ne, 99, 5, i, GD_I32)

C     100: GDALLC check
      CALL GDALLC(d, 'new2', 4, 3, 'in4', 3, 9.9d-1, 7.8d0, 'in5', 3,
     +1.1d1, 2.2d-2, 'in6', 3, 1.96d0, 0d0)
      CALL CHKOK2(ne, 100, 1, d)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL CHKOK2(ne, 100, 2, d)
      CALL CHKIN2(ne, 100, 3, l, flen)
      CALL CHKIN2(ne, 100, 4, i, 3)
      CALL CHKIN2(ne, 100, 5, n, 0)
      CALL CHKST2(ne, 100, 6, fields(1), 'in4')
      CALL CHKST2(ne, 100, 7, fields(2), 'in5')
      CALL CHKST2(ne, 100, 8, fields(3), 'in6')

      q(1) = 9.9d-1
      q(2) = 7.8d0
      q(3) = 1.1d1
      q(4) = 2.2d-2
      q(5) = 1.96d0
      q(6) = 0d0
      DO 1000 i=1,6
      CALL CHKDB2(ne, 100, i, p(i), q(i))
 1000 CONTINUE

C     101: GDALCL check
      cq(1) = dcmplx(0.1d0, 0.2d0)
      cq(2) = dcmplx(0.3d0, 0.4d0)
      cq(3) = dcmplx(0.4d0, 0.5d0)
      cq(4) = dcmplx(0.6d0, 0.7d0)
      CALL GDALCL(d, 'new3', 4, 2, 'in4', 3, cq(1), cq(2), 'in3', 3,
     +cq(3), cq(4), '', 0, cq(5), cq(6))
      CALL CHKOK2(ne, 101, 1, d)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new3', 4)
      CALL CHKOK2(ne, 101, 2, d)
      CALL CHKIN2(ne, 101, 1, l, flen)
      CALL CHKIN2(ne, 101, 2, i, 2)
      CALL CHKIN2(ne, 101, 3, n, 0)
      CALL CHKST2(ne, 101, 4, fields(1), 'in4')
      CALL CHKST2(ne, 101, 5, fields(2), 'in3')

      cq(1) = dcmplx(0.1d0, 0.2d0)
      cq(2) = dcmplx(0.3d0, 0.4d0)
      cq(3) = dcmplx(0.4d0, 0.5d0)
      cq(4) = dcmplx(0.6d0, 0.7d0)
      DO 1010 i=1,4
      CALL CHKCP2(ne, 101, i, cp(i), cq(i))
 1010 CONTINUE

C     102: GDALPN check
      CALL GDALPN(d, 'new4', 4, 4, 'in1', 3, 3d0, 4d0, 5d0, 6d0, 7d0,
     +0d0)
      CALL CHKOK2(ne, 102, 1, d)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL CHKOK2(ne, 102, 2, d)
      CALL CHKIN2(ne, 102, 1, l, flen)
      CALL CHKIN2(ne, 102, 2, i, 4)
      CALL CHKIN2(ne, 102, 3, n, 0)
      CALL CHKST2(ne, 102, 4, fn, 'in1')

      DO 1020 i=1,5
      CALL CHKDB2(ne, 102, i, p(i), 2d0 + i)
 1020 CONTINUE

C     103: GDALCP check
      cq(1) = dcmplx(1.1d0, 5.0d0)
      cq(2) = dcmplx(1.2d0, 4.0d0)
      cq(3) = dcmplx(1.2d0, 3.0d0)
      cq(4) = dcmplx(1.3d0, 2.4d0)
      CALL GDALCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6))
      CALL CHKOK2(ne, 103, 1, d)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new5', 4)
      CALL CHKOK2(ne, 103, 2, d)
      CALL CHKIN2(ne, 103, 1, l, flen)
      CALL CHKIN2(ne, 103, 2, i, 3)
      CALL CHKIN2(ne, 103, 3, n, 0)
      CALL CHKST2(ne, 103, 4, fn, 'in1')

      cq(1) = dcmplx(1.1d0, 5.0d0)
      cq(2) = dcmplx(1.2d0, 4.0d0)
      cq(3) = dcmplx(1.2d0, 3.0d0)
      cq(4) = dcmplx(1.3d0, 2.4d0)
      DO 1030 i=1,4
      CALL CHKCP2(ne, 103, i, cp(i), cq(i))
 1030 CONTINUE

C     104: GDALLT check
      CALL GDALLT(d, "new6", 4, "in3", 3, "./other/table", 13, 0)
      CALL CHKOK2(ne, 104, 1, d)

      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'new6', 4)
      CALL CHKOK2(ne, 104, 2, d)
      CALL CHKIN2(ne, 104, 1, l, flen)
      CALL CHKIN2(ne, 104, 2, n, 0)
      CALL CHKST2(ne, 104, 3, fn, 'in3')
      CALL CHKST2(ne, 104, 4, str, './other/table')

C     105: GDALBT check
      CALL GDALBT(d, "new7", 4, "in3", 3, 3, 2)
      CALL CHKOK2(ne, 105, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL CHKOK2(ne, 105, 2, d)
      CALL CHKIN2(ne, 105, 1, l, flen)
      CALL CHKIN2(ne, 105, 2, n, 0)
      CALL CHKIN2(ne, 105, 3, i, 2)
      CALL CHKIN2(ne, 105, 4, m, 3)
      CALL CHKST2(ne, 105, 5, fn, 'in3')

C     106: GDALSB check
      CALL GDALSB(d, "new8", 4, "out", 3, 1, 22)
      CALL CHKOK2(ne, 106, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL CHKOK2(ne, 106, 2, d)
      CALL CHKIN2(ne, 106, 1, l, flen)
      CALL CHKIN2(ne, 106, 2, n, 0)
      CALL CHKIN2(ne, 106, 3, i, 22)
      CALL CHKIN2(ne, 106, 4, m, 1)
      CALL CHKST2(ne, 106, 5, fn, 'out')

C     107: GDALMT check
      CALL GDALMT(d, 'new9', 4, 'in6', 3, 'in4', 3)
      CALL CHKOK2(ne, 107, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKOK2(ne, 107, 2, d)
      CALL CHKIN2(ne, 107, 1, l, flen)
      CALL CHKIN2(ne, 107, 2, n, 0)
      CALL CHKST2(ne, 107, 3, fields(1), 'in6')
      CALL CHKST2(ne, 107, 4, fields(2), 'in4')

C     108: GDALPH check
      CALL GDALPH(d, 'new10', 5, 'in2', 3, 8)
      CALL CHKOK2(ne, 108, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKOK2(ne, 108, 2, d)
      CALL CHKIN2(ne, 108, 1, l, flen)
      CALL CHKIN2(ne, 108, 2, n, 0)
      CALL CHKIN2(ne, 108, 3, i, 8)
      CALL CHKST2(ne, 108, 4, fn, 'in2')

C     109: GDALCO check
      CALL GDALCO(d, 'new11', 5, GD_F32)
      CALL CHKOK2(ne, 109, 2, d)

      CALL GDGECO(i, n, d, 'new11', 5)
      CALL CHKOK2(ne, 109, 2, d)
      CALL CHKIN2(ne, 109, 1, l, flen)
      CALL CHKIN2(ne, 109, 2, n, 0)
      CALL CHKIN2(ne, 109, 3, i, GD_F32)

      CALL GDGTCO(d, 'new11', 5, GD_F32, fl)
      CALL CHKOK2(ne, 109, 3, d)
      CALL CHKDBL(ne, 109, 1d0 * fl, 1d0 * (-8.1))

C     110: GDGENC check
      CALL GDGENC(n, d, 0)
      CALL CHKEOK(ne, 110, d)
      CALL CHKINT(ne, 110, n, GDE_UN)

C     111: GDGEND check
      CALL GDGEND(n, d, 0)
      CALL CHKEOK(ne, 111, d)
      CALL CHKINT(ne, 111, n, GD_LE + GD_NA)

C     112: GDNAME check
      l = plen
      CALL GDNAME(path, l, d, 0)
      CALL CHKEOK(ne, 112, d)
      CALL CHKINT(ne, 112, l, plen)
      CALL CHKEOS(ne, 112, path, fildir)

C     113: GDPFRG check
      CALL GDPFRG(n, d, 1)
      CALL CHKEOK(ne, 113, d)
      CALL CHKINT(ne, 113, n, 0)

C     114: GDAPRT check
      CALL GDAPRT(d, GDPR_D, 1)
      CALL CHKEOK(ne, 114, d)

C     115: GDGPRT check
      CALL GDGPRT(n, d, 1)
      CALL CHKEOK(ne, 115, d)
      CALL CHKINT(ne, 115, n, GDPR_D)

C     116: GDRWFN check
      l = plen
      CALL GDRWFN(path, l, d, "data", 4)
      CALL CHKEOK(ne, 116, d)
      CALL CHKINT(ne, 116, l, plen)
      CALL CHKEOS(ne, 116, path, fildir//DIRSEP//'data')

C     117: GDREFE check
      l = slen
      CALL GDREFE(str, l, d, "new1", 4)
      CALL CHKEOK(ne, 117, d)
      CALL CHKINT(ne, 117, l, slen)
      CALL CHKSTR(ne, 117, str, 'new1')

C     118: GDGEOF check
      CALL GDGEOF(n, d, 'lincom', 6)
      CALL CHKEOK(ne, 118, d)
      CALL CHKINT(ne, 118, n, 80)

C     119: GDAENC check
      CALL GDAENC(d, GDE_TX, 1, 0)
      CALL CHKOK2(ne, 119, 1, d)

      CALL GDGENC(n, d, 1)
      CALL CHKOK2(ne, 119, 2, d)
      CALL CHKINT(ne, 119, n, GDE_TX)

C     120: GDAEND check
      CALL GDAEND(d, GD_BE, 1, 0)
      CALL CHKOK2(ne, 120, 1, d)

      CALL GDGEND(n, d, 1)
      CALL CHKOK2(ne, 120, 2, d)
      CALL CHKINT(ne, 120, n, GD_BE)

C     121: GDALSP check
      CALL GDALSP(d, 'new10 PHASE in1 3', 17, 0)
      CALL CHKOK2(ne, 121, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKOK2(ne, 121, 2, d)
      CALL CHKIN2(ne, 121, 1, l, flen)
      CALL CHKIN2(ne, 121, 2, n, 0)
      CALL CHKIN2(ne, 121, 3, i, 3)
      CALL CHKST2(ne, 121, 4, fn, 'in1')

C     122: GDDELE check
      CALL GDDELE(d, 'new10', 5, 0)
      CALL CHKOK2(ne, 122, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKER2(ne, 122, 2, d, GD_EBC)

C     123: GDMLSP check
      CALL GDMLSP(d, 'mnew10 PHASE in4 11', 19, 'data', 4, 0)
      CALL CHKOK2(ne, 123, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'data/mnew10', 11)
      CALL CHKOK2(ne, 123, 2, d)
      CALL CHKIN2(ne, 123, 1, l, flen)
      CALL CHKIN2(ne, 123, 2, n, 0)
      CALL CHKIN2(ne, 123, 3, i, 11)
      CALL CHKST2(ne, 123, 4, fn, 'in4')

C     124: GDMOVE check
      CALL GDMOVE(d, 'new9', 4, 1, 0)
      CALL CHKOK2(ne, 124, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKOK2(ne, 124, 2, d)
      CALL CHKIN2(ne, 124, 1, l, flen)
      CALL CHKIN2(ne, 124, 2, n, 1)
      CALL CHKST2(ne, 124, 3, fields(1), 'in6')
      CALL CHKST2(ne, 124, 4, fields(2), 'in4')

C     125: GDRENM check
      CALL GDRENM(d, 'new9', 4, 'newer', 5, 0)
      CALL CHKOK2(ne, 125, 1, d)

      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKER2(ne, 125, 2, d, GD_EBC)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'newer', 5)
      CALL CHKOK2(ne, 125, 3, d)
      CALL CHKIN2(ne, 125, 1, l, flen)
      CALL CHKIN2(ne, 125, 2, n, 1)
      CALL CHKST2(ne, 125, 3, fields(1), 'in6')
      CALL CHKST2(ne, 125, 4, fields(2), 'in4')

C     126: GDUINC check
      CALL GDUINC(d, 1, 0)
      CALL CHKOK2(ne, 126, 1, d)

      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'newer', 5)
      CALL CHKER2(ne, 126, 2, d, GD_EBC)

C     127: GDGFOF check
      CALL GDGFOF(n, d, 0)
      CALL CHKEOK(ne, 127, d)
      CALL CHKINT(ne, 127, n, 0)

C     128: GDAFOF check
      CALL GDAFOF(d, 33, 0, 0)
      CALL CHKOK2(ne, 128, 1, d)

      CALL GDGFOF(n, d, 0)
      CALL CHKOK2(ne, 128, 2, d)
      CALL CHKINT(ne, 128, n, 33)

C     129: GDNTYP check
      CALL GDNTYP(n, d, 'data', 4)
      CALL CHKEOK(ne, 129, d)
      CALL CHKINT(ne, 129, n, GD_I8)

C     130: GDENFL check
      CALL GDENFL(n, d, 'polynom', 7)
      CALL CHKEOK(ne, 130, d)
      CALL CHKINT(ne, 130, n, GDE_CS + GDE_CA)

C     131: GDVLDT check
      CALL GDVLDT(n, d, 'new7', 4)
      CALL CHKERR(ne, 131, d, GD_EBC)
      CALL CHKINT(ne, 131, n, GD_EBC)

C     132: GDFNUM check
      l = slen
      CALL GDREFE(str, l, d, "data", 4)
      CALL GDFNUM(dp, d, 'INDEX', 5, 33.3d0)
      CALL CHKEOK(ne, 132, d)
      CALL CHKDBL(ne, 132, dp, 33.3D0)

C     133: GDFNSS check
      CALL GDFNSS(dp, d, 'data', 4, 33.3d0, 6, 0)
      CALL CHKEOK(ne, 133, d)
      CALL CHKDBL(ne, 133, dp, 37.0375D0)

C     134: GDGSCA check
      l = slen
      CALL GDGSCA(str, l, n, d, 'lincom', 6, 6)
      CALL CHKEOK(ne, 134, d)
      CALL CHKINT(ne, 134, n, -1)
      CALL CHKSTR(ne, 134, str, "const")

C     135: GDASRW check
      CALL GDASRW(d, 'new135', 6, GD_F32, 0, 'carray', 6, 2, 0)
      CALL CHKOK2(ne, 135, 1, d)

      CALL GDGERW(l, i, n, d, 'new135', 6)
      CALL CHKOK2(ne, 135, 2, d)
      CALL CHKIN2(ne, 135, 3, n, 0)
      CALL CHKIN2(ne, 135, 4, l, 2)
      CALL CHKIN2(ne, 135, 5, i, GD_F32)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new135', 6, 1)
      CALL CHKOK2(ne, 135, 6, d)
      CALL CHKST2(ne, 135, 7, str, "carray")
      CALL CHKIN2(ne, 135, 8, n, 2)

C     140: GDASCA check
      CALL GDASCA(d, 'lincom', 6, 6, 'new11', 5, -1, 0)
      CALL CHKOK2(ne, 140, 1, d)

      l = slen
      CALL GDGSCA(str, l, n, d, 'lincom', 6, 6)
      CALL CHKOK2(ne, 140, 2, d)
      CALL CHKINT(ne, 140, n, -1)
      CALL CHKSTR(ne, 140, str, "new11")

C     141: GDLSRW check
      CALL GDLSRW(d, 'new135', 6, GD_F64, 0, 'const', 5, -1, 0)
      CALL CHKOK2(d, 141, 1, d)

      CALL GDGERW(l, i, n, d, 'new135', 6)
      CALL CHKOK2(ne, 135, 2, d)
      CALL CHKIN2(ne, 135, 3, n, 0)
      CALL CHKIN2(ne, 135, 4, l, 10)
      CALL CHKIN2(ne, 135, 5, i, GD_F64)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new135', 6, 1)
      CALL CHKOK2(ne, 135, 6, d)
      CALL CHKST2(ne, 135, 7, str, "const")
      CALL CHKIN2(ne, 135, 8, n, -1)


C     142: GDGBOF check
      CALL GDGBOF(n, d, 'lincom', 6)
      CALL CHKEOK(ne, 142, d)
      CALL CHKINT(ne, 142, n, 264)

C     143: GDGEDV check
      l = flen
      CALL GDGEDV(fields(1), l, fields(2), l, n, d, 'div', 3)
      CALL CHKEOK(ne, 143, d)
      CALL CHKIN2(ne, 143, 1, l, flen)
      CALL CHKIN2(ne, 143, 2, n, 0)
      CALL CHKST2(ne, 143, 3, fields(1), 'mult')
      CALL CHKST2(ne, 143, 4, fields(2), 'bit')

C     144: GDGERC check
      l = flen
      CALL GDGERC(fields(1), l, dp, n, d, 'recip', 5)
      CALL CHKEOK(ne, 144, d)
      CALL CHKIN2(ne, 144, 1, l, flen)
      CALL CHKIN2(ne, 144, 2, n, 0)
      CALL CHKST2(ne, 144, 3, fields(1), 'div')
      CALL CHKDB2(ne, 144, 4, dp, 6.5D0)

C     145: GDGECR check
      l = flen
      CALL GDGECR(fields(1), l, dc, n, d, 'recip', 5)
      CALL CHKEOK(ne, 145, d)
      CALL CHKIN2(ne, 145, 1, l, flen)
      CALL CHKIN2(ne, 145, 2, n, 0)
      CALL CHKST2(ne, 145, 3, fields(1), 'div')
      CALL CHKCP2(ne, 145, 4, dc, dcmplx(6.5d0, 4.3d0))

C     146: GDADDV check
      CALL GDADDV(d, 'new14', 5, 'in1', 3, 'in2', 3, 0)
      CALL CHKOK2(ne, 146, 1, d)

      l = flen
      CALL GDGEDV(fields(1), l, fields(2), l, n, d, 'new14', 5)
      CALL CHKOK2(ne, 146, 2, d)
      CALL CHKIN2(ne, 146, 1, l, flen)
      CALL CHKIN2(ne, 146, 2, n, 0)
      CALL CHKST2(ne, 146, 3, fields(1), 'in1')
      CALL CHKST2(ne, 146, 4, fields(2), 'in2')

C     147: GDADRC check
      p(1) = 31.9
      CALL GDADRC(d, 'new15', 5, 'in1', 3, p(1), 0)
      CALL CHKOK2(ne, 147, 1, d)

      l = flen
      CALL GDGERC(fields(1), l, dp, n, d, 'new15', 5)
      CALL CHKOK2(ne, 147, 2, d)
      CALL CHKIN2(ne, 147, 1, l, flen)
      CALL CHKIN2(ne, 147, 2, n, 0)
      CALL CHKST2(ne, 147, 3, fields(1), 'in1')
      CALL CHKDB2(ne, 147, 4, dp, p(1))

C     148: GDADCR check
      cp(1) = dcmplx(31.9d0, 38.2d0)
      CALL GDADCR(d, 'new16', 5, 'in1', 3, cp(1), 0)
      CALL CHKOK2(ne, 148, 1, d)

      l = flen
      CALL GDGECR(fields(1), l, dc, n, d, 'new16', 5)
      CALL CHKOK2(ne, 148, 2, d)
      CALL CHKIN2(ne, 148, 1, l, flen)
      CALL CHKIN2(ne, 148, 2, n, 0)
      CALL CHKST2(ne, 148, 3, fields(1), 'in1')
      CALL CHKCP2(ne, 148, 4, dc, cp(1))

C     149: GDMDDV check
      CALL GDMDDV(d, 'data', 4, 'new14', 5, 'in3', 3, 'in4', 3)
      CALL CHKOK2(ne, 149, 1, d)

      l = flen
      CALL GDGEDV(fields(1), l, fields(2), l, n, d, 'data/new14', 10)
      CALL CHKOK2(ne, 149, 2, d)
      CALL CHKIN2(ne, 149, 1, l, flen)
      CALL CHKIN2(ne, 149, 2, n, 0)
      CALL CHKST2(ne, 149, 3, fields(1), 'in3')
      CALL CHKST2(ne, 149, 4, fields(2), 'in4')

C     150: GDMDRC check
      p(1) = 95.5
      CALL GDMDRC(d, 'data', 4, 'new15', 5, 'in0', 3, p(1))
      CALL CHKOK2(ne, 150, 1, d)

      l = flen
      CALL GDGERC(fields(1), l, dp, n, d, 'data/new15', 10)
      CALL CHKOK2(ne, 150, 2, d)
      CALL CHKIN2(ne, 150, 1, l, flen)
      CALL CHKIN2(ne, 150, 2, n, 0)
      CALL CHKST2(ne, 150, 3, fields(1), 'in0')
      CALL CHKDB2(ne, 150, 4, dp, p(1))

C     151: GDADCR check
      cp(1) = dcmplx(8.47d0, 6.22d0)
      CALL GDMDCR(d,'data', 4,  'new16', 5, 'in3', 3, cp(1))
      CALL CHKOK2(ne, 151, 1, d)

      l = flen
      CALL GDGECR(fields(1), l, dc, n, d, 'data/new16', 10)
      CALL CHKOK2(ne, 151, 2, d)
      CALL CHKIN2(ne, 151, 1, l, flen)
      CALL CHKIN2(ne, 151, 2, n, 0)
      CALL CHKST2(ne, 151, 3, fields(1), 'in3')
      CALL CHKCP2(ne, 151, 4, dc, cp(1))

C     152: GDALDV check
      CALL GDALDV(d, 'new14', 5, 'in6', 3, 'in4', 3)
      CALL CHKOK2(ne, 152, 1, d)

      l = flen
      CALL GDGEDV(fields(1), l, fields(2), l, n, d, 'new14', 5)
      CALL CHKOK2(ne, 152, 2, d)
      CALL CHKIN2(ne, 152, 1, l, flen)
      CALL CHKIN2(ne, 152, 2, n, 0)
      CALL CHKST2(ne, 152, 3, fields(1), 'in6')
      CALL CHKST2(ne, 152, 4, fields(2), 'in4')

C     153: GDALRC check
      p(1) = 0.187
      CALL GDALRC(d, 'new15', 5, 'in5', 3, p(1))
      CALL CHKOK2(ne, 153, 1, d)

      l = flen
      CALL GDGERC(fields(1), l, dp, n, d, 'new15', 5)
      CALL CHKOK2(ne, 153, 2, d)
      CALL CHKIN2(ne, 153, 1, l, flen)
      CALL CHKIN2(ne, 153, 2, n, 0)
      CALL CHKST2(ne, 153, 3, fields(1), 'in5')
      CALL CHKDB2(ne, 153, 4, dp, p(1))

C     154: GDALCR check
      cp(1) = dcmplx(4.3d0, 81.81d0)
      CALL GDALCR(d, 'new16', 5, 'in6', 3, cp(1))
      CALL CHKOK2(ne, 154, 1, d)

      l = flen
      CALL GDGECR(fields(1), l, dc, n, d, 'new16', 5)
      CALL CHKOK2(ne, 154, 2, d)
      CALL CHKIN2(ne, 154, 1, l, flen)
      CALL CHKIN2(ne, 154, 2, n, 0)
      CALL CHKST2(ne, 154, 3, fields(1), 'in6')
      CALL CHKCP2(ne, 154, 4, dc, cp(1))

C     155: GDRFRG check
      CALL GDRFRG(d, 0)
      CALL CHKEOK(ne, 155, d)

C     156: GDINVD check
      CALL GDINVD(m)
      CALL CHKOK2(ne, 156, 1, m)

      CALL GDNFRG(n, m)
      CALL CHKER2(ne, 156, 2, m, GD_EBD)

      CALL GDCLOS(m)

C     157: GDSTDV check
      n = GDSV_C
      CALL GDSTDV(n, d)
      CALL CHKOK2(ne, 157, 1, d)
      CALL CHKINT(ne, 157, n, GD_DSV)

      n = 0
      CALL GDSTDV(n, d)
      CALL CHKER2(ne, 157, 2, d, GD_EAR)

C     158: GDGTCA check
      CALL GDGTCA(d, 'carray', 6, GD_F64, p)
      CALL CHKEOK(ne, 158, d)

      DO 1580 i=1,6
      CALL CHKDB2(ne, 158, i, p(i), 1.1d0 * i)
 1580 CONTINUE

C     159: GDGCAS check
      CALL GDGCAS(d, 'carray', 6, 3, 2, GD_F64, p)
      CALL CHKEOK(ne, 159, d)

      DO 1590 i=1,2
      CALL CHKDB2(ne, 159, i, p(i), 2.2d0 + 1.1 * i)
 1590 CONTINUE

C     168: GDPTCA check
      p(1) = 9.6d0
      p(2) = 8.5d0
      p(3) = 7.4d0
      p(4) = 6.3d0
      p(5) = 5.2d0
      p(6) = 4.1d0
      CALL GDPTCA(d, 'carray', 6, GD_F64, p)
      CALL CHKOK2(ne, 168, 1, d)

      CALL GDGTCA(d, 'carray', 6, GD_F64, q)
      CALL CHKOK2(ne, 168, 2, d)

      DO 1680 i=1,6
      CALL CHKDB2(ne, 168, i, q(i), 10.7d0 - 1.1d0 * i)
 1680 CONTINUE

C     169: GDGCAS check
      p(1) = 5.5d0
      p(2) = 5.6d0
      p(3) = 5.7d0
      p(4) = 5.8d0
      p(5) = 5.9d0
      p(6) = 6.0d0
      CALL GDPCAS(d, 'carray', 6, 3, 2, GD_F64, p)
      CALL CHKOK2(ne, 169, 1, d)

      CALL GDGTCA(d, 'carray', 6, GD_F64, q)
      CALL CHKOK2(ne, 169, 2, d)

      DO 1690 i=1,6
      IF (i .eq. 3 .or. i .eq. 4) THEN
        dp = 5.2d0 + 0.1d0 * i
      ELSE
        dp = 10.7d0 - 1.1d0 * i
      ENDIF
      CALL CHKDB2(ne, 169, i, q(i), dp)
 1690 CONTINUE

C     177: GDARLN check
      CALL GDARLN(n, d, 'carray', 6)
      CALL CHKEOK(ne, 177, d)
      CALL CHKINT(ne, 177, n, 6)

C     178: GDGECA check
      CALL GDGECA(i, l, n, d, 'carray', 6)
      CALL CHKEOK(ne, 178, d)
      CALL CHKIN2(ne, 178, 1, l, 6)
      CALL CHKIN2(ne, 178, 2, n, 0)
      CALL CHKIN2(ne, 178, 3, i, GD_F64)

C     179: GDADCA check
      p(1) = 1.2d0
      p(2) = 3.4d0
      p(3) = 5.6d0
      p(4) = 7.8d0
      CALL GDADCA(d, 'new17', 5, GD_F64, 4, GD_F64, p, 0)
      CALL CHKOK2(ne, 179, 1, d)

      CALL GDGECA(i, l, n, d, 'new17', 5)
      CALL CHKOK2(ne, 179, 2, d)
      CALL CHKIN2(ne, 179, 1, n, 0)
      CALL CHKIN2(ne, 179, 2, i, GD_F64)
      CALL CHKIN2(ne, 179, 3, l, 4)

      CALL GDGTCA(d, 'new17', 5, GD_F64, q)
      CALL CHKOK2(ne, 179, 3, d)

      DO 1790 i=1,4
      CALL CHKDB2(ne, 179, i, q(i), i * 2.2d0 - 1.0d0)
 1790 CONTINUE

C     180: GDMDCA check
      p(1) = 3.2d0
      p(2) = 5.4d0
      p(3) = 7.6d0
      p(4) = 9.8d0
      CALL GDMDCA(d, 'data', 4, 'new17', 5, GD_F64, 4, GD_F64, p)
      CALL CHKOK2(ne, 180, 1, d)

      CALL GDGECA(i, l, n, d, 'data/new17', 10)
      CALL CHKOK2(ne, 180, 2, d)
      CALL CHKIN2(ne, 180, 1, n, 0)
      CALL CHKIN2(ne, 180, 2, i, GD_F64)
      CALL CHKIN2(ne, 180, 3, l, 4)

      CALL GDGTCA(d, 'data/new17', 10, GD_F64, q)
      CALL CHKOK2(ne, 180, 3, d)

      DO 1800 i=1,4
      CALL CHKDB2(ne, 180, i, q(i), 1.0d0 + i * 2.2d0)
 1800 CONTINUE

C     181: GDALCA check
      CALL GDALCA(d, 'new17', 5, GD_F32, 3)
      CALL CHKOK2(ne, 181, 1, d)

      CALL GDGECA(i, l, n, d, 'new17', 5)
      CALL CHKOK2(ne, 181, 2, d)
      CALL CHKIN2(ne, 181, 1, n, 0)
      CALL CHKIN2(ne, 181, 2, i, GD_F32)
      CALL CHKIN2(ne, 181, 3, l, 3)

C     183: GDCONS check
      p(1) = 10.0d0
      p(2) = -8.1
      CALL GDNFDT(n, d, GD_COE)

      DO 1830 i = 1, n
      l = flen
      CALL GDCONS(fl, d, GD_F32, i)
      CALL CHKOK2(ne, 183, i, d)
      CALL CHKDB2(ne, 183, i, 1d0 * fl, p(i))
 1830 CONTINUE

C     191: GDMCOS check
      p(1) = 3.3d0
      p(2) = -8.1 * 1d0
      CALL GDNMFT(n, d, "data", 4, GD_COE)

      DO 1910 i = 1, n
      l = flen
      CALL GDMCOS(fl, d, "data", 4, GD_F32, i)
      CALL CHKOK2(ne, 191, i, d)
      CALL CHKDB2(ne, 191, i, 1d0 * fl, p(i))
 1910 CONTINUE

C     199: GDSTRS check
      strings(1) = "Lorem ipsum         "
      strings(2) = "---string---        "
      strings(3) = "Arthur Dent         "
      CALL GDNFDT(n, d, GD_STE)

      DO 1990 i = 1, n
      l = slen

      CALL GDSTRS(str, l, d, i)
      CALL CHKOK2(ne, 199, i, d)
      CALL CHKIN2(ne, 199, i, l, slen)
      CALL CHKST2(ne, 199, i, str, strings(i))
 1990 CONTINUE

C     200: GDMSTS check
      strings(1) = 'This is a string constant.'
      strings(2) = 'kai su, technon;          '
      CALL GDNMFT(n, d, 'data', 4, GD_STE)

      DO 2000 i = 1, n
      l = slen
      CALL GDMSTS(str, l, d, "data", 4, i)
      CALL CHKOK2(ne, 200, i, d)
      CALL CHKIN2(ne, 200, i, l, slen)
      CALL CHKST2(ne, 200, i, str, strings(i))
 2000 CONTINUE

C     201: GDSTRX check
      CALL GDSTRX(i, d)
      CALL CHKEOK(ne, 201, d)
      CALL CHKINT(ne, 201, i, 12)

C     202: GDMSTX check
      CALL GDMSTX(i, d, 'data', 4)
      CALL CHKEOK(ne, 202, d)
      CALL CHKINT(ne, 202, i, slen)

C     203: GDSEEK check
      CALL GDSEEK(n, d, 'data', 4, 35, 0, GDSK_S)
      CALL CHKOK2(ne, 203, 1, d)
      CALL CHKIN2(ne, 203, 1, n, 280)

      CALL GDGETD(m, d, 'data', 4, GD_HER, 0, 1, 0, GD_I8, c)
      CALL CHKOK2(ne, 203, 2, d)
      CALL CHKIN2(ne, 203, 2, m, 8)

      DO 2030 i = 1, 8
      CALL CHKIN2(ne, 203, i, INT(c(i)), 16 + i)
 2030 CONTINUE

C     204: GDTELL check
      CALL GDTELL(n, d, 'data', 4)
      CALL CHKEOK(ne, 204, d)
      CALL CHKINT(ne, 204, n, 288)

C     205: GDHIDE check
      CALL GDHIDE(d, 'data', 4)
      CALL CHKEOK(ne, 205, d)

C     206: GDHIDN check
      CALL GDHIDN(n, d, 'data', 4)
      CALL CHKOK2(ne, 206, 1, d)
      CALL CHKIN2(ne, 206, 1, n, 1)

      CALL GDHIDN(n,d, 'lincom', 6)
      CALL CHKOK2(ne, 206, 2, d)
      CALL CHKIN2(ne, 206, 2, n, 0)

C     207: GDUHID check
      CALL GDUHID(d, 'data', 4)
      CALL CHKOK2(ne, 206, 1, d)
      CALL GDHIDN(n, d, 'data', 4)
      CALL CHKOK2(ne, 206, 2, d)
      CALL CHKINT(ne, 206, n, 0)

C     208: GDSYNC check
      CALL GDSYNC(d, 'data', 4)
      CALL CHKEOK(ne, 208, d)

C     209: GDFLSH check
      CALL GDFLSH(d, 'data', 4)
      CALL CHKEOK(ne, 209, d)

C     210: GDMFLS check
      CALL GDMFLS(d)
      CALL CHKEOK(ne, 210, d)

C     211: GDGEWD check
      l = flen
      i = flen
      CALL GDGEWD(fields(1), i, fields(2), l, m, j, dp, n, d, 'window',
     +6)
      CALL CHKEOK(ne, 211, d)
      CALL CHKIN2(ne, 211, 1, i, flen)
      CALL CHKIN2(ne, 211, 2, l, flen)
      CALL CHKIN2(ne, 211, 3, n, 0)
      CALL CHKIN2(ne, 211, 4, m, GDW_LT)
      CALL CHKST2(ne, 211, 5, fields(1), 'linterp')
      CALL CHKST2(ne, 211, 6, fields(2), 'mult')
      CALL CHKDB2(ne, 211, 7, dp, 4.1D0)

C     212: GDADWD check
      CALL GDADWD(d, 'new18', 5, 'in1', 3, 'in2', 3, GDW_NE, 32, 0)
      CALL CHKOK2(ne, 212, 1, d)

      l = flen
      i = flen
      CALL GDGEWD(fields(1), i, fields(2), l, m, j, dp, n, d, 'new18',
     +5)
      CALL CHKOK2(ne, 212, 2, d)
      CALL CHKIN2(ne, 212, 1, i, flen)
      CALL CHKIN2(ne, 212, 2, l, flen)
      CALL CHKIN2(ne, 212, 3, n, 0)
      CALL CHKIN2(ne, 212, 4, m, GDW_NE)
      CALL CHKST2(ne, 212, 5, fields(1), 'in1')
      CALL CHKST2(ne, 212, 6, fields(2), 'in2')
      CALL CHKIN2(ne, 212, 7, j, 32)

C     214: GDMDWD check
      CALL GDMDWD(d, 'data', 4, 'mnew18', 6, 'in2', 3, 'in3', 3, GDW_ST,
     +128, 0)
      CALL CHKOK2(ne, 214, 1, d)

      l = flen
      i = flen
      CALL GDGEWD(fields(1), i, fields(2), l, m, j, dp, n, d,
     +'data/mnew18', 11)
      CALL CHKOK2(ne, 214, 2, d)
      CALL CHKIN2(ne, 214, 1, i, flen)
      CALL CHKIN2(ne, 214, 2, l, flen)
      CALL CHKIN2(ne, 214, 3, n, 0)
      CALL CHKIN2(ne, 214, 4, m, GDW_ST)
      CALL CHKST2(ne, 214, 5, fields(1), 'in2')
      CALL CHKST2(ne, 214, 6, fields(2), 'in3')
      CALL CHKIN2(ne, 214, 7, j, 128)

C     217: GDALWD check
      CALL GDALWD(d, 'new18', 5, 'in3', 3, 'in4', 3, GDW_GE, 32d3)
      CALL CHKOK2(ne, 217, 1, d)

      l = flen
      i = flen
      CALL GDGEWD(fields(1), i, fields(2), l, m, j, dp, n, d, 'new18',
     +5)
      CALL CHKOK2(ne, 217, 2, d)
      CALL CHKIN2(ne, 217, 1, i, flen)
      CALL CHKIN2(ne, 217, 2, l, flen)
      CALL CHKIN2(ne, 217, 3, n, 0)
      CALL CHKIN2(ne, 217, 4, m, GDW_GE)
      CALL CHKST2(ne, 217, 5, fields(1), 'in3')
      CALL CHKST2(ne, 217, 6, fields(2), 'in4')
      CALL CHKDB2(ne, 217, 7, dp, 32d3)

C     218: GDATRG check
      l = flen
      CALL GDATRG(fields(1), l, d, 'alias', 5)
      CALL CHKEOK(ne, 218, d)
      CALL CHKIN2(ne, 218, 1, l, flen)
      CALL CHKST2(ne, 218, 2, fields(1), 'data')

C     219: GDADAL check
      CALL GDADAL(d, 'new20', 5, 'data', 4, 0)
      CALL CHKOK2(ne, 219, 1, d)

      l = flen
      CALL GDATRG(fields(1), l, d, 'new20', 5)
      CALL CHKOK2(ne, 219, 2, d)
      CALL CHKIN2(ne, 219, 1, l, flen)
      CALL CHKST2(ne, 219, 2, fields(1), 'data')

C     220: GDMDAL check
      CALL GDMDAL(d, 'data', 4, 'mnew20', 6, 'data', 4)
      CALL CHKOK2(ne, 220, 1, d)

      l = flen
      CALL GDATRG(fields(1), l, d, 'data/mnew20', 11)
      CALL CHKOK2(ne, 220, 2, d)
      CALL CHKIN2(ne, 220, 1, l, flen)
      CALL CHKST2(ne, 220, 2, fields(1), 'data')

C     221: GDNALS check
      CALL GDNALS(n, d, 'data', 4)
      CALL CHKEOK(ne, 221, d)
      CALL CHKINT(ne, 221, n, 4)

C     222: GDALSS check
      fields(1) = 'data'
      fields(2) = 'alias'
      fields(3) = 'new20'
      fields(4) = 'data/mnew20'
      DO 2220 i = 1, n
      l = flen
      CALL GDALSS(fn, l, d, 'data', 4, i)
      CALL CHKOK2(ne, 222, i, d)
      CALL CHKIN2(ne, 222, i, l, flen)
      CALL CHKST2(ne, 222, i, fn, fields(i))
 2220 CONTINUE

C     223: GDINCA check
      CALL GDINCA(d, 'format1', 7, 0, 'A', 1, 'Z', 1, GD_CR + GD_EX)
      CALL CHKEOK(ne, 223, d)

C     226: GDFRAF check
      l = flen
      n = flen
      CALL GDFRAF(fields(1), l, fields(2), n, d, 1)
      CALL CHKEOK(ne, 226, d)
      CALL CHKIN2(ne, 226, 1, l, flen)
      CALL CHKIN2(ne, 226, 2, n, flen)
      CALL CHKST2(ne, 226, 3, fields(1), 'A')
      CALL CHKST2(ne, 226, 4, fields(2), 'Z')

C     227: GDAAFX check
      CALL GDAAFX(d, 1, 'B', 1, '', 0)
      CALL CHKOK2(ne, 227, 1, d)

      l = flen
      n = flen
      CALL GDFRAF(fields(1), l, fields(2), n, d, 1)
      CALL CHKOK2(ne, 227, 2, d)
      CALL CHKIN2(ne, 227, 1, l, flen)
      CALL CHKIN2(ne, 227, 2, n, flen)
      CALL CHKST2(ne, 227, 3, fields(1), 'B')
      CALL CHKST2(ne, 227, 3, fields(2), '')

C     228: GDGEMX check
      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'mplex', 5)
      CALL CHKEOK(ne, 228, d)
      CALL CHKIN2(ne, 228, 1, i, flen)
      CALL CHKIN2(ne, 228, 2, l, flen)
      CALL CHKIN2(ne, 228, 3, n, 0)
      CALL CHKIN2(ne, 228, 4, m, 1)
      CALL CHKST2(ne, 228, 5, fields(1), 'data')
      CALL CHKST2(ne, 228, 6, fields(2), 'sbit')
      CALL CHKIN2(ne, 228, 7, j, 10)

C     229: GDADMX check
      CALL GDADMX(d, 'new21', 5, 'in1', 3, 'in2', 3, 5, 6, 0)
      CALL CHKOK2(ne, 229, 1, d)

      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'new21', 5)
      CALL CHKOK2(ne, 229, 2, d)
      CALL CHKIN2(ne, 229, 1, i, flen)
      CALL CHKIN2(ne, 229, 2, l, flen)
      CALL CHKIN2(ne, 229, 3, n, 0)
      CALL CHKIN2(ne, 229, 4, m, 5)
      CALL CHKST2(ne, 229, 5, fields(1), 'in1')
      CALL CHKST2(ne, 229, 6, fields(2), 'in2')
      CALL CHKIN2(ne, 229, 7, j, 6)

C     230: GDMDMX check
      CALL GDMDMX(d, 'data', 4, 'mnew21', 6, 'in2', 3, 'in3', 3, 0, 12)
      CALL CHKOK2(ne, 230, 1, d)

      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'data/mnew21',
     +11)
      CALL CHKOK2(ne, 230, 2, d)
      CALL CHKIN2(ne, 230, 1, i, flen)
      CALL CHKIN2(ne, 230, 2, l, flen)
      CALL CHKIN2(ne, 230, 3, n, 0)
      CALL CHKIN2(ne, 230, 4, m, 0)
      CALL CHKST2(ne, 230, 5, fields(1), 'in2')
      CALL CHKST2(ne, 230, 6, fields(2), 'in3')
      CALL CHKIN2(ne, 230, 7, j, 12)

C     231: GDALMX check
      CALL GDALMX(d, 'new21', 5, 'in3', 3, 'in4', 3, 7, -1)
      CALL CHKOK2(ne, 231, 1, d)

      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'new21', 5)
      CALL CHKOK2(ne, 231, 2, d)
      CALL CHKIN2(ne, 231, 1, i, flen)
      CALL CHKIN2(ne, 231, 2, l, flen)
      CALL CHKIN2(ne, 231, 3, n, 0)
      CALL CHKIN2(ne, 231, 4, m, 7)
      CALL CHKST2(ne, 231, 5, fields(1), 'in3')
      CALL CHKST2(ne, 231, 6, fields(2), 'in4')
      CALL CHKIN2(ne, 231, 7, j, 6)

C     232: GDTOKE check
      l = slen
      CALL GDTOKE(str, l, d,
     +'"test1 test2" test3' // backslash // ' test4 test5', 32)
      CALL CHKOK2(ne, 232, 1, d)
      CALL CHKIN2(ne, 232, 2, l, slen)
      CALL CHKST2(ne, 232, 3, str, 'test1 test2')

      l = slen
      CALL GDTOKE(str, l, d, '', -1)
      CALL CHKOK2(ne, 232, 4, d)
      CALL CHKIN2(ne, 232, 5, l, slen)
      CALL CHKST2(ne, 232, 6, str, 'test3 test4')

C     233: GDRCLO check
      CALL GDRCLO(d, 'data', 4)
      CALL CHKEOK(ne, 233, d)

C     234: GDDSYN check
      CALL GDDSYN(n, d, 0)
      CALL CHKEOK(ne, 234, d)
      CALL CHKINT(ne, 234, n, 0)

C     235: GDFLAG check
      CALL GDFLAG(n, d, GD_PP, 0)
      CALL CHKEOK(ne, 235, d)
      CALL CHKINT(ne, 235, n, GD_PP)

C     236: GDVBPX check
      CALL GDVBPX(d, "big_test", 8)
      CALL CHKEOK(ne, 236, d)

C     237: GDNENT check
      CALL GDNENT(n, d, "data", 4, GDEN_S, GDEN_H + GDEN_N)
      CALL CHKOK2(ne, 237, 1, d)
      CALL CHKIN2(ne, 237, 1, n, 6)
      CALL GDNENT(n, d, "", -1, GDEN_V, GDEN_H + GDEN_N)
      CALL CHKOK2(ne, 237, 2, d)
      CALL CHKIN2(ne, 237, 2, n, 28)

C     238: GDENTX check
      CALL GDENTX(l, d, "", -1, GDEN_V, GDEN_H + GDEN_N)
      CALL CHKEOK(ne, 238, d)
      CALL CHKINT(ne, 238, l, 7)

C     239: GDENTN check
      fields( 1) = "bit"
      fields( 2) = "div"
      fields( 3) = "data"
      fields( 4) = "mult"
      fields( 5) = "new1"
      fields( 6) = "new2"
      fields( 7) = "new3"
      fields( 8) = "new4"
      fields( 9) = "new5"
      fields(10) = "new6"
      fields(11) = "new7"
      fields(12) = "new8"
      fields(13) = "sbit"
      fields(14) = "INDEX"
      fields(15) = "indir"
      fields(16) = "mplex"
      fields(17) = "new14"
      fields(18) = "new15"
      fields(19) = "new16"
      fields(20) = "new18"
      fields(21) = "new21"
      fields(22) = "phase"
      fields(23) = "recip"
      fields(24) = "lincom"
      fields(25) = "new135"
      fields(26) = "window"
      fields(27) = "linterp"
      fields(28) = "polynom"
      DO 2390 i = 1, n
      l = flen
      CALL GDENTN(fn, l, d, "", -1, GDEN_V, GDEN_H + GDEN_N, i)
      CALL CHKOK2(ne, 239, i, d)
      CALL CHKIN2(ne, 239, i, l, flen)
      CALL CHKST2(ne, 239, i, fn, fields(i))
 2390 CONTINUE

C     240: GDMXLB check
      CALL GDMXLB(d, GDLB_A)
      CALL CHKEOK(ne, 240, d)

C     241: GDLTTN check
      l = plen
      CALL GDLTTN(path, l, d, "linterp", 7)
      CALL CHKEOK(ne, 241, d)
      CALL CHKINT(ne, 241, l, plen)
      CALL CHKEOS(ne, 241, path, fildir//DIRSEP//'lut')

C     243: GDASLC check
      CALL GDASLC(d, 'new243', 6, 3,
     +'in1', 3, 1.1d0,      '', -1, -1, 0d0, 'carray', 6, 3,
     +'in2', 3,   0d0, 'const',  5, -1, 0d0, 'carray', 6, 4,
     +'in3', 3, 1.4d0,      '', -1, -1, 0d0, 'carray', 6, 5, 0)
      CALL CHKOK2(ne, 243, 1, d)

      CALL GDENFL(n, d, 'new243', 6)
      CALL CHKOK2(ne, 243, 2, d)
      CALL CHKIN2(ne, 243, 3, n, GDE_CA)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new243', 6)
      CALL CHKOK2(ne, 243,  4, d)
      CALL CHKIN2(ne, 243,  5, l, flen)
      CALL CHKIN2(ne, 243,  6, i, 3)
      CALL CHKIN2(ne, 243,  7, n, 0)
      CALL CHKST2(ne, 243,  8, fields(1), 'in1')
      CALL CHKST2(ne, 243,  9, fields(2), 'in2')
      CALL CHKST2(ne, 243, 10, fields(3), 'in3')

      q(1) = 1.1d0
      q(3) = 10d0
      q(5) = 1.4d0
      q(2) = 5.5
      q(4) = 5.6d0
      q(6) = 5.2d0
      DO 2430 i=1,6
      CALL CHKDB2(ne, 243, 10 + i, p(i), q(i))
 2430 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 1)
      CALL CHKOK2(ne, 243, 17, d)
      CALL CHKST2(ne, 243, 18, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 2)
      CALL CHKOK2(ne, 243, 19, d)
      CALL CHKST2(ne, 243, 20, str, "const")
      CALL CHKIN2(ne, 243, 21, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 3)
      CALL CHKOK2(ne, 243, 22, d)
      CALL CHKST2(ne, 243, 23, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 4)
      CALL CHKOK2(ne, 243, 24, d)
      CALL CHKST2(ne, 243, 25, str, "carray")
      CALL CHKIN2(ne, 243, 26, n, 3)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 5)
      CALL CHKOK2(ne, 243, 27, d)
      CALL CHKST2(ne, 243, 28, str, "carray")
      CALL CHKIN2(ne, 243, 29, n, 4)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 6)
      CALL CHKOK2(ne, 243, 30, d)
      CALL CHKST2(ne, 243, 31, str, "carray")
      CALL CHKIN2(ne, 243, 32, n, 5)

C     244: GDASPN check
      CALL GDASPN(d, 'new244', 6, 3, 'in1', 3, 33d0, '', -1, -1, 
     +44d0, '', -1, -1, 66d0, '', -1, -1, 0d0,  'carray', 6, 1, 
     +0d0,  '', -1, -1, 0d0,  '', -1, -1, 0)
      CALL CHKOK2(ne, 244, 1, d)

      CALL GDENFL(n, d, 'new244', 6)
      CALL CHKOK2(ne, 244, 2, d)
      CALL CHKIN2(ne, 244, 3, n, GDE_CA)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new244', 6)
      CALL CHKOK2(ne, 244, 4, d)
      CALL CHKIN2(ne, 244, 5, l, flen)
      CALL CHKIN2(ne, 244, 6, i, 3)
      CALL CHKIN2(ne, 244, 7, n, 0)
      CALL CHKST2(ne, 244, 8, fn, 'in1')

      q(1) = 33d0
      q(2) = 44d0
      q(3) = 66d0
      q(4) = 9.6d0

      DO 2440 i=1,4
      CALL CHKDB2(ne, 244, 8 + i, p(i), q(i))
 2440 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 1)
      CALL CHKOK2(ne, 244, 13, d)
      CALL CHKST2(ne, 244, 14, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 2)
      CALL CHKOK2(ne, 244, 15, d)
      CALL CHKST2(ne, 244, 16, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 3)
      CALL CHKOK2(ne, 244, 17, d)
      CALL CHKST2(ne, 244, 18, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 4)
      CALL CHKOK2(ne, 244, 19, d)
      CALL CHKST2(ne, 244, 20, str, "carray")
      CALL CHKIN2(ne, 244, 21, n, 1)

C     246: GDASBT check
      CALL GDASBT(d, 'new246', 6, 'in1', 3, 11, '', -1, -1, 0, 'const',
     +5, 0, 0)
      CALL CHKOK2(ne, 246, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new246', 6)
      CALL CHKOK2(ne, 246, 2, d)
      CALL CHKIN2(ne, 246, 3, l, flen)
      CALL CHKIN2(ne, 246, 4, n, 0)
      CALL CHKIN2(ne, 246, 5, m, 11)
      CALL CHKIN2(ne, 246, 6, i, 10)
      CALL CHKST2(ne, 246, 7, fn, 'in1')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new246', 6, 1)
      CALL CHKOK2(ne, 246, 8, d)
      CALL CHKST2(ne, 246, 9, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new246', 6, 2)
      CALL CHKOK2(ne, 246, 10, d)
      CALL CHKST2(ne, 246, 11, str, "const")
      CALL CHKIN2(ne, 246, 12, n, -1)

C     248: GDASPH check
      CALL GDASPH(d, 'new248', 6, 'in1', 3, 0, 'carray', 6, 3, 0)
      CALL CHKOK2(ne, 248, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new248', 6)
      CALL CHKOK2(ne, 248, 2, d)
      CALL CHKIN2(ne, 248, 3, l, flen)
      CALL CHKIN2(ne, 248, 4, n, 0)
      CALL CHKIN2(ne, 248, 5, i, 5)
      CALL CHKST2(ne, 248, 6, fn, 'in1')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new248', 6, 1)
      CALL CHKOK2(ne, 248, 7, d)
      CALL CHKST2(ne, 248, 8, str, "carray")
      CALL CHKIN2(ne, 248, 9, n, 3)

C     251: GDASRC check
      CALL GDASRC(d, 'new251', 6, 'in1', 3, 0d0, 'carray', 6, 4, 0)
      CALL CHKOK2(ne, 251, 1, d)

      CALL GDENFL(n, d, 'new251', 6)
      CALL CHKOK2(ne, 251, 2, d)
      CALL CHKIN2(ne, 251, 3, n, GDE_CA)

      l = flen
      CALL GDGERC(fn, l, dp, n, d, 'new251', 6)
      CALL CHKOK2(ne, 251, 4, d)
      CALL CHKIN2(ne, 251, 5, l, flen)
      CALL CHKIN2(ne, 251, 6, n, 0)
      CALL CHKDB2(ne, 251, 7, dp, 5.6d0)
      CALL CHKST2(ne, 251, 8, fn, 'in1')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new251', 6, 1)
      CALL CHKOK2(ne, 251, 9, d)
      CALL CHKST2(ne, 251, 10, str, "carray")
      CALL CHKIN2(ne, 251, 11, n, 4)

C     253: GDASWD check
      CALL GDASWD(d, 'new253', 6, 'in2', 3, 'in3', 3, GDW_EQ, 0,
     +'const', 5, -1, 0)
      CALL CHKOK2(ne, 253, 1, d)

      l = flen
      i = flen
      CALL GDGEWD(fields(1), i, fields(2), l, m, j, dp, n, d, 'new253',
     +6)
      CALL CHKOK2(ne, 253, 2, d)
      CALL CHKIN2(ne, 253, 3, i, flen)
      CALL CHKIN2(ne, 253, 4, l, flen)
      CALL CHKIN2(ne, 253, 5, n, 0)
      CALL CHKIN2(ne, 253, 6, m, GDW_EQ)
      CALL CHKST2(ne, 253, 7, fields(1), 'in2')
      CALL CHKST2(ne, 253, 8, fields(2), 'in3')
      CALL CHKIN2(ne, 253, 9, j, 10)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new253', 6, 1)
      CALL CHKOK2(ne, 253, 10, d)
      CALL CHKST2(ne, 253, 11, str, "const")
      CALL CHKIN2(ne, 253, 12, n, -1)

C     254: GDASMX check
      CALL GDASMX(d, 'new254', 6, 'in1', 3, 'in2', 3, 0, 'carray', 6, 3,
     +0, 'carray', 6, 4, 0)
      CALL CHKOK2(ne, 254, 1, d)

      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'new254', 6)
      CALL CHKOK2(ne, 254, 2, d)
      CALL CHKIN2(ne, 254, 3, i, flen)
      CALL CHKIN2(ne, 254, 4, l, flen)
      CALL CHKIN2(ne, 254, 5, n, 0)
      CALL CHKIN2(ne, 254, 6, m, 5)
      CALL CHKST2(ne, 254, 7, fields(1), 'in1')
      CALL CHKST2(ne, 254, 8, fields(2), 'in2')
      CALL CHKIN2(ne, 254, 9, j, 5)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new254', 6, 1)
      CALL CHKOK2(ne, 254, 10, d)
      CALL CHKST2(ne, 254, 11, str, "carray")
      CALL CHKIN2(ne, 254, 12, n, 3)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new254', 6, 2)
      CALL CHKOK2(ne, 254, 13, d)
      CALL CHKST2(ne, 254, 14, str, "carray")
      CALL CHKIN2(ne, 254, 15, n, 4)

C     255: GDASCL check
      CALL GDASCL(d, 'new255', 6, 3, 'in1', 3, dcmplx(1.1d0, 1.2d0),
     +'', -1, -1, dcmplx(0, 0), 'carray', 6, 3, 'in2', 3, 
     +dcmplx(    0,     0), 'const',  5, -1, dcmplx(0, 0), 'carray', 6,
     +4, 'in3', 3, dcmplx(1.3d0, 1.4d0),      '', -1, -1, dcmplx(0, 0),
     +'carray', 6, 5, 0)
      CALL CHKOK2(ne, 255, 1, d)

      CALL GDENFL(n, d, 'new255', 6)
      CALL CHKOK2(ne, 255, 2, d)
      CALL CHKIN2(ne, 255, 3, n, GDE_CS + GDE_CA)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new255', 6)
      CALL CHKOK2(ne, 255,  4, d)
      CALL CHKIN2(ne, 255,  5, l, flen)
      CALL CHKIN2(ne, 255,  6, i, 3)
      CALL CHKIN2(ne, 255,  7, n, 0)
      CALL CHKST2(ne, 255,  8, fields(1), 'in1')
      CALL CHKST2(ne, 255,  9, fields(2), 'in2')
      CALL CHKST2(ne, 255, 10, fields(3), 'in3')

      cq(1) = dcmplx(1.1d0, 1.2d0)
      cq(3) = dcmplx(10d0, 0d0)
      cq(5) = dcmplx(1.3d0, 1.4d0)
      cq(2) = dcmplx(5.5, 0d0)
      cq(4) = dcmplx(5.6d0, 0d0)
      cq(6) = dcmplx(5.2d0, 0d0)
      DO 2550 i=1,6
      CALL CHKCP2(ne, 255, 10 + i, cp(i), cq(i))
 2550 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new255', 6, 1)
      CALL CHKOK2(ne, 255, 17, d)
      CALL CHKST2(ne, 255, 18, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new255', 6, 2)
      CALL CHKOK2(ne, 255, 19, d)
      CALL CHKST2(ne, 255, 20, str, "const")
      CALL CHKIN2(ne, 255, 21, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new255', 6, 3)
      CALL CHKOK2(ne, 255, 22, d)
      CALL CHKST2(ne, 255, 23, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new255', 6, 4)
      CALL CHKOK2(ne, 255, 24, d)
      CALL CHKST2(ne, 255, 25, str, "carray")
      CALL CHKIN2(ne, 255, 26, n, 3)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new255', 6, 5)
      CALL CHKOK2(ne, 255, 27, d)
      CALL CHKST2(ne, 255, 28, str, "carray")
      CALL CHKIN2(ne, 255, 29, n, 4)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new255', 6, 6)
      CALL CHKOK2(ne, 255, 30, d)
      CALL CHKST2(ne, 255, 31, str, "carray")
      CALL CHKIN2(ne, 255, 32, n, 5)

C     256: GDASCP check
      CALL GDASCP(d, 'new256', 6, 3, 'in1', 3, dcmplx(22d0, 33d0), '',
     +-1, -1, dcmplx(44d0, 55d0), '', -1, -1, dcmplx(66d0, 77d0), '',
     +-1, -1, dcmplx(0, 0),  'carray', 6, 1, dcmplx(0, 0),  '', -1, -1,
     +dcmplx(0, 0),  '', -1, -1, 0)
      CALL CHKOK2(ne, 256, 1, d)

      CALL GDENFL(n, d, 'new256', 6)
      CALL CHKOK2(ne, 256, 2, d)
      CALL CHKIN2(ne, 256, 3, n, GDE_CS + GDE_CA)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new256', 6)
      CALL CHKOK2(ne, 256, 4, d)
      CALL CHKIN2(ne, 256, 5, l, flen)
      CALL CHKIN2(ne, 256, 6, i, 3)
      CALL CHKIN2(ne, 256, 7, n, 0)
      CALL CHKST2(ne, 256, 8, fn, 'in1')

      cq(1) = dcmplx(22d0, 33d0)
      cq(2) = dcmplx(44d0, 55d0)
      cq(3) = dcmplx(66d0, 77d0)
      cq(4) = dcmplx(9.6d0, 0)

      DO 2560 i=1,4
      CALL CHKCP2(ne, 256, 8 + i, cp(i), cq(i))
 2560 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new256', 6, 1)
      CALL CHKOK2(ne, 256, 13, d)
      CALL CHKST2(ne, 256, 14, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new256', 6, 2)
      CALL CHKOK2(ne, 256, 15, d)
      CALL CHKST2(ne, 256, 16, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new256', 6, 3)
      CALL CHKOK2(ne, 256, 17, d)
      CALL CHKST2(ne, 256, 18, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new256', 6, 4)
      CALL CHKOK2(ne, 256, 19, d)
      CALL CHKST2(ne, 256, 20, str, "carray")
      CALL CHKIN2(ne, 256, 21, n, 1)

C     257: GDASCR check
      CALL GDASCR(d, 'new257', 6, 'in1', 3, 0d0, 'carray', 6, 4, 0)
      CALL CHKOK2(ne, 257, 1, d)

      CALL GDENFL(n, d, 'new257', 6)
      CALL CHKOK2(ne, 257, 2, d)
      CALL CHKIN2(ne, 257, 3, n, GDE_CA)

      l = flen
      CALL GDGECR(fn, l, dc, n, d, 'new257', 6)
      CALL CHKOK2(ne, 257, 4, d)
      CALL CHKIN2(ne, 257, 5, l, flen)
      CALL CHKIN2(ne, 257, 6, n, 0)
      CALL CHKCP2(ne, 257, 7, dc, dcmplx(5.6d0, 0))
      CALL CHKST2(ne, 257, 8, fn, 'in1')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new257', 6, 1)
      CALL CHKOK2(ne, 257, 9, d)
      CALL CHKST2(ne, 257, 10, str, "carray")
      CALL CHKIN2(ne, 257, 11, n, 4)

C     258: GDASSB check
      CALL GDASSB(d, 'new258', 6, 'in1', 3, 11, '', -1, -1, 0, 'const',
     +5, 0, 0)
      CALL CHKOK2(ne, 258, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new258', 6)
      CALL CHKOK2(ne, 258, 2, d)
      CALL CHKIN2(ne, 258, 3, l, flen)
      CALL CHKIN2(ne, 258, 4, n, 0)
      CALL CHKIN2(ne, 258, 5, m, 11)
      CALL CHKIN2(ne, 258, 6, i, 10)
      CALL CHKST2(ne, 258, 7, fn, 'in1')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new258', 6, 1)
      CALL CHKOK2(ne, 258, 8, d)
      CALL CHKST2(ne, 258, 9, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new258', 6, 2)
      CALL CHKOK2(ne, 258, 10, d)
      CALL CHKST2(ne, 258, 11, str, "const")
      CALL CHKIN2(ne, 258, 12, n, -1)

C     259: GDLSLC check
      CALL GDLSLC(d, 'new243', 6, 3,
     +'', -1, 2.2d0, '', -1, 1, 0d0, 'carray', 6, 4,
     +'in4', 3, 0d0, '', 0, 0, 259d0, '', -1, 0,
     +'', -1, 0d0, 'const', 5, -1, 0d0, 'const', 5, -1)
      CALL CHKOK2(ne, 259, 1, d)

      CALL GDENFL(n, d, 'new243', 6)
      CALL CHKOK2(ne, 259, 2, d)
      CALL CHKIN2(ne, 259, 3, n, GDE_CA)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new243', 6)
      CALL CHKOK2(ne, 259,  4, d)
      CALL CHKIN2(ne, 259,  5, l, flen)
      CALL CHKIN2(ne, 259,  6, i, 3)
      CALL CHKIN2(ne, 259,  7, n, 0)
      CALL CHKST2(ne, 259,  8, fields(1), 'in1')
      CALL CHKST2(ne, 259,  9, fields(2), 'in4')
      CALL CHKST2(ne, 259, 10, fields(3), 'in3')

      q(1) = 2.2d0
      q(3) = 10d0
      q(5) = 10d0
      q(2) = 5.6d0
      q(4) = 259d0
      q(6) = 10d0
      DO 2590 i=1,6
      CALL CHKDB2(ne, 259, 10 + i, p(i), q(i))
 2590 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 1)
      CALL CHKOK2(ne, 259, 17, d)
      CALL CHKST2(ne, 259, 18, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 2)
      CALL CHKOK2(ne, 259, 19, d)
      CALL CHKST2(ne, 259, 20, str, "const")
      CALL CHKIN2(ne, 259, 21, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 3)
      CALL CHKOK2(ne, 259, 22, d)
      CALL CHKST2(ne, 259, 23, str, "const")
      CALL CHKIN2(ne, 259, 24, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 4)
      CALL CHKOK2(ne, 259, 25, d)
      CALL CHKST2(ne, 259, 26, str, "carray")
      CALL CHKIN2(ne, 259, 27, n, 4)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 5)
      CALL CHKOK2(ne, 259, 28, d)
      CALL CHKST2(ne, 259, 29, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 6)
      CALL CHKOK2(ne, 259, 30, d)
      CALL CHKST2(ne, 259, 31, str, "const")
      CALL CHKIN2(ne, 259, 32, n, -1)

C     260: GDLSLC check
      CALL GDLSCL(d, 'new243', 6, 3,
     +'', -1, dcmplx(9d0, 8d0), '', -1, 1, dcmplx(0, 0), 'carray', 6, 3,
     +'in4', 3, dcmplx(0, 0), '', 0, 0, dcmplx(260d0, 0), '', -1, 0,
     +'', -1, dcmplx(0, 0), '', 0, -1, dcmplx(0, 0), '', 0, -1)
      CALL CHKOK2(ne, 260, 1, d)

      CALL GDENFL(n, d, 'new243', 6)
      CALL CHKOK2(ne, 260, 2, d)
      CALL CHKIN2(ne, 260, 3, n, GDE_CS + GDE_CA)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new243', 6)
      CALL CHKOK2(ne, 260,  4, d)
      CALL CHKIN2(ne, 260,  5, l, flen)
      CALL CHKIN2(ne, 260,  6, i, 3)
      CALL CHKIN2(ne, 260,  7, n, 0)
      CALL CHKST2(ne, 260,  8, fields(1), 'in1')
      CALL CHKST2(ne, 260,  9, fields(2), 'in4')
      CALL CHKST2(ne, 260, 10, fields(3), 'in3')

      cq(1) = dcmplx(9d0, 8d0)
      cq(3) = dcmplx(10d0, 0)
      cq(5) = dcmplx(10d0, 0)
      cq(2) = dcmplx(5.5d0, 0)
      cq(4) = dcmplx(260d0, 0)
      cq(6) = dcmplx(10d0, 0)
      DO 2600 i=1,6
      CALL CHKCP2(ne, 260, 10 + i, cp(i), cq(i))
 2600 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 1)
      CALL CHKOK2(ne, 260, 17, d)
      CALL CHKST2(ne, 260, 18, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 2)
      CALL CHKOK2(ne, 260, 19, d)
      CALL CHKST2(ne, 260, 20, str, "const")
      CALL CHKIN2(ne, 260, 21, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 3)
      CALL CHKOK2(ne, 260, 22, d)
      CALL CHKST2(ne, 260, 23, str, "const")
      CALL CHKIN2(ne, 260, 24, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 4)
      CALL CHKOK2(ne, 260, 25, d)
      CALL CHKST2(ne, 260, 26, str, "carray")
      CALL CHKIN2(ne, 260, 27, n, 3)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 5)
      CALL CHKOK2(ne, 260, 28, d)
      CALL CHKST2(ne, 260, 29, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new243', 6, 6)
      CALL CHKOK2(ne, 260, 30, d)
      CALL CHKST2(ne, 260, 31, str, "const")
      CALL CHKIN2(ne, 260, 32, n, -1)

C     261: GDLSPN check
      CALL GDLSPN(d, 'new244', 6, 3, 'in3', 3, 2d0, '', 0, 0,
     +6d0, '', 0, 0, 0d0, 'carray', 6, 5, 261d0, '', -1, 0,
     +0d0, '', -1, 0, 0d0, '', -1, 0)
      CALL CHKOK2(ne, 261, 1, d)

      CALL GDENFL(n, d, 'new244', 6)
      CALL CHKOK2(ne, 261, 2, d)
      CALL CHKIN2(ne, 261, 3, n, GDE_CA)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new244', 6)
      CALL CHKOK2(ne, 261, 4, d)
      CALL CHKIN2(ne, 261, 5, l, flen)
      CALL CHKIN2(ne, 261, 6, i, 3)
      CALL CHKIN2(ne, 261, 7, n, 0)
      CALL CHKST2(ne, 261, 8, fn, 'in3')

      q(1) = 2d0
      q(2) = 6d0
      q(3) = 5.2d0
      q(4) = 261d0

      DO 2610 i=1,4
      CALL CHKDB2(ne, 261, 8 + i, p(i), q(i))
 2610 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 1)
      CALL CHKOK2(ne, 261, 13, d)
      CALL CHKST2(ne, 261, 14, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 2)
      CALL CHKOK2(ne, 261, 15, d)
      CALL CHKST2(ne, 261, 16, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 3)
      CALL CHKOK2(ne, 261, 17, d)
      CALL CHKST2(ne, 261, 18, str, "carray")
      CALL CHKIN2(ne, 261, 19, n, 5)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 4)
      CALL CHKOK2(ne, 261, 20, d)
      CALL CHKST2(ne, 261, 21, str, "")

C     262: GDLSCP check
      CALL GDLSCP(d, 'new244', 6, 3, '', -1,
     +dcmplx(0, 0), 'const', 5, -1, dcmplx(0, 0), 'const', 5, -1,
     +dcmplx(262d0, 0d0), '', -1, 5, dcmplx(262d0, 0d0), '', 0, 0,
     +dcmplx(0d0, 0d0), '', -1, 0, dcmplx(0d0, 0d0), '', -1, 0)
      CALL CHKOK2(ne, 262, 1, d)

      CALL GDENFL(n, d, 'new244', 6)
      CALL CHKOK2(ne, 262, 2, d)
      CALL CHKIN2(ne, 262, 3, n, GDE_CA)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new244', 6)
      CALL CHKOK2(ne, 262, 4, d)
      CALL CHKIN2(ne, 262, 5, l, flen)
      CALL CHKIN2(ne, 262, 6, i, 3)
      CALL CHKIN2(ne, 262, 7, n, 0)
      CALL CHKST2(ne, 262, 8, fn, 'in3')

      cq(1) = dcmplx(10d0, 0)
      cq(2) = dcmplx(10d0, 0)
      cq(3) = dcmplx(262d0, 0)
      cq(4) = dcmplx(262d0, 0)

      DO 2620 i=1,4
      CALL CHKCP2(ne, 262, 8 + i, cp(i), cq(i))
 2620 CONTINUE

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 1)
      CALL CHKOK2(ne, 262, 13, d)
      CALL CHKST2(ne, 262, 14, str, "const")
      CALL CHKIN2(ne, 262, 15, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 2)
      CALL CHKOK2(ne, 262, 16, d)
      CALL CHKST2(ne, 262, 17, str, "const")
      CALL CHKIN2(ne, 262, 18, n, -1)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 3)
      CALL CHKOK2(ne, 262, 19, d)
      CALL CHKST2(ne, 262, 20, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new244', 6, 4)
      CALL CHKOK2(ne, 262, 21, d)
      CALL CHKST2(ne, 262, 22, str, "")

C     263: GDLSBT check
      CALL GDLSBT(d, 'new246', 6, 'in0', 3, 0, 'carray', 6, 6, 0,
     +'', -1, -1)
      CALL CHKOK2(ne, 263, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new246', 6)
      CALL CHKOK2(ne, 263, 2, d)
      CALL CHKIN2(ne, 263, 3, l, flen)
      CALL CHKIN2(ne, 263, 4, n, 0)
      CALL CHKIN2(ne, 263, 5, m, 4)
      CALL CHKIN2(ne, 263, 6, i, 10)
      CALL CHKST2(ne, 263, 7, fn, 'in0')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new246', 6, 1)
      CALL CHKOK2(ne, 263, 8, d)
      CALL CHKST2(ne, 263, 9, str, "carray")
      CALL CHKIN2(ne, 263, 10, n, 6)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new246', 6, 2)
      CALL CHKOK2(ne, 263, 11, d)
      CALL CHKST2(ne, 263, 12, str, "")

C     264: GDLSSB check
      CALL GDLSSB(d, 'new258', 6, 'in2', 3, -1, '', -1, 0, 0,
     +'carray', 6, 5)
      CALL CHKOK2(ne, 264, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new258', 6)
      CALL CHKOK2(ne, 264, 2, d)
      CALL CHKIN2(ne, 264, 3, l, flen)
      CALL CHKIN2(ne, 264, 4, n, 0)
      CALL CHKIN2(ne, 264, 5, m, 11)
      CALL CHKIN2(ne, 264, 6, i, 5)
      CALL CHKST2(ne, 264, 7, fn, 'in2')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new258', 6, 1)
      CALL CHKOK2(ne, 264, 8, d)
      CALL CHKST2(ne, 264, 9, str, "")

      l = slen
      CALL GDGSCA(str, l, n, d, 'new258', 6, 2)
      CALL CHKOK2(ne, 264, 10, d)
      CALL CHKST2(ne, 264, 11, str, "carray")
      CALL CHKIN2(ne, 264, 12, n, 5)

C     265: GDLSPH check
      CALL GDLSPH(d, 'new248', 6, '', -1, -265, '', -1, -1)
      CALL CHKOK2(ne, 265, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new248', 6)
      CALL CHKOK2(ne, 265, 2, d)
      CALL CHKIN2(ne, 265, 3, l, flen)
      CALL CHKIN2(ne, 265, 4, n, 0)
      CALL CHKIN2(ne, 265, 5, i, -265)
      CALL CHKST2(ne, 265, 6, fn, 'in1')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new248', 6, 1)
      CALL CHKOK2(ne, 265, 7, d)
      CALL CHKST2(ne, 265, 8, str, "")

C     266: GDLSRC check
      CALL GDLSRC(d, 'new251', 6, 'in5', 3, 0d0, 'carray', 6, 2)
      CALL CHKOK2(ne, 266, 1, d)

      CALL GDENFL(n, d, 'new251', 6)
      CALL CHKOK2(ne, 266, 2, d)
      CALL CHKIN2(ne, 266, 3, n, GDE_CA)

      l = flen
      CALL GDGERC(fn, l, dp, n, d, 'new251', 6)
      CALL CHKOK2(ne, 266, 4, d)
      CALL CHKIN2(ne, 266, 5, l, flen)
      CALL CHKIN2(ne, 266, 6, n, 0)
      CALL CHKDB2(ne, 266, 7, dp, 8.5d0)
      CALL CHKST2(ne, 266, 8, fn, 'in5')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new251', 6, 1)
      CALL CHKOK2(ne, 266, 9, d)
      CALL CHKST2(ne, 266, 10, str, "carray")
      CALL CHKIN2(ne, 266, 11, n, 2)

C     267: GDLSCR check
      CALL GDLSCR(d, 'new251', 6, 'in5', 3, dcmplx(12d0, 14d0), '', -1,
     +-1)
      CALL CHKOK2(ne, 267, 1, d)

      CALL GDENFL(n, d, 'new251', 6)
      CALL CHKOK2(ne, 267, 2, d)
      CALL CHKIN2(ne, 267, 3, n, GDE_CS + GDE_CA)

      l = flen
      CALL GDGECR(fn, l, dc, n, d, 'new251', 6)
      CALL CHKOK2(ne, 267, 4, d)
      CALL CHKIN2(ne, 267, 5, l, flen)
      CALL CHKIN2(ne, 267, 6, n, 0)
      CALL CHKCP2(ne, 267, 7, dc, dcmplx(12d0, 14d0))
      CALL CHKST2(ne, 267, 8, fn, 'in5')

      l = slen
      CALL GDGSCA(str, l, n, d, 'new251', 6, 1)
      CALL CHKOK2(ne, 267, 9, d)
      CALL CHKST2(ne, 267, 10, str, "")

C     268: GDLSWD check
      CALL GDLSWD(d, 'new253', 6, '', -1, 'in4', 3, GDW_LT,
     +0d0, 'carray', 6, 3)
      CALL CHKOK2(ne, 268, 1, d)

      l = flen
      i = flen
      CALL GDGEWD(fields(1), i, fields(2), l, m, j, dp, n, d, 'new253',
     +6)
      CALL CHKOK2(ne, 268, 2, d)
      CALL CHKIN2(ne, 268, 3, i, flen)
      CALL CHKIN2(ne, 268, 4, l, flen)
      CALL CHKIN2(ne, 268, 5, n, 0)
      CALL CHKIN2(ne, 268, 6, m, GDW_LT)
      CALL CHKST2(ne, 268, 7, fields(1), 'in2')
      CALL CHKST2(ne, 268, 8, fields(2), 'in4')
      CALL CHKDB2(ne, 268, 9, dp, 5.5d0)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new253', 6, 1)
      CALL CHKOK2(ne, 268, 10, d)
      CALL CHKST2(ne, 268, 11, str, "carray")
      CALL CHKIN2(ne, 268, 12, n, 3)

C     269: GDLSMX check
      CALL GDLSMX(d, 'new254', 6, 'in0', 3, '', -1, 0, '', 0, -1,
     +-1, '', -1, -1)
      CALL CHKOK2(ne, 269, 1, d)

      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'new254', 6)
      CALL CHKOK2(ne, 269, 2, d)
      CALL CHKIN2(ne, 269, 3, i, flen)
      CALL CHKIN2(ne, 269, 4, l, flen)
      CALL CHKIN2(ne, 269, 5, n, 0)
      CALL CHKIN2(ne, 269, 6, m, 5)
      CALL CHKST2(ne, 269, 7, fields(1), 'in0')
      CALL CHKST2(ne, 269, 8, fields(2), 'in2')
      CALL CHKIN2(ne, 269, 9, j, 5)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new254', 6, 1)
      CALL CHKOK2(ne, 269, 10, d)
      CALL CHKST2(ne, 269, 11, str, "carray")
      CALL CHKIN2(ne, 269, 12, n, 3)

      l = slen
      CALL GDGSCA(str, l, n, d, 'new254', 6, 2)
      CALL CHKOK2(ne, 269, 13, d)
      CALL CHKST2(ne, 269, 14, str, "")

C     271: GDENCS
      CALL GDENCS(n, GDE_SI);
      CALL CHKINT(ne, 271, n, GD_RW);

C     272: NULL return from gd_reference
      CALL GDOPEN(m, fildir//DIRSEP//'empty', 18, GD_RW + GD_CR + GD_EX)
      CALL CHKOK2(ne, 272, 1, m)

      CALL GDREFE(str, l, m, "", -1)
      CALL CHKOK2(ne, 272, 2, m)
      CALL CHKINT(ne, 272, l, 0)

      CALL GDDSCD(m)

C     275: GDSARX
      CALL GDSARX(n, d)
      CALL CHKEOK(ne, 275, d)
      CALL CHKINT(ne, 275, n, 5)

C     276: GDMSAX
      CALL GDMSAX(n, d, 'data', 4)
      CALL CHKEOK(ne, 276, d)
      CALL CHKINT(ne, 276, n, 6)

C     277: GDGESA check
      CALL GDGESA(l, n, d, 'sarray', 6)
      CALL CHKEOK(ne, 277, d)
      CALL CHKIN2(ne, 277, 1, l, 7)
      CALL CHKIN2(ne, 277, 2, n, 0)

C     279: GDGTSA check
      fields(1) = 'one'
      fields(2) = 'two'
      fields(3) = 'three'
      fields(4) = 'four'
      fields(5) = 'five'
      fields(6) = 'six'
      fields(7) = 'seven'
      DO 2790 i = 1, n
      l = flen
      CALL GDGTSA(fn, l, d, 'sarray', 6, i)
      CALL CHKOK2(ne, 279, i, d)
      CALL CHKIN2(ne, 279, i, l, flen)
      CALL CHKST2(ne, 279, i, fn, fields(i))
 2790 CONTINUE

C     282: GDPTSA check
      fields(1) = 'abc'
      fields(2) = 'def'
      fields(3) = 'ghi'
      fields(4) = 'jkl'
      fields(5) = 'mno'
      fields(6) = 'pqr'
      fields(7) = 'stu'
      DO 2820 i = 1, 7
      l = flen
      CALL GDPTSA(d, 'sarray', 6, i, fields(i), 3)
      CALL CHKOK2(ne, 282, i, d)
 2820 CONTINUE

      DO 2821 i = 1, n
      l = flen
      CALL GDGTSA(fn, l, d, 'sarray', 6, i)
      CALL CHKOK2(ne, 282, i + n, d)
      CALL CHKIN2(ne, 282, i, l, flen)
      CALL CHKST2(ne, 282, i, fn, fields(i))
 2821 CONTINUE

C     283: GDADSA check
      CALL GDADSA(d, 'new283', 6, 4, 0)
      CALL CHKOK2(ne, 283, 0, d)

      CALL GDARLN(n, d, 'new283', 6)
      CALL CHKOK2(ne, 283, 1, d)
      CALL CHKIN2(ne, 283, 0, n, 4)

      DO 2830 i = 1, 4
      l = flen
      CALL GDGTSA(fn, l, d, 'new283', 6, i)
      CALL CHKOK2(ne, 283, i + 1, d)
      CALL CHKIN2(ne, 283, i, l, flen)
      CALL CHKST2(ne, 283, i, fn, '')
 2830 CONTINUE

C     285: GDMDSA check
      CALL GDMDSA(d, 'data', 4, 'mnew285', 7, 3, 0)
      CALL CHKOK2(ne, 285, 0, d)

      CALL GDARLN(n, d, 'data/mnew285', 12)
      CALL CHKOK2(ne, 285, 1, d)
      CALL CHKIN2(ne, 285, 0, n, 3)

      DO 2850 i = 1, 3
      l = flen
      CALL GDGTSA(fn, l, d, 'data/mnew285', 12, i)
      CALL CHKOK2(ne, 285, i + n, d)
      CALL CHKIN2(ne, 285, i, l, flen)
      CALL CHKST2(ne, 285, i, fn, '')
 2850 CONTINUE

C     286: GDALSA check
      CALL GDALSA(d, 'new283', 6, 3)
      CALL CHKOK2(ne, 286, 0, d)

      CALL GDARLN(n, d, 'new283', 6)
      CALL CHKOK2(ne, 286, 1, d)
      CALL CHKIN2(ne, 286, 0, n, 3)

C     288: GDGEID check
      l = flen
      CALL GDGEID(fields(1), l, fields(2), l, n, d, 'indir', 5)
      CALL CHKEOK(ne, 288, d)
      CALL CHKIN2(ne, 288, 1, l, flen)
      CALL CHKIN2(ne, 288, 2, n, 0)
      CALL CHKST2(ne, 288, 3, fields(1), 'data')
      CALL CHKST2(ne, 288, 4, fields(2), 'carray')

C     289: GDADID check
      CALL GDADID(d, 'new289', 6, 'in1', 3, 'in2', 3, 0)
      CALL CHKOK2(ne, 289, 1, d)

      l = flen
      CALL GDGEID(fields(1), l, fields(2), l, n, d, 'new289', 6)
      CALL CHKOK2(ne, 289, 2, d)
      CALL CHKIN2(ne, 289, 1, l, flen)
      CALL CHKIN2(ne, 289, 2, n, 0)
      CALL CHKST2(ne, 289, 3, fields(1), 'in1')
      CALL CHKST2(ne, 289, 4, fields(2), 'in2')

C     290: GDMDID check
      CALL GDMDID(d, 'data', 4, 'new290', 6, 'in3', 3, 'in4', 3)
      CALL CHKOK2(ne, 290, 1, d)

      l = flen
      CALL GDGEID(fields(1), l, fields(2), l, n, d, 'data/new290', 11)
      CALL CHKOK2(ne, 290, 2, d)
      CALL CHKIN2(ne, 290, 1, l, flen)
      CALL CHKIN2(ne, 290, 2, n, 0)
      CALL CHKST2(ne, 290, 3, fields(1), 'in3')
      CALL CHKST2(ne, 290, 4, fields(2), 'in4')

C     291: GDALID check
      CALL GDALID(d, 'new289', 6, 'in6', 3, 'in4', 3)
      CALL CHKOK2(ne, 291, 1, d)

      l = flen
      CALL GDGEID(fields(1), l, fields(2), l, n, d, 'new289', 6)
      CALL CHKOK2(ne, 291, 2, d)
      CALL CHKIN2(ne, 291, 1, l, flen)
      CALL CHKIN2(ne, 291, 2, n, 0)
      CALL CHKST2(ne, 291, 3, fields(1), 'in6')
      CALL CHKST2(ne, 291, 4, fields(2), 'in4')

C     292: GDGESD check
      l = flen
      CALL GDGESD(fields(1), l, fields(2), l, n, d, 'sindir', 6)
      CALL CHKEOK(ne, 292, d)
      CALL CHKIN2(ne, 292, 1, l, flen)
      CALL CHKIN2(ne, 292, 2, n, 0)
      CALL CHKST2(ne, 292, 3, fields(1), 'data')
      CALL CHKST2(ne, 292, 4, fields(2), 'sarray')

C     293: GDADSD check
      CALL GDADSD(d, 'new293', 6, 'in1', 3, 'in2', 3, 0)
      CALL CHKOK2(ne, 293, 1, d)

      l = flen
      CALL GDGESD(fields(1), l, fields(2), l, n, d, 'new293', 6)
      CALL CHKOK2(ne, 293, 2, d)
      CALL CHKIN2(ne, 293, 1, l, flen)
      CALL CHKIN2(ne, 293, 2, n, 0)
      CALL CHKST2(ne, 293, 3, fields(1), 'in1')
      CALL CHKST2(ne, 293, 4, fields(2), 'in2')

C     294: GDMDSD check
      CALL GDMDSD(d, 'data', 4, 'new294', 6, 'in3', 3, 'in4', 3)
      CALL CHKOK2(ne, 294, 1, d)

      l = flen
      CALL GDGESD(fields(1), l, fields(2), l, n, d, 'data/new294', 11)
      CALL CHKOK2(ne, 294, 2, d)
      CALL CHKIN2(ne, 294, 1, l, flen)
      CALL CHKIN2(ne, 294, 2, n, 0)
      CALL CHKST2(ne, 294, 3, fields(1), 'in3')
      CALL CHKST2(ne, 294, 4, fields(2), 'in4')

C     295: GDALSD check
      CALL GDALSD(d, 'new293', 6, 'in6', 3, 'in4', 3)
      CALL CHKOK2(ne, 295, 1, d)

      l = flen
      CALL GDGESD(fields(1), l, fields(2), l, n, d, 'new293', 6)
      CALL CHKOK2(ne, 295, 2, d)
      CALL CHKIN2(ne, 295, 1, l, flen)
      CALL CHKIN2(ne, 295, 2, n, 0)
      CALL CHKST2(ne, 295, 3, fields(1), 'in6')
      CALL CHKST2(ne, 295, 4, fields(2), 'in4')

C     296: GDGSTD check
      l = flen
      CALL GDGSTD(fn, l, d, 'sindir', 6, 0, 0)
      CALL CHKEOK(ne, 296, d)
      CALL CHKINT(ne, 296, l, flen)
      CALL CHKSTR(ne, 296, fn, 'abc')

C     297: GDGSTP check
      CALL GDGSTP(n, fn, d, 'sindir', 6, 0, 0, 1, 0)
      CALL CHKEOK(ne, 297, d)
      CALL CHKINT(ne, 297, n, 8)

C     298: GDXSTP check
      DO 2980 i = 1, 8
      l = slen
      CALL GDXSTP(str, l, fn, i)
      CALL CHKIN2(ne, 298, i, l, slen)
      CALL CHKST2(ne, 298, i, str, 'abc')
 2980 CONTINUE

C     299: GDDSTP check
      CALL GDDSTP(fn)

C     302: GDINCA check
      CALL GDINCN(d, 'format2', 7, 0, 'NS', 2, GD_CR + GD_EX)
      CALL CHKEOK(ne, 302, d)

C     303: GDFRNS check (read)
      l = flen
      CALL GDFRNS(fields(1), l, d, 2, "", -1)
      CALL CHKEOK(ne, 303, d)
      CALL CHKIN2(ne, 303, 1, l, flen)
      CALL CHKST2(ne, 303, 2, fields(1), 'NS')

C     304: GDFRNS check (update)
      l = flen
      CALL GDFRNS(fields(1), l, d, 2, "NS2", 3)
      CALL CHKEOK(ne, 304, d)
      CALL CHKIN2(ne, 304, 1, l, flen)
      CALL CHKST2(ne, 304, 2, fields(1), 'NS2')

C     305: GDMATN, GDMATX, GDNMAT checks
      CALL GDMATX(m, d, "^lin", 4, 0, 0, 0)
      CALL CHKOK2(ne, 305, 0, d)
      CALL CHKINT(ne, 305, m, 7)

      CALL GDNMAT(n, d, "^lin", 4, 0, 0, 0)
      CALL CHKOK2(ne, 305, 1, d)
      CALL CHKINT(ne, 305, n, 2)

      CALL GDMATN(fn, l, d, "^lin", 4, 0, 0, 0, 1)
      CALL CHKOK2(ne, 305, 3, d)
      CALL CHKIN2(ne, 305, 4, l, flen)
      CALL CHKST2(ne, 305, 5, fn, "lincom")

      CALL GDMATN(fn, l, d, "^lin", 4, 0, 0, 0, 2)
      CALL CHKOK2(ne, 305, 6, d)
      CALL CHKIN2(ne, 305, 7, l, flen)
      CALL CHKST2(ne, 305, 8, fn, "linterp")













C     ===============================================================
C     Cleanup
      CALL GDDSCD(d)

      CALL SYSTEM ( 'rm -rf ' // fildir )

      IF (ne .GT. 0) THEN
        WRITE(*, 9000) ne
        CALL EXIT(1)
      ENDIF

 9000 FORMAT('ne = ', i8)

      STOP
      END
