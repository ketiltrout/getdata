C     Copyright (C) 2009-2012 D. V. Wiebe
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
 9001 FORMAT('e[', i3, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKINT(NE, T, N, V)
      INTEGER NE, T, N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9002) T, N, V
      ENDIF
 9002 FORMAT('n[', i3, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKER2(NE, T, I, D, V)
      INCLUDE "getdata.f"
      INTEGER NE, T, I, D, V, E
      CALL GDEROR(E, D)

      IF (E .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9006) I, T, E, V
      ENDIF
 9006 FORMAT('e(', i3, ')[', i3, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKIN2(NE, T, I, N, V)
      INTEGER NE, T, I, N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9007) I, T, N, V
      ENDIF
 9007 FORMAT('n(', i3, ')[', i3, '] = ', i4, ', expected ', i4)
      END SUBROUTINE

      SUBROUTINE CHKST2(NE, T, I, N, V)
      INTEGER NE, T, I
      CHARACTER*(*) N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9008) I, T, N, V
      ENDIF
 9008 FORMAT('s(', i3, ')[', i3, '] = "', a, '", expected "', a, '"')
      END SUBROUTINE

      SUBROUTINE CHKSTR(NE, T, N, V)
      INTEGER NE, T
      CHARACTER*(*) N, V

      IF (N .NE. V) THEN
        NE = NE + 1
        WRITE(*, 9009) T, N, V
      ENDIF
 9009 FORMAT('s[', i3, '] = "', a, '", expected "', a, '"')
      END SUBROUTINE

      SUBROUTINE CHKDB2(NE, T, I, N, V)
      INTEGER NE, T, I
      REAL*8 N, V

      IF (ABS(N - V) .GT. 1E-5) THEN
        NE = NE + 1
        WRITE(*, 9010) I, T, N, V
      ENDIF
 9010 FORMAT('d(', i3, ')[', i3, '] = ', d16.10, ', expected ', d16.10)
      END SUBROUTINE

      SUBROUTINE CHKCP2(NE, T, I, N, V)
      INTEGER NE, T, I
      COMPLEX*16 N, V

      IF (ABS(N - V) .GT. 1E-5) THEN
        NE = NE + 1
        WRITE(*, 9011) I, T, REAL(REAL(N)), REAL(AIMAG(N)),
     +REAL(REAL(V)), REAL(AIMAG(V))
      ENDIF
 9011 FORMAT('x(', i3, ')[', i3, '] = ', d16.10, ';', d16.10,
     +', expected ', d16.10, ';', d16.10)
      END SUBROUTINE

      SUBROUTINE CHKDBL(NE, T, N, V)
      INTEGER NE, T
      REAL*8 N, V

      IF (ABS(N - V) .GT. 1E-5) THEN
        NE = NE + 1
        WRITE(*, 9012) T, N, V
      ENDIF
 9012 FORMAT('d[', i3, '] = ', d16.10, ', expected ', d16.10)
      END SUBROUTINE

      SUBROUTINE CHKCPX(NE, T, N, V)
      INTEGER NE, T
      COMPLEX*16 N, V

      IF (ABS(N - V) .GT. 1E-5) THEN
        NE = NE + 1
        WRITE(*, 9013) T, REAL(REAL(N)), REAL(AIMAG(N)),
     +REAL(REAL(V)), REAL(AIMAG(V))
      ENDIF
 9013 FORMAT('x[', i3, '] = ', d16.10, ';', d16.10,
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
      PARAMETER (nfields = 17)
      INTEGER slen
      PARAMETER (slen = 26)
      INTEGER plen
      PARAMETER (plen = 4096)

      CHARACTER*26 strings(3)
      CHARACTER*11 fields(nfields + 8)
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

      fields(1) = 'INDEX  '
      fields(2) = 'alias  '
      fields(3) = 'bit    '
      fields(4) = 'carray '
      fields(5) = 'const  '
      fields(6) = 'data   '
      fields(7) = 'div    '
      fields(8) = 'lincom '
      fields(9) = 'linterp'
      fields(10) = 'mplex  '
      fields(11) = 'mult   '
      fields(12) = 'phase  '
      fields(13) = 'polynom'
      fields(14) = 'recip  '
      fields(15) = 'sbit   '
      fields(16) = 'string '
      fields(17) = 'window '

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
      WRITE(1, *) 'linterp LINTERP data /look/up/file'
      WRITE(1, *) 'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4
     + const const'
      WRITE(1, *) 'bit BIT data 3 4'
      WRITE(1, *) 'sbit SBIT data 5 6'
      WRITE(1, *) 'mplex MPLEX sbit data 1 10'
      WRITE(1, *) 'mult MULTIPLY data sbit'
      WRITE(1, *) 'phase PHASE data 11'
      WRITE(1, *) 'div DIVIDE mult bit'
      WRITE(1, *) 'recip RECIP div 6.5;4.3'
      WRITE(1, *) 'window WINDOW linterp mult LT 4.1'
      WRITE(1, *) '/ALIAS alias data'
      WRITE(1, *) 'string STRING "Zaphod Beeblebrox"'
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=frm2, STATUS='NEW')
      WRITE(1, *) 'const2 CONST INT8 -19'
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=dat, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=80,
     +STATUS='NEW')
      WRITE(1, REC=1) datdat
      CLOSE(1, STATUS='KEEP')

      ne = 0
C     0: GDEROR check
      CALL GDOPEN(d, "x", 1, GD_RO)
      CALL CHKERR(ne, 0, d, GD_EOP)

C     1: GDOPEN check
      CALL GDOPEN(d, fildir, 12, GD_RW)
      CALL CHKEOK(ne, 1, d)

C     2: GDGETD check
      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c)
      CALL CHKEOK(ne, 2, d)
      CALL CHKINT(ne, 2, n, 8)

      DO 20 i = 1, 8
      CALL CHKIN2(ne, 2, i, INT(c(i)), 40 + i)
   20 CONTINUE

C     3: GDGTCO check
      CALL GDGTCO(d, 'const', 5, GD_F32, fl)
      CALL CHKEOK(ne, 3, d)
      CALL CHKDBL(ne, 3, 1d0 * fl, 5.5d0)

C     4: GDFDNX check
      CALL GDFDNX(i, d)
      CALL CHKEOK(ne, 4, d)
      CALL CHKINT(ne, 4, i, 7)

C     5: GDMFNX check
      CALL GDMFNX(i, d, 'data', 4)
      CALL CHKEOK(ne, 5, d)
      CALL CHKINT(ne, 5, i, 6)

C     6: GDNFLD check
      CALL GDNFLD(n, d)
      CALL CHKEOK(ne, 6, d)
      CALL CHKINT(ne, 6, n, nfields)

C     7: This is a check of (one of many instances of) _GDF_FString
      l = 2
      CALL GDFLDN(fn, l, d, 1)
      CALL CHKEOK(ne, 7, d)
      CALL CHKINT(ne, 7, l, 5)

C     8: GDFLDN check
      DO 80 i = 1, n
      l = flen
      CALL GDFLDN(fn, l, d, i)
      CALL CHKOK2(ne, 8, i, d)
      CALL CHKIN2(ne, 8, i, l, flen)
      CALL CHKST2(ne, 8, i, fn, fields(i))
   80 CONTINUE

C     9: GDNMFD check
      CALL GDNMFD(n, d, 'data', 4)
      CALL CHKEOK(ne, 9, d)
      CALL CHKINT(ne, 9, n, 3)

C     10: GDMFDN check
      fields(1) = 'mstr'
      fields(2) = 'mconst'
      fields(3) = 'mlut'
      DO 100 i = 1, n
      l = flen
      CALL GDMFDN(fn, l, d, 'data', 4, i)
      CALL CHKOK2(ne, 10, i, d)
      CALL CHKIN2(ne, 10, i, l, flen)
      CALL CHKST2(ne, 10, i, fn, fields(i))
  100 CONTINUE

C     11: GDNFRM check
      CALL GDNFRM(n, d)
      CALL CHKEOK(ne, 11, d)
      CALL CHKINT(ne, 11, n, 10)

C     12: GDGSPF check
      CALL GDGSPF(n, d, 'data', 4)
      CALL CHKEOK(ne, 12, d)
      CALL CHKINT(ne, 12, n, 8)

C     13: GDPUTD check
      c(1) = 13
      c(2) = 14
      c(3) = 15
      c(4) = 16
      c(5) = 17
      c(6) = 18
      c(7) = 19
      c(8) = 20
      CALL GDPUTD(n, d, 'data', 4, 5, 1, 0, 4, GD_I8, c)
      CALL CHKEOK(ne, 13, d)
      CALL CHKINT(ne, 13, n, 4)

      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c)

      DO 130 i = 1, 8
      IF (i .EQ. 1 .OR. i .GT. 5) THEN
        n = 40 + i
      ELSE
        n = 11 + i
      ENDIF
      CALL CHKIN2(ne, 13, i, INT(c(i)), n)
  130 CONTINUE

C     14: GDESTR check
      CALL GDGETD(n, d, 'x', 1, 5, 0, 1, 0, GD_I8, c)
      CALL GDESTR(d, str, slen)
      CALL CHKSTR(ne, 14, str, 'Field not found: x')

C     15: GDENTY check
      CALL GDENTY(n, d, 'data', 4)
      CALL CHKEOK(ne, 15, d)
      CALL CHKINT(ne, 15, n, GD_RWE)

C     16: GDGERW check
      CALL GDGERW(l, i, n, d, 'data', 4)
      CALL CHKEOK(ne, 16, d)
      CALL CHKIN2(ne, 16, 1, n, 0)
      CALL CHKIN2(ne, 16, 2, l, 8)
      CALL CHKIN2(ne, 16, 3, i, GD_I8)

C     17: GDGELC check
      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'lincom', 6)
      CALL CHKEOK(ne, 17, d)
      CALL CHKIN2(ne, 17, 1, l, flen)
      CALL CHKIN2(ne, 17, 2, i, 3)
      CALL CHKIN2(ne, 17, 3, n, 0)
      CALL CHKST2(ne, 17, 4, fields(1), 'data')
      CALL CHKST2(ne, 17, 5, fields(2), 'INDEX')
      CALL CHKST2(ne, 17, 6, fields(3), 'linterp')

      q(1) = 1.1
      q(2) = 2.2
      q(3) = 2.2
      q(4) = 3.3
      q(5) = 5.5
      q(6) = 5.5
      DO 170 i=1,6
      CALL CHKDB2(ne, 17, i, p(i), q(i))
  170 CONTINUE

C     18: GDGECL check
      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'lincom', 6)
      CALL CHKEOK(ne, 18, d)
      CALL CHKIN2(ne, 18, 1, l, flen)
      CALL CHKIN2(ne, 18, 2, i, 3)
      CALL CHKIN2(ne, 18, 3, n, 0)
      CALL CHKST2(ne, 18, 4, fields(1), 'data')
      CALL CHKST2(ne, 18, 5, fields(2), 'INDEX')
      CALL CHKST2(ne, 18, 6, fields(3), 'linterp')

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      cq(5) = cmplx(5.5, 0.0)
      cq(6) = cmplx(5.5, 0.0)
      DO 180 i=1,6
      CALL CHKCP2(ne, 18, i, cp(i), cq(i))
  180 CONTINUE

C     19: GDGEPN check
      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'polynom', 7)
      CALL CHKEOK(ne, 19, d)
      CALL CHKIN2(ne, 19, 1, l, flen)
      CALL CHKIN2(ne, 19, 2, i, 5)
      CALL CHKIN2(ne, 19, 3, n, 0)
      CALL CHKST2(ne, 19, 4, fn, 'data')

      q(1) = 1.1
      q(2) = 2.2
      q(3) = 2.2
      q(4) = 3.3
      q(5) = 5.5
      q(6) = 5.5
      DO 190 i=1,6
      CALL CHKDB2(ne, 19, i, p(i), q(i))
  190 CONTINUE

C     20: GDGECP check
      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'polynom', 7)
      CALL CHKEOK(ne, 20, d)
      CALL CHKIN2(ne, 20, 1, l, flen)
      CALL CHKIN2(ne, 20, 2, i, 5)
      CALL CHKIN2(ne, 20, 3, n, 0)
      CALL CHKST2(ne, 20, 4, fn, 'data')

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      cq(5) = cmplx(5.5, 0.0)
      cq(6) = cmplx(5.5, 0.0)
      DO 200 i=1,6
      CALL CHKCP2(ne, 20, i, cp(i), cq(i))
  200 CONTINUE

C     21: GDGELT check
      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'linterp', 7)
      CALL CHKEOK(ne, 21, d)
      CALL CHKIN2(ne, 21, 1, l, flen)
      CALL CHKIN2(ne, 21, 2, n, 0)
      CALL CHKST2(ne, 21, 3, fn, 'data')
      CALL CHKST2(ne, 21, 4, str, '/look/up/file')

C     22: GDGEBT check
      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'bit', 3)
      CALL CHKEOK(ne, 22, d)
      CALL CHKIN2(ne, 22, 1, l, flen)
      CALL CHKIN2(ne, 22, 2, n, 0)
      CALL CHKIN2(ne, 22, 3, i, 4)
      CALL CHKIN2(ne, 22, 4, m, 3)
      CALL CHKST2(ne, 22, 5, fn, 'data')

C     23: GDGESB check
      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'sbit', 4)
      CALL CHKEOK(ne, 23, d)
      CALL CHKIN2(ne, 23, 1, l, flen)
      CALL CHKIN2(ne, 23, 2, n, 0)
      CALL CHKIN2(ne, 23, 3, i, 6)
      CALL CHKIN2(ne, 23, 4, m, 5)
      CALL CHKST2(ne, 23, 5, fn, 'data')

C     24: GDGEMT check
      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'mult', 4)
      CALL CHKEOK(ne, 24, d)
      CALL CHKIN2(ne, 24, 1, l, flen)
      CALL CHKIN2(ne, 24, 2, n, 0)
      CALL CHKST2(ne, 24, 3, fields(1), 'data')
      CALL CHKST2(ne, 24, 4, fields(2), 'sbit')

C     25: GDGEPH check
      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'phase', 5)
      CALL CHKEOK(ne, 25, d)
      CALL CHKIN2(ne, 25, 1, l, flen)
      CALL CHKIN2(ne, 25, 2, n, 0)
      CALL CHKIN2(ne, 25, 3, i, 11)
      CALL CHKST2(ne, 25, 4, fn, 'data')

C     26: GDGECO check
      CALL GDGECO(i, n, d, 'const', 5)
      CALL CHKEOK(ne, 26, d)
      CALL CHKIN2(ne, 26, 1, n, 0)
      CALL CHKIN2(ne, 26, 2, i, GD_F64)

C     27: GDFRGI check
      CALL GDFRGI(n, d, 'const', 5)
      CALL CHKEOK(ne, 27, d)
      CALL CHKINT(ne, 27, n, 0)

C     28: GDADRW check
      CALL GDADRW(d, 'new1', 4, GD_F64, 3, 0)
      CALL CHKOK2(ne, 28, 1, d)

      CALL GDGERW(l, i, n, d, 'new1', 4)
      CALL CHKOK2(ne, 28, 2, d)
      CALL CHKIN2(ne, 28, 3, n, 0)
      CALL CHKIN2(ne, 28, 4, l, 3)
      CALL CHKIN2(ne, 28, 5, i, GD_F64)

C     29: GDADLC check
      CALL GDADLC(d, 'new2', 4, 2, 'in1', 3, 9.9d0, 8.8d0, 'in2', 3,
     +7.7d0, 6.6d0, '', 0, 0d0, 0d0, 0)
      CALL CHKOK2(ne, 29, 1, d)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL CHKOK2(ne, 29, 2, d)
      CALL CHKIN2(ne, 29, 3, l, flen)
      CALL CHKIN2(ne, 29, 4, i, 2)
      CALL CHKIN2(ne, 29, 5, n, 0)
      CALL CHKST2(ne, 29, 6, fields(1), 'in1')
      CALL CHKST2(ne, 29, 7, fields(2), 'in2')

      q(1) = 9.9
      q(2) = 8.8
      q(3) = 7.7
      q(4) = 6.6
      q(5) = 5.5
      q(6) = 5.5
      DO 290 i=1,4
      CALL CHKDB2(ne, 29, i, p(i), q(i))
  290 CONTINUE

C     30: GDADCL check
      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      CALL GDADCL(d, 'new3', 4, 2, 'in1', 3, cq(1), cq(2), 'in2', 3,
     +cq(3), cq(4), '', 0, cq(5), cq(6), 0)
      CALL CHKOK2(ne, 30, 1, d)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new3', 4)
      CALL CHKOK2(ne, 30, 2, d)
      CALL CHKIN2(ne, 30, 1, l, flen)
      CALL CHKIN2(ne, 30, 2, i, 2)
      CALL CHKIN2(ne, 30, 3, n, 0)
      CALL CHKST2(ne, 30, 4, fields(1), 'in1')
      CALL CHKST2(ne, 30, 5, fields(2), 'in2')

      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      DO 300 i=1,4
      CALL CHKCP2(ne, 30, i, cp(i), cq(i))
  300 CONTINUE

C     31: GDADPN check
      CALL GDADPN(d, 'new4', 4, 3, 'in1', 3, 3d3, 4d4, 5d5, 6d6, 0d0,
     +0d0, 0)
      CALL CHKOK2(ne, 31, 1, d)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL CHKOK2(ne, 31, 2, d)
      CALL CHKIN2(ne, 31, 1, l, flen)
      CALL CHKIN2(ne, 31, 2, i, 3)
      CALL CHKIN2(ne, 31, 3, n, 0)
      CALL CHKST2(ne, 31, 4, fn, 'in1')

      q(1) = 3d3
      q(2) = 4d4
      q(3) = 5d5
      q(4) = 6d6
      q(5) = 5.5d0
      q(6) = 5.5d0

      DO 310 i=1,4
      CALL CHKDB2(ne, 31, i, p(i), q(i))
  310 CONTINUE

C     32: GDADCP check
      cq(1) = cmplx(3.1, 7.0)
      cq(2) = cmplx(4.2, 8.0)
      cq(3) = cmplx(5.2, 9.0)
      cq(4) = cmplx(6.3, 4.4)
      CALL GDADCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6), 0)
      CALL CHKOK2(ne, 32, 1, d)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new5', 4)
      CALL CHKOK2(ne, 32, 2, d)
      CALL CHKIN2(ne, 32, 1, l, flen)
      CALL CHKIN2(ne, 32, 2, i, 3)
      CALL CHKIN2(ne, 32, 3, n, 0)
      CALL CHKST2(ne, 32, 4, fn, 'in1')

      cq(1) = cmplx(3.1, 7.0)
      cq(2) = cmplx(4.2, 8.0)
      cq(3) = cmplx(5.2, 9.0)
      cq(4) = cmplx(6.3, 4.4)
      DO 320 i=1,4
      CALL CHKCP2(ne, 32, i, cp(i), cq(i))
  320 CONTINUE

C     33: GDADLT check
      CALL GDADLT(d, "new6", 4, "in", 2, "./some/table", 12, 0)
      CALL CHKOK2(ne, 33, 1, d)

      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'new6', 4)
      CALL CHKOK2(ne, 33, 2, d)
      CALL CHKIN2(ne, 33, 1, l, flen)
      CALL CHKIN2(ne, 33, 2, n, 0)
      CALL CHKST2(ne, 33, 3, fn, 'in')
      CALL CHKST2(ne, 33, 4, str, './some/table')

C     34: GDADBT check
      CALL GDADBT(d, "new7", 4, "in", 2, 13, 12, 0)
      CALL CHKOK2(ne, 34, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL CHKOK2(ne, 34, 2, d)
      CALL CHKIN2(ne, 34, 1, l, flen)
      CALL CHKIN2(ne, 34, 2, n, 0)
      CALL CHKIN2(ne, 34, 3, i, 12)
      CALL CHKIN2(ne, 34, 4, m, 13)
      CALL CHKST2(ne, 34, 5, fn, 'in')

C     35: GDADSB check
      CALL GDADSB(d, "new8", 4, "in", 2, 13, 12, 0)
      CALL CHKOK2(ne, 35, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL CHKOK2(ne, 35, 2, d)
      CALL CHKIN2(ne, 35, 1, l, flen)
      CALL CHKIN2(ne, 35, 2, n, 0)
      CALL CHKIN2(ne, 35, 3, i, 12)
      CALL CHKIN2(ne, 35, 4, m, 13)
      CALL CHKST2(ne, 35, 5, fn, 'in')

C     36: GDADMT check
      CALL GDADMT(d, 'new9', 4, 'in1', 3, 'in2', 3, 0)
      CALL CHKOK2(ne, 36, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKOK2(ne, 36, 2, d)
      CALL CHKIN2(ne, 36, 1, l, flen)
      CALL CHKIN2(ne, 36, 2, n, 0)
      CALL CHKST2(ne, 36, 3, fields(1), 'in1')
      CALL CHKST2(ne, 36, 4, fields(2), 'in2')

C     37: GDADPH check
      CALL GDADPH(d, 'new10', 5, 'in1', 3, 22, 0)
      CALL CHKOK2(ne, 37, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKOK2(ne, 37, 2, d)
      CALL CHKIN2(ne, 37, 1, l, flen)
      CALL CHKIN2(ne, 37, 2, n, 0)
      CALL CHKIN2(ne, 37, 3, i, 22)
      CALL CHKST2(ne, 37, 4, fn, 'in1')

C     38: GDADCO check
      CALL GDADCO(d, 'new11', 5, GD_F64, GD_F32, -8.1, 0)
      CALL CHKOK2(ne, 38, 1, d)

      CALL GDGECO(i, n, d, 'new11', 5)
      CALL CHKOK2(ne, 38, 2, d)
      CALL CHKIN2(ne, 38, 1, n, 0)
      CALL CHKIN2(ne, 38, 2, i, GD_F64)

      CALL GDGTCO(d, 'new11', 5, GD_F32, fl)
      CALL CHKOK2(ne, 38, 3, d)
      CALL CHKDBL(ne, 38, 1d0 * fl, -8.1d0)

C     39: GDFRGN check
      l = plen;
      CALL GDFRGN(path, l, d, 0)

      CALL CHKEOK(ne, 39, d)
      CALL CHKINT(ne, 39, l, plen)
      CALL CHKEOS(ne, 39, path, 'test_dirfile/format')

C     40: GDNFRG check
      CALL GDNFRG(n, d)
      CALL CHKEOK(ne, 40, d)
      CALL CHKINT(ne, 40, n, 1)

C     41: GDINCL check
      CALL GDINCL(d, 'form2', 5, 0, 0)
      CALL CHKOK2(ne, 41, 1, d)

      CALL GDGTCO(d, 'const2', 6, GD_I8, c(1))
      CALL CHKOK2(ne, 41, 2, d)
      CALL CHKINT(ne, 41, INT(c(1)), -19)

C     42: GDNFDT check
      CALL GDNFDT(n, d, GD_LCE)
      CALL CHKEOK(ne, 42, d)
      CALL CHKINT(ne, 42, n, 3)

C     43: GDFDNT check
      fields(1) = 'lincom'
      fields(2) = 'new2'
      fields(3) = 'new3'
      DO 430 i = 1, n
      l = flen
      CALL GDFDNT(fn, l, d, GD_LCE, i)
      CALL CHKOK2(ne, 43, i, d)
      CALL CHKIN2(ne, 43, i, l, flen)
      CALL CHKST2(ne, 43, i, fn, fields(i))
  430 CONTINUE

C     44: GDNVEC check
      CALL GDNVEC(n, d)
      CALL CHKEOK(ne, 44, d)
      CALL CHKINT(ne, 44, n, 24)

C     45: GDVECN check
      fields(1) = 'INDEX  '
      fields(2) = 'alias  '
      fields(3) = 'bit    '
      fields(4) = 'data   '
      fields(5) = 'div    '
      fields(6) = 'lincom '
      fields(7) = 'linterp'
      fields(8) = 'mplex  '
      fields(9) = 'mult   '
      fields(10) = 'new1   '
      fields(11) = 'new10  '
      fields(12) = 'new2   '
      fields(13) = 'new3   '
      fields(14) = 'new4   '
      fields(15) = 'new5   '
      fields(16) = 'new6   '
      fields(17) = 'new7   '
      fields(18) = 'new8   '
      fields(19) = 'new9   '
      fields(20) = 'phase  '
      fields(21) = 'polynom'
      fields(22) = 'recip  '
      fields(23) = 'sbit   '
      fields(24) = 'window '
      DO 450 i = 1, n
      l = flen
      CALL GDVECN(fn, l, d, i)
      CALL CHKOK2(ne, 45, i, d)
      CALL CHKIN2(ne, 45, i, l, flen)
      CALL CHKST2(ne, 45, i, fn, fields(i))
  450 CONTINUE

C     46: GDMDLC check
      CALL GDMDLC(d, 'data', 4, 'mnew1', 5, 2, 'in1', 3, 9.9d0, 8.8d0,
     +'in2', 3, 7.7d0, 6.6d0, '', 0, 0d0, 0d0)
      CALL CHKOK2(ne, 46, 1, d)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'data/mnew1', 10)
      CALL CHKOK2(ne, 46, 2, d)
      CALL CHKIN2(ne, 46, 3, l, flen)
      CALL CHKIN2(ne, 46, 4, i, 2)
      CALL CHKIN2(ne, 46, 5, n, 0)
      CALL CHKST2(ne, 46, 6, fields(1), 'in1')
      CALL CHKST2(ne, 46, 7, fields(2), 'in2')

      q(1) = 9.9
      q(2) = 8.8
      q(3) = 7.7
      q(4) = 6.6
      q(5) = 5.5
      q(6) = 5.5
      DO 460 i=1,4
      CALL CHKDB2(ne, 46, i, p(i), q(i))
  460 CONTINUE

C     47: GDMDCL check
      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      CALL GDMDCL(d, 'data', 4, 'mnew2', 5, 2, 'in1', 3, cq(1), cq(2),
     +'in2', 3, cq(3), cq(4), '', 0, cq(5), cq(6))
      CALL CHKOK2(ne, 47, 1, d)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'data/mnew2', 10)
      CALL CHKOK2(ne, 47, 2, d)
      CALL CHKIN2(ne, 47, 1, l, flen)
      CALL CHKIN2(ne, 47, 2, i, 2)
      CALL CHKIN2(ne, 47, 3, n, 0)
      CALL CHKST2(ne, 47, 4, fields(1), 'in1')
      CALL CHKST2(ne, 47, 5, fields(2), 'in2')

      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      DO 470 i=1,4
      CALL CHKCP2(ne, 47, i, cp(i), cq(i))
  470 CONTINUE

C     48: GDMDPN check
      CALL GDMDPN(d, 'data', 4, 'mnew3', 5, 3, 'in1', 3, 3d3, 4d4, 5d5,
     +6d6, 0d0, 0d0)
      CALL CHKOK2(ne, 48, 1, d)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'data/mnew3', 10)
      CALL CHKOK2(ne, 48, 2, d)
      CALL CHKIN2(ne, 48, 1, l, flen)
      CALL CHKIN2(ne, 48, 2, i, 3)
      CALL CHKIN2(ne, 48, 3, n, 0)
      CALL CHKST2(ne, 48, 4, fn, 'in1')

      q(1) = 3d3
      q(2) = 4d4
      q(3) = 5d5
      q(4) = 6d6
      q(5) = 5.5d0
      q(6) = 5.5d0
      DO 480 i=1,4
      CALL CHKDB2(ne, 48, i, p(i), q(i))
  480 CONTINUE

C     49: GDMDCP check
      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      CALL GDMDCP(d, 'data', 4, 'mnew5', 5, 3, 'in1', 3, cq(1), cq(2),
     +cq(3), cq(4), cq(5), cq(6))
      CALL CHKOK2(ne, 49, 1, d)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'data/mnew5', 10)
      CALL CHKOK2(ne, 49, 2, d)
      CALL CHKIN2(ne, 49, 1, l, flen)
      CALL CHKIN2(ne, 49, 2, i, 3)
      CALL CHKIN2(ne, 49, 3, n, 0)
      CALL CHKST2(ne, 49, 4, fn, 'in1')

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      DO 490 i=1,4
      CALL CHKCP2(ne, 49, i, cp(i), cq(i))
  490 CONTINUE

C     50: GDMDLT check
      CALL GDMDLT(d, "data", 4, "mnew6", 5, "in", 2, "./more/table", 12)
      CALL CHKOK2(ne, 50, 1, d)

      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'data/mnew6', 10)
      CALL CHKOK2(ne, 50, 2, d)
      CALL CHKIN2(ne, 50, 1, l, flen)
      CALL CHKIN2(ne, 50, 2, n, 0)
      CALL CHKST2(ne, 50, 3, fn, 'in')
      CALL CHKST2(ne, 50, 4, str, './more/table')

C     51: GDMDBT check
      CALL GDMDBT(d, "data", 4, "mnew7", 5, "in", 2, 13, 12)
      CALL CHKOK2(ne, 51, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'data/mnew7', 10)
      CALL CHKOK2(ne, 51, 2, d)
      CALL CHKIN2(ne, 51, 1, l, flen)
      CALL CHKIN2(ne, 51, 2, n, 0)
      CALL CHKIN2(ne, 51, 3, i, 12)
      CALL CHKIN2(ne, 51, 4, m, 13)
      CALL CHKST2(ne, 51, 5, fn, 'in')

C     52: GDMDSB check
      CALL GDMDSB(d, "data", 4, "mnew8", 5, "in", 2, 13, 12)
      CALL CHKOK2(ne, 52, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'data/mnew8', 10)
      CALL CHKOK2(ne, 52, 2, d)
      CALL CHKIN2(ne, 52, 1, l, flen)
      CALL CHKIN2(ne, 52, 2, n, 0)
      CALL CHKIN2(ne, 52, 3, i, 12)
      CALL CHKIN2(ne, 52, 4, m, 13)
      CALL CHKST2(ne, 52, 5, fn, 'in')

C     53: GDMDMT check
      CALL GDMDMT(d, 'data', 4, 'mnew9', 5, 'in1', 3, 'in2', 3)
      CALL CHKOK2(ne, 53, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'data/mnew9', 10)
      CALL CHKOK2(ne, 53, 2, d)
      CALL CHKIN2(ne, 53, 1, l, flen)
      CALL CHKIN2(ne, 53, 2, n, 0)
      CALL CHKST2(ne, 53, 3, fields(1), 'in1')
      CALL CHKST2(ne, 53, 4, fields(2), 'in2')

C     54: GDMDPH check
      CALL GDMDPH(d, 'data', 4, 'mnew10', 6, 'in1', 3, 22)
      CALL CHKOK2(ne, 54, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'data/mnew10', 11)
      CALL CHKOK2(ne, 54, 2, d)
      CALL CHKIN2(ne, 54, 1, l, flen)
      CALL CHKIN2(ne, 54, 2, n, 0)
      CALL CHKIN2(ne, 54, 3, i, 22)
      CALL CHKST2(ne, 54, 4, fn, 'in1')

C     55: GDMDCO check
      CALL GDMDCO(d, 'data', 4, 'mnew11', 6, GD_F64, GD_F32, -8.1)
      CALL CHKOK2(ne, 55, 1, d)

      CALL GDGECO(i, n, d, 'data/mnew11', 11)
      CALL CHKOK2(ne, 55, 2, d)
      CALL CHKIN2(ne, 55, 1, l, flen)
      CALL CHKIN2(ne, 55, 2, n, 0)
      CALL CHKIN2(ne, 55, 3, i, GD_F64)

      CALL GDGTCO(d, 'data/mnew11', 11, GD_F32, fl)
      CALL CHKOK2(ne, 55, 3, d)
      CALL CHKDBL(ne, 55, 1d0 * fl, -8.1d0)

C     56: GDGTST check
      CALL GDGTST(n, d, 'string', 6, slen, str)
      CALL CHKEOK(ne, 56, d)
      CALL CHKINT(ne, 56, n, 17)
      CALL CHKSTR(ne, 56, str, "Zaphod Beeblebrox")

C     57: GDADST check
      CALL GDADST(d, 'new12', 5, "---string---", 12, 0)
      CALL CHKOK2(ne, 57, 1, d)

      CALL GDGTST(n, d, 'new12', 5, slen, str)
      CALL CHKOK2(ne, 57, 2, d)
      CALL CHKSTR(ne, 57, str, "---string---")

C     58: GDMDST check
      CALL GDMDST(d, "data", 4, 'mnew12', 6, "kai su, technon;", 16)
      CALL CHKOK2(ne, 58, 1, d)

      CALL GDGTST(n, d, 'data/mnew12', 11, slen, str)
      CALL CHKOK2(ne, 58, 2, d)
      CALL CHKSTR(ne, 58, str, "kai su, technon;")

C     59: GDADSP check
      CALL GDADSP(d, 'lorem STRING "Lorem ipsum"', 26, 0)
      CALL CHKOK2(ne, 59, 1, d)

      CALL GDGTST(n, d, 'lorem', 5, slen, str)
      CALL CHKOK2(ne, 59, 2, d)
      CALL CHKSTR(ne, 59, str, "Lorem ipsum")

C     60: GDMDSP check
      CALL GDMDSP(d, 'ipsum STRING "dolor sit amet."', 30, 'lorem', 5)
      CALL CHKOK2(ne, 60, 1, d)

      CALL GDGTST(n, d, 'lorem/ipsum', 11, slen, str)
      CALL CHKOK2(ne, 60, 2, d)
      CALL CHKSTR(ne, 60, str, "dolor sit amet.")

C     61: GDPTCO check
      CALL GDPTCO(d, 'const', 5, GD_I32, 10)
      CALL CHKOK2(ne, 61, 1, d)

      CALL GDGTCO(d, 'const', 5, GD_F32, fl)
      CALL CHKOK2(ne, 61, 2, d)
      CALL CHKDBL(ne, 61, 1d0 * fl, 10.0d0)

C     62: GDPTST check
      CALL GDPTST(n, d, 'string', 6, 11, "Arthur Dent")
      CALL CHKOK2(ne, 62, 1, d)
      CALL CHKINT(ne, 62, n, 11)

      CALL GDGTST(n, d, 'string', 6, slen, str)
      CALL CHKOK2(ne, 62, 2, d)
      CALL CHKSTR(ne, 62, str, "Arthur Dent")

C     63: GDNMFT check
      CALL GDNMFT(n, d, "data", 4, GD_LCE)
      CALL CHKEOK(ne, 63, d)
      CALL CHKINT(ne, 63, n, 2)

C     64: GDMFDT check
      fields(1) = 'mnew1'
      fields(2) = 'mnew2'
      DO 640 i = 1, n
      l = flen
      CALL GDMFDT(fn, l, d, "data", 4, GD_LCE, i)
      CALL CHKOK2(ne, 64, i, d)
      CALL CHKIN2(ne, 64, i, l, flen)
      CALL CHKST2(ne, 64, i, fn, fields(i))
  640 CONTINUE

C     65: GDNMVE check
      CALL GDNMVE(n, d, "data", 4)
      CALL CHKEOK(ne, 65, d)
      CALL CHKINT(ne, 65, n, 10)

C     66: GDMVEN check
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
      DO 660 i = 1, n
      l = flen
      CALL GDMVEN(fn, l, d, "data", 4, i)
      CALL CHKOK2(ne, 66, i, d)
      CALL CHKIN2(ne, 66, i, l, flen)
      CALL CHKST2(ne, 66, i, fn, fields(i))
  660 CONTINUE

C     67: GDALRW check
      CALL GDALRW(d, 'new1', 4, GD_I32, 4, 0)
      CALL CHKOK2(ne, 67, 1, d)

      CALL GDGERW(l, i, n, d, 'new1', 4)
      CALL CHKOK2(ne, 67, 2, d)
      CALL CHKIN2(ne, 67, 3, n, 0)
      CALL CHKIN2(ne, 67, 4, l, 4)
      CALL CHKIN2(ne, 67, 5, i, GD_I32)

C     68: GDALLC check
      CALL GDALLC(d, 'new2', 4, 3, 'in4', 3, 9.9d-1, 7.8d0, 'in5', 3,
     +1.1d1, 2.2d-2, 'in6', 3, 1.96d0, 0d0)
      CALL CHKOK2(ne, 68, 1, d)

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL CHKOK2(ne, 68, 2, d)
      CALL CHKIN2(ne, 68, 3, l, flen)
      CALL CHKIN2(ne, 68, 4, i, 3)
      CALL CHKIN2(ne, 68, 5, n, 0)
      CALL CHKST2(ne, 68, 6, fields(1), 'in4')
      CALL CHKST2(ne, 68, 7, fields(2), 'in5')
      CALL CHKST2(ne, 68, 8, fields(3), 'in6')

      q(1) = 9.9d-1
      q(2) = 7.8d0
      q(3) = 1.1d1
      q(4) = 2.2d-2
      q(5) = 1.96d0
      q(6) = 0d0
      DO 680 i=1,6
      CALL CHKDB2(ne, 68, i, p(i), q(i))
  680 CONTINUE

C     69: GDALCL check
      cq(1) = cmplx(0.1, 0.2)
      cq(2) = cmplx(0.3, 0.4)
      cq(3) = cmplx(0.4, 0.5)
      cq(4) = cmplx(0.6, 0.7)
      CALL GDALCL(d, 'new3', 4, 2, 'in4', 3, cq(1), cq(2), 'in3', 3,
     +cq(3), cq(4), '', 0, cq(5), cq(6))
      CALL CHKOK2(ne, 69, 1, d)

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new3', 4)
      CALL CHKOK2(ne, 69, 2, d)
      CALL CHKIN2(ne, 69, 1, l, flen)
      CALL CHKIN2(ne, 69, 2, i, 2)
      CALL CHKIN2(ne, 69, 3, n, 0)
      CALL CHKST2(ne, 69, 4, fields(1), 'in4')
      CALL CHKST2(ne, 69, 5, fields(2), 'in3')

      cq(1) = cmplx(0.1, 0.2)
      cq(2) = cmplx(0.3, 0.4)
      cq(3) = cmplx(0.4, 0.5)
      cq(4) = cmplx(0.6, 0.7)
      DO 690 i=1,4
      CALL CHKCP2(ne, 69, i, cp(i), cq(i))
  690 CONTINUE

C     70: GDALPN check
      CALL GDALPN(d, 'new4', 4, 4, 'in1', 3, 3d0, 4d0, 5d0, 6d0, 7d0,
     +0d0)
      CALL CHKOK2(ne, 70, 1, d)

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL CHKOK2(ne, 70, 2, d)
      CALL CHKIN2(ne, 70, 1, l, flen)
      CALL CHKIN2(ne, 70, 2, i, 4)
      CALL CHKIN2(ne, 70, 3, n, 0)
      CALL CHKST2(ne, 70, 4, fn, 'in1')

      DO 700 i=1,5
      CALL CHKDB2(ne, 70, i, p(i), 2d0 + i)
  700 CONTINUE

C     71: GDALCP check
      cq(1) = cmplx(1.1, 5.0)
      cq(2) = cmplx(1.2, 4.0)
      cq(3) = cmplx(1.2, 3.0)
      cq(4) = cmplx(1.3, 2.4)
      CALL GDALCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6))
      CALL CHKOK2(ne, 71, 1, d)

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new5', 4)
      CALL CHKOK2(ne, 71, 2, d)
      CALL CHKIN2(ne, 71, 1, l, flen)
      CALL CHKIN2(ne, 71, 2, i, 3)
      CALL CHKIN2(ne, 71, 3, n, 0)
      CALL CHKST2(ne, 71, 4, fn, 'in1')

      cq(1) = cmplx(1.1, 5.0)
      cq(2) = cmplx(1.2, 4.0)
      cq(3) = cmplx(1.2, 3.0)
      cq(4) = cmplx(1.3, 2.4)
      DO 710 i=1,4
      CALL CHKCP2(ne, 71, i, cp(i), cq(i))
  710 CONTINUE

C     72: GDALLT check
      CALL GDALLT(d, "new6", 4, "in3", 3, "./other/table", 13, 0)
      CALL CHKOK2(ne, 72, 1, d)

      l = flen
      CALL GDGELT(fn, l, str, slen, n, d, 'new6', 4)
      CALL CHKOK2(ne, 72, 2, d)
      CALL CHKIN2(ne, 72, 1, l, flen)
      CALL CHKIN2(ne, 72, 2, n, 0)
      CALL CHKST2(ne, 72, 3, fn, 'in3')
      CALL CHKST2(ne, 73, 4, str, './other/table')

C     73: GDALBT check
      CALL GDALBT(d, "new7", 4, "in3", 3, 3, 2)
      CALL CHKOK2(ne, 73, 1, d)

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL CHKOK2(ne, 73, 2, d)
      CALL CHKIN2(ne, 73, 1, l, flen)
      CALL CHKIN2(ne, 73, 2, n, 0)
      CALL CHKIN2(ne, 73, 3, i, 2)
      CALL CHKIN2(ne, 73, 4, m, 3)
      CALL CHKST2(ne, 73, 5, fn, 'in3')

C     74: GDALSB check
      CALL GDALSB(d, "new8", 4, "out", 3, 1, 22)
      CALL CHKOK2(ne, 74, 1, d)

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL CHKOK2(ne, 74, 2, d)
      CALL CHKIN2(ne, 74, 1, l, flen)
      CALL CHKIN2(ne, 74, 2, n, 0)
      CALL CHKIN2(ne, 74, 3, i, 22)
      CALL CHKIN2(ne, 74, 4, m, 1)
      CALL CHKST2(ne, 74, 5, fn, 'out')

C     75: GDALMT check
      CALL GDALMT(d, 'new9', 4, 'in6', 3, 'in4', 3)
      CALL CHKOK2(ne, 75, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKOK2(ne, 75, 2, d)
      CALL CHKIN2(ne, 75, 1, l, flen)
      CALL CHKIN2(ne, 75, 2, n, 0)
      CALL CHKST2(ne, 75, 3, fields(1), 'in6')
      CALL CHKST2(ne, 75, 4, fields(2), 'in4')

C     76: GDALPH check
      CALL GDALPH(d, 'new10', 5, 'in2', 3, 8)
      CALL CHKOK2(ne, 76, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKOK2(ne, 76, 2, d)
      CALL CHKIN2(ne, 76, 1, l, flen)
      CALL CHKIN2(ne, 76, 2, n, 0)
      CALL CHKIN2(ne, 76, 3, i, 8)
      CALL CHKST2(ne, 76, 4, fn, 'in2')

C     77: GDALCO check
      CALL GDALCO(d, 'new11', 5, GD_F32)
      CALL CHKOK2(ne, 77, 2, d)

      CALL GDGECO(i, n, d, 'new11', 5)
      CALL CHKOK2(ne, 77, 2, d)
      CALL CHKIN2(ne, 77, 1, l, flen)
      CALL CHKIN2(ne, 77, 2, n, 0)
      CALL CHKIN2(ne, 77, 3, i, GD_F32)

      CALL GDGTCO(d, 'new11', 5, GD_F32, fl)
      CALL CHKOK2(ne, 77, 3, d)
      CALL CHKDBL(ne, 77, 1d0 * fl, -8.1d0)

C     78: GDGENC check
      CALL GDGENC(n, d, 0)
      CALL CHKEOK(ne, 78, d)
      CALL CHKINT(ne, 78, n, GD_EN)

C     79: GDGEND check
      CALL GDGEND(n, d, 0)
      CALL CHKEOK(ne, 79, d)
      CALL CHKINT(ne, 79, n, GD_LE + GD_NA)

C     80: GDNAME check
      l = slen
      CALL GDNAME(str, l, d, 0)
      CALL CHKEOK(ne, 80, d)
      CALL CHKINT(ne, 80, l, slen)
      CALL CHKSTR(ne, 80, str, 'test_dirfile')

C     81: GDPFRG check
      CALL GDPFRG(n, d, 1)
      CALL CHKEOK(ne, 81, d)
      CALL CHKINT(ne, 81, n, 0)

C     82: GDAPRT check
      CALL GDAPRT(d, GDPR_D, 1)
      CALL CHKEOK(ne, 82, d)

C     83: GDGPRT check
      CALL GDGPRT(n, d, 1)
      CALL CHKEOK(ne, 83, d)
      CALL CHKINT(ne, 83, n, GDPR_D)

C     84: GDRWFN check
      l = plen
      CALL GDRWFN(path, l, d, "data", 4)
      CALL CHKEOK(ne, 84, d)
      CALL CHKINT(ne, 84, l, plen)
      CALL CHKEOS(ne, 84, path, 'test_dirfile/data')

C     85: GDREFE check
      l = slen
      CALL GDREFE(str, l, d, "new1", 4)
      CALL CHKEOK(ne, 85, d)
      CALL CHKINT(ne, 85, l, slen)
      CALL CHKSTR(ne, 85, str, 'new1')

C     87: GDAENC check
      CALL GDAENC(d, GD_ES, 1, 0)
      CALL CHKOK2(ne, 87, 1, d)

      CALL GDGENC(n, d, 1)
      CALL CHKOK2(ne, 87, 2, d)
      CALL CHKINT(ne, 87, n, GD_ES)

C     88: GDAEND check
      CALL GDAEND(d, GD_BE, 1, 0)
      CALL CHKOK2(ne, 88, 1, d)

      CALL GDGEND(n, d, 1)
      CALL CHKOK2(ne, 88, 2, d)
      CALL CHKINT(ne, 88, n, GD_BE)

C     89: GDALSP check
      CALL GDALSP(d, 'new10 PHASE in1 3', 17, 0)
      CALL CHKOK2(ne, 89, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKOK2(ne, 89, 2, d)
      CALL CHKIN2(ne, 89, 1, l, flen)
      CALL CHKIN2(ne, 89, 2, n, 0)
      CALL CHKIN2(ne, 89, 3, i, 3)
      CALL CHKST2(ne, 89, 4, fn, 'in1')

C     90: GDDELE check
      CALL GDDELE(d, 'new10', 5, 0)
      CALL CHKOK2(ne, 90, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL CHKER2(ne, 90, 2, d, GD_EBC)

C     91: GDMLSP check
      CALL GDMLSP(d, 'mnew10 PHASE in4 11', 19, 'data', 4, 0)
      CALL CHKOK2(ne, 91, 1, d)

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'data/mnew10', 11)
      CALL CHKOK2(ne, 91, 2, d)
      CALL CHKIN2(ne, 91, 1, l, flen)
      CALL CHKIN2(ne, 91, 2, n, 0)
      CALL CHKIN2(ne, 91, 3, i, 11)
      CALL CHKST2(ne, 91, 4, fn, 'in4')

C     92: GDMOVE check
      CALL GDMOVE(d, 'new9', 4, 1, 0)
      CALL CHKOK2(ne, 92, 1, d)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKOK2(ne, 92, 2, d)
      CALL CHKIN2(ne, 92, 1, l, flen)
      CALL CHKIN2(ne, 92, 2, n, 1)
      CALL CHKST2(ne, 92, 3, fields(1), 'in6')
      CALL CHKST2(ne, 92, 4, fields(2), 'in4')

C     93: GDRENM check
      CALL GDRENM(d, 'new9', 4, 'newer', 5, 0)
      CALL CHKOK2(ne, 93, 1, d)

      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL CHKER2(ne, 93, 2, d, GD_EBC)

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'newer', 5)
      CALL CHKOK2(ne, 93, 3, d)
      CALL CHKIN2(ne, 93, 1, l, flen)
      CALL CHKIN2(ne, 93, 2, n, 1)
      CALL CHKST2(ne, 93, 3, fields(1), 'in6')
      CALL CHKST2(ne, 93, 4, fields(2), 'in4')

C     94: GDUINC check
      CALL GDUINC(d, 1, 0)
      CALL CHKOK2(ne, 94, 1, d)

      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'newer', 5)
      CALL CHKER2(ne, 94, 2, d, GD_EBC)

C     95: GDGFOF check
      CALL GDGFOF(n, d, 0)
      CALL CHKEOK(ne, 95, d)
      CALL CHKINT(ne, 95, n, 0)

C     96: GDAFOF check
      CALL GDAFOF(d, 33, 0, 0)
      CALL CHKOK2(ne, 96, 1, d)

      CALL GDGFOF(n, d, 0)
      CALL CHKOK2(ne, 96, 2, d)
      CALL CHKINT(ne, 96, n, 33)

C     97: GDNTYP check
      CALL GDNTYP(n, d, 'data', 4)
      CALL CHKEOK(ne, 97, d)
      CALL CHKINT(ne, 97, n, GD_I8)

C     98: GDCSCL check
      CALL GDCSCL(n, d, 'polynom', 7)
      CALL CHKEOK(ne, 98, d)
      CALL CHKINT(ne, 98, n, 1)

C     99: GDVLDT check
      CALL GDVLDT(n, d, 'new7', 4)
      CALL CHKERR(ne, 99, d, GD_EBC)
      CALL CHKINT(ne, 99, n, -1)

C     100: GDFNUM check
      l = slen
      CALL GDREFE(str, l, d, "data", 4)
      CALL GDFNUM(dp, d, 'INDEX', 5, 33.3d0)
      CALL CHKEOK(ne, 100, d)
      CALL CHKDBL(ne, 100, dp, 33.3D0)

C     101: GDFNSS check
      CALL GDFNSS(dp, d, 'data', 4, 33.3d0, 6, 0)
      CALL CHKEOK(ne, 101, d)
      CALL CHKDBL(ne, 101, dp, 37.0375D0)

C     138: GDGSCA check
      l = slen
      CALL GDGSCA(str, l, n, d, 'lincom', 6, 6)
      CALL CHKEOK(ne, 138, d)
      CALL CHKINT(ne, 138, n, -1)
      CALL CHKSTR(ne, 138, str, "const")

C     139: GDASCA check
      CALL GDASCA(d, 'lincom', 6, 6, 'new11', 5, -1, 0)
      CALL CHKOK2(ne, 139, 1, d)

      l = slen
      CALL GDGSCA(str, l, n, d, 'lincom', 6, 6)
      CALL CHKOK2(ne, 139, 2, d)
      CALL CHKINT(ne, 139, n, -1)
      CALL CHKSTR(ne, 139, str, "new11")

C     86: GDGEOF check
      CALL GDGEOF(n, d, 'lincom', 6)
      CALL CHKEOK(ne, 86, d)
      CALL CHKINT(ne, 86, n, 344)

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
      CALL CHKCP2(ne, 145, 4, dc, dcmplx(6.5, 4.3))

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
      cp(1) = cmplx(31.9, 38.2)
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
      cp(1) = cmplx(8.47, 6.22)
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
      cp(1) = cmplx(4.3, 81.81)
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
      CALL CHKINT(ne, 157, n, 9)

      n = 0
      CALL GDSTDV(n, d)
      CALL CHKER2(ne, 157, 2, d, GD_EVR)

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
      p(1) = 9.6
      p(2) = 8.5
      p(3) = 7.4
      p(4) = 6.3
      p(5) = 5.2
      p(6) = 4.1
      CALL GDPTCA(d, 'carray', 6, GD_F64, p)
      CALL CHKOK2(ne, 168, 1, d)

      CALL GDGTCA(d, 'carray', 6, GD_F64, q)
      CALL CHKOK2(ne, 168, 2, d)

      DO 1680 i=1,6
      CALL CHKDB2(ne, 168, i, q(i), 10.7d0 - 1.1d0 * i)
 1680 CONTINUE

C     169: GDGCAS check
      p(1) = 5.5
      p(2) = 5.6
      p(3) = 5.7
      p(4) = 5.8
      p(5) = 5.9
      p(6) = 6.0
      CALL GDPCAS(d, 'carray', 6, 3, 2, GD_F64, p)
      CALL CHKOK2(ne, 169, 1, d)

      CALL GDGTCA(d, 'carray', 6, GD_F64, q)
      CALL CHKOK2(ne, 169, 2, d)

      DO 1690 i=1,6
      IF (i .eq. 3 .or. i .eq. 4) THEN
        dp = 5.2 + 0.1 * i
      ELSE
        dp = 10.7 - 1.1 * i
      ENDIF
      CALL CHKDB2(ne, 169, i, q(i), dp)
 1690 CONTINUE

C     177: GDCALN check
      CALL GDCALN(n, d, 'carray', 6)
      CALL CHKEOK(ne, 177, d)
      CALL CHKINT(ne, 177, n, 6)

C     178: GDGECA check
      CALL GDGECA(i, l, n, d, 'carray', 6)
      CALL CHKEOK(ne, 178, d)
      CALL CHKIN2(ne, 178, 1, l, 6)
      CALL CHKIN2(ne, 178, 2, n, 0)
      CALL CHKIN2(ne, 178, 3, i, GD_F64)

C     179: GDADCA check
      p(1) = 1.2
      p(2) = 3.4
      p(3) = 5.6
      p(4) = 7.8
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
      p(1) = 3.2
      p(2) = 5.4
      p(3) = 7.6
      p(4) = 9.8
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
      p(1) = 10.0
      p(2) = -8.1
      CALL GDNFDT(n, d, GD_COE)

      DO 1830 i = 1, n
      l = flen
      CALL GDCONS(fl, d, GD_F32, i)
      CALL CHKOK2(ne, 183, i, d)
      CALL CHKDB2(ne, 183, i, 1d0 * fl, p(i))
 1830 CONTINUE

C     191: GDMCOS check
      p(1) = 3.3
      p(2) = -8.1
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
     +6)
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
     +6)
      CALL CHKOK2(ne, 217, 2, d)
      CALL CHKIN2(ne, 217, 1, i, flen)
      CALL CHKIN2(ne, 217, 2, l, flen)
      CALL CHKIN2(ne, 217, 3, n, 0)
      CALL CHKIN2(ne, 217, 4, m, GDW_GE)
      CALL CHKST2(ne, 217, 5, fields(1), 'in3')
      CALL CHKST2(ne, 217, 6, fields(2), 'in4')
      CALL CHKIN2(ne, 217, 7, dp, 32d3)

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
      fields(3) = 'data/mnew20'
      fields(4) = 'new20'
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

C     224: GDMOVA check
      CALL GDMOVA(d, 'new20', 5, 1)
      CALL CHKOK2(ne, 224, 1, d)

      CALL GDFRGI(n, d, 'Anew20Z', 7)
      CALL CHKOK2(ne, 224, 2, d)
      CALL CHKINT(ne, 224, n, 1)

C     225: GDDELA check
      CALL GDDELA(d, 'Anew20Z', 7, 0)
      CALL CHKOK2(ne, 225, 1, d)

      CALL GDFRGI(n, d, 'Anew20Z', 7)
      CALL CHKER2(ne, 225, 2, d, GD_EBC)
      CALL CHKINT(ne, 225, n, -1)

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
      CALL CHKDB2(ne, 228, 7, j, 10)

C     229: GDADMX check
      CALL GDADMX(d, 'new21', 5, 'in1', 3, 'in2', 3, 5, 6, 0)
      CALL CHKOK2(ne, 229, 1, d)

      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'new21', 6)
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
      CALL GDALMX(d, 'new21', 5, 'in3', 3, 'in4', 3, GD_CMX, 7)
      CALL CHKOK2(ne, 231, 1, d)

      l = flen
      i = flen
      CALL GDGEMX(fields(1), i, fields(2), l, m, j, n, d, 'new21', 6)
      CALL CHKOK2(ne, 231, 2, d)
      CALL CHKIN2(ne, 231, 1, i, flen)
      CALL CHKIN2(ne, 231, 2, l, flen)
      CALL CHKIN2(ne, 231, 3, n, 0)
      CALL CHKIN2(ne, 231, 4, m, 5)
      CALL CHKST2(ne, 231, 5, fields(1), 'in3')
      CALL CHKST2(ne, 231, 6, fields(2), 'in4')
      CALL CHKIN2(ne, 231, 7, j, 7)










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
