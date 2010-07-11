C     (C) 2009-2010 D. V. Wiebe
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
C     F77 bindings.  Procedures not tested include: GDCOPN GDMFLS GDFLSH
C     GDDSCD GDCLBK GDCLOS (although this last one is used)

      PROGRAM GETTST
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
      PARAMETER (flen = 7)
      INTEGER nfields
      PARAMETER (nfields = 11)

      CHARACTER*7 fields(nfields + 9)
      CHARACTER*7 fn
      CHARACTER*20 str
      INTEGER*1 c(8)
      INTEGER*1 datdat(80)
      INTEGER i
      INTEGER d
      INTEGER e
      INTEGER m
      INTEGER n
      INTEGER l
      INTEGER ne
      REAL fl
      REAL*8 dp
      REAL*8 p(6), q(6)
      COMPLEX*16 cp(6), cq(6)

      CALL SYSTEM ( 'rm -rf ' // fildir )
      CALL SYSTEM ( 'mkdir ' // fildir )

      DO 10 i = 1, 80
      datdat(i) = i
   10 CONTINUE

      fields = (/ 'INDEX  ', 'bit    ', 'const  ', 'data   ', 'lincom ',
     +'linterp', 'mult   ', 'phase  ', 'polynom', 'sbit   ', 'string ',
     +'       ', '       ', '       ', '       ', '       ', '       ',
     +'       ', '       ', '       ' /)

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
      WRITE(1, *) 'linterp LINTERP data /look/up/file'
      WRITE(1, *) 'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4
     + const const'
      WRITE(1, *) 'bit BIT data 3 4'
      WRITE(1, *) 'sbit SBIT data 5 6'
      WRITE(1, *) 'mult MULTIPLY data sbit'
      WRITE(1, *) 'phase PHASE data 11'
      WRITE(1, *) 'string STRING "Zaphod Beeblebrox"'
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=frm2, STATUS='NEW')
      WRITE(1, *) 'const2 CONST INT8 -19'
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=dat, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=80,
     +STATUS='NEW')
      WRITE (1,REC=1) datdat
      CLOSE(1, STATUS='KEEP')

C     0: GDEROR check
      CALL GDOPEN(d, "x", 1, GD_RO)
      CALL GDEROR(e, d)

      ne = 0
      IF (e .NE. GD_EOP) THEN
        ne = ne + 1
        WRITE(*, 2001) 0, e
      ENDIF

C     1: GDOPEN check
      CALL GDOPEN(d, fildir, 12, GD_RW)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 1, e
      ENDIF

C     2: GDGETD check
      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 2, e
      ENDIF

      IF (n .NE. 8) THEN
        ne = ne + 1
        WRITE(*, 2002) 2, n
      ENDIF

      DO 20 i = 1, 8
      IF (c(i) .NE. 40 + i) THEN
        ne = ne + 1
        WRITE(*, 2004) i, 2, c(i)
      ENDIF
   20 CONTINUE

C     3: GDGTCO check
      CALL GDGTCO(n, d, 'const', 5, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 3, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2002) 3, n
      ENDIF

      IF (abs(fl - 5.5) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2005) 3, fl
      ENDIF

C     4: GDFDNX check
      CALL GDFDNX(i, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 4, e
      ENDIF

      IF (i .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2002) 4, i
      ENDIF

C     5: GDMFNX check
      CALL GDMFNX(i, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 5, e
      ENDIF

      IF (i .NE. 6) THEN
        ne = ne + 1
        WRITE(*, 2002) 5, i
      ENDIF

C     6: GDNFLD check
      CALL GDNFLD(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 6, e
      ENDIF

      IF (n .NE. nfields) THEN
        ne = ne + 1
        WRITE(*, 2002) 6, n
      ENDIF

C     7: This is a check of (one of many instances of) _GDF_FString
      l = 2
      CALL GDFLDN(fn, l, d, 1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 7, e
      ENDIF

      IF (l .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 2002) 7, l
      ENDIF

C     8: GDFLDN check
      DO 80 i = 1, n
      l = flen
      CALL GDFLDN(fn, l, d, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 8, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 8, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 2008) i, 8, fn
      ENDIF
   80 CONTINUE

C     9: GDNMFD check
      CALL GDNMFD(n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 9, e
      ENDIF

      IF (n .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2002) 9, n
      ENDIF

C     10: GDMFDN check
      fields(1) = 'mstr'
      fields(2) = 'mconst'
      fields(3) = 'mlut'
      DO 100 i = 1, n
      l = flen
      CALL GDMFDN(fn, l, d, 'data', 4, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 10, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 10, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 2008) i, 10, fn
      ENDIF
  100 CONTINUE

C     11: GDNFRM check
      CALL GDNFRM(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 11, e
      ENDIF

      IF (n .NE. 10) THEN
        ne = ne + 1
        WRITE(*, 2002) 11, n
      ENDIF

C     12: GDGSPF check
      CALL GDGSPF(n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 12, e
      ENDIF

      IF (n .NE. 8) THEN
        ne = ne + 1
        WRITE(*, 2002) 12, n
      ENDIF

C     13: GDPUTD check
      c = (/ 13, 14, 15, 16, 17, 18, 19, 20 /)
      CALL GDPUTD(n, d, 'data', 4, 5, 1, 0, 4, GD_I8, c)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 13, e
      ENDIF

      IF (n .NE. 4) THEN
        ne = ne + 1
        WRITE(*, 2002) 13, n
      ENDIF

      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c)

      DO 130 i = 1, 8
      IF (((i .EQ. 1 .OR. i .GT. 5) .AND. c(i) .NE. 40 + i) .OR.
     +(i .GT. 1 .AND. i .LT. 6) .AND. c(i) .NE. 11 + i) THEN
        ne = ne + 1
        WRITE(*, 2004) i, 13, c(i)
      ENDIF
  130 CONTINUE

C     14: GDESTR check
      CALL GDGETD(n, d, 'x', 1, 5, 0, 1, 0, GD_I8, c)
      CALL GDESTR(d, str, 20)

      IF (str .NE. 'Field not found: x  ') THEN
        ne = ne + 1
        WRITE(*, 2009) 14, str
      ENDIF

C     15: GDENTY check
      CALL GDENTY(n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 15, e
      ENDIF

      IF (n .NE. GD_RWE) THEN
        ne = ne + 1
        WRITE(*, 2002) 15, n
      ENDIF

C     16: GDGERW check
      CALL GDGERW(l, i, n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 16, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 16, 1, n
      ENDIF

      IF (l .NE. 8) THEN
        ne = ne + 1
        WRITE(*, 2007) 16, 2, l
      ENDIF

      IF (i .NE. GD_I8) THEN
        ne = ne + 1
        WRITE(*, 2007) 16, 3, i
      ENDIF

C     17: GDGELC check
      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'lincom', 6)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 17, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 17, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 17, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 17, 3, n
      ENDIF

      IF (fields(1) .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 17, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'INDEX') THEN
        ne = ne + 1
        WRITE(*, 2008) 17, 5, fields(2)
      ENDIF

      IF (fields(3) .NE. 'linterp') THEN
        ne = ne + 1
        WRITE(*, 2008) 17, 6, fields(3)
      ENDIF

      q = (/ 1.1, 2.2, 2.2, 3.3, 5.5, 5.5 /)
      DO 170 i=1,6
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 17, p(i)
      ENDIF
  170 CONTINUE

C     18: GDGECL check
      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'lincom', 6)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 18, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 18, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 18, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 18, 3, n
      ENDIF

      IF (fields(1) .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 18, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'INDEX') THEN
        ne = ne + 1
        WRITE(*, 2008) 18, 5, fields(2)
      ENDIF

      IF (fields(3) .NE. 'linterp') THEN
        ne = ne + 1
        WRITE(*, 2008) 18, 6, fields(3)
      ENDIF

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      cq(5) = cmplx(5.5, 0.0)
      cq(6) = cmplx(5.5, 0.0)
      DO 180 i=1,6
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 18, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  180 CONTINUE

C     19: GDGEPN check
      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'polynom', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 19, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 19, 1, l
      ENDIF

      IF (i .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 2007) 19, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 19, 3, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 19, 4, fn
      ENDIF

      q = (/ 1.1, 2.2, 2.2, 3.3, 5.5, 5.5 /)
      DO 190 i=1,6
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 19, p(i)
      ENDIF
  190 CONTINUE

C     20: GDGECP check
      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'polynom', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 20, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 20, 1, l
      ENDIF

      IF (i .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 2007) 20, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 20, 3, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 20, 4, fn
      ENDIF

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      cq(5) = cmplx(5.5, 0.0)
      cq(6) = cmplx(5.5, 0.0)
      DO 200 i=1,6
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 30, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  200 CONTINUE

C     21: GDGELT check
      l = flen
      CALL GDGELT(fn, l, str, 20, n, d, 'linterp', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 21, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 21, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 21, 2, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 21, 3, fn
      ENDIF

      IF (str .NE. '/look/up/file') THEN
        ne = ne + 1
        WRITE(*, 2008) 21, 4, str
      ENDIF

C     22: GDGEBT check
      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'bit', 3)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 22, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 22, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 22, 2, n
      ENDIF

      IF (i .NE. 4) THEN
        ne = ne + 1
        WRITE(*, 2007) 22, 3, i
      ENDIF

      IF (m .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 22, 4, m
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 22, 5, fn
      ENDIF

C     23: GDGESB check
      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'sbit', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 23, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 23, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 23, 2, n
      ENDIF

      IF (i .NE. 6) THEN
        ne = ne + 1
        WRITE(*, 2007) 23, 3, i
      ENDIF

      IF (m .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 2007) 23, 4, m
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 23, 5, fn
      ENDIF

C     24: GDGEMT check
      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'mult', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 24, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 24, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 24, 2, n
      ENDIF

      IF (fields(1) .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 24, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'sbit') THEN
        ne = ne + 1
        WRITE(*, 2008) 24, 4, fields(2)
      ENDIF

C     25: GDGEPH check
      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'phase', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 25, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 25, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 25, 2, n
      ENDIF

      IF (i .NE. 11) THEN
        ne = ne + 1
        WRITE(*, 2007) 25, 3, i
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 2008) 25, 4, fn
      ENDIF

C     26: GDGECO check
      CALL GDGECO(i, n, d, 'const', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 26, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 26, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 26, 2, n
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 2007) 26, 3, i
      ENDIF

C     27: GDFRGI check
      CALL GDFRGI(n, d, 'const', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 27, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2002) 27, n
      ENDIF

C     28: GDADRW check
      CALL GDADRW(d, 'new1', 4, GD_F64, 3, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 28, 1, e
      ENDIF

      CALL GDGERW(l, i, n, d, 'new1', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 28, 2, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 28, 3, n
      ENDIF

      IF (l .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 28, 4, l
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 2007) 28, 5, i
      ENDIF

C     29: GDADLC check
      CALL GDADLC(d, 'new2', 4, 2, 'in1', 3, 9.9d0, 8.8d0, 'in2', 3,
     +7.7d0, 6.6d0, '', 0, 0d0, 0d0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 29, 1, e
      ENDIF

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 29, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 29, 3, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 2007) 29, 4, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 29, 5, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 29, 6, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 2008) 29, 7, fields(2)
      ENDIF

      q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
      DO 290 i=1,4
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 29, p(i)
      ENDIF
  290 CONTINUE

C     30: GDADCL check
      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      CALL GDADCL(d, 'new3', 4, 2, 'in1', 3, cq(1), cq(2), 'in2', 3,
     +cq(3), cq(4), '', 0, cq(5), cq(6), 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 30, 1, e
      ENDIF

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new3', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 30, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 30, 1, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 2007) 30, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 30, 3, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 30, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 2008) 30, 5, fields(2)
      ENDIF

      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      DO 300 i=1,4
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 30, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  300 CONTINUE

C     31: GDADPN check
      CALL GDADPN(d, 'new4', 4, 3, 'in1', 3, 3d3, 4d4, 5d5, 6d6, 0d0,
     +0d0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 31, 1, e
      ENDIF

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 31, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 31, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 31, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 31, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 31, 4, fn
      ENDIF

      q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
      DO 310 i=1,4
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 31, p(i)
      ENDIF
  310 CONTINUE

C     32: GDADCP check
      cq(1) = cmplx(3.1, 7.0)
      cq(2) = cmplx(4.2, 8.0)
      cq(3) = cmplx(5.2, 9.0)
      cq(4) = cmplx(6.3, 4.4)
      CALL GDADCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6), 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 32, 1, e
      ENDIF

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new5', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 32, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 32, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 32, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 32, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 32, 4, fn
      ENDIF

      cq(1) = cmplx(3.1, 7.0)
      cq(2) = cmplx(4.2, 8.0)
      cq(3) = cmplx(5.2, 9.0)
      cq(4) = cmplx(6.3, 4.4)
      DO 320 i=1,4
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 32, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  320 CONTINUE

C     33: GDADLT check
      CALL GDADLT(d, "new6", 4, "in", 2, "./some/table", 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 33, 1, e
      ENDIF

      l = flen
      CALL GDGELT(fn, l, str, 20, n, d, 'new6', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 33, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 33, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 33, 2, n
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 2008) 33, 3, fn
      ENDIF

      IF (str .NE. './some/table') THEN
        ne = ne + 1
        WRITE(*, 2008) 33, 4, str
      ENDIF

C     34: GDADBT check
      CALL GDADBT(d, "new7", 4, "in", 2, 13, 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 34, 1, e
      ENDIF

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 34, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 34, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 34, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 2007) 34, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 2007) 34, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 2008) 34, 5, fn
      ENDIF

C     35: GDADSB check
      CALL GDADSB(d, "new8", 4, "in", 2, 13, 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 35, 1, e
      ENDIF

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 35, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 35, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 35, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 2007) 35, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 2007) 35, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 2008) 35, 5, fn
      ENDIF

C     36: GDADMT check
      CALL GDADMT(d, 'new9', 4, 'in1', 3, 'in2', 3, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 36, 1, e
      ENDIF

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 36, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 36, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 36, 2, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 36, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 2008) 36, 4, fields(2)
      ENDIF

C     37: GDADPH check
      CALL GDADPH(d, 'new10', 5, 'in1', 3, 22, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 37, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 37, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 37, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 37, 2, n
      ENDIF

      IF (i .NE. 22) THEN
        ne = ne + 1
        WRITE(*, 2007) 37, 3, i
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 37, 4, fn
      ENDIF

C     38: GDADCO check
      CALL GDADCO(d, 'new11', 5, GD_F64, GD_F32, -8.1, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 38, 1, e
      ENDIF

      CALL GDGECO(i, n, d, 'new11', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 38, 2, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 38, 1, n
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 2007) 38, 2, i
      ENDIF

      CALL GDGTCO(n, d, 'new11', 5, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 38, 3, e
      ENDIF

      IF (abs(fl + 8.1) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2005) 38, fl
      ENDIF

C     39: GDFRGN check
      CALL GDFRGN(str, 20, d, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 39, e
      ENDIF

      IF (str .NE. 'test_dirfile/format') THEN
        ne = ne + 1
        WRITE(*, 2009), 39, str
      ENDIF

C     40: GDNFRG check
      CALL GDNFRG(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 40, e
      ENDIF

      IF (n .NE. 1) THEN
        ne = ne + 1
        WRITE(*, 2002), 40, n
      ENDIF

C     41: GDINCL check
      CALL GDINCL(d, 'form2', 5, 0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 41, 3, e
      ENDIF

      CALL GDGTCO(n, d, 'const2', 6, GD_I8, c(1))
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 41, 3, e
      ENDIF

      IF (c(1) .NE. -19) THEN
        ne = ne + 1
        WRITE(*, 2004) 1, 41, c(1)
      ENDIF

C     42: GDNFDT check
      CALL GDNFDT(n, d, GD_LCE)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 42, e
      ENDIF

      IF (n .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2002), 42, n
      ENDIF

C     43: GDFDNT check
      fields(1) = 'lincom'
      fields(2) = 'new2'
      fields(3) = 'new3'
      DO 430 i = 1, n
      l = flen
      CALL GDFDNT(fn, l, d, GD_LCE, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 43, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 43, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 2008) i, 43, fn
      ENDIF
  430 CONTINUE

C     44: GDNVEC check
      CALL GDNVEC(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 44, e
      ENDIF

      IF (n .NE. 19) THEN
        ne = ne + 1
        WRITE(*, 2002), 44, n
      ENDIF

C     45: GDVECN check
      fields = (/ 'INDEX  ', 'bit    ', 'data   ', 'lincom ', 'linterp',
     +'mult   ', 'new1   ', 'new10  ', 'new2   ', 'new3   ', 'new4   ',
     +'new5   ', 'new6   ', 'new7   ', 'new8   ', 'new9   ', 'phase  ',
     +'polynom', 'sbit   ', 'string ' /)
      DO 450 i = 1, n
      l = flen
      CALL GDVECN(fn, l, d, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 45, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 45, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 2008) i, 45, fn
      ENDIF
  450 CONTINUE

C     46: GDMDLC check
      CALL GDMDLC(d, 'data', 4, 'mnew1', 5, 2, 'in1', 3, 9.9d0, 8.8d0,
     +'in2', 3, 7.7d0, 6.6d0, '', 0, 0d0, 0d0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 46, 1, e
      ENDIF

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'data/mnew1', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 46, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 46, 3, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 2007) 46, 4, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 46, 5, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 46, 6, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 2008) 46, 7, fields(2)
      ENDIF

      q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
      DO 460 i=1,4
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 46, p(i)
      ENDIF
  460 CONTINUE

C     47: GDMDCL check
      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      CALL GDMDCL(d, 'data', 4, 'mnew2', 5, 2, 'in1', 3, cq(1), cq(2),
     +'in2', 3, cq(3), cq(4), '', 0, cq(5), cq(6))
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 47, 1, e
      ENDIF

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'data/mnew2', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 47, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 47, 1, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 2007) 47, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 47, 3, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 47, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 2008) 47, 5, fields(2)
      ENDIF

      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      DO 470 i=1,4
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 47, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  470 CONTINUE

C     48: GDMDPN check
      CALL GDMDPN(d, 'data', 4, 'mnew3', 5, 3, 'in1', 3, 3d3, 4d4, 5d5,
     +6d6, 0d0, 0d0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 48, 1, e
      ENDIF

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'data/mnew3', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 48, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 48, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 48, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 48, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 48, 4, fn
      ENDIF

      q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
      DO 480 i=1,4
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 48, p(i)
      ENDIF
  480 CONTINUE

C     49: GDMDCP check
      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      CALL GDMDCP(d, 'data', 4, 'mnew5', 5, 3, 'in1', 3, cq(1), cq(2),
     +cq(3), cq(4), cq(5), cq(6))
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 49, 1, e
      ENDIF

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'data/mnew5', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 49, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 49, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 49, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 49, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 49, 4, fn
      ENDIF

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      DO 490 i=1,4
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 49, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  490 CONTINUE

C     50: GDMDLT check
      CALL GDMDLT(d, "data", 4, "mnew6", 5, "in", 2, "./more/table", 12)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 50, 1, e
      ENDIF

      l = flen
      CALL GDGELT(fn, l, str, 20, n, d, 'data/mnew6', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 50, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 50, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 50, 2, n
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 2008) 50, 3, fn
      ENDIF

      IF (str .NE. './more/table') THEN
        ne = ne + 1
        WRITE(*, 2008) 50, 4, str
      ENDIF

C     51: GDMDBT check
      CALL GDMDBT(d, "data", 4, "mnew7", 5, "in", 2, 13, 12)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 51, 1, e
      ENDIF

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'data/mnew7', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 51, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 51, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 51, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 2007) 51, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 2007) 51, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 2008) 51, 5, fn
      ENDIF

C     52: GDMDSB check
      CALL GDMDSB(d, "data", 4, "mnew8", 5, "in", 2, 13, 12)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 52, 1, e
      ENDIF

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'data/mnew8', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 52, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 52, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 52, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 2007) 52, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 2007) 52, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 2008) 52, 5, fn
      ENDIF

C     53: GDMDMT check
      CALL GDMDMT(d, 'data', 4, 'mnew9', 5, 'in1', 3, 'in2', 3)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 53, 1, e
      ENDIF

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'data/mnew9', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 53, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 53, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 53, 2, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 53, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 2008) 53, 4, fields(2)
      ENDIF

C     54: GDMDPH check
      CALL GDMDPH(d, 'data', 4, 'mnew10', 6, 'in1', 3, 22)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 54, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'data/mnew10', 11)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 54, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 54, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 54, 2, n
      ENDIF

      IF (i .NE. 22) THEN
        ne = ne + 1
        WRITE(*, 2007) 54, 3, i
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 54, 4, fn
      ENDIF

C     55: GDMDCO check
      CALL GDMDCO(d, 'data', 4, 'mnew11', 6, GD_F64, GD_F32, -8.1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 55, 1, e
      ENDIF

      CALL GDGECO(i, n, d, 'data/mnew11', 11)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 55, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 55, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 55, 2, n
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 2007) 55, 3, i
      ENDIF

      CALL GDGTCO(n, d, 'data/mnew11', 11, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 55, 3, e
      ENDIF

      IF (abs(fl + 8.1) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2005) 55, fl
      ENDIF

C     56: GDGTST check
      CALL GDGTST(n, d, 'string', 6, 20, str)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 56, e
      ENDIF

      IF (n .NE. 17) THEN
        ne = ne + 1
        WRITE(*, 2002) 56, n
      ENDIF

      IF (str .NE. "Zaphod Beeblebrox") THEN
        ne = ne + 1
        WRITE(*, 2009) 56, str
      ENDIF

C     57: GDADST check
      CALL GDADST(d, 'new12', 5, "---string---", 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 57, 1, e
      ENDIF

      CALL GDGTST(n, d, 'new12', 5, 20, str)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 57, 2, e
      ENDIF

      IF (str .NE. "---string---") THEN
        ne = ne + 1
        WRITE(*, 2009) 57, str
      ENDIF

C     58: GDMDST check
      CALL GDMDST(d, "data", 4, 'mnew12', 6, "kai su, technon;", 16)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 58, 1, e
      ENDIF

      CALL GDGTST(n, d, 'data/mnew12', 11, 20, str)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 58, 2, e
      ENDIF

      IF (str .NE. "kai su, technon;") THEN
        ne = ne + 1
        WRITE(*, 2009) 58, str
      ENDIF

C     59: GDADSP check
      CALL GDADSP(d, 'lorem STRING "Lorem ipsum"', 26, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 59, 1, e
      ENDIF

      CALL GDGTST(n, d, 'lorem', 5, 20, str)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 59, 2, e
      ENDIF

      IF (str .NE. "Lorem ipsum") THEN
        ne = ne + 1
        WRITE(*, 2009) 59, str
      ENDIF

C     60: GDMDSP check
      CALL GDMDSP(d, 'ipsum STRING "dolor sit amet."', 30, 'lorem', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 60, 1, e
      ENDIF

      CALL GDGTST(n, d, 'lorem/ipsum', 11, 20, str)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 60, 2, e
      ENDIF

      IF (str .NE. "dolor sit amet.") THEN
        ne = ne + 1
        WRITE(*, 2009) 60, str
      ENDIF

C     61: GDPTCO check
      CALL GDPTCO(n, d, 'const', 5, GD_I32, 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 61, 1, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2002) 61, n
      ENDIF

      CALL GDGTCO(n, d, 'const', 11, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 61, 2, e
      ENDIF

      IF (abs(fl - 10) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2005) 61, fl
      ENDIF

C     62: GDPTST check
      CALL GDPTST(n, d, 'string', 6, 11, "Arthur Dent")
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 62, 1, e
      ENDIF

      IF (n .NE. 11) THEN
        ne = ne + 1
        WRITE(*, 2002) 62, n
      ENDIF

      CALL GDGTST(n, d, 'string', 6, 20, str)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 62, 2, e
      ENDIF

      IF (str .NE. "Arthur Dent") THEN
        ne = ne + 1
        WRITE(*, 2009) 62, str
      ENDIF

C     63: GDNMFT check
      CALL GDNMFT(n, d, "data", 4, GD_LCE)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 63, e
      ENDIF

      IF (n .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 2002), 63, n
      ENDIF

C     64: GDMFDT check
      fields(1) = 'mnew1'
      fields(2) = 'mnew2'
      DO 640 i = 1, n
      l = flen
      CALL GDMFDT(fn, l, d, "data", 4, GD_LCE, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 64, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 64, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 2008) i, 64, fn
      ENDIF
  640 CONTINUE

C     65: GDNMVE check
      CALL GDNMVE(n, d, "data", 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 65, e
      ENDIF

      IF (n .NE. 10) THEN
        ne = ne + 1
        WRITE(*, 2002), 65, n
      ENDIF

C     66: GDMVEN check
      fields = (/ 'mlut  ', 'mnew1 ', 'mnew2 ', 'mnew3 ', 'mnew5 ',
     +'mnew6 ', 'mnew7 ', 'mnew8 ', 'mnew9 ', 'mnew10', '      ',
     +'      ', '      ', '      ', '      ', '      ', '      ',
     +'      ', '      ', '      ' /)
      DO 660 i = 1, n
      l = flen
      CALL GDMVEN(fn, l, d, "data", 4, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 66, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 66, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 2008) i, 66, fn
      ENDIF
  660 CONTINUE

C     67: GDALRW check
      CALL GDALRW(d, 'new1', 4, GD_I32, 4, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 67, 1, e
      ENDIF

      CALL GDGERW(l, i, n, d, 'new1', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 67, 2, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 67, 3, n
      ENDIF

      IF (l .NE. 4) THEN
        ne = ne + 1
        WRITE(*, 2007) 67, 4, l
      ENDIF

      IF (i .NE. GD_I32) THEN
        ne = ne + 1
        WRITE(*, 2007) 67, 5, i
      ENDIF

C     68: GDALLC check
      CALL GDALLC(d, 'new2', 4, 3, 'in4', 3, 9.9d-1, 7.8d0, 'in5', 3,
     +1.1d1, 2.2d-2, 'in6', 3, 1.96d0, 0d0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 68, 1, e
      ENDIF

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 68, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 68, 3, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 68, 4, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 68, 5, n
      ENDIF

      IF (fields(1) .NE. 'in4') THEN
        ne = ne + 1
        WRITE(*, 2008) 68, 6, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in5') THEN
        ne = ne + 1
        WRITE(*, 2008) 68, 7, fields(2)
      ENDIF

      IF (fields(3) .NE. 'in6') THEN
        ne = ne + 1
        WRITE(*, 2008) 68, 8, fields(3)
      ENDIF

      q = (/ 9.9d-1, 7.8d0, 1.1d1, 2.2d-2, 1.96d0, 0d0 /)
      DO 680 i=1,6
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 68, p(i)
      ENDIF
  680 CONTINUE

C     69: GDALCL check
      cq(1) = cmplx(0.1, 0.2)
      cq(2) = cmplx(0.3, 0.4)
      cq(3) = cmplx(0.4, 0.5)
      cq(4) = cmplx(0.6, 0.7)
      CALL GDALCL(d, 'new3', 4, 2, 'in4', 3, cq(1), cq(2), 'in3', 3,
     +cq(3), cq(4), '', 0, cq(5), cq(6))
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 69, 1, e
      ENDIF

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new3', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 69, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 69, 1, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 2007) 69, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 69, 3, n
      ENDIF

      IF (fields(1) .NE. 'in4') THEN
        ne = ne + 1
        WRITE(*, 2008) 69, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in3') THEN
        ne = ne + 1
        WRITE(*, 2008) 69, 5, fields(2)
      ENDIF

      cq(1) = cmplx(0.1, 0.2)
      cq(2) = cmplx(0.3, 0.4)
      cq(3) = cmplx(0.4, 0.5)
      cq(4) = cmplx(0.6, 0.7)
      DO 690 i=1,4
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 69, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  690 CONTINUE

C     70: GDALPN check
      CALL GDALPN(d, 'new4', 4, 4, 'in1', 3, 3d0, 4d0, 5d0, 6d0, 7d0,
     +0d0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 70, 1, e
      ENDIF

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 70, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 70, 1, l
      ENDIF

      IF (i .NE. 4) THEN
        ne = ne + 1
        WRITE(*, 2007) 70, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 70, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 70, 4, fn
      ENDIF

      q = (/ 3d0, 4d0, 5d0, 6d0, 7d0, 0d0 /)
      DO 700 i=1,5
      IF (abs(p(i) - q(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2010) i, 70, p(i)
      ENDIF
  700 CONTINUE

C     71: GDALCP check
      cq(1) = cmplx(1.1, 5.0)
      cq(2) = cmplx(1.2, 4.0)
      cq(3) = cmplx(1.2, 3.0)
      cq(4) = cmplx(1.3, 2.4)
      CALL GDALCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6))
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 71, 1, e
      ENDIF

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'new5', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 71, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 71, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 71, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 71, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 71, 4, fn
      ENDIF

      cq(1) = cmplx(1.1, 5.0)
      cq(2) = cmplx(1.2, 4.0)
      cq(3) = cmplx(1.2, 3.0)
      cq(4) = cmplx(1.3, 2.4)
      DO 710 i=1,4
      IF (abs(cp(i) - cq(i)) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2011) i, 71, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  710 CONTINUE

C     72: GDALLT check
      CALL GDALLT(d, "new6", 4, "in3", 3, "./other/table", 13, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 72, 1, e
      ENDIF

      l = flen
      CALL GDGELT(fn, l, str, 20, n, d, 'new6', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 72, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 72, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 72, 2, n
      ENDIF

      IF (fn .NE. 'in3') THEN
        ne = ne + 1
        WRITE(*, 2008) 72, 3, fn
      ENDIF

      IF (str .NE. './other/table') THEN
        ne = ne + 1
        WRITE(*, 2008) 72, 4, str
      ENDIF

C     73: GDALBT check
      CALL GDALBT(d, "new7", 4, "in3", 3, 3, 2)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 73, 1, e
      ENDIF

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 73, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 73, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 73, 2, n
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 2007) 73, 3, i
      ENDIF

      IF (m .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 73, 4, m
      ENDIF

      IF (fn .NE. 'in3') THEN
        ne = ne + 1
        WRITE(*, 2008) 73, 5, fn
      ENDIF

C     74: GDALSB check
      CALL GDALSB(d, "new8", 4, "out", 3, 1, 22)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 74, 1, e
      ENDIF

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 74, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 74, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 74, 2, n
      ENDIF

      IF (i .NE. 22) THEN
        ne = ne + 1
        WRITE(*, 2007) 74, 3, i
      ENDIF

      IF (m .NE. 1) THEN
        ne = ne + 1
        WRITE(*, 2007) 74, 4, m
      ENDIF

      IF (fn .NE. 'out') THEN
        ne = ne + 1
        WRITE(*, 2008) 74, 5, fn
      ENDIF

C     75: GDALMT check
      CALL GDALMT(d, 'new9', 4, 'in6', 3, 'in4', 3)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 75, 1, e
      ENDIF

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 75, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 75, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 75, 2, n
      ENDIF

      IF (fields(1) .NE. 'in6') THEN
        ne = ne + 1
        WRITE(*, 2008) 75, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in4') THEN
        ne = ne + 1
        WRITE(*, 2008) 75, 4, fields(2)
      ENDIF

C     76: GDALPH check
      CALL GDALPH(d, 'new10', 5, 'in2', 3, 8)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 76, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 76, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 76, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 76, 2, n
      ENDIF

      IF (i .NE. 8) THEN
        ne = ne + 1
        WRITE(*, 2007) 76, 3, i
      ENDIF

      IF (fn .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 2008) 76, 4, fn
      ENDIF

C     77: GDALCO check
      CALL GDALCO(d, 'new11', 5, GD_F32)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 77, 1, e
      ENDIF

      CALL GDGECO(i, n, d, 'new11', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 77, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 77, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 77, 2, n
      ENDIF

      IF (i .NE. GD_F32) THEN
        ne = ne + 1
        WRITE(*, 2007) 77, 3, i
      ENDIF

      CALL GDGTCO(n, d, 'new11', 5, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 77, 3, e
      ENDIF

      IF (abs(fl + 8.1) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2005) 77, fl
      ENDIF

C     78: GDGENC check
      CALL GDGENC(n, d, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 78, e
      ENDIF

      IF (n .NE. GD_EN) THEN
        ne = ne + 1
        WRITE(*, 2002) 78, n
      ENDIF

C     79: GDGEND check
      CALL GDGEND(n, d, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 79, e
      ENDIF

      IF (n .NE. GD_LE) THEN
        ne = ne + 1
        WRITE(*, 2002) 79, n
      ENDIF

C     80: GDNAME check
      l = 20
      CALL GDNAME(str, l, d, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 80, e
      ENDIF

      IF (l .NE. 20) THEN
        ne = ne + 1
        WRITE(*, 2002) 80, l
      ENDIF

      IF (str .NE. 'test_dirfile') THEN
        ne = ne + 1
        WRITE(*, 2009) 80, str
      ENDIF

C     81: GDPFRG check
      CALL GDPFRG(n, d, 1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 81, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2002) 81, n
      ENDIF

C     82: GDAPRT check
      CALL GDAPRT(d, GDPR_D, 1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 82, e
      ENDIF

C     83: GDGPRT check
      CALL GDGPRT(n, d, 1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 83, e
      ENDIF

      IF (n .NE. GDPR_D) THEN
        ne = ne + 1
        WRITE(*, 2002) 83, n
      ENDIF

C     84: GDRWFN check
      l = 20
      CALL GDRWFN(str, l, d, "data", 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 84, e
      ENDIF

      IF (l .NE. 20) THEN
        ne = ne + 1
        WRITE(*, 2002) 84, l
      ENDIF

      IF (str .NE. 'test_dirfile/data') THEN
        ne = ne + 1
        WRITE(*, 2009) 84, str
      ENDIF

C     85: GDREFE check
      l = 20
      CALL GDREFE(str, l, d, "new1", 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 85, e
      ENDIF

      IF (l .NE. 20) THEN
        ne = ne + 1
        WRITE(*, 2002) 85, l
      ENDIF

      IF (str .NE. 'new1') THEN
        ne = ne + 1
        WRITE(*, 2009) 85, str
      ENDIF

C     87: GDAENC check
      CALL GDAENC(d, GD_ES, 1, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 87, 1, e
      ENDIF

      CALL GDGENC(n, d, 1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 87, 2, e
      ENDIF

      IF (n .NE. GD_ES) THEN
        ne = ne + 1
        WRITE(*, 2002) 87, n
      ENDIF

C     88: GDAEND check
      CALL GDAEND(d, GD_BE, 1, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 88, 1, e
      ENDIF

      CALL GDGEND(n, d, 1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 88, 2, e
      ENDIF

      IF (n .NE. GD_BE) THEN
        ne = ne + 1
        WRITE(*, 2002) 88, n
      ENDIF

C     89: GDALSP check
      CALL GDALSP(d, 'new10 PHASE in1 3', 17, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 89, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 89, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 89, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 89, 2, n
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 2007) 89, 3, i
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 2008) 89, 4, fn
      ENDIF

C     90: GDDELE check
      CALL GDDELE(d, 'new10', 5, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 90, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EBC) THEN
        ne = ne + 1
        WRITE(*, 2006) 90, 2, e
      ENDIF

C     91: GDMLSP check
      CALL GDMLSP(d, 'mnew10 PHASE in4 11', 19, 'data', 4, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 91, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'data/mnew10', 11)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 91, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 91, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2007) 91, 2, n
      ENDIF

      IF (i .NE. 11) THEN
        ne = ne + 1
        WRITE(*, 2007) 91, 3, i
      ENDIF

      IF (fn .NE. 'in4') THEN
        ne = ne + 1
        WRITE(*, 2008) 91, 4, fn
      ENDIF

C     92: GDMOVE check
      CALL GDMOVE(d, 'new9', 4, 1, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 92, 1, e
      ENDIF

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 92, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 92, 1, l
      ENDIF

      IF (n .NE. 1) THEN
        ne = ne + 1
        WRITE(*, 2007) 92, 2, n
      ENDIF

      IF (fields(1) .NE. 'in6') THEN
        ne = ne + 1
        WRITE(*, 2008) 92, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in4') THEN
        ne = ne + 1
        WRITE(*, 2008) 92, 4, fields(2)
      ENDIF

C     93: GDRENM check
      CALL GDRENM(d, 'new9', 4, 'newer', 5, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 93, 1, e
      ENDIF

      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EBC) THEN
        ne = ne + 1
        WRITE(*, 2006) 93, 2, e
      ENDIF

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'newer', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 93, 3, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 2007) 93, 1, l
      ENDIF

      IF (n .NE. 1) THEN
        ne = ne + 1
        WRITE(*, 2007) 93, 2, n
      ENDIF

      IF (fields(1) .NE. 'in6') THEN
        ne = ne + 1
        WRITE(*, 2008) 93, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in4') THEN
        ne = ne + 1
        WRITE(*, 2008) 93, 4, fields(2)
      ENDIF

C     94: GDUINC check
      CALL GDUINC(d, 1, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 94, 1, e
      ENDIF

      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'newer', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EBC) THEN
        ne = ne + 1
        WRITE(*, 2006) 94, 2, e
      ENDIF

C     95: GDGFOF check
      CALL GDGFOF(n, d, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 95, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 2002) 95, n
      ENDIF

C     96: GDAFOF check
      CALL GDAFOF(d, 33, 0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 96, e
      ENDIF

      CALL GDGFOF(n, d, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 96, e
      ENDIF

      IF (n .NE. 33) THEN
        ne = ne + 1
        WRITE(*, 2002) 96, n
      ENDIF

C     97: GDNTYP check
      CALL GDNTYP(n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 97, e
      ENDIF

      IF (n .NE. GD_I8) THEN
        ne = ne + 1
        WRITE(*, 2002) 97, n
      ENDIF

C     98: GDCSCL check
      CALL GDCSCL(n, d, 'polynom', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 98, e
      ENDIF

      IF (n .NE. 1) THEN
        ne = ne + 1
        WRITE(*, 2002) 98, n
      ENDIF

C     99: GDVLDT check
      CALL GDVLDT(n, d, 'new7', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EBC) THEN
        ne = ne + 1
        WRITE(*, 2001) 99, e
      ENDIF

      IF (n .NE. -1) THEN
        ne = ne + 1
        WRITE(*, 2002) 99, n
      ENDIF

C     100: GDFNUM check
      l = 20
      CALL GDREFE(str, l, d, "data", 4)
      CALL GDFNUM(dp, d, 'INDEX', 5, 33.3d0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 100, e
      ENDIF

      IF (abs(dp - 33.3) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2012) 100, dp
      ENDIF

C     101: GDFNSS check
      CALL GDFNSS(dp, d, 'data', 4, 33.3d0, 6, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 101, e
      ENDIF

      IF (abs(dp - 37.0375) .gt. 0.001) THEN
        ne = ne + 1
        WRITE(*, 2012) 101, dp
      ENDIF

C     138: GDGSCA check
      l = 20
      CALL GDGSCA(str, l, d, 'lincom', 6, 6)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 138, e
      ENDIF

      IF (str .NE. "const") THEN
        ne = ne + 1
        WRITE(*, 2009) 138, str
      ENDIF

C     139: GDASCA check
      CALL GDASCA(d, 'lincom', 6, 6, 'new11', 5, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 139, 1, e
      ENDIF

      l = 20
      CALL GDGSCA(str, l, d, 'lincom', 6, 6)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2006) 139, 2, e
      ENDIF

      IF (str .NE. "new11") THEN
        ne = ne + 1
        WRITE(*, 2009) 139, str
      ENDIF

C     86: GDGEOF check
      CALL GDGEOF(n, d, 'lincom', 6)
      CALL GDEROR(e, d)

      IF (e. NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 86, e
      ENDIF

      IF (n .NE. 344) THEN
        ne = ne + 1
        WRITE(*, 2002) 86, n
      ENDIF

C     142: GDGBOF check
      CALL GDGBOF(n, d, 'lincom', 6)
      CALL GDEROR(e, d)

      IF (e. NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 2001) 142, e
      ENDIF

      IF (n .NE. 264) THEN
        ne = ne + 1
        WRITE(*, 2002) 142, n
      ENDIF

C     Cleanup
      CALL GDCLOS(d)

      CALL SYSTEM ( 'rm -rf ' // fildir )

      IF (ne .GT. 0) THEN
        WRITE(*, 2003) ne
        CALL EXIT(1)
      ENDIF

 2001 FORMAT('e[', i0, '] = ', i0)
 2002 FORMAT('n[', i0, '] = ', i0)
 2003 FORMAT('ne = ', i0)
 2004 FORMAT('c(', i0, ')[', i0, '] = ', i0)
 2005 FORMAT('fl[', i0, '] = ', f0.16)
 2006 FORMAT('e[', i0, ', ', i0, '] = ', i0)
 2007 FORMAT('n[', i0, ', ', i0, '] = ', i0)
 2008 FORMAT('fn(', i0, ')[', i0, '] = "', a, '"')
 2009 FORMAT('s[' i0, '] = "', a, '"')
 2010 FORMAT('p(', i0, ')[', i0, '] = ', d16.10)
 2011 FORMAT('p(', i0, ')[', i0, '] = ', d16.10, ';', d16.10)
 2012 FORMAT('d[', i0, '] = ', d16.10)

      STOP
      END 
