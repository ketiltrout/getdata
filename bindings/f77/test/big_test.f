C     General test
C
C     This verly large test checks almost every procedure defined by the
C     F77 bindings.  Procedures not tested include: GDCOPN GDMFLS GDFLSH
C     GDCLOS (although this last one is used)

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
      PARAMETER (nfields = 10)

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
      REAL*8 p(6), q(6)
      COMPLEX*16 cp(6), cq(6)

      CALL SYSTEM ( 'rm -rf ' // fildir )
      CALL SYSTEM ( 'mkdir ' // fildir )

      DO 10 i = 1, 80
      datdat(i) = i
   10 CONTINUE

      fields = (/ 'INDEX', 'bit', 'const', 'data', 'lincom', 'linterp',
     +'mult', 'phase', 'polynom', 'sbit', '', '', '', '', '', '', '',
     +'', '' /)

C     Write the test dirfile
      OPEN(1, FILE=frmat, STATUS='NEW')
      WRITE(1, *) 'data RAW INT8 8'
      WRITE(1, *) 'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp
     +const const'
      WRITE(1, *) '/META data mstr STRING "This is a string constant."'
      WRITE(1, *) '/META data mconst CONST COMPLEX128 3.3;4.4'
      WRITE(1, *) '/META data mlut LINTERP DATA ./lut'
      WRITE(1, *) 'const CONST FLOAT64 5.5'
      WRITE(1, *) 'linterp LINTERP data /look/up/file'
      WRITE(1, *) 'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4
     +const const'
      WRITE(1, *) 'bit BIT data 3 4'
      WRITE(1, *) 'sbit SBIT data 5 6'
      WRITE(1, *) 'mult MULTIPLY data sbit'
      WRITE(1, *) 'phase PHASE data 11'
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
        WRITE(*, 1001) 0, e
      ENDIF

C     1: GDOPEN check
      CALL GDOPEN(d, fildir, 12, GD_RW)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 1, e
      ENDIF

C     2: GDGETD check
      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c);
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 2, e
      ENDIF

      IF (n .NE. 8) THEN
        ne = ne + 1
        WRITE(*, 1002) 2, n
      ENDIF

      DO 20 i = 1, 8
      IF (c(i) .NE. 40 + i) THEN
        ne = ne + 1
        WRITE(*, 1004) i, 2, c(i)
      ENDIF
   20 CONTINUE

C     3: GDGTCO check
      CALL GDGTCO(n, d, 'const', 5, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 3, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1002) 3, n
      ENDIF

      IF (abs(fl - 5.5) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1005) 3, fl
      ENDIF

C     4: GDFDNX check
      CALL GDFDNX(i, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 4, e
      ENDIF

      IF (i .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1002) 4, i
      ENDIF

C     5: GDMFNX check
      CALL GDMFNX(i, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 5, e
      ENDIF

      IF (i .NE. 6) THEN
        ne = ne + 1
        WRITE(*, 1002) 5, i
      ENDIF

C     6: GDNFLD check
      CALL GDNFLD(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 6, e
      ENDIF

      IF (n .NE. nfields) THEN
        ne = ne + 1
        WRITE(*, 1002) 6, n
      ENDIF

C     7: This is a check of (one of many instances of) _GDF_FString
      l = 2
      CALL GDFLDN(fn, l, d, 1)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 7, e
      ENDIF

      IF (l .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 1002) 7, l
      ENDIF

C     8: GDFLDN check
      DO 80 i = 1, n
      l = flen
      CALL GDFLDN(fn, l, d, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 8, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 8, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 1008) i, 8, fn
      ENDIF
   80 CONTINUE

C     9: GDNMFD check
      CALL GDNMFD(n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 9, e
      ENDIF

      IF (n .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1002) 9, n
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
        WRITE(*, 1006) 10, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 10, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 1008) i, 10, fn
      ENDIF
  100 CONTINUE

C     11: GDNFRM check
      CALL GDNFRM(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 11, e
      ENDIF

      IF (n .NE. 10) THEN
        ne = ne + 1
        WRITE(*, 1002) 11, n
      ENDIF

C     12: GDGSPF check
      CALL GDGSPF(n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 12, e
      ENDIF

      IF (n .NE. 8) THEN
        ne = ne + 1
        WRITE(*, 1002) 12, n
      ENDIF

C     13: GDPUTD check
      c = (/ 13, 14, 15, 16, 17, 18, 19, 20 /)
      CALL GDPUTD(n, d, 'data', 4, 5, 1, 0, 4, GD_I8, c)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 13, e
      ENDIF

      IF (n .NE. 4) THEN
        ne = ne + 1
        WRITE(*, 1002) 13, n
      ENDIF

      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c);

      DO 130 i = 1, 8
      IF (((i .EQ. 1 .OR. i .GT. 5) .AND. c(i) .NE. 40 + i) .OR.
     +(i .GT. 1 .AND. i .LT. 6) .AND. c(i) .NE. 11 + i) THEN
        ne = ne + 1
        WRITE(*, 1004) i, 13, c(i)
      ENDIF
  130 CONTINUE

C     14: GDESTR check
      CALL GDGETD(n, d, 'x', 1, 5, 0, 1, 0, GD_I8, c);
      CALL GDESTR(d, str, 20)

      IF (str .NE. 'Field not found: x  ') THEN
        ne = ne + 1
        WRITE(*, 1009) 14, str
      ENDIF

C     15: GDENTY check
      CALL GDENTY(n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 15, e
      ENDIF

      IF (n .NE. GD_RWE) THEN
        ne = ne + 1
        WRITE(*, 1002) 15, n
      ENDIF

C     16: GDGERW check
      CALL GDGERW(l, i, n, d, 'data', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 16, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 16, 1, n
      ENDIF

      IF (l .NE. 8) THEN
        ne = ne + 1
        WRITE(*, 1007) 16, 2, l
      ENDIF

      IF (i .NE. GD_I8) THEN
        ne = ne + 1
        WRITE(*, 1007) 16, 3, i
      ENDIF

C     17: GDGELC check
      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'lincom', 6)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 17, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 17, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1007) 17, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 17, 3, n
      ENDIF

      IF (fields(1) .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 17, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'INDEX') THEN
        ne = ne + 1
        WRITE(*, 1008) 17, 5, fields(2)
      ENDIF

      IF (fields(3) .NE. 'linterp') THEN
        ne = ne + 1
        WRITE(*, 1008) 17, 6, fields(3)
      ENDIF

      q = (/ 1.1, 2.2, 2.2, 3.3, 5.5, 5.5 /)
      DO 170 i=1,6
      IF (abs(p(i) - q(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1010) i, 17, p(i)
      ENDIF
  170 CONTINUE

C     18: GDGECL check
      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'lincom', 6)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 18, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 18, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1007) 18, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 18, 3, n
      ENDIF

      IF (fields(1) .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 18, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'INDEX') THEN
        ne = ne + 1
        WRITE(*, 1008) 18, 5, fields(2)
      ENDIF

      IF (fields(3) .NE. 'linterp') THEN
        ne = ne + 1
        WRITE(*, 1008) 18, 6, fields(3)
      ENDIF

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      cq(5) = cmplx(5.5, 0.0)
      cq(6) = cmplx(5.5, 0.0)
      DO 180 i=1,6
      IF (abs(cp(i) - cq(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1011) i, 18, realpart(cp(i)), imagpart(cp(i))
      ENDIF
  180 CONTINUE

C     19: GDGEPN check
      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'polynom', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 19, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 19, 1, l
      ENDIF

      IF (i .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 1007) 19, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 19, 3, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 19, 4, fn
      ENDIF

      q = (/ 1.1, 2.2, 2.2, 3.3, 5.5, 5.5 /)
      DO 190 i=1,6
      IF (abs(p(i) - q(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1010) i, 19, p(i)
      ENDIF
  190 CONTINUE

C     20: GDGECP check
      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'polynom', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 20, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 20, 1, l
      ENDIF

      IF (i .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 1007) 20, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 20, 3, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 20, 4, fn
      ENDIF

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      cq(5) = cmplx(5.5, 0.0)
      cq(6) = cmplx(5.5, 0.0)
      DO 200 i=1,6
      IF (abs(cp(i) - cq(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1011) i, 30, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  200 CONTINUE

C     21: GDGELT check
      l = flen
      CALL GDGELT(fn, l, str, 20, n, d, 'linterp', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 21, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 21, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 21, 2, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 21, 3, fn
      ENDIF

      IF (str .NE. '/look/up/file') THEN
        ne = ne + 1
        WRITE(*, 1008) 21, 4, str
      ENDIF

C     22: GDGEBT check
      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'bit', 3)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 22, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 22, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 22, 2, n
      ENDIF

      IF (i .NE. 4) THEN
        ne = ne + 1
        WRITE(*, 1007) 22, 3, i
      ENDIF

      IF (m .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1007) 22, 4, m
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 22, 5, fn
      ENDIF

C     23: GDGESB check
      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'sbit', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 23, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 23, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 23, 2, n
      ENDIF

      IF (i .NE. 6) THEN
        ne = ne + 1
        WRITE(*, 1007) 23, 3, i
      ENDIF

      IF (m .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 1007) 23, 4, m
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 23, 5, fn
      ENDIF

C     24: GDGEMT check
      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'mult', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 24, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 24, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 24, 2, n
      ENDIF

      IF (fields(1) .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 24, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'sbit') THEN
        ne = ne + 1
        WRITE(*, 1008) 24, 4, fields(2)
      ENDIF

C     25: GDGEPH check
      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'phase', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 25, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 25, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 25, 2, n
      ENDIF

      IF (i .NE. 11) THEN
        ne = ne + 1
        WRITE(*, 1007) 25, 3, i
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 25, 4, fn
      ENDIF

C     26: GDGECO check
      CALL GDGECO(i, n, d, 'const', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 26, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 26, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 26, 2, n
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 1007) 26, 3, i
      ENDIF

C     27: GDFRGI check
      CALL GDFRGI(n, d, 'const', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 27, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1002) 27, n
      ENDIF

C     28: GDADRW check
      CALL GDADRW(d, 'new1', 4, GD_F64, 3, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 28, 1, e
      ENDIF

      CALL GDGERW(l, i, n, d, 'new1', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 28, 2, e
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 28, 3, n
      ENDIF

      IF (l .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1007) 28, 4, l
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 1007) 28, 5, i
      ENDIF

C     29: GDADLC check
      CALL GDADLC(d, 'new2', 4, 2, 'in1', 3, 9.9d0, 8.8d0, 'in2', 3,
     +7.7d0, 6.6d0, '', 0, 0d0, 0d0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 29, 1, e
      ENDIF

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 29, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 29, 3, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 1007) 29, 4, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 29, 5, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 29, 6, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 1008) 29, 7, fields(2)
      ENDIF

      q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
      DO 290 i=1,4
      IF (abs(p(i) - q(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1010) i, 29, p(i)
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
        WRITE(*, 1006) 30, 1, e
      ENDIF

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'new3', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 30, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 30, 1, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 1007) 30, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 30, 3, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 30, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 1008) 30, 5, fields(2)
      ENDIF

      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      DO 300 i=1,4
      IF (abs(cp(i) - cq(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1011) i, 30, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  300 CONTINUE

C     31: GDADPN check
      CALL GDADPN(d, 'new4', 4, 3, 'in1', 3, 3d3, 4d4, 5d5, 6d6, 0d0,
     +0d0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 31, 1, e
      ENDIF

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 31, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 31, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1007) 31, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 31, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 31, 4, fn
      ENDIF

      q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
      DO 310 i=1,4
      IF (abs(p(i) - q(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1010) i, 31, p(i)
      ENDIF
  310 CONTINUE

C     32: GDADCP check
      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      CALL GDADCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6), 0d0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 32, 1, e
      ENDIF

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'polynom', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 32, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 32, 1, l
      ENDIF

      IF (i .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 1007) 32, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 32, 3, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 32, 4, fn
      ENDIF

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      DO 320 i=1,6
      IF (abs(cp(i) - cq(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1011) i, 32, realpart(cp(i)), imagpart(cp(i))
      ENDIF
  320 CONTINUE

C     33: GDADLT check
      CALL GDADLT(d, "new6", 4, "in", 2, "./some/table", 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 33, 1, e
      ENDIF

      l = flen
      CALL GDGELT(fn, l, str, 20, n, d, 'new6', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 32, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 33, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 33, 2, n
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 1008) 33, 3, fn
      ENDIF

      IF (str .NE. './some/table') THEN
        ne = ne + 1
        WRITE(*, 1008) 33, 4, str
      ENDIF

C     34: GDADBT check
      CALL GDADBT(d, "new7", 4, "in", 2, 13, 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 34, 1, e
      ENDIF

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 34, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 34, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 34, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 1007) 34, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 1007) 34, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 1008) 34, 5, fn
      ENDIF

C     35: GDADSB check
      CALL GDADSB(d, "new8", 4, "in", 2, 13, 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 35, 1, e
      ENDIF

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 35, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 35, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 35, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 1007) 35, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 1007) 35, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 1008) 35, 5, fn
      ENDIF

C     36: GDADMT check
      CALL GDADMT(d, 'new9', 4, 'in1', 3, 'in2', 3, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 36, 1, e
      ENDIF

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 36, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 36, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 36, 2, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 36, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 1008) 36, 4, fields(2)
      ENDIF

C     37: GDADPH check
      CALL GDADPH(d, 'new10', 5, 'in1', 3, 22, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 37, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 37, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 37, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 37, 2, n
      ENDIF

      IF (i .NE. 22) THEN
        ne = ne + 1
        WRITE(*, 1007) 37, 3, i
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 37, 4, fn
      ENDIF

C     38: GDGECO check
      CALL GDADCO(d, 'new11', 5, GD_F64, GD_F32, -8.1, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 38, 1, e
      ENDIF

      CALL GDGECO(i, n, d, 'const', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 38, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 38, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 38, 2, n
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 1007) 38, 3, i
      ENDIF

      CALL GDGTCO(n, d, 'new11', 5, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 38, 3, e
      ENDIF

      IF (abs(fl + 8.1) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1005) 38, fl
      ENDIF

C     39: GDFRGN check
      CALL GDFRGN(str, 20, d, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 39, e
      ENDIF

      IF (str .NE. 'test_dirfile/format') THEN
        ne = ne + 1
        WRITE(*, 1009), 39, str
      ENDIF

C     40: GDFNRG check
      CALL GDNFRG(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 40, e
      ENDIF

      IF (n .NE. 1) THEN
        ne = ne + 1
        WRITE(*, 1002), 40, n
      ENDIF

C     41: GDINCL check
      CALL GDINCL(d, 'form2', 5, 0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 41, 3, e
      ENDIF

      CALL GDGTCO(n, d, 'const2', 6, GD_I8, c(1))
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 41, 3, e
      ENDIF

      IF (c(1) .NE. -19) THEN
        ne = ne + 1
        WRITE(*, 1004) 1, 41, c(1)
      ENDIF

C     42: GDNFDT check
      CALL GDNFDT(n, d, GD_LCE)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 42, e
      ENDIF

      IF (n .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1002), 42, n
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
        WRITE(*, 1006) 43, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 43, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 1008) i, 43, fn
      ENDIF
  430 CONTINUE

C     44: GDNVEC check
      CALL GDNVEC(n, d)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 44, e
      ENDIF

      IF (n .NE. 19) THEN
        ne = ne + 1
        WRITE(*, 1002), 44, n
      ENDIF

C     45: GDVECN check
      fields = (/ 'INDEX', 'bit', 'data', 'lincom', 'linterp', 'mult',
     +'new1', 'new10', 'new2', 'new3', 'new4', 'new5', 'new6', 'new7',
     +'new8', 'new9', 'phase', 'polynom', 'sbit' /)
      DO 450 i = 1, n
      l = flen
      CALL GDVECN(fn, l, d, i)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 43, i, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 43, i, l
      ENDIF

      IF (fn .NE. fields(i)) THEN
        ne = ne + 1
        WRITE(*, 1008) i, 43, fn
      ENDIF
  450 CONTINUE

C     46: GDMDLC check
      CALL GDMDLC(d, 'data', 4, 'mnew1', 5, 2, 'in1', 3, 9.9d0, 8.8d0,
     +'in2', 3, 7.7d0, 6.6d0, '', 0, 0d0, 0d0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 46, 1, e
      ENDIF

      l = flen
      CALL GDGELC(i, fields(1), l, p(1), p(2), fields(2), l, p(3),
     +p(4), fields(3), l, p(5), p(6), n, d, 'new2', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 46, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 46, 3, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 1007) 46, 4, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 46, 5, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 46, 6, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 1008) 46, 7, fields(2)
      ENDIF

      q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
      DO 460 i=1,4
      IF (abs(p(i) - q(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1010) i, 46, p(i)
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
        WRITE(*, 1006) 47, 1, e
      ENDIF

      l = flen
      CALL GDGECL(i, fields(1), l, cp(1), cp(2), fields(2), l, cp(3),
     +cp(4), fields(3), l, cp(5), cp(6), n, d, 'data/mnew2', 10)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 47, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 47, 1, l
      ENDIF

      IF (i .NE. 2) THEN
        ne = ne + 1
        WRITE(*, 1007) 47, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 47, 3, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 47, 4, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 1008) 47, 5, fields(2)
      ENDIF

      cq(1) = cmplx(1.1, 1.2)
      cq(2) = cmplx(1.3, 1.4)
      cq(3) = cmplx(1.4, 1.5)
      cq(4) = cmplx(1.6, 1.7)
      DO 470 i=1,4
      IF (abs(cp(i) - cq(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1011) i, 47, REAL(REAL(cp(i))), REAL(AIMAG(cp(i)))
      ENDIF
  470 CONTINUE

C     48: GDADPN check
      CALL GDADPN(d, 'new4', 4, 3, 'in1', 3, 3d3, 4d4, 5d5, 6d6, 0d0,
     +0d0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 48, 1, e
      ENDIF

      l = flen
      CALL GDGEPN(i, fn, l, p(1), p(2), p(3), p(4), p(5), p(6),
     +n, d, 'new4', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1001) 48, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 48, 1, l
      ENDIF

      IF (i .NE. 3) THEN
        ne = ne + 1
        WRITE(*, 1007) 48, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 48, 3, n
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 48, 4, fn
      ENDIF

      q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
      DO 480 i=1,4
      IF (abs(p(i) - q(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1010) i, 48, p(i)
      ENDIF
  480 CONTINUE

C     49: GDADCP check
      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      CALL GDADCP(d, 'new5', 4, 3, 'in1', 3, cq(1), cq(2), cq(3), cq(4),
     +cq(5), cq(6), 0d0, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 49, 1, e
      ENDIF

      l = flen
      CALL GDGECP(i, fn, l, cp(1), cp(2), cp(3), cp(4), cp(5), cp(6),
     +n, d, 'polynom', 7)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 49, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 49, 1, l
      ENDIF

      IF (i .NE. 5) THEN
        ne = ne + 1
        WRITE(*, 1007) 49, 2, i
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 49, 3, n
      ENDIF

      IF (fn .NE. 'data') THEN
        ne = ne + 1
        WRITE(*, 1008) 49, 4, fn
      ENDIF

      cq(1) = cmplx(1.1, 0.0)
      cq(2) = cmplx(2.2, 0.0)
      cq(3) = cmplx(2.2, 0.0)
      cq(4) = cmplx(3.3, 4.4)
      DO 490 i=1,6
      IF (abs(cp(i) - cq(i)) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1011) i, 49, realpart(cp(i)), imagpart(cp(i))
      ENDIF
  490 CONTINUE

C     50: GDADLT check
      CALL GDADLT(d, "new6", 4, "in", 2, "./some/table", 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 50, 1, e
      ENDIF

      l = flen
      CALL GDGELT(fn, l, str, 20, n, d, 'new6', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 32, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 50, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 50, 2, n
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 1008) 50, 3, fn
      ENDIF

      IF (str .NE. './some/table') THEN
        ne = ne + 1
        WRITE(*, 1008) 50, 4, str
      ENDIF

C     51: GDADBT check
      CALL GDADBT(d, "new7", 4, "in", 2, 13, 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 51, 1, e
      ENDIF

      l = flen
      CALL GDGEBT(fn, l, m, i, n, d, 'new7', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 51, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 51, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 51, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 1007) 51, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 1007) 51, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 1008) 51, 5, fn
      ENDIF

C     52: GDADSB check
      CALL GDADSB(d, "new8", 4, "in", 2, 13, 12, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 52, 1, e
      ENDIF

      l = flen
      CALL GDGESB(fn, l, m, i, n, d, 'new8', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 52, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 52, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 52, 2, n
      ENDIF

      IF (i .NE. 12) THEN
        ne = ne + 1
        WRITE(*, 1007) 52, 3, i
      ENDIF

      IF (m .NE. 13) THEN
        ne = ne + 1
        WRITE(*, 1007) 52, 4, m
      ENDIF

      IF (fn .NE. 'in') THEN
        ne = ne + 1
        WRITE(*, 1008) 52, 5, fn
      ENDIF

C     53: GDADMT check
      CALL GDADMT(d, 'new9', 4, 'in1', 3, 'in2', 3, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 53, 1, e
      ENDIF

      l = flen
      CALL GDGEMT(fields(1), l, fields(2), l, n, d, 'new9', 4)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 53, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 53, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 53, 2, n
      ENDIF

      IF (fields(1) .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 53, 3, fields(1)
      ENDIF

      IF (fields(2) .NE. 'in2') THEN
        ne = ne + 1
        WRITE(*, 1008) 53, 4, fields(2)
      ENDIF

C     54: GDADPH check
      CALL GDADPH(d, 'new10', 5, 'in1', 3, 22, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 54, 1, e
      ENDIF

      l = flen
      CALL GDGEPH(fn, l, i, n, d, 'new10', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 54, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 54, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 54, 2, n
      ENDIF

      IF (i .NE. 22) THEN
        ne = ne + 1
        WRITE(*, 1007) 54, 3, i
      ENDIF

      IF (fn .NE. 'in1') THEN
        ne = ne + 1
        WRITE(*, 1008) 54, 4, fn
      ENDIF

C     55: GDGECO check
      CALL GDADCO(d, 'new11', 5, GD_F64, GD_F32, -8.1, 0)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 55, 1, e
      ENDIF

      CALL GDGECO(i, n, d, 'const', 5)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 55, 2, e
      ENDIF

      IF (l .NE. flen) THEN
        ne = ne + 1
        WRITE(*, 1007) 55, 1, l
      ENDIF

      IF (n .NE. 0) THEN
        ne = ne + 1
        WRITE(*, 1007) 55, 2, n
      ENDIF

      IF (i .NE. GD_F64) THEN
        ne = ne + 1
        WRITE(*, 1007) 55, 3, i
      ENDIF

      CALL GDGTCO(n, d, 'new11', 5, GD_F32, fl)
      CALL GDEROR(e, d)

      IF (e .NE. GD_EOK) THEN
        ne = ne + 1
        WRITE(*, 1006) 55, 3, e
      ENDIF

      IF (abs(fl + 8.1) > 0.001) THEN
        ne = ne + 1
        WRITE(*, 1005) 55, fl
      ENDIF












      CALL GDCLOS(d)

C     CALL SYSTEM ( 'rm -rf ' // fildir )

      IF (ne .GT. 0) THEN
        WRITE(*, 1003) ne
        CALL EXIT(1)
      ENDIF

 1001 FORMAT('e[', i0, '] = ', i0)
 1002 FORMAT('n[', i0, '] = ', i0)
 1003 FORMAT('ne = ', i0)
 1004 FORMAT('c(', i0, ')[', i0, '] = ', i0)
 1005 FORMAT('fl[', i0, '] = ', f0.16)
 1006 FORMAT('e[', i0, ', ', i0, '] = ', i0)
 1007 FORMAT('n[', i0, ', ', i0, '] = ', i0)
 1008 FORMAT('fn(', i0, ')[', i0, '] = "', a, '"')
 1009 FORMAT('s[' i0, '] = "', a, '"')
 1010 FORMAT('p(', i0, ')[', i0, '] = ', d16.10)
 1011 FORMAT('p(', i0, ')[', i0, '] = ', d16.10, ';', d16.10)

      STOP
      END
