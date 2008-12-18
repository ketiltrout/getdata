C     Attempt to read INT8 via the F77 bindings
      PROGRAM GETTST
      INCLUDE "getdata.f"
      CHARACTER*12 fildir
      PARAMETER (fildir = 'test_dirfile')
      CHARACTER*19 frmat
      PARAMETER (frmat = 'test_dirfile/format')
      CHARACTER*17 dat
      PARAMETER (dat = 'test_dirfile/data')
      CHARACTER*15 frmdat
      PARAMETER (frmdat = 'data RAW INT8 8')
      INTEGER*1 c(8)
      INTEGER*1 datdat(80)
      INTEGER i;
      INTEGER d;
      INTEGER n;
      INTEGER e;

      DO 10 i = 1, 8
      c(i) = 0;
   10 CONTINUE

      CALL SYSTEM ( 'rm -rf ' // fildir )
      CALL SYSTEM ( 'mkdir ' // fildir )

      DO 20 i = 1, 80
      datdat(i) = i
   20 CONTINUE

      OPEN(1, FILE=frmat, STATUS='NEW')
      WRITE(1, *) frmdat
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=dat, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=80,
     +STATUS='NEW')
      WRITE (1,REC=1) datdat
      CLOSE(1, STATUS='KEEP')

      CALL GDOPEN(d, fildir, 12, GD_RO)
      CALL GDGETD(n, d, 'data', 4, 5, 0, 1, 0, GD_I8, c);
      CALL GDEROR(e, d)
      CALL GDCLOS(d)

      CALL SYSTEM ( 'rm -rf ' // fildir )

      IF (e .NE. GD_EOK) CALL EXIT(1)
      IF (n .NE. 8) CALL EXIT(1)
      DO 30 i = 1, 8
      IF (c(i) .NE. 40 + i) CALL EXIT(1)
   30 CONTINUE

      STOP
      END
