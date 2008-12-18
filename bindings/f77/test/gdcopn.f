C     Callback test

      SUBROUTINE CALBCK(R, DUNIT, SUBERROR, LINE)
      INCLUDE "getdata.f"

      INTEGER R, DUNIT, SUBERROR
      CHARACTER*(GD_MLL) LINE

C     Tell the parser to ignore syntax errors
      R = GDSX_I

      END SUBROUTINE

      PROGRAM GETTST
      EXTERNAL CALBCK
      INCLUDE "getdata.f"

      CHARACTER*12 fildir
      PARAMETER (fildir = 'test_dirfile')
      CHARACTER*19 frmat
      PARAMETER (frmat = 'test_dirfile/format')
      CHARACTER*17 dat
      PARAMETER (dat = 'test_dirfile/data')
      CHARACTER*15 frmdat1
      PARAMETER (frmdat1 =  'data RAW INT8 8')
      CHARACTER*9 frmdat2
      PARAMETER (frmdat2 =  'bad line')
      INTEGER*1 datdat(80)
      INTEGER i;
      INTEGER d;
      INTEGER e;

      CALL SYSTEM ( 'rm -rf ' // fildir )
      CALL SYSTEM ( 'mkdir ' // fildir )

      DO 20 i = 1, 80
      datdat(i) = i
   20 CONTINUE

      OPEN(1, FILE=frmat, STATUS='NEW')
      WRITE(1, *) frmdat1
      WRITE(1, *) frmdat2
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=dat, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=80,
     +STATUS='NEW')
      WRITE (1,REC=1) datdat
      CLOSE(1, STATUS='KEEP')

      CALL GDCOPN(d, fildir, 12, GD_RO, CALBCK)
      CALL GDEROR(e, d)
      CALL GDCLOS(d)

      CALL SYSTEM ( 'rm -rf ' // fildir )

      IF (e .NE. GD_EOK) CALL EXIT(1)

      STOP
      END
