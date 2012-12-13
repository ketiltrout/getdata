C     Callback test

      SUBROUTINE CALBCK(R, DUNIT, SUBERROR, LINE)
      INCLUDE "getdata.f"

      INTEGER R, DUNIT, SUBERROR
      CHARACTER*(GD_MLL) LINE

C     Tell the parser to ignore syntax errors
      R = GDSX_I

      END SUBROUTINE

      SUBROUTINE CALBK2(R, DUNIT, SUBERROR, LINE)
      INTEGER R

C     An invalid response
      R = 3333

      END SUBROUTINE

      PROGRAM GETTST
      IMPLICIT NONE
      EXTERNAL CALBCK
      EXTERNAL CALBK2
      INCLUDE "getdata.f"

      CHARACTER*12 fildir
      PARAMETER (fildir = 'test_dirfile')
      CHARACTER*19 frmat
      PARAMETER (frmat = 'test_dirfile/format')
      CHARACTER*20 frmat2
      PARAMETER (frmat2 = 'test_dirfile/format2')
      CHARACTER*20 frmat3
      PARAMETER (frmat3 = 'test_dirfile/format3')
      CHARACTER*17 dat
      PARAMETER (dat = 'test_dirfile/data')
      INTEGER*1 datdat(80)
      INTEGER*1 i
      INTEGER d
      INTEGER e1, e2, e3, e4
      INTEGER r

      r = 0

      CALL SYSTEM ( 'rm -rf ' // fildir )
      CALL SYSTEM ( 'mkdir ' // fildir )

      DO 20 i = 1, 80
      datdat(i) = i
   20 CONTINUE

      OPEN(1, FILE=frmat, STATUS='NEW')
      WRITE(1, *) "data RAW INT 8"
      WRITE(1, *) "bad line"
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=frmat2, STATUS='NEW')
      WRITE(1, *) "bad line2"
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=frmat3, STATUS='NEW')
      WRITE(1, *) "bad line3"
      CLOSE(1, STATUS='KEEP')

      OPEN(1, FILE=dat, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=80,
     +STATUS='NEW')
      WRITE (1,REC=1) datdat
      CLOSE(1, STATUS='KEEP')

      CALL GDCOPN(d, fildir, 12, GD_RW, CALBCK)
      CALL GDEROR(e1, d)
      CALL GDINCL(d, "format2", 7, 0, 0)
      CALL GDEROR(e2, d)

C     Change callback
      CALL GDCLBK(d, CALBK2)
      CALL GDINCL(d, "format3", 7, 0, 0)
      CALL GDEROR(e3, d)

C     Delete callback
      CALL GDNOCB(d)
      CALL GDINCL(d, "format3", 7, 0, 0)
      CALL GDEROR(e4, d)
      CALL GDDSCD(d)

      CALL SYSTEM ( 'rm -rf ' // fildir )

      IF (e1 .NE. GD_EOK) THEN
        r = 1
        WRITE(*, 9001) 1, e1, GD_EOK
      ENDIF
      IF (e2 .NE. GD_EOK) THEN
        r = 1
        WRITE(*, 9001) 2, e2, GD_EOK
      ENDIF
      IF (e3 .NE. GD_ECB) THEN
        r = 1
        WRITE(*, 9001) 3, e3, GD_ECB
      ENDIF
      IF (e4 .NE. GD_EFO) THEN
        r = 1
        WRITE(*, 9001) 3, e4, GD_EFO
      ENDIF

      IF (r .GT. 0) CALL ExIT(1)

 9001 FORMAT('e[', i3, '] = ', i4, ', expected ', i4)

      STOP
      END
