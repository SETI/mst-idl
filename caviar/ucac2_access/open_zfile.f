************************************************************************

      SUBROUTINE open_zfile (pathz,un,zn,only_rd)
C
C  input : pathz = path name for zone files
C          un    = Fortran file unit number
C          zn    = zone number = 1, 288
C          only_rd = .TRUE. if only read access

      IMPLICIT NONE
      CHARACTER*(*) pathz
      INTEGER  un,zn, jp
      LOGICAL  only_rd, ifex
      CHARACTER*80 fnzone, answer

      IF (zn.LT.1.OR.zn.GT.288) THEN
        WRITE (*,'(a,i5)') '<open_zf> invalid zone number = ',zn
        STOP
      ENDIF

      IF (only_rd) THEN               ! read file, check existence
 51     jp = INDEX (pathz,' ') - 1

        WRITE (fnzone,'(a,a,i3.3)') pathz,'z',zn

        INQUIRE (FILE=fnzone,EXIST=ifex)

        IF (.NOT.ifex) THEN
          WRITE (*,'(/a)') 'can not find the file:'
          WRITE (*,'(a)') fnzone
          WRITE (*,'(a)') 'please use correct CD or enter new path:'
          WRITE (*,'(a)') '(exit with "x")'
          READ (*,'(a)') answer
          IF (answer.NE.' ') pathz = answer
          IF (pathz(1:1).EQ.'x'.AND.pathz(2:2).EQ.' ') THEN
            STOP
          ELSE
            GOTO 51
          ENDIF
        ENDIF        ! file does not exist

CC      OPEN (un,FILE=fnzone,ACCESS='direct',RECL=44,READONLY)
        OPEN (un,FILE=fnzone,ACCESS='direct',RECL=44)

      ELSE
        WRITE (fnzone,'(a,a,i3.3)') pathz,'z',zn
        OPEN (un,FILE=fnzone,ACCESS='direct',RECL=44)
      ENDIF ! read or write access

      END   ! subr. <open_zfile>

************************************************************************