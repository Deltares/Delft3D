SUBROUTINE MOZOPENHS (IN, NAME, IOPT)

! *********************************************************************
! *** if IOPT=1 then as Sequentiall formatted file, if IOPT=2 as Binary file.
! **********************************************************************

   INTEGER       IN, IOPT
   CHARACTER*(*)  NAME

   IF (IOPT .EQ. 1) THEN

      OPEN(IN, FILE=NAME, STATUS='unknown')

   ELSE

      OPEN(IN, FILE=NAME, FORM='BINARY', STATUS='unknown')

   ENDIF
!
   RETURN
END
