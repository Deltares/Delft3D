!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_dhgnam
      use m_srstop
      use m_getcom


      implicit none

      contains


      SUBROUTINE DHGNAM ( NAME   , CHECK )
!
!     Deltares
!
!     CREATED           : Jan 2003 by Jan van Beek
!
!     FUNCTION          : Reads the input filename from keyboard /command (ini file?)
!
!     SUBROUTINE CALLED : DHGARG
!                         SRSTOP
!
!     PARAMETERS        :
!
!     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     NAME    CHAR*(*)   *         OUT     file name input file
!     CHECK   CHAR*(*)   *         IN      if not empty then check existance of file with this extension
!                                          when name is enterd interactive from keyboard
!
      USE ISO_FORTRAN_ENV, ONLY: INPUT_UNIT, OUTPUT_UNIT
      use m_dhgarg

      CHARACTER*(*) NAME
      CHARACTER*(*) CHECK
!
!     Local
!
      CHARACTER*4   EXTS
      CHARACTER*9   EXTS9,FILRUN
      CHARACTER*256 CHECK2
      CHARACTER*3   ANSWER
      LOGICAL       EXI  ,LFOUND
      CHARACTER*1   CRJV  , CJVB  , CDUMMY
      INTEGER       LUNAM, MAXNAM, IDUMMY, IERR, INDX, INDX1, INDX2, IOERR
      REAL          RDUMMY


      CRJV = '/'
      CJVB = '\\'
!
      MAXNAM = LEN(NAME)
!
!     first argument from command line if it is not a option starting with -
!
      CALL DHGARG ( 1     , NAME  )

!
!     Get filename from steering file Delft3D
!
      IF ( NAME .EQ. ' ' .OR. NAME(1:1) .EQ. '-' ) THEN
         FILRUN = ' '
         CALL GETCOM ( '-waq' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.waq'
         ENDIF
         CALL GETCOM ( '-sed' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.sed'
         ENDIF
         CALL GETCOM ( '-eco' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.eco'
         ENDIF
         CALL GETCOM ( '-chem' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.chm'
         ENDIF
         IF ( FILRUN .NE. ' ' ) THEN
            INQUIRE ( FILE = FILRUN , EXIST = EXI )
            IF ( EXI ) THEN
               OPEN(NEWUNIT=LUNAM,FILE=FILRUN)
               READ(LUNAM,'(A)',IOSTAT=IOERR) NAME
               IF ( IOERR .NE. 0 ) NAME = ' '
               CLOSE(LUNAM)
            ENDIF
         ENDIF
      ENDIF
!
!     Get filename filename from keyboard
!
      IF ( NAME .EQ. ' ' .OR. NAME(1:1) .EQ. '-' ) THEN
         WRITE (  OUTPUT_UNIT  ,  *  ) ' Name of the model files ? '
         WRITE (  OUTPUT_UNIT  ,  *  ) ' DELWAQ will provide the extensions. '
         READ  (  INPUT_UNIT  , '(A)' )   NAME
!
!        remove quotes if they are the first and the last
!
         IF ( NAME(1:1) .EQ. '''' ) THEN
            INDX = INDEX ( NAME(2:),'''' )
            IF ( INDX .NE. 0 ) THEN
               IF ( INDX .EQ. MAXNAM-1 ) THEN
                  NAME(INDX+1:) = ' '
                  NAME = NAME(2:)
               ELSEIF ( NAME(INDX+2:) .EQ. ' ' ) THEN
                  NAME(INDX+1:) = ' '
                  NAME = NAME(2:)
               ENDIF
            ENDIF
         ENDIF
!
!        If empty string then stop
!
         IF ( NAME .EQ. ' ' ) THEN
            WRITE (  OUTPUT_UNIT  ,  *  ) ' ERROR no filename entered!'
            CALL SRSTOP(1)
         ENDIF
!
!        check existence of output file
!
         IF ( CHECK .NE. ' ' ) THEN
            INDX  = INDEX ( NAME , ' ' )
            IF ( INDX .EQ. 0 ) INDX = MAXNAM + 1
            CHECK2 = NAME(1:INDX-1)//CHECK
            INQUIRE ( FILE = CHECK2 , EXIST = EXI )
!
            IF ( EXI ) THEN
               WRITE ( OUTPUT_UNIT , * ) ' File:',CHECK2(1:INDX+3),
     +                         ' already exist.'
               WRITE ( OUTPUT_UNIT , * ) ' Do you want it to be replaced ? '
               WRITE ( OUTPUT_UNIT , * ) ' Answer yes or no ? '
               READ  ( INPUT_UNIT , '(A3)' ) ANSWER
               IF ( ANSWER(1:1) .EQ. 'N' .OR.  ANSWER(1:1) .EQ. 'n' )
     *              CALL SRSTOP ( 1 )
            ENDIF
         ENDIF
!
      ELSE
!
!           if there is remove extension
!
         DO 7 INDX1 = MAXNAM , 1 , -1
            IF ( NAME(INDX1:INDX1) .EQ. CRJV) GOTO 8
            IF ( NAME(INDX1:INDX1) .EQ. CJVB) GOTO 8
    7    CONTINUE
    8    CONTINUE
         IF ( INDX1 .EQ. 0 ) INDX1 = 1
         INDX2 = INDEX ( NAME(INDX1:), '.', .TRUE. )
         IF ( INDX2 .GT. 0 ) NAME(INDX1+INDX2-1:MAXNAM) = ' '
!
      ENDIF
!
      RETURN
      END
      end module m_dhgnam
