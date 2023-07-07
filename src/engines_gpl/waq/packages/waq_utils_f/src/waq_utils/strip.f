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
      module m_strip

      implicit none

      contains


      SUBROUTINE STRIP ( LUNIN  , LFILE  , LUNUT  , LUREP  , NPOS   ,
     *                                              CCHAR  , VRSION )
      use m_srstop

!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : march '88  BY  M.E.Sileon
!
!     FUNCTION            : Deletes empty lines and comment in the
!                           DELWAQ input file
!
!     SUBROUTINE CALLED   : SRSTOP
!
!     LOGICAL UNITNUMBERS : LUNIN = unitnumber auxilary input file
!                           LUNUT = unitnumber scratch file
!                           LUREP = unitnumber error messages
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     LFILE   CHAR*?     1         INPUT   file name
!     NPOS    INTEGER    1         INPUT   number of significant
!                                          positions in one line
!     CCHAR   CHAR*1     1         INPUT   'comment' indicator
!     VRSION  REAL*4     1         OUTPUT  Version number
!
!
      CHARACTER*2000 CAR
      CHARACTER*1    CCHAR
      CHARACTER*(*)  LFILE
      LOGICAL        EMPTY, STRING
      integer :: LUNIN, LUNUT, LUREP
      integer :: NPOS, IL, I, IS
      real    :: VERS2, VRSION
!
      IF ( NPOS .GT. 2000 ) GOTO 60
!
      DO 30 IL = 1 , 1000000
      READ ( LUNIN , '(A)' , END=40 , ERR=50 ) CAR  (1:NPOS)
      EMPTY = .TRUE.
      STRING = .FALSE.
!
!        write line until CCHAR in LUNUT, strip heading spaces
!
      DO 5 I  = 1 , NPOS-19
         IF ( CAR(I:I+14) .EQ. 'DELWAQ_VERSION_' ) THEN
            READ ( CAR(I+15:I+20) , '(F5.0)' ) VERS2
            IF ( VERS2 .GT. VRSION ) THEN
               VRSION = VERS2
               WRITE ( * , '(A,A,F6.3)' ) '       ---------->',
     *                   ' Version number of the input file is: ',VERS2
            ENDIF
            CAR(I:I+19) = ' '
         ENDIF
    5 CONTINUE
!
!        write line until CCHAR in LUNUT, strip heading spaces
!
      IS = 0
      DO 10 I  = 1 , NPOS
      IF ( CAR  (I:I) .EQ. CCHAR ) GOTO 20
      IF ( CAR  (I:I) .NE. ' '  ) EMPTY = .FALSE.
      IF ( .NOT. EMPTY ) THEN
         IF ( CAR  (I:I) .EQ. '''' .OR. CAR  (I:I) .EQ. '"' ) THEN
            IF ( STRING ) THEN
               STRING = .FALSE.
            ELSE
               STRING = .TRUE.
            ENDIF
         ENDIF
         IF ( CAR  (I-1:I) .NE. '  ' .OR. STRING ) THEN
            IS = IS + 1
            CAR  (IS:IS) = CAR  (I:I)
         ENDIF
      ENDIF
   10 CONTINUE
   20 IF ( .NOT. EMPTY ) WRITE( LUNUT , '(A)' )  CAR  (1:IS)
   30 CONTINUE
!
!        end of file encountered
!
   40 REWIND LUNUT
      RETURN
!
!        errors during read
!
   50 WRITE ( LUREP , 2000 ) LUNIN , LFILE
      CALL SRSTOP(1)
!
!        work string to short
!
   60 WRITE ( LUREP , 2001 ) NPOS
      CALL SRSTOP(1)
!
 2000 FORMAT (  ' ERROR, reading file on unit',I3,' !!',
     *         /' Filename is: ',A20,
     *         /' ********** EXECUTION TERMINATED ********' )
 2001 FORMAT (  ' ERROR, number of characters per line exceed maximum',
     *         /' In file ',I4,' , maximum is 1000 ' )
!
      END
      end module m_strip
