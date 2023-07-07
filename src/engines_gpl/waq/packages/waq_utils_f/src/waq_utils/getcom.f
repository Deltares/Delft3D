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
      module m_getcom

      implicit none

      contains


      SUBROUTINE GETCOM ( COMSTR, ARGMOD, FOUND , INTARG, REAARG,
     +                    CHAARG, IERR  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: nov -1992 by Jan van Beek
!
!     FUNCTION            : gives command from command line
!
!     LOGICAL UNITNUMBERS :
!
!     SUBROUTINES CALLED  : DHCARG, gives no. of commandline arguments
!                           DHGARG, gives argument
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     COMSTR  CHA*(*)       1     INPUT   Command to be looked for
!     ARGMOD  INTEGER       1     INPUT   Kind of command argument
!                                         == 0 , no argument asked
!                                         == 1 , integer argument
!                                         == 2 , real argument
!                                         == 3 , character argument
!     FOUND   LOGICAL       1     OUTPUT  Flag if command is found
!     INTARG  INTEGER       1     OUTPUT  Integer argument
!     REAARG  REAL          1     OUTPUT  Real argument
!     CHAARG  CHA*(*)       1     OUTPUT  Character argument
!     IERR    INTEGER       1     OUTPUT  Error indicator
!
!     Declaration of arguments
!
      use m_dhucas
      USE Timers
      use m_dhcarg
      use m_dhgarg

      INTEGER        ARGMOD, INTARG, IERR
      INTEGER        I , I1 , I2 , IA , IF ,  ILEN , ILENC
      REAL           REAARG
      LOGICAL        FOUND
      CHARACTER*(*)  COMSTR, CHAARG
!
!     Local
!

      CHARACTER*256  ARGV , ARG , COM
      integer(4) ithndl /0/
      if ( timon ) call timstrt( "getcom", ithndl )
!
      FOUND = .FALSE.
      IERR  = 0
!
!
!
      I1 = 0
      ILEN = LEN(COMSTR)
      I2 = ILEN
      DO 50 I = 1 , ILEN
         IF ( COMSTR(I:I) .NE. ' ' .AND. I1 .EQ. 0 ) THEN
            I1 = I
         ELSEIF ( COMSTR(I:I) .EQ. ' ' .AND. I1 .NE. 0 ) THEN
            I2 = I - 1
            GOTO 51
         ENDIF
   50 CONTINUE
   51 CONTINUE
      IF ( I1 .EQ. 0 ) GOTO 800
      ILENC = I2 - I1 + 1
      CALL DHUCAS(COMSTR(I1:I2),COM,ILENC)
      DO 100 IA = 2 , DHCARG()
         CALL DHGARG(IA-1,ARGV)
         CALL DHUCAS(ARGV,ARG,ILENC)
         IF ( ARG(1:ILENC) .EQ. COM(1:ILENC) ) THEN
            FOUND  = .TRUE.
            IF     = IA - 1
            GOTO 101
         ENDIF
  100 CONTINUE
  101 CONTINUE

      IF ( FOUND ) THEN
         CALL DHGARG(IF,ARG)
         IF ( ARGMOD .EQ. 1 ) THEN
            IF ( ARG(ILENC+1:ILENC+1) .NE. ' ' ) THEN
               READ ( ARG(ILENC+1:) , '(I20)' , ERR = 110 )
     +              INTARG
               GOTO 111
  110          CONTINUE
               IERR = 3
  111          CONTINUE
            ELSEIF ( IF .LT. DHCARG() - 1 ) THEN
               CALL DHGARG(IF+1,ARG)
               READ ( ARG , '(I20)' , ERR = 120 )
     +              INTARG
               GOTO 121
  120          CONTINUE
               IERR = 2
  121          CONTINUE
            ELSE
               IERR = 1
            ENDIF
         ELSEIF ( ARGMOD .EQ. 2 ) THEN
            IF ( ARG(ILENC+1:ILENC+1) .NE. ' ' ) THEN
               READ ( ARG(ILENC+1:) , '(F20.0)' , ERR = 130 )
     +              REAARG
               GOTO 131
  130          CONTINUE
               IERR = 3
  131          CONTINUE
            ELSEIF ( IF .LT. DHCARG() - 1 ) THEN
               CALL DHGARG(IF+1,ARG)
               READ ( ARG , '(F20.0)' , ERR = 140 )
     +              REAARG
               GOTO 141
  140          CONTINUE
               IERR = 2
  141          CONTINUE
            ELSE
               IERR = 1
            ENDIF
         ELSEIF ( ARGMOD .EQ. 3 ) THEN
            IF ( ARG(ILENC+1:ILENC+1) .NE. ' ' ) THEN
               CHAARG = ARG(ILENC+1:)
            ELSEIF ( IF .LT. DHCARG() - 1 ) THEN
               CALL DHGARG(IF+1,CHAARG)
               IF ( CHAARG(1:1) .EQ. '-' )
     +         THEN
                  CHAARG = ' '
                  IERR = 1
               ENDIF
            ELSE
               IERR = 1
            ENDIF
         ENDIF
      ENDIF
!
  800 CONTINUE
!
      if ( timon ) call timstop( ithndl )
      RETURN
      END
      end module m_getcom
