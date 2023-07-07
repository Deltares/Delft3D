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

      SUBROUTINE VALPOI ( NOTOT  , NOPA     , NOSFUN , SYNAME , NOCONS ,
     +                    NOFUN  , constants, PANAME , FUNAME , SFNAME ,
     +                    VALNAM , IVALIP   , LINE   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:    december  1992 by Jan van Beek
!
!     FUNCTION            : sets pointers for process parametrs
!
!     LOGICAL UNITNUMBERS :
!
!     SUBROUTINES CALLED  : ZOEK  , searches a string in an array

      use m_zoek
      use dlwq_data

!     PARAMETERS          : 13
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOTOT   INTEGER       1     INPUT   Total number of substances
!     NOPA    INTEGER       1     INPUT   Number of parameters
!     NOSFUN  INTEGER       1     INPUT   Number of segment functions
!     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
!     NOCONS  INTEGER       1     INPUT   Number of constants used
!     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
!     CONAME  CHAR*20   NOCONS    INPUT   Constant names
!     PANAME  CHAR*20   NOPA      INPUT   Parameter names
!     FUNAME  CHAR*20   NOFUN     INPUT   Function names
!     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
!     VALNAM  CHAR*20       1     INPUT   Name of variable in question
!     IVALIP  INTEGER       1     OUTPUT  Pointer in SSA.
!     LINE    CHAR*(*)      1     OUTPUT  Report line
!
      use timers       !   performance timers

      INTEGER       NOTOT , NOPA  , NOSFUN, NOCONS, NOFUN ,
     +              IVALIP
      CHARACTER*(*) VALNAM, LINE
      CHARACTER*(*) SYNAME(*),
     +              PANAME(*), FUNAME(*),
     +              SFNAME(*)
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
!
!     Local
!
      INTEGER       NZOEK
      PARAMETER   ( NZOEK = 20 )
      PARAMETER   ( NOPRED = 6 )
      CHARACTER(NZOEK) PREDEF(NOPRED)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "valpoi", ithndl )
!
      PREDEF(1) = 'VOLUME'
      PREDEF(2) = 'ITIME'
      PREDEF(3) = 'IDT'
      PREDEF(4) = 'DELT'
      PREDEF(5) = 'ITSTRT'
      PREDEF(6) = 'ITSTOP'
!
!
!     determine how VAL is modelled
!
!     Predefined ?
!
      CALL ZOEK ( VALNAM , NOPRED , PREDEF , NZOEK , IVALIP )
      IF ( IVALIP .EQ. 1 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ volume'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 2 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ time'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 3 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ timestep'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 4 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ timestep in days'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 5 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ start time'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 6 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ stop time'
         GOTO 800
      ENDIF
!
!     as model variable ?
!
      CALL ZOEK ( VALNAM , NOTOT , SYNAME , NZOEK , ISYS   )
      IF ( ISYS .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using substance nr ',ISYS
         IVALIP = NOPRED + NOCONS + NOPA + NOFUN + NOSFUN + ISYS
         GOTO 800
      ENDIF
!
!     as segment function ?
!
      CALL ZOEK ( VALNAM , NOSFUN, SFNAME , NZOEK , ISFUN  )
      IF ( ISFUN .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using segment function nr',ISFUN
         IVALIP = NOPRED + NOCONS + NOPA + NOFUN + ISFUN
         GOTO 800
      ENDIF
!
!     as function ?
!
      CALL ZOEK ( VALNAM , NOFUN , FUNAME , NZOEK , IFUN   )
      IF ( IFUN .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using function nr',IFUN
         IVALIP = NOPRED + NOCONS + NOPA + IFUN
         GOTO 800
      ENDIF
!
!     as parameter ?
!
      CALL ZOEK ( VALNAM , NOPA  , PANAME , NZOEK , IPA    )
      IF ( IPA .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using parameter nr',IPA
         IVALIP = NOPRED + NOCONS + IPA
         GOTO 800
      ENDIF
!
!     as constant ?
!
      ico = dlwq_find(constants,valnam)
      if ( ico .gt. 0 ) then
         write(line,'(a,i3,a,g13.6)') '       Using constant nr',ico,' with value:',constants%constant(ico)
         ivalip = nopred + ico
         goto 800
      endif
!
!     not found
!
      IVALIP = -1
!
  800 CONTINUE
!
      if (timon) call timstop( ithndl )
      RETURN
      END
