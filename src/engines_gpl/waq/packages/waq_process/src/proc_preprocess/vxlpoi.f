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

      SUBROUTINE VXLPOI ( NOCONS , NOFUN  , NODISP , NOVELO , constants,
     +                    FUNAME , DINAME , VENAME , VALNAM , IVALIP   ,
     +                    LINE   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:    december  1994 by Jan van Beek
!
!     FUNCTION            : sets pointers for input on exchange level
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
!     NOCONS  INTEGER       1     INPUT   Number of constants used
!     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
!     NODISP  INTEGER       1     INPUT   Number of dispersion array's
!     NOVELO  INTEGER       1     INPUT   Number of velocity array's
!     CONAME  CHAR*20   NOCONS    INPUT   Constant names
!     FUNAME  CHAR*20   NOFUN     INPUT   Function names
!     DINAME  CHAR*20   NODISP    INPUT   Dispersion names
!     VENAME  CHAR*20   NOVELO    INPUT   Velocity names
!     VALNAM  CHAR*20       1     INPUT   Name of variable in question
!     IVALIP  INTEGER       1     OUTPUT  Pointer in delwaq array
!     LINE    CHAR*(*)      1     OUTPUT  Report line
!
      use timers       !   performance timers

      INTEGER       NOCONS , NOFUN  , NODISP , NOVELO , IVALIP
      CHARACTER*(*) VALNAM, LINE
      CHARACTER*(*)            FUNAME(*),
     +              DINAME(*), VENAME(*)
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
!
!     Local
!
      PARAMETER   (NOPREF=4)
      CHARACTER*10 PREDEF(NOPREF)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "vxlpoi", ithndl )
!
      PREDEF(1) = 'FLOW'
      PREDEF(2) = 'XAREA'
      PREDEF(3) = 'XLENFROM'
      PREDEF(4) = 'XLENTO'
!
!
!     determine how VAL is modelled
!
!     Predefined ?
!
      CALL ZOEK ( VALNAM , NOPREF, PREDEF , 10   , IVALIP )
      IF ( IVALIP .EQ. 1 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ flow'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 2 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ exchange area'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 3 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ from- length'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 4 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ to- length'
         GOTO 800
      ENDIF
!
!     as dispersion ?
!
      CALL ZOEK ( VALNAM , NODISP, DINAME , 10   , IDSP   )
      IF ( IDSP .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using dispersion nr ',IDSP
         IVALIP = NOPREF + IDSP
         GOTO 800
      ENDIF
!
!     as a velocity ?
!
      CALL ZOEK ( VALNAM , NOVELO, VENAME , 10   , IVEL   )
      IF ( IVEL  .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using velocity nr',IVEL
         IVALIP = NOPREF + NODISP + IVEL
         GOTO 800
      ENDIF
!
!     as function ?
!
      CALL ZOEK ( VALNAM , NOFUN , FUNAME , 10   , IFUN   )
      IF ( IFUN .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using function nr',IFUN
         IVALIP = NOPREF + NODISP + NOVELO + IFUN
         GOTO 800
      ENDIF
!
!     as constant ?
!
      ico = dlwq_find(constants,valnam)
      if ( ico .gt. 0 ) then
         write(line,'(a,i3,a,g13.6)') '       Using constant nr',ico,' with value:',constants%constant(ico)
         ivalip = nopref + nodisp + novelo + nofun + ico
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
