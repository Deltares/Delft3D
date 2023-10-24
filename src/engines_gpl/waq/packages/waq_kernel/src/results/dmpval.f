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
      module m_dmpval
      use m_waq_type_definitions


      implicit none

      contains


      SUBROUTINE DMPVAL (NDMPAR, IPDMP , VALSEG, VALDMP)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : dec 2003 by Jan van Beek
!
!     FUNCTION            : sums values for sub-area's
!
!     PARAMETERS          : 4
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NDMPAR  INTEGER(kind=int_32) ::1     INPUT   Number of dump areas
!     IPDMP   INTEGER(kind=int_32) ::*     INPUT   pointer structure dump area's
!     VALSEG  REAL(kind=sp) ::*     INPUT   values on segment grid
!     VALDMP  REAL(kind=sp) ::*     INPUT   values on dump grid
!
!     Declaration of arguments
!
      use timers

      INTEGER(kind=int_32) ::NDMPAR
      INTEGER(kind=int_32) ::IPDMP(*)
      REAL(kind=sp) ::VALSEG(*)
      REAL(kind=sp) ::VALDMP(*)
!
!     Local declarations
!
      INTEGER(kind=int_32) ::ITEL  , IDUMP , NSC   , ISC   , ISEG
      integer(kind=int_32) ::ithandl = 0
      if ( timon ) call timstrt ( "dmpval", ithandl )

!     Loop over the dump area's, sum value

      VALDMP(1:NDMPAR) = 0.0
      ITEL              = 0
      DO IDUMP = 1 , NDMPAR
         NSC = IPDMP(IDUMP)
         DO ISC = 1 , NSC
            ITEL  = ITEL + 1
            ISEG  = IPDMP(NDMPAR+ITEL)
            IF ( ISEG .GT. 0 ) THEN
               VALDMP(IDUMP) = VALDMP(IDUMP) + VALSEG(ISEG)
            ENDIF
         ENDDO
      ENDDO

      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dmpval
