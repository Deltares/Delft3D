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
      module m_movint

      implicit none

      contains


      SUBROUTINE MOVINT ( IAR    , NSTRT  , NOTOT  )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : May '97    BY L. Postma
!
!     FUNCTION           : Shifts an array of integers 1 locations
!
!     SUBROUTINES CALLED : none
!
!     LOGICAL UNITS      : none
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     IAR     INTEGER  NOMAX      IN/OUT  array to be shifted
!     NSTRT   INTEGER  1          INPUT   start of shift
!     NOTOT   INTEGER  1          IN/OUT  stop of shift
!
!
      DIMENSION IAR(*)
      integer ::  I , NOTOT , NSTRT , IAR
!
      DO 10 I=NOTOT,NSTRT,-1
         IAR(I+1) = IAR(I)
   10 CONTINUE
!
      RETURN
      END
      end module m_movint
