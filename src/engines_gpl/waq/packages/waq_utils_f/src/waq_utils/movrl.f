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
      module m_movrl

      implicit none

      contains


      SUBROUTINE MOVRL  ( RAR    , NSTRT  , NOTOT  , NLOC   )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : May '97    BY L. Postma
!
!     FUNCTION           : Shifts an array of reals NLOC locations
!
!     SUBROUTINES CALLED : none
!
!     LOGICAL UNITS      : none
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     RAR     REAL     *          IN/OUT  array to be shifted
!     NSTRT   INTEGER  1          INPUT   start of shift
!     NOTOT   INTEGER  1          IN/OUT  stop of shift
!     NLOC    INTEGER  1          INPUT   nr of locations to shift
!
!
      DIMENSION RAR(*)
      real :: RAR
      integer :: I , NOTOT , NLOC , NSTRT
!
      DO 10 I=NOTOT,NSTRT,-1
         RAR(I+NLOC) = RAR(I)
   10 CONTINUE
!
      RETURN
      END
      end module m_movrl
