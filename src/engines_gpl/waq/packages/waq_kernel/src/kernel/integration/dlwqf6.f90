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
module m_dlwqf6

implicit none

contains


      subroutine dlwqf6 ( noseg  , notot  , isys   , nsys   , rhs    ,         &
     &                    conc   , iknmrk )

!     Deltares - Delft Software Department

!>/file
!>           puts solution from RHS in CONC, zeros RHS


!     Created   : June 1988 by Leo Postma

!     Modified  : July 2008, Leo Postma  : WAQ performance timers
!                 July 2009, Leo Postma  : double precission version

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(in   ) :: noseg               !< Number of computational volumes
      integer(4), intent(in   ) :: notot               !< Total number of substances
      integer(4), intent(in   ) :: isys                !< First substance to update
      integer(4), intent(in   ) :: nsys                !< Total number of substances to update
      real   (8), intent(inout) :: rhs (nsys ,noseg)   !< RHS matrix for the nsys substances
      real   (4), intent(inout) :: conc(notot,noseg)   !< Target array for update
      integer(4), intent(in   ) :: iknmrk(noseg)       !< feature array, bit zero indicates wet or not

!     Local declarations

      integer(4)    iseg, j       ! loop variables

!     The WAQ-timer

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqf6", ithandl )

!         put result in concentration array

      do iseg = 1, noseg
         do j = 1, nsys
               conc(isys+j-1,iseg) = rhs(j,iseg)
            rhs (  j     ,iseg) = 0.0d00
         enddo
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

end module m_dlwqf6
