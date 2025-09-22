!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_calcorder
   use m_waq_precision

   implicit none

contains

   subroutine calcorder(noseg, downstream, simorder)

!        Determine an order of simulation of a WFLOW grid
!        that guaranties that a cell is only simulated if all upstream cells have been simulated
!
!        Noseg (in)      : nr of cells
!        downstream (in) : nr of downstream segment (if 0 or >0 it is an outflow point)
!        simorder (out)  : cell numbers in an order that guarantees that a cell is always treated
!                          after all its upstream neighbours

      integer(kind=int_wp) :: noseg
      integer(kind=int_wp) :: downstream(noseg), simorder(noseg)

      integer(kind=int_wp) :: simcount, iseg
      logical, allocatable :: hasbeendone(:), canbedone(:)

      simcount = 0                   ! count of cells already ranked
      allocate (hasbeendone(noseg))   ! cell was already ranked
      allocate (canbedone(noseg))     ! cell is ready to be ranked, all upstreams have been ranked

      ! quick exit in case there is no network defined (all downstream = -99)
      ! set simorder unchanged
      if (downstream(1) <= -98.) then
         do iseg = 1, noseg
            simorder(iseg) = iseg
         end do
         return
      end if

      ! Determine cells without upstream cells
      hasbeendone = .false.
      canbedone = .true.
      do iseg = 1, noseg
         if (downstream(iseg) > 0) canbedone(downstream(iseg)) = .false.
      end do

      ! deal with these cells without upstream cells first
      do iseg = 1, noseg
         if (canbedone(iseg)) then
            simcount = simcount + 1
            simorder(simcount) = iseg
            hasbeendone(iseg) = .true.
         end if
      end do

      ! iteration
      do
         ! check which cells have all upstreams done
         canbedone = .true.
         do iseg = 1, noseg
            if (downstream(iseg) > 0) then
               if (.not. hasbeendone(iseg)) canbedone(downstream(iseg)) = .false.
            end if
         end do

         ! add cells twhich have all upstreams done to the sequence
         do iseg = 1, noseg
            if (.not. hasbeendone(iseg) .and. canbedone(iseg)) then
               simcount = simcount + 1
               simorder(simcount) = iseg
               hasbeendone(iseg) = .true.
            end if
         end do

         ! all cells done?
         if (simcount == noseg) exit
      end do

      deallocate (hasbeendone)
      deallocate (canbedone)

   end subroutine calcorder
end module m_calcorder
