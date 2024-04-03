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


subroutine calcorder(noseg,downstream,simorder)   
!<
!     Description of the module :
!
!        Determine an order of simulation of a WFLOW grid
!        that guaranties that a cell is only simulated if all upstream cells have been simulated
!     
!        Noseg (in)      : nr of cells
!        downstream (in) : nr of downstream segment (if 0 or >0 it is an outflow point)
!        simorder (out)  : cell numbers in an order that guarantees that a cell is always treated 
!                          after all its upstream neighbours

integer          :: noseg
integer          :: downstream(noseg),simorder(noseg)

integer          :: simcount, iseg
logical,allocatable :: hasbeendone(:), canbedone(:)

simcount = 0 ! count of cells already ranked
allocate(hasbeendone(noseg))   ! cell was already ranked
allocate(canbedone(noseg)) ! cell is ready to be ranked, all upstreams have been ranked

! quick exit in case there is no network defined (all downstream = -99)
! set simorder unchanged
if (downstream(1).le.-98.) then
    do iseg = 1,noseg
        simorder(iseg) = iseg
    enddo
    return
endif

! Determine cells without upstream cells
hasbeendone = .false.
canbedone = .true.
do iseg = 1,noseg
    if (downstream(iseg).gt.0) canbedone(downstream(iseg)) = .false.
enddo

! deal with these cells without upstream cells first
do iseg = 1,noseg
    if (canbedone(iseg)) then
        simcount = simcount + 1
        simorder(simcount) = iseg
        hasbeendone(iseg) = .true.
    endif
enddo
      
! iteration
do
    ! check which cells have all upstreams done
    canbedone = .true.
    do iseg = 1,noseg
        if (downstream(iseg).gt.0) then
            if (.not.hasbeendone(iseg)) canbedone(downstream(iseg)) = .false.
        endif
    enddo
          
    ! add cells twhich have all upstreams done to the sequence
    do iseg = 1,noseg
        if (.not.hasbeendone(iseg).and.canbedone(iseg)) then
            simcount = simcount + 1
            simorder(simcount) = iseg
            hasbeendone(iseg) = .true.
        endif
    enddo
          
    ! all cells done?
    if (simcount.eq.noseg) exit
enddo

deallocate(hasbeendone)
deallocate(canbedone)

!
end subroutine calcorder
