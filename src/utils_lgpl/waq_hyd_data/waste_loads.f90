!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!

module m_waste_loads
    !! module contains everything for the wastloads discription

    implicit none

    integer, parameter, private :: NAME_SIZE = 256      ! size of descriptive names
    integer, parameter, private :: MAX_NUM = 5      ! allocated per bunch

    ! wateload types
    integer, parameter :: DLWQ_WASTE_NORMAL = 1  ! normal wasteload
    integer, parameter :: DLWQ_WASTE_INLET = 2  ! inlet wasteload
    integer, parameter :: DLWQ_WASTE_OUTLET = 3  ! outlet wasteload
    integer, parameter :: DLWQ_WASTE_WALK = 4  ! walking wasteload

    type t_wasteload
        character(len = NAME_SIZE) :: name                   ! name of wasteload
        character(len = NAME_SIZE) :: waqtype                ! waqtype of wasteload
        integer :: m                      ! m coordinate
        integer :: n                      ! n coordinate
        integer :: k                      ! k coordinate
        integer :: type                   ! type of wasteload
    end type t_wasteload

    type t_wasteload_coll
        type(t_wasteload), pointer :: wasteload_pnts(:)      ! pointer to the wasteloads
        integer :: maxsize                ! maximum size of the current array
        integer :: current_size                ! filled up to this size
        integer :: no_flow                ! total number of flows
        logical :: l_seconds              ! if time is in seconds or ddhhmmss
    contains
        procedure :: add => wasteload_coll_add
    end type t_wasteload_coll

    private
    public :: t_wasteload, t_wasteload_coll
    public :: DLWQ_WASTE_NORMAL, DLWQ_WASTE_INLET, DLWQ_WASTE_OUTLET, DLWQ_WASTE_WALK

contains

    function wasteload_coll_add(self, wasteload) result (current_size)
        ! function to add to a collection of wasteloads (copy)
        class(t_wasteload_coll) :: self         ! collection of wasteloads
        type(t_wasteload) :: wasteload              ! wasteload to be added
        integer :: current_size                ! return value the new current collection size
        ! and the index of the added wasteload
        type(t_wasteload), pointer :: wasteload_pnts(:)      ! pointer for the resize operation
        integer :: i                      ! loop counter

        if (self%current_size == self%maxsize) then

            ! resize, allocate new array
            allocate (wasteload_pnts (self%maxsize + MAX_NUM))

            ! copy the wasteloads into the new array
            do i = 1, self%maxsize
                wasteload_pnts(i) = self%wasteload_pnts(i)   ! copies the wasteloads
            enddo

            ! deallocate the old array and attach the new array to the collection
            if (self%maxsize /= 0) deallocate (self%wasteload_pnts)
            self%wasteload_pnts => wasteload_pnts
            self%maxsize = self%maxsize + MAX_NUM

        endif

        self%current_size = self%current_size + 1
        self%wasteload_pnts(self%current_size) = wasteload
        current_size = self%current_size

    end function wasteload_coll_add

end module m_waste_loads
